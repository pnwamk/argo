{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SAWServer.ProofScript
  ( ProofScript
  , interpretProofScript
  , makeSimpset
  , prove
  ) where

import Control.Applicative
import Control.Monad (foldM)
import Data.Aeson
import Data.Text (Text)

import Argo
import qualified SAWScript.Builtins as SB
import qualified SAWScript.Value as SV
import SAWServer
import SAWServer.Exceptions
import SAWServer.OK
import SAWServer.TopLevel
import Verifier.SAW.Rewriter (addSimp, emptySimpset)
import Verifier.SAW.TermNet (merge)
import Verifier.SAW.TypedTerm (TypedTerm(..))

data Prover
  = ABC
  | CVC4 [String]
  | RME
  | Yices [String]
  | Z3 [String]

data ProofTactic
  = UseProver Prover
  | Unfold [String]
  | BetaReduceGoal
  | EvaluateGoal [String]
  | Simplify ServerName
  | AssumeUnsat
  | Trivial

newtype ProofScript = ProofScript [ProofTactic]

instance FromJSON Prover where
  parseJSON =
    withObject "prover" $ \o -> do
      (name :: String) <- o .: "name"
      case name of
        "abc"   -> pure ABC
        "cvc4"  -> CVC4 <$> o .: "uninterpreted functions"
        "rme"   -> pure RME
        "yices" -> Yices <$> o .: "uninterpreted functions"
        "z3"    -> Z3 <$> o .: "uninterpreted functions"
        _       -> empty

instance FromJSON ProofTactic where
  parseJSON =
    withObject "proof tactic" $ \o -> do
      (tac :: String) <- o .: "tactic"
      case tac of
        "use prover"       -> UseProver <$> o .: "prover"
        "unfold"           -> Unfold <$> o .: "names"
        "beta reduce goal" -> pure BetaReduceGoal
        "evalute goal"     -> EvaluateGoal <$> o .: "uninterpreted functions"
        "simplify"         -> Simplify <$> o .: "rules"
        "assume unsat"     -> pure AssumeUnsat
        "trivial"          -> pure Trivial
        _                  -> empty

instance FromJSON ProofScript where
  parseJSON =
    withObject "proof script" $ \o -> ProofScript <$> o .: "tactics"

data MakeSimpsetParams =
  MakeSimpsetParams
  { ssElements :: [ServerName]
  , ssResult :: ServerName
  }

instance FromJSON MakeSimpsetParams where
  parseJSON =
    withObject "SAW/make simpset params" $ \o ->
    MakeSimpsetParams <$> o .: "elements"
                      <*> o .: "result"

makeSimpset :: MakeSimpsetParams -> Method SAWState OK
makeSimpset params = do
  let add ss n = do
        v <- getServerVal n
        case v of
          VSimpset ss' -> return (merge ss ss')
          VTerm t -> return (addSimp (ttTerm t) ss)
          _ -> raise (notASimpset n)
  ss <- foldM add emptySimpset (ssElements params)
  setServerVal (ssResult params) ss
  ok

data ProveParams =
  ProveParams
  { ppScript   :: ProofScript
  , ppTermName :: ServerName
  }

instance FromJSON ProveParams where
  parseJSON =
    withObject "SAW/prove params" $ \o ->
    ProveParams <$> o .: "script"
                <*> o .: "term"

--data CexValue = CexValue String TypedTerm

data ProveResult
  = ProofValid
  | ProofInvalid -- [CexValue]

--instance ToJSON CexValue where
--  toJSON (CexValue n t) = object [ "name" .= n, "value" .= t ]

instance ToJSON ProveResult where
  toJSON ProofValid = object [ "status" .= ("valid" :: Text)]
  toJSON ProofInvalid {-cex-} =
    object [ "status" .= ("invalid" :: Text) ] -- , "counterexample" .= cex]

prove :: ProveParams -> Method SAWState ProveResult
prove params = do
  t <- getTerm (ppTermName params)
  proofScript <- interpretProofScript (ppScript params)
  res <- tl $ SB.provePrim proofScript t
  case res of
    SV.Valid _ -> return ProofValid
    SV.InvalidMulti _  _ -> return ProofInvalid

interpretProofScript :: ProofScript -> Method SAWState (SV.ProofScript SV.SatResult)
interpretProofScript (ProofScript ts) = go ts
  where go [UseProver ABC]            = return $ SB.satABC
        go [UseProver (CVC4 unints)]  = return $ SB.satWhat4_UnintCVC4 unints
        go [UseProver RME]            = return $ SB.satRME
        go [UseProver (Yices unints)] = return $ SB.satWhat4_UnintYices unints
        go [UseProver (Z3 unints)]    = return $ SB.satWhat4_UnintZ3 unints
        go [Trivial]                  = return $ SB.trivial
        go [AssumeUnsat]              = return $ SB.assumeUnsat
        go (BetaReduceGoal : rest)    = do
          m <- go rest
          return (SB.beta_reduce_goal >> m)
        go (EvaluateGoal fns : rest)  = do
          m <- go rest
          return (SB.goal_eval fns >> m)
        go (Unfold fns : rest)        = do
          m <- go rest
          return (SB.unfoldGoal fns >> m)
        go (Simplify sn : rest)       = do
          ss <- getSimpset sn
          m <- go rest
          return (SB.simplifyGoal ss >> m)
        go _ = fail "malformed proof script"
