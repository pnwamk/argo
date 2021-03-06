#!/usr/bin/env python3
from saw import *
from saw.dashboard import Dashboard
from saw.llvm import uint32_t, Contract, void

import os
import os.path

dir_path = os.path.dirname(os.path.realpath(__file__))
swap_bc = os.path.join(dir_path, 'swap.bc')

class Swap(Contract):
    def __init__(self) -> None:
        super().__init__()
        self.t = uint32_t

    def pre(self) -> None:
        self.x = self.declare(self.t)
        self.y = self.declare(self.t)
        self.x_pointer = self.declare_pointer(self.t)
        self.y_pointer = self.declare_pointer(self.t)
        self.points_to(self.x_pointer, self.x)
        self.points_to(self.y_pointer, self.y)

    def call(self) -> None:
        self.arguments(self.x_pointer, self.y_pointer)

    def post(self) -> None:
        self.points_to(self.x_pointer, self.y)
        self.points_to(self.y_pointer, self.x)
        self.returns(void)

connect("cabal -v0 v2-run exe:saw-remote-api")
view(DebugLog(err=None))
view(LogResults())

mod = llvm_load_module(swap_bc)

result = llvm_verify(mod, 'swap', Swap())
