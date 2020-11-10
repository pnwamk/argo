import unittest
import random
from bits import Bits


class BitsBaseTest(unittest.TestCase):
    """Base class for Bits test cases."""

    def assertBitsEqual(self, b, size, value):
        """Assert Bits `b` has the specified `size` and `value`."""
        self.assertEqual(b.size(),  size)
        self.assertEqual(b.value(), value)

    
    def assertUnOpExpected(self, op_fn, expected_fn):
        """Assert `prop` holds for any Bits value."""
        for width in range(0, 129):
            max_val = 2 ** width - 1
            for i in range(0, 100):
                b = Bits(width, random.randint(0, max_val))
                # Put `b` in the assertion so we can see its value
                # on failed test cases.
                self.assertEqual((b, op_fn(b)), (b, expected_fn(b)))

    def assertBinOpExpected(self, op_fn, expected_fn):
        """Assert `prop` holds for any Bits value."""
        for width in range(0, 129):
            max_val = 2 ** width - 1
            for i in range(0, 100):
                b1 = Bits(width, random.randint(0, max_val))
                b2 = Bits(width, random.randint(0, max_val))
                # Put `b1` and `b2` in the assertion so we can
                # see its value on failed test cases.
                self.assertEqual((b1,b2,op_fn(b1, b2)), (b1,b2,expected_fn(b1, b2)))


class BitsBasicTests(BitsBaseTest):
    def test_constructor1(self):
        b = Bits(0,0)
        self.assertBitsEqual(b, 0, 0)
        
    def test_constructor2(self):
        b = Bits(8,42)
        self.assertBitsEqual(b, 8, 42)

    def test_constructor_fails(self):
        with self.assertRaises(ValueError):
            Bits(8, 256)
        with self.assertRaises(ValueError):
            Bits(8, -1)

    def test_hex(self):
        self.assertEqual(hex(Bits(0,0)), "0x0")
        self.assertEqual(Bits(0,0).hex(), "0x0")
        self.assertEqual(hex(Bits(4,0)), "0x0")
        self.assertEqual(Bits(4,0).hex(), "0x0")
        self.assertEqual(hex(Bits(5,0)), "0x0")
        self.assertEqual(Bits(5,0).hex(), "0x00")
        self.assertEqual(hex(Bits(5,11)), "0xb")
        self.assertEqual(Bits(5,11).hex(), "0x0b")
        self.assertEqual(hex(Bits(8,255)), "0xff")
        self.assertEqual(Bits(8,255).hex(), "0xff")
        self.assertEqual(hex(Bits(9,255)), "0xff")
        self.assertEqual(Bits(9,255).hex(), "0x0ff")

    def test_repr(self):
        self.assertEqual(repr(Bits(0,0)), "Bits(0, 0x0)")
        self.assertEqual(repr(Bits(9,255)), "Bits(9, 0x0ff)")

    def test_int(self):
        self.assertEqual(int(Bits(0,0)), 0)
        self.assertEqual(int(Bits(9,255)), 255)
        self.assertUnOpExpected(
            lambda b: Bits(b.size(), int(b)),
            lambda b: b)

    def test_len(self):
        self.assertEqual(len(Bits(0,0)), 0)
        self.assertEqual(len(Bits(9,255)), 9)

    def test_popcount(self):
        self.assertEqual(Bits(0,0).popcount(), 0)
        self.assertEqual(Bits(8,0).popcount(), 0)
        self.assertEqual(Bits(8,1).popcount(), 1)
        self.assertEqual(Bits(8,2).popcount(), 1)
        self.assertEqual(Bits(8,3).popcount(), 2)
        self.assertEqual(Bits(8,255).popcount(), 8)

    def test_eq(self):
        self.assertEqual(Bits(0,0), Bits(0,0))
        self.assertEqual(Bits(8,255), Bits(8,255))
        self.assertEqual(Bits(8,255), 255)
        self.assertEqual(255, Bits(8,255))
        self.assertNotEqual(Bits(8,255), 254)
        self.assertNotEqual(254, Bits(8,255))
        self.assertTrue(Bits(8,255) == Bits(8,255))
        self.assertTrue(Bits(8,255) == 255)
        self.assertTrue(255 == Bits(8,255))
        self.assertFalse(Bits(8,255) == Bits(8,254))
        self.assertFalse(Bits(8,255) == 254)
        self.assertFalse(254 == Bits(8,255))
        self.assertFalse(Bits(8,255) == Bits(9,255))

    def test_neq(self):
        self.assertNotEqual(Bits(0,0), Bits(1,0))
        self.assertNotEqual(Bits(0,0), 1)
        self.assertNotEqual(1, Bits(0,0))
        self.assertTrue(Bits(0,0) != Bits(1,0))
        self.assertTrue(Bits(1,0) != Bits(0,0))
        self.assertFalse(Bits(0,0) != Bits(0,0))
        self.assertFalse(Bits(0,0) != 0)
        self.assertFalse(0 != Bits(0,0))

    def test_add(self):
        self.assertEqual(Bits(16,7) + Bits(16,9), Bits(16,16))
        self.assertEqual(Bits(16,9) + Bits(16,7), Bits(16,16))
        self.assertEqual(Bits(16,9) + Bits(16,7) + 1, Bits(16,17))
        self.assertEqual(1 + Bits(16,9) + Bits(16,7), Bits(16,17))
        self.assertBinOpExpected(
            lambda b1, b2: b1 + b2,
            lambda b1, b2: Bits(0,0) if b1.size() == 0 else
                           Bits(b1.size(), (int(b1) + int(b2)) % ((2 ** b1.size() - 1) + 1)))
        with self.assertRaises(ValueError):
            Bits(15,7) + Bits(16,9)
                
    
    def test_bitewise_and(self):
        self.assertEqual(Bits(0,0) & Bits(0,0), Bits(0,0))
        self.assertEqual(Bits(8,0xff) & Bits(8,0xff), Bits(8,0xff))
        self.assertEqual(Bits(8,0xff) & Bits(8,42), Bits(8,42))
        self.assertEqual(Bits(16,7) & Bits(16,9), Bits(16,1))
        self.assertEqual(Bits(16,9) & Bits(16,7), Bits(16,1))
        self.assertEqual(Bits(16,9) & Bits(16,7) & 1, Bits(16,1))
        self.assertEqual(1 & Bits(16,9) & Bits(16,7), Bits(16,1))
        self.assertUnOpExpected(
            lambda b: b & 0,
            lambda b: Bits(b.size(), 0))
        self.assertUnOpExpected(
            lambda b: b & (2 ** b.size() - 1),
            lambda b: b)
        self.assertBinOpExpected(
            lambda b1, b2: b1 & b2,
            lambda b1, b2: Bits(b1.size(), int(b1) & int(b2)))
        with self.assertRaises(ValueError):
            Bits(15,7) & Bits(16,9)

    def test_bitewise_not(self):
        self.assertEqual(~Bits(0,0), Bits(0,0))
        self.assertEqual(~Bits(1,0b0), Bits(1,0b1))
        self.assertEqual(~Bits(8,0x0f), Bits(8,0xf0))
        self.assertEqual(~Bits(10,0b0001110101), Bits(10,0b1110001010))
        self.assertEqual(~~Bits(10,0b0001110101), Bits(10,0b0001110101))
        self.assertUnOpExpected(
            lambda b: ~~b,
            lambda b: b)
        self.assertUnOpExpected(
            lambda b: ~b & b,
            lambda b: Bits(b.size(), 0))


    def test_positional_index(self):
        self.assertFalse(Bits(16,0b10)[0])
        self.assertTrue(Bits(16,0b10)[1])
        self.assertFalse(Bits(16,0b10)[3])
        self.assertFalse(Bits(8,0b10)[7])
        with self.assertRaises(ValueError):
            Bits(8,7)["Bad Index"]
        with self.assertRaises(ValueError):
            Bits(8,7)[-1]
        with self.assertRaises(ValueError):
            Bits(8,7)[8]

    def test_positional_slice(self):
        self.assertEqual(Bits(0,0)[0:0], Bits(0,0))
        self.assertEqual(Bits(16,0b10)[2:0], Bits(2,0b10))
        self.assertEqual(Bits(16,0b10)[16:0], Bits(16,0b10))
        self.assertEqual(Bits(16,0b1100110011001100)[16:8], Bits(8,0b11001100))
        with self.assertRaises(ValueError):
            Bits(0,0)[2:0]
        with self.assertRaises(ValueError):
            Bits(8,42)[0:1]
        with self.assertRaises(ValueError):
            Bits(8,42)[9:0]
        with self.assertRaises(ValueError):
            Bits(8,42)[8:-1]
        with self.assertRaises(ValueError):
            Bits(8,42)[10:10]

    def test_concat(self):
        self.assertEqual(Bits(0,0).concat(Bits(0,0)), Bits(0,0))
        self.assertEqual(Bits(1,0b1).concat(Bits(0,0b0)), Bits(1,0b1))
        self.assertEqual(Bits(0,0b0).concat(Bits(1,0b1)), Bits(1,0b1))
        self.assertEqual(Bits(1,0b1).concat(Bits(1,0b0)), Bits(2,0b10))
        self.assertEqual(Bits(1,0b0).concat(Bits(1,0b1)), Bits(2,0b01))
        self.assertEqual(Bits(1,0b1).concat(Bits(1,0b1)), Bits(2,0b11))
        self.assertEqual(Bits(5,0b11111).concat(Bits(3,0b000)), Bits(8,0b11111000))
        self.assertEqual(Bits(0,0).concat(), Bits(0,0))
        self.assertEqual(Bits(0,0).concat(Bits(2,0b10),Bits(2,0b01)), Bits(4,0b1001))
        self.assertBinOpExpected(
            lambda b1, b2: b1.concat(b2)[b2.size():0],
            lambda b1, b2: b2)
        self.assertBinOpExpected(
            lambda b1, b2: b1.concat(b2)[b1.size() + b2.size():b2.size()],
            lambda b1, b2: b1)
        with self.assertRaises(ValueError):
            Bits(8,42).concat(42)
        with self.assertRaises(ValueError):
            Bits(8,42).concat("Oops not a Bits")

    def test_join(self):
        self.assertEqual(Bits.join(), Bits(0,0))
        self.assertEqual(Bits.join(*[]), Bits(0,0))
        self.assertEqual(Bits.join(Bits(8,42)), Bits(8,42))
        self.assertEqual(Bits.join(*[Bits(8,42)]), Bits(8,42))
        self.assertEqual(Bits.join(Bits(0,0), Bits(2,0b10),Bits(3,0b110)), Bits(5,0b10110))
        self.assertEqual(Bits.join(*[Bits(0,0), Bits(2,0b10),Bits(3,0b110)]), Bits(5,0b10110))

    def test_bytes(self):
        self.assertEqual(bytes(Bits(0,0)), b'')
        self.assertEqual(bytes(Bits(1,1)), b'\x01')
        self.assertEqual(bytes(Bits(8,255)), b'\xff')
        self.assertEqual(bytes(Bits(16,255)), b'\x00\xff')


    def test_zero(self):
        self.assertEqual(Bits(0,0).zero(), Bits(0,0))
        self.assertEqual(Bits(9,255).zero(), Bits(9,0))

    def test_of_signed_int(self):
        self.assertEqual(Bits.of_signed_int(8,127), Bits(8,127))
        self.assertEqual(Bits.of_signed_int(8,-128), Bits(8,0x80))
        self.assertEqual(Bits.of_signed_int(8,-1), Bits(8,255))
        with self.assertRaises(ValueError):
            Bits.of_signed_int(8,128)
        with self.assertRaises(ValueError):
            Bits.of_signed_int(8,-129)

    def test_sub(self):
        self.assertEqual(Bits(0,0) - Bits(0,0), Bits(0,0))
        self.assertEqual(Bits(0,0) - 0, Bits(0,0))
        self.assertEqual(0 - Bits(0,0), Bits(0,0))
        self.assertEqual(Bits(8,5) - 3, Bits(8,2))
        self.assertEqual(5 - Bits(8,3), Bits(8,2))
        self.assertEqual(Bits(8,3) - Bits(8,3), Bits(8,0))
        self.assertEqual(Bits(8,3) - Bits(8,4), Bits(8,255))
        self.assertEqual(Bits(8,3) - Bits(8,255), Bits(8,4))
        self.assertEqual(Bits(8,255) - Bits(8,3), Bits(8,252))
        self.assertEqual(Bits(8,3) - 255, Bits(8,4))
        self.assertEqual(255 - Bits(8,3), Bits(8,252))
        self.assertUnOpExpected(
            lambda b: b - b,
            lambda b: b.zero())
        self.assertUnOpExpected(
            lambda b: b - Bits(b.size(), 2 ** b.size() - 1),
            lambda b: b + 1)
        with self.assertRaises(ValueError):
            Bits(9,3) - Bits(8,3)
        with self.assertRaises(ValueError):
            256 - Bits(8,3)
        with self.assertRaises(ValueError):
            Bits(8,3) - 256
        with self.assertRaises(ValueError):
            (-1) - Bits(8,3)
        with self.assertRaises(ValueError):
            Bits(8,3) - (-1)


    def test_mul(self):
        self.assertEqual(Bits(8,5) * Bits(8,4), Bits(8,20))
        self.assertEqual(5 * Bits(8,4), Bits(8,20))
        self.assertEqual(4 * Bits(8,5), Bits(8,20))
        self.assertEqual(100 * Bits(8,5), Bits(8,0xf4))
        self.assertEqual(Bits(8,5) * 100, Bits(8,0xf4))
        self.assertUnOpExpected(
            lambda b: b * 3 if b.size() >= 3 else b.zero(),
            lambda b: b + b + b if b.size() >= 3 else b.zero())
        with self.assertRaises(ValueError):
            Bits(9,3) * Bits(8,3)
        with self.assertRaises(ValueError):
            256 * Bits(8,3)
        with self.assertRaises(ValueError):
            Bits(8,3) * 256
        with self.assertRaises(ValueError):
            (-1) * Bits(8,3)
        with self.assertRaises(ValueError):
            Bits(8,3) * (-1)

    def test_split(self):
        self.assertEqual(
            Bits(8,0xff).split(1),
            [Bits(1,0x1),
             Bits(1,0x1),
             Bits(1,0x1),
             Bits(1,0x1),
             Bits(1,0x1),
             Bits(1,0x1),
             Bits(1,0x1),
             Bits(1,0x1)])
        self.assertEqual(
            Bits(9,0b100111000).split(3),
            [Bits(3,0b100),
             Bits(3,0b111),
             Bits(3,0x000)])
        self.assertEqual(
            Bits(64,0x0123456789abcdef).split(4),
            [Bits(4,0x0),
             Bits(4,0x1),
             Bits(4,0x2),
             Bits(4,0x3),
             Bits(4,0x4),
             Bits(4,0x5),
             Bits(4,0x6),
             Bits(4,0x7),
             Bits(4,0x8),
             Bits(4,0x9),
             Bits(4,0xa),
             Bits(4,0xb),
             Bits(4,0xc),
             Bits(4,0xd),
             Bits(4,0xe),
             Bits(4,0xf)])
        with self.assertRaises(ValueError):
            Bits(9,3).split("4")
        with self.assertRaises(ValueError):
            Bits(9,3).split(4)


    def test_from_bytes(self):
        self.assertEqual(Bits.from_bytes(b''), Bits(0,0))
        self.assertEqual(Bits.from_bytes(b'', size=64), Bits(64,0))
        self.assertEqual(Bits.from_bytes(b'\x00'), Bits(8,0))
        self.assertEqual(Bits.from_bytes(b'\x01'), Bits(8,1))
        self.assertEqual(Bits.from_bytes(b'\x01', size=16), Bits(16,1))
        self.assertEqual(Bits.from_bytes(b'\x00\x01'), Bits(16,1))
        self.assertEqual(Bits.from_bytes(b'\x01\x00', byteorder='little'), Bits(16,1))
        self.assertEqual(Bits.from_bytes(b'\x01\x00'), Bits(16,0x0100))
        self.assertEqual(Bits.from_bytes(b'\x01\x00', byteorder='little'), Bits(16,0x0001))
        self.assertEqual(Bits.from_bytes(b'\x01\x00', size=32,byteorder='little'), Bits(32,0x0001))

    def test_to_bytes(self):
        self.assertEqual(Bits(0,0).to_bytes() ,b'')
        self.assertEqual(Bits(8,0).to_bytes() ,b'\x00')
        self.assertEqual(Bits(8,1).to_bytes() ,b'\x01')
        self.assertEqual(Bits(16,1).to_bytes(), b'\x00\x01')


    def test_bitewise_or(self):
        self.assertEqual(Bits(0,0) | Bits(0,0), Bits(0,0))
        self.assertEqual(Bits(8,0xff) | Bits(8,0x00), Bits(8,0xff))
        self.assertEqual(Bits(8,0x00) | Bits(8,0xff), Bits(8,0xff))
        self.assertEqual(Bits(8,0x00) | 0xff, Bits(8,0xff))
        self.assertEqual(0xff | Bits(8,0x00), Bits(8,0xff))
        self.assertEqual(Bits(8,0x00) | Bits(8,42), Bits(8,42))
        self.assertUnOpExpected(
            lambda b: b | 0,
            lambda b: b)
        with self.assertRaises(ValueError):
            Bits(15,7) | Bits(16,9)
        with self.assertRaises(ValueError):
            Bits(8,255) | 256
        with self.assertRaises(ValueError):
            256 | Bits(8,9)
        with self.assertRaises(ValueError):
            Bits(8,255) | -1
        with self.assertRaises(ValueError):
            -1 | Bits(8,9)


    def test_bitewise_xor(self):
        self.assertEqual(Bits(0,0) ^ Bits(0,0), Bits(0,0))
        self.assertEqual(Bits(8,0xff) ^ Bits(8,0x00), Bits(8,0xff))
        self.assertEqual(Bits(8,0x00) ^ Bits(8,0xff), Bits(8,0xff))
        self.assertEqual(Bits(8,0x0f) ^ Bits(8,0xff), Bits(8,0xf0))
        self.assertEqual(Bits(8,0xf0) ^ Bits(8,0xff), Bits(8,0x0f))
        self.assertUnOpExpected(
            lambda b: b ^ 0,
            lambda b: b)
        self.assertUnOpExpected(
            lambda b: b ^ ~b,
            lambda b: 2 ** b.size() - 1)
        with self.assertRaises(ValueError):
            Bits(15,7) ^ Bits(16,9)
        with self.assertRaises(ValueError):
            Bits(8,255) ^ 256
        with self.assertRaises(ValueError):
            256 ^ Bits(8,9)
        with self.assertRaises(ValueError):
            Bits(8,255) ^ -1
        with self.assertRaises(ValueError):
            -1 ^ Bits(8,9)

    def test_with_bit(self):
        self.assertEqual(Bits(1,0).with_bit(0,True), Bits(1,1))
        self.assertEqual(Bits(1,1).with_bit(0,False), Bits(1,0))
        self.assertEqual(Bits(8,0b11001100).with_bit(0,True), Bits(8,0b11001101))
        self.assertEqual(Bits(8,0b11001100).with_bit(3,False), Bits(8,0b11000100))
        self.assertEqual(Bits(8,0b11001100).with_bit(7,False), Bits(8,0b01001100))
        with self.assertRaises(ValueError):
            Bits(8,0b11001100).with_bit(8,False)
        with self.assertRaises(ValueError):
            Bits(8,0b11001100).with_bit(-1,False)
