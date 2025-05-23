/-
Copyright (c) 2025 Lean FRO, LLC. or its affiliates. All Rights Reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Kim Morrison
-/
module

prelude
import Init.Grind.CommRing.Basic
import Init.Data.BitVec.Lemmas

namespace Lean.Grind

instance : CommRing (BitVec w) where
  add_assoc := BitVec.add_assoc
  add_comm := BitVec.add_comm
  add_zero := BitVec.add_zero
  neg_add_cancel := BitVec.add_left_neg
  mul_assoc := BitVec.mul_assoc
  mul_comm := BitVec.mul_comm
  mul_one := BitVec.mul_one
  left_distrib _ _ _ := BitVec.mul_add
  zero_mul _ := BitVec.zero_mul
  sub_eq_add_neg := BitVec.sub_eq_add_neg
  pow_zero _ := BitVec.pow_zero
  pow_succ _ _ := BitVec.pow_succ
  ofNat_succ x := BitVec.ofNat_add x 1
  intCast_neg _ := BitVec.ofInt_neg

instance : IsCharP (BitVec w) (2 ^ w) where
  ofNat_eq_zero_iff {x} := by simp [BitVec.ofInt, BitVec.toNat_eq]

end Lean.Grind
