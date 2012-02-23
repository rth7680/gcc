;;- Predicate definitions for the pdp11 for GNU C compiler
;; Copyright (C) 1994-2014 Free Software Foundation, Inc.
;; Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Match CONST_DOUBLE zero for tstd/tstf.
(define_predicate "register_or_const0_operand"
  (ior (match_operand 0 "register_operand")
       (match_test "op == CONST0_RTX (GET_MODE (op))")))

;; Match inline shift counts.
(define_predicate "const_shifthi_operand"
  (and (match_code "const_int")
       (ior (match_test "TARGET_40_PLUS")
	    (match_test "IN_RANGE (INTVAL (op), -4, 4)")
	    (match_test "INTVAL (op) == 8 || INTVAL (op) == -8"))))

(define_predicate "shifthi_operand"
  (ior (and (match_test "TARGET_40_PLUS")
	    (match_operand 0 "general_operand"))
       (match_operand 0 "const_shifthi_operand")))

;; Accept only hard floating point registers
(define_predicate "hard_float_reg_operand"
  (and (match_code "reg")
       (match_test "IN_RANGE (REGNO (op), AC0_REGNUM, AC5_REGNUM)")))

;; Accept anything general_operand accepts,
;; except that hard registers must be FPU registers.
(define_predicate "float_operand"
  (if_then_else (match_test "REG_P (op) && HARD_REGISTER_P (op)")
    (match_operand 0 "hard_float_reg_operand")
    (match_operand 0 "general_operand")))

;; Accept anything nonimmediate_operand accepts,
;; except that hard registers must be FPU registers.
(define_predicate "float_nonimm_operand"
  (if_then_else (match_test "REG_P (op) && HARD_REGISTER_P (op)")
    (match_operand 0 "hard_float_reg_operand")
    (match_operand 0 "nonimmediate_operand")))

;; Return true for any operand that would require an extra word.
;; This includes constants and memories including constants.
(define_predicate "extra_word_operand"
  (ior (match_code "const_int,const_double,const,symbol_ref,label_ref")
       (and (match_code "mem")
	    (match_code "plus" "0"))))
