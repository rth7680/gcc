;;- Machine description for the pdp11 for GNU C compiler
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

(include "predicates.md")
(include "constraints.md")

(define_c_enum "unspecv"
  [
    UNSPECV_BLOCKAGE
    UNSPECV_SETD
    UNSPECV_SETI
    UNSPECV_CLC
    UNSPECV_ADD
    UNSPECV_ADC
    UNSPECV_SUB
    UNSPECV_SBC
    UNSPECV_NEG
    UNSPECV_ASL
    UNSPECV_ASR
    UNSPECV_ROR
    UNSPECV_ROL
    UNSPECV_MOV
    UNSPECV_SXT
  ])

(define_c_enum "unspec"
  [
    UNSPEC_DIVMOD
  ])

(define_constants
  [
   ;; Register numbers
   (R0_REGNUM     	  0)
   (RETVAL_REGNUM     	  0)
   (HARD_FRAME_POINTER_REGNUM  5)
   (STACK_POINTER_REGNUM  6)
   (PC_REGNUM             7)
   (AC0_REGNUM            8)
   (AC3_REGNUM            11)
   (AC4_REGNUM            12)
   (AC5_REGNUM            13)
   ;; The next two are not physical registers but are used for addressing
   ;; arguments.
   (FRAME_POINTER_REGNUM  14)
   (ARG_POINTER_REGNUM    15)
   (FIRST_PSEUDO_REGISTER 16)
   ;; Branch offset limits, as byte offsets from instruction address
   (MIN_BRANCH            -254)
   (MAX_BRANCH            256)
   (MIN_SOB               -126)
   (MAX_SOB               0)])

;; HI is 16 bit
;; QI is 8 bit

;; Integer modes supported on the PDP11, with a mapping from machine mode
;; to mnemonic suffix.  SImode and DImode always are special cases.
(define_mode_iterator I12 [QI HI])
(define_mode_iterator I124 [QI HI SI])
(define_mode_iterator I48 [SI DI])
(define_mode_attr  isfx [(QI "b") (HI "")])

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

;; The only thing that remains to be done then is output
;; the floats in a way the assembler can handle it (and
;; if you're really into it, use a PDP11 float emulation
;; library to do floating point constant folding - but
;; I guess you'll get reasonable results even when not
;; doing this)
;; the last thing to do is fix the UPDATE_CC macro to check
;; for floating point condition codes, and set cc_status
;; properly, also setting the CC_IN_FCCR flag.


;; define attributes
;; currently type is only fpu or arith or unknown, maybe branch later ?
;; default is arith
(define_attr "type" "unknown,arith,fp" (const_string "arith"))

;; Used by the length attribute to automatically calculate adjustments
;; to the length of an insn based on its arguments.
(define_attr "extra_word_ops" "none,op0,op1,op2,op01,op02"
  (const_string "none"))

;; ??? Improve genattrtab to allow if_then_else as operand of plus.
(define_attr "length" ""
  (cond [
	(eq_attr "extra_word_ops" "op0")
	  (if_then_else (match_operand 0 "extra_word_operand" "")
	    (const_int 4)
	    (const_int 2))

	(eq_attr "extra_word_ops" "op1")
	  (if_then_else (match_operand 1 "extra_word_operand" "")
	    (const_int 4)
	    (const_int 2))

	(eq_attr "extra_word_ops" "op2")
	  (if_then_else (match_operand 2 "extra_word_operand" "")
	    (const_int 4)
	    (const_int 2))

	(eq_attr "extra_word_ops" "op01")
	  (if_then_else (match_operand 0 "extra_word_operand" "")
	    (if_then_else (match_operand 1 "extra_word_operand" "")
	      (const_int 6)
	      (const_int 4))
	    (if_then_else (match_operand 1 "extra_word_operand" "")
	      (const_int 4)
	      (const_int 2)))

	(eq_attr "extra_word_ops" "op02")
	  (if_then_else (match_operand 0 "extra_word_operand" "")
	    (if_then_else (match_operand 2 "extra_word_operand" "")
	      (const_int 6)
	      (const_int 4))
	    (if_then_else (match_operand 2 "extra_word_operand" "")
	      (const_int 4)
	      (const_int 2)))
	]
	(const_int 2)))

;; a user's asm statement
(define_asm_attributes
  [(set_attr "type" "unknown")
  ; length for asm is the max length per statement.  That would be
  ; 3 words, for a two-operand instruction with extra word addressing
  ; modes for both operands.
  (set_attr "length" "6")])

;; define function units

;; Prologue and epilogue support.

(define_expand "prologue"
  [(const_int 0)]
  ""
{
  pdp11_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  pdp11_expand_epilogue ();
  DONE;
})

(define_expand "return"
  [(return)]
  "reload_completed && !frame_pointer_needed && pdp11_sp_frame_offset () == 0"
  "")

(define_insn "*rts"
  [(return)]
  ""
  "rts pc")

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")])

(define_insn "setd"
  [(unspec_volatile [(const_int 0)] UNSPECV_SETD)]
  ""
  "setd")

(define_insn "seti"
  [(unspec_volatile [(const_int 0)] UNSPECV_SETI)]
  ""
  "seti")

;; arithmetic - values here immediately when next insn issued
;; or does it mean the number of cycles after this insn was issued?
;; how do I say that fpu insns use cpu also? (pre-interaction phase)

;(define_function_unit "cpu" 1 1 (eq_attr "type" "arith") 0 0)
;(define_function_unit "fpu" 1 1 (eq_attr "type" "fp") 0 0)

;; Compare instructions.

;; currently we only support df floats, which saves us quite some
;; hassle switching the FP mode!
;; we assume that CPU is always in long float mode, and
;; 16 bit integer mode - currently, the prologue for main does this,
;; but maybe we should just set up a NEW crt0 properly,
;; -- and what about signal handling code?
;; (we don't even let sf floats in the register file, so
;; we only should have to worry about truncating and widening
;; when going to memory)

;; abort() call by g++ - must define libfunc for cmp_optab
;; and ucmp_optab for mode SImode, because we don't have that!!!
;; - yet since no libfunc is there, we abort ()


;; compare
(define_insn "*cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "general_operand" "fm,fmF")
		 (match_operand:DF 1 "register_or_const0_operand" "G,a")))]
  "TARGET_FPU"
{
  cc_status.flags = CC_IN_FPU;
  if (which_alternative == 0)
    return "{tstd|tstf} %0\;cfcc";
  else
    return "{cmpd|cmpf} %0, %1\;cfcc";
}
  [(set_attr "extra_word_ops" "op0")])

(define_insn "cmp<mode>"
  [(set (cc0)
	(compare (match_operand:I12 0 "general_operand" "g,g")
		 (match_operand:I12 1 "general_operand" "N,g")))]
  ""
  "@
   tst<isfx> %0
   cmp<isfx> %0,%1"
  [(set_attr "extra_word_ops" "op0,op01")])

;; PDP11/45 manual says that "BIT MEM,REG" is faster than "BIT REG,MEM",
;; even though the result is not stored.
(define_insn "*bit<mode>"
  [(set (cc0)
	(compare (and:I12 (match_operand:I12 0 "general_operand" "g,g")
			  (match_operand:I12 1 "general_operand" "r,g"))
		 (const_int 0)))]
  ""
  "@
   bit<isfx> %0,%1
   bit<isfx> %1,%0"
  [(set_attr "extra_word_ops" "op01")])

(define_expand "doloop_end"
  [(use (match_operand 0 "" ""))        ; loop pseudo
   (use (match_operand 1 "" ""))        ; iterations; zero if unknown
   (use (match_operand 2 "" ""))        ; max iterations
   (use (match_operand 3 "" ""))        ; loop level
   (use (match_operand 4 "" ""))]       ; label
  "TARGET_40_PLUS"
{
  emit_jump_insn (gen_sob (operands[0], operands[4]));
  DONE;
})

(define_insn "sob"
  [(set (pc)
	(if_then_else
	 (ne (plus:HI (match_operand:HI 0 "nonimmediate_operand" "+r,!m")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:HI (match_dup 0)
		 (const_int -1)))]
  "TARGET_40_PLUS"
{
  switch (get_attr_length (insn))
    {
    case 2:
      return "sob %0, %l1";
    /* Emulate sob, modulo the clobbering of the flags.  */
    case 4:
      return "dec %0\;bne %l1";
    case 8:
      return "dec %0\;beq .+6\;jmp %l1";
    default:
      gcc_unreachable ();
    }
}
  [(set (attr "length")
	(cond [(ior (lt (minus (match_dup 1) (pc)) (const_int MIN_BRANCH))
		    (gt (minus (match_dup 1) (pc)) (const_int MAX_BRANCH)))
	         (const_int 8)
	       (ior (lt (minus (match_dup 1) (pc)) (const_int MIN_SOB))
		    (gt (minus (match_dup 1) (pc)) (const_int MAX_SOB)))
		 (const_int 4)
	       (match_operand 0 "memory_operand" "")
		 (const_int 4)
	      ]
	      (const_int 2)))])

;; These control RTL generation for conditional jump insns
;; and match them for register allocation.

(define_expand "cbranchdf4"
  [(set (cc0)
        (compare (match_operand:DF 1 "general_operand")
		 (match_operand:DF 2 "register_or_const0_operand")))
   (set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(cc0) (const_int 0)])
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  "TARGET_FPU"
  "")

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "ordered_comparison_operator"
	    [(match_operand:I124 1 "general_operand")
	     (match_operand:I124 2 "general_operand")])
          (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
{
  pdp11_expand_branch (GET_CODE (operands[0]), operands[1],
		       operands[2], operands[3]);
  DONE;
})

;; problem with too short jump distance! we need an assembler which can
;; make this valid for all jump distances!
;; e.g. gas!

;; these must be changed to check for CC_IN_FCCR if float is to be
;; enabled

(define_insn "*branch"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(cc0) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
{
  return output_jump (GET_CODE (operands[0]), 0, get_attr_length (insn));
}
  [(set (attr "length")
	(if_then_else (ior (lt (minus (match_dup 1) (pc))
			       (const_int MIN_BRANCH))
			   (gt (minus (match_dup 1) (pc))
			       (const_int MAX_BRANCH)))
	  (const_int 6)
	  (const_int 2)))])

;; These match inverted jump insns for register allocation.

(define_insn "*branch_inverted"
  [(set (pc)
	(if_then_else (match_operator 0 "ordered_comparison_operator"
		       [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
{
  return output_jump (GET_CODE (operands[0]), 1, get_attr_length (insn));
}
  [(set (attr "length")
	(if_then_else (ior (lt (minus (match_dup 1) (pc))
			       (const_int MIN_BRANCH))
			   (gt (minus (match_dup 1) (pc))
			       (const_int MAX_BRANCH)))
	  (const_int 6)
	  (const_int 2)))])

;; Move instructions

(define_insn_and_split "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=g")
	(match_operand:DI 1 "general_operand" "g"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  split_move_multiple (operands);
  DONE;
})

(define_insn_and_split "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=g")
	(match_operand:SI 1 "general_operand" "g"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  split_move_multiple (operands);
  DONE;
})

(define_insn "mov<mode>"
  [(set (match_operand:I12 0 "nonimmediate_operand" "=rm,rm")
	(match_operand:I12 1 "general_operand"      "  N, g"))]
  ""
  "@
   clr<isfx> %0
   mov<isfx> %1, %0"
  [(set_attr "extra_word_ops" "op0,op01")])

(define_insn "mov<mode>_flags_out"
  [(set (match_operand:I12 0 "nonimmediate_operand" "=rm,rm")
	(unspec_volatile:I12 [(match_operand:I12 1 "general_operand" "N,g")]
			     UNSPECV_MOV))]
  "reload_completed"
  "@
   clr<isfx> %0
   mov<isfx> %1, %0"
  [(set_attr "extra_word_ops" "op0,op01")])

(define_insn_and_split "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=a,fm,rm")
        (match_operand:DF 1 "float_operand"       "fmF, a, g"))]
  "TARGET_FPU"
  "@
   ldd %1, %0
   std %1, %0
   #"
  "&& reload_completed
   && !hard_float_reg_operand (operands[0], DFmode)
   && !hard_float_reg_operand (operands[1], DFmode)"
  [(const_int 0)]
{
  split_move_multiple (operands);
}
  [(set_attr "extra_word_ops" "op01")])

(define_insn_and_split "movsf"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=a,fm,rm")
        (match_operand:SF 1 "float_operand"       "fmF, a,g"))]
  "TARGET_FPU"
  "@
   {ldcfd|movof} %1, %0
   {stcdf|movfo} %1, %0
   #"
  "&& reload_completed
   && !hard_float_reg_operand (operands[0], SFmode)
   && !hard_float_reg_operand (operands[1], SFmode)"
  [(const_int 0)]
{
  split_move_multiple (operands);
}
  [(set_attr "extra_word_ops" "op01")])

;; maybe fiddle a bit with move_ratio, then
;; let constraints only accept a register ...

(define_expand "movmemhi"
  [(parallel [(set (match_operand:BLK 0 "memory_operand" "")
		   (match_operand:BLK 1 "memory_operand" ""))
	      (use (match_operand:HI 2 "nonmemory_operand" ""))
	      (use (match_operand:HI 3 "const_int_operand" ""))
	      (clobber (match_scratch:HI 4 "=&r,X"))
	      (clobber (match_dup 5))
	      (clobber (match_dup 6))
	      (clobber (match_dup 2))])]
  "TARGET_BCOPY_BUILTIN"
{
  operands[5] = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  operands[0] = replace_equiv_address (operands[0], operands[5]);
  operands[6] = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
  operands[1] = replace_equiv_address (operands[1], operands[6]);
})

(define_insn_and_split "*movmemhi"
  [(set (mem:BLK (match_operand:HI 0 "register_operand" "r,r"))
	(mem:BLK (match_operand:HI 1 "register_operand" "r,r")))
   (use (match_operand:HI 2 "nonmemory_operand" "n,r"))
   (use (match_operand:HI 3 "const_int_operand" "n,n"))
   (clobber (match_scratch:HI 4 "=&r,X"))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_dup 2))]
  "TARGET_BCOPY_BUILTIN"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  split_block_move (operands);
  DONE;
})

;;- truncation instructions

(define_insn  "truncdfsf2"
  [(set (match_operand:SF 0 "float_nonimm_operand" "=f,m")
	(float_truncate:SF
	  (match_operand:DF 1 "register_operand"   " f,a")))]
  "TARGET_FPU"
  "@
   /* nothing */
   {stcdf|movfo} %1, %0"
  [(set_attr "extra_word_ops" "op0")])

;;- zero extension instructions
;; Without these patterns, optabs.c will try to use shifts, not ands.

(define_insn_and_split "zero_extendqihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,o")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "0,0")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  if (REG_P (operands[0]))
    emit_insn (gen_andhi3 (operands[0], operands[0], GEN_INT (0xff)));
  else
    emit_move_insn (adjust_address (operands[0], QImode, 1), const0_rtx);
  DONE;
})

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "")))]
  ""
{
  emit_move_insn (gen_lowpart (HImode, operands[0]), operands[1]);
  emit_move_insn (gen_highpart (HImode, operands[0]), const0_rtx);
  DONE;
})

;;- sign extension instructions

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f,a")
	(float_extend:DF
	  (match_operand:SF 1 "float_operand"  " f,m")))]
  "TARGET_FPU"
  "@
   /* nothing */
   {ldcfd|movof} %1, %0"
  [(set_attr "extra_word_ops" "op1")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "movb %1, %0"
  [(set_attr "extra_word_ops" "op1")])

(define_insn "*extendqihi_flags_out"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(unspec_volatile:HI [(match_operand:QI 1 "general_operand" "g")]
			    UNSPECV_MOV))]
  "reload_completed"
  "movb %1, %0"
  [(set_attr "extra_word_ops" "op1")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "")))]
  "")

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "")))]
  "")

(define_insn "*extendqisi2_40p"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "g")))]
  "TARGET_40_PLUS"
  "#")

(define_insn "*extendhisi2_40p"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=ro<")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g")))]
  "TARGET_40_PLUS"
  "#")

(define_insn "*extendqisi2_10"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "g")))]
  "!TARGET_40_PLUS"
  "#")

(define_insn "*extendhisi2_10"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g")))]
  "!TARGET_40_PLUS"
  "#")

;; Split both QI and HImode extensions to SImode, for 40+.
(define_split
  [(set (match_operand:SI 0 "nonimmediate_operand")
	(sign_extend:SI (match_operand 1 "general_operand")))]
  "TARGET_40_PLUS && reload_completed"
  [(const_int 0)]
{
  rtx exops[2][2];
  pdp11_expand_operands (operands, exops, 1, NULL, little);

  if (rtx_equal_p (exops[0][0], operands[1]))
    {
      emit_insn (gen_cmphi (exops[0][0], const0_rtx));
      emit_insn (gen_sxt_cc0_in (exops[1][0]));
    }
  else
    {
      emit_insn (gen_movhi_flags_out (exops[0][0], operands[1]));
      emit_insn (gen_sxt_flags_in (exops[1][0]));
    }
  DONE;
})

;; Split both QI and HImode extensions to SImode, for non-40+
(define_split
  [(set (match_operand:SI 0 "register_operand")
	(sign_extend:SI (match_operand 1 "general_operand")))]
  "!TARGET_40_PLUS && reload_completed"
  [(const_int 0)]
{
  rtx hi = gen_highpart (HImode, operands[0]);
  rtx lo = gen_lowpart (HImode, operands[0]);

  if (GET_MODE (operands[1]) == QImode)
    emit_insn (gen_extendqihi2 (lo, operands[1]));
  else
    emit_move_insn (lo, operands[1]);
  emit_move_insn (hi, const0_rtx);
  emit_insn (gen_aslhi_carry_out (lo));
  emit_insn (gen_subhi_carry_in (hi));
  emit_insn (gen_rorhi_carry_in (lo));
  DONE;
})

(define_insn "sxt_flags_in"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(unspec_volatile:HI [(const_int 0)] UNSPECV_SXT))]
  "TARGET_40_PLUS && reload_completed"
  "sxt %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn "sxt_cc0_in"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(if_then_else:HI (lt (cc0) (const_int 0))
	  (const_int -1)
	  (const_int 0)))]
  "TARGET_40_PLUS"
  "sxt %0"
  [(set_attr "extra_word_ops" "op0")])

;; make float to int and vice versa
;; using the cc_status.flag field we could probably cut down
;; on seti and setl
;; assume that we are normally in double and integer mode -
;; what do pdp library routines do to fpu mode ?

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=a,a")
	(float:DF (match_operand:SI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_FPU"
  "@
   #
   setl\;{ldcld|movif} %1, %0\;seti"
  [(set (attr "length")
	(if_then_else (match_operand 1 "extra_word_operand")
	  (const_int 8)
	  (const_int 6)))])

(define_split
  [(set (match_operand:DF 0 "register_operand")
	(float:DF (match_operand:SI 1 "register_operand" )))]
  "TARGET_FPU && reload_completed"
  [(set (match_dup 0) (float:DF (match_dup 1)))]
{
  rtx x;
  x = gen_rtx_PRE_DEC (HImode, stack_pointer_rtx);
  x = gen_rtx_MEM (SImode, x);
  emit_move_insn (x, operands[1]);

  x = gen_rtx_POST_INC (HImode, stack_pointer_rtx);
  operands[1] = gen_rtx_MEM (SImode, x);
})

(define_insn "floathidf2"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(float:DF (match_operand:HI 1 "general_operand" "g")))]
  "TARGET_FPU"
  "{ldcid|movif} %1, %0"
  [(set_attr "extra_word_ops" "op1")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,m")
	(fix:SI (match_operand:DF 1 "register_operand" "a,a")))]
  "TARGET_FPU"
  "@
   #
   setl\;{stcdl|movfi} %1, %0\;seti"
  [(set (attr "length")
	(if_then_else (match_operand 0 "extra_word_operand")
	  (const_int 8)
	  (const_int 6)))])

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(fix:SI (match_operand:DF 1 "register_operand" )))]
  "TARGET_FPU && reload_completed"
  [(set (match_dup 2) (fix:SI (match_dup 1)))
   (set (match_dup 0) (match_dup 3))]
{
  rtx x;
  x = gen_rtx_PRE_DEC (HImode, stack_pointer_rtx);
  operands[2] = gen_rtx_MEM (SImode, x);

  x = gen_rtx_POST_INC (HImode, stack_pointer_rtx);
  operands[3] = gen_rtx_MEM (SImode, x);
})

(define_insn "fix_truncdfhi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(fix:HI (match_operand:DF 1 "register_operand" "a")))]
  "TARGET_FPU"
  "{stcdi|movfi} %1, %0"
  [(set_attr "extra_word_ops" "op0")])


;;- arithmetic instructions
;;- add instructions

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(plus:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "general_operand" "fmF")))]
  "TARGET_FPU"
  "{addd|addf} %2, %0"
  [(set_attr "extra_word_ops" "op2")])

(define_insn_and_split "add<mode>3"
  [(set (match_operand:I48 0 "nonimmediate_operand" "=&ro")
	(plus:I48 (match_operand:I48 1 "general_operand" "%0")
		  (match_operand:I48 2 "general_operand" "ron")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx exops[4][2];
  int i, j;

  pdp11_expand_operands (operands + 1, exops, 2, NULL, either);

  for (i = 0; i < (<MODE>mode == SImode ? 2 : 4); ++i)
    {
      if (exops[i][1] == const0_rtx)
	continue;
      emit_insn (gen_addhi_carry_out (exops[i][0], exops[i][1]));

      /* Note that there is no two-operand add-with-carry insn.
	 Thus the carry bit must be propagated all the way up
	 for each input word for which we perform an add.  */
      for (j = i - 1; j >= 0; --j)
	emit_insn (gen_addhi_carry_in (exops[j][0]));
    }
  DONE;
})

(define_insn "addhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand"    "=rm,rm")
	(plus:HI (match_operand:HI 1 "general_operand" "%0,0")
		 (match_operand:HI 2 "general_operand" "LM,g")))]
  ""
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL(operands[2]) == 1)
	return "inc %0";
      else if (INTVAL(operands[2]) == -1)
        return "dec %0";
    }

  return "add %2, %0";
}
  [(set_attr "extra_word_ops" "op0,op02")])

;; Insns for add-with-carry-out and add-with-carry-in.
;; This should use a flags register, but until everything clobbers the
;; flags register properly, unspec_volatile is as good as we can get.

;; Note that INC/DEC do not set the carry flag.
(define_insn "addhi_carry_out"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI
	  [(match_dup 0)
	   (match_operand:HI 1 "general_operand" "g")]
	  UNSPECV_ADD))]
  "reload_completed"
  "add %1, %0"
  [(set_attr "extra_word_ops" "op01")])

(define_insn "addhi_carry_in"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI [(match_dup 0)] UNSPECV_ADC))]
  "reload_completed"
  "adc %0"
  [(set_attr "extra_word_ops" "op0")])

;;- subtract instructions
;; we don't have to care for constant second
;; args, since they are canonical plus:xx now!
;; also for minus:DF ??

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(minus:DF (match_operand:DF 1 "register_operand" "0")
		  (match_operand:DF 2 "general_operand" "fm")))]
  "TARGET_FPU"
  "{subd|subf} %2, %0"
  [(set_attr "extra_word_ops" "op2")])

(define_insn_and_split "sub<mode>3"
  [(set (match_operand:I48 0 "nonimmediate_operand" "=&ro")
	(minus:I48 (match_operand:I48 1 "general_operand" "0")
		   (match_operand:I48 2 "general_operand" "ron")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx exops[4][2];
  int i, j;

  pdp11_expand_operands (operands + 1, exops, 2, NULL, either);

  for (i = 0; i < (<MODE>mode == SImode ? 2 : 4); ++i)
    {
      emit_insn (gen_subhi_carry_out (exops[i][0], exops[i][1]));

      /* Note that there is no two-operand sub-with-borrow insn.
	 Thus the carry bit must be propagated all the way up
	 for each input word for which we perform a subtract.  */
      for (j = i - 1; j >= 0; --j)
	emit_insn (gen_subhi_carry_in (exops[j][0]));
    }
  DONE;
})

;; The second alternative helps reduce register pressure in some instances.
(define_insn "subhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm,?r")
	(minus:HI (match_operand:HI 1 "general_operand" "0,g")
		  (match_operand:HI 2 "general_operand" "g,0")))]
  ""
  "@
   sub %2, %0
   #"
  [(set_attr "extra_word_ops" "op02")])

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(minus:HI (match_operand:HI 1 "general_operand" "")
		  (match_dup 0)))]
  "reload_completed"
  [(set (match_dup 0) (neg:HI (match_dup 0)))
   (set (match_dup 0) (plus:HI (match_dup 0) (match_dup 1)))]
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      emit_move_insn (operands[0], const0_rtx);
      DONE;
    }
})

(define_insn "subhi_carry_out"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI
	  [(match_dup 0)
	   (match_operand:HI 1 "general_operand" "g")]
	  UNSPECV_SUB))]
  "reload_completed"
  "sub %1, %0"
  [(set_attr "extra_word_ops" "op01")])

(define_insn "subhi_carry_in"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI [(match_dup 0)] UNSPECV_SBC))]
  "reload_completed"
  "sbc %0"
  [(set_attr "extra_word_ops" "op0")])


;;;;- and instructions
;; Bit-and on the pdp (like on the VAX) is done with a clear-bits insn.

(define_expand "and<mode>3"
  [(set (match_operand:I12 0 "nonimmediate_operand" "")
	(and:I12 (not:I12 (match_operand:I12 1 "general_operand" ""))
		 (match_operand:I12 2 "general_operand" "")))]
  ""
{
  rtx op1 = operands[1];

  /* If there is a constant argument, complement that one.
     Similarly, if one of the inputs is the same as the output,
     complement the other input.  */
  if ((CONST_INT_P (operands[2]) && ! CONST_INT_P (op1))
      || rtx_equal_p (operands[0], operands[1]))
    {
      operands[1] = operands[2];
      operands[2] = op1;
      op1 = operands[1];
    }

  if (CONST_INT_P (op1))
    operands[1] = GEN_INT (~INTVAL (op1));
  else
    operands[1] = expand_unop (<MODE>mode, one_cmpl_optab, op1, 0, 1);
})

(define_insn "bic<mode>3"
  [(set (match_operand:I12 0 "nonimmediate_operand" "=rm")
	(and:I12
	  (not:I12 (match_operand:I12 2 "general_operand" "g"))
	  (match_operand:I12 1 "general_operand" "0")))]
  ""
  "bic<isfx> %2, %0"
  [(set_attr "extra_word_ops" "op02")])

;;- Bit set (inclusive or) instructions
(define_insn "ior<mode>3"
  [(set (match_operand:I12 0 "nonimmediate_operand" "=rm")
	(ior:I12 (match_operand:I12 1 "general_operand" "%0")
		 (match_operand:I12 2 "general_operand" "g")))]
  ""
  "bis<isfx> %2, %0"
  [(set_attr "extra_word_ops" "op02")])

;;- xor instructions
(define_expand "xorhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(xor:HI (match_operand:HI 1 "general_operand" "")
		(match_operand:HI 2 "general_operand" "")))]
  ""
{
  if (!TARGET_40_PLUS)
    {
      rtx op1, op2, t1, t2;

      op1 = force_reg (HImode, operands[1]);
      op2 = force_reg (HImode, operands[2]);
      t1 = gen_reg_rtx (HImode);
      t2 = gen_reg_rtx (HImode);

      emit_insn (gen_bichi3 (t1, op1, op2));
      emit_insn (gen_bichi3 (t2, op2, op1));
      emit_insn (gen_iorhi3 (operands[0], t1, t2));
      DONE;
    }
})

(define_insn "*xorhi3"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(xor:HI (match_operand:HI 1 "general_operand" "%0")
		(match_operand:HI 2 "general_operand" "r")))]
  "TARGET_40_PLUS"
  "xor %2, %0"
  [(set_attr "extra_word_ops" "op0")])

;;- one complement instructions

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:I12 0 "nonimmediate_operand" "=rm")
        (not:I12 (match_operand:I12 1 "general_operand" "0")))]
  ""
  "com<isfx> %0"
  [(set_attr "extra_word_ops" "op0")])


;;- arithmetic shift instructions

(define_expand "ashlhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(ashift:HI (match_operand:HI 1 "register_operand" "")
		   (match_operand:HI 2 "shifthi_operand" "")))]
  "")

(define_expand "ashrhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(ashift:HI (match_operand:HI 1 "register_operand" "")
		   (match_operand:HI 2 "shifthi_operand" "")))]
  ""
{
  /* Arithmetic right shift on the pdp works by negating the shift count.  */
  emit_insn (gen_ashlhi3 (operands[0], operands[1],
			  negate_rtx (HImode, operands[2])));
  DONE;
})

(define_insn "*aslhi3_one"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(ashift:HI (match_operand:HI 1 "nonimmediate_operand" "0")
		   (const_int 1)))]
  ""
  "asl %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn "*asrhi3_one"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(ashift:HI (match_operand:HI 1 "general_operand" "0")
		   (const_int -1)))]
  ""
  "asr %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn "*ashlhi3_40p"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ashift:HI (match_operand:HI 1 "register_operand" "0")
		   (match_operand:HI 2 "general_operand" "g")))]
  "TARGET_40_PLUS"
  "ash %2,%0"
  [(set_attr "extra_word_ops" "op2")])

(define_insn "*ashlhi3_small"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ashift:HI (match_operand:HI 1 "register_operand" "0")
		   (match_operand:HI 2 "const_shifthi_operand" "n")))]
  "!TARGET_40_PLUS"
  "#")

(define_split
  [(set (match_operand:HI 0 "register_operand" "")
	(ashift:HI (match_dup 0)
		   (match_operand:HI 1 "const_shifthi_operand" "")))]
  "reload_completed"
  [(const_int 0)]
{
  int i, n = INTVAL (operands[1]);
  rtx shift, op = operands[0];

  if (n == 0)
    emit_note (NOTE_INSN_DELETED);
  else if (n == 1 || n == -1)
    FAIL;
  else if (n == 8 || n == -8)
    {
      emit_insn (gen_bswaphi2 (op, op));
      if (n == -8)
	emit_insn (gen_extendqihi2 (op, gen_lowpart (QImode, op)));
      else
        emit_insn (gen_andhi3 (op, op, gen_int_mode (0xff00, HImode)));
    }
  else if (n >= 16)
    emit_move_insn (op, const0_rtx);
  else if (TARGET_40_PLUS && n <= -15)
    {
      emit_insn (gen_cmphi (op, const0_rtx));
      emit_insn (gen_sxt_cc0_in (op));
    }
  else if (n <= -15)
    {
      emit_insn (gen_andhi3 (op, op, gen_int_mode (0x8000, HImode)));
      emit_insn (gen_aslhi_carry_out (op));
      emit_insn (gen_subhi_carry_in (op));
    }
  else if (n == 15 && (!TARGET_40_PLUS || optimize_insn_for_speed_p ()))
    {
      /* According to pdp11/45 instruction timing, this sequence
	 is 5.56us vs 7.28us for "ash $15,r".  Use when optimizing
	 for speed, even if ash is available.  */
      emit_insn (gen_andhi3 (op, op, const1_rtx));
      emit_insn (gen_asrhi_carry_out (op));
      emit_insn (gen_rorhi_carry_in (op));
    }
  else if (TARGET_40_PLUS)
    FAIL;
  else
    {
      /* Arithmetic right shift on the pdp works by negating the shift
	 count; support that as input from other shift ops.  */
      shift = const1_rtx;
      if (n < 0)
	{
	  shift = constm1_rtx;
	  n = -n;
	}
      for (i = 0; i < n; ++i)
	emit_insn (gen_ashlhi3 (op, op, shift));
    }
  DONE;
})

(define_expand "lshrhi3"
  [(set (match_operand:HI 0 "register_operand" "")
	(lshiftrt:HI (match_operand:HI 1 "register_operand" "")
		     (match_operand:HI 2 "shifthi_operand" "")))]
  ""
{
  if (!CONST_INT_P (operands[2]))
    {
      rtx tmp;

      /* For variable shift, convert to an arithmetic shift of
	 a zero-extended quantity.  */
      gcc_checking_assert (TARGET_40_PLUS);
      tmp = convert_to_mode (SImode, operands[1], 1);
      emit_insn (gen_ashrsi3 (tmp, tmp, operands[2]));
      emit_move_insn (operands[0], gen_lowpart (HImode, tmp));
      DONE;
    }
})

(define_insn_and_split "*lshrhi3_const"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lshiftrt:HI (match_operand:HI 1 "register_operand" "0")
		     (match_operand:HI 2 "const_shifthi_operand" "n")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx op = operands[0];
  int i, n = INTVAL (operands[2]) & 15;

  if (n == 0)
    emit_note (NOTE_INSN_DELETED);
  else if (n >= 16)
    emit_move_insn (op, const0_rtx);
  else if (n == 8)
    {
      emit_insn (gen_bswaphi2 (op, op));
      emit_insn (gen_andhi3 (op, op, GEN_INT (0xff)));
    }
  else if (n == 15)
    {
      emit_insn (gen_andhi3 (op, op, gen_int_mode (0x8000, HImode)));
      emit_insn (gen_aslhi_carry_out (op));
      emit_insn (gen_rolhi_carry_in (op));
    }
  else if (TARGET_40_PLUS && n >= 2)
    {
      emit_insn (gen_ashlhi3 (op, op, GEN_INT (-n)));
      emit_insn (gen_bichi3 (op, op, GEN_INT (~(0xffff >> n))));
    }
  else
    {
      emit_insn (gen_clear_carry_out ());
      emit_insn (gen_rorhi_carry_in (op));
      for (i = 1; i < n; ++i)
	emit_insn (gen_ashlhi3 (op, op, constm1_rtx));
    }
  DONE;
})

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:HI 2 "general_operand" "g")))]
  "TARGET_40_PLUS"
  "ashc %2,%0"
  [(set_attr "extra_word_ops" "op2")])

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:HI 2 "general_operand" "")))]
  "TARGET_40_PLUS"
{
  operands[2] = negate_rtx (HImode, operands[2]);
})

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_dup 0)
		   (match_operand:HI 1 "const_int_operand" "")))]
  "TARGET_40_PLUS && reload_completed
   && (INTVAL (operands[1]) >= 16 || INTVAL (operands[1]) <= -16)"
  [(const_int 0)]
{
  rtx hi = gen_highpart (HImode, operands[0]);
  rtx lo = gen_lowpart (HImode, operands[0]);
  int c = INTVAL (operands[1]);

  if (c > 0)
    {
      emit_move_insn (hi, lo);
      emit_move_insn (lo, const0_rtx);
      if (c > 16)
        emit_insn (gen_ashlhi3 (hi, hi, GEN_INT (c - 16)));
    }
  else if (c <= -31)
    {
      emit_insn (gen_ashlhi3 (hi, hi, GEN_INT (-15)));
      emit_move_insn (lo, hi);
    }
  else
    {
      emit_move_insn (lo, hi);
      emit_insn (gen_cmphi (lo, const0_rtx));
      emit_insn (gen_sxt_cc0_in (hi));
      if (c < -16)
	emit_insn (gen_ashlhi3 (lo, lo, GEN_INT (c + 16)));
    }
  DONE;
})

(define_insn_and_split "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "0,0")
		     (match_operand:HI 2 "general_operand" "n,g")))
   (clobber (match_scratch:HI 3 "=X,&r"))]
  "TARGET_40_PLUS"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx op = operands[0];
  if (CONST_INT_P (operands[2]))
    {
      int n = INTVAL (operands[2]);
      rtx hi = gen_highpart (HImode, op);
      rtx lo = gen_lowpart (HImode, op);

      if (n == 0)
	emit_note (NOTE_INSN_DELETED);
      else if (n == 1)
	{
	  emit_insn (gen_clear_carry_out ());
	  emit_insn (gen_rorhi_carry_in (hi));
	  emit_insn (gen_rorhi_carry_in (hi));
	}
      else if (n >= 16)
	{
	  emit_move_insn (lo, hi);
	  emit_move_insn (hi, const0_rtx);
	  if (n > 16)
	    {
	      n -= 16;
	      emit_insn (gen_lshrhi3 (lo, lo, GEN_INT (n)));
	    }
	}
      else
	{
	  emit_insn (gen_ashlsi3 (op, op, GEN_INT (-n)));
	  emit_insn (gen_bichi3 (hi, hi, GEN_INT (~(0xffff >> n))));
	}
    }
  else
    {
      rtx shift, over, x;

      shift = operands[3];
      emit_move_insn (shift, operands[2]);

      /* Skip the whole thing if the count is zero.  */
      over = gen_label_rtx ();
      x = gen_rtx_EQ (VOIDmode, shift, const0_rtx);
      emit_jump_insn (gen_cbranchhi4 (x, shift, const0_rtx, over));

      /* Shift right logical by one bit.  */
      emit_insn (gen_clear_carry_out ());
      emit_insn (gen_rorhi_carry_in (gen_highpart (HImode, op)));
      emit_insn (gen_rorhi_carry_in (gen_lowpart (HImode, op)));

      /* Now that the sign bit must be clear, shift right arithmetic
	 by n-1, which is left shift by -n+1.  */
      emit_insn (gen_neghi2 (shift, shift));
      emit_insn (gen_addhi3 (shift, shift, const1_rtx));
      emit_insn (gen_ashlsi3 (op, op, shift));

      emit_label (over);
    }
  DONE;
})

(define_insn_and_split "*rothi_1"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=ro")
	(rotate:HI (match_operand:HI 1 "nonimmediate_operand" "0")
		   (const_int 1)))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_aslhi_carry_out (operands[0]));
  emit_insn (gen_addhi_carry_in (operands[0]));
  DONE;
})

(define_insn "bswaphi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(rotate:HI (match_operand:HI 1 "nonimmediate_operand" "0")
		   (const_int 8)))]
  ""
  "swab %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn_and_split "*rothi_9"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=rm")
	(rotate:HI (match_operand:HI 1 "nonimmediate_operand" "0")
		   (const_int 9)))]
  ""
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (rotate:HI (match_dup 0) (const_int 8)))
   (set (match_dup 0) (rotate:HI (match_dup 0) (const_int 1)))]
  "")

(define_insn "clear_carry_out"
  [(unspec_volatile [(const_int 0)] UNSPECV_CLC)]
  "reload_completed"
  "clc")

(define_insn "aslhi_carry_out"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI [(match_dup 0)] UNSPECV_ASL))]
  "reload_completed"
  "asl %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn "asrhi_carry_out"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI [(match_dup 0)] UNSPECV_ASR))]
  ""
  "asr %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn "rorhi_carry_in"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI [(match_dup 0)] UNSPECV_ROR))]
  "reload_completed"
  "ror %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn "rolhi_carry_in"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI [(match_dup 0)] UNSPECV_ROL))]
  "reload_completed"
  "rol %0"
  [(set_attr "extra_word_ops" "op0")])

;; absolute

(define_insn "absdf2"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=fm")
	(abs:DF (match_operand:DF 1 "general_operand" "0")))]
  "TARGET_FPU"
  "{absd|absf} %0"
  [(set_attr "extra_word_ops" "op0")])


;; negate insns

(define_insn "negdf2"
  [(set (match_operand:DF 0 "float_nonimm_operand" "=fm")
	(neg:DF (match_operand:DF 1 "register_operand" "0")))]
  "TARGET_FPU"
  "{negd|negf} %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn_and_split "neg<mode>2"
  [(set (match_operand:I48 0 "nonimmediate_operand" "=r<o")
	(neg:I48 (match_operand:I48 1 "general_operand" "0")))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  rtx exops[4][2];
  int i, n = (<MODE>mode == SImode ? 2 : 4);

  pdp11_expand_operands (operands, exops, 1, NULL, little);

  emit_insn (gen_neghi_carry_out (exops[0][0]));
  for (i = 1; i < n; ++i)
    {
      emit_insn (gen_subhi_carry_in (exops[i][0]));
      emit_insn (gen_neghi_carry_out (exops[i][0]));
    }
  DONE;
})

(define_insn "neg<mode>2"
  [(set (match_operand:I12 0 "nonimmediate_operand" "=rm")
	(neg:I12 (match_operand:I12 1 "general_operand" "0")))]
  ""
  "neg<isfx> %0"
  [(set_attr "extra_word_ops" "op0")])

(define_insn "neghi_carry_out"
  [(set (match_operand:HI 0 "nonimmediate_operand" "+rm")
	(unspec_volatile:HI [(match_dup 0)] UNSPECV_NEG))]
  "reload_completed"
  "neg %0"
  [(set_attr "extra_word_ops" "op0")])

;; Unconditional and other jump instructions
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
{
  if (get_attr_length (insn) == 2)
    return "br %l0";
  return "jmp %l0";
}
  [(set (attr "length") (if_then_else (ior (lt (minus (match_dup 0)
						      (pc))
					       (const_int MIN_BRANCH))
					   (gt (minus (match_dup 0)
						      (pc))
					       (const_int MAX_BRANCH)))
				      (const_int 4)
				      (const_int 2)))])

(define_insn ""
  [(set (pc)
    (label_ref (match_operand 0 "" "")))
   (clobber (const_int 1))]
  ""
  "jmp %l0"
  [(set_attr "length" "4")])

(define_insn "tablejump"
  [(set (pc) (match_operand:HI 0 "general_operand" "r,m"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "@
  jmp (%0)
  jmp %@%0"
  [(set_attr "extra_word_ops" "op0")])

;; indirect jump - let's be conservative!
;; allow only register_operand, even though we could also
;; allow labels etc.

(define_insn "indirect_jump"
  [(set (pc) (match_operand:HI 0 "register_operand" "r"))]
  ""
  "jmp (%0)")

;;- jump to subroutine

(define_insn "call"
  [(call (match_operand:HI 0 "memory_operand" "m")
	 (match_operand:HI 1 "" ""))]
  ""
  "jsr pc, %0"
  [(set_attr "extra_word_ops" "op0")])

;;- jump to subroutine
(define_insn "call_value"
  [(set (match_operand 0 "" "")
	(call (match_operand:HI 1 "memory_operand" "m")
	      (match_operand:HI 2 "" "")))]
  ""
  "jsr pc, %1"
  [(set_attr "extra_word_ops" "op1")])

;;- nop instruction
(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")


;;- multiply

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(mult:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "float_operand" "fmF")))]
  "TARGET_FPU"
  "{muld|mulf} %2, %0"
  [(set_attr "extra_word_ops" "op2")])

;; 16 bit result multiply.
;; We force the use of odd numbered registers, so that we don't need
;; extra clobbers.

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(mult:HI (match_operand:HI 1 "register_operand" "%0")
		 (match_operand:HI 2 "general_operand" "g")))]
  "TARGET_40_PLUS"
  "mul %2, %0"
  [(set_attr "extra_word_ops" "op2")])

;; 32 bit result multiply.
;; Getting the registers matched up is tricky.  If we use matching
;; constraints, reload will put the HImode input into the odd numbered
;; register (i.e. the low half of match with the SImode), which produces
;; incorrect results.

(define_insn_and_split "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI
	  (match_operand:HI 1 "general_operand" "g")
	  (match_operand:HI 2 "general_operand" "g")))]
  "TARGET_40_PLUS"
{
  if (REG_P (operands[1]) && REGNO (operands[0]) == REGNO (operands[1]))
    return "mul %2, %0";
  else
    return "#";
}
  "&& reload_completed
   && !(REG_P (operands[1]) && REGNO (operands[0]) == REGNO (operands[1]))"
  [(set (match_dup 0) (mult:SI (match_dup 1) (match_dup 2)))]
{
  unsigned d = REGNO (operands[0]);
  rtx dhi = gen_rtx_REG (HImode, d);

  if (REG_P (operands[2]) && REGNO (operands[2]) == d)
    {
      dhi = operands[2];
      operands[2] = operands[1];
      operands[1] = dhi;
    }
  else if (!refers_to_regno_p (d, d, operands[2], NULL))
    {
      emit_move_insn (dhi, operands[1]);
      operands[1] = dhi;
    }
  else if (!refers_to_regno_p (d, d, operands[1], NULL))
    {
      emit_move_insn (dhi, operands[2]);
      operands[2] = operands[1];
      operands[1] = dhi;
    }
  else
    {
      /* Both op1 and op2 refer to D.  The only way for this to be true
         is for both operands to of the form (mem (plus d ofs)).  Since we
	 do not have reg+reg addressing, this means that D+1 is available
	 as a scratch.  Load both operands into registers.  */
      rtx dlo = gen_rtx_REG (HImode, d + 1);
      gcc_assert (!refers_to_regno_p (d + 1, d + 1, operands[1], NULL));
      emit_move_insn (dlo, operands[2]);
      emit_move_insn (dhi, operands[1]);
      operands[1] = dhi;
      operands[2] = dlo;
    }
}
  [(set_attr "extra_word_ops" "op2")])


;;- divide
(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(div:DF (match_operand:DF 1 "register_operand" "0")
		(match_operand:DF 2 "general_operand" "fmF")))]
  "TARGET_FPU"
  "{divd|divf} %2, %0"
  [(set_attr "extra_word_ops" "op2")])

(define_expand "divmodhi4"
  [(match_operand:HI 0 "register_operand" "")		; quotient
   (match_operand:HI 1 "register_operand" "")		; dividend
   (match_operand:HI 2 "general_operand" "")		; divisor
   (match_operand:HI 3 "general_operand" "")]		; remainder
  "TARGET_40_PLUS"
{
  rtx out, in;

  in = convert_modes (SImode, HImode, operands[1], 0);
  in = force_reg (SImode, in);
  out = gen_reg_rtx (SImode);
  emit_insn (gen_divmodhi4_internal (out, in, operands[2]));

  emit_move_insn (operands[0], gen_highpart (HImode, out));
  emit_move_insn (operands[3], gen_lowpart (HImode, out));
  DONE;
})

;; This is a 32/16->16 bit computation, thus the dividend must be properly
;; sign-extended to SImode on input.  The result is a register pair containing
;; both the quotient and the remainder; the only way to get the pair from the
;; register allocator is to use an SImode output.  We *could* represent this
;; with some rather complicated rtl (see s390 divmoddi4), but really this is
;; no better than an unspec, particularly since we're forced to use differing
;; operand sizes.  Also note that there's no div-by-0 trap, so may_trap_p is
;; not affected.

(define_insn "divmodhi4_internal"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand:SI 1 "register_operand" "0")
		    (match_operand:HI 2 "general_operand" "g")]
		   UNSPEC_DIVMOD))]
  "TARGET_40_PLUS"
  "div %2, %0"
  [(set_attr "extra_word_ops" "op2")])
