/* Copyright (C) 2012 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define GLOBAL(X)	___##X

#define HAVE_ASHC	(defined(__pdp11_40__) || defined(__pdp11_45__))
#define HAVE_MUL	(defined(__pdp11_40__) || defined(__pdp11_45__))
#define HAVE_DIV	(defined(__pdp11_40__) || defined(__pdp11_45__))

.macro	FUNC	X
	.text
	.globl	\X
	.even
\X:
.endm

.macro	ENDF	X
.endm


/*----------------------------------------------------------------------
 * HImode shift routines
 *----------------------------------------------------------------------*/

#ifdef L_ashlhi3
FUNC	GLOBAL(ashlhi3)

#if HAVE_ASHC
	mov	02(sp), r0
	ash	04(sp), r0
#else
	mov	02(sp), r0	/* load data */
	mov	04(sp), r1	/* load count */
#ifdef __OPTIMIZE_SIZE__
	beq	1f
0:	asl	r0
	dec	r1
	bne	0b
#else
	bit	$010, r1	/* shift >= 8? */
	beq	0f
	swab	r0		/* rotate by 8 */
	bic	$0377, r0	/* zero low 8 bits */
0:	bic	$-010, r1	/* limit shift to 3 bits */
	neg	r1
	asl	r1
	add	1f, r1
	jmp	(r1)
.rept 7
	asl	r0
.endr
#endif /* SIZE */
#endif /* ASHC */
1:	rts	pc

ENDF	GLOBAL(ashlhi3)
#endif /* L_ashlhi3 */

#ifdef L_ashrhi3
FUNC	GLOBAL(ashrhi3)

	mov	02(sp), r0	/* load data */
	mov	04(sp), r1	/* load count */
#if HAVE_ASHC
	neg	r1
	ash	r1, r0
#elif defined(__OPTIMIZE_SIZE__)
	beq	1f
0:	asr	r0
	dec	r1
	bne	0b
#else
	bit	$010, r1	/* shift >= 8? */
	beq	0f
	swab	r0		/* rotate by 8 */
	movb	r0, r0		/* propagate sign to high 8 bits */
0:	bic	$-010, r1	/* limit shift to 3 bits */
	neg	r1
	asl	r1
	add	1f, r1
	jmp	(r1)
.rept 7
	asr	r0
.endr
#endif /* ASHC/SIZE */
1:	rts	pc

ENDF	GLOBAL(ashrhi3)
#endif /* L_ashrhi3 */

#ifdef L_lshrhi3
FUNC	GLOBAL(lshrhi3)

#if HAVE_ASHC
	clr	r0
	mov	02(sp), r1
	neg	04(sp)
	ashc	04(sp), r0
#else
	mov	02(sp),r0	/* load data */
	mov	04(sp),r1	/* load count */
#if defined(__OPTIMIZE_SIZE__)
	beq	1f
	clc
	ror	r0
	dec	r1
	beq	1f
0:	asr	r0
	dec	r1
	bne	0b
#else
	bit	$010, r1	/* shift >= 8?  */
	beq	0f

	swab	r0		/* rotate by 8 */
	bic	$-0400, r0	/* zero high 8 bits */
	bic	$-010, r1	/* limit shift count to 3 bits */
	neg	r1
	asl	r1
	add	1f, r1
	jmp	(r1)

0:	bic	$-010,r1	/* limit shift count to 3 bits */
	beq	1f		/* shift == 0? */
	clc			/* logical shift of first bit */
	ror	r0
	dec	r1		/* decrement shift count */
	neg	r1
	asl	r1
	add	1f, r1
	jmp	(r1)
.rept 7
	asr	r0
.endr
#endif /* SIZE */
#endif /* ASHC */
1:	rts	pc

ENDF	GLOBAL(lshrhi3)
#endif /* L_lshrhi3 */

/*----------------------------------------------------------------------
 * SImode shift routines
 *----------------------------------------------------------------------*/

#ifdef L_ashlsi3
FUNC	GLOBAL(ashlsi3)

#if HAVE_ASHC
	mov	02(sp), r0
	mov	04(sp), r1
	ashc	06(sp), r0
	rts	pc
#else
	mov	r2, -(sp)
	mov	04(sp), r0	/* load data */
	mov	06(sp), r1
	mov	010(sp), r2	/* load count */

	bit	$020, r2	/* shift >= 16 */
	beq	0f
	mov	r1, r0
	clr	r1
0:
	bic	$-020, r2	/* limit shift count to 4 bits */
	beq	2f
1:
	asl	r1
	rol	r0
	dec	r2
	bne	1b
2:
	mov	(sp)+, r2
	rts	pc
#endif /* ASHC */

ENDF	GLOBAL(ashlsi3)
#endif /* L_ashlsi3 */

#ifdef L_ashrsi3
FUNC	GLOBAL(ashrsi3)

#if HAVE_ASHC
	mov	02(sp), r0
	mov	04(sp), r1
	neg	06(sp)
	ashc	06(sp), r0
	rts	pc
#else
	mov	r2, -(sp)
	mov	04(sp), r0	/* load data */
	mov	06(sp), r1
	mov	010(sp), r2	/* load count */

	bit	$020, r2	/* shift >= 16? */
	beq	0f
	mov	r0, r1
	clr	r0
	tst	r1		/* sign-extend from 16 bits */
	bpl	0f
	com	r0
0:
	bic	$-020, r2	/* limit shift count to 4 bits */
	beq	2f
1:
	asr	r0
	ror	r1
	dec	r2
	bne	1b
2:
	mov	(sp)+, r2
	rts	pc
#endif /* ASHC */

ENDF	GLOBAL(ashrsi3)
#endif /* L_ashrsi3 */

#ifdef L_lshrsi3
FUNC	GLOBAL(lshrsi3)

	mov	r2, -(sp)
	mov	04(sp), r0	/* load data */
	mov	06(sp), r1
	mov	010(sp), r2	/* load count */

#if HAVE_ASHC
	beq	2f
	clc
	ror	r0
	ror	r1
	dec	r2
	neg	r2
	ashc	r2, r0
#else
	bit	$020, r2	/* shift >= 16 */
	beq	0f
	mov	r0, r1
	clr	r0
0:
	bic	$-020, r2	/* limit shift count to 4 bits */
	beq	2f
	clc			/* logical shift for first bit */
	ror	r0
	ror	r1
	dec	r2
	beq	2f
1:
	asr	r0		/* arithmetic shift for subsequent bits */
	ror	r1
	dec	r2
	bne	1b
#endif /* ASHC */

2:	mov	(sp)+, r2
	rts	pc

ENDF	GLOBAL(lshrsi3)
#endif /* L_lshrsi3 */

/*----------------------------------------------------------------------
 * DImode shift routines
 *----------------------------------------------------------------------*/

#ifdef L_ashldi3
FUNC	GLOBAL(ashldi3)

	mov	r2, -(sp)
	mov	r3, -(sp)
	mov	r4, -(sp)

	mov	012(sp), r0	/* load data */
	mov	014(sp), r1
	mov	016(sp), r2
	mov	020(sp), r3
	mov	022(sp), r4	/* load count */

	bic	$-0100, r4	/* limit shift count to 6 bits */
	beq	L_finish
	bit	$040, r4
	beq	L_lt32

#if HAVE_ASHC
	/* 32 <= shift < 64 */
	mov	r2, r0
	mov	r3, r1
	clr	r2
	clr	r3
	sub	$040, r4
	ashc	r4, r0
	br	L_finish

L_lt32:	/* 0 < shift < 32 */
	ashc	r4, r0		/* left shift high pair */
	clc			/* logical right shift low pair */
	ror	r2
	ror	r3
	sub	$037, r4	/* -((32-count)-1) = -(31-count) = r4-31 */
	ashc	r4, r2
	bis	r2, r0		/* combine shifted high and low pair */
	bis	r3, r1
	mov	016(sp), r2	/* reload low pair ... */
	mov	020(sp), r3
	ashc	022(sp), r2	/* ... and left shift */
#else
	bit	$020, r4
	beq	L_ge32

	/* 48 <= shift < 64 */
	mov	r3, r0
	clr	r1
	clr	r2
	clr	r3
	sub	$060, r4
	beq	L_finish
0:	asl	r0
	dec	r4
	bne	0b
	br	L_finish

L_ge32:	/* 32 <= shift < 48 */
	mov	r2, r0
	mov	r3, r1
	clr	r2
	clr	r3
	sub	$040, r4
	beq	L_finish
0:	asl	r1
	rol	r0
	dec	r4
	bne	0b
	br	L_finish

L_lt32:	/* 0 < shift < 32 */
	bit	$020, r4
	beq	L_lt16

	/* 16 <= shift < 32 */
	mov	r1, r0
	mov	r2, r1
	mov	r3, r2
	clr	r3
	sub	$020, r4
	beq	L_finish
0:	asl	r2
	rol	r1
	rol	r0
	dec	r4
	bne	0b
	br	L_finish

L_lt16:	/* 0 < shift < 16 */
0:	asl	r3
	rol	r2
	rol	r1
	rol	r0
	dec	r4
	bne	0b
#endif /* ASHC */

L_finish:
	mov	010(sp), r4	/* load return pointer */
	mov	r0, (r4)+
	mov	r1, (r4)+
	mov	r2, (r4)+
	mov	r3, (r4)

	mov	(sp)+, r4
	mov	(sp)+, r3
	mov	(sp)+, r2
	rts	pc

ENDF	GLOBAL(ashlsi3)
#endif /* L_ashlsi3 */

#ifdef L_ashrdi3
FUNC	GLOBAL(ashrdi3)

	mov	r2, -(sp)
	mov	r3, -(sp)
	mov	r4, -(sp)

	mov	012(sp), r0	/* load data */
	mov	014(sp), r1
	mov	016(sp), r2
	mov	020(sp), r3
	mov	022(sp), r4	/* load count */

	bic	$-0100, r4	/* limit shift count to 6 bits */
	beq	L_finish
	bit	$040, r4
	beq	L_lt32

#if HAVE_ASHC
	/* 32 <= shift < 64 */
	mov	r1, r3
	mov	r0, r2
	sxt	r1
	sxt	r0
	neg	r4
	add	$0100, r4
	ashc	r4, r2
	br	L_finish

L_lt32:	/* 0 < shift < 32 */
	neg	r4		/* 32 - count */
	add	$040, r4
	ashc	r4, r0		/* left shift high pair */
	clc			/* logical right shift one */
	ror	r2
	ror	r3
	sub	$037, r4	/* -(count-1) = -((32-r4)-1) = r4-31 */
	ashc	r4, r2		/* arithmetic right shift n-1 */
	bis	r0, r2		/* combine shifted high and low pair */
	bis	r1, r3
	mov	012(sp), r0	/* reload high pair ... */
	mov	014(sp), r1
	ashc	r4, r0		/* ... and right shift */
#else
	bit	$020, r4
	beq	L_ge32

	/* 48 <= shift < 64 */
	clr	r1
	mov	r0, r3
	bpl	0f
	dec	r1
0:	mov	r1, r2
	mov	r1, r0
	sub	$060, r4
	beq	L_finish
1:	asr	r3
	dec	r4
	bne	1b
	br	L_finish

L_ge32:	/* 32 <= shift < 48 */
	mov	r1, r3
	clr	r1
	mov	r0, r2
	bpl	0f
	dec	r1
0:	mov	r1, r0
	sub	$040, r4
	beq	L_finish
1:	asr	r2
	ror	r3
	dec	r4
	bne	1b
	br	L_finish

L_lt32:	/* 0 < shift < 32 */
	bit	$020, r4
	beq	L_lt16

	/* 16 <= shift < 32 */
	mov	r2, r3
	mov	r1, r2
	mov	r0, r1
	clr	r0
	tst	r1
	bpl	0f
	dec	r0
0:	sub	$020, r4
	beq	L_finish
1:	asr	r1
	ror	r2
	ror	r3
	dec	r4
	bne	1b
	br	L_finish

L_lt16:	/* 0 < shift < 16 */
0:	asr	r0
	ror	r1
	ror	r2
	ror	r3
	dec	r4
	bne	0b
#endif /* ASHC */

L_finish:
	mov	010(sp), r4	/* load return pointer */
	mov	r0, (r4)+
	mov	r1, (r4)+
	mov	r2, (r4)+
	mov	r3, (r4)

	mov	(sp)+, r4
	mov	(sp)+, r3
	mov	(sp)+, r2
	rts	pc

ENDF	GLOBAL(ashrsi3)
#endif /* L_ashrsi3 */

#ifdef L_lshrdi3
FUNC	GLOBAL(lshrdi3)

	mov	r2, -(sp)
	mov	r3, -(sp)
	mov	r4, -(sp)

	mov	012(sp), r0	/* load data */
	mov	014(sp), r1
	mov	016(sp), r2
	mov	020(sp), r3
	mov	022(sp), r4	/* load count */

	bic	$-0100, r4	/* limit shift count to 6 bits */
	beq	L_finish
	bit	$040, r4
	beq	L_lt32

#if HAVE_ASHC
	/* 32 <= shift < 64 */
	mov	r1, r3
	mov	r0, r2
	clr	r1
	clr	r0
	sub	$040, r4	/* reduce shift count and clear carry */
	beq	L_finish
	ror	r2		/* logical right shift one */
	ror	r3
	dec	r4		/* arithmetic right shift n-1 */
	neg	r4
	ashc	r4, r2
	br	L_finish

L_lt32:	/* 0 < shift < 32 */
	neg	r4		/* r4 = 32 - count */
	add	$040, r4
	ashc	r4, r0		/* left shift high pair */
	clc			/* logical right shift one */
	ror	r2
	ror	r3
	sub	$037, r4	/* -(count-1) = -((32-r4)-1) = r4-31 */
	ashc	r4, r2		/* arithmetic right shift n-1 */
	bis	r0, r2		/* combine shifted high and low pair */
	bis	r1, r3
	mov	012(sp), r0	/* reload high pair ... */
	mov	014(sp), r1
	clc			/* logical right shift one */
	ror	r0
	ror	r1
	ashc	r4, r0		/* arithmetic right shift n-1 */
#else
	bit	$020, r4
	beq	L_ge32

	/* 48 <= shift < 64 */
	mov	r0, r3
	clr	r2
	clr	r1
	clr	r0
	sub	$060, r4	/* reduce shift count and clear carry */
	beq	L_finish
	ror	r2		/* logical right shift one */
	dec	r4
	beq	L_finish
1:	asr	r3		/* arithmetic right shift n-1 */
	dec	r4
	bne	1b
	br	L_finish

L_ge32:	/* 32 <= shift < 48 */
	mov	r1, r3
	mov	r0, r2
	clr	r1
	clr	r0
	sub	$040, r4	/* reduce shift count and clear carry */
	beq	L_finish
	ror	r2		/* logical right shift one */
	ror	r3
	dec	r4
	beq	L_finish
1:	asr	r2		/* arithmetic right shift n-1 */
	ror	r3
	dec	r4
	bne	1b
	br	L_finish

L_lt32:	/* 0 < shift < 32 */
	bit	$020, r4
	beq	L_lt16

	/* 16 <= shift < 32 */
	mov	r2, r3
	mov	r1, r2
	mov	r0, r1
	clr	r0
	sub	$020, r4	/* reduce shift count and clear carry */
	beq	L_finish
	ror	r1		/* logical right shift one */
	ror	r2
	ror	r3
	dec	r4
	beq	L_finish
1:	asr	r1		/* arithmetic right shift n-1 */
	ror	r2
	ror	r3
	dec	r4
	bne	1b
	br	L_finish

L_lt16:	/* 0 < shift < 16 */
	clc			/* logical right shift one */
	ror	r0
	ror	r1
	ror	r2
	ror	r3
	dec	r4
	beq	L_finish
0:	asr	r0		/* arithmetic right shift n-1 */
	ror	r1
	ror	r2
	ror	r3
	dec	r4
	bne	0b
#endif /* ASHC */

L_finish:
	mov	010(sp), r4	/* load return pointer */
	mov	r0, (r4)+
	mov	r1, (r4)+
	mov	r2, (r4)+
	mov	r3, (r4)

	mov	(sp)+, r4
	mov	(sp)+, r3
	mov	(sp)+, r2
	rts	pc

ENDF	GLOBAL(lshrsi3)
#endif /* L_lshrsi3 */

/*----------------------------------------------------------------------
 * Multiplication routines
 *----------------------------------------------------------------------*/

#ifdef L_mulhi3
FUNC	GLOBAL(mulhi3)

#if HAVE_MUL
	mov	02(sp), r1
	mul	04(sp), r1
	mov	r1, r0
	rts	pc
#else
	mov	r2, -(sp)
	mov	04(sp), r1	/* load multiplicands */
	mov	06(sp), r2
	clr	r0		/* initialize product (and clear carry) */
	ror	r2		/* shift out first counter bit */
1:	bcc	2f		/* accumulate iff previous counter bit set */
	add	r1, r0
2:	asl	r1		/* double multiplicand */
	asr	r2		/* handle next counter bit */
	bne	1b		/* counter dropped to 0? */

	bcc	3f		/* accumulate iff last counter bit set */
	add	r1, r0
3:	mov	(sp)+, r2
	rts	pc
#endif /* MUL */

ENDF	GLOBAL(mulhi3)
#endif /* L_mulhi3 */

#ifdef L_mulsi3
FUNC	GLOBAL(mulsi3)

#ifdef HAVE_MUL
	mov	r2, -(sp)
	mov	r3, -(sp)

	/* The trick here is to correct for the sign the inputs.  We want to
	   add 1 to the high part iff the low part has the sign bit set.
	   With SXT, we can produce -1; subtract the high part from that and
	   we get -1-high = -(high+1), the negation of the correct value. */
        mov     010(sp), r2	/* low(a) */
        sxt     r1
        sub     006(sp), r1	/* -(high(a) + correction) */
        mov     014(sp), r0	/* low(b) */
        sxt     r3
        sub     012(sp), r3	/* -(high(b) + correction) */
        mul     r0, r1		/* low(b) * -high(a) */
        mul     r2, r3		/* low(a) * -high(b) */
        add     r1, r3
        mul     r2, r0		/* low(a) * low(b) as 32-bit */
        sub     r3, r0		/* subtract negated high-part products */

        mov     (sp)+,r3
        mov     (sp)+,r2
        rts     pc
#else
	mov	r2, -(sp)
	mov	r3, -(sp)
	mov	r4, -(sp)

	mov	010(sp), r2	/* high(a) */
	mov	012(sp), r3	/* low(a) */
	mov	016(sp), r4	/* low(b) */

	clr	r0		/* clear product, and carry flag */
	clr	r1
	ror	r4
0:	bcc	1f		/* accumulate product if carry set */
	add	r2, r0
	add	r3, r1
	adc	r0
1:	asl	r3		/* a << 1 */
	rol	r2
	asr	r4		/* b >> 1 */
	bne	0b
	bcc	2f		/* final accumulation */
	add	r2, r0
	add	r3, r1
	adc	r0
2:
	mov	012(sp), r2	/* low(a) << 16 */
	mov	014(sp), r4	/* high(b) */
	clc
	ror	r4
0:	bcc	1f
	add	r2, r0		/* accumulate to high part of product */
1:	asl	r2
	asr	r4
	bne	0b
	bcc	2f
	add	r2, r0
2:
	mov	(sp)+, r4
	mov	(sp)+, r3
	mov	(sp)+, r2
	rts	pc

ENDF	GLOBAL(mulsi3)
#endif /* MUL */
#endif /* L_mulsi3 */

/*----------------------------------------------------------------------
 * HImode division routines
 *----------------------------------------------------------------------*/

#ifdef L_udivmodhi4
/* This is an internal routine to the other division entry points.

   Input:
	r0	numerator
	r1	denominator
   Output:
	r0	quotient
	r1	remainder
*/
FUNC	GLOBAL(udivmodhi4)
#if HAVE_DIV

	/* We have a 32x16->16 bit signed division instruction.
	   Use that to perform a 16x16->16 bit unsigned division.  */
	mov	r1, -(sp)
	bmi	1f

	/* The denomenator is < 2**15, and can be used in the DIV insn
	   directly.  Zero-extend the numerator and go.  */
	mov	r0, r1
	clr	r0
	div	(sp)+, r0
	rts	pc

	/* The denominator is >= 2**15.
	   There are only two possible quotients: 0 and 1.  */
1:	add	$2, sp			/* discard push from above */
	sub	r1, r0			/* compute remainder */
	bhi	2f			/* underflow? */
	mov	r0, r1			/* return remainder */
	mov	$1, r0			/* return quotient */
	rts	pc
2:	add	r0, r1			/* regenerate and return remainder */
	clr	r0			/* return quotient */
	rts	pc

#elif defined(__OPTIMIZE_SIZE__)

	mov	r2, -(sp)
	mov	r3, -(sp)

#define arg1	r0		/* dividend, quotient */
#define arg2	r1		/* divisor */
#define rem	r2
#define cnt	r3

	mov	$020, cnt	/* init loop counter */
	clr	rem
	asl	arg1
0:
	rol	rem		/* shift dividend into remainder */
	cmp	rem, arg2	/* compare remainder and divisor */
	bcc	1f		/* rem <= devisor */
	sub	arg2, rem
1:
	rol	arg1
	dec	cnt
	bne	0b

	mov	rem, r1
	mov	(sp)+, r3
	mov	(sp)+, r2
	rts	pc

#else /* !DIV && !SIZE */

	mov	r2, -(sp)
	mov	r3, -(sp)

#define	num	r0
#define den	r1
#define bit	r2
#define quo	r3

	mov	$1, bit
	clr	quo
	tst	den
	beq	L_divbyzero
	bmi	L_denmsb		/* DEN already has msb set? */
0:
	cmp	den, num		/* shift up until DEN >= NUM.  */
	bhis	L_divloop
	asl	bit
	asl	den
	bpl	0b			/* quit shifting if msb now set.  */

	/* Here, DEN has msb set.  In the worst case BIT may also have msb set.
	   This case requres extra care with the logical right shift.  By
	   peeling this one loop iteration, we can use quicker arithmetic
	   shifts in the main division loop.  */
L_denmsb:
	cmp	num, den		/* num >= den? */
	blo	0f
	sub	den, num
	bis	bit, quo
0:	clc
	ror	den
	clc
	ror	bit
	beq	L_finish

L_divloop:
	cmp	num, den		/* num >= den? */
	blo	0f
	sub	den, num
	bis	bit, quo
0:	asr	den
	asr	bit
	bne	L_divloop

L_finish:
L_divbyzero:				/* returns quo=0, rem=num.  */
	mov	num, r1
	mov	quo, r0
	mov	(sp)+, r2
	mov	(sp)+, r3
	rts	pc

#endif /* HAVE_DIV */
ENDF	GLOBAL(udivmodhi4)
#endif /* L_udivmodhi4 */

#ifdef L_udivhi3
FUNC	GLOBAL(udivhi3)

	mov	2(sp), r0
	mov	4(sp), r1
	jmp	GLOBAL(udivmodhi4)

ENDF	GLOBAL(udivhi3)
#endif /* L_udivhi3 */

#ifdef L_umodhi3
FUNC	GLOBAL(umodhi3)

	mov	2(sp), r0
	mov	4(sp), r1
	jsr	pc, GLOBAL(udivmodhi4)
	mov	r1, r0
	rts	pc

ENDF	GLOBAL(umodhi3)
#endif /* L_umodhi3 */

#ifdef L_divhi3
FUNC	GLOBAL(divhi3)

#if HAVE_DIV
	mov	r1, -(sp)
	mov	r0, r1
	sxt	r0
	div	(sp)+, r0
	rts	pc
#else
	mov	r2, -(sp)
	clr	r2
	mov	4(sp), r0	/* load abs(numerator) */
	bpl	0f
	neg	r0
	com	r2		/* toggle result negation */
0:	mov	6(sp), r1	/* load abs(denominator) */
	bpl	1f
	neg	r1
	com	r2		/* toggle result negation */
1:	jsr	pc, GLOBAL(udivmodhi4)
	tst	r2		/* negate result if required */
	beq	2f
	neg	r0
2:	mov	(sp)+, r2
	rts	pc
#endif /* DIV */

ENDF	GLOBAL(divhi3)
#endif /* L_divhi3 */

#ifdef L_modhi3
FUNC	GLOBAL(modhi3)

#if HAVE_DIV
	mov	r1, -(sp)
	mov	r0, r1
	sxt	r0
	div	(sp)+, r0
	mov	r1, r0
	rts	pc
#else
	mov	r2, -(sp)
	clr	r2
	mov	4(sp), r0	/* load abs(numerator) */
	bpl	0f
	neg	r0
	com	r2		/* negate result */
0:	mov	6(sp), r1	/* load abs(denominator) */
	bpl	1f
	neg	r1
1:	jsr	pc, GLOBAL(udivmodhi4)
	mov	r1, r0		/* return remainder */
	tst	r2		/* negate result if required */
	beq	2f
	neg	r0
2:	mov	(sp)+, r2
	rts	pc
#endif /* DIV */

ENDF	GLOBAL(modhi3)
#endif /* L_modhi3 */

/*----------------------------------------------------------------------
 * Misc bit manipulation routines.
 *----------------------------------------------------------------------*/

#ifdef L_bswapsi2
FUNC	GLOBAL(bswapsi2)

	mov	02(sp), r1
	mov	04(sp), r0
	swab	r1
	swab	r0
	rts	pc

ENDF	GLOBAL(bswapsi2)
#endif /* L_bswapsi2 */

#ifdef L_bswapdi2
FUNC	GLOBAL(bswapdi2)

	mov	02(sp), r0
	mov	012(sp), r1
	swab	r1
	mov	r1, (r0)+
	mov	010(sp), r1
	swab	r1
	mov	r1, (r0)+
	mov	06(sp), r1
	swab	r1
	mov	r1, (r0)+
	mov	04(sp), r1
	swab	r1
	mov	r1, (r0)
	rts	pc

ENDF    GLOBAL(bswapdi2)
#endif /* L_bswapdi2 */

#ifdef L_popcounthi2
FUNC	GLOBAL(popcounthi2)

#ifdef __OPTIMIZE_SIZE__
	mov	2(sp), r1
	clr	r0		/* clear output and clear carry */
0:	adc	r0
	asl	r1
	bne	0b		/* 3.74us/loop */
	adc	r0		/* 5.72us min; 61.82us max */
	rts	pc
#elif 1
	mov	2(sp), r0
	mov	r0, r1
	asr	r0
	bic	$0125252, r0
	sub	r0, r1		/* 8 2-bit fields */

	mov	r1, r0
	asr	r1
	asr	r1
	bic	$0146314, r0
	bic	$0146314, r1
	add	r0, r1		/* 4 4-bit fields */

	mov	r1, r0
	asr	r1
	asr	r1
	asr	r1
	asr	r1
	bic	$0170360, r0
	bic	$0170360, r1
	add	r0, r1		/* 2 8-bit fields */

	mov	r1, r0		/* sum into a single field  */
	bic	$-0400, r0
	clrb	r1
	swab	r1
	add	r1, r0		/* 37.01us */
	rts	pc
#else
	mov	2(sp), r0
	mov	r2, -(sp)
	clr	r2
0:	mov	r0, r1
	bic	$-020, r1
	movb	$GLOBAL(__popcount_tab)(r1), r1
	add	r1, r2		/* carry must be clear */
	ror	r0
	asr	r0
	asr	r0
	asr	r0
	bne	0b		/* 15.06us/loop */
	mov	r2, r0
	mov	(sp)+ r2	/* 21.2us min; 66.38us max */
	rts	pc
#endif /* SIZE */

ENDF	GLOBAL(popcounthi2)
#endif /* L_popcounthi2 */

#ifdef L_popcountsi2
FUNC	GLOBAL(popcountsi2)

#ifdef __OPTIMIZE_SIZE__
	mov	2(sp), r0			/* cfa = sp+2 */
	mov	r0, -(sp)			/*     = sp+4 */
	jsr	pc, GLOBAL(popcounthi2)
	mov	r0, -(sp)			/*     = sp+6; sum = cfa-6 */
	mov	010(sp), r0			/* w[1] = cfa+2 = sp+8 */
	mov	r0, -(sp)			/*     = sp+8 */
	jsr	pc, GLOBAL(popcounthi2)
	add	2(sp), r0
	add	$6, sp
	rts	pc
#else
	mov	r2, -(sp)
	mov	r3, -(sp)
	mov	010(sp), r0
	mov	012(sp), r1

	mov	r0, r2
	mov	r1, r3
	asr	r0
	ror	r1
	bic	$0125252, r0
	bic	$0125252, r1
	sub	r0, r2
	sub	r1, r3
	sbc	r2		/* 16 2-bit fields, max value 2 */

	mov	r2, r0
	mov	r3, r1
	asr	r0
	ror	r1
	asr	r0
	ror	r1
	bic	$0146314, r0
	bic	$0146314, r1
	bic	$0146314, r2
	bic	$0146314, r3
	add	r1, r0
	add	r2, r0
	add	r3, r0		/* 4 4-bit fields, max value 8 */

	mov	r0, r1
	asr	r0
	asr	r0
	asr	r0
	asr	r0
	bic	$0170360, r0
	bic	$0170360, r1
	add	r0, r1		/* 2 8-bit fields, max value 16 */

	mov	r1, r0
	bic	$-0400, r0
	clrb	r1
	swab	r1
	add	r1, r0

	mov	(sp)+, r3
	mov	(sp)+, r2
	rts	pc
#endif /* SIZE */

ENDF	GLOBAL(popcountsi2)
#endif /* L_popcountsi2 */

#ifdef L_popcountdi2
FUNC	GLOBAL(popcountdi2)

	mov	2(sp), r0			/* w[0] = cfa = sp+2 */
	mov	r2, -(sp)			/* cfa = sp+4 */
	mov	r0, -(sp)			/* cfa = sp+6 */
	jsr	pc, GLOBAL(popcounthi2)
	mov	r0, r2
	mov	010(sp), (sp)			/* w[1] = cfa+2 = sp+8 */
	jsr	pc, GLOBAL(popcounthi2)
	add	r0, r2
	mov	012(sp), (sp)			/* w[2] = cfa+4 = sp+10 */
	jsr	pc, GLOBAL(popcounthi2)
	add	r0, r2
	mov	014(sp), (sp)			/* w[3] = cfa+6 = sp+12 */
	jsr	pc, GLOBAL(popcounthi2)
	add	r2, r0
	mov	(sp)+, r2			/* discard function arg */
	mov	(sp)+, r2
	rts	pc

ENDF	GLOBAL(popcountdi2)
#endif /* L_popcountdi2 */

#ifdef L_parityhi2
FUNC	GLOBAL(parityhi2)

#ifdef __OPTIMIZE_SIZE__
	mov	2(sp), r1
	mov	r1, r0
	clc
	ror	r1
0:	add	r1, r0
	asr	r1
	bne	0b		/* 4us/loop */
	bic	$-2, r0		/* 11.32us min; 71.32us max */
	rts	pc
#else

	mov	2(sp), r0

#if !HAVE_ASHC
	mov	r2, -(sp)
.macro	xor	x, y
	mov	\y, r2
	bic	\x, \y
	bic	r2, \x
	bis	\x, \y
.endm
#endif

	/* Left shifts are quicker than right shifts; accumulate to msb.  */
	mov	r0, r1
	swab	r1
	xor	r1, r0

	mov	r0, r1
	asl	r1
	asl	r1
	asl	r1
	asl	r1
	xor	r1, r0

	mov	r0, r1
	asl	r1
	asl	r1
	xor	r1, r0

	mov	r0, r1
	asl	r1
	xor	r0, r1

	clr	r0		/* move high bit of r1 to low bit of r0 */
	rol	r1
	rol	r0		/* 18.81us */

#if !HAVE_ASHC
	mov	(sp)+, r2
#endif

	rts	pc
#endif /* SIZE */
ENDF	GLOBAL(parityhi2)
#endif /* L_parityhi2 */

#ifdef L_paritysi2
FUNC	GLOBAL(paritysi2)

#if HAVE_ASHC
	mov	2(sp), r0
	mov	4(sp), r1
	xor	r1, r0
#else
	mov	2(sp), r0
	mov	4(sp), r1
	bic	r0, r1
	bic	4(sp), r1
	bis	r1, r0
#endif /* ASHC */
	mov	r0, 2(sp)
	jmp	GLOBAL(parityhi2)

ENDF	GLOBAL(paritysi2)
#endif /* L_paritysi2 */

#ifdef L_paritydi2
FUNC	GLOBAL(paritydi2)

#if HAVE_ASHC
	mov	2(sp), r0
	mov	4(sp), r1
	xor	r1, r0
	mov	6(sp), r1
	xor	r1, r0
	mov	010(sp), r1
	xor	r1, r0
#else
	mov	2(sp), r0
	mov	4(sp), r1
	bic	r0, r1
	bic	4(sp), r0
	bis	r1, r0
	mov	6(sp), r1
	bic	r0, r1
	bic	6(sp), r0
	bis	r1, r0
	mov	010(sp), r1
	bic	r0, r1
	bic	010(sp), r0
	bis	r1, r0
#endif /* ASHC */
	mov	r0, 2(sp)
	jmp	GLOBAL(parityhi2)

ENDF	GLOBAL(paritydi2)
#endif /* L_paritydi2 */
