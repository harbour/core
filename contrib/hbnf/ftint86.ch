/*
 * File......: Header file for users of ft_int86() function
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.5   07 Jan 1993 08:01:04   GLENN
 * We forgot to escape the less-than symbols in the HIGHBYTE() and
 * LOWBYTE() #translates.
 *
 *    Rev 1.4   31 Dec 1992 21:35:46   GLENN
 * Some typos made it into the most recent version of ftint86,
 * particularly within the HIGHBYTE() macro.  This has been fixed.
 *
 *    Rev 1.3   01 Jul 1992 01:00:52   GLENN
 * Rodgers Moore submitted some fixes to the HIGHBYTE() and LOWBYTE()
 * macros that take negative numbers into account.  Ted Means and
 * Glenn Scott added #defines for the Flag registers.  General cleanup
 * of formatting, etc.
 *
 *
 *    Rev 1.2   15 Aug 1991 23:08:48   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   27 May 1991 13:25:18   GLENN
 * Revised for new version of ft_int86() package, which is written in C
 * (cint86.c), assembler (aint86.asm).
 *
 *    Rev 1.0   01 Apr 1991 01:02:38   GLENN
 * Nanforum Toolkit
 *
 */

#ifndef __FTINT86_CH__
#define __FTINT86_CH__

#define INT86_MAX_REGS      10

#define AX                  1
#define BX                  2
#define CX                  3
#define DX                  4
#define SI                  5
#define DI                  6
#define BP                  7
#define DS                  8
#define ES                  9
#define FLAGS               10

#define FLAG_CARRY          0     // Carry flag
#define FLAG_PARITY         2     // Parity flag
#define FLAG_AUX            4     // Auxillary flag
#define FLAG_ZERO           6     // Zero flag
#define FLAG_SIGN           7     // Sign flag
#define FLAG_TRAP           8     // Trap flag
#define FLAG_INT            9     // Interrupt flag
#define FLAG_DIR            10    // Direction flag
#define FLAG_OFLOW          11    // Overflow flag

#translate MAKEHI( <X> )        => ( ( <X> ) * 256 )
#translate REG_DS               => .T.
#translate REG_ES               => .F.
#translate HIGHBYTE( <X> )      => ( Int( iif( ( <X> ) \< 0, 65536 + ( <X> ), ( <X> ) ) / 256 ) )
#translate LOWBYTE( <X> )       => (      iif( ( <X> ) \< 0, 65536 + ( <X> ), ( <X> ) ) % 256   )
#translate CARRYSET( <XFLAGS> ) => ( hb_bitTest( <XFLAGS>, FLAG_CARRY ) )

#endif // __FTINT86_CH__
