/*
 * File......: SCANCODE.PRG
 * Author....: Glenn Scott (from John Kaster)
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:04:32   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:52:52   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:30:32   GLENN
 * Documentation mod and check for ft_int86() compatibility
 *
 *    Rev 1.0   01 Apr 1991 01:02:12   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SCANCODE()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Wait for keypress and return keyboard scan code
 *  $SYNTAX$
 *     FT_SCANCODE() -> cCode
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     A two-character string, corresponding to the keyboard scan code.
 *  $DESCRIPTION$
 *     FT_SCANCODE() enables you to distinguish the different scancodes
 *     of similar keys (such as Grey minus versus regular minus), thus
 *     increasing the number of keys your input routine can recognize.
 *
 *     It works like INKEY(), in that it waits for a key to be pressed.
 *     The scan code consists of two bytes, which are returned as a
 *     two-character string.
 *
 *     For example, calling FT_SCANCODE() and pressing the Grey-minus
 *     key will return a two character string:
 *
 *            CHR(45) + CHR(74)
 *
 *     LASTKEY() is not updated by FT_SCANCODE(), so don't try to
 *     test LASTKEY() to see what was pressed during an FT_SCANCODE()
 *     call.  Simply assign the return value to a variable and test
 *     that (see the test driver below).
 *
 *     *  This was adapted from a short C routine posted by John Kaster on
 *        NANFORUM.  It was written in Clipper to help demonstrate the
 *        FT_INT86 function of the Nanforum Toolkit.
 *
 *     This program requires FT_INT86().
 *  $EXAMPLES$
 *        cKey := FT_SCANCODE()
 *
 *      [grey-] returns:  CHR(45) + CHR(74)
 *      [-]     returns:  CHR(45) + CHR(12)
 *      [grey+] returns:  CHR(43) + CHR(78)
 *      [+]     returns:  CHR(43) + CHR(13)
 *  $END$
 */

#include "ftint86.ch"

#define KEYB       22

#ifdef FT_TEST

  #DEFINE SCANCODE_ESCAPE   (chr(27) + chr(1))

  FUNCTION main()
     LOCAL getlist, cKey
     CLEAR
     QOut("Press any key, ESCape to exit:")

     while .t.
        cKey := FT_SCANCODE()
        QOUT( "chr(" + str(asc(substr(cKey,1,1)),3) + ")+chr(" + str(asc(substr(cKey,2,1)),3) + ")" )
        if cKey == SCANCODE_ESCAPE
           exit
        endif
     end
  RETURN nil

#endif

FUNCTION FT_SCANCODE()
  LOCAL aRegs[ INT86_MAX_REGS ]

  aRegs[ AX ] = MAKEHI( 0 )
  FT_INT86( KEYB, aRegs )
  RETURN ( chr(LOWBYTE( aRegs[AX] )) + chr(HIGHBYTE( aRegs[AX] )) )

