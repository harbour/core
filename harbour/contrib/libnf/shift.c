/*
 * $Id$
 */

/*
 * File......: SHIFT.C
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   15 Jul 1993 23:53:12   GLENN
 * Dropped _MK_FP for the preferred 0x00400017
 *
 *    Rev 1.3   15 Jul 1993 08:06:46   GLENN
 * Added call to _MK_FP() in order to make this work in protected mode.
 *
 *    Rev 1.2   15 Aug 1991 23:08:26   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:56   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:03:00   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SHIFT()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Determine status of shift key
 *  $SYNTAX$
 *     FT_SHIFT() -> lValue
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     .T. if a shift key is pressed, .F. if otherwise.
 *  $DESCRIPTION$
 *     This function is useful for times you need to know whether or not the
 *     shift key is pressed, such as during a MemoEdit().
 *  $EXAMPLES$
 *     IF FT_SHIFT()
 *        @24, 0 say "Shift"
 *     ELSE
 *        @24, 0 say "     "
 *     ENDIF
 *  $SEEALSO$
 *     FT_CAPLOCK() FT_CTRL() FT_NUMLOCK() FT_PRTSCR() FT_ALT()
 *  $END$
 */

#include <hbapi.h>

HB_FUNC(FT_SHIFT )
{
#if defined(HB_OS_DOS)
   {

   hb_retl( ( int ) ( ( *( char * ) 0x00400017 ) & 0x3 ) );

   return;
   }
#endif
}
