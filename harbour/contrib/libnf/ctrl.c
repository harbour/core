/*
 * $Id$
 */

/*
 * File......: CTRL.C
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   15 Jul 1993 23:51:28   GLENN
 * Dropped _MK_FP for preferred 0x00400017
 *
 *    Rev 1.3   13 Jul 1993 22:20:22   GLENN
 * Modified to use _MK_FP for compatibility in protected mode.
 *
 *    Rev 1.2   15 Aug 1991 23:08:10   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:40   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:44   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_CTRL()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Determine status of the Ctrl key
 *  $SYNTAX$
 *     FT_CTRL() -> lValue
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     .T. if Ctrl key is pressed, .F. if otherwise.
 *  $DESCRIPTION$
 *     This function is useful for times you need to know whether or not
 *     the Ctrl key is pressed, such as during a MemoEdit().
 *  $EXAMPLES$
 *     IF FT_CTRL()
 *        @24, 0 say "Ctrl"
 *     ELSE
 *        @24, 0 say "    "
 *     ENDIF
 *  $SEEALSO$
 *     FT_CAPLOCK() FT_NUMLOCK() FT_PRTSCR() FT_SHIFT() FT_ALT()
 *  $END$
 */

#include <hbapi.h>

HB_FUNC( FT_CTRL )
{
#if defined(HB_OS_DOS)
   {

      hb_retl( ( int ) ( ( *( char * ) 0x00400017 ) & 0x4 ) );
      return;
   }
#endif
}
