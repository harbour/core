/*
 * $Id$
 */

/*
 * File......: PRTSCR.C
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   01 Jan 1995 03:01:00   TED
 * Added dual-mode compatibility.
 *
 *    Rev 1.2   15 Aug 1991 23:08:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:58   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_PRTSCR()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Enable or disable the Print Screen key
 *  $SYNTAX$
 *     FT_PRTSCR( [ <lSetStat> ] ) -> lCurStat
 *  $ARGUMENTS$
 *     <lSetStat> set to .T. will enable the Print Screen key,
 *     .F. will disable it.  If omitted, leaves status as is.
 *  $RETURNS$
 *     The current state: .T. if enabled, .F. if disabled.
 *  $DESCRIPTION$
 *     This function is valuable if you have a need to disable the
 *     printscreen key.  It works by fooling the BIOS into thinking that
 *     a printscreen is already in progress.  The BIOS will then refuse
 *     to invoke the printscreen handler.
 *  $EXAMPLES$
 *     FT_PRTSCR( .F. )       && Disable the printscreen key
 *     FT_PRTSCR( .T. )       && Enable the printscreen key
 *     MemVar := FT_PRTSCR()  && Get the current status
 *  $SEEALSO$
 *     FT_CAPLOCK() FT_CTRL() FT_NUMLOCK() FT_SHIFT() FT_ALT()
 *  $END$
 */

#include <hbapi.h>

#define pbyte *( ( char * ) 0x00400100 )

HB_FUNC(FT_PRTSCR)
{
#if defined(HB_OS_DOS)
   {

   if ( PCOUNT && ISLOG( 1 ) )
   {      
      if ( hb_parl( 1 ) )
          pbyte = 0;
      else
          pbyte = 1;
   }

   if ( pbyte == 1)
      hb_retl( FALSE );
   else
      hb_retl( TRUE );

   return;
   }
#endif
}
