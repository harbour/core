/*
 * $Id$
 */

/*
 * File......: CAPLOCK.C
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.5   01 May 1995 03:05:00   TED
 * Modified typecasting to tame compiler warning.
 *
 *    Rev 1.4   15 Jul 1993 00:12:22   GLENN
 * Changed status_byte to make the function work in protected mode.
 *
 *    Rev 1.3   15 Aug 1991 23:08:30   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:38   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   27 May 1991 14:41:56   GLENN
 * Added a parameter to turn CapLock on or off.
 *
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_CAPLOCK()
 *  $CATEGORY$
 *     Keyboard/Mouse
 *  $ONELINER$
 *     Determine and optionally change the status of CapLock key
 *  $SYNTAX$
 *     FT_CAPLOCK([ <lNewSetting> ]) -> lCurrentSetting
 *  $ARGUMENTS$
 *     <lNewSetting> is optional and if supplied is the new setting
 *     for the CapLock key.  Specify .T. to turn CapLock on, or .F. to
 *     turn it off.
 *  $RETURNS$
 *     .T. if CapLock is set, .F. if it isn't set.  The value returned
 *      represents the setting in effect prior to any changes that might
 *      by made by <lNewSetting>.
 *  $DESCRIPTION$
 *     This function is useful if you need to know or set the status of the
 *     CapLock key for some reason.
 *  $EXAMPLES$
 *     IF FT_CAPLOCK()
 *        Qout( "CapLock is active" )
 *     ENDIF
 *  $SEEALSO$
 *     FT_ALT() FT_CTRL() FT_NUMLOCK() FT_PRTSCR() FT_SHIFT()
 *  $END$
 */

#include <hbapi.h>

#define status_byte ( *( unsigned char * ) ( 0x00400017 ) )

HB_FUNC(FT_CAPLOCK)
{
#if defined(HB_OS_DOS) 
   {
   hb_retl( ( int ) ( status_byte & 0x40 ) );

   if ( hb_pcount() )
      if ( ISLOG(1) )
         status_byte = ( status_byte | ( unsigned char ) 0x40 );
      else
         status_byte = ( status_byte & ( unsigned char ) 0xBF );

   return;
   }
#endif
}
