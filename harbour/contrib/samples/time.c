/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Time functions
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"

extern HB_FUNC( DAYS );
extern HB_FUNC( AMPM );
extern HB_FUNC( SECS );
extern HB_FUNC( TSTRING );
extern HB_FUNC( ELAPTIME );

/* SECONDSASDAYS( <nSeconds> ) --> <nDays>
*/
HB_FUNC( SECONDSASDAYS )
{
   HB_FUNCNAME( DAYS )();
}

/* TIMEASAMPM( <cTime> ) --> <cTime> + " am" / " pm"
*/
HB_FUNC( TIMEASAMPM )
{
   HB_FUNCNAME( AMPM )();
}

/* TIMEASSECONDS( <cTime> ) --> <nSecondsFromMidnight>
*/
HB_FUNC( TIMEASSECONDS )
{
   HB_FUNCNAME( SECS )();
}

/* TIMEASSTRING( <nSeconds> ) --> <cTime>
*/
HB_FUNC( TIMEASSTRING )
{
   HB_FUNCNAME( TSTRING )();
}

/* TIMEDIFF( <cStartTime>, <cEndTime> ) --> <cDiffTime>
*/
HB_FUNC( TIMEDIFF )
{
   HB_FUNCNAME( ELAPTIME )();
}

/* TIMEISVALID( <cTime> ) --> <lValid>
*/
HB_FUNC( TIMEISVALID )
{
   char * pszTime = hb_parc( 1 );
   BOOL bRet = FALSE;

   if( pszTime )
   {
      if( atol( pszTime ) < 24 &&
          atol( pszTime + 3 ) < 60 &&
          atol( pszTime + 6 ) < 60 )
      {
         bRet = TRUE;
      }
   }

   hb_retl( bRet );
}
