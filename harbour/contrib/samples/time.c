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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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
