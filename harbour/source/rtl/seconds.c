/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SECONDS() function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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

#include "hbapi.h"

#include <time.h>
#if defined( OS_UNIX_COMPATIBLE )
   #include <sys/timeb.h>
#else
   #include <sys\timeb.h>
#endif
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__)
   #include <dos.h>
#endif

double hb_secondsToday( void )
{
#if defined(_MSC_VER)
   #define timeb _timeb
   #define ftime _ftime
#endif
   struct timeb tb;
   struct tm * oTime;

   HB_TRACE(HB_TR_DEBUG, ("hb_secondsToday()"));

   ftime( &tb );
   oTime = localtime( &tb.time );

   return ( oTime->tm_hour * 3600 ) +
          ( oTime->tm_min * 60 ) +
            oTime->tm_sec +
          ( ( double ) tb.millitm / 1000 );
}

HARBOUR HB_SECONDS( void )
{
   hb_retnd( hb_secondsToday() );
}

