/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TONE() function
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
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

/*
 * ChangeLog:
 *
 * V 1.10   David G. Holm               Added __MINGW32__ support
 * V 1.8    David G. Holm               Added '&& ! defined(_Windows)'
 *                                      check to all __BORLANDC__ checks.
 * V 1.6    David G. Holm               Added Win32 Beep(), thanks to
 *                                      Chen Kedem.
 * V 1.4    David G. Holm               Upper limit for frequency for OS/2
 *                                      DosBeep() is 32767. The CA-Clipper
 *                                      Tone() function does not have an
 *                                      upper limit on the duration, so I
 *                                      had to add an inner loop to deal
 *                                      with very long durations. There are
 *                                      actually 18.2 Clipper (PC) timer
 *                                      ticks per second.
 * V 1.2    David G. Holm               Added OS/2 GCC/EMX support.
 * V 1.1    David G. Holm               Split machine dependent code into
 *                                      hb_tone() function for internal use
 *                                      by other Harbour C functions.
 * V 1.0    Chen Kedem                  Initial version (only OS/2 support).
 *
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( TONE )
{
   if( ISNUM( 1 ) )
      hb_gtTone( hb_parnd( 1 ), ( ISNUM( 2 ) ? hb_parnd( 2 ) : 1.0 ) );
}

