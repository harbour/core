/* $Id$

   Harbour Project source code

   This module contains the Harbour declarations for INKEY management.

   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   V 1.1    David G. Holm               Committed to CVS.
   V 1.0    David G. Holm               Initial version.
*/

#ifndef HB_INKEY_H_
#define HB_INKEY_H_

#include <hbdefs.h>

/* Harbour keyboard support functions */
int hb_inkeyGet( void );      /* Extract the next key from the Harbour keyboard buffer */
int hb_inkeyLast( void );     /* Return the value of the last key that was extracted */
int hb_inkeyNext( void );     /* Return the next key without extracting it */
void hb_inkeyPoll( void );    /* Poll the console keyboard to stuff the Harbour buffer */
void hb_inkeyReset( BOOL allocate );    /* Reset the Harbour keyboard buffer */

#endif
