/*
 * $Id$
 */

/* NOTE: This file is also used by C code. */

/*
   Harbour Project source code

   This module contains the defines needed for Harbour programs to use
   the SET function

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

   V 1.4    David G. Holm               Added my email address.
   V 1.3    David G. Holm               Added copyright and license header,
                                        along with a complete version history.
   V 1.2    Victor Szel                 Change not documented.
   V 1.1    Manuel Ruiz                 Committed to CVS.
   V 1.0    David G. Holm               Initial version.
*/

#ifndef _SET_CH
#define _SET_CH

#define _SET_EXACT        1
#define _SET_FIXED        2
#define _SET_DECIMALS     3
#define _SET_DATEFORMAT   4
#define _SET_EPOCH        5
#define _SET_PATH         6
#define _SET_DEFAULT      7

#define _SET_EXCLUSIVE    8
#define _SET_SOFTSEEK     9
#define _SET_UNIQUE       10
#define _SET_DELETED      11

#define _SET_CANCEL       12
#define _SET_DEBUG        13
#define _SET_TYPEAHEAD    14

#define _SET_COLOR        15
#define _SET_CURSOR       16
#define _SET_CONSOLE      17
#define _SET_ALTERNATE    18
#define _SET_ALTFILE      19
#define _SET_DEVICE       20
#define _SET_EXTRA        21
#define _SET_EXTRAFILE    22
#define _SET_PRINTER      23
#define _SET_PRINTFILE    24
#define _SET_MARGIN       25

#define _SET_BELL         26
#define _SET_CONFIRM      27
#define _SET_ESCAPE       28
#define _SET_INSERT       29
#define _SET_EXIT         30
#define _SET_INTENSITY    31
#define _SET_SCOREBOARD   32
#define _SET_DELIMITERS   33
#define _SET_DELIMCHARS   34

#define _SET_WRAP         35
#define _SET_MESSAGE      36
#define _SET_MCENTER      37
#define _SET_SCROLLBREAK  38

#define _SET_EVENTMASK    39

#define _SET_COUNT        39

#endif /* _SET_CH */
