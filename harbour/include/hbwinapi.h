/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * This include file determines whether or not to include the Windows API
 * and defines HARBOUR_USE_WIN if the Windows API is included.
 *
 * Copyright 1999 by David G. Holm, who derived this module from various
 * similar #if blocks that used to be locate in several source modules,
 * all of which now #include "hbwinapi.h".
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

#ifndef HB_WINAPI_H_
#define HB_WINAPI_H_

#if defined(HARBOUR_USE_WIN_GTAPI) || defined(WINNT)
   #define HARBOUR_USE_WIN
   #define WIN32_LEAN_AND_MEAN
   #include <windows.h>
   #if defined(__GNUC__)
      #define HB_DONT_DEFINE_BASIC_TYPES
   #endif
#endif

#endif /* HB_WINAPI_H_ */
