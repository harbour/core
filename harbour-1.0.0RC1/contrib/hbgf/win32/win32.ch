/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour GUI framework for Win32
 * Win32.ch constants definitions header file
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
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

#ifndef HB_WIN32_CH_
#define HB_WIN32_CH_

#define WM_CLOSE        0x0010
#define WM_COMMAND      0x0111
#define WM_DESTROY      0x0002
#define WM_LBUTTONDOWN  0x0201

#define WS_VISIBLE      0x10000000
#define WS_OVERLAPPED   0x00000000
#define WS_MINIMIZE     0x20000000
#define WS_MAXIMIZE     0x01000000
#define WS_CAPTION      0x00C00000
#define WS_BORDER       0x00800000
#define WS_SYSMENU      0x00080000
#define WS_THICKFRAME   0x00040000
#define WS_MINIMIZEBOX  0x00020000
#define WS_MAXIMIZEBOX  0x00010000

#define WS_OVERLAPPEDWINDOW ( WS_OVERLAPPED + WS_CAPTION + ;
                              WS_SYSMENU + WS_THICKFRAME + WS_MINIMIZEBOX + ;
                              WS_MAXIMIZEBOX)

#define WS_CHILD     0x40000000
#define WS_TABSTOP   0x00010000

#endif