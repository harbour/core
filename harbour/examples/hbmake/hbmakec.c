/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbmake C support code
 *
 * Copyright 2000,2001 Luiz Rafael Culik <culik@sl.conex.net> [GETUSERLANG()]
 * Copyright 2003 Marcelo Lombardo - lombardo@uol.com.br [HB_FREADLINE()]
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

#define HB_OS_WIN_USED

#include "hbapi.h"

HB_FUNC( GETUSERLANG )
{
   long lRet;

#if defined(HB_OS_WIN) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))

   switch( GetSystemDefaultLangID() )
   {
   case 0x0416:
   case 0x0816:
      lRet = 1;
      break;

   case 0x0409:
   case 0x0809:
   case 0x0C09:
   case 0x1009:
   case 0x1409:
   case 0x1809:
   case 0x1C09:
   case 0x2009:
   case 0x2409:
   case 0x2809:
   case 0x2C09:
      lRet = 2;
      break;

   case 0x040A:
   case 0x080A:
   case 0x0C0A:
   case 0x100A:
   case 0x140A:
   case 0x180A:
   case 0x1C0A:
   case 0x200A:
   case 0x240A:
   case 0x280A:
   case 0x2C0A:
   case 0x300A:
   case 0x340A:
   case 0x380A:
   case 0x3C0A:
   case 0x400A:
   case 0x440A:
   case 0x480A:
   case 0x4C0A:
   case 0x500A:
      lRet = 3;
      break;

   default:
      lRet = 2;
      break;
   }
#else
   lRet = 2;
#endif
   hb_retnl( lRet );
}
