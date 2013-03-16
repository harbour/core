/*
 * Harbour Project source code:
 * UUID definitions (for watcom)
 *
 * Copyright 2011 Viktor Szakats (harbour syenar.net)
 * Copyright 2011 Andi Jahja <andi.jahja yahoo.co.id>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbwinole.h"

#if defined( __WATCOMC__ )
   HB_EXTERN_BEGIN
   const GUID GUID_NULL                     = { 0x00000000, 0x0000, 0x0000, { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 } };
   const IID  IID_IUnknown                  = { 0x00000000, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
   const IID  IID_IOleObject                = { 0x00000112, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
   const IID  IID_IDispatch                 = { 0x00020400, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
   const IID  IID_IEnumVARIANT              = { 0x00020404, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
   const IID  IID_IPicture                  = { 0x7bf80980, 0xbf32, 0x101a, { 0x8b, 0xbb, 0x00, 0xaa, 0x00, 0x30, 0x0c, 0xab } };
   const IID  IID_IClassFactory2            = { 0xb196b28f, 0xbab4, 0x101a, { 0xb6, 0x9c, 0x00, 0xaa, 0x00, 0x34, 0x1d, 0x07 } };
   const IID  IID_IProvideClassInfo         = { 0xb196b283, 0xbab4, 0x101a, { 0xb6, 0x9c, 0x00, 0xaa, 0x00, 0x34, 0x1d, 0x07 } };
   const IID  IID_IProvideClassInfo2        = { 0xa6bc3ac0, 0xdbaa, 0x11ce, { 0x9d, 0xe3, 0x00, 0xaa, 0x00, 0x4b, 0xb8, 0x51 } };
   const IID  IID_IConnectionPointContainer = { 0xb196b284, 0xbab4, 0x101a, { 0xb6, 0x9c, 0x00, 0xaa, 0x00, 0x34, 0x1d, 0x07 } };
   HB_EXTERN_END
#endif
