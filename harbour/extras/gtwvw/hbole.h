/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file so MSVC can use ole in C mode while compile in C++ mode
 *
 * Copyright 2006 Paul Tucker <ptucker@sympatico.ca>
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

/*
 * These definitions prevent having to change any Platform SDK or MSVC
 * standard files to use xHarbour
 *
 */

#ifndef HB_OLE_H_
#define HB_OLE_H_

#if defined( _MSC_VER ) && ! defined( HB_OS_WIN_64 ) && \
   ! defined( __LCC__ ) && ! defined( __POCC__ ) && ! defined( __XCC__ )

#ifndef CINTERFACE
#define CINTERFACE               1
#endif

#ifndef _REFGUID_DEFINED
#define _REFGUID_DEFINED         1
#define REFGUID                  const GUID * const
#endif

#ifndef _REFIID_DEFINED
#define _REFIID_DEFINED          1
#define REFIID                   const IID * const
#endif

#ifndef _REFCLSID_DEFINED
#define _REFCLSID_DEFINED        1
#define REFCLSID                 const IID * const
#endif

#ifndef _REFFMTID_DEFINED
#define _REFFMTID_DEFINED        1
#define REFFMTID                 const IID * const
#endif

#ifndef _SYS_GUID_OPERATOR_EQ_
#define _SYS_GUID_OPERATOR_EQ_   1
#endif   // _SYS_GUID_OPERATOR_EQ_

#endif   // _MSC_VER
#endif   // HB_OLE_H_
