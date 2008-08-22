/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Debugging compatibility functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

#include "hbapi.h"

#ifdef HB_LEGACY_LEVEL

HB_FUNC_EXTERN( __DBGVMSTKLCOUNT );

HB_FUNC( HB_DBG_VMSTKLCOUNT )
{
   HB_FUNC_EXEC( __DBGVMSTKLCOUNT );
}

HB_FUNC_EXTERN( __DBGVMPARLLIST );

HB_FUNC( HB_DBG_VMPARLLIST )
{
   HB_FUNC_EXEC( __DBGVMPARLLIST );
}

HB_FUNC_EXTERN( __DBGVMVARLGET );

HB_FUNC( HB_DBG_VMVARLGET )
{
   HB_FUNC_EXEC( __DBGVMVARLGET );
}

HB_FUNC_EXTERN( __DBGVMVARSLEN );

HB_FUNC( HB_DBG_VMVARSLEN )
{
   HB_FUNC_EXEC( __DBGVMVARSLEN );
}

#if 0

HB_FUNC_EXTERN( __DBGVMSTKLLIST );

HB_FUNC( HB_DBG_VMSTKLLIST )
{
   HB_FUNC_EXEC( __DBGVMSTKLLIST );
}

HB_FUNC_EXTERN( __DBGVMVARLSET );

HB_FUNC( HB_DBG_VMVARLSET )
{
   HB_FUNC_EXEC( __DBGVMVARLSET );
}

HB_FUNC_EXTERN( __DBGVMSTKGLIST );

HB_FUNC( HB_DBG_VMSTKGLIST )
{
   HB_FUNC_EXEC( __DBGVMSTKGLIST );
}

HB_FUNC_EXTERN( __DBGVMSTKGCOUNT );

HB_FUNC( HB_DBG_VMSTKGCOUNT )
{
   HB_FUNC_EXEC( __DBGVMSTKGCOUNT );
}

HB_FUNC_EXTERN( __DBGVMVARGLIST );

HB_FUNC( HB_DBG_VMVARGLIST )
{
   HB_FUNC_EXEC( __DBGVMVARGLIST );
}

HB_FUNC_EXTERN( __DBGVMVARSLIST );

HB_FUNC( HB_DBG_VMVARSLIST )
{
   HB_FUNC_EXEC( __DBGVMVARSLIST );
}

HB_FUNC_EXTERN( __DBGVMVARSGET );

HB_FUNC( HB_DBG_VMVARSGET )
{
   HB_FUNC_EXEC( __DBGVMVARSGET );
}

HB_FUNC_EXTERN( __DBGVMVARSSET );

HB_FUNC( HB_DBG_VMVARSSET )
{
   HB_FUNC_EXEC( __DBGVMVARSSET );
}

#endif

#endif
