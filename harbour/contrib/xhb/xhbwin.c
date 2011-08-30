/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility calls (MS-Win).
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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

/* This functions are deprecated, kept for compatibility with old
   OLE implementation. Please update your code to use this new API,
   the old one will be removed in a future revision. [vszakats] */

/* NOTE: Add to this file functions coming from old OLE source only.
         [vszakats] */

#include "hbapi.h"

#if ! defined( HB_OS_WIN )

HB_FUNC( ANSITOWIDE ) {}
HB_FUNC( WIDETOANSI ) {}
HB_FUNC( MESSAGEBOX ) {}

#else

#include "hbwinuni.h"
#include <windows.h>

HB_FUNC_EXTERN( WIN_ANSITOWIDE );
/* Original version may have returned NIL in some error situations. */
HB_FUNC( ANSITOWIDE )
{
   HB_FUNC_EXEC( WIN_ANSITOWIDE );
}

HB_FUNC_EXTERN( WIN_WIDETOANSI );
/* Original version may have returned NIL in some error situations. */
HB_FUNC( WIDETOANSI )
{
   HB_FUNC_EXEC( WIN_WIDETOANSI );
}

HB_FUNC( MESSAGEBOX )
{
   void * hStr1;
   void * hStr2;
   HWND hWnd = HB_ISNUM( 1 ) ? ( HWND ) ( HB_PTRUINT ) hb_parnint( 1 ) :
                               ( HWND ) hb_parptr( 1 );
   hb_retni( MessageBox( hWnd, HB_PARSTR( 2, &hStr1, NULL ), HB_PARSTR( 3, &hStr2, NULL ), hb_parni( 4 ) ) );
   hb_strfree( hStr1 );
   hb_strfree( hStr2 );
}

#endif /* HB_OS_WIN */
