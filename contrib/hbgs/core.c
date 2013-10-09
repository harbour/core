/*
 * Harbour Project source code:
 * Ghostscript high-level API
 *
 * Copyright 2011 Viktor Szakats (vszakats.net/harbour)
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#if defined( HB_OS_WIN ) && ! defined( _Windows )
#  define _Windows
#  include <windows.h>
#  define GSDLLEXPORT  __declspec( dllimport )
#endif

#include "ierrors.h"
#include "iapi.h"

HB_FUNC( HB_GS )
{
   HB_BOOL  bResult = HB_FALSE;
   PHB_ITEM pParam  = hb_param( 1, HB_IT_ARRAY );

   if( pParam )
   {
      void *  minst;
      int     pos;
      int     code, code1;
      int     gsargc = ( int ) hb_arrayLen( pParam ) + 1;
      char ** gsargv = ( char ** ) hb_xgrab( gsargc * sizeof( const char * ) );

      gsargv[ 0 ] = ( char * ) "hbgs"; /* actual value doesn't matter */

      for( pos = 1; pos < gsargc; ++pos )
      {
         const char * pszParam = hb_arrayGetCPtr( pParam, pos );
         gsargv[ pos ] = ( char * ) ( pszParam ? pszParam : "" );
      }

      code = gsapi_new_instance( &minst, NULL );
      if( code >= 0 )
      {
         code  = gsapi_init_with_args( minst, gsargc, gsargv );
         code1 = gsapi_exit( minst );

         if( code == 0 || code == e_Quit )
            code = code1;

         gsapi_delete_instance( minst );

         bResult = ( code == 0 || code == e_Quit );
      }
   }

   hb_retl( bResult );
}

HB_FUNC( HB_GSAPI_REVISION )
{
   gsapi_revision_t r;
   int result = gsapi_revision( &r, sizeof( r ) );

   if( result == 0 )
   {
      hb_storc( r.product, 1 );
      hb_storc( r.copyright, 2 );
      hb_stornl( r.revision, 3 );
      hb_stornl( r.revisiondate, 4 );
   }
   else
   {
      hb_storc( NULL, 1 );
      hb_storc( NULL, 2 );
      hb_stornl( 0, 3 );
      hb_stornl( 0, 4 );
   }

   hb_retni( result );
}
