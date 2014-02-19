/*
 * Harbour Project source code:
 *    hb_run()
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapierr.h"
#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#if defined( HB_OS_WIN )
   #include "hbwinuni.h"
   #include <windows.h>
#endif

HB_FUNC( HB_RUN )
{
   const char * pszCommand = hb_parc( 1 );

   if( pszCommand )
   {
      int iResult = -1;

      if( hb_gtSuspend() == HB_SUCCESS )
      {
#if defined( HB_OS_WIN_CE )
         iResult = hb_fsProcessRun( pszCommand,
                                    NULL, 0, NULL, NULL, NULL, NULL, HB_FALSE );
#elif defined( HB_OS_WIN )
         LPTSTR lpCommand;

         lpCommand = HB_CHARDUP( pszCommand );
         iResult = HB_WINAPI_SYSTEM( lpCommand );
         hb_xfree( lpCommand );
#else
         char * pszFree = NULL;

         iResult = system( hb_osEncodeCP( pszCommand, &pszFree, NULL ) );

         if( pszFree )
            hb_xfree( pszFree );
#endif

         hb_gtResume();
      }
      hb_retni( iResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
