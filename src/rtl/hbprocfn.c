/*
 * Harbour Project source code:
 * .prg level functions to create, wait and terminate processes
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 * based on xHarbour code by
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 * www - http://www.xharbour.org
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"

HB_FUNC( HB_PROCESSOPEN )
{
   const char * szName = hb_parc( 1 );
   PHB_ITEM pStdIn  = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pStdOut = hb_param( 3, HB_IT_BYREF );
   PHB_ITEM pStdErr = hb_param( 4, HB_IT_BYREF );
   HB_BOOL fDetach = hb_parl( 5 );
   HB_FHANDLE hStdIn, *phStdIn, hStdOut, *phStdOut, hStdErr, *phStdErr;
   HB_FHANDLE hProcess;
   HB_ULONG ulPID;

   if( szName &&
       ( pStdIn  || HB_ISNIL( 2 ) ) &&
       ( pStdOut || HB_ISNIL( 3 ) ) &&
       ( pStdErr || HB_ISNIL( 4 ) ) &&
       ( HB_ISLOG( 5 ) || HB_ISNIL( 5 ) ) &&
       ( HB_ISBYREF( 6 ) || HB_ISNIL( 6 ) ) &&
       ( ! pStdIn || ( pStdIn != pStdOut && pStdIn != pStdErr ) ) )
   {
      phStdIn  = pStdIn  ? &hStdIn  : NULL;
      phStdOut = pStdOut ? &hStdOut : NULL;
      phStdErr = pStdErr ? ( pStdOut == pStdErr ? phStdOut : &hStdErr ) : NULL;

      hProcess = hb_fsProcessOpen( szName, phStdIn, phStdOut, phStdErr,
                                   fDetach, &ulPID );
      if( hProcess != FS_ERROR )
      {
         if( phStdIn )
            hb_stornint( ( HB_NHANDLE ) *phStdIn, 2 );
         if( phStdOut )
            hb_stornint( ( HB_NHANDLE ) *phStdOut, 3 );
         if( phStdErr && phStdOut != phStdErr )
            hb_stornint( ( HB_NHANDLE ) *phStdErr, 4 );
         hb_stornint( ulPID, 6 );
      }
      hb_retnint( ( HB_NHANDLE ) hProcess );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 4001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_PROCESSVALUE )
{
   HB_FHANDLE hProcess = hb_numToHandle( hb_parnint( 1 ) );

   if( hProcess != 0 && hProcess != FS_ERROR && ( hb_pcount() < 2 || HB_ISLOG( 2 ) ) )
      hb_retni( hb_fsProcessValue( hProcess, hb_pcount() < 2 || hb_parl( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 4001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_PROCESSCLOSE )
{
   HB_FHANDLE hProcess = hb_numToHandle( hb_parnint( 1 ) );

   if( hProcess != 0 && hProcess != FS_ERROR && ( hb_pcount() < 2 || HB_ISLOG( 2 ) ) )
      hb_retl( hb_fsProcessClose( hProcess, hb_pcount() < 2 || hb_parl( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 4001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hb_processRun( <cCommand>, [ <cStdIn> ], [ @<cStdOut> ], [ @<cStdErr> ], ;
                  [ <lDetach> ] ) -> <nResult> */
HB_FUNC( HB_PROCESSRUN )
{
   const char * szName = hb_parc( 1 );
   const char * szStdIn = hb_parc( 2 );
   PHB_ITEM pStdOut = hb_param( 3, HB_IT_BYREF );
   PHB_ITEM pStdErr = hb_param( 4, HB_IT_BYREF );
   HB_BOOL fDetach = hb_parl( 5 );

   if( szName &&
       ( szStdIn || HB_ISNIL( 2 ) ) &&
       ( pStdOut || HB_ISNIL( 3 ) ) &&
       ( pStdErr || HB_ISNIL( 4 ) ) &&
       ( HB_ISLOG( 5 ) || HB_ISNIL( 5 ) ) )
   {
      HB_SIZE nStdOut, nStdErr;
      char * pStdOutBuf, * pStdErrBuf;
      char ** pStdOutPtr, ** pStdErrPtr;
      int iResult;

      nStdOut = nStdErr = 0;
      pStdOutBuf = pStdErrBuf = NULL;
      pStdOutPtr = pStdOut ? &pStdOutBuf : NULL;
      pStdErrPtr = pStdErr ? ( pStdOut == pStdErr ? pStdOutPtr : &pStdErrBuf ) : NULL;

      iResult = hb_fsProcessRun( szName, szStdIn, hb_parclen( 2 ),
                                 pStdOutPtr, &nStdOut, pStdErrPtr, &nStdErr,
                                 fDetach );

      if( pStdOutBuf )
      {
         if( ! hb_storclen_buffer( pStdOutBuf, nStdOut, 3 ) )
            hb_xfree( pStdOutBuf );
      }
      else if( pStdOut )
         hb_storc( NULL, 3 );

      if( pStdErrBuf )
      {
         if( ! hb_storclen_buffer( pStdErrBuf, nStdErr, 4 ) )
            hb_xfree( pStdErrBuf );
      }
      else if( pStdErr && pStdOut != pStdErr )
         hb_storc( NULL, 4 );

      hb_retni( iResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 4001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
