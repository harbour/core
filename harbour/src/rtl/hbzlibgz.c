/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    GZIP functions wrapper
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"

#include <zlib.h>

/* GZIP stream destructor */
static HB_GARBAGE_FUNC( hb_gz_Destructor )
{
   gzFile * gzHolder = ( gzFile * ) Cargo;
   if( * gzHolder )
   {
      hb_vmUnlock();
      gzclose( * gzHolder );
      hb_vmLock();
      * gzHolder = NULL;
   }
}

static const HB_GC_FUNCS s_gcGZFuncs =
{
   hb_gz_Destructor,
   hb_gcDummyMark
};

static gzFile hb_gzParam( int iParam )
{
   gzFile * gzHolder = ( gzFile * ) hb_parptrGC( &s_gcGZFuncs, iParam );

   if( gzHolder && * gzHolder )
      return * gzHolder;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

/*
 * HB_GZOPEN( <cFile>, <cMode> ) => <pGZipStream> or NIL on Error
 */
HB_FUNC( HB_GZOPEN )
{
   const char * cFile = hb_parc( 1 ), * cMode = hb_parc( 2 );
   if( cFile && cMode )
   {
      hb_vmUnlock();
      gzFile gz = gzopen( cFile, cMode );
      hb_vmLock();
      if( gz )
      {
         gzFile * gzHolder = ( gzFile * ) hb_gcAllocate( sizeof( gzFile ),
                                                         &s_gcGZFuncs );
         * gzHolder = gz;
         hb_retptrGC( gzHolder );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZDOPEN( <hFile>, <cMode> ) => <pGZipStream> or NIL on Error
 */
HB_FUNC( HB_GZDOPEN )
{
   const char * cMode = hb_parc( 2 );
   if( HB_ISNUM( 1 ) && cMode )
   {
      gzFile gz;

      hb_vmUnlock();
      gz = gzdopen( hb_parni( 1 ), cMode );
      hb_vmLock();

      if( gz )
      {
         gzFile * gzHolder = ( gzFile * ) hb_gcAllocate( sizeof( gzFile ),
                                                         &s_gcGZFuncs );
         * gzHolder = gz;
         hb_retptrGC( gzHolder );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZCLOSE( <pGZipStream> ) => <nResult>
 */
HB_FUNC( HB_GZCLOSE )
{
   gzFile * gzHolder = ( gzFile * ) hb_parptrGC( &s_gcGZFuncs, 1 );

   if( gzHolder )
   {
      gzFile gz = * gzHolder;
      int iResult;

      * gzHolder = NULL;

      hb_vmUnlock();
      iResult = gzclose( gz );
      hb_vmLock();

      hb_retni( iResult );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZSETPARAMS( <pGZipStream>, <nLevel>, <nStrategy> ) => <nResult>
 */
HB_FUNC( HB_GZSETPARAMS )
{
   if( HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      gzFile gz = hb_gzParam( 1 );
      if( gz )
         hb_retni( gzsetparams( gz, hb_parni( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZREAD( <pGZipStream>, <@cData>, [ <nLen> ] ) => <nResult>
 */
HB_FUNC( HB_GZREAD )
{
   PHB_ITEM pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
   char * szBuffer;
   HB_SIZE nLen;

   if( pBuffer && hb_itemGetWriteCL( pBuffer, &szBuffer, &nLen ) )
   {
      gzFile gz = hb_gzParam( 1 );
      if( gz )
      {
         int iResult;

         if( HB_ISNUM( 3 ) )
         {
            HB_SIZE nLim = hb_parns( 3 );
            if( nLim < nLen )
               nLen = nLim;
         }

         hb_vmUnlock();
         iResult = gzread( gz, szBuffer, ( unsigned ) nLen );
         hb_vmLock();

         hb_retni( iResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZWRITE( <pGZipStream>, <cData>, [ <nLen> ] ) => <nResult>
 */
HB_FUNC( HB_GZWRITE )
{
   const char * szData = hb_parc( 2 );
   if( szData )
   {
      gzFile gz = hb_gzParam( 1 );
      if( gz )
      {
         int iResult;

         hb_vmUnlock();
         iResult = gzwrite( gz, szData,
                            HB_ISNUM( 3 ) ? ( unsigned ) hb_parns( 3 ) :
                                            ( unsigned ) hb_parclen( 2 ) );
         hb_vmLock();

         hb_retni( iResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZGETS( <pGZipStream>, <nMaxBytes> ) => <cLine> or NIL on error
 */
HB_FUNC( HB_GZGETS )
{
   int iLen = hb_parni( 2 );
   if( iLen > 0 )
   {
      gzFile gz = hb_gzParam( 1 );
      if( gz )
      {
         char * szBuffer = ( char * ) hb_xalloc( iLen + 1 );

         if( szBuffer )
         {
            char * szBuff;

            hb_vmUnlock();
            szBuff = gzgets( gz, szBuffer, iLen );
            hb_vmLock();

            if( szBuff != Z_NULL )
               hb_retc_buffer( szBuffer );
            else
               hb_xfree( szBuffer );
         }
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZPUTS( <pGZipStream>, <cData> ) => <nResult>
 */
HB_FUNC( HB_GZPUTS )
{
   const char * szData = hb_parc( 2 );
   if( szData )
   {
      gzFile gz = hb_gzParam( 1 );
      if( gz )
      {
         int iResult;

         hb_vmUnlock();
         iResult = gzputs( gz, szData );
         hb_vmLock();

         hb_retni( iResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZPUTC( <pGZipStream>, <nByte> ) => <nResult>
 */
HB_FUNC( HB_GZPUTC )
{
   if( HB_ISNUM( 2 ) )
   {
      gzFile gz = hb_gzParam( 1 );
      if( gz )
      {
         int iResult;

         hb_vmUnlock();
         iResult = gzputc( gz, hb_parni( 2 ) );
         hb_vmLock();

         hb_retni( iResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZGETC( <pGZipStream> ) => <nByte>
 */
HB_FUNC( HB_GZGETC )
{
   gzFile gz = hb_gzParam( 1 );
   if( gz )
   {
      int iResult;

      hb_vmUnlock();
      iResult = gzgetc( gz );
      hb_vmLock();

      hb_retni( iResult );
   }
}

/*
 * HB_GZUNGETC( <nByte>, <pGZipStream> ) => <nByte>
 */
HB_FUNC( HB_GZUNGETC )
{
   if( HB_ISNUM( 1 ) )
   {
#if ZLIB_VERNUM >= 0x1202
      gzFile gz = hb_gzParam( 2 );
      if( gz )
      {
         int iResult;

         hb_vmUnlock();
         iResult = gzungetc( hb_parni( 1 ), gz );
         hb_vmLock();

         hb_retni( iResult );
      }
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZFLUSH( <pGZipStream>, [ <nFlush> ] ) => <nResult>
 */
HB_FUNC( HB_GZFLUSH )
{
   gzFile gz = hb_gzParam( 1 );
   if( gz )
   {
      int iResult;

      hb_vmUnlock();
      iResult = gzflush( gz, hb_parnidef( 2, Z_SYNC_FLUSH ) );
      hb_vmLock();

      hb_retni( iResult );
   }
}

/*
 * HB_GZSEEK( <pGZipStream>, <nOffset>, [ <nWhence> ] ) => <nOffset>
 */
HB_FUNC( HB_GZSEEK )
{
   if( HB_ISNUM( 2 ) )
   {
      gzFile gz = hb_gzParam( 1 );
      if( gz )
      {
         HB_MAXINT nResult;

         hb_vmUnlock();
         nResult = gzseek( gz, ( z_off_t ) hb_parnint( 2 ),
                           hb_parnidef( 3, SEEK_SET ) );
         hb_vmLock();

         hb_retnint( nResult );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZREWIND( <pGZipStream> ) => <nResult>
 */
HB_FUNC( HB_GZREWIND )
{
   gzFile gz = hb_gzParam( 1 );
   if( gz )
   {
      int iResult;

      hb_vmUnlock();
      iResult = gzrewind( gz );
      hb_vmLock();

      hb_retni( iResult );
   }
}

/*
 * HB_GZTELL( <pGZipStream> ) => <nResult>
 */
HB_FUNC( HB_GZTELL )
{
   gzFile gz = hb_gzParam( 1 );
   if( gz )
   {
      HB_MAXINT nResult;

      hb_vmUnlock();
      nResult = gztell( gz );
      hb_vmLock();

      hb_retnint( nResult );
   }
}

/*
 * HB_GZEOF( <pGZipStream> ) => <lResult>
 */
HB_FUNC( HB_GZEOF )
{
   gzFile gz = hb_gzParam( 1 );
   if( gz )
   {
      int iResult;

      hb_vmUnlock();
      iResult = gzeof( gz );
      hb_vmLock();

      hb_retl( iResult != 0 );
   }
}

/*
 * HB_GZDIRECT( <pGZipStream> ) => <lResult>
 */
HB_FUNC( HB_GZDIRECT )
{
#if ZLIB_VERNUM >= 0x1230
   gzFile gz = hb_gzParam( 1 );
   if( gz )
   {
      int iResult;

      hb_vmUnlock();
      iResult = gzdirect( gz );
      hb_vmLock();

      hb_retl( iResult != 0 );
   }
#endif
}

/*
 * HB_GZERROR( <pGZipStream>, [ <@nError> ] ) => <cError>
 */
HB_FUNC( HB_GZERROR )
{
   gzFile gz = hb_gzParam( 1 );
   if( gz )
   {
      int iErrNum = 0;

      hb_retc( gzerror( gz, &iErrNum ) );
      hb_storni( iErrNum, 2 );
   }
}

/*
 * HB_GZCLEARERR( <pGZipStream> ) => NIL
 */
HB_FUNC( HB_GZCLEARERR )
{
#if ZLIB_VERNUM >= 0x1202
   gzFile gz = hb_gzParam( 1 );
   if( gz )
      gzclearerr( gz );
#endif
}
