/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Functions:
 * SETFCREATE(), CSETSAFETY(), STRFILE(), FILESTR(), SCREENFILE()
 * SCREENFILE(), FILESCREEN()
 *
 * Copyright 2004 Pavel Tsarenko <tpe2@mail.ru>
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
#include "hbapifs.h"
#include "hbapigt.h"

#include "ctstrfil.h"

static int s_iFileAttr = HB_FA_NORMAL;
static BOOL s_bSafety = 0;

void ct_setfcreate( int iFileAttr )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_setfcreate(%i)", iFileAttr ) );
   s_iFileAttr = iFileAttr;
}

int ct_getfcreate( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_getfcreate()" ) );
   return s_iFileAttr;
}

HB_FUNC( SETFCREATE )
{
   hb_retni( ct_getfcreate() );

   if( HB_ISNUM( 1 ) )
   {
      ct_setfcreate( hb_parni( 1 ) );
   }
}

void ct_setsafety( BOOL bSafety )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_setsafety(%i)", bSafety ) );
   s_bSafety = bSafety;
}

BOOL ct_getsafety( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_getsafety()" ) );
   return s_bSafety;
}

HB_FUNC( CSETSAFETY )
{
   hb_retni( ct_getsafety() );

   if( HB_ISLOG( 1 ) )
   {
      ct_setsafety( hb_parnl( 1 ) );
   }
}

static LONG ct_StrFile( BYTE * pFileName, BYTE * pcStr, ULONG ulLen, BOOL bOverwrite, LONG lOffset,
                        BOOL bTrunc )
{
   HB_FHANDLE hFile;
   BOOL bOpen = FALSE;
   BOOL bFile = hb_fsFile( pFileName );
   ULONG ulWrite = 0;

   if( bFile && bOverwrite )
   {
      hFile = hb_fsOpen( pFileName, FO_READWRITE );
      bOpen = TRUE;
   }
   else if( !bFile || !ct_getsafety() )
   {
      hFile = hb_fsCreate( pFileName, ct_getfcreate() );
   }
   else
   {
      hFile = FS_ERROR;
   }

   if( hFile != FS_ERROR )
   {
      if( lOffset )
         hb_fsSeek( hFile, lOffset, FS_SET );
      else if( bOpen )
         hb_fsSeek( hFile, 0, FS_END );

      ulWrite = hb_fsWriteLarge( hFile, pcStr, ulLen );
      if( ( ulWrite == ulLen ) && bOpen && bTrunc )
         hb_fsWrite( hFile, NULL, 0 );

      hb_fsClose( hFile );
   }
   return ulWrite;
}

HB_FUNC( STRFILE )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      hb_retnl( ct_StrFile( ( BYTE * ) hb_parc( 2 ), ( BYTE * ) hb_parc( 1 ),
                            hb_parclen( 1 ), HB_ISLOG( 3 ) && hb_parl( 3 ),
                            hb_parnl( 4 ), HB_ISLOG( 5 ) && hb_parl( 5 ) ) );
   }
   else
   {
      hb_retni( 0 );
   }
}

HB_FUNC( FILESTR )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_FHANDLE hFile = hb_fsOpen( ( BYTE * ) hb_parc( 1 ), FO_READ );

      if( hFile != FS_ERROR )
      {
         LONG lFileSize = hb_fsSeek( hFile, 0, FS_END );
         LONG lPos = hb_fsSeek( hFile, hb_parnl( 3 ), FS_SET ), lLength;
         char *pcResult, *pCtrlZ;
         BOOL bCtrlZ = HB_ISLOG( 4 ) && hb_parl( 4 );

         if( HB_ISNUM( 2 ) )
         {
            lLength = hb_parnl( 2 );
            if( lLength > lFileSize - lPos )
               lLength = lFileSize - lPos;
         }
         else
            lLength = lFileSize - lPos;

         pcResult = ( char * ) hb_xgrab( lLength + 1 );
         if( lLength > 0 )
         {
            lLength = hb_fsReadLarge( hFile, ( BYTE * ) pcResult, ( ULONG ) lLength );
         }

         if( bCtrlZ )
         {
            pCtrlZ = ( char * ) memchr( pcResult, 26, lLength );
            if( pCtrlZ )
               lLength = pCtrlZ - pcResult;
         }

         hb_fsClose( hFile );
         hb_retclen_buffer( pcResult, lLength );
      }
      else
      {
         hb_retc( NULL );
      }
   }
   else
   {
      hb_retc( NULL );
   }
}

HB_FUNC( SCREENFILE )
{
   if( HB_ISCHAR( 1 ) )
   {
      char *pBuffer;
      ULONG ulSize;

      hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &ulSize );
      pBuffer = ( char * ) hb_xgrab( ulSize );

      hb_gtSave( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer );

      hb_retnl( ct_StrFile( ( BYTE * ) hb_parc( 1 ), ( BYTE * ) pBuffer,
                            ulSize, HB_ISLOG( 2 ) && hb_parl( 2 ), hb_parnl( 3 ),
                            HB_ISLOG( 4 ) && hb_parl( 4 ) ) );
      hb_xfree( pBuffer );
   }
   else
   {
      hb_retni( 0 );
   }
}

HB_FUNC( FILESCREEN )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_FHANDLE hFile = hb_fsOpen( ( BYTE * ) hb_parc( 1 ), FO_READ );

      if( hFile != FS_ERROR )
      {
         char *pBuffer;
         ULONG ulSize;
         LONG lLength;

         if( HB_ISNUM( 2 ) )
         {
            hb_fsSeek( hFile, hb_parnl( 2 ), FS_SET );
         }

         hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &ulSize );
         pBuffer = ( char * ) hb_xgrab( ulSize );

         lLength = hb_fsReadLarge( hFile, ( BYTE * ) pBuffer, ulSize );
         hb_gtRest( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer );

         hb_xfree( pBuffer );

         hb_fsClose( hFile );
         hb_retnl( lLength );
      }
      else
      {
         hb_retni( 0 );
      }
   }
   else
   {
      hb_retni( 0 );
   }
}
