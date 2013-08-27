/*
 * xHarbour Project source code:
 * Functions:
 * SetFCreate(), CSetSafety(), StrFile(), FileStr(), ScreenFile()
 * ScreenFile(), FileScreen()
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
#include "hbapifs.h"
#include "hbapigt.h"

#include "ctstrfil.h"

static HB_FATTR s_nFileAttr = HB_FA_NORMAL;
static HB_BOOL  s_bSafety   = HB_FALSE;

void ct_setfcreate( HB_FATTR nFileAttr )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_setfcreate(%u)", nFileAttr ) );
   s_nFileAttr = nFileAttr;
}

HB_FATTR ct_getfcreate( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_getfcreate()" ) );
   return s_nFileAttr;
}

HB_FUNC( SETFCREATE )
{
   hb_retnl( ct_getfcreate() );

   if( HB_ISNUM( 1 ) )
      ct_setfcreate( hb_parnl( 1 ) );
}

void ct_setsafety( HB_BOOL bSafety )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_setsafety(%i)", bSafety ) );
   s_bSafety = bSafety;
}

HB_BOOL ct_getsafety( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "ct_getsafety()" ) );
   return s_bSafety;
}

HB_FUNC( CSETSAFETY )
{
   hb_retl( ct_getsafety() );

   if( HB_ISLOG( 1 ) )
      ct_setsafety( hb_parl( 1 ) );
}

static HB_SIZE ct_StrFile( const char * pFileName, const char * pcStr, HB_SIZE nLen, HB_BOOL bOverwrite, HB_FOFFSET nOffset,
                           HB_BOOL bTrunc )
{
   HB_FHANDLE hFile;
   HB_BOOL bOpen = HB_FALSE;
   HB_BOOL bFile = hb_fsFile( pFileName );
   HB_SIZE nWrite = 0;

   if( bFile && bOverwrite )
   {
      hFile = hb_fsOpen( pFileName, FO_READWRITE );
      bOpen = HB_TRUE;
   }
   else if( ! bFile || ! ct_getsafety() )
      hFile = hb_fsCreate( pFileName, ct_getfcreate() );
   else
      hFile = FS_ERROR;

   if( hFile != FS_ERROR )
   {
      if( nOffset )
         hb_fsSeekLarge( hFile, nOffset, FS_SET );
      else if( bOpen )
         hb_fsSeek( hFile, 0, FS_END );

      nWrite = hb_fsWriteLarge( hFile, pcStr, nLen );
      if( ( nWrite == nLen ) && bOpen && bTrunc )
         hb_fsWrite( hFile, NULL, 0 );

      hb_fsClose( hFile );
   }
   return nWrite;
}

HB_FUNC( STRFILE )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      hb_retns( ct_StrFile( hb_parc( 2 ), hb_parc( 1 ),
                            hb_parclen( 1 ), hb_parl( 3 ),
                            ( HB_FOFFSET ) hb_parnint( 4 ), hb_parl( 5 ) ) );
   }
   else
      hb_retns( 0 );
}

HB_FUNC( FILESTR )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_FHANDLE hFile = hb_fsOpen( hb_parc( 1 ), FO_READ );

      if( hFile != FS_ERROR )
      {
         HB_FOFFSET nFileSize = hb_fsSeekLarge( hFile, 0, FS_END );
         HB_FOFFSET nPos = hb_fsSeekLarge( hFile, ( HB_FOFFSET ) hb_parnint( 3 ), FS_SET );
         HB_ISIZ nLength;
         char * pcResult, * pCtrlZ;
         HB_BOOL bCtrlZ = hb_parl( 4 );

         if( HB_ISNUM( 2 ) )
         {
            nLength = hb_parns( 2 );
            if( nLength > ( HB_ISIZ ) ( nFileSize - nPos ) )
               nLength = ( HB_ISIZ ) ( nFileSize - nPos );
         }
         else
            nLength = ( HB_ISIZ ) ( nFileSize - nPos );

         pcResult = ( char * ) hb_xgrab( nLength + 1 );
         if( nLength > 0 )
            nLength = hb_fsReadLarge( hFile, pcResult, ( HB_SIZE ) nLength );

         if( bCtrlZ )
         {
            pCtrlZ = ( char * ) memchr( pcResult, 26, nLength );
            if( pCtrlZ )
               nLength = pCtrlZ - pcResult;
         }

         hb_fsClose( hFile );
         hb_retclen_buffer( pcResult, nLength );
      }
      else
         hb_retc_null();
   }
   else
      hb_retc_null();
}

HB_FUNC( SCREENFILE )
{
   if( HB_ISCHAR( 1 ) )
   {
      char * pBuffer;
      HB_SIZE nSize;

      hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &nSize );
      pBuffer = ( char * ) hb_xgrab( nSize );

      hb_gtSave( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer );

      hb_retns( ct_StrFile( hb_parc( 1 ), pBuffer,
                            nSize, hb_parl( 2 ),
                            ( HB_FOFFSET ) hb_parnint( 3 ), hb_parl( 4 ) ) );
      hb_xfree( pBuffer );
   }
   else
      hb_retns( 0 );
}

HB_FUNC( FILESCREEN )
{
   if( HB_ISCHAR( 1 ) )
   {
      HB_FHANDLE hFile = hb_fsOpen( hb_parc( 1 ), FO_READ );

      if( hFile != FS_ERROR )
      {
         char * pBuffer;
         HB_SIZE nSize;
         HB_SIZE nLength;

         if( HB_ISNUM( 2 ) )
            hb_fsSeekLarge( hFile, ( HB_FOFFSET ) hb_parnint( 2 ), FS_SET );

         hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &nSize );
         pBuffer = ( char * ) hb_xgrab( nSize );

         nLength = hb_fsReadLarge( hFile, pBuffer, nSize );
         hb_gtRest( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer );

         hb_xfree( pBuffer );

         hb_fsClose( hFile );
         hb_retns( nLength );
      }
      else
         hb_retns( 0 );
   }
   else
      hb_retns( 0 );
}
