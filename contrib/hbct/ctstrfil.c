/*
 * StrFile(), FileStr(), ScreenFile(), FileScreen()
 * SetFCreate(), CSetSafety()
 *
 * Copyright 2004 Pavel Tsarenko <tpe2@mail.ru>
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
#include "hbstack.h"

#include "ctstrfil.h"

typedef struct
{
   HB_FATTR nFileAttr;
   HB_BOOL  bSafety;
} CT_STRFIL;

static void s_strfil_init( void * cargo )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) cargo;

   strfil->nFileAttr = HB_FA_NORMAL;
   strfil->bSafety   = HB_FALSE;
}

static HB_TSD_NEW( s_strfil, sizeof( CT_STRFIL ), s_strfil_init, NULL );

void ct_setfcreate( HB_FATTR nFileAttr )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) hb_stackGetTSD( &s_strfil );

   HB_TRACE( HB_TR_DEBUG, ( "ct_setfcreate(%u)", nFileAttr ) );

   strfil->nFileAttr = nFileAttr;
}

HB_FATTR ct_getfcreate( void )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) hb_stackGetTSD( &s_strfil );

   HB_TRACE( HB_TR_DEBUG, ( "ct_getfcreate()" ) );

   return strfil->nFileAttr;
}

HB_FUNC( SETFCREATE )
{
   hb_retnl( ct_getfcreate() );

   if( HB_ISNUM( 1 ) )
      ct_setfcreate( hb_parnl( 1 ) );
}

void ct_setsafety( HB_BOOL bSafety )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) hb_stackGetTSD( &s_strfil );

   HB_TRACE( HB_TR_DEBUG, ( "ct_setsafety(%i)", bSafety ) );

   strfil->bSafety = bSafety;
}

HB_BOOL ct_getsafety( void )
{
   CT_STRFIL * strfil = ( CT_STRFIL * ) hb_stackGetTSD( &s_strfil );

   HB_TRACE( HB_TR_DEBUG, ( "ct_getsafety()" ) );

   return strfil->bSafety;
}

HB_FUNC( CSETSAFETY )
{
   hb_retl( ct_getsafety() );

   if( HB_ISLOG( 1 ) )
      ct_setsafety( hb_parl( 1 ) );
}

static HB_SIZE ct_StrFile( const char * pFileName, const char * pcStr, HB_SIZE nLen,
                           HB_BOOL bAppend, HB_FOFFSET nOffset, HB_BOOL bTrunc )
{
   HB_SIZE nWrite = 0;
   HB_BOOL bFile = hb_fileExists( pFileName, NULL );

   PHB_FILE hFile = hb_fileExtOpen( pFileName, NULL,
                                    FO_WRITE | FO_PRIVATE |
                                    FXO_SHARELOCK |
                                    ( bAppend ? FXO_APPEND : FXO_TRUNCATE ) |
                                    ( ct_getsafety() ? FXO_UNIQUE : 0 ),
                                    NULL, NULL );

   if( hFile )
   {
      if( ! bFile )
         hb_fileAttrSet( pFileName, ct_getfcreate() );

      if( nOffset )
         hb_fileSeek( hFile, nOffset, FS_SET );
      else
         hb_fileSeek( hFile, 0, FS_END );

      nWrite = hb_fileResult( hb_fileWrite( hFile, pcStr, nLen, -1 ) );
      if( nWrite == nLen && bTrunc )
         hb_fileWrite( hFile, NULL, 0, -1 );

      hb_fileClose( hFile );
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
      PHB_FILE hFile;

      if( ( hFile = hb_fileExtOpen( hb_parc( 1 ), NULL,
                                    FO_READ | FO_SHARED | FO_PRIVATE |
                                    FXO_SHARELOCK | FXO_NOSEEKPOS,
                                    NULL, NULL ) ) != NULL )
      {
         HB_FOFFSET nFileSize = hb_fileSize( hFile );
         HB_FOFFSET nPos = hb_fileSeek( hFile, ( HB_FOFFSET ) hb_parnint( 3 ), FS_SET );
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
            nLength = hb_fileResult( hb_fileRead( hFile, pcResult, ( HB_SIZE ) nLength, -1 ) );

         if( bCtrlZ )
         {
            pCtrlZ = ( char * ) memchr( pcResult, 26, nLength );
            if( pCtrlZ )
               nLength = pCtrlZ - pcResult;
         }

         hb_fileClose( hFile );
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
      PHB_FILE hFile;

      if( ( hFile = hb_fileExtOpen( hb_parc( 1 ), NULL,
                                    FO_READ | FO_SHARED | FO_PRIVATE |
                                    FXO_SHARELOCK | FXO_NOSEEKPOS,
                                    NULL, NULL ) ) != NULL )
      {
         char * pBuffer;
         HB_SIZE nSize;
         HB_SIZE nLength;

         if( HB_ISNUM( 2 ) )
            hb_fileSeek( hFile, ( HB_FOFFSET ) hb_parnint( 2 ), FS_SET );

         hb_gtRectSize( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), &nSize );
         pBuffer = ( char * ) hb_xgrab( nSize );

         nLength = hb_fileResult( hb_fileRead( hFile, pBuffer, nSize, -1 ) );
         hb_gtRest( 0, 0, hb_gtMaxRow(), hb_gtMaxCol(), pBuffer );

         hb_xfree( pBuffer );

         hb_fileClose( hFile );
         hb_retns( nLength );
      }
      else
         hb_retns( 0 );
   }
   else
      hb_retns( 0 );
}
