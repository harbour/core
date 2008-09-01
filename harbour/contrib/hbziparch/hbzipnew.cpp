/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zlib low level interface for Harbour
 *
 * Copyright 2000-2003 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or ( at your option )
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
 * Boston, MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbdate.h"
#include "hbvm.h"
#include "hbzlib.h"

#include <time.h>

#ifdef HB_EXTERN_C
   #include "ZipArchive.h"
#endif

#define HBZA_FI_FILEPOS    1
#define HBZA_FI_LENGTH     2
#define HBZA_FI_METHOD     3
#define HBZA_FI_SIZE       4
#define HBZA_FI_RATIO      5
#define HBZA_FI_DATE       6
#define HBZA_FI_TIME       7
#define HBZA_FI_CRC32      8
#define HBZA_FI_ATTR       9

typedef struct
{
   int      iWrite;
   int      iExtract;
   int      iRead;
   char *   pszComment;
   int      iReadOnly;
   PHB_ITEM pChangeDiskBlock;
   PHB_ITEM pProgressBlock;
} HB_ZA_SETTINGS;

static HB_ZA_SETTINGS s_hbzaSettings = { 0 };

#ifdef __cplusplus
extern "C" {
#endif

class SegmCallback : public CZipSegmCallback
{
   bool Callback( ZIP_SIZE_TYPE iProgress )
   {
      PHB_ITEM Disk = hb_itemPutNL( NULL, m_uVolumeNeeded );
      HB_SYMBOL_UNUSED( iProgress );
      hb_vmEvalBlockV( s_hbzaSettings.pChangeDiskBlock, 1, Disk);
      hb_itemRelease( Disk );

      return true;
   }
};

class SegmActionCallback : public CZipActionCallback
{
   bool Callback( ZIP_SIZE_TYPE iProgress )
   {
      PHB_ITEM Disk  = hb_itemPutNL( NULL, m_uProcessed );
      PHB_ITEM Total = hb_itemPutNL( NULL, m_uTotalToProcess );
      HB_SYMBOL_UNUSED( iProgress );
      hb_vmEvalBlockV( s_hbzaSettings.pProgressBlock, 2, Disk, Total );
      hb_itemRelease( Disk );
      hb_itemRelease( Total );

      return true;
   }
};

class SegmCallbackc : public CZipSegmCallback
{
   bool Callback( ZIP_SIZE_TYPE iProgress )
   {
      PHB_ITEM Disk = hb_itemPutNL( NULL, m_uVolumeNeeded );
      HB_SYMBOL_UNUSED( iProgress );
      hb_vmEvalBlockV( s_hbzaSettings.pChangeDiskBlock, 1, Disk );
      hb_itemRelease( Disk );

      return true;
   }
};

class SegmActionCallbackc : public CZipActionCallback
{
   bool Callback( ZIP_SIZE_TYPE iProgress )
   {
      PHB_ITEM Disk = hb_itemPutNL( NULL, m_uProcessed );
      PHB_ITEM Total = hb_itemPutNL( NULL, m_uTotalToProcess );
      HB_SYMBOL_UNUSED( iProgress );
      hb_vmEvalBlockV( s_hbzaSettings.pProgressBlock, 2, Disk, Total );
      hb_itemRelease( Disk );
      hb_itemRelease( Total );

      return true;
   }
};

static HB_FOFFSET hb_GetCurrentFileSize( const char * szFile )
{
   return hb_fsFSize( ( BYTE * ) szFile, TRUE );
}

int hb_CheckSpanMode( char * szFile )
{
   int iReturn = 0;

   CZipArchive szZip;
   SegmCallback span;

   szZip.SetSegmCallback( &span );

   try
   {
      szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 ); /* s.r. to avoid GPF when ZIP file is read only ! */
   }
   catch( CZipException &e )
   {
      if( e.m_iCause == CZipException::cdirNotFound )
      {
         szZip.Close( true );
         iReturn = 114;
      }
      else if( e.m_iCause == CZipException::noCallback )
      {
         szZip.Close( true );
         iReturn = 103;
      }
      else if( e.m_iCause == CZipException::genericError )
      {
         szZip.Close( true );
         iReturn = 100;
      }      
      else if( e.m_iCause == CZipException::badZipFile )
      {
         szZip.Close( true );
         iReturn = 101;
      }
      else if( e.m_iCause == CZipException::badCrc )
      {
         szZip.Close( true );
         iReturn = 102;
      }
      else if( e.m_iCause == CZipException::aborted )
      {
         szZip.Close( true );
         iReturn = 104;
      }
      else if( e.m_iCause == CZipException::abortedAction )
      {
         szZip.Close( true );
         iReturn = 105;
      }
      else if( e.m_iCause == CZipException::abortedSafely )
      {
         szZip.Close( true );
         iReturn = 106;
      }
      else if( e.m_iCause == CZipException::nonRemovable )
      {
         szZip.Close( true );
         iReturn = 107;
      }
      else if( e.m_iCause == CZipException::tooManyVolumes )
      {
         szZip.Close( true );
         iReturn = 108;
      }
      else if( e.m_iCause == CZipException::tooLongData )
      {
         szZip.Close( true );
         iReturn = 109;
      }
      else if( e.m_iCause == CZipException::badPassword )
      {
         szZip.Close( true );
         iReturn = 110;
      }
      else if( e.m_iCause == CZipException::dirWithSize )
      {
         szZip.Close( true );
         iReturn = 111;
      }
      else if( e.m_iCause == CZipException::internalError )
      {
         szZip.Close( true );
         iReturn = 112;
      }
      else if( e.m_iCause == CZipException::notRemoved )
      {
         szZip.Close( true );
         iReturn = 113;
      }
      else if( e.m_iCause == CZipException::notRenamed )
      {
         szZip.Close( true );
         iReturn = 114;
      }
      else if( e.m_iCause == CZipException::platfNotSupp )
      {
         szZip.Close( true );
         iReturn = 115;
      }
   }

   if( ! iReturn )
   {
      iReturn = szZip.GetSegmMode();
      szZip.Close();
   }

   return iReturn;
}

int hb_CmpPkSpan( char *szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   ULONG ulCount;

   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFile( ( BYTE * ) szFile );
   CZipString szArchive = szFile;

   CZipArchive szZip;
   SegmCallback span;
   SegmActionCallback spanac;

   szZip.SetSegmCallback( &span );
   bDrive = false;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         szZip.Open( szArchive, CZipArchive::zipCreateSegm, 0 );
      else
         bReturn = FALSE;
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   if( ! bReturn )
      return ( int ) bReturn;

   if( szPassWord )
      szZip.SetPassword( szPassWord );

   if( s_hbzaSettings.pszComment )
   {
      szZip.SetGlobalComment( s_hbzaSettings.pszComment );
      hb_xfree( s_hbzaSettings.pszComment );
   }

   if( pProgress && HB_IS_BLOCK( pProgress ) )
   {
      s_hbzaSettings.pProgressBlock = pProgress;
      szZip.SetCallback( &spanac );
   }

   for( ulCount = 1; ulCount <= hb_arrayLen( pArray ); ulCount++ )
   {
      if( pBlock && HB_IS_BLOCK( pBlock ) )
      {
         PHB_ITEM FileName = hb_itemPutC( NULL, hb_arrayGetCPtr( pArray, ulCount ) );
         PHB_ITEM FilePos = hb_itemPutNI( NULL, ulCount );
         hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );
         hb_itemRelease( FileName );
         hb_itemRelease( FilePos );
      }

      try
      {
         szZip.AddNewFile( hb_arrayGetCPtr( pArray, ulCount ), iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 );
      }
      catch( ... )
      {}
   }

   try
   {
      szZip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return ( int ) bReturn;
}

PHB_ITEM hb_GetFileNamesFromZip( char * szFile, BOOL iMode )
{
   PHB_ITEM pZipArray;
   bool iReturn = true;

   CZipArchive szZip;
   SegmCallback span;

   if( s_hbzaSettings.iWrite > 0 )
      szZip.SetAdvanced( s_hbzaSettings.iWrite, s_hbzaSettings.iExtract, s_hbzaSettings.iRead );

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSegmCallback( &span );
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }
   catch( ... )
   {}

   if( iReturn )
   {
      int iNumberOfFiles = szZip.GetCount();
      ULONG ulCount;

      pZipArray = hb_itemArrayNew( iNumberOfFiles );

      for( ulCount = 0; ulCount < ( ULONG ) iNumberOfFiles; ulCount++ )
      {
         CZipFileHeader fh;
        
         szZip.GetFileInfo( fh, ( WORD ) ulCount );

         if( iMode )
         {
            PHB_ITEM TempArray = hb_itemArrayNew( 9 );
            char szAttr[ 5 ];
            char szTime[ 9 ];
            char * szMethod = NULL;
            char szCRC[ 9 ];
            int iRatio;

            hb_arraySetC( TempArray, HBZA_FI_FILEPOS, ( char * ) ( LPCTSTR ) fh.GetFileName() );

            if( fh.m_uUncomprSize > 0 )
            {
               iRatio = 100 - ( ( fh.m_uComprSize * 100 ) / fh.m_uUncomprSize );

               if( iRatio < 0 )
                  iRatio = 0;

               hb_arraySetNL( TempArray, HBZA_FI_LENGTH, fh.m_uUncomprSize );
               hb_arraySetNL( TempArray, HBZA_FI_SIZE, fh.m_uComprSize );
               hb_arraySetNL( TempArray, HBZA_FI_RATIO, iRatio );
            }
            else
            {
               iRatio = 0;
               hb_arraySetNL( TempArray, HBZA_FI_LENGTH, fh.m_uUncomprSize );
               hb_arraySetNL( TempArray, HBZA_FI_SIZE, fh.m_uComprSize );
               hb_arraySetNL( TempArray, HBZA_FI_RATIO, iRatio );
            }

            #if defined( HB_OS_WIN_32 )
            {
               DWORD uAttr = fh.GetSystemAttr();
               szAttr[ 0 ] = uAttr & FILE_ATTRIBUTE_READONLY ? ( char ) _T( 'r' ) : ( char ) _T( '-' );
               szAttr[ 1 ] = uAttr & FILE_ATTRIBUTE_HIDDEN ? ( char ) _T( 'h' ) : ( char ) _T( '-' );
               szAttr[ 2 ] = uAttr & FILE_ATTRIBUTE_SYSTEM ? ( char ) _T( 's' ) : ( char ) _T( 'w' );
               szAttr[ 3 ] = uAttr & FILE_ATTRIBUTE_DIRECTORY ? ( char ) _T( 'D' ) : uAttr & FILE_ATTRIBUTE_ARCHIVE ? ( char ) _T( 'a' ): ( char ) _T( '-' );

               if( fh.m_uMethod == 0 || uAttr & FILE_ATTRIBUTE_DIRECTORY )
                  szMethod = "Stored";
            }
            #else
               szAttr[ 0 ] =
               szAttr[ 1 ] =
               szAttr[ 2 ] =
               szAttr[ 3 ] = ' ';
            #endif

            szAttr[ 4 ] = fh.IsEncrypted() ? ( char ) _T( '*' ) : ( char ) _T( ' ' );

            if( fh.m_uMethod == Z_DEFLATED )
            {
               UINT iLevel = ( UINT )( ( fh.m_uFlag & 0x6 ) / 2 );

               switch( iLevel )
               {
                  case 0:
                     szMethod = "DeflatN";
                     break;

                  case 1:
                     szMethod = "DeflatX";
                     break;

                  case 2:
                  case 3:
                     szMethod = "DeflatF";
                     break;

                  default:
                     szMethod = "Unknown";
               }
            }
            
            hb_arraySetC( TempArray, HBZA_FI_METHOD, szMethod );

            snprintf( szCRC, sizeof( szCRC ), "%8.8lx", ( ULONG ) fh.m_uCrc32 );

            hb_arraySetCL( TempArray, HBZA_FI_CRC32, szCRC, 8 );
            hb_arraySetDL( TempArray, HBZA_FI_DATE, hb_dateEncode( ( LONG ) ( fh.m_uModDate >> 9 ) + 1980, ( LONG ) ( ( fh.m_uModDate & ~0xFE00 ) >> 5 ), ( LONG ) fh.m_uModDate & ~0xFFE0 ) );

            {
               time_t theTime = fh.GetTime();
               tm * SzTime = localtime( &theTime );

               snprintf( szTime, sizeof( szTime ), "%02d:%02d:%02d", SzTime->tm_hour, SzTime->tm_min, SzTime->tm_sec );
            }

            hb_arraySetCL( TempArray, HBZA_FI_TIME, szTime, 8 );
            hb_arraySetCL( TempArray, HBZA_FI_ATTR, szAttr, 5 );
            hb_arraySetForward( pZipArray, ulCount+1, TempArray );
            hb_itemRelease( TempArray );
         }
         else
            hb_arraySetC( pZipArray, ulCount + 1, ( char * ) ( LPCTSTR ) fh.GetFileName() );
      }
   }
   else
      pZipArray = hb_itemArrayNew( 0 );

   szZip.Close();

   return pZipArray;
}

BOOL hb_IsPassWord( char *szFile )
{
   BOOL bReturn = TRUE;
   CZipFileHeader fh;

   CZipArchive szZip;
   SegmCallback span;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSegmCallback( &span );
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            bReturn = FALSE;
      }
   }
   catch( CZipException )
   {}

   if( bReturn )
   {
      szZip.GetFileInfo( fh, ( WORD )0 );

      bReturn = fh.IsEncrypted();

      szZip.Close();
   }

   return bReturn;
}

int hb_GetNumberofFilestoUnzip( char * szFile )
{
   int iNumberOfFiles = 0;

   CZipArchive szZip;
   SegmCallback span;

   szZip.SetSegmCallback( &span );

   try
   {
      szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
      iNumberOfFiles = szZip.GetCount();
      szZip.Close();
   }
   catch( CZipException &e )
   {
      if( e.m_iCause == CZipException::badZipFile || e.m_iCause == CZipException::cdirNotFound )
         iNumberOfFiles = -1;
   }

   return iNumberOfFiles;
}

static void hb_CallbackFuncFree( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( s_hbzaSettings.pChangeDiskBlock )
   {
      hb_itemRelease( s_hbzaSettings.pChangeDiskBlock );
      s_hbzaSettings.pChangeDiskBlock = NULL;
   }
}

int hb_SetCallbackFunc( PHB_ITEM pFunc )
{
   if( s_hbzaSettings.pChangeDiskBlock )
      hb_itemClear( s_hbzaSettings.pChangeDiskBlock );
   else
   {
      /* register cleanup function, it's executed only once */
      hb_vmAtExit( hb_CallbackFuncFree, NULL );
      s_hbzaSettings.pChangeDiskBlock = hb_itemNew( NULL );
   }

   if( pFunc )
      hb_itemCopy( s_hbzaSettings.pChangeDiskBlock, pFunc );

   return ( int ) true;
}

int hb_DeleteSel( char * szFile, PHB_ITEM pArray, BOOL bCase )
{
   bool iReturn = true;
   CZipArchive szZip;
   CZipStringArray aFiles;

   HB_SYMBOL_UNUSED( bCase );

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
/*       default: */
            iReturn = false;
      }
   }
   catch( CZipException )
   {}

   if( iReturn )
   {
      ULONG ulCount;

      for( ulCount = 1; ulCount <= ( ULONG ) hb_arrayLen( pArray ); ulCount ++ )
         aFiles.Add( ( char * ) hb_arrayGetCPtr( pArray, ulCount ) );

      szZip.RemoveFiles( aFiles );
   }

   szZip.Close();

   return ( int ) iReturn;
}

int hb_UnzipAll( char * szFile, PHB_ITEM pBlock, BOOL bWithPath, char * szPassWord, char * pbyBuffer, PHB_ITEM pDiskBlock, PHB_ITEM pProgress )
{
   bool iReturn = true;
   int iMode;
   CZipArchive szZip;
   SegmCallback span;
   SegmActionCallback spanac;

   char * szPath = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   HB_SYMBOL_UNUSED( pDiskBlock );

   if( pProgress && HB_IS_BLOCK( pProgress ) )
   {
      s_hbzaSettings.pProgressBlock = pProgress;
      szZip.SetCallback( &spanac );
   }

   if( szPassWord )
      szZip.SetPassword( szPassWord );

   iMode = hb_CheckSpanMode( szFile );

   try
   {
      if( iMode == 0 )
          szZip.Open( szFile, CZipArchive::zipOpen, 0 );
      else
      {
         if( iMode == -1 )
         {
            szZip.SetSegmCallback( &span );
            szZip.Open( szFile, CZipArchive::zipOpen, 0 );
         }
         else
         {
            if( iMode == -2 )
               szZip.Open( szFile, CZipArchive::zipOpen, 1 );
            else
               iReturn = false;
         }
      }
   }
   catch( ... )
   {}

   if( iReturn )
   {
      uLong uiCount;

      if( pbyBuffer )
      {
         if( hb_stricmp( pbyBuffer, ".\\" ) == 0 )
            hb_fsCurDirBuff( 0, ( BYTE * ) szPath, _POSIX_PATH_MAX + 1 );
         else
            hb_strncpy( szPath, pbyBuffer, _POSIX_PATH_MAX );

         szZip.SetRootPath( szPath );
      }

      for( uiCount = 0; uiCount < ( uLong ) szZip.GetCount(); uiCount++ )
      {
         CZipFileHeader fh;
         szZip.GetFileInfo( fh, ( WORD ) uiCount );
      
         if( pBlock && HB_IS_BLOCK( pBlock ) )
         {
            PHB_ITEM FileName = hb_itemPutC( NULL, ( char * ) ( LPCTSTR ) fh.GetFileName() );
            PHB_ITEM FilePos = hb_itemPutNI( NULL, uiCount );
            hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );
            hb_itemRelease( FileName );
            hb_itemRelease( FilePos );
         }
      
         try
         {
            szZip.SetPassword( szPassWord );
            szZip.ExtractFile( ( WORD ) uiCount, ( LPCTSTR ) szPath, bWithPath ? true : false, NULL, 65536 );
         }
         catch( ... )
         {
            szZip.CloseFile( NULL, true );
         }
      }
   }

   hb_xfree( szPath );

   return iReturn;
}

int hb_UnzipSel( char * szFile, PHB_ITEM pBlock, BOOL lWithPath, char * szPassWord, const char * pbyBuffer, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   bool iReturn = true;
   char * szPath = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   CZipArchive szZip;
   SegmCallback span;
   SegmActionCallback spanac;

   if( pProgress && HB_IS_BLOCK( pProgress ) )
   {
      s_hbzaSettings.pProgressBlock = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSegmCallback( &span );
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }
   catch( CZipException )
   {}

   if( iReturn )
   {
      int iCause;

      if( szPassWord !=  NULL )
         szZip.SetPassword( szPassWord );

      if( pbyBuffer )
      {
         if( hb_stricmp( pbyBuffer, ".\\" ) == 0 )
            hb_fsCurDirBuff( 0, ( BYTE * ) szPath, _POSIX_PATH_MAX + 1 );
         else
            hb_strncpy( szPath, pbyBuffer, _POSIX_PATH_MAX );

         szZip.SetRootPath( szPath );
      }

      for( iCause = 1; iCause <= ( int ) hb_arrayLen( pSelArray ); iCause++ )
      {
         LPCTSTR lpFiletoExtract = hb_arrayGetC( pSelArray, iCause );
         ULONG ulCount = szZip.FindFile( lpFiletoExtract, false );

         if( ulCount == ( ULONG ) -1 )
            ulCount = szZip.FindFile( lpFiletoExtract, true );

         {
            CZipFileHeader fh;
            szZip.GetFileInfo( fh, ( WORD ) ulCount );

            if( pBlock && HB_IS_BLOCK( pBlock ) )
            {
               PHB_ITEM FileName = hb_itemPutC( NULL, ( char * ) ( LPCTSTR ) fh.GetFileName() );
               PHB_ITEM FilePos = hb_itemPutNI( NULL, iCause );
               hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );
               hb_itemRelease( FileName );
               hb_itemRelease( FilePos );
            }

            try
            {
               szZip.ExtractFile( ( WORD ) ulCount, ( LPCTSTR ) szPath, lWithPath ? true : false, NULL, 65536 );
            }
            catch( ... )
            {
               iReturn = false;
               szZip.CloseFile( NULL, true );
            }
         }

         if( lpFiletoExtract )
            hb_xfree( ( void * ) lpFiletoExtract );
      }
   }

   szZip.Close();

   hb_xfree( szPath );

   return ( int ) iReturn;
}


int hb_UnzipSelIndex( char * szFile, PHB_ITEM pBlock, BOOL lWithPath, char * szPassWord, char * szPath, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   bool iReturn = true;

   CZipArchive szZip;
   SegmCallback span;
   SegmActionCallback spanac;

   if( pProgress && HB_IS_BLOCK( pProgress ) )
   {
      s_hbzaSettings.pProgressBlock = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSegmCallback( &span );
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }
   catch( CZipException )
   {}

   if( iReturn )
   {
      int iCause;

      if( szPassWord )
         szZip.SetPassword( szPassWord );

      for( iCause = 0; iCause <= ( int ) hb_arrayLen( pSelArray ); iCause++ )
      {
         ULONG ulCount = hb_arrayGetNI( pSelArray, iCause ) - 1;

         CZipFileHeader fh;

         szZip.GetFileInfo( fh, ( WORD ) ulCount );

         if( pBlock && HB_IS_BLOCK( pBlock ) )
         {
            PHB_ITEM FileName = hb_itemPutC( NULL, ( char * ) ( LPCTSTR ) fh.GetFileName() );
            hb_vmEvalBlockV( pBlock, 1, FileName );
            hb_itemRelease( FileName );
         }

         try
         {
            szZip.ExtractFile( ( WORD ) ulCount, ( LPCTSTR ) szPath, lWithPath ? true : false, NULL, 65536 );
         }
         catch( ... )
         {
            iReturn = false;
            szZip.CloseFile( NULL, true );
         }
      }
   }

   szZip.Close();

   return ( int ) iReturn;
}

void hb_SetZipBuff( int a, int b, int c )
{
   s_hbzaSettings.iWrite   = HB_MIN( a, 65535 );
   s_hbzaSettings.iExtract = HB_MIN( b, 16384 );
   s_hbzaSettings.iRead    = HB_MIN( c, 32768 );
}

void hb_SetZipComment( char * szComment )
{
   s_hbzaSettings.pszComment = hb_strdup( szComment );
}

void hb_SetZipReadOnly( int iRead )
{
   s_hbzaSettings.iReadOnly = iRead;
}

char * hb_GetZipComment( char * szFile )
{
   char * szTemp;
   bool iReturn = true;

   CZipArchive szZip;
   SegmCallback span;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSegmCallback( &span );
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }
   catch( CZipException )
   {}

   szTemp = hb_strdup( iReturn ? ( const char * ) szZip.GetGlobalComment() : "" );

   szZip.Close();

   return szTemp;
}

BOOL hb_TransferFilesFromzip( char * szSource, char * szDest, PHB_ITEM pArray )
{
   CZipArchive szZSource;
   CZipArchive szZDest;
   CZipStringArray aFiles;
   BOOL bReturn = TRUE;
   BOOL bReturn1 = TRUE;

   try
   {
      switch( hb_CheckSpanMode( szSource ) )
      {
         case 0:
            szZSource.Open( szSource, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
/*       default: */
            bReturn = FALSE;
      }
   }
   catch( CZipException )
   {}

   try
   {
      switch( hb_CheckSpanMode( szDest ) )
      {
         case 0:
            szZDest.Open( szDest, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
/*       default: */
            bReturn1 = FALSE;
      }
   }
   catch( CZipException )
   {}

   if( bReturn && bReturn1 )
   {
      ULONG ulCount;

      for( ulCount = 1; ulCount <= ( ULONG ) hb_arrayLen( pArray ); ulCount++ )
         aFiles.Add( ( char * ) hb_arrayGetCPtr( pArray, ulCount ) );

      szZDest.GetFromArchive( szZSource, aFiles, false );

      szZDest.Close();
      szZSource.Close();

      return TRUE;
   }

   return FALSE;
}

int hb_CompressFile( char * szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   char * szFileLower = hb_strdup( ( char * ) szFile );
   BOOL bFileExist = hb_fsFile( ( BYTE * ) szFile );
   BOOL bReturn = TRUE;

   CZipArchive szZip;
   SegmCallbackc span;
   SegmActionCallbackc spanac;

   szZip.SetSegmCallback( &span );

   #ifdef HB_OS_WIN_32
      hb_strLower( szFileLower, strlen( szFileLower ) );
   #endif

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         szZip.Open( szFile, CZipArchive::zipCreate, 0 );
      else
         szZip.Open( szFile, CZipArchive::zipOpen, 0 );
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   if( bReturn )
   {
      ULONG ulCount;

      if( szPassWord )
         szZip.SetPassword( szPassWord );

      if( s_hbzaSettings.pszComment )
      {
         szZip.SetGlobalComment( s_hbzaSettings.pszComment );
         hb_xfree( s_hbzaSettings.pszComment );
      }

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         szZip.SetCallback( &spanac );
      }

      for( ulCount = 1; ulCount <= hb_arrayLen( pArray ); ulCount++ )
      {
         const char * szDummy = ( char * ) hb_arrayGetCPtr( pArray, ulCount );
         char * szDummyLower = hb_strdup( ( char * ) szDummy );

         #ifdef HB_OS_WIN_32
            hb_strLower( szDummyLower, strlen( szDummyLower ) );
         #endif

         /* Prevent adding current archive file. */
         if( ! strstr( szFileLower, szDummyLower ) && 
             ! strstr( szDummyLower, szFileLower ) )
         {
            if( hb_GetCurrentFileSize( szDummy ) != F_ERROR )
            {
               if( pBlock && HB_IS_BLOCK( pBlock ) )
               {
                  PHB_ITEM FileName = hb_itemPutC( NULL, szDummy );
                  PHB_ITEM FilePos = hb_itemPutNI( NULL, ulCount );
                  hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );
                  hb_itemRelease( FileName );
                  hb_itemRelease( FilePos );
               }

               try
               {
                  szZip.AddNewFile( szDummy, iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 );
               }
               catch( ... )
               {}
            }
         }

         hb_xfree( szDummyLower );
      }
   }

   hb_xfree( szFileLower );

   try
   {
      szZip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return ( int ) bReturn;
}

int hb_CompressFileStd( char * szFile, char * szFiletoCompress, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFile( ( BYTE * ) szFile );

   CZipArchive szZip;
   SegmCallbackc span;
   SegmActionCallbackc spanac;

   szZip.SetSegmCallback( &span );

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         szZip.Open( szFile, CZipArchive::zipCreate, 0 );
      else
         szZip.Open( szFile, CZipArchive::zipOpen, 0 );
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }

   if( bReturn )
   {
      if( szPassWord )
         szZip.SetPassword( szPassWord );

      if( s_hbzaSettings.pszComment )
      {
         szZip.SetGlobalComment( s_hbzaSettings.pszComment );
         hb_xfree( s_hbzaSettings.pszComment );
      }

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         szZip.SetCallback( &spanac );
      }

      try
      {
         if( hb_GetCurrentFileSize( szFiletoCompress ) != F_ERROR )
         {
            if( pBlock && HB_IS_BLOCK( pBlock ) )
            {
               PHB_ITEM FileName = hb_itemPutC( NULL, szFiletoCompress );
               hb_vmEvalBlockV( pBlock, 1, FileName );
               hb_itemRelease( FileName );
            }

            if( ! szZip.AddNewFile( szFiletoCompress, iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 ) )
               bReturn = FALSE;
         }
      }
      catch( ... )
      {}
   }

   try
   {
      szZip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return ( int ) bReturn;
}

int hb_CmpTdSpanStd( char * szFile, char * szFiletoCompress, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassWord, int iSpanSize, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFile( ( BYTE * ) szFile );

   CZipArchive szZip;
   SegmCallbackc span;
   SegmActionCallbackc spanac;

   szZip.SetSegmCallback( &span );

   if( iSpanSize == 0 )
      iSpanSize = 1457664;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         szZip.Open( szFile, CZipArchive::zipCreateSegm, iSpanSize );
      else
         return ( int ) false;
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   if( szPassWord )
      szZip.SetPassword( szPassWord );

   if( s_hbzaSettings.pszComment )
   {
      szZip.SetGlobalComment( s_hbzaSettings.pszComment );
      hb_xfree( s_hbzaSettings.pszComment );
   }

   if( pProgress && HB_IS_BLOCK( pProgress ) )
   {
      s_hbzaSettings.pProgressBlock = pProgress;
      szZip.SetCallback( &spanac );
   }

   if( bReturn )
   {
      try
      {
         if( szPassWord )
            szZip.SetPassword( szPassWord );

         if( pBlock && HB_IS_BLOCK( pBlock ) )
         {
            PHB_ITEM FileName = hb_itemPutC( NULL, szFiletoCompress );
            hb_vmEvalBlockV( pBlock, 1, FileName );
            hb_itemRelease( FileName );
         }

         if( ! szZip.AddNewFile( szFiletoCompress, iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 ) )
            bReturn = FALSE;
      }
      catch( ... )
      {}
   }

   try
   {
      szZip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return ( int ) bReturn;
}

int hb_CmpTdSpan( char * szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassWord, int iSpanSize, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFile( ( BYTE * ) szFile );

   CZipArchive szZip;
   SegmCallbackc span;
   SegmActionCallbackc spanac;

   szZip.SetSegmCallback( &span );

   if( iSpanSize == 0 )
      iSpanSize = 1457664;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         szZip.Open( szFile, CZipArchive::zipCreateSegm, iSpanSize );
      else
      {
         bReturn = FALSE;
         return ( int ) bReturn;
      }
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   /*if( ! bReturn )*/
   {
      ULONG ulCount;

      if( szPassWord )
         szZip.SetPassword( szPassWord );

      if( s_hbzaSettings.pszComment )
      {
         szZip.SetGlobalComment( s_hbzaSettings.pszComment );
         hb_xfree( s_hbzaSettings.pszComment );
      }

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         szZip.SetCallback( &spanac );
      }

      for( ulCount = 1; ulCount <= hb_arrayLen( pArray ); ulCount++ )
      {
         const char * szDummy = ( char * ) hb_arrayGetCPtr( pArray, ulCount );

         if( hb_GetCurrentFileSize( szDummy ) != F_ERROR )
         {
            if( pBlock && HB_IS_BLOCK( pBlock ) )
            {
               PHB_ITEM FileName = hb_itemPutC( NULL, szDummy );
               PHB_ITEM FilePos = hb_itemPutNI( NULL, ulCount );
               hb_vmEvalBlockV( pBlock, 2, FileName, FilePos );
               hb_itemRelease( FileName );
               hb_itemRelease( FilePos );
            }

            try
            {
               szZip.AddNewFile( szDummy, iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 );
            }
            catch( ... )
            {}
         }
      }
   }

   try
   {
      szZip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return ( int ) bReturn;
}

#ifdef __cplusplus
}
#endif
