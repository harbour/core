/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ZipArchive low level interface for Harbour
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

#define HBZA_FI_FILENAME   1
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

class CHBZipSegmCallback : public CZipSegmCallback
{
   bool Callback( ZIP_SIZE_TYPE iProgress )
   {
      PHB_ITEM pDisk = hb_itemPutNL( NULL, m_uVolumeNeeded );
      HB_SYMBOL_UNUSED( iProgress );
      hb_vmEvalBlockV( s_hbzaSettings.pChangeDiskBlock, 1, pDisk );
      hb_itemRelease( pDisk );

      return true;
   }
};

class CHBZipActionCallback : public CZipActionCallback
{
   bool Callback( ZIP_SIZE_TYPE iProgress )
   {
      PHB_ITEM pDisk = hb_itemPutNL( NULL, m_uProcessed );
      PHB_ITEM pTotal = hb_itemPutNL( NULL, m_uTotalToProcess );
      HB_SYMBOL_UNUSED( iProgress );
      hb_vmEvalBlockV( s_hbzaSettings.pProgressBlock, 2, pDisk, pTotal );
      hb_itemRelease( pDisk );
      hb_itemRelease( pTotal );

      return true;
   }
};

int hb_CheckSpanMode( char * szFile )
{
   CZipArchive myzip;
   int iReturn = 0;

   CHBZipSegmCallback cb_Segm;
   myzip.SetSegmCallback( &cb_Segm );

   try
   {
      myzip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 ); /* s.r. to avoid GPF when ZIP file is read only ! */
   }
   catch( CZipException &e )
   {
      switch( e.m_iCause )
      {
      case CZipException::genericError  : iReturn = 100; break;
      case CZipException::badZipFile    : iReturn = 101; break;
      case CZipException::badCrc        : iReturn = 102; break;
      case CZipException::noCallback    : iReturn = 103; break;
      case CZipException::aborted       : iReturn = 104; break;
      case CZipException::abortedAction : iReturn = 105; break;
      case CZipException::abortedSafely : iReturn = 106; break;
      case CZipException::nonRemovable  : iReturn = 107; break;
      case CZipException::tooManyVolumes: iReturn = 108; break;
      case CZipException::tooLongData   : iReturn = 109; break;
      case CZipException::badPassword   : iReturn = 110; break;
      case CZipException::dirWithSize   : iReturn = 111; break;
      case CZipException::internalError : iReturn = 112; break;
      case CZipException::notRemoved    : iReturn = 113; break;
      case CZipException::notRenamed    : iReturn = 114; break;
      case CZipException::platfNotSupp  : iReturn = 115; break;
      case CZipException::cdirNotFound  : iReturn = 116; break;
      }
   }

   if( iReturn == 0 )
   {
      iReturn = myzip.GetSegmMode();
      myzip.Close();
   }
   else
      myzip.Close( true );

   return iReturn;
}

static BOOL hb_zipopenread( CZipArchive * myzip, CHBZipSegmCallback * cb_Segm, char * szFile )
{
   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            myzip->Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         {
            myzip->SetSegmCallback( cb_Segm );
            myzip->Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;
         }
         case -2:
            myzip->Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            return FALSE;
      }
   }
   catch( ... )
   {}

   return TRUE;
}

BOOL hb_IsPassWord( char * szFile )
{
   CZipArchive myzip;
   CHBZipSegmCallback cb_Segm;
   BOOL bReturn = hb_zipopenread( &myzip, &cb_Segm, szFile );

   if( bReturn )
   {
      CZipFileHeader fh;

      myzip.GetFileInfo( fh, 0 );

      bReturn = fh.IsEncrypted();

      myzip.Close();
   }

   return bReturn;
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

BOOL hb_SetCallbackFunc( PHB_ITEM pChangeDiskBlock )
{
   if( s_hbzaSettings.pChangeDiskBlock )
      hb_itemClear( s_hbzaSettings.pChangeDiskBlock );
   else
   {
      /* register cleanup function, it's executed only once */
      hb_vmAtExit( hb_CallbackFuncFree, NULL );
      s_hbzaSettings.pChangeDiskBlock = hb_itemNew( NULL );
   }

   if( pChangeDiskBlock )
      hb_itemCopy( s_hbzaSettings.pChangeDiskBlock, pChangeDiskBlock );

   return TRUE;
}

void hb_SetZipBuff( int a, int b, int c )
{
   s_hbzaSettings.iWrite   = HB_MIN( a, 65535 );
   s_hbzaSettings.iExtract = HB_MIN( b, 16384 );
   s_hbzaSettings.iRead    = HB_MIN( c, 32768 );
}

void hb_SetZipReadOnly( int iRead )
{
   s_hbzaSettings.iReadOnly = iRead;
}

void hb_SetZipComment( char * szComment )
{
   s_hbzaSettings.pszComment = hb_strdup( szComment );
}

char * hb_GetZipComment( char * szFile )
{
   CZipArchive myzip;
   CHBZipSegmCallback cb_Segm;
   char * szTemp;

   szTemp = hb_strdup( hb_zipopenread( &myzip, &cb_Segm, szFile ) ? ( const char * ) myzip.GetGlobalComment() : "" );

   myzip.Close();

   return szTemp;
}

PHB_ITEM hb_GetFileNamesFromZip( char * szFile, BOOL bVerbose )
{
   CZipArchive myzip;
   CHBZipSegmCallback cb_Segm;
   BOOL bReturn = hb_zipopenread( &myzip, &cb_Segm, szFile );
   PHB_ITEM pZipArray;

   if( bReturn )
   {
      ZIP_INDEX_TYPE nZipFCount = myzip.GetCount();
      ZIP_INDEX_TYPE nZipFPos;

      if( s_hbzaSettings.iWrite > 0 )
         myzip.SetAdvanced( s_hbzaSettings.iWrite, s_hbzaSettings.iExtract, s_hbzaSettings.iRead );

      pZipArray = hb_itemArrayNew( nZipFCount );

      for( nZipFPos = 0; nZipFPos < nZipFCount; nZipFPos++ )
      {
         CZipFileHeader fh;
        
         myzip.GetFileInfo( fh, nZipFPos );

         if( bVerbose )
         {
            PHB_ITEM pTempArray = hb_itemArrayNew( 9 );
            char szAttr[ 5 ];
            char szTime[ 9 ];
            char * szMethod = "Unknown";
            char szCRC[ 9 ];
            int iRatio;

            hb_arraySetC( pTempArray, HBZA_FI_FILENAME, ( char * ) ( LPCTSTR ) fh.GetFileName() );

            if( fh.m_uUncomprSize > 0 )
            {
               iRatio = 100 - ( ( fh.m_uComprSize * 100 ) / fh.m_uUncomprSize );

               if( iRatio < 0 )
                  iRatio = 0;

               hb_arraySetNL( pTempArray, HBZA_FI_LENGTH , fh.m_uUncomprSize );
               hb_arraySetNL( pTempArray, HBZA_FI_SIZE   , fh.m_uComprSize );
               hb_arraySetNL( pTempArray, HBZA_FI_RATIO  , iRatio );
            }
            else
            {
               iRatio = 0;
               hb_arraySetNL( pTempArray, HBZA_FI_LENGTH , fh.m_uUncomprSize );
               hb_arraySetNL( pTempArray, HBZA_FI_SIZE   , fh.m_uComprSize );
               hb_arraySetNL( pTempArray, HBZA_FI_RATIO  , iRatio );
            }

            #if defined( HB_OS_WIN_32 )
            {
               DWORD uAttr = fh.GetSystemAttr();
               szAttr[ 0 ] = uAttr & FILE_ATTRIBUTE_READONLY  ? 'r' : '-';
               szAttr[ 1 ] = uAttr & FILE_ATTRIBUTE_HIDDEN    ? 'h' : '-';
               szAttr[ 2 ] = uAttr & FILE_ATTRIBUTE_SYSTEM    ? 's' : 'w';
               szAttr[ 3 ] = uAttr & FILE_ATTRIBUTE_DIRECTORY ? 'D' : uAttr & FILE_ATTRIBUTE_ARCHIVE ? 'a' : '-';

               if( fh.m_uMethod == 0 || uAttr & FILE_ATTRIBUTE_DIRECTORY )
                  szMethod = "Stored";
            }
            #else
               szAttr[ 0 ] =
               szAttr[ 1 ] =
               szAttr[ 2 ] =
               szAttr[ 3 ] = ' ';
            #endif

            szAttr[ 4 ] = fh.IsEncrypted() ? '*' : ' ';

            if( fh.m_uMethod == Z_DEFLATED )
            {
               UINT iLevel = ( UINT ) ( ( fh.m_uFlag & 0x6 ) / 2 );

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
               }
            }
            
            hb_arraySetC( pTempArray, HBZA_FI_METHOD, szMethod );

            snprintf( szCRC, sizeof( szCRC ), "%8.8lx", ( ULONG ) fh.m_uCrc32 );

            hb_arraySetCL( pTempArray, HBZA_FI_CRC32, szCRC, 8 );
            hb_arraySetDL( pTempArray, HBZA_FI_DATE, hb_dateEncode( ( LONG ) ( fh.m_uModDate >> 9 ) + 1980, ( LONG ) ( ( fh.m_uModDate & ~0xFE00 ) >> 5 ), ( LONG ) fh.m_uModDate & ~0xFFE0 ) );

            {
               time_t theTime = fh.GetTime();
               tm * SzTime = localtime( &theTime );

               snprintf( szTime, sizeof( szTime ), "%02d:%02d:%02d", SzTime->tm_hour, SzTime->tm_min, SzTime->tm_sec );
            }

            hb_arraySetCL( pTempArray, HBZA_FI_TIME, szTime, 8 );
            hb_arraySetCL( pTempArray, HBZA_FI_ATTR, szAttr, 5 );
            hb_arraySetForward( pZipArray, ( ULONG ) nZipFPos + 1, pTempArray );
            hb_itemRelease( pTempArray );
         }
         else
            hb_arraySetC( pZipArray, ( ULONG ) nZipFPos + 1, ( char * ) ( LPCTSTR ) fh.GetFileName() );
      }

      myzip.Close();
   }
   else
      pZipArray = hb_itemArrayNew( 0 );

   return pZipArray;
}

ULONG hb_GetNumberofFilestoUnzip( char * szFile )
{
   CZipArchive myzip;
   ZIP_INDEX_TYPE nZipFCount = 0;

   CHBZipSegmCallback cb_Segm;
   myzip.SetSegmCallback( &cb_Segm );

   try
   {
      myzip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
      nZipFCount = myzip.GetCount();
      myzip.Close();
   }
   catch( CZipException &e )
   {
      if( e.m_iCause == CZipException::badZipFile || e.m_iCause == CZipException::cdirNotFound )
         nZipFCount = ( ZIP_INDEX_TYPE ) -1;
   }

   return ( ULONG ) nZipFCount;
}

BOOL hb_DeleteSel( char * szFile, PHB_ITEM pArray )
{
   CZipArchive myzip;
   BOOL bReturn = TRUE;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            myzip.Open( szFile, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
/*       default: */
            bReturn = FALSE;
      }
   }
   catch( CZipException )
   {}

   if( bReturn )
   {
      CZipStringArray aFiles;
      ULONG nPos;

      for( nPos = 1; nPos <= hb_arrayLen( pArray ); nPos ++ )
         aFiles.Add( hb_arrayGetCPtr( pArray, nPos ) );

      myzip.RemoveFiles( aFiles );

      myzip.Close();
   }

   return bReturn;
}

BOOL hb_UnzipAll( char * szFile, PHB_ITEM pBlock, BOOL bWithPath, char * szPassword, char * pbyBuffer, PHB_ITEM pDiskBlock, PHB_ITEM pProgress )
{
   CZipArchive myzip;
   CHBZipSegmCallback cb_Segm;
   BOOL bReturn = hb_zipopenread( &myzip, &cb_Segm, szFile );

   HB_SYMBOL_UNUSED( pDiskBlock );

   if( bReturn )
   {
      CHBZipActionCallback cb_Action;
      char * pszPath = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
      ZIP_INDEX_TYPE uiCount;

      if( szPassword )
         myzip.SetPassword( szPassword );

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         myzip.SetCallback( &cb_Action );
      }

      hb_strncpy( pszPath, pbyBuffer && strcmp( pbyBuffer, ".\\" ) ? pbyBuffer : "", _POSIX_PATH_MAX );

      myzip.SetRootPath( pszPath );

      for( uiCount = 0; uiCount < myzip.GetCount(); uiCount++ )
      {
         CZipFileHeader fh;
         myzip.GetFileInfo( fh, uiCount );
      
         if( pBlock && HB_IS_BLOCK( pBlock ) )
         {
            PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * ) ( LPCTSTR ) fh.GetFileName() );
            PHB_ITEM pFilePos = hb_itemPutNL( NULL, ( LONG ) uiCount );
            hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
            hb_itemRelease( pFileName );
            hb_itemRelease( pFilePos );
         }
      
         try
         {
            myzip.SetPassword( szPassword );
            myzip.ExtractFile( uiCount, ( LPCTSTR ) pszPath, bWithPath ? true : false, NULL, 65536 );
         }
         catch( ... )
         {
            myzip.CloseFile( NULL, true );
         }
      }

      hb_xfree( pszPath );

      myzip.Close();
   }

   return bReturn;
}

BOOL hb_UnzipSel( char * szFile, PHB_ITEM pBlock, BOOL lWithPath, char * szPassword, const char * pbyBuffer, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   CZipArchive myzip;
   CHBZipSegmCallback cb_Segm;
   BOOL bReturn = hb_zipopenread( &myzip, &cb_Segm, szFile );

   if( bReturn )
   {
      CHBZipActionCallback cb_Action;
      char * pszPath = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
      ULONG nPos;

      if( szPassword )
         myzip.SetPassword( szPassword );

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         myzip.SetCallback( &cb_Action );
      }

      hb_strncpy( pszPath, pbyBuffer && strcmp( pbyBuffer, ".\\" ) ? pbyBuffer : "", _POSIX_PATH_MAX );

      myzip.SetRootPath( pszPath );

      for( nPos = 1; nPos <= hb_arrayLen( pSelArray ); nPos++ )
      {
         ZIP_INDEX_TYPE nZipFPos = myzip.FindFile( ( LPCTSTR ) hb_arrayGetCPtr( pSelArray, nPos ), false );

         if( nZipFPos == ( ZIP_INDEX_TYPE ) -1 )
            nZipFPos = myzip.FindFile( ( LPCTSTR ) hb_arrayGetCPtr( pSelArray, nPos ), true );

         if( nZipFPos != ( ZIP_INDEX_TYPE ) -1 )
         {
            CZipFileHeader fh;
            myzip.GetFileInfo( fh, nZipFPos );

            if( pBlock && HB_IS_BLOCK( pBlock ) )
            {
               PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * ) ( LPCTSTR ) fh.GetFileName() );
               PHB_ITEM pFilePos = hb_itemPutNI( NULL, nPos );
               hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
               hb_itemRelease( pFileName );
               hb_itemRelease( pFilePos );
            }

            try
            {
               myzip.ExtractFile( nZipFPos, ( LPCTSTR ) pszPath, lWithPath ? true : false, NULL, 65536 );
            }
            catch( ... )
            {
               bReturn = FALSE;
               myzip.CloseFile( NULL, true );
            }
         }
      }

      hb_xfree( pszPath );

      myzip.Close();
   }

   return bReturn;
}

BOOL hb_UnzipSelIndex( char * szFile, PHB_ITEM pBlock, BOOL lWithPath, char * szPassword, char * pszPath, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   CZipArchive myzip;
   CHBZipSegmCallback cb_Segm;
   BOOL bReturn = hb_zipopenread( &myzip, &cb_Segm, szFile );

   if( bReturn )
   {
      CHBZipActionCallback cb_Action;
      ULONG nPos;

      if( szPassword )
         myzip.SetPassword( szPassword );

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         myzip.SetCallback( &cb_Action );
      }

      for( nPos = 0; nPos <= hb_arrayLen( pSelArray ); nPos++ )
      {
         ZIP_INDEX_TYPE nZipFPos = ( ZIP_INDEX_TYPE ) hb_arrayGetNL( pSelArray, nPos ) - 1;

         CZipFileHeader fh;

         myzip.GetFileInfo( fh, nZipFPos );

         if( pBlock && HB_IS_BLOCK( pBlock ) )
         {
            PHB_ITEM pFileName = hb_itemPutC( NULL, ( char * ) ( LPCTSTR ) fh.GetFileName() );
            hb_vmEvalBlockV( pBlock, 1, pFileName );
            hb_itemRelease( pFileName );
         }

         try
         {
            myzip.ExtractFile( nZipFPos, ( LPCTSTR ) pszPath, lWithPath ? true : false, NULL, 65536 );
         }
         catch( ... )
         {
            bReturn = FALSE;
            myzip.CloseFile( NULL, true );
         }
      }

      myzip.Close();
   }

   return bReturn;
}

BOOL hb_TransferFilesFromzip( char * szSource, char * szDest, PHB_ITEM pArray )
{
   CZipArchive myzipSource;
   CZipArchive myzipDest;
   CZipStringArray aFiles;
   BOOL bReturn1 = TRUE;
   BOOL bReturn2 = TRUE;

   try
   {
      switch( hb_CheckSpanMode( szSource ) )
      {
         case 0:
            myzipSource.Open( szSource, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
/*       default: */
            bReturn1 = FALSE;
      }
   }
   catch( CZipException )
   {}

   try
   {
      switch( hb_CheckSpanMode( szDest ) )
      {
         case 0:
            myzipDest.Open( szDest, s_hbzaSettings.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
/*       default: */
            bReturn2 = FALSE;
      }
   }
   catch( CZipException )
   {}

   if( bReturn1 && bReturn2 )
   {
      ULONG nPos;

      for( nPos = 1; nPos <= hb_arrayLen( pArray ); nPos++ )
         aFiles.Add( hb_arrayGetCPtr( pArray, nPos ) );

      myzipDest.GetFromArchive( myzipSource, aFiles, false );

      myzipDest.Close();
      myzipSource.Close();

      return TRUE;
   }

   return FALSE;
}

BOOL hb_CompressFile( char * szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassword, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   CZipArchive myzip;
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFileExists( szFile );
   char * szFileLower = hb_strdup( ( char * ) szFile );

   CHBZipSegmCallback cb_Segm;
   myzip.SetSegmCallback( &cb_Segm );

   #ifdef HB_OS_WIN_32
      hb_strLower( szFileLower, strlen( szFileLower ) );
   #endif

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         myzip.Open( szFile, CZipArchive::zipCreate, 0 );
      else
         myzip.Open( szFile, CZipArchive::zipOpen, 0 );
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   if( bReturn )
   {
      CHBZipActionCallback cb_Action;
      ULONG nPos;

      if( szPassword )
         myzip.SetPassword( szPassword );

      if( s_hbzaSettings.pszComment )
      {
         myzip.SetGlobalComment( s_hbzaSettings.pszComment );
         hb_xfree( s_hbzaSettings.pszComment );
      }

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         myzip.SetCallback( &cb_Action );
      }

      for( nPos = 1; nPos <= hb_arrayLen( pArray ); nPos++ )
      {
         const char * szDummy = hb_arrayGetCPtr( pArray, nPos );
         char * szDummyLower = hb_strdup( szDummy );

         #ifdef HB_OS_WIN_32
            hb_strLower( szDummyLower, strlen( szDummyLower ) );
         #endif

         /* Prevent adding current archive file. */
         /* TOFIX: strstr() is not suitable for portable filename comparison. */
         if( ! strstr( szFileLower, szDummyLower ) && 
             ! strstr( szDummyLower, szFileLower ) )
         {
            if( hb_fsFileExists( szDummy ) )
            {
               if( pBlock && HB_IS_BLOCK( pBlock ) )
               {
                  PHB_ITEM pFileName = hb_itemPutC( NULL, szDummy );
                  PHB_ITEM pFilePos = hb_itemPutNI( NULL, nPos );
                  hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
                  hb_itemRelease( pFileName );
                  hb_itemRelease( pFilePos );
               }

               try
               {
                  myzip.AddNewFile( szDummy, iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 );
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
      myzip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return bReturn;
}

BOOL hb_CompressFileStd( char * szFile, char * szFiletoCompress, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassword, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   CZipArchive myzip;
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFileExists( szFile );

   CHBZipSegmCallback cb_Segm;
   myzip.SetSegmCallback( &cb_Segm );

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         myzip.Open( szFile, CZipArchive::zipCreate, 0 );
      else
         myzip.Open( szFile, CZipArchive::zipOpen, 0 );
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }

   if( bReturn )
   {
      CHBZipActionCallback cb_Action;

      if( szPassword )
         myzip.SetPassword( szPassword );

      if( s_hbzaSettings.pszComment )
      {
         myzip.SetGlobalComment( s_hbzaSettings.pszComment );
         hb_xfree( s_hbzaSettings.pszComment );
      }

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         myzip.SetCallback( &cb_Action );
      }

      try
      {
         if( hb_fsFileExists( szFiletoCompress ) )
         {
            if( pBlock && HB_IS_BLOCK( pBlock ) )
            {
               PHB_ITEM pFileName = hb_itemPutC( NULL, szFiletoCompress );
               hb_vmEvalBlockV( pBlock, 1, pFileName );
               hb_itemRelease( pFileName );
            }

            if( ! myzip.AddNewFile( szFiletoCompress, iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 ) )
               bReturn = FALSE;
         }
      }
      catch( ... )
      {}
   }

   try
   {
      myzip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return bReturn;
}

BOOL hb_CmpPkSpan( char * szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassword, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   CZipArchive myzip;
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFileExists( szFile );

   CHBZipSegmCallback cb_Segm;
   myzip.SetSegmCallback( &cb_Segm );

   bDrive = FALSE;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         myzip.Open( szFile, CZipArchive::zipCreateSegm, 0 );
      else
         bReturn = FALSE;
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   if( bReturn )
   {
      CHBZipActionCallback cb_Action;
      ULONG nPos;

      if( szPassword )
         myzip.SetPassword( szPassword );
      
      if( s_hbzaSettings.pszComment )
      {
         myzip.SetGlobalComment( s_hbzaSettings.pszComment );
         hb_xfree( s_hbzaSettings.pszComment );
      }
      
      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         myzip.SetCallback( &cb_Action );
      }
      
      for( nPos = 1; nPos <= hb_arrayLen( pArray ); nPos++ )
      {
         if( pBlock && HB_IS_BLOCK( pBlock ) )
         {
            PHB_ITEM pFileName = hb_itemPutC( NULL, hb_arrayGetCPtr( pArray, nPos ) );
            PHB_ITEM pFilePos = hb_itemPutNI( NULL, nPos );
            hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
            hb_itemRelease( pFileName );
            hb_itemRelease( pFilePos );
         }
      
         try
         {
            myzip.AddNewFile( hb_arrayGetCPtr( pArray, nPos ), iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 );
         }
         catch( ... )
         {}
      }
   }

   try
   {
      myzip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return bReturn;
}

BOOL hb_CmpTdSpan( char * szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char * szPassword, long iSpanSize, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   CZipArchive myzip;
   BOOL bReturn = TRUE;
   BOOL bFileExist = hb_fsFileExists( szFile );

   CHBZipSegmCallback cb_Segm;
   myzip.SetSegmCallback( &cb_Segm );

   if( iSpanSize == 0 )
      iSpanSize = 1457664;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
         myzip.Open( szFile, CZipArchive::zipCreateSegm, iSpanSize );
      else
         bReturn = FALSE;
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   if( bReturn )
   {
      CHBZipActionCallback cb_Action;
      ULONG nPos;

      if( szPassword )
         myzip.SetPassword( szPassword );

      if( s_hbzaSettings.pszComment )
      {
         myzip.SetGlobalComment( s_hbzaSettings.pszComment );
         hb_xfree( s_hbzaSettings.pszComment );
      }

      if( pProgress && HB_IS_BLOCK( pProgress ) )
      {
         s_hbzaSettings.pProgressBlock = pProgress;
         myzip.SetCallback( &cb_Action );
      }

      for( nPos = 1; nPos <= hb_arrayLen( pArray ); nPos++ )
      {
         const char * szDummy = hb_arrayGetCPtr( pArray, nPos );

         if( hb_fsFileExists( szDummy ) )
         {
            if( pBlock && HB_IS_BLOCK( pBlock ) )
            {
               PHB_ITEM pFileName = hb_itemPutC( NULL, szDummy );
               PHB_ITEM pFilePos = hb_itemPutNI( NULL, nPos );
               hb_vmEvalBlockV( pBlock, 2, pFileName, pFilePos );
               hb_itemRelease( pFileName );
               hb_itemRelease( pFilePos );
            }

            try
            {
               myzip.AddNewFile( szDummy, iCompLevel, ( bDrive || bPath ) ? true : false, CZipArchive::zipsmSafeSmart, 65536 );
            }
            catch( ... )
            {}
         }
      }
   }

   try
   {
      myzip.Close();
   }
   catch( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... )
   {}

   return bReturn;
}

#ifdef __cplusplus
}
#endif
