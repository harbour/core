/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zlib low level interface for Harbour
 *
 * Copyright 2000-2001 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2,  or ( at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not,  write to
 * the Free Software Foundation,  Inc.,  59 Temple Place,  Suite 330,
 * Boston,  MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
 *
 * As a special exception,  the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that,  if you link the Harbour libraries with other
 * files to produce an executable,  this does not by itself cause the
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
 * Harbour,  as the General Public License permits,  the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files,  you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour,  it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that,  delete this exception notice.
 *
 */

#define HB_OS_WIN_32_USED

#include "hbziparc.h"

#ifdef __cplusplus
extern "C" {
#endif

class SpanCallbackc : public CZipSpanCallback
{
   bool Callback( int iProgress )
   {
      PHB_ITEM Disk=hb_itemPutNL( NULL, m_uDiskNeeded );
        
      hb_vmEvalBlockV( hbza_ChangeDiskBlock, 1, Disk  );
      hb_itemRelease( Disk );

      return TRUE;
   }
};

class SpanActionCallbackc : public CZipActionCallback
{
   bool Callback( int iProgress )
   {
      PHB_ITEM Disk = hb_itemPutNL(NULL, m_uTotalSoFar ),  Total=hb_itemPutNL( NULL, m_uTotalToDo );

      hb_vmEvalBlockV( hbza_pProgressInfo, 2, Disk,Total);

      hb_itemRelease( Disk );
      hb_itemRelease( Total );

      return TRUE;
   }
};

static DWORD hb_GetCurrentFileSize( LPCTSTR szFile )
#if defined( HB_OS_WIN_32 ) || defined( __MINGW32__ )
{
   DWORD dwFlags = FILE_ATTRIBUTE_ARCHIVE;
   HANDLE hFind;
   WIN32_FIND_DATA  hFilesFind;

   hFind = FindFirstFile( szFile, &hFilesFind );

   if ( hFind != INVALID_HANDLE_VALUE )
   {
      if ( dwFlags & hFilesFind.dwFileAttributes )
      {
         if( hFilesFind.nFileSizeHigh>0 )
         {
            return ( ( hFilesFind.nFileSizeHigh*MAXDWORD )+hFilesFind.nFileSizeLow );
         }
         else
         {
            return ( hFilesFind.nFileSizeLow );
         }

      }
   }

   FindClose( hFind );

   return ( DWORD ) -1;

}
#elif defined( __GNUC__ )
{
   USHORT   ushbMask   = 63;
   USHORT   usFileAttr = HB_FA_ARCHIVE;
   struct stat sStat;

   if ( stat( szFile, &sStat ) !=  -1 )
   {
      return sStat.st_size;
   }

   return -1;
}

#endif

int hb_CompressFile( char *szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   ULONG ulCount;
   const char *szDummy ;
   char *szDummyLower ;
   char *szFileLower = hb_strdup((char *)szFile) ;
   BOOL bFileExist = hb_fsFile( ( BYTE* )szFile );
   BOOL bAdded;
   BOOL bReturn    = TRUE;
   DWORD dwSize;

   CZipArchive szZip;
   SpanCallbackc span;
   SpanActionCallbackc spanac;

   szZip.SetSpanCallback( &span );

   #ifdef HB_OS_WIN_32
   hb_strLower( szFileLower, strlen( szFileLower )) ;
   #endif

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         szZip.Open( szFile, CZipArchive::zipCreate, 0 );
      }
      else
      {
         szZip.Open( szFile, CZipArchive::zipOpen, 0 );
      }
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( bReturn )
   {

      if ( szPassWord != NULL )
      {
         szZip.SetPassword( szPassWord );
      }

      if ( hbza_pZipI.szComment != NULL )
      {
         szZip.SetGlobalComment( hbza_pZipI.szComment );
         hb_xfree( hbza_pZipI.szComment );
      }

      if ( HB_IS_BLOCK( pProgress ) )
      {
         hbza_pProgressInfo = pProgress;
         szZip.SetCallback( &spanac );
      }

      for ( ulCount = 1; ( ulCount <= hb_arrayLen( pArray ) ) ;ulCount++ )
      {
         szDummy = ( char * ) hb_arrayGetCPtr( pArray, ulCount ) ;
         dwSize  = hb_GetCurrentFileSize( szDummy );
         bAdded  = FALSE;

         szDummyLower = hb_strdup( (char *)szDummy ) ;

         #ifdef HB_OS_WIN_32
         hb_strLower( szDummyLower, strlen( szDummyLower )) ;
         #endif

// Prevent adding current archive file !
         if ( strstr( szFileLower, szDummyLower ) == NULL && strstr( szDummyLower, szFileLower ) == NULL)
         {
            if( dwSize != (DWORD) -1 )
            {
               if( pBlock != NULL )
               {
                  PHB_ITEM FileName=hb_itemPutC(NULL,hb_arrayGetCPtr(pArray,ulCount)), FilePos = hb_itemPutNI(NULL,ulCount);


                  hb_vmEvalBlockV( pBlock, 2, FileName,FilePos );

                  hb_itemRelease( FileName );
                  hb_itemRelease( FilePos );
               }

               try
               {
                  if ( bPath && !bAdded  )
                  {
                     szZip.AddNewFile( szDummy, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
                     // bAdded = TRUE;
                  }
                  else if ( !bDrive && !bPath && !bAdded  )
                  {
                     szZip.AddNewFile( szDummy, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 );
                  }

               }
               catch( ... ){}
            }
         }
         hb_xfree( szDummyLower ) ;
      }
   }
   hb_xfree( szFileLower ) ;
   try
   {
      szZip.Close(  );
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   return ( int ) bReturn;
}

int hb_CmpTdSpan( char *szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, int iSpanSize, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   ULONG ulCount;
   const char *szDummy;
   DWORD dwSize;
   BOOL bAdded;
   BOOL bReturn    = TRUE;
   BOOL bFileExist = hb_fsFile( ( BYTE* )szFile );

   CZipArchive szZip;
   SpanCallbackc span;
   SpanActionCallbackc spanac;

   szZip.SetSpanCallback( &span );


   if ( iSpanSize  == 0 )
   {
      iSpanSize = 1457664;
   }

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         szZip.Open( szFile, CZipArchive::zipCreateSpan, iSpanSize );
      }
      else
      {
         bReturn = FALSE;
         return ( int ) bReturn;
      }
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   //if (! bReturn )
   //{

      if ( szPassWord != NULL )
      {
         szZip.SetPassword( szPassWord );
      }

      if ( hbza_pZipI.szComment != NULL )
      {
         szZip.SetGlobalComment( hbza_pZipI.szComment );
         hb_xfree( hbza_pZipI.szComment );
      }

      if ( HB_IS_BLOCK( pProgress ) )
      {
         hbza_pProgressInfo = pProgress;
         szZip.SetCallback( &spanac );
      }

      for ( ulCount = 1;( ulCount<= hb_arrayLen( pArray ) ) ;ulCount++ )
      {
         szDummy     = ( char * )hb_arrayGetCPtr( pArray, ulCount ) ;
         dwSize      = hb_GetCurrentFileSize( szDummy );

         bAdded      = FALSE;

         if( dwSize != (DWORD) -1 )
         {
            if( pBlock != NULL )
            {
               PHB_ITEM FileName=hb_itemPutC(NULL,hb_arrayGetCPtr(pArray,ulCount)), FilePos = hb_itemPutNI(NULL,ulCount);

               hb_vmEvalBlockV( pBlock, 2, FileName,FilePos);

               hb_itemRelease( FileName );
               hb_itemRelease( FilePos );
            }

            try
            {

               if ( bPath && !bAdded  )
               {
                  szZip.AddNewFile( szDummy, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
                  // bAdded = TRUE;
               }
               else if ( !bDrive && !bPath && !bAdded  )
               {
                  szZip.AddNewFile( szDummy, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 );
               }

            }
            catch( ... ){}
         }
      }
   //}

   try
   {
      szZip.Close();
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}


   return ( int ) bReturn;
}

int hb_CompressFileStd( char *szFile, char *szFiletoCompress, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   DWORD dwSize;
   BOOL bFileExist = hb_fsFile( ( BYTE* )szFile );
   BOOL bReturn    = TRUE;
   BOOL bAdded     = FALSE;

   CZipArchive szZip;
   SpanCallbackc span;
   SpanActionCallbackc spanac;

   szZip.SetSpanCallback( &span );

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         szZip.Open( szFile, CZipArchive::zipCreate, 0 );
      }
      else
      {
         szZip.Open( szFile, CZipArchive::zipOpen, 0 );
      }
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   if ( bReturn )
   {
      if ( szPassWord != NULL )
      {
         szZip.SetPassword( szPassWord );
      }

      if ( hbza_pZipI.szComment != NULL )
      {
         szZip.SetGlobalComment( hbza_pZipI.szComment );
         hb_xfree( hbza_pZipI.szComment );
      }

      if ( HB_IS_BLOCK( pProgress ) )
      {
         hbza_pProgressInfo = pProgress;
         szZip.SetCallback( &spanac );
      }

      try
      {
         dwSize = hb_GetCurrentFileSize( szFiletoCompress );

         if ( dwSize != (DWORD) -1 )
         {

            if( pBlock  != NULL )
            {
               PHB_ITEM FileName=hb_itemPutC( NULL, szFiletoCompress ) ;

               hb_vmEvalBlockV( pBlock, 1, FileName);

               hb_itemRelease( FileName );
               
            }

            #if ( defined( __WIN32__ ) || defined( __MINGW32__ ) ) && defined( HB_USE_DRIVE_ADD )
               if ( bDrive && !bAdded  )
               {
                  if (! szZip.AddNewFileDrv( szFiletoCompress, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 ) )
                  {
                     bReturn = FALSE;
                  }
                  else
                  {
                     bAdded = TRUE;
                  }
               }
            #endif

            if ( bPath && !bAdded  )
            {
               if( ! szZip.AddNewFile( szFiletoCompress, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 ))
               {
                  bReturn = FALSE;
               }
               else
               {
                  // bAdded = TRUE;
               }
            }
            else if ( !bDrive && !bPath && !bAdded  )
            {
               if (! szZip.AddNewFile( szFiletoCompress, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 ))
               {
                  bReturn = FALSE;
               }
            }


         }
      }
      catch( ... ){}
   }
   try
   {
      szZip.Close(  );
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   return ( int ) bReturn;
}

int hb_CmpTdSpanStd( char *szFile, char * szFiletoCompress, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, int iSpanSize, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   BOOL bAdded     = FALSE;
   BOOL bReturn    = TRUE;
   BOOL bFileExist = hb_fsFile( ( BYTE* )szFile );

   CZipArchive szZip;
   SpanCallbackc span;
   SpanActionCallbackc spanac;

   szZip.SetSpanCallback( &span );


   if ( iSpanSize  == 0 )
   {
      iSpanSize = 1457664;
   }

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         szZip.Open( szFile, CZipArchive::zipCreateSpan, iSpanSize );
      }
      else
      {
         return ( int ) false;
      }
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( szPassWord != NULL )
   {
      szZip.SetPassword( szPassWord );
   }

   if ( hbza_pZipI.szComment != NULL )
   {
      szZip.SetGlobalComment( hbza_pZipI.szComment );
      hb_xfree( hbza_pZipI.szComment );
   }

   if ( HB_IS_BLOCK( pProgress ) )
   {
      hbza_pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }
   if ( bReturn )
   {
      try
      {
         if ( szPassWord != NULL )
         {
            szZip.SetPassword( szPassWord );
         }

         if( pBlock  != NULL )
         {
            PHB_ITEM FileName=hb_itemPutC( NULL, szFiletoCompress  ) ;

            hb_vmEvalBlockV(  pBlock, 1, FileName);

            hb_itemRelease( FileName );
           
         }

         #if defined( __WIN32__ ) || defined( __MINGW32__ )
            if ( bDrive && !bAdded  )
            {
               if (! szZip.AddNewFileDrv( szFiletoCompress, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 ) )
               {
                  bReturn = FALSE;
               }
               else
               {
                  bAdded = TRUE;
               }
            }
         #endif

         if ( bPath && !bAdded )
         {
            if (! szZip.AddNewFile( szFiletoCompress, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 ) )
            {
               bReturn = FALSE;
            }
            else
            {
               // bAdded = TRUE;
            }
         }
         else if ( !bDrive && !bPath && !bAdded  )
         {
            if (! szZip.AddNewFile( szFiletoCompress, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 ) )
            {
               bReturn = FALSE;
            }
         }

      }
      catch( ... ){}
   }

   try
   {
      szZip.Close(  );
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   return ( int ) bReturn;
}

#ifdef __cplusplus
}
#endif
