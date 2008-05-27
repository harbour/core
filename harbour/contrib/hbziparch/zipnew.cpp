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

#include "hbzip2.h"
#include "hbapifs.h"

PHB_ITEM ZipArray;

extern PHB_ITEM ChangeDiskBlock;
extern PHB_ITEM pProgressInfo;

int hb_CheckSpanMode( char * szFile );

#ifdef __cplusplus
extern "C" {

bool hb_SetCallBack( DWORD iNumber, int, void* pData );

extern bool hb_SetProgressofTdSpan( DWORD, int iSoFar, void* pData );

HB_ZIP_INTERNAL pZipI;

#endif

class SpanCallback : public CZipSpanCallback
{
   bool Callback( int iProgress )
   {
      PHB_ITEM Disk = hb_itemPutNL( NULL, m_uDiskNeeded ) ;

      hb_vmEvalBlockV( ChangeDiskBlock, 1, Disk);

      hb_itemRelease( Disk );

      return true;
   }
};

class SpanActionCallback : public CZipActionCallback
{
   bool Callback( int iProgress )
   {
      PHB_ITEM Disk = hb_itemPutNL( NULL , m_uTotalSoFar ), Total= hb_itemPutNL( NULL, m_uTotalToDo );


      hb_vmEvalBlockV( pProgressInfo, 2, Disk, Total);

      hb_itemRelease( Disk );
      hb_itemRelease( Total );

      return true;
   }
};

int hb_CmpPkSpan( char *szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverWrite, char *szPassWord, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress )
{
   ULONG ulCount;

   const char *szDummy;

   BOOL bReturn = TRUE;
   BOOL bAdded;
   BOOL bFileExist = hb_fsFile( ( BYTE* )szFile );
   CZipString szArchive = szFile;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   szZip.SetSpanCallback( &span );
   bDrive = false;

   try
   {
      if( ( bFileExist && bOverWrite ) || !bFileExist )
      {
         szZip.Open( szArchive, CZipArchive::zipCreateSpan, 0 );
      }
      else
      {
         bReturn = FALSE;
      }
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }

   catch( ... ){}

   if ( ! bReturn )
   {
      return ( int ) bReturn;
   }

   if ( szPassWord != NULL )
   {
      szZip.SetPassword( szPassWord );
   }

   if ( pZipI.szComment != NULL )
   {
      szZip.SetGlobalComment( pZipI.szComment );
      hb_xfree( pZipI.szComment );
   }

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   for( ulCount = 1;( ulCount <= hb_arrayLen( pArray ) ); ulCount++ )
   {
      szDummy = ( char * )hb_arrayGetCPtr( pArray, ulCount );
      bAdded = FALSE;

      if( pBlock  !=  NULL )
      {
         PHB_ITEM FileName=hb_itemPutC( NULL, hb_arrayGetCPtr( pArray, ulCount ) ), FilePos=hb_itemPutNI( NULL , ulCount );
 

         hb_vmEvalBlockV( pBlock, 2, FileName,FilePos);

         hb_itemRelease( FileName );
         hb_itemRelease( FilePos );
      }
      try
      {

         if ( bPath && !bAdded )
         {
            szZip.AddNewFile( szDummy, iCompLevel, true, CZipArchive::zipsmSafeSmart, 65536 );
            bAdded = true;
         }
         else if ( !bDrive && !bPath && !bAdded )
         {
            szZip.AddNewFile( szDummy, iCompLevel, false, CZipArchive::zipsmSafeSmart, 65536 );
         }

      }
      catch( ... ){}
   }

   try
   {
      szZip.Close( );
   }

   catch ( CZipException )
   {
      bReturn = FALSE;
   }
   catch( ... ){}

   return ( int ) bReturn;
}

PHB_ITEM hb___GetFileNamesFromZip( char *szFile, BOOL iMode )
{
   int iNumberOfFiles;
   ULONG ulCount;
   bool iReturn = true;

   CZipArchive szZip;
   SpanCallback span;

   if ( pZipI.iWrite > 0 )
   {
      szZip.SetAdvanced( pZipI.iWrite, pZipI.iExtract, pZipI.iRead );
   }
   try
   {

      switch( hb_CheckSpanMode( szFile ))
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( ... )   { }

   if ( iReturn )
   {
      iNumberOfFiles = szZip.GetCount();

      ZipArray = hb_itemNew(NULL);
      hb_arrayNew( ZipArray, iNumberOfFiles );
      time_t theTime;
      tm *SzTime;

      for( ulCount = 0 ; ulCount < ( ULONG )iNumberOfFiles ; ulCount++ )
      {
         CZipFileHeader fh;
         PHB_ITEM Item;
        
         szZip.GetFileInfo( fh, ( WORD )ulCount );

         if ( iMode )
         {
            const char * szFileNameInZip;
            CZipString szTempString;
            PHB_ITEM TempArray;
            char szAttr[ 5 ];
            char szTime[ 9 ];
            char *szMethod;
            char szCRC[ 8 ];
            int iRatio;
            int iMeth = fh.m_uMethod;
            DWORD uAttr = fh.GetSystemAttr( );

            TempArray =hb_itemNew(NULL);           
            hb_arrayNew( TempArray, 9 );

            szTempString  = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;
            Item  = hb_itemNew( NULL );
            hb_arraySetForward( TempArray, filePos, hb_itemPutC( Item, ( char * )szFileNameInZip ) );
            hb_itemRelease( Item );

            #if defined( __WIN32__ )
               szAttr[ 0 ] = uAttr & FILE_ATTRIBUTE_READONLY ? ( char ) _T( 'r' ) : ( char ) _T( '-' );
               szAttr[ 1 ] = uAttr & FILE_ATTRIBUTE_HIDDEN ? ( char ) _T( 'h' ) : ( char ) _T( '-' );
               szAttr[ 2 ] = uAttr & FILE_ATTRIBUTE_SYSTEM ? ( char ) _T( 's' ) : ( char ) _T( 'w' );
               szAttr[ 3 ] = ( uAttr & FILE_ATTRIBUTE_DIRECTORY ) ? ( char ) _T( 'D' ) : uAttr & FILE_ATTRIBUTE_ARCHIVE ? ( char ) _T( 'a' ): ( char ) _T( '-' );
            #endif

            szAttr[ 4 ] = fh.IsEncrypted( ) ? ( char ) _T( '*' ) : ( char ) _T( ' ' );

            if ( fh.m_uUncomprSize>0 )
            {
               Item  = hb_itemNew( NULL );
               hb_arraySetForward( TempArray, Lenght, hb_itemPutNL( Item, fh.m_uUncomprSize ) );               
               hb_itemRelease( Item );
               Item  = hb_itemNew( NULL );
               hb_arraySetForward( TempArray, Size, hb_itemPutNL( Item, fh.m_uComprSize ));
               hb_itemRelease( Item );
               iRatio = 100-( ( fh.m_uComprSize*100 ) / fh.m_uUncomprSize );

               if ( iRatio <0 )
               {
                  iRatio = 0;
               }
               Item  = hb_itemNew( NULL );
               hb_arraySetForward( TempArray, Ratio, hb_itemPutNL( Item, iRatio ));
               hb_itemRelease( Item );               
            }
            else
            {
               Item  = hb_itemNew( NULL );
               hb_arraySetForward( TempArray, Lenght, hb_itemPutNL( Item, fh.m_uUncomprSize ));
               hb_itemRelease( Item );
               Item  = hb_itemNew( NULL );
               hb_arraySetForward( TempArray, Size, hb_itemPutNL( Item, fh.m_uComprSize ) );
               iRatio = 0;
               hb_itemRelease( Item );
               Item  = hb_itemNew( NULL );              
               hb_arraySetForward( TempArray, Ratio, hb_itemPutNL( Item, iRatio ));
               hb_itemRelease( Item );
            }

            #if defined( __WIN32__ )
               if ( iMeth == 0  || uAttr & FILE_ATTRIBUTE_DIRECTORY )
               {
                  szMethod = "Stored";
               }
            #endif

            if ( iMeth == Z_DEFLATED )
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
                     szMethod = "Unknow";
               }
            }

            
            Item  = hb_itemNew( NULL );
            hb_arraySetForward( TempArray, Method, hb_itemPutC( Item, szMethod ));
            hb_itemRelease( Item );

            sprintf( szCRC, "%8.8lx\n", ( ULONG )fh.m_uCrc32 );
            Item  = hb_itemNew( NULL );
            hb_arraySetForward( TempArray, Crc32, hb_itemPutCL( Item, szCRC, 8 ) );
            hb_itemRelease( Item );
            Item  = hb_itemNew( NULL );
            hb_arraySetForward( TempArray, Date, hb_itemPutD( Item, ( LONG ) ( fh.m_uModDate >> 9 ) +1980,( LONG )( ( fh.m_uModDate & ~0xFE00 ) >> 5 ), ( LONG )fh.m_uModDate & ~0xFFE0 ) );
            hb_itemRelease( Item );

            theTime = fh.GetTime( );
            SzTime =  localtime( &theTime );
            sprintf( szTime, "%02d:%02d:%02d", SzTime->tm_hour, SzTime->tm_min, SzTime->tm_sec );

            Item  = hb_itemNew( NULL );
            hb_arraySetForward( TempArray, Time, hb_itemPutCL( Item, szTime, 8 ) );
            hb_itemRelease( Item );
            Item  = hb_itemNew( NULL );
            hb_arraySetForward( TempArray, Attr, hb_itemPutCL( Item, szAttr, 5 ));
            hb_itemRelease( Item );
            hb_arraySetForward( ZipArray, ulCount+1, TempArray );
            hb_itemRelease( TempArray );

         }
         else
         {
            const char *  szFileNameInZip;
            CZipString szTempString = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;
            Item  = hb_itemNew( NULL );
            hb_arraySetForward( ZipArray, ulCount+1, hb_itemPutC( Item, ( char * ) szFileNameInZip ));
            hb_itemRelease( Item );
         }

         
      }
   }

   szZip.Close( );

   return ZipArray;
}

char *hb___CheckFile( char * szFile )
{
   PHB_FNAME pFileName = hb_fsFNameSplit( szFile );
   char *szZipName = (char*) hb_xgrab( _POSIX_PATH_MAX + 1 );

   if ( ! pFileName->szExtension )
   {
      pFileName->szExtension = ".zip";
   }

   hb_fsFNameMerge( szZipName, pFileName );
   hb_xfree( pFileName );
   return( szZipName );
}

BOOL hb_IsPassWord( char *szFile )
{
   BOOL bReturn = TRUE;
   CZipFileHeader fh;

   CZipArchive szZip;
   SpanCallback span;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            bReturn = FALSE;
      }
   }
   catch ( CZipException )   {}

   if ( bReturn )
   {
      szZip.GetFileInfo( fh, ( WORD )0 );

      bReturn = fh.IsEncrypted();

      szZip.Close( );
   }

   return bReturn;
}

int hb___GetNumberofFilestoUnzip( char *szFile )
{
   int iNumberOfFiles;

   CZipArchive szZip;
   SpanCallback span;

   szZip.SetSpanCallback( &span );
   try {
   szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
   iNumberOfFiles = szZip.GetCount( );
   szZip.Close( );
   }
   catch ( CZipException &e )
   {
      if ( e.m_iCause == CZipException::badZipFile  || e.m_iCause == CZipException::cdirNotFound )
      {
      iNumberOfFiles = -1;
      }
   }

   return iNumberOfFiles;
}

int hb___SetCallbackFunc( PHB_ITEM pFunc )
{

   //ChangeDiskBlock.type = HB_IT_NIL;
   if ( ChangeDiskBlock )
   {
      hb_itemRelease( ChangeDiskBlock );
      ChangeDiskBlock = NULL;
   }
   ChangeDiskBlock = hb_itemNew(NULL);

   if( pFunc )
   {
     hb_itemCopy( ChangeDiskBlock, pFunc );
   }

//   pZipI.pItem = ChangeDiskBlock;

   return ( int ) true;
}

bool hb_SetCallBack( DWORD iNumber, int, void* pData )
{
   PHB_ITEM Disk=hb_itemPutNL( NULL, iNumber ) ;
  
   HB_SYMBOL_UNUSED( pData );

   hb_vmEvalBlockV( ChangeDiskBlock, 1, Disk);

   hb_itemRelease( Disk );

   return TRUE;
}

int hb_DeleteSel( char *szFile, PHB_ITEM pArray, BOOL bCase )
{
   bool iReturn = true;
   ULONG ulCount;
   CZipArchive szZip;
   CZipStringArray  aFiles;

   try
   {
      switch(hb_CheckSpanMode( szFile ))
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            iReturn = false;
      }
   }

   catch ( CZipException )    {}

   if ( iReturn )
   {
      for ( ulCount = 1 ; ( ulCount <=  ( ULONG ) hb_arrayLen( pArray ) ) ; ulCount ++ )
      {
         const char *szDummy = ( char * )hb_arrayGetCPtr( pArray, ulCount );
         aFiles.Add( szDummy );
      }

      szZip.DeleteFiles( aFiles );
   }

   szZip.Close( );

   return ( int ) iReturn;
}

int hb_UnzipSel( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, char *szPassWord, const char *pbyBuffer, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   bool bWithPath = lWithPath?true:false;
   bool iReturn = true;
   ULONG ulCount;
   int iCause;
   char  * szPath = (char*) hb_xgrab( _POSIX_PATH_MAX + 1 );
   BOOL bFreePath = TRUE;

   BOOL bChange = FALSE;
   LPCTSTR lpFiletoExtract;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException )    {}

   if ( iReturn )
   {
      if ( szPassWord !=  NULL )
      {
         szZip.SetPassword( szPassWord );
      }
      if ( pbyBuffer )
      {
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0, (BYTE*) szPath, _POSIX_PATH_MAX + 1 );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }

         hb_fsChDir((BYTE*)"\\");
         szZip.SetRootPath(szPath);
      }
      for ( iCause = 1 ; ( iCause <=  ( int ) hb_arrayLen( pSelArray ) ) ; iCause ++ )
      {
         lpFiletoExtract = hb_arrayGetC( pSelArray, iCause );
         ulCount = szZip.FindFile( ( LPCTSTR )lpFiletoExtract, false );

         if ( ulCount == (ULONG ) -1 )
         {
            ulCount = szZip.FindFile( ( LPCTSTR )lpFiletoExtract, true );
         }

         if ( ulCount >= 0 )
         {
            CZipFileHeader fh;
            const char *  szFileNameInZip;
            CZipString szTempString;
            PHB_FNAME pOut;
            szZip.GetFileInfo( fh, ( WORD )ulCount );
            szTempString  = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;
            pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );
            if ( szPath == NULL )
            {
               szPath = ( char* )pOut->szDrive;
               pOut->szDrive = "";
               hb_fsFNameMerge( ( char* )szFileNameInZip, pOut );
               bChange = TRUE;
               bFreePath = FALSE;
            }
            szZip.SetRootPath(szPath);
            hb_xfree( pOut );

            if( pBlock != NULL )
            {
               PHB_ITEM FileName=hb_itemPutC( NULL, ( char * )szFileNameInZip  ), Pos=hb_itemPutNI( NULL , iCause );

               hb_vmEvalBlockV( pBlock, 2, FileName,Pos );

               hb_itemRelease( FileName );
               hb_itemRelease( Pos );
            }

            try
            {
               /* TODO:  They're both the same.... */
               if ( !HB_IS_BLOCK( pProgress ) )
               {

//                szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
               else
               {
//                szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
            }

            catch( CZipException& e )
            {
                  HB_SYMBOL_UNUSED( e );

                  szZip.CloseFile( NULL, true);
/*            szZip.CloseNewFile(true);*/
            }

            if ( bChange )
            {
               bChange = FALSE;
               szPath = "";
            }
         }

         if ( lpFiletoExtract)
            hb_xfree( (void*) lpFiletoExtract );
      }
   }

   szZip.Close();

   if (szPath)
   {
      hb_fsChDir((BYTE*)szPath);
      if ( bFreePath )
      {
         hb_xfree(szPath);
      }
   }

   return ( int ) iReturn;
}

int hb_TestForPKS( char *szFile )
{
   return hb_CheckSpanMode( szFile );
}

void hb_SetZipBuff( int a, int b, int c )
{
   pZipI.iWrite   = a > 65535 ? a : 65535;
   pZipI.iExtract = b > 16384 ? b : 16384;
   pZipI.iRead    = c > 32768 ? c : 32768;
}

void hb_SetZipComment( char *szComment )
{
   int iLen = strlen( ( const char * ) szComment ) + 1;
   pZipI.szComment = ( char* ) hb_xgrab( iLen );
   strcpy( pZipI.szComment, szComment );
}

void hb_SetZipReadOnly(int iRead )
{
   pZipI.iReadOnly = iRead ;
}

const char * hb_GetZipComment( char *szFile )
{
   const char *szReturn;
   char *szTempR;
   bool iReturn = true;
   CZipString szTemp;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException )  {}

   if ( iReturn )
   {
      szTemp = szZip.GetGlobalComment( );
      szReturn = ( const char * ) szTemp;
   }

   szTempR = ( char* ) hb_xgrab( strlen( ( const char* ) szReturn ) + 1 );
   strcpy( szTempR, ( char* ) szReturn );

   szZip.Close( );

   return szTempR;

}

int hb_UnzipSelIndex( char *szFile, PHB_ITEM pBlock, BOOL lWithPath, char *szPassWord, char *szPath, PHB_ITEM pSelArray, PHB_ITEM pProgress )
{
   bool bWithPath = lWithPath?true:false;
   bool iReturn = true;
   ULONG ulCount;
   int iCause;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

   try
   {
      switch( hb_CheckSpanMode( szFile ) )
      {
         case 0:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
            szZip.SetSpanCallback( &span );
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -2:
            szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 1 );
            break;

         default:
            iReturn = false;
      }
   }

   catch ( CZipException )  {}

   if ( iReturn )
   {
      if ( szPassWord !=  NULL )
      {
         szZip.SetPassword( szPassWord );
      }

      for ( iCause = 0;( iCause <= ( int ) hb_arrayLen( pSelArray ) ) ; iCause++ )
      {
         ulCount = hb_arrayGetNI( pSelArray, iCause ) - 1;

         if ( ulCount >= 0 )
         {
            CZipFileHeader fh;
            const char *  szFileNameInZip;
            CZipString szTempString;
            szZip.GetFileInfo( fh, ( WORD )ulCount );
            szTempString = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;

            if( pBlock  !=  NULL )
            {
               PHB_ITEM FileName=hb_itemPutC(NULL, ( char * )szFileNameInZip ) ;

               hb_vmEvalBlockV( pBlock, 1,  FileName);
               hb_itemRelease( FileName );
            }

            try
            {
               /* TODO:  They're both the same.... */
               if ( !HB_IS_BLOCK( pProgress ) )
               {
//                szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
               else
               {
//                szZip.SetPassword( szPassWord );
                  szZip.ExtractFile( ( WORD )ulCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
               }
            }

            catch( CZipException&  e )
            {
                  HB_SYMBOL_UNUSED( e );

                  szZip.CloseFile( NULL, true);
            }
         }
      }
   }

   szZip.Close( );

   return (int) iReturn;
}

BOOL hb_TransferFilesFromzip( char *szSource, char *szDest, PHB_ITEM pArray )
{
   CZipArchive szZSource;
   CZipArchive szZDest;
   CZipStringArray aFiles;
   const char *szDummy;
   ULONG ulCount;
   BOOL bReturn  = TRUE;
   BOOL bReturn1 = TRUE;

   try
   {
      switch(hb_CheckSpanMode( szSource ))
      {
         case 0:
            szZSource.Open( szSource, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            bReturn = FALSE;
      }
   }

   catch ( CZipException ) { }

   try
   {
      switch(hb_CheckSpanMode( szDest ))
      {
         case 0:
            szZDest.Open( szDest, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
            break;

         case -1:
         case -2:
//       default:
            bReturn1 = FALSE;
      }
   }

   catch ( CZipException ) { }

   if ( bReturn && bReturn1 )
   {
      for ( ulCount = 1 ; ( ulCount <=  ( ULONG ) hb_arrayLen( pArray ) ) ; ulCount++ )
      {
         szDummy = ( char * )hb_arrayGetCPtr( pArray, ulCount );
         aFiles.Add( szDummy );
      }

      if ( szZDest.GetFromArchive( szZSource, aFiles, false ) )
      {
         bReturn  = true;
      }

      szZDest.Close( );
      szZSource.Close( );

      return TRUE;

   }

   return FALSE;
}

DWORD GetCurrentFileSize( LPCTSTR szFile )
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


int hb_UnzipAll(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *pbyBuffer,PHB_ITEM pDiskBlock,PHB_ITEM pProgress)
{
bool iReturn=true;
uLong uiCount=0;
int iCause=0;
int iMode=true;
CZipArchive szZip;
BOOL bChange=FALSE;
   SpanCallback span;
   SpanActionCallback spanac;

   char  * szPath = (char*) hb_xgrab( _POSIX_PATH_MAX + 1 );
   BOOL bFreePath = TRUE;


   if ( HB_IS_BLOCK( pProgress ) )
   {
      pProgressInfo = pProgress;
      szZip.SetCallback( &spanac );
   }

    if (szPassWord != NULL){
        szZip.SetPassword(szPassWord);
      }
     iMode=hb_CheckSpanMode(szFile);

     try {
        if(iMode==0) {
            szZip.Open(szFile,CZipArchive::zipOpen,0);
                     }
        else {
            if (iMode ==-1) {
                szZip.SetSpanCallback( &span );
                szZip.Open(szFile,CZipArchive::zipOpen,0);
                            }
             else {
                if (iMode==-2) {
                    szZip.Open(szFile,CZipArchive::zipOpen,1);
                              }
                else {
                    iReturn =false;
                     }
                  }
             }
    }
    catch (CZipException &e)    {
      iCause=e.m_iCause       ;
	}

 if (iReturn) {

      if ( pbyBuffer )
      {
         if (hb_stricmp(pbyBuffer,".\\")==0 )
         {
            hb_fsCurDirBuffEx( 0, (BYTE*) szPath, _POSIX_PATH_MAX + 1 );
         }
         else
         {
            strcpy(szPath,pbyBuffer);
         }

         hb_fsChDir((BYTE*)"\\");
         szZip.SetRootPath(szPath);
      }

    for (uiCount=0;uiCount<(uLong)szZip.GetCount();uiCount++){
		CZipFileHeader fh;
            const char *  szFileNameInZip;
            CZipString szTempString;
            PHB_FNAME pOut;
            szZip.GetFileInfo( fh, ( WORD )uiCount );
            szTempString  = ( LPCTSTR )fh.GetFileName( );
            szFileNameInZip = ( const char * )szTempString;
            pOut = hb_fsFNameSplit( ( char * ) szFileNameInZip );
            if ( szPath == NULL )
            {
               szPath = ( char* )pOut->szDrive;
               pOut->szDrive = "";
               hb_fsFNameMerge( ( char* )szFileNameInZip, pOut );
               bChange = TRUE;
               bFreePath = FALSE;
            }
            szZip.SetRootPath(szPath);
            hb_xfree( pOut );


            if( pBlock  !=  NULL )
            {
               PHB_ITEM FileName=hb_itemPutC( NULL, ( char * )szFileNameInZip );
               PHB_ITEM Pos=hb_itemPutNI(NULL,uiCount) ;

               hb_vmEvalBlockV( pBlock, 2, FileName, Pos);

               hb_itemRelease( FileName );
               hb_itemRelease( Pos );
            }


        try {
                     if (!HB_IS_BLOCK(pProgress))
                     {

            szZip.SetPassword(szPassWord);
            szZip.ExtractFile( ( WORD )uiCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
            }
            else
            {
            szZip.SetPassword(szPassWord);

            szZip.ExtractFile( ( WORD )uiCount, ( LPCTSTR )szPath, bWithPath, NULL, 65536 );
            }
            
        }
        catch ( CZipException&  e )
        {
           szZip.CloseFile( NULL, true);
           iCause=e.m_iCause       ;
        }
        if(bChange)
        {
           bChange=FALSE;
//        szPath=NULL;
        }

    }

    }
   if (szPath)
   {
      hb_fsChDir((BYTE*)szPath);
      if ( bFreePath )
      {
         hb_xfree(szPath);
      }
   }

return iReturn;
}


#ifdef __cplusplus
}
#endif

int hb_CheckSpanMode( char * szFile )
{
   int iReturn = 0;

   CZipArchive szZip;
   SpanCallback span;
   SpanActionCallback spanac;

   szZip.SetSpanCallback( &span );

   try
   {
// s.r. to avoid GPF when ZIP file is read only !
//      szZip.Open( szFile, CZipArchive::zipOpen, 0 );
        szZip.Open( szFile, pZipI.iReadOnly ? CZipArchive::zipOpenReadOnly : CZipArchive::zipOpen, 0 );
   }

   catch( CZipException &e )
   {
      if ( e.m_iCause == CZipException::cdirNotFound )
      {
         szZip.Close( true );
         iReturn = 114;
      } else if ( e.m_iCause == CZipException::noCallback )
      {
         szZip.Close( true );
         iReturn = 103;
      }
      
      else if ( e.m_iCause == CZipException::generic )
      {
         szZip.Close( true );
         iReturn = 100;
      }      
      else if ( e.m_iCause == CZipException::badZipFile )
      {
         szZip.Close( true );
         iReturn = 101;
      }

      
      else if ( e.m_iCause == CZipException::badCrc )
      {
         szZip.Close( true );
         iReturn = 102;
      }


      
      else if ( e.m_iCause == CZipException::aborted )
      {
         szZip.Close( true );
         iReturn = 104;
      }

      
      else if ( e.m_iCause == CZipException::abortedAction )
      {
         szZip.Close( true );
         iReturn = 105;
      }

      
      else if ( e.m_iCause == CZipException::abortedSafely )
      {
         szZip.Close( true );
         iReturn = 106;
      }

      
      else if ( e.m_iCause == CZipException::nonRemovable )
      {
         szZip.Close( true );
         iReturn = 107;
      }

      
      else if ( e.m_iCause == CZipException::tooManyVolumes )
      {
         szZip.Close( true );
         iReturn = 108;
      }

      
      else if ( e.m_iCause == CZipException::tooLongFileName )
      {
         szZip.Close( true );
         iReturn = 109;
      }

      
      else if ( e.m_iCause == CZipException::badPassword )
      {
         szZip.Close( true );
         iReturn = 110;
      }

      
      else if ( e.m_iCause == CZipException::dirWithSize )
      {
         szZip.Close( true );
         iReturn = 111;
      }

      
      else if ( e.m_iCause == CZipException::internal )
      {
         szZip.Close( true );
         iReturn = 112;
      }

      
      else if ( e.m_iCause == CZipException::notRemoved )
      {
         szZip.Close( true );
         iReturn = 113;
      }

      
      else if ( e.m_iCause == CZipException::notRenamed )
      {
         szZip.Close( true );
         iReturn = 114;
      }

      
      else if ( e.m_iCause == CZipException::platfNotSupp)
      {
         szZip.Close( true );
         iReturn = 115;
      }




   }
   if ( ! iReturn )
   {
      iReturn = szZip.GetSpanMode( );
      szZip.Close( );
   }

   return iReturn;
}
