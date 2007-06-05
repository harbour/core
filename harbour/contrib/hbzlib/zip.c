/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour zip file compress function,
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

#include <hbzip2.h>
#include "hbapifs.h"
#include "hbapierr.h"
#if defined(HB_OS_LINUX)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <dirent.h>
#endif
extern PHB_ITEM ZipArray;
static PHB_ITEM FileToZip;
static PHB_ITEM ExcludeFile;
static PHB_ITEM UnzipFiles;
static PHB_ITEM DelZip;
static PHB_ITEM FileAttribs;
PHB_ITEM ChangeDiskBlock;
#define FA_RDONLY           1   /* R */
#define FA_HIDDEN           2   /* H */
#define FA_SYSTEM           4   /* S */
#define FA_LABEL            8   /* V */
#define FA_DIREC           16   /* D */
#define FA_ARCH            32   /* A */
#define FA_NORMAL         128

extern void hb_fsDirectory( PHB_ITEM pDir, char* szSkleton, char* szAttributes, BOOL bDirOnly, BOOL bFullPath );

#if defined(HB_OS_LINUX)
extern int GetFileAttributes( char *szEntry );
extern void SetFileAttributes( char * szEntry, ULONG ulAttr );
#endif


static void ResetAttribs( void )
{
   ULONG ulAtt, ulZipLen = hb_arrayLen(FileToZip);
   char *szFile;
   int iAttr;

   for( ulAtt = 0; ulAtt < ulZipLen; ulAtt ++ )
   {
     szFile = hb_arrayGetC( FileToZip, ulAtt + 1 );
     iAttr  = hb_arrayGetNI( FileAttribs, ulAtt + 1 );
     SetFileAttributes( szFile, iAttr  );
     hb_xfree( szFile );
   }

   hb_itemRelease( FileAttribs );
   hb_itemRelease( FileToZip );
}

static void UnzipCreateArray( char *szZipFileName, char *szSkleton, int uiOption)
{
   int ul;
   char * szEntry;
   PHB_ITEM pZipEntry;
   PHB_ITEM Temp;
   BOOL bOkAdd;
   int ulLen = hb_arrayLen(ZipArray);
   char sRegEx[ _POSIX_PATH_MAX + _POSIX_PATH_MAX ];

   for ( ul = 0 ; ul < ulLen; ul ++ )
   {
      bOkAdd = TRUE;
      pZipEntry = hb_arrayGetItemPtr( ZipArray, ul + 1 );
      szEntry = hb_arrayGetC( pZipEntry, 1 );

      if ( szSkleton )
      {
         bOkAdd = hb_strMatchFile( (const char *) szEntry, (const char *) sRegEx );
      }

      if ( !bOkAdd )      
      {
         PHB_FNAME pFileName = hb_fsFNameSplit( szEntry );

         if( pFileName->szName )
         {
            char *szFile = (char*) hb_xgrab( _POSIX_PATH_MAX + 1 );
            pFileName->szPath = "";
            hb_fsFNameMerge( szFile, pFileName );
            bOkAdd =  ( hb_stricmp(szSkleton,szFile) ==0  ? 1 : 0 );
            hb_xfree(szFile);
            if ( !bOkAdd )
            {
               bOkAdd =  ( hb_stricmp(szSkleton,szEntry ) ==0  ? 1 : 0 );
            }

         }
         hb_xfree( pFileName );         
      }


      if ( bOkAdd )
      {
         if ( uiOption == 1 )
         {
            Temp = hb_itemNew( NULL ) ;
            hb_arrayAddForward( UnzipFiles, hb_itemPutC( Temp, szEntry ) );
            hb_itemRelease( Temp );
         }
         else
         {
            Temp = hb_itemNew( NULL ) ;
            hb_arrayAddForward( DelZip, hb_itemPutC( Temp, szEntry ) );
            hb_itemRelease( Temp );
         }
      }

      

      hb_xfree( szEntry );
   }

   
}

static BOOL ZipTestExclude ( char *szEntry )
{
   int uiEx;
   BOOL bNotFound = TRUE;
   int uiExLen = hb_arrayLen(ExcludeFile);

   for ( uiEx = 0; uiEx < uiExLen; uiEx ++ )
   {
      char *szExclude = hb_arrayGetC( ExcludeFile, uiEx + 1 );
      if ( strcmp ( szExclude, hb_strupr( szEntry ) ) == NULL )
      {
         hb_xfree( szExclude );
         bNotFound = FALSE;
         break;
      }
      hb_xfree( szExclude );
   }

   return ( bNotFound );
}

static void ZipCreateExclude( PHB_ITEM pExclude )
{
   PHB_ITEM ExTmp;

   ExcludeFile= hb_itemNew(NULL);

   hb_arrayNew( ExcludeFile, 0 );

   if( pExclude == NULL )
   {
      return;
   }

   if ( HB_IS_STRING( pExclude ) )
   {
      if ( hb_itemGetCLen( pExclude ) == 0 )
      {
         return;
      }

      if ( strchr( hb_itemGetCPtr( pExclude ) , '*') != NULL || strchr( hb_itemGetCPtr( pExclude ), '?' ) != NULL )
      {
         PHB_ITEM WildFile;
         PHB_ITEM pDirEntry;
         int uiLen;
         int ui;

         WildFile= hb_itemNew(NULL);
         
         hb_fsDirectory( WildFile, hb_itemGetCPtr( pExclude ), NULL, 0, TRUE);
         uiLen = hb_arrayLen(WildFile);

         for ( ui = 0 ; ui < uiLen; ui ++ )
         {
            char * szEntry;
            pDirEntry = hb_arrayGetItemPtr( WildFile, ui + 1 );
            szEntry = hb_arrayGetC( pDirEntry, 1 );

            if( szEntry )
            {
               ExTmp = hb_itemNew( NULL );
               hb_arrayAddForward( ExcludeFile, hb_itemPutC( ExTmp, hb_strupr( szEntry ) ) );
               hb_xfree( szEntry );
               hb_itemRelease( ExTmp );
            }
         }

         hb_itemRelease( WildFile );
      }
      else
      {
         ExTmp = hb_itemNew( NULL );
         hb_arrayAddForward( ExcludeFile, hb_itemPutC( ExTmp, hb_itemGetCPtr( pExclude ) ) );
         hb_itemRelease( ExTmp ) ;
      }
   }
   else if ( HB_IS_ARRAY( pExclude ) )
   {
      int ux;
      int ufx = hb_arrayLen(pExclude);
      char * szExclude;
      PHB_ITEM WildFile;
      PHB_ITEM pDirEntry;

      WildFile= hb_itemNew( NULL );

      if ( ufx == 0 )
      {
         return;
      }

      for ( ux = 0 ; ux < ufx ; ux ++  )
      {
         szExclude = hb_arrayGetC( pExclude, ux + 1 );

         if( szExclude )
         {
            if ( strchr( szExclude, '*' ) != NULL || strchr( szExclude, '?' ) != NULL )
            {
               int uiW, uiWLen;
               char *szEntry;

               hb_fsDirectory(WildFile,szExclude,NULL,0,TRUE);
               uiWLen = hb_arrayLen(WildFile);

               for ( uiW = 0; uiW < uiWLen; uiW ++ )
               {
                  pDirEntry = hb_arrayGetItemPtr( WildFile, uiW + 1 );
                  szEntry = hb_arrayGetC( pDirEntry, 1 );
                  ExTmp = hb_itemNew( NULL);
                  hb_arrayAddForward( ExcludeFile, hb_itemPutC( ExTmp, szEntry ));
                  hb_itemRelease( ExTmp );
                  hb_xfree( szEntry );
               }
            }
            else
            {
               ExTmp = hb_itemNew( NULL);
               hb_arrayAddForward( ExcludeFile, hb_itemPutC( ExTmp, szExclude ));
               hb_itemRelease( ExTmp );
            }

            hb_xfree( szExclude );
         }
      }

      hb_itemRelease( WildFile );
   }
}

static void ZipCreateArray( PHB_ITEM pParam, BYTE *pCurDir )
{
   PHB_ITEM pDirEntry;
   PHB_ITEM Temp, TempArray, WildFile;
   int ul, ulLen, ulArr, ulLenArr;

   WildFile= hb_itemNew(NULL);
   //FileAttribs.type = HB_IT_NIL;
   //FileToZip.type = HB_IT_NIL;
   FileToZip = hb_itemNew(NULL);  
   TempArray= hb_itemNew(NULL);
   FileAttribs = hb_itemNew(NULL);
   hb_arrayNew( FileAttribs, 0 );
   hb_arrayNew( TempArray, 0 );

   if( pParam->type == HB_IT_STRING )
   {
      Temp= hb_itemNew(NULL);      
      hb_arrayAddForward( TempArray, hb_itemPutC( Temp, hb_itemGetCPtr( pParam ) ) );
      hb_itemRelease( Temp );
   }
   else
   {
      PHB_ITEM pClone = hb_arrayClone( pParam );
      hb_itemCopy( TempArray, pClone );
      hb_itemRelease( pClone );
   }

   ulLenArr = hb_arrayLen(TempArray);  

   hb_arrayNew( FileToZip, 0 );

   for ( ulArr = 0; ulArr < ulLenArr ; ulArr ++ )
   {
      char *szArrEntry = hb_arrayGetC( TempArray, ulArr + 1 );

      if ( szArrEntry )
      {
         if ( strchr( szArrEntry, '*' ) != NULL || strchr( szArrEntry, '?' ) != NULL )
         {
// if don't gave path add current dir !
            if ( ( pCurDir ) && ( ! strchr( szArrEntry, OS_PATH_DELIMITER ) ) )
            {
               char * szTemp = szArrEntry ;
               szArrEntry = ( char * ) hb_xrealloc( szArrEntry, _POSIX_PATH_MAX );
               hb_xstrcpy( szArrEntry, (char *) pCurDir,
                              OS_PATH_DELIMITER_STRING, szTemp, NULL );
            }

            hb_fsDirectory(WildFile,szArrEntry,NULL,0,TRUE);
            ulLen = hb_arrayLen(WildFile); 

            for ( ul = 0; ul < ulLen ; ul ++ )
            {
               char * szEntry;
               pDirEntry = hb_arrayGetItemPtr( WildFile, ul + 1 );
               szEntry = hb_arrayGetC( pDirEntry, 1 );

               if ( ZipTestExclude ( szEntry ) )
               {
                  Temp= hb_itemNew(NULL);
                  hb_arrayAddForward( FileToZip, hb_itemPutC( Temp, szEntry ) );
                  hb_itemRelease( Temp ) ;
                  Temp= hb_itemNew(NULL);
                  hb_arrayAddForward( FileAttribs, hb_itemPutNI( Temp, GetFileAttributes( szEntry ) ) );
                  hb_itemRelease( Temp ) ;
                  #if defined(HB_OS_LINUX)
                  SetFileAttributes( szEntry, 0777 );
                  #else
                  SetFileAttributes( szEntry, FA_ARCH );
                  #endif
               }

               if ( szEntry )
               {
                  hb_xfree( szEntry );
               }
            }

            hb_itemRelease( WildFile );
         }
         else
         {
            Temp = hb_itemNew( NULL);
            hb_arrayAddForward( FileToZip, hb_itemPutC( Temp, szArrEntry ) );
            hb_itemRelease( Temp ) ;
            Temp = hb_itemNew( NULL);
            hb_arrayAddForward( FileAttribs, hb_itemPutNI( Temp, GetFileAttributes( szArrEntry ) ) );
            hb_itemRelease( Temp ) ;
            
                  #if defined(HB_OS_LINUX)
                  SetFileAttributes( szArrEntry, 0777 );
                  #else
                  SetFileAttributes( szArrEntry, FA_ARCH );
                  #endif
         }

         hb_xfree( szArrEntry );
      }
   }

   hb_itemRelease( TempArray );
}

HB_FUNC( HB_ZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if ( pParam )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 9, HB_IT_BLOCK );
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         HB_ITEM iProgress;
         BYTE *pCurDir;
         char *szZipFileName;

         if ( ! ISNIL( 4 ) && ! ISBLOCK( 4 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 2017, "Invalid Codeblock ","hb_zipfile",
                                  4 ,hb_paramError( 1 ),
                                  hb_paramError( 2 ),
                                  hb_paramError( 3 ),
                                  hb_paramError( 4 ) );
            return;
         }

         iProgress.type = HB_IT_NIL;


         pCurDir = ( BYTE * )hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir );

         hb_fsChDir( pCurDir ) ;


         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }
// add current dir to file name if not specified

         if ( ! strchr( hb_parc( 1 ), OS_PATH_DELIMITER ) )
         {
            strcpy( szFile, (char *) pCurDir );
            strcat( szFile, OS_PATH_DELIMITER_STRING) ;
            strcat( szFile, hb_parc( 1 ) ) ;
         }
         else
         {
            strcpy( szFile, hb_parc( 1 ) );
         }
         hb_xfree( pCurDir) ;
         szZipFileName = hb___CheckFile( szFile );

         if ( hb_arrayLen(FileToZip) > 0 )
         {
            bRet = hb_CompressFile( szZipFileName,
                                    FileToZip,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                    hb_param( 4, HB_IT_BLOCK ),
                                    ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                    ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                    ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                    ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                    &iProgress );

            ResetAttribs();
         }

         hb_itemClear( &iProgress );
         hb_xfree( szZipFileName );
         hb_itemRelease(ExcludeFile);
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      char szFile[ _POSIX_PATH_MAX ];
      char *szZipFileName;
      PHB_ITEM pArray;
      strcpy( szFile, hb_parc( 1 ) );

      szZipFileName = hb___CheckFile( szFile );

      pArray = hb___GetFileNamesFromZip( szZipFileName,
                                ISLOG( 2 ) ? hb_parl( 2 ) : 0 );

      hb_itemReturn( pArray );
      hb_itemRelease( pArray ) ;
      hb_xfree( szZipFileName );
   }
}

HB_FUNC( HB_ZIPWITHPASSWORD )
{
   hb_retl( hb_IsPassWord( hb_parc( 1 ) ) );
}

HB_FUNC( HB_GETFILECOUNT )
{
   int iRet = 0;

   if( ISCHAR( 1 ) )
   {
      char szFile[ _POSIX_PATH_MAX ];
      char *szZipFileName;
      strcpy( szFile, hb_parc( 1 ) );

      szZipFileName = hb___CheckFile( szFile );

      iRet = hb___GetNumberofFilestoUnzip( szZipFileName );

      hb_xfree( szZipFileName );
   }

   hb_retni( iRet );
}

HB_FUNC( HB_ZIPFILEBYTDSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if ( pParam )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 10, HB_IT_BLOCK );
         PHB_ITEM pExclude = hb_param( 11, HB_IT_STRING | HB_IT_ARRAY );
         HB_ITEM iProgress;
         char *szZipFileName;
         BYTE *pCurDir;

         iProgress.type = HB_IT_NIL;
         pCurDir = ( BYTE * )hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir );

         hb_fsChDir( pCurDir );
         hb_xfree( pCurDir );

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

// add current dir to file name if not specified
         if ( ! strchr( szFile, OS_PATH_DELIMITER ) )
         {
            strcpy( szFile, (char *) pCurDir );
            strcat( szFile, OS_PATH_DELIMITER_STRING) ;
            strcat( szFile, hb_parc( 1 ) ) ;
         }
         else
         {
            strcpy( szFile, hb_parc( 1 ) );
         }

         szZipFileName = hb___CheckFile( szFile );

         if ( hb_arrayLen(FileToZip) > 0 )
         {
            bRet = hb_CmpTdSpan( szZipFileName,
                                 FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISNUM( 7 ) ? hb_parni( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 ISLOG( 9 ) ? hb_parl( 9 ) : 0,
                                 &iProgress );

            ResetAttribs();
         }

         hb_itemClear( &iProgress );
         hb_xfree( szZipFileName );
         hb_itemRelease(ExcludeFile);
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPFILEBYPKSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if ( pParam )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 9, HB_IT_BLOCK );
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         HB_ITEM iProgress;
         char *szZipFileName;
         BYTE * pCurDir ;

         iProgress.type = HB_IT_NIL;
         pCurDir = ( BYTE * )hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir );

         hb_fsChDir( pCurDir ) ;
         hb_xfree( pCurDir ) ;

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         if ( hb_arrayLen(FileToZip) > 0 )
         {
            bRet = hb_CmpPkSpan( szZipFileName,
                                 FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : 0,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISLOG( 7 ) ? hb_parl( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : 0,
                                 &iProgress );
            ResetAttribs();
         }

         hb_itemClear( &iProgress );
         hb_xfree( szZipFileName );
         hb_itemRelease(ExcludeFile);
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_UNZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) && ( ISARRAY( 6 ) || ISCHAR( 6 ) ) )
   {
      char szFile[ _POSIX_PATH_MAX ];
      PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
      PHB_ITEM pUnzip = hb_param( 6, HB_IT_ANY );
      HB_ITEM iProgress;
//      PHB_ITEM Temp;
      char *szZipFileName;
      BYTE *pCurDir;

      //Temp.type = HB_IT_NIL;
      iProgress.type = HB_IT_NIL;


      pCurDir = ( BYTE * )hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

      if( pProgress )
      {
        hb_itemCopy( &iProgress, pProgress );
      }

      strcpy( szFile, hb_parc( 1 ) );
      szZipFileName = hb___CheckFile( szFile );

      UnzipFiles= hb_itemNew( NULL);
      hb_arrayNew( UnzipFiles, 0 );

      if(  hb_TestForPKS( szZipFileName ) <=0 ) 
      {
      hb___GetFileNamesFromZip( szZipFileName, TRUE );
      

      if( pUnzip )
      {
         if( HB_IS_STRING( pUnzip ) )
         {
            UnzipCreateArray( szZipFileName, hb_itemGetCPtr( pUnzip ), 1 );
         }
         else if( HB_IS_ARRAY( pUnzip ) )
         {
            int uiZ, uiZLen = hb_arrayLen(pUnzip);
            char *szUnzip;
            PHB_ITEM Temp;

            

            for ( uiZ = 0; uiZ < uiZLen; uiZ ++ )
            {
               szUnzip = hb_arrayGetC( pUnzip, uiZ + 1 );

               if ( szUnzip )
               {
                  UnzipCreateArray( szZipFileName, szUnzip, 1 );
                  hb_xfree( szUnzip );
               }
            }
         }
      }
      else
      {
//s.r. change "*.*" to "*" because file without extension were ignored
         UnzipCreateArray( szZipFileName, "*", 1 );
      }
      if ( hb_arrayLen(UnzipFiles) > 0 )
      {
         bRet = hb_UnzipSel( szZipFileName,
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             UnzipFiles,
                             &iProgress );
      }
   
      hb_xfree( szZipFileName );
      hb_itemClear( &iProgress );
      hb_itemRelease( UnzipFiles );
      hb_fsChDir( pCurDir ) ;
      hb_xfree( pCurDir ) ;
      hb_itemClear( ZipArray );
      hb_itemRelease( ZipArray );
    }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_SETDISKZIP )
{
   hb_retl( hb___SetCallbackFunc( hb_param( 1, HB_IT_BLOCK ) ) );
}

HB_FUNC( HB_ZIPDELETEFILES )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pDelZip = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY | HB_IT_NUMERIC );

      DelZip = hb_itemNew(NULL);
      hb_arrayNew( DelZip, 0 );

      if ( pDelZip )
      {
         char szFile[ _POSIX_PATH_MAX ];
         char *szZipFileName;
         int ulLen;

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         hb___GetFileNamesFromZip( szZipFileName, TRUE );
         ulLen = hb_arrayLen(ZipArray);

         if ( !ulLen )
         {
            hb_xfree( szZipFileName );
            hb_itemClear( ZipArray );
            hb_itemRelease( ZipArray );
            hb_retl ( bRet );
            return;
         }

         if ( HB_IS_STRING( pDelZip ) )
         {
            if ( hb_itemGetCLen( pDelZip ) > 0 )
            {
               UnzipCreateArray( szZipFileName, hb_itemGetCPtr( pDelZip ), 2 );
            }
         }
         else if ( HB_IS_ARRAY( pDelZip ) )
         {
            int uiInLen = hb_arrayLen(pDelZip);

            if ( uiInLen > 0 )
            {
               int uiIn;
               char *szInput;

               for ( uiIn = 0; uiIn < uiInLen; uiIn ++ )
               {
                  szInput = hb_arrayGetC( pDelZip, uiIn + 1 );

                  if( szInput )
                  {
                     UnzipCreateArray( szZipFileName, szInput, 2 );
                     hb_xfree( szInput );
                  }
               }

            }
         }
         else if ( HB_IS_NUMERIC( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );
            PHB_ITEM Temp;
            

            if( iIndex > 0 && iIndex <= ulLen )
            {
               PHB_ITEM pZipEntry = hb_arrayGetItemPtr( ZipArray, iIndex );
               char* szEntry = hb_arrayGetC( pZipEntry, 1 );
               Temp = hb_itemNew(NULL);
               hb_arrayAddForward( DelZip, hb_itemPutC( Temp, szEntry ) );
               hb_xfree( szEntry );
               hb_itemRelease( Temp );
            }
         }

         if ( hb_arrayLen(DelZip) > 0 )
         {
            bRet = hb_DeleteSel( szZipFileName,
                                 DelZip,
                                 ISLOG( 3 ) ? hb_parl( 3 ) : 0 );
         }

         hb_xfree(szZipFileName);
         hb_itemClear( ZipArray );
         hb_itemRelease( ZipArray );
      }

      hb_itemRelease( DelZip );
   }

   hb_retl( bRet );
}

HB_FUNC( HB_ZIPTESTPK )
{
   char szFile[ _POSIX_PATH_MAX ];
   char *szZipFileName;

   strcpy( szFile, hb_parc( 1 ) );
   szZipFileName = hb___CheckFile( szFile );

   hb_retni( hb_TestForPKS( szZipFileName ) );

   hb_xfree(szZipFileName);
}

HB_FUNC( HB_SETBUFFER )
{
   hb_SetZipBuff( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ));
}

HB_FUNC( HB_SETZIPCOMMENT )
{
   hb_SetZipComment( hb_parc( 1 ) );
}

HB_FUNC( HB_GETZIPCOMMENT )
{
   char *szComment=( char* )hb_GetZipComment( hb_parc( 1 ) );
   hb_retcAdopt( szComment );
}

HB_FUNC( HB_UNZIPFILEINDEX )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pDelZip = hb_param( 6, HB_IT_NUMERIC | HB_IT_ARRAY );

      if ( pDelZip )
      {
         char szFile[ _POSIX_PATH_MAX ];
         PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
         PHB_ITEM Temp,DelZip;
         HB_ITEM iProgress ;
         char* szZipFileName;
         int ulLen;

         iProgress.type = HB_IT_NIL;
         DelZip = hb_itemNew( NULL);
         hb_arrayNew( DelZip, 0 );

         if( pProgress )
         {
           hb_itemCopy( &iProgress, pProgress );
         }

         strcpy( szFile, hb_parc( 1 ) );
         szZipFileName = hb___CheckFile( szFile );

         hb___GetFileNamesFromZip( szZipFileName, TRUE );
         ulLen = hb_arrayLen(ZipArray);

         if ( HB_IS_NUMERIC ( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );

            if ( iIndex > 0 && iIndex <= ulLen )
            {
               Temp = hb_itemNew(NULL);
               hb_arrayAddForward( DelZip, hb_itemPutNI( Temp, iIndex ) );
               hb_itemRelease( Temp );
            }
         }
         else
         {
            int ui, iIndex;

            for ( ui = 0 ; ui < ulLen; ui ++ )
            {
               iIndex = hb_arrayGetNI( pDelZip, ui + 1 );
               if ( iIndex && iIndex > 0 && iIndex <= ulLen )
               {
                  Temp = hb_itemNew(NULL);
                  hb_arrayAddForward( DelZip, hb_itemPutNI( Temp, iIndex ) );
                  hb_itemRelease( Temp );
               }
            }

         }

         if( hb_arrayLen(DelZip) > 0 )
         {
            bRet = hb_UnzipSelIndex( szZipFileName,
                                     hb_param( 2, HB_IT_BLOCK ),
                                     ISLOG( 3 ) ? hb_parl( 3 ) : 0,
                                     ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                     hb_parc( 5 ),
                                     DelZip,
                                     &iProgress );
         }

         hb_itemRelease( DelZip );
         hb_itemClear( &iProgress );
         hb_xfree( szZipFileName );
         hb_itemClear( ZipArray );
         hb_itemRelease( ZipArray );

      }
   }

   hb_retl( bRet );
}

/*
HB_FUNC( HB_ZIPINMEMORY )
{
    hb_retl( hb_CreateZipInMemory( hb_parc( 1 ), hb_parc( 2 ) ) );
}

HB_FUNC( HB_SAVEZIPFROMMEMORY )
{
    hb_retl( hb_SaveZipFileFromMemory( ) );
}
*/

HB_FUNC( TRANSFERFROMZIP )
{
   hb_retl( hb_TransferFilesFromzip( hb_parc( 1 ),
                                     hb_parc( 2 ),
                                     hb_param( 3, HB_IT_ARRAY ) ) );
}

HB_FUNC(SETZIPREADONLY)
{
   hb_SetZipReadOnly( hb_parl( 1 ) );
}
HB_FUNC(HB_UNZIPALLFILE)

{
    PHB_ITEM pProgress = hb_param( 7, HB_IT_BLOCK );
    HB_ITEM iProgress;
    iProgress.type = HB_IT_NIL;

    if( pProgress )
    {
        hb_itemCopy( &iProgress, pProgress );
    }

    if ( ! ISCHAR(6) && ! ISARRAY(6) )
    { 
        char szFile[_POSIX_PATH_MAX];
        char *szZipFile;
        strcpy(szFile, hb_parc(1));
        szZipFile = hb___CheckFile(szFile);
        hb_retl(hb_UnzipAll(szZipFile, hb_param( 2, HB_IT_BLOCK),ISLOG(3) ? hb_parl(3) : 0 ,hb_parc(4),ISCHAR(5) ? hb_parc(5) : NULL,hb_param( 6, HB_IT_BLOCK),&iProgress));
        hb_xfree( szZipFile );
    }
}

HB_FUNC_EXIT(HBZIPCLEANUP)
{
   if ( ChangeDiskBlock )
   {
      hb_itemRelease( ChangeDiskBlock );
      ChangeDiskBlock  = NULL ;
   }

}


#if defined(HB_OS_LINUX)

int GetFileAttributes( char *szEntry )
{
      struct stat sStat;
      stat( szEntry, &sStat );
      return (int) sStat.st_mode;
}
void SetFileAttributes( char * szEntry,ULONG ulAttr)
{
   chmod(szEntry,ulAttr);
}
#endif
