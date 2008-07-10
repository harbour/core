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

#include "hbziparc.h"

#include "hbapifs.h"
#include "hbapierr.h"
#if defined(HB_OS_LINUX)
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <dirent.h>
#endif

extern void hb_fsDirectory( PHB_ITEM pDir, char* szSkleton, char* szAttributes, BOOL bDirOnly, BOOL bFullPath );

static PHB_ITEM s_FileToZip;
static PHB_ITEM s_ExcludeFile;
static PHB_ITEM s_UnzipFiles;
static PHB_ITEM s_DelZip;
static PHB_ITEM s_FileAttribs;

PHB_ITEM hbza_ChangeDiskBlock = NULL;
PHB_ITEM hbza_pProgressInfo = NULL;
PHB_ITEM hbza_ZipArray;
HB_ZIP_INTERNAL hbza_pZipI;

#define FA_RDONLY           1   /* R */
#define FA_HIDDEN           2   /* H */
#define FA_SYSTEM           4   /* S */
#define FA_LABEL            8   /* V */
#define FA_DIREC           16   /* D */
#define FA_ARCH            32   /* A */
#define FA_NORMAL         128

#if defined(HB_OS_LINUX)

static int GetFileAttributes( char * szEntry )
{
   struct stat sStat;
   stat( szEntry, &sStat );
   return ( int ) sStat.st_mode;
}

static void SetFileAttributes( char * szEntry, ULONG ulAttr)
{
   chmod( szEntry, ulAttr );
}

#endif

static void ResetAttribs( void )
{
   ULONG ulAtt, ulZipLen = hb_arrayLen( s_FileToZip );

   for( ulAtt = 0; ulAtt < ulZipLen; ulAtt++ )
   {
      char * szFile = hb_arrayGetC( s_FileToZip, ulAtt + 1 );
      int iAttr = hb_arrayGetNI( s_FileAttribs, ulAtt + 1 );
      SetFileAttributes( szFile, iAttr );
      hb_xfree( szFile );
   }

   hb_itemRelease( s_FileAttribs );
   hb_itemRelease( s_FileToZip );
}

static void UnzipCreateArray( char *szSkleton, int uiOption)
{
   int ul;
   char * szEntry;
   PHB_ITEM pZipEntry;
   PHB_ITEM Temp;
   BOOL bOkAdd;
   int ulLen = hb_arrayLen(hbza_ZipArray);
   char sRegEx[ _POSIX_PATH_MAX + _POSIX_PATH_MAX + 1 ];

   for( ul = 0; ul < ulLen; ul++ )
   {
      bOkAdd = TRUE;
      pZipEntry = hb_arrayGetItemPtr( hbza_ZipArray, ul + 1 );
      szEntry = hb_arrayGetC( pZipEntry, 1 );

      if( szSkleton )
         bOkAdd = hb_strMatchFile( (const char *) szEntry, (const char *) sRegEx );

      if( !bOkAdd )
      {
         PHB_FNAME pFileName = hb_fsFNameSplit( szEntry );

         if( pFileName->szName )
         {
            char * szFile = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
            pFileName->szPath = ( char * ) "";
            hb_fsFNameMerge( szFile, pFileName );
            bOkAdd = hb_stricmp( szSkleton, szFile ) == 0 ? 1 : 0;
            hb_xfree( szFile );

            if( ! bOkAdd )
               bOkAdd = hb_stricmp( szSkleton, szEntry ) == 0 ? 1 : 0;

         }

         hb_xfree( pFileName );
      }

      if( bOkAdd )
      {
         if( uiOption == 1 )
         {
            Temp = hb_itemNew( NULL ) ;
            hb_arrayAddForward( s_UnzipFiles, hb_itemPutC( Temp, szEntry ) );
            hb_itemRelease( Temp );
         }
         else
         {
            Temp = hb_itemNew( NULL ) ;
            hb_arrayAddForward( s_DelZip, hb_itemPutC( Temp, szEntry ) );
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
   int uiExLen = hb_arrayLen(s_ExcludeFile);

   for( uiEx = 0; uiEx < uiExLen; uiEx++ )
   {
      char * szExclude = hb_arrayGetC( s_ExcludeFile, uiEx + 1 );
      if( strcmp ( szExclude, hb_strupr( szEntry ) ) == 0 )
      {
         hb_xfree( szExclude );
         bNotFound = FALSE;
         break;
      }
      hb_xfree( szExclude );
   }

   return bNotFound;
}

static void ZipCreateExclude( PHB_ITEM pExclude )
{
   PHB_ITEM ExTmp;

   s_ExcludeFile = hb_itemArrayNew(0);

   if( pExclude == NULL )
      return;

   if( HB_IS_STRING( pExclude ) )
   {
      if( hb_itemGetCLen( pExclude ) == 0 )
         return;

      if( strchr( hb_itemGetCPtr( pExclude ), '*' ) != NULL || strchr( hb_itemGetCPtr( pExclude ), '?' ) != NULL )
      {
         PHB_ITEM WildFile;
         PHB_ITEM pDirEntry;
         int uiLen;
         int ui;

         WildFile = hb_itemNew( NULL );

         hb_fsDirectory( WildFile, hb_itemGetCPtr( pExclude ), NULL, 0, TRUE );
         uiLen = hb_arrayLen( WildFile );

         for( ui = 0; ui < uiLen; ui++ )
         {
            char * szEntry;
            pDirEntry = hb_arrayGetItemPtr( WildFile, ui + 1 );
            szEntry = hb_arrayGetC( pDirEntry, 1 );

            if( szEntry )
            {
               ExTmp = hb_itemPutC( NULL, hb_strupr( szEntry ) );
               hb_arrayAddForward( s_ExcludeFile, ExTmp );
               hb_xfree( szEntry );
               hb_itemRelease( ExTmp );
            }
         }

         hb_itemRelease( WildFile );
      }
      else
      {
         ExTmp = hb_itemPutC( NULL, hb_itemGetCPtr( pExclude ) );
         hb_arrayAddForward( s_ExcludeFile, ExTmp );
         hb_itemRelease( ExTmp ) ;
      }
   }
   else if( HB_IS_ARRAY( pExclude ) )
   {
      int ux;
      int ufx = hb_arrayLen( pExclude );
      char * szExclude;
      PHB_ITEM WildFile;
      PHB_ITEM pDirEntry;

      WildFile = hb_itemNew( NULL );

      if( ufx == 0 )
         return;

      for( ux = 0; ux < ufx; ux++ )
      {
         szExclude = hb_arrayGetC( pExclude, ux + 1 );

         if( szExclude )
         {
            if( strchr( szExclude, '*' ) != NULL || strchr( szExclude, '?' ) != NULL )
            {
               int uiW, uiWLen;
               char *szEntry;

               hb_fsDirectory( WildFile, szExclude, NULL, 0, TRUE );
               uiWLen = hb_arrayLen( WildFile );

               for( uiW = 0; uiW < uiWLen; uiW++ )
               {
                  pDirEntry = hb_arrayGetItemPtr( WildFile, uiW + 1 );
                  szEntry = hb_arrayGetC( pDirEntry, 1 );
                  ExTmp = hb_itemNew( NULL);
                  hb_arrayAddForward( s_ExcludeFile, hb_itemPutC( ExTmp, szEntry ));
                  hb_itemRelease( ExTmp );
                  hb_xfree( szEntry );
               }
            }
            else
            {
               ExTmp = hb_itemNew( NULL );
               hb_arrayAddForward( s_ExcludeFile, hb_itemPutC( ExTmp, szExclude ) );
               hb_itemRelease( ExTmp );
            }

            hb_xfree( szExclude );
         }
      }

      hb_itemRelease( WildFile );
   }
}

static void ZipCreateArray( PHB_ITEM pParam, BYTE *pCurDir, BOOL bFullPath )    /* bFullPath by JGS */
{
   PHB_ITEM pDirEntry, Temp, TempArray;
   PHB_ITEM WildFile = hb_itemNew( NULL );
   int ul, ulLen, ulArr, ulLenArr;

   s_FileToZip = hb_itemArrayNew(0);
   s_FileAttribs = hb_itemArrayNew(0);

   if( HB_IS_STRING( pParam ) )
   {
      TempArray = hb_itemArrayNew( 0 );
      Temp = hb_itemPutC( NULL, hb_itemGetCPtr( pParam ) );
      hb_arrayAddForward( TempArray, Temp );
      hb_itemRelease( Temp );
   }
   else
   {
      TempArray = hb_arrayClone( pParam );
   }

   ulLenArr = hb_arrayLen( TempArray );

   for( ulArr = 0; ulArr < ulLenArr; ulArr++ )
   {
      char *szArrEntry = hb_arrayGetC( TempArray, ulArr + 1 );

      if( szArrEntry )
      {
         if( strchr( szArrEntry, '*' ) != NULL || strchr( szArrEntry, '?' ) != NULL )
         {
         #if defined(HB_WIN32_IO)
            /* by JGS if don't gave path or there is a relative path add current dir ! */
            PHB_FNAME fDirSpec = hb_fsFNameSplit( (char*) szArrEntry );

            if( ( pCurDir ) && ( fDirSpec != NULL ) &&
                 ! ( fDirSpec->szDrive ) && ( fDirSpec->szPath ) && ( fDirSpec->szPath[0] != OS_PATH_DELIMITER ) )
         #else
            /* if don't gave path add current dir ! */
            if( ( pCurDir ) && ( ! strchr( szArrEntry, OS_PATH_DELIMITER ) ) )
         #endif
            {
               char * szTemp = szArrEntry ;
               szArrEntry = hb_xstrcpy( NULL, (char *) pCurDir, OS_PATH_DELIMITER_STRING, szTemp, NULL );
               hb_xfree( szTemp );
            }

            hb_fsDirectory(WildFile,szArrEntry,NULL,0,bFullPath ); /* bFullPath by JGS */
            ulLen = hb_arrayLen(WildFile);

            for( ul = 0; ul < ulLen; ul++ )
            {
               char * szEntry;
               pDirEntry = hb_arrayGetItemPtr( WildFile, ul + 1 );
               szEntry = hb_arrayGetC( pDirEntry, 1 );

               /* by JGS */
               #if defined(HB_WIN32_IO)
                  if(! ( bFullPath ) && ( fDirSpec != NULL ) && ( fDirSpec->szPath ) )
                  {
                     char * szFile = szEntry;
                     szEntry = hb_xstrcpy( NULL, fDirSpec->szPath, szFile, NULL );
                     hb_xfree( szFile );
                  }
               #endif
               /* by JGS */

               if( ZipTestExclude ( szEntry ) )
               {
                  Temp= hb_itemNew(NULL);
                  hb_arrayAddForward( s_FileToZip, hb_itemPutC( Temp, szEntry ) );
                  hb_itemRelease( Temp ) ;
                  Temp= hb_itemNew(NULL);
                  hb_arrayAddForward( s_FileAttribs, hb_itemPutNI( Temp, GetFileAttributes( szEntry ) ) );
                  hb_itemRelease( Temp ) ;
                  #if defined(HB_OS_LINUX)
                  SetFileAttributes( szEntry, 0777 );
                  #else
                  SetFileAttributes( szEntry, FA_ARCH );
                  #endif
               }

               if( szEntry )
               {
                  hb_xfree( szEntry );
               }
            }

            /* by JGS */
            #if defined(HB_WIN32_IO)
               if( fDirSpec )
               {
                  hb_xfree( fDirSpec );
               }
            #endif

            hb_itemClear( WildFile );
            /* by JGS */
         }
         else
         {
            Temp = hb_itemPutC( NULL, szArrEntry ) ;
            hb_arrayAddForward( s_FileToZip, Temp );
            hb_itemRelease( Temp ) ;
            Temp = hb_itemPutNI( NULL, GetFileAttributes( szArrEntry ) );
            hb_arrayAddForward( s_FileAttribs, Temp );
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

   hb_itemRelease( WildFile );
   hb_itemRelease( TempArray );
}

/*
 * $DOC$
 * $FUNCNAME$
 *      HB_ZIPFILE()
 * $CATEGORY$
 *      Zip Functions
 * $ONELINER$
 *      Create a zip file
 * $SYNTAX$
 *      HB_ZIPFILE( <cFile>, <cFileToCompress> | <aFiles>, <nLevel>,
 *      <bBlock>, <lOverWrite>, <cPassword>, <lWithPath>, <lWithDrive>,
 *      <pFileProgress> ) ---> lCompress
 * $ARGUMENTS$
 *      <cFile>   Name of the zip file to create
 *
 *      <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *      can be used
 *         _or_
 *      <aFiles>  An array containing files to compress, Drive and/or path
 *      can be used
 *
 *      <nLevel>  Compression level ranging from 0 to 9
 *
 *      <bBlock>  Code block to execute while compressing
 *
 *      <lOverWrite>  Toggle to overwrite the file if exists
 *
 *      <cPassword> Password to encrypt the files
 *
 *      <lWithPath> Toggle to store the path or not
 *
 *      <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *      <pFileProgress> Code block for File Progress
 * $RETURNS$
 *      <lCompress>  .t. if file was create, otherwise .f.
 * $DESCRIPTION$
 *      This function creates a zip file named <cFile>. If the extension
 *      is omitted, .ZIP will be assumed. If the second parameter is a
 *      character string, this file will be added to the zip file. If the
 *      second parameter is an array, all file names contained in <aFiles>
 *      will be compressed.
 *
 *      If <nLevel> is used, it determines the compression type where 0 means
 *      no compression and 9 means best compression.
 *
 *      If <bBlock> is used, every time the file is opened to compress it
 *      will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *      If <lOverWrite> is used, it toggles to overwrite or not the existing
 *      file. Default is to overwrite the file,otherwise if <lOverWrite> is false
 *      the new files are added to the <cFile>.
 *
 *      If <cPassword> is used, all files that are added to the archive are encrypted
 *      with the password.
 *
 *      If <lWithPath> is used, it tells  the path should also be stored with
 *      the file name. Default is false.
 *
 *      If <lWithDrive> is used, it tells thats the Drive and path should also be stored
 *      with the file name. Default is false.
 *
 *      If <pFileProgress> is used, an Code block is evaluated, showing the total
 *      of that file has being processed.
 *      The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 *
 * $EXAMPLES$
 *      FUNCTION MAIN()
 *
 *      IF HB_ZIPFILE( "TEST.ZIP", "TEST.PRG" )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILE( "TEST1.ZIP", { "TEST.PRG", "c:\windows\win.ini" } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILE( "TEST2.ZIP", { "TEST.PRG", "c:\windows\win.ini" }, 9, {|cFile,nPos,| qout(cFile) } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      aFiles := { "TEST.PRG", "c:\windows\win.ini" }
 *      nLen   := Len( aFiles )
 *      aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B" , "²" )
 *      GaugeDisplay( aGauge )
 *      HB_ZIPFILE( "test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen ) },, "hello" )
 *      Return Nil
 * $STATUS$
 *      R
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if( pParam )
      {
         char szFile[ _POSIX_PATH_MAX + 1 ];
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         BYTE *pCurDir;
         char *szZipFileName;

         /* by JGS */
         BOOL bFullPath = TRUE;
         #if defined(HB_WIN32_IO)
            if( ISLOG( 11 ) )
            {
               bFullPath = hb_parl( 11 );
            }
         #endif
         /* by JGS */

         if( ! ISNIL( 4 ) && ! ISBLOCK( 4 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 2017, "Invalid Codeblock ","hb_zipfile",
                                  4 ,hb_paramError( 1 ),
                                  hb_paramError( 2 ),
                                  hb_paramError( 3 ),
                                  hb_paramError( 4 ) );
            return;
         }

         pCurDir = ( BYTE * ) hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

         /* Always needs to create an array */
         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir, bFullPath );  /* bFullPath by JGS */

         hb_fsChDir( pCurDir ) ;

         if( ! strchr( hb_parc( 1 ), OS_PATH_DELIMITER ) )
         {
            hb_strncpy( szFile, (char *) pCurDir, sizeof( szFile ) - 1 );
            hb_strncat( szFile, OS_PATH_DELIMITER_STRING, sizeof( szFile ) - 1 );
            hb_strncat( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
         }
         else
            hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );

         hb_xfree( pCurDir) ;
         szZipFileName = hb___CheckFile( szFile );

         if( hb_arrayLen(s_FileToZip) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_CompressFile( szZipFileName,
                                    s_FileToZip,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                    hb_param( 4, HB_IT_BLOCK ),
                                    ISLOG( 5 ) ? hb_parl( 5 ) : FALSE,
                                    ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                    ISLOG( 7 ) ? hb_parl( 7 ) : FALSE,
                                    ISLOG( 8 ) ? hb_parl( 8 ) : FALSE,
                                    pProgress );
            ResetAttribs();
            hb_itemRelease( pProgress );
         }

         hb_xfree( szZipFileName );
         hb_itemRelease(s_ExcludeFile);
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      char szFile[ _POSIX_PATH_MAX + 1 ];
      char *szZipFileName;
      PHB_ITEM pArray;

      hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );

      szZipFileName = hb___CheckFile( szFile );

      pArray = hb___GetFileNamesFromZip( szZipFileName,
                                ISLOG( 2 ) ? hb_parl( 2 ) : FALSE );

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
      char szFile[ _POSIX_PATH_MAX + 1 ];
      char * szZipFileName;

      hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );

      szZipFileName = hb___CheckFile( szFile );

      iRet = hb___GetNumberofFilestoUnzip( szZipFileName );

      hb_xfree( szZipFileName );
   }

   hb_retni( iRet );
}

/*
 * $DOC$
 * $FUNCNAME$
 *      HB_ZIPFILEBYTDSPAN()
 * $CATEGORY$
 *      Zip Functions
 * $ONELINER$
 *      Create a zip file
 * $SYNTAX$
 *      HB_ZIPFILEBYTDSPAN( <cFile> ,<cFileToCompress> | <aFiles>, <nLevel>,
 *      <bBlock>, <lOverWrite>, <cPassword>, <iSize>, <lWithPath>, <lWithDrive>,
 *      <pFileProgress>) ---> lCompress
 * $ARGUMENTS$
 *      <cFile>   Name of the zip file
 *
 *      <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *      can be used
 *          _or_
 *      <aFiles>  An array containing files to compress, Drive and/or path
 *      can be used
 *
 *      <nLevel>  Compression level ranging from 0 to 9
 *
 *      <bBlock>  Code block to execute while compressing
 *
 *      <lOverWrite>  Toggle to overwrite the file if exists
 *
 *      <cPassword> Password to encrypt the files
 *
 *      <iSize> Size of the archive, in bytes. Default is 1457664 bytes
 *
 *      <lWithPath> Toggle to store the path or not
 *
 *      <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *      <pFileProgress> Code block for File Progress
 * $RETURNS$
 *      <lCompress>  .t. if file was create, otherwise .f.
 * $DESCRIPTION$
 *      This function creates a zip file named <cFile>. If the extension
 *      is omitted, .ZIP will be assumed. If the second parameter is a
 *      character string, this file will be added to the zip file. If the
 *      second parameter is an array, all file names contained in <aFiles>
 *      will be compressed.
 *
 *      If <nLevel> is used, it determines the compression type where 0 means
 *      no compression and 9 means best compression.
 *
 *      If <bBlock> is used, every time the file is opened to compress it
 *      will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *      If <lOverWrite> is used, it toggles to overwrite or not the existing
 *      file. Default is to overwrite the file, otherwise if <lOverWrite> is
 *      false the new files are added to the <cFile>.
 *
 *      If <lWithPath> is used, it tells thats the path should also be stored '
 *      with the file name. Default is false.
 *
 *      If <lWithDrive> is used, it tells thats the Drive and path should also
 *      be stored with the file name. Default is false.
 *
 *      If <pFileProgress> is used, an Code block is evaluated, showing the total
 *      of that file has being processed.
 *      The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 * $EXAMPLES$
 *      FUNCTION MAIN()
 *
 *      IF HB_ZIPFILEBYTDSPAN( "TEST.ZIP", "TEST.PRG" )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYTDSPAN( "TEST1.ZIP", { "TEST.PRG", "c:\windows\win.ini" } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYTDSPAN( "TEST2.ZIP", { "TEST.PRG", "c:\windows\win.ini" }, 9, {|nPos,cFile| qout(cFile) }, "hello",, 521421 )
 *         qout("File was successfully created" )
 *      ENDIF
 *
 *      aFiles := { "TEST.PRG", "c:\windows\win.ini" }
 *      nLen   := Len( aFiles )
 *      aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "²" )
 *      GaugeDisplay( aGauge )
 *      HB_ZIPFILEBYTDSPAN( "test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen) },, "hello",, 6585452 )
 *      Return Nil
 * $STATUS$
 *      R
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPFILEBYTDSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if( pParam )
      {
         char szFile[ _POSIX_PATH_MAX + 1 ];
         PHB_ITEM pExclude = hb_param( 11, HB_IT_STRING | HB_IT_ARRAY );
         char *szZipFileName;
         BYTE *pCurDir;

         /* by JGS */
         BOOL bFullPath = TRUE;
         #if defined(HB_WIN32_IO)
            if( ISLOG( 12 ) )
            {
               bFullPath = hb_parl( 12 );
            }
         #endif
         /* by JGS */

         pCurDir = ( BYTE * )hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir, bFullPath );  /* bFullPath by JGS */

         hb_fsChDir( pCurDir );
         /* by JGS, wait until adding the directory to the file name if not specified
         hb_xfree( pCurDir );
         */
         if( ! strchr( szFile, OS_PATH_DELIMITER ) )
         {
            hb_strncpy( szFile, (char *) pCurDir, sizeof( szFile ) - 1 );
            hb_strncat( szFile, OS_PATH_DELIMITER_STRING, sizeof( szFile ) - 1 );
            hb_strncat( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
         }
         else
            hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );

         hb_xfree( pCurDir ); /* by JGS */
         szZipFileName = hb___CheckFile( szFile );

         if( hb_arrayLen(s_FileToZip) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 10 ) ? hb_itemNew( hb_param( 10, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_CmpTdSpan( szZipFileName,
                                 s_FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : FALSE,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISNUM( 7 ) ? hb_parni( 7 ) : 0,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : FALSE,
                                 ISLOG( 9 ) ? hb_parl( 9 ) : FALSE,
                                 pProgress );
            ResetAttribs();
            hb_itemRelease( pProgress );
         }

         hb_xfree( szZipFileName );
         hb_itemRelease(s_ExcludeFile);
      }
   }

   hb_retl( bRet );
}

/*
 * $DOC$
 * $FUNCNAME$
 *      HB_ZIPFILEBYPKSPAN()
 * $CATEGORY$
 *      Zip Functions
 * $ONELINER$
 *      Create a zip file on removable media
 * $SYNTAX$
 *      HB_ZIPFILEBYPKSPAN( <cFile>, <cFileToCompress> | <aFiles>, <nLevel>,
 *      <bBlock>, <lOverWrite>, <cPassword>, <lWithPath>, <lWithDrive>,
 *      <pFileProgress>) ---> lCompress
 * $ARGUMENTS$
 *      <cFile>   Name of the zip file
 *
 *      <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *      can be used
 *          _or_
 *      <aFiles>  An array containing files to compress, Drive and/or path
 *      can be used
 *
 *      <nLevel>  Compression level ranging from 0 to 9
 *
 *      <bBlock>  Code block to execute while compressing
 *
 *      <lOverWrite>  Toggle to overwrite the file if exists
 *
 *      <cPassword> Password to encrypt the files
 *
 *      <lWithPath> Toggle to store the path or not
 *
 *      <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *      <pFileProgress> Code block for File Progress
 * $RETURNS$
 *      <lCompress>  .t. if file was create, otherwise .f.
 * $DESCRIPTION$
 *      This function creates a zip file named <cFile>. If the extension
 *      is omitted, .ZIP will be assumed. If the second parameter is a
 *      character string, this file will be added to the zip file. If the
 *      second parameter is an array, all file names contained in <aFiles>
 *      will be compressed.  Also, the use of this function is for creating
 *      backup in removable media like an floppy drive/zip drive.
 *
 *      If <nLevel> is used, it determines the compression type where 0 means
 *      no compression and 9 means best compression.
 *
 *      If <bBlock> is used, every time the file is opened to compress it
 *      will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *      If <lOverWrite> is used , it toggles to overwrite or not the existing
 *      file. Default is to overwrite the file, otherwise if <lOverWrite> is false
 *      the new files are added to the <cFile>.
 *
 *      If <cPassword> is used, all files that are added to the archive are encrypted
 *      with the password.
 *
 *      If <lWithPath> is used, it tells thats the path should also be stored with
 *      the file name. Default is false.
 *
 *      If <lWithDrive> is used, it tells thats the Drive and path should also be stored
 *      with the file name. Default is false.
 *
 *      If <pFileProgress> is used, an Code block is evaluated, showing the total
 *      of that file has being processed.
 *      The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 *
 *      Before calling this function, Set an Changedisk codeblock by calling
 *      the HB_SETDISKZIP().
 * $EXAMPLES$
 *      FUNCTION MAIN()
 *
 *      hb_setdiskzip( {|nDisk| Alert( "Please insert disk no " + Str( nDisk, 3 ) ) } )
 *
 *      IF HB_ZIPFILEBYPKSPAN( "a:\TEST.ZIP", "TEST.PRG" )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYPKSPAN( "a:\TEST1.ZIP", { "TEST.PRG", "c:\windows\win.ini" } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYPKSPAN( "TEST2.ZIP", { "TEST.PRG", "c:\windows\win.ini"}, 9, {|nPos,cFile| qout(cFile) } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      aFiles := { "TEST.PRG", "c:\windows\win.ini" }
 *      nLen   := Len( aFiles )
 *      aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "²" )
 *      GaugeDisplay( aGauge )
 *      HB_ZIPFILEBYPKSPAN( "f:\test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen ) },, "hello" )
 *      // assuming f:\ is a Zip Drive
 *      Return Nil
 * $STATUS$
 *      R
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPFILEBYPKSPAN )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pParam = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY );

      if( pParam )
      {
         char szFile[ _POSIX_PATH_MAX + 1 ];
         PHB_ITEM pExclude = hb_param( 10, HB_IT_STRING | HB_IT_ARRAY );
         char *szZipFileName;
         BYTE * pCurDir ;

         /* by JGS */
         BOOL bFullPath = TRUE;
         #if defined(HB_WIN32_IO)
            if( ISLOG( 11 ) )
            {
               bFullPath = hb_parl( 11 );
            }
         #endif
         /* by JGS */

         pCurDir = ( BYTE * )hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * )hb_fsCurDir( 0 ) , NULL );

         ZipCreateExclude( pExclude );

         ZipCreateArray( pParam, pCurDir, bFullPath );  /* bFullPath by JGS */

         hb_fsChDir( pCurDir ) ;
         /* by JGS, wait until adding the directory to the file name if not specified
         hb_xfree( pCurDir ) ;
         hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
         */
         if( ! strchr( szFile, OS_PATH_DELIMITER ) )
         {
            hb_strncpy( szFile, (char *) pCurDir, sizeof( szFile ) - 1 );
            hb_strncat( szFile, OS_PATH_DELIMITER_STRING, sizeof( szFile ) - 1 );
            hb_strncat( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
         }
         else
            hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );

         hb_xfree( pCurDir );
         /* by JGS */
         szZipFileName = hb___CheckFile( szFile );

         if( hb_arrayLen(s_FileToZip) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 9 ) ? hb_itemNew( hb_param( 9, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_CmpPkSpan( szZipFileName,
                                 s_FileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : ( -1 ),
                                 hb_param( 4, HB_IT_BLOCK ),
                                 ISLOG( 5 ) ? hb_parl( 5 ) : FALSE,
                                 ISCHAR( 6 ) ? hb_parc( 6 ) : NULL,
                                 ISLOG( 7 ) ? hb_parl( 7 ) : FALSE,
                                 ISLOG( 8 ) ? hb_parl( 8 ) : FALSE,
                                 pProgress );
            ResetAttribs();
            hb_itemRelease( pProgress );
         }

         hb_xfree( szZipFileName );
         hb_itemRelease(s_ExcludeFile);
      }
   }

   hb_retl( bRet );
}

/*
 * $DOC$
 * $FUNCNAME$
 *      HB_UNZIPFILE()
 * $CATEGORY$
 *      Zip Functions
 * $ONELINER$
 *      Unzip a compressed file
 * $SYNTAX$
 *      HB_UNZIPFILE( <cFile>, <bBlock>, <lWithPath>, <cPassWord>, <cPath>,
 *      <cFile> | <aFile>, <pFileProgress> ) ---> lCompress
 * $ARGUMENTS$
 *      <cFile>   Name of the zip file to extract
 *
 *      <bBlock>  Code block to execute while extracting
 *
 *      <lWithPath> Toggle to create directory if needed
 *
 *      <cPassWord> Password to use to extract files
 *
 *      <cPath>    Path to extract the files to - mandatory
 *
 *      <cFile> | <aFiles> A File or Array of files to extract - mandatory
 *
 *      <pFileProgress> Code block for File Progress
 * $RETURNS$
 *      <lCompress>  .t. if all file was successfully restored, otherwise .f.
 * $DESCRIPTION$
 *      This function restores all files contained inside the <cFile>.
 *      If the extension is omitted, .ZIP will be assumed. If a file already
 *      exists, it will be overwritten.
 *
 *      If <bBlock> is used, every time the file is opened to compress it
 *      will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *      The <cPath> is a mandatory parameter. Set to ".\" to extract to the
 *      current directory
 *
 *      If <cFile> or <aFiles> are not provided, no files will be extracted!
 *      Make sure you provide the file or files you want extracted
 *
 *      If <pFileProgress> is used, an Code block is evaluated, showing the total
 *      of that file has being processed.
 *      The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 * $EXAMPLES$
 *      FUNCTION MAIN()
 *
 *      aExtract := hb_GetFilesInZip( "TEST.ZIP" )  // extract all files in zip
 *      IF HB_UNZIPFILE( "TEST.ZIP",,,, ".\", aExtract )
 *         qout("File was successfully extracted")
 *      ENDIF
 *
 *      aExtract := hb_GetFilesInZip( "TEST2.ZIP" )  // extract all files in zip
 *      IF HB_UNZIPFILE( "TEST2.ZIP", {|cFile| qout( cFile ) },,, ".\", aExtract )
 *         qout("File was successfully extracted")
 *      ENDIF
 *      Return Nil
 * $STATUS$
 *      R
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_UNZIPFILE )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) && ( ISARRAY( 6 ) || ISCHAR( 6 ) ) )
   {
      char szFile[ _POSIX_PATH_MAX + 1 ];
      PHB_ITEM pUnzip = hb_param( 6, HB_IT_ANY );
      char *szZipFileName;
      BYTE *pCurDir;

      pCurDir = ( BYTE * ) hb_xstrcpy( NULL, OS_PATH_DELIMITER_STRING, ( const char * ) hb_fsCurDir( 0 ) , NULL );

      hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );

      szZipFileName = hb___CheckFile( szFile );

      s_UnzipFiles = hb_itemArrayNew( 0 );

      if( hb_TestForPKS( szZipFileName ) <= 0 )
      {
         hb___GetFileNamesFromZip( szZipFileName, TRUE );

      if( pUnzip )
      {
         if( HB_IS_STRING( pUnzip ) )
         {
            UnzipCreateArray( hb_itemGetCPtr( pUnzip ), 1 );
         }
         else if( HB_IS_ARRAY( pUnzip ) )
         {
            int uiZ, uiZLen = hb_arrayLen(pUnzip);
            char *szUnzip;

            for( uiZ = 0; uiZ < uiZLen; uiZ++ )
            {
               szUnzip = hb_arrayGetC( pUnzip, uiZ + 1 );

               if( szUnzip )
               {
                  UnzipCreateArray( szUnzip, 1 );
                  hb_xfree( szUnzip );
               }
            }
         }
      }
      else
      {
//s.r. change "*.*" to "*" because file without extension were ignored
         UnzipCreateArray( (char*) "*", 1 );
      }

      if( hb_arrayLen(s_UnzipFiles) > 0 )
      {
         PHB_ITEM pProgress = ISBLOCK( 7 ) ? hb_itemNew( hb_param( 7 , HB_IT_BLOCK ) ) : hb_itemNew( NULL );
         bRet = hb_UnzipSel( szZipFileName,
                             hb_param( 2, HB_IT_BLOCK ),
                             ISLOG( 3 ) ? hb_parl( 3 ) : FALSE,
                             ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                             ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                             s_UnzipFiles,
                             pProgress );
         hb_itemRelease( pProgress );
      }

      hb_xfree( szZipFileName );
      hb_itemRelease( s_UnzipFiles );
      hb_fsChDir( pCurDir ) ;
      hb_xfree( pCurDir ) ;
      hb_itemClear( hbza_ZipArray );
      hb_itemRelease( hbza_ZipArray );
    }
   }

   hb_retl( bRet );
}

/* $DOC$
 * $FUNCNAME$
 *     HB_SETDISKZIP()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Set an codeblock for disk changes
 * $SYNTAX$
 *     HB_SETDISKZIP( <bBlock> ) ---> TRUE
 * $ARGUMENTS$
 *     <bBlock> an Code block that contains an function that will be performed
 *     when the need of changing disk are need.
 * $RETURNS$
 *     It always returns True
 * $DESCRIPTION$
 *     This function will set an codeblock that will be evaluated every time
 *     that an changedisk event is necessary. <bBlock> receives nDisk as a
 *     code block param that corresponds to the diskette number to be processed.
 *
 *     Set this function before opening archives that are in removable media.
 *     This block will be released, when the caller finish it job.
 * $EXAMPLES$
 *      HB_SETDISKZIP( {|nDisk| Alert( "Please insert disk no " + Str( nDisk, 3 ) ) } )
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_SETDISKZIP )
{
   hb_retl( hb___SetCallbackFunc( hb_param( 1, HB_IT_BLOCK ) ) );
}

/* $DOC$
 * $FUNCNAME$
 *     HB_ZIPDELETEFILES()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Delete files from an zip archive
 * $SYNTAX$
 *     HB_ZIPDELETEFILES( <cFile>, <cFiletoDelete> | <aFiles> | <nFilePos> ) --> <lDeleted>
 * $ARGUMENTS$
 *     <cFile>  The name of the zip files from where the files will be deleted
 *
 *     <cFiletoDelete> An File to be removed
 *        _or_
 *     <aFiles>    An Array of Files to be removed
 *        _or_
 *     <nFilePos> The Position of the file to be removed
 * $RETURNS$
 *     <lDeleted> If the files are deleted, it will return .T.; otherwise
 *     it will return .f. in the following cases: Spanned Archives; the file(s)
 *     could not be found in the zip file.
 * $DESCRIPTION$
 *     This  function removes files from an Zip archive.
 * $EXAMPLES$
 *     ? "has the file zipnew.i been deleted ", if( HB_ZIPDELETEFILES( "\test23.zip", "zipnew.i" ), "Yes", "No" )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPDELETEFILES )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pDelZip = hb_param( 2, HB_IT_STRING | HB_IT_ARRAY | HB_IT_NUMERIC );

      s_DelZip = hb_itemArrayNew(0);

      if( pDelZip )
      {
         char szFile[ _POSIX_PATH_MAX + 1 ];
         char *szZipFileName;
         int ulLen;

         hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
         szZipFileName = hb___CheckFile( szFile );

         hb___GetFileNamesFromZip( szZipFileName, TRUE );
         ulLen = hb_arrayLen(hbza_ZipArray);

         if( !ulLen )
         {
            hb_xfree( szZipFileName );
            hb_itemClear( hbza_ZipArray );
            hb_itemRelease( hbza_ZipArray );
            hb_retl ( bRet );
            return;
         }

         if( HB_IS_STRING( pDelZip ) )
         {
            if( hb_itemGetCLen( pDelZip ) > 0 )
            {
               UnzipCreateArray( hb_itemGetCPtr( pDelZip ), 2 );
            }
         }
         else if( HB_IS_ARRAY( pDelZip ) )
         {
            int uiInLen = hb_arrayLen(pDelZip);

            if( uiInLen > 0 )
            {
               int uiIn;
               char * szInput;

               for( uiIn = 0; uiIn < uiInLen; uiIn++ )
               {
                  szInput = hb_arrayGetC( pDelZip, uiIn + 1 );

                  if( szInput )
                  {
                     UnzipCreateArray( szInput, 2 );
                     hb_xfree( szInput );
                  }
               }

            }
         }
         else if( HB_IS_NUMERIC( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );
            PHB_ITEM Temp;

            if( iIndex > 0 && iIndex <= ulLen )
            {
               PHB_ITEM pZipEntry = hb_arrayGetItemPtr( hbza_ZipArray, iIndex );
               char* szEntry = hb_arrayGetC( pZipEntry, 1 );
               Temp = hb_itemNew(NULL);
               hb_arrayAddForward( s_DelZip, hb_itemPutC( Temp, szEntry ) );
               hb_xfree( szEntry );
               hb_itemRelease( Temp );
            }
         }

         if( hb_arrayLen(s_DelZip) > 0 )
         {
            bRet = hb_DeleteSel( szZipFileName,
                                 s_DelZip,
                                 ISLOG( 3 ) ? hb_parl( 3 ) : FALSE );
         }

         hb_xfree(szZipFileName);
         hb_itemClear( hbza_ZipArray );
         hb_itemRelease( hbza_ZipArray );
      }

      hb_itemRelease( s_DelZip );
   }

   hb_retl( bRet );
}

/* $DOC$
 * $FUNCNAME$
 *     HB_ZIPTESTPK()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Test pkSpanned zip files
 * $SYNTAX$
 *     HB_ZIPTESTPK( <cFile> ) --> <nReturnCode>
 * $ARGUMENTS$
 *     <cFile>  File to be tested.
 * $RETURNS$
 *     <nReturn> A code that tells if the current disk is the last of a
 *     pkSpanned disk set.
 * $DESCRIPTION$
 *     This function tests if the disk inserted is the last disk of an backup
 *     set or not.
 *     It will return the follow return code when an error is found
 *
 *     <table>
 *     Error code     Meaning
 *     114            Incorrect Disk
 *     103            No Call back was set with HB_SETDISKZIP()
 *     </table>
 *
 *     Call this function to determine if the disk inserted is the correct
 *     one before any other function.
 * $EXAMPLES$
 *      if HB_ZIPTESTPK( "a:\test22.zip" ) == 114
 *          ? "Invalid Diskette"
 *      endif
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPTESTPK )
{
   char szFile[ _POSIX_PATH_MAX + 1 ];
   char * szZipFileName;

   hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
   szZipFileName = hb___CheckFile( szFile );

   hb_retni( hb_TestForPKS( szZipFileName ) );

   hb_xfree(szZipFileName);
}

/* $DOC$
 * $FUNCNAME$
 *     HB_SETBUFFER()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *
 * $SYNTAX$
 *     HB_SETBUFFER( [<nWriteBuffer>], [<nExtractBuffer>], [<nReadBuffer>] ) --> Nil
 * $ARGUMENTS$
 *     <nWriteBuffer>   The size of the write buffer.
 *
 *     <nExtractBuffer> The size of the extract buffer.
 *
 *     <nReadBuffer>    The size of the read buffer.
 * $RETURNS$
 *     <NIL>            This function always returns NIL.
 * $DESCRIPTION$
 *     This function set the size of the internal buffers for write/extract/read
 *     operation
 *
 *     If the size of the buffer is smaller then the default, the function
 *     will automatically use the default values, which are 65535/16384/32768
 *     respectively.
 *
 *     This function be called before any of the compression/decompression
 *     functions.
 * $EXAMPLES$
 *     HB_SETBUFFER( 100000, 115214, 65242 )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_SETBUFFER )
{
   hb_SetZipBuff( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

/* $DOC$
 * $FUNCNAME$
 *     HB_SETZIPCOMMENT()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Set an Zip archive Comment
 * $SYNTAX$
 *     HB_SETZIPCOMMENT( <cComment> ) --> Nil
 * $ARGUMENTS$
 *     <cComment>   Comment to add to the zip archive
 * $RETURNS$
 *     <NIL> this function always return NIL
 * $DESCRIPTION$
 *     This function stored an global comment to an zip archive.
 *     It should be called before any of the compression functions.
 * $EXAMPLES$
 *     HB_SETZIPCOMMENT( "This is an Test" )
 *     hb_zipfile( "test.zip", { "\windows\ios.ini", "\windows\win.ini" } )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_SETZIPCOMMENT )
{
   hb_SetZipComment( hb_parc( 1 ) );
}

/* $DOC$
 * $FUNCNAME$
 *     HB_GETZIPCOMMENT()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Return the comment of an zip file
 * $SYNTAX$
 *     HB_GETZIPCOMMENT( <szFile> ) --> <szComment>
 * $ARGUMENTS$
 *     <szFile>  File to get the comment from
 * $RETURNS$
 *     <szComment>  The comment that was stored in <szFile>
 * $DESCRIPTION$
 *     This function receives a valid zip file name as parameter,
 *     and returns the global comment stored within.
 * $EXAMPLES$
 *     ? "The comment in test.zip is ", HB_GETZIPCOMMENT( "test.zip" )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_GETZIPCOMMENT )
{
   hb_retc_buffer( ( char * ) hb_GetZipComment( hb_parc( 1 ) ) );
}

/*
 * $DOC$
 * $FUNCNAME$
 *      HB_UNZIPFILEINDEX()
 * $CATEGORY$
 *      Zip Functions
 * $ONELINER$
 *      Unzip a compressed file referenced by it number in the zipfile
 * $SYNTAX$
 *      HB_UNZIPFILEINDEX( <cFile>, <bBlock>, <lWithPath>, <cPassWord>, <cPath>,
 *      <nFile> | <anFiles>, <pFileProgress> ) ---> lCompress
 * $ARGUMENTS$
 *      <cFile>   Name of the zip file
 *
 *      <bBlock>  Code block to execute while compressing
 *
 *      <lWithPath> Toggle to create directory if needed
 *
 *      <cPassWord> Password to use to extract files
 *
 *      <cPath>    Path to extract the files to - mandatory.
 *
 *      <cFile> | <anFiles> A File or Array of files position to extract - mandatory
 *
 *      <pFileProgress> Code block for File Progress
 * $RETURNS$
 *      <lCompress>  .t. if all file was successfully restored, otherwise .f.
 * $DESCRIPTION$
 *      This function restores all files contained inside the <cFile>.
 *      If the extension is omitted, .ZIP will be assumed. If a file already
 *      exists, it will be overwritten.
 *
 *      If <bBlock> is used, every time the file is opened to compress it
 *      will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *      The <cPath> is a mandatory parameter. Set to ".\" to extract to the
 *      current dir
 *
 *      If <cFile> or <anFiles> are not provided, no files will be extracted!
 *      Make sure you provide the file or files you want extracted
 *
 *      If <pFileProgress> is used, an Code block is evaluated, showing the total
 *      of that file has being processed.
 *      The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 * $EXAMPLES$
 *      FUNCTION MAIN()
 *
 *      IF HB_UNZIPFILEINDEX( "TEST.ZIP",,,, ".\", 1 )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_UNZIPFILEINDEX( "TEST2.ZIP", {|cFile|, qout(cFile) },,, ".\", { 1, 2 } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      Return Nil
 * $STATUS$
 *      R
 * $COMPLIANCE$
 *      This function is a Harbour extension
 * $PLATFORMS$
 *      All
 * $FILES$
 *      Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_UNZIPFILEINDEX )
{
   BOOL bRet = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pDelZip = hb_param( 6, HB_IT_NUMERIC | HB_IT_ARRAY );

      if( pDelZip )
      {
         char szFile[ _POSIX_PATH_MAX + 1 ];
         PHB_ITEM Temp,DelZip;
         char* szZipFileName;
         int ulLen;

         DelZip = hb_itemArrayNew(0);

         hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
         szZipFileName = hb___CheckFile( szFile );

         hb___GetFileNamesFromZip( szZipFileName, TRUE );
         ulLen = hb_arrayLen(hbza_ZipArray);

         if( HB_IS_NUMERIC( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );

            if( iIndex > 0 && iIndex <= ulLen )
            {
               Temp = hb_itemNew(NULL);
               hb_arrayAddForward( DelZip, hb_itemPutNI( Temp, iIndex ) );
               hb_itemRelease( Temp );
            }
         }
         else
         {
            int ui, iIndex;

            for( ui = 0; ui < ulLen; ui++ )
            {
               iIndex = hb_arrayGetNI( pDelZip, ui + 1 );
               if( iIndex && iIndex > 0 && iIndex <= ulLen )
               {
                  Temp = hb_itemNew( NULL );
                  hb_arrayAddForward( DelZip, hb_itemPutNI( Temp, iIndex ) );
                  hb_itemRelease( Temp );
               }
            }
         }

         if( hb_arrayLen( DelZip ) > 0 )
         {
            PHB_ITEM pProgress = ISBLOCK( 7 ) ? hb_itemNew( hb_param( 7 , HB_IT_BLOCK ) ) : hb_itemNew( NULL );
            bRet = hb_UnzipSelIndex( szZipFileName,
                                     hb_param( 2, HB_IT_BLOCK ),
                                     ISLOG( 3 ) ? hb_parl( 3 ) : FALSE,
                                     ISCHAR( 4 ) ? hb_parc( 4 ) : NULL,
                                     hb_parc( 5 ),
                                     DelZip,
                                     pProgress );
            hb_itemRelease( pProgress );
         }

         hb_itemRelease( DelZip );
         hb_xfree( szZipFileName );
         hb_itemClear( hbza_ZipArray );
         hb_itemRelease( hbza_ZipArray );

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

HB_FUNC( SETZIPREADONLY )
{
   hb_SetZipReadOnly( hb_parl( 1 ) );
}

HB_FUNC(HB_UNZIPALLFILE)
{
    if( ! ISCHAR( 6 ) && ! ISARRAY( 6 ) )
    {
        char szFile[ _POSIX_PATH_MAX + 1 ];
        char * szZipFile;
        PHB_ITEM pProgress = ISBLOCK( 7 ) ? hb_itemNew( hb_param( 7, HB_IT_BLOCK ) ) : hb_itemNew( NULL );
        hb_strncpy( szFile, hb_parc( 1 ), sizeof( szFile ) - 1 );
        szZipFile = hb___CheckFile( szFile );
        hb_retl( hb_UnzipAll( szZipFile,
                              hb_param( 2, HB_IT_BLOCK ),
                              ISLOG( 3 ) ? hb_parl( 3 ) : FALSE,
                              hb_parc( 4 ),
                              hb_parc( 5 ),
                              hb_param( 6, HB_IT_BLOCK ),
                              pProgress ) );
        hb_xfree( szZipFile );
        hb_itemRelease( pProgress );
    }
}
