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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#include "directry.ch"

HB_EXTERN_BEGIN

extern void         hb_fsDirectory( PHB_ITEM pDir, char* szSkleton, char* szAttributes, BOOL bDirOnly, BOOL bFullPath );

extern BOOL         hb_TransferFilesFromzip(char *szSource,char *szDest,PHB_ITEM pArray);
extern PHB_ITEM     hb___GetFileNamesFromZip(char *szFile,BOOL iMode);
extern int          hb___GetNumberofFilestoUnzip(char *szFile);
extern int          hb___SetCallbackFunc(PHB_ITEM pFunc);
extern int          hb_CmpPkSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress);
extern int          hb_CmpTdSpan(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,int iSpanSize ,BOOL bPath ,BOOL bDrive,PHB_ITEM pProgress);
extern int          hb_CompressFile(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite,char *szPassWord,BOOL bPath,BOOL bDrive,PHB_ITEM pProgress);
extern int          hb_UnzipAll(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *pbyBuffer,PHB_ITEM pDiskBlock,PHB_ITEM pProgress);
extern int          hb_UnzipSel(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,const char *szPath,PHB_ITEM pArray,PHB_ITEM pProgress);
extern int          hb_UnzipSelIndex(char *szFile,PHB_ITEM pBlock,BOOL bWithPath,char *szPassWord,char *szPath,PHB_ITEM pSelArray,PHB_ITEM pProgress);
extern int          hb_DeleteSel(char *szFile,PHB_ITEM pArray,BOOL bCase);
extern int          hb_TestForPKS(char *szFile);
extern void         hb_SetZipBuff(int a,int b,int c);
extern void         hb_SetZipComment(char *szComment);
extern const char * hb_GetZipComment(char *szFile);
extern BOOL         hb_IsPassWord(char *szName);
extern void         hb_SetZipReadOnly(BOOL bRead);

HB_EXTERN_END

#if defined( HB_OS_LINUX )
   #include <sys/types.h>
   #include <sys/stat.h>
#endif

#if defined( HB_OS_LINUX )

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

static void ResetAttribs( PHB_ITEM pFileToZip, PHB_ITEM pFileAttribs )
{
   ULONG ulAtt;
   ULONG ulZipLen = hb_arrayLen( pFileToZip );

   for( ulAtt = 0; ulAtt < ulZipLen; ulAtt++ )
   {
      SetFileAttributes( hb_arrayGetCPtr( pFileToZip, ulAtt + 1 ),
                         hb_arrayGetNI( pFileAttribs, ulAtt + 1 ) );
   }
}

static void UnzipCreateArray( const char * szSkleton, PHB_ITEM pList, PHB_ITEM pZipArray )
{
   int ul;
   int ulLen = hb_arrayLen( pZipArray );

   for( ul = 0; ul < ulLen; ul++ )
   {
      PHB_ITEM pZipEntry = hb_arrayGetItemPtr( pZipArray, ul + 1 );
      const char * szEntry = hb_arrayGetCPtr( pZipEntry, 1 );
      BOOL bOkAdd = TRUE;

      if( szSkleton )
         bOkAdd = hb_strMatchFile( szEntry, ( const char * ) szSkleton );

      if( !bOkAdd )
      {
         PHB_FNAME pFileName = hb_fsFNameSplit( szEntry );

         if( pFileName->szName )
         {
            char * szFile = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
            pFileName->szPath = ( char * ) "";
            hb_fsFNameMerge( szFile, pFileName );
            bOkAdd = ( hb_stricmp( szSkleton, szFile ) == 0 );
            hb_xfree( szFile );

            if( ! bOkAdd )
               bOkAdd = ( hb_stricmp( szSkleton, szEntry ) == 0 );
         }

         hb_xfree( pFileName );
      }

      if( bOkAdd )
      {
         PHB_ITEM Temp = hb_itemNew( NULL );
         hb_arrayAddForward( pList, hb_itemPutC( Temp, szEntry ) );
         hb_itemRelease( Temp );
      }
   }
}

static BOOL ZipTestExclude( char * szEntry, PHB_ITEM pExcludeFile )
{
   int uiEx;
   int uiExLen = hb_arrayLen( pExcludeFile );

   for( uiEx = 0; uiEx < uiExLen; uiEx++ )
   {
      if( strcmp( hb_arrayGetCPtr( pExcludeFile, uiEx + 1 ), hb_strupr( szEntry ) ) == 0 )
         return FALSE;
   }

   return TRUE;
}

static PHB_ITEM ZipCreateExclude( PHB_ITEM pExclude )
{
   PHB_ITEM pExcludeFile = hb_itemArrayNew( 0 );

   if( pExclude )
   {
      if( HB_IS_STRING( pExclude ) )
      {
         if( strchr( hb_itemGetCPtr( pExclude ), '*' ) != NULL ||
             strchr( hb_itemGetCPtr( pExclude ), '?' ) != NULL )
         {
            PHB_ITEM WildFile;
            int uiLen;
            int ui;

            WildFile = hb_itemNew( NULL );

            hb_fsDirectory( WildFile, hb_itemGetCPtr( pExclude ), NULL, 0, TRUE );
            uiLen = hb_arrayLen( WildFile );

            for( ui = 0; ui < uiLen; ui++ )
            {
               PHB_ITEM ExTmp = hb_itemPutC( NULL, hb_arrayGetCPtr( hb_arrayGetItemPtr( WildFile, ui + 1 ), F_NAME ) );
               hb_arrayAddForward( pExcludeFile, ExTmp );
               hb_itemRelease( ExTmp );
            }

            hb_itemRelease( WildFile );
         }
         else if( hb_itemGetCLen( pExclude ) > 0 )
         {
            PHB_ITEM ExTmp = hb_itemPutC( NULL, hb_itemGetCPtr( pExclude ) );
            hb_arrayAddForward( pExcludeFile, ExTmp );
            hb_itemRelease( ExTmp );
         }
      }
      else if( HB_IS_ARRAY( pExclude ) )
      {
         int ux;
         int ufx = hb_arrayLen( pExclude );

         for( ux = 0; ux < ufx; ux++ )
         {
            char * szExclude = hb_arrayGetCPtr( pExclude, ux + 1 );

            if( strchr( szExclude, '*' ) != NULL ||
                strchr( szExclude, '?' ) != NULL )
            {
               PHB_ITEM WildFile = hb_itemNew( NULL );
               int uiW, uiWLen;
      
               hb_fsDirectory( WildFile, szExclude, NULL, 0, TRUE );
               uiWLen = hb_arrayLen( WildFile );
      
               for( uiW = 0; uiW < uiWLen; uiW++ )
               {
                  PHB_ITEM ExTmp = hb_itemNew( NULL );
                  hb_arrayAddForward( pExcludeFile, hb_itemPutC( ExTmp, hb_arrayGetCPtr( hb_arrayGetItemPtr( WildFile, uiW + 1 ), F_NAME ) ) );
                  hb_itemRelease( ExTmp );
               }
      
               hb_itemRelease( WildFile );
            }
            else
            {
               PHB_ITEM ExTmp = hb_itemNew( NULL );
               hb_arrayAddForward( pExcludeFile, hb_itemPutC( ExTmp, szExclude ) );
               hb_itemRelease( ExTmp );
            }
         }
      }
   }

   return pExcludeFile;
}

static void ZipCreateArray( PHB_ITEM pParam, BOOL bFullPath, PHB_ITEM pFileToZip, PHB_ITEM pFileAttribs, PHB_ITEM pExclude )
{
   BYTE * pCurDir = ( BYTE * ) hb_xstrcpy( NULL, HB_OS_PATH_DELIM_CHR_STRING, ( const char * ) hb_fsCurDir( 0 ), NULL );

   PHB_ITEM pExcludeFile = ZipCreateExclude( pExclude );
   PHB_ITEM pDirEntry, Temp, TempArray;
   PHB_ITEM WildFile = hb_itemNew( NULL );
   int ul, ulLen, ulArr, ulLenArr;

   if( HB_IS_STRING( pParam ) )
   {
      TempArray = hb_itemArrayNew( 0 );
      Temp = hb_itemPutC( NULL, hb_itemGetCPtr( pParam ) );
      hb_arrayAddForward( TempArray, Temp );
      hb_itemRelease( Temp );
   }
   else
      TempArray = hb_arrayClone( pParam );

   ulLenArr = hb_arrayLen( TempArray );

   for( ulArr = 0; ulArr < ulLenArr; ulArr++ )
   {
      char * szArrEntry = hb_arrayGetC( TempArray, ulArr + 1 );

      if( szArrEntry )
      {
         if( strchr( szArrEntry, '*' ) != NULL || 
             strchr( szArrEntry, '?' ) != NULL )
         {
         #if !defined( HB_OS_UNIX_COMPATIBLE )
            /* by JGS if don't gave path or there is a relative path add current dir ! */
            PHB_FNAME fDirSpec = hb_fsFNameSplit( ( char * ) szArrEntry );

            if( ! fDirSpec->szDrive && 
                fDirSpec->szPath && 
                fDirSpec->szPath[ 0 ] != HB_OS_PATH_DELIM_CHR )
         #else
            /* if don't gave path add current dir ! */
            if( ! strchr( szArrEntry, HB_OS_PATH_DELIM_CHR ) )
         #endif
            {
               char * szTemp = szArrEntry;
               szArrEntry = hb_xstrcpy( NULL, ( char * ) pCurDir, HB_OS_PATH_DELIM_CHR_STRING, szTemp, NULL );
               hb_xfree( szTemp );
            }

            hb_fsDirectory( WildFile, szArrEntry, NULL, 0, bFullPath ); /* bFullPath by JGS */
            ulLen = hb_arrayLen( WildFile );

            for( ul = 0; ul < ulLen; ul++ )
            {
               char * szEntry;
               pDirEntry = hb_arrayGetItemPtr( WildFile, ul + 1 );
               szEntry = hb_arrayGetC( pDirEntry, F_NAME );

               #if !defined( HB_OS_UNIX_COMPATIBLE ) /* by JGS */
                  if( ! bFullPath && fDirSpec->szPath )
                  {
                     char * szFile = szEntry;
                     szEntry = hb_xstrcpy( NULL, fDirSpec->szPath, szFile, NULL );
                     hb_xfree( szFile );
                  }
               #endif

               if( ZipTestExclude( szEntry, pExcludeFile ) )
               {
                  Temp = hb_itemNew( NULL );
                  hb_arrayAddForward( pFileToZip, hb_itemPutC( Temp, szEntry ) );
                  hb_itemRelease( Temp );
                  Temp = hb_itemNew( NULL );
                  hb_arrayAddForward( pFileAttribs, hb_itemPutNI( Temp, GetFileAttributes( szEntry ) ) );
                  hb_itemRelease( Temp );

                  #if defined(HB_OS_LINUX)
                     SetFileAttributes( szEntry, 0777 );
                  #else
                     SetFileAttributes( szEntry, HB_FA_ARCHIVE );
                  #endif
               }

               if( szEntry )
                  hb_xfree( szEntry );
            }

            #if !defined( HB_OS_UNIX_COMPATIBLE ) /* by JGS */
               hb_xfree( fDirSpec );
            #endif

            hb_itemClear( WildFile );
         }
         else
         {
            Temp = hb_itemPutC( NULL, szArrEntry );
            hb_arrayAddForward( pFileToZip, Temp );
            hb_itemRelease( Temp );
            Temp = hb_itemPutNI( NULL, GetFileAttributes( szArrEntry ) );
            hb_arrayAddForward( pFileAttribs, Temp );
            hb_itemRelease( Temp );

            #if defined(HB_OS_LINUX)
               SetFileAttributes( szArrEntry, 0777 );
            #else
               SetFileAttributes( szArrEntry, HB_FA_ARCHIVE );
            #endif
         }

         hb_xfree( szArrEntry );
      }
   }

   hb_itemRelease( WildFile );
   hb_itemRelease( TempArray );

   hb_itemRelease( pExcludeFile );

   hb_xfree( pCurDir );
}

static char * hb___CheckFile( const char * szFile )
{
   PHB_FNAME pFileName = hb_fsFNameSplit( szFile );
   char * szZipName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".zip";

   hb_fsFNameMerge( szZipName, pFileName );
   hb_xfree( pFileName );

   return szZipName;
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
 *      <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *      This function creates a zip file named <cFile>. If the extension
 *      is omitted, .zip will be assumed. If the second parameter is a
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
 *      IF HB_ZIPFILE( "test.zip", "test.prg" )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILE( "test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILE( "test2.zip", { "test.prg", "C:\windows\win.ini" }, 9, {|cFile,nPos,| qout(cFile) } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      aFiles := { "test.prg", "C:\windows\win.ini" }
 *      nLen   := Len( aFiles )
 *      aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B" , "²" )
 *      GaugeDisplay( aGauge )
 *      HB_ZIPFILE( "test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen ) },, "hello" )
 *      Return NIL
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
         char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );
         PHB_ITEM pFileToZip = hb_itemArrayNew( 0 );
         PHB_ITEM pFileAttribs = hb_itemArrayNew( 0 );

         ZipCreateArray( pParam,
                         ISLOG( 11 ) ? hb_parl( 11 ) : TRUE, 
                         pFileToZip, 
                         pFileAttribs, 
                         hb_param( 10, HB_IT_STRING | HB_IT_ARRAY ) );

         if( hb_arrayLen( pFileToZip ) > 0 )
         {
            bRet = hb_CompressFile( szZipFileName,
                                    pFileToZip,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : -1,
                                    hb_param( 4, HB_IT_BLOCK ),
                                    hb_parl( 5 ),
                                    hb_parc( 6 ),
                                    hb_parl( 7 ),
                                    hb_parl( 8 ),
                                    hb_param( 9, HB_IT_BLOCK ) );

            ResetAttribs( pFileToZip, pFileAttribs );
         }

         hb_itemRelease( pFileAttribs );
         hb_itemRelease( pFileToZip );
         hb_xfree( szZipFileName );
      }
   }

   hb_retl( bRet );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );

      hb_itemReturnRelease( hb___GetFileNamesFromZip( szZipFileName, hb_parl( 2 ) ) );

      hb_xfree( szZipFileName );
   }
   else
      hb_reta( 0 );
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
      char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );

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
 *      <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *      This function creates a zip file named <cFile>. If the extension
 *      is omitted, .zip will be assumed. If the second parameter is a
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
 *      IF HB_ZIPFILEBYTDSPAN( "test.zip", "test.prg" )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYTDSPAN( "test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYTDSPAN( "test2.zip", { "test.prg", "C:\windows\win.ini" }, 9, {|nPos,cFile| qout(cFile) }, "hello",, 521421 )
 *         qout("File was successfully created" )
 *      ENDIF
 *
 *      aFiles := { "test.prg", "C:\windows\win.ini" }
 *      nLen   := Len( aFiles )
 *      aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "²" )
 *      GaugeDisplay( aGauge )
 *      HB_ZIPFILEBYTDSPAN( "test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen) },, "hello",, 6585452 )
 *      Return NIL
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
         char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );
         PHB_ITEM pFileToZip = hb_itemArrayNew( 0 );
         PHB_ITEM pFileAttribs = hb_itemArrayNew( 0 );

         ZipCreateArray( pParam,
                         ISLOG( 12 ) ? hb_parl( 12 ) : TRUE,
                         pFileToZip,
                         pFileAttribs,
                         hb_param( 11, HB_IT_STRING | HB_IT_ARRAY ) );

         if( hb_arrayLen( pFileToZip ) > 0 )
         {
            bRet = hb_CmpTdSpan( szZipFileName,
                                 pFileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : -1,
                                 hb_param( 4, HB_IT_BLOCK ),
                                 hb_parl( 5 ),
                                 hb_parc( 6 ),
                                 hb_parni( 7 ),
                                 hb_parl( 8 ),
                                 hb_parl( 9 ),
                                 hb_param( 10, HB_IT_BLOCK ) );

            ResetAttribs( pFileToZip, pFileAttribs );
         }

         hb_itemRelease( pFileAttribs );
         hb_itemRelease( pFileToZip );
         hb_xfree( szZipFileName );
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
 *      <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *      This function creates a zip file named <cFile>. If the extension
 *      is omitted, .zip will be assumed. If the second parameter is a
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
 *      IF HB_ZIPFILEBYPKSPAN( "A:\test.zip", "test.prg" )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYPKSPAN( "A:\test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_ZIPFILEBYPKSPAN( "test2.zip", { "test.prg", "C:\windows\win.ini"}, 9, {|nPos,cFile| qout(cFile) } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      aFiles := { "test.prg", "C:\windows\win.ini" }
 *      nLen   := Len( aFiles )
 *      aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "²" )
 *      GaugeDisplay( aGauge )
 *      HB_ZIPFILEBYPKSPAN( "F:\test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen ) },, "hello" )
 *      // assuming F:\ is a Zip Drive
 *      Return NIL
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
         char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );
         PHB_ITEM pFileToZip = hb_itemArrayNew( 0 );
         PHB_ITEM pFileAttribs = hb_itemArrayNew( 0 );

         ZipCreateArray( pParam,
                         ISLOG( 11 ) ? hb_parl( 11 ) : TRUE,
                         pFileToZip,
                         pFileAttribs,
                         hb_param( 10, HB_IT_STRING | HB_IT_ARRAY ) );

         if( hb_arrayLen( pFileToZip ) > 0 )
         {
            bRet = hb_CmpPkSpan( szZipFileName,
                                 pFileToZip,
                                 ISNUM( 3 ) ? hb_parni( 3 ) : -1,
                                 hb_param( 4, HB_IT_BLOCK ),
                                 hb_parl( 5 ),
                                 hb_parc( 6 ),
                                 hb_parl( 7 ),
                                 hb_parl( 8 ),
                                 hb_param( 9, HB_IT_BLOCK ) );

            ResetAttribs( pFileToZip, pFileAttribs );
         }

         hb_itemRelease( pFileAttribs );
         hb_itemRelease( pFileToZip );
         hb_xfree( szZipFileName );
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
 *      <lCompress>  .T. if all file was successfully restored, otherwise .F.
 * $DESCRIPTION$
 *      This function restores all files contained inside the <cFile>.
 *      If the extension is omitted, .zip will be assumed. If a file already
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
 *      aExtract := hb_GetFilesInZip( "test.zip" )  // extract all files in zip
 *      IF HB_UNZIPFILE( "test.zip",,,, ".\", aExtract )
 *         qout("File was successfully extracted")
 *      ENDIF
 *
 *      aExtract := hb_GetFilesInZip( "test2.zip" )  // extract all files in zip
 *      IF HB_UNZIPFILE( "test2.zip", {|cFile| qout( cFile ) },,, ".\", aExtract )
 *         qout("File was successfully extracted")
 *      ENDIF
 *      Return NIL
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
      char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );
      PHB_ITEM pUnzip = hb_param( 6, HB_IT_ANY );
      BYTE * pCurDir = ( BYTE * ) hb_xstrcpy( NULL, HB_OS_PATH_DELIM_CHR_STRING, ( const char * ) hb_fsCurDir( 0 ), NULL );
      PHB_ITEM pUnzipFiles = hb_itemArrayNew( 0 );

      if( hb_TestForPKS( szZipFileName ) <= 0 )
      {
         PHB_ITEM pZipArray = hb___GetFileNamesFromZip( szZipFileName, TRUE );

         if( pUnzip )
         {
            if( HB_IS_STRING( pUnzip ) )
            {
               UnzipCreateArray( hb_itemGetCPtr( pUnzip ), pUnzipFiles, pZipArray );
            }
            else if( HB_IS_ARRAY( pUnzip ) )
            {
               int uiZ, uiZLen = hb_arrayLen( pUnzip );
        
               for( uiZ = 0; uiZ < uiZLen; uiZ++ )
                  UnzipCreateArray( hb_arrayGetCPtr( pUnzip, uiZ + 1 ), pUnzipFiles, pZipArray );
            }
         }
         else
            UnzipCreateArray( "*", pUnzipFiles, pZipArray );
        
         if( hb_arrayLen( pUnzipFiles ) > 0 )
         {
            bRet = hb_UnzipSel( szZipFileName,
                                hb_param( 2, HB_IT_BLOCK ),
                                hb_parl( 3 ),
                                hb_parc( 4 ),
                                ISCHAR( 5 ) ? hb_parc( 5 ) : ".\\",
                                pUnzipFiles,
                                hb_param( 7 , HB_IT_BLOCK ) );
         }
      
         hb_itemRelease( pZipArray );
      }
      
      hb_itemRelease( pUnzipFiles );
      hb_fsChDir( pCurDir );
      hb_xfree( pCurDir );
      hb_xfree( szZipFileName );
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
 *     it will return .F. in the following cases: Spanned Archives; the file(s)
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
      PHB_ITEM pDelZipFiles = hb_itemArrayNew( 0 );

      if( pDelZip )
      {
         char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );
         PHB_ITEM pZipArray = hb___GetFileNamesFromZip( szZipFileName, TRUE );
         int ulLen = hb_arrayLen( pZipArray );
         
         if( ulLen )
         {
            if( HB_IS_STRING( pDelZip ) )
            {
               if( hb_itemGetCLen( pDelZip ) > 0 )
                  UnzipCreateArray( hb_itemGetCPtr( pDelZip ), pDelZipFiles, pZipArray );
            }
            else if( HB_IS_ARRAY( pDelZip ) )
            {
               int uiIn, uiInLen = hb_arrayLen(pDelZip);
            
               for( uiIn = 0; uiIn < uiInLen; uiIn++ )
                  UnzipCreateArray( hb_arrayGetCPtr( pDelZip, uiIn + 1 ), pDelZipFiles, pZipArray );
            }
            else if( HB_IS_NUMERIC( pDelZip ) )
            {
               int iIndex = hb_itemGetNI( pDelZip );
            
               if( iIndex > 0 && iIndex <= ulLen )
               {
                  PHB_ITEM pZipEntry = hb_arrayGetItemPtr( pZipArray, iIndex );
                  PHB_ITEM Temp = hb_itemNew( NULL );
                  hb_arrayAddForward( pDelZipFiles, hb_itemPutC( Temp, hb_arrayGetCPtr( pZipEntry, 1 ) ) );
                  hb_itemRelease( Temp );
               }
            }
            
            if( hb_arrayLen( pDelZipFiles ) > 0 )
            {
               bRet = hb_DeleteSel( szZipFileName,
                                    pDelZipFiles,
                                    hb_parl( 3 ) );
            }
         }

         hb_itemRelease( pZipArray );
         hb_xfree( szZipFileName );
      }

      hb_itemRelease( pDelZipFiles );
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
 *      if HB_ZIPTESTPK( "A:\test22.zip" ) == 114
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
   char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );

   hb_retni( hb_TestForPKS( szZipFileName ) );

   hb_xfree( szZipFileName );
}

/* $DOC$
 * $FUNCNAME$
 *     HB_SETBUFFER()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *
 * $SYNTAX$
 *     HB_SETBUFFER( [<nWriteBuffer>], [<nExtractBuffer>], [<nReadBuffer>] ) --> NIL
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
 *     HB_SETZIPCOMMENT( <cComment> ) --> NIL
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
 *      <lCompress>  .T. if all file was successfully restored, otherwise .F.
 * $DESCRIPTION$
 *      This function restores all files contained inside the <cFile>.
 *      If the extension is omitted, .zip will be assumed. If a file already
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
 *      IF HB_UNZIPFILEINDEX( "test.zip",,,, ".\", 1 )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      IF HB_UNZIPFILEINDEX( "test2.zip", {|cFile| qout(cFile) },,, ".\", { 1, 2 } )
 *         qout( "File was successfully created" )
 *      ENDIF
 *
 *      Return NIL
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
         char * szZipFileName = hb___CheckFile( hb_parc( 1 ) );
         PHB_ITEM pZipArray = hb___GetFileNamesFromZip( szZipFileName, TRUE );
         int ulLen = hb_arrayLen( pZipArray );
         PHB_ITEM DelZip = hb_itemArrayNew( 0 );

         if( HB_IS_NUMERIC( pDelZip ) )
         {
            int iIndex = hb_itemGetNI( pDelZip );

            if( iIndex > 0 && iIndex <= ulLen )
            {
               PHB_ITEM Temp = hb_itemNew( NULL );
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
                  PHB_ITEM Temp = hb_itemNew( NULL );
                  hb_arrayAddForward( DelZip, hb_itemPutNI( Temp, iIndex ) );
                  hb_itemRelease( Temp );
               }
            }
         }

         if( hb_arrayLen( DelZip ) > 0 )
         {
            bRet = hb_UnzipSelIndex( szZipFileName,
                                     hb_param( 2, HB_IT_BLOCK ),
                                     hb_parl( 3 ),
                                     hb_parc( 4 ),
                                     hb_parc( 5 ),
                                     DelZip,
                                     hb_param( 7, HB_IT_BLOCK ) );
         }

         hb_itemRelease( DelZip );
         hb_itemRelease( pZipArray );
         hb_xfree( szZipFileName );
      }
   }

   hb_retl( bRet );
}

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

HB_FUNC( HB_UNZIPALLFILE )
{
   if( ISCHAR( 1 ) && ! ISCHAR( 6 ) && ! ISARRAY( 6 ) )
   {
      char * szZipFile = hb___CheckFile( hb_parc( 1 ) );

      hb_retl( hb_UnzipAll( szZipFile,
                            hb_param( 2, HB_IT_BLOCK ),
                            hb_parl( 3 ),
                            hb_parc( 4 ),
                            hb_parc( 5 ),
                            hb_param( 6, HB_IT_BLOCK ),
                            hb_param( 7, HB_IT_BLOCK ) ) );
      hb_xfree( szZipFile );
   }
   else
      hb_retl( FALSE );
}
