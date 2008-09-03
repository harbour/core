/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ZipArchive high level interface for Harbour
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#include "directry.ch"

HB_EXTERN_BEGIN

extern void     hb_fsDirectory( PHB_ITEM pDir, char * szSkleton, char * szAttributes, BOOL bDirOnly, BOOL bFullPath );

extern char *   hb_FNAddZipExt( const char * szFile );
extern PHB_ITEM hb_GetFileNamesFromZip( char * szFile, BOOL bVerbose );
extern ULONG    hb_GetNumberofFilestoUnzip( char * szFile );
extern BOOL     hb_TransferFilesFromzip( char * szSource, char * szDest, PHB_ITEM pArray );
extern BOOL     hb_SetCallbackFunc( PHB_ITEM pFunc );
extern BOOL     hb_CompressFile( char * szFile, PHB_ITEM pArray, int iCompLevel, PHB_ITEM pBlock, BOOL bOverwrite, char * szPassword, long iSpanSize, BOOL bPath, BOOL bDrive, PHB_ITEM pProgress, BOOL bSpan );
extern BOOL     hb_Unzip( char * szFile, PHB_ITEM pBlock, BOOL bWithPath, char * szPassword, const char * szPath, PHB_ITEM pArray, PHB_ITEM pProgress );
extern BOOL     hb_DeleteSel( char * szFile, PHB_ITEM pArray );
extern int      hb_CheckSpanMode( char * szFile );
extern void     hb_SetZipBuff( int a, int b, int c );
extern void     hb_SetZipComment( char * szComment );
extern char *   hb_GetZipComment( char * szFile );
extern BOOL     hb_IsPassWord( char * szName );
extern void     hb_SetZipReadOnly( BOOL bRead );

HB_EXTERN_END

static BOOL ZipTestExclude( char * szEntry, PHB_ITEM pExcludeFiles )
{
   ULONG nPos, nLen = hb_arrayLen( pExcludeFiles );

   for( nPos = 0; nPos < nLen; nPos++ )
   {
      if( hb_strMatchFile( hb_arrayGetCPtr( pExcludeFiles, nPos + 1 ), szEntry ) )
         return FALSE;
   }

   return TRUE;
}

static PHB_ITEM ZipCreateExclude( PHB_ITEM pExcludeParam )
{
   PHB_ITEM pExcludeFiles = hb_itemArrayNew( 0 );

   if( pExcludeParam )
   {
      if( HB_IS_STRING( pExcludeParam ) )
      {
         if( strchr( hb_itemGetCPtr( pExcludeParam ), '*' ) ||
             strchr( hb_itemGetCPtr( pExcludeParam ), '?' ) )
         {
            PHB_ITEM pDirFiles;
            ULONG nPos, nLen;

            pDirFiles = hb_itemNew( NULL );

            hb_fsDirectory( pDirFiles, hb_itemGetCPtr( pExcludeParam ), NULL, 0, TRUE );
            nLen = hb_arrayLen( pDirFiles );

            for( nPos = 0; nPos < nLen; nPos++ )
            {
               PHB_ITEM pTemp = hb_itemPutC( NULL, hb_arrayGetCPtr( hb_arrayGetItemPtr( pDirFiles, nPos + 1 ), F_NAME ) );
               hb_arrayAddForward( pExcludeFiles, pTemp );
               hb_itemRelease( pTemp );
            }

            hb_itemRelease( pDirFiles );
         }
         else if( hb_itemGetCLen( pExcludeParam ) )
         {
            PHB_ITEM pTemp = hb_itemPutC( NULL, hb_itemGetCPtr( pExcludeParam ) );
            hb_arrayAddForward( pExcludeFiles, pTemp );
            hb_itemRelease( pTemp );
         }
      }
      else if( HB_IS_ARRAY( pExcludeParam ) )
      {
         ULONG nPos, nLen = hb_arrayLen( pExcludeParam );

         for( nPos = 0; nPos < nLen; nPos++ )
         {
            char * szExclude = hb_arrayGetCPtr( pExcludeParam, nPos + 1 );

            if( strchr( szExclude, '*' ) ||
                strchr( szExclude, '?' ) )
            {
               PHB_ITEM pDirFiles = hb_itemNew( NULL );
               ULONG nDirPos, nDirLen;

               hb_fsDirectory( pDirFiles, szExclude, NULL, 0, TRUE );
               nDirLen = hb_arrayLen( pDirFiles );

               for( nDirPos = 0; nDirPos < nDirLen; nDirPos++ )
               {
                  PHB_ITEM pTemp = hb_itemPutC( NULL, hb_arrayGetCPtr( hb_arrayGetItemPtr( pDirFiles, nDirPos + 1 ), F_NAME ) );
                  hb_arrayAddForward( pExcludeFiles, pTemp );
                  hb_itemRelease( pTemp );
               }

               hb_itemRelease( pDirFiles );
            }
            else
            {
               PHB_ITEM pTemp = hb_itemPutC( NULL, szExclude );
               hb_arrayAddForward( pExcludeFiles, pTemp );
               hb_itemRelease( pTemp );
            }
         }
      }
   }

   return pExcludeFiles;
}

static PHB_ITEM ZipCreateArray( PHB_ITEM pParamFiles, BOOL bFullPath, PHB_ITEM pExcludeParam )
{
   PHB_ITEM pProcFiles = hb_itemArrayNew( 0 );

   if( pParamFiles )
   {
      PHB_ITEM pExcludeFiles = ZipCreateExclude( pExcludeParam );
      PHB_ITEM pParamArray;
      PHB_ITEM pDirFiles = hb_itemNew( NULL );

      ULONG nArrayPos, nArrayLen;

      if( HB_IS_STRING( pParamFiles ) )
      {
         PHB_ITEM pTemp;

         pParamArray = hb_itemArrayNew( 0 );

         pTemp = hb_itemPutC( NULL, hb_itemGetCPtr( pParamFiles ) );
         hb_arrayAddForward( pParamArray, pTemp );
         hb_itemRelease( pTemp );
      }
      else
         pParamArray = hb_arrayClone( pParamFiles );

      nArrayLen = hb_arrayLen( pParamArray );

      for( nArrayPos = 0; nArrayPos < nArrayLen; nArrayPos++ )
      {
         char * szArrEntry = hb_arrayGetCPtr( pParamArray, nArrayPos + 1 );

         if( strchr( szArrEntry, '*' ) ||
             strchr( szArrEntry, '?' ) )
         {
            ULONG nPos, nLen;

            hb_fsDirectory( pDirFiles, szArrEntry, NULL, 0, bFullPath );
            nLen = hb_arrayLen( pDirFiles );

            for( nPos = 0; nPos < nLen; nPos++ )
            {
               char * pszEntry = hb_arrayGetCPtr( hb_arrayGetItemPtr( pDirFiles, nPos + 1 ), F_NAME );

               if( ZipTestExclude( pszEntry, pExcludeFiles ) )
               {
                  PHB_ITEM pTemp = hb_itemPutC( NULL, pszEntry );
                  hb_arrayAddForward( pProcFiles, pTemp );
                  hb_itemRelease( pTemp );
               }
            }

            hb_itemClear( pDirFiles );
         }
         else
         {
            PHB_ITEM pTemp = hb_itemPutC( NULL, szArrEntry );
            hb_arrayAddForward( pProcFiles, pTemp );
            hb_itemRelease( pTemp );
         }
      }

      hb_itemRelease( pParamArray );
      hb_itemRelease( pDirFiles );
      hb_itemRelease( pExcludeFiles );
   }

   return pProcFiles;
}

/*
 * $DOC$
 * $FUNCNAME$
 *     HB_ZIPFILE()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Create a zip file
 * $SYNTAX$
 *     HB_ZIPFILE( <cFile>, <cFileToCompress> | <aFiles>, <nLevel>,
 *     <bBlock>, <lOverWrite>, <cPassword>, <lWithPath>, <lWithDrive>,
 *     <pFileProgress> ) ---> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file to create
 *
 *     <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *     can be used
 *        _or_
 *     <aFiles>  An array containing files to compress, Drive and/or path
 *     can be used
 *
 *     <nLevel>  Compression level ranging from 0 to 9
 *
 *     <bBlock>  Code block to execute while compressing
 *
 *     <lOverWrite>  Toggle to overwrite the file if exists
 *
 *     <cPassword> Password to encrypt the files
 *
 *     <lWithPath> Toggle to store the path or not
 *
 *     <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *     This function creates a zip file named <cFile>. If the extension
 *     is omitted, .zip will be assumed. If the second parameter is a
 *     character string, this file will be added to the zip file. If the
 *     second parameter is an array, all file names contained in <aFiles>
 *     will be compressed.
 *
 *     If <nLevel> is used, it determines the compression type where 0 means
 *     no compression and 9 means best compression.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     If <lOverWrite> is used, it toggles to overwrite or not the existing
 *     file. Default is to overwrite the file,otherwise if <lOverWrite> is false
 *     the new files are added to the <cFile>.
 *
 *     If <cPassword> is used, all files that are added to the archive are encrypted
 *     with the password.
 *
 *     If <lWithPath> is used, it tells  the path should also be stored with
 *     the file name. Default is false.
 *
 *     If <lWithDrive> is used, it tells thats the Drive and path should also be stored
 *     with the file name. Default is false.
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 *
 * $EXAMPLES$
 *     FUNCTION MAIN()
 *
 *     IF HB_ZIPFILE( "test.zip", "test.prg" )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     IF HB_ZIPFILE( "test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     IF HB_ZIPFILE( "test2.zip", { "test.prg", "C:\windows\win.ini" }, 9, {|cFile,nPos,| qout(cFile) } )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     aFiles := { "test.prg", "C:\windows\win.ini" }
 *     nLen   := Len( aFiles )
 *     aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B" , "." )
 *     GaugeDisplay( aGauge )
 *     HB_ZIPFILE( "test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen ) },, "hello" )
 *     Return NIL
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPFILE )
{
   BOOL bReturn = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pProcFiles = ZipCreateArray( hb_param( 2, HB_IT_STRING | HB_IT_ARRAY ),
                                            ISLOG( 11 ) ? hb_parl( 11 ) : TRUE,
                                            hb_param( 10, HB_IT_STRING | HB_IT_ARRAY ) );

      if( hb_arrayLen( pProcFiles ) )
      {
         bReturn = hb_CompressFile( hb_parc( 1 ),
                                    pProcFiles,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : -1,
                                    hb_param( 4, HB_IT_BLOCK ),
                                    hb_parl( 5 ),
                                    hb_parc( 6 ),
                                    0,
                                    hb_parl( 7 ),
                                    hb_parl( 8 ),
                                    hb_param( 9, HB_IT_BLOCK ), 
                                    FALSE );
      }

      hb_itemRelease( pProcFiles );
   }

   hb_retl( bReturn );
}

/*
 * $DOC$
 * $FUNCNAME$
 *     HB_ZIPFILEBYPKSPAN()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Create a zip file on removable media
 * $SYNTAX$
 *     HB_ZIPFILEBYPKSPAN( <cFile>, <cFileToCompress> | <aFiles>, <nLevel>,
 *     <bBlock>, <lOverWrite>, <cPassword>, <lWithPath>, <lWithDrive>,
 *     <pFileProgress>) ---> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file
 *
 *     <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *     can be used
 *         _or_
 *     <aFiles>  An array containing files to compress, Drive and/or path
 *     can be used
 *
 *     <nLevel>  Compression level ranging from 0 to 9
 *
 *     <bBlock>  Code block to execute while compressing
 *
 *     <lOverWrite>  Toggle to overwrite the file if exists
 *
 *     <cPassword> Password to encrypt the files
 *
 *     <lWithPath> Toggle to store the path or not
 *
 *     <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *     This function creates a zip file named <cFile>. If the extension
 *     is omitted, .zip will be assumed. If the second parameter is a
 *     character string, this file will be added to the zip file. If the
 *     second parameter is an array, all file names contained in <aFiles>
 *     will be compressed.  Also, the use of this function is for creating
 *     backup in removable media like an floppy drive/zip drive.
 *
 *     If <nLevel> is used, it determines the compression type where 0 means
 *     no compression and 9 means best compression.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     If <lOverWrite> is used , it toggles to overwrite or not the existing
 *     file. Default is to overwrite the file, otherwise if <lOverWrite> is false
 *     the new files are added to the <cFile>.
 *
 *     If <cPassword> is used, all files that are added to the archive are encrypted
 *     with the password.
 *
 *     If <lWithPath> is used, it tells thats the path should also be stored with
 *     the file name. Default is false.
 *
 *     If <lWithDrive> is used, it tells thats the Drive and path should also be stored
 *     with the file name. Default is false.
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 *
 *     Before calling this function, Set an Changedisk codeblock by calling
 *     the HB_SETDISKZIP().
 * $EXAMPLES$
 *     FUNCTION MAIN()
 *
 *     hb_setdiskzip( {|nDisk| Alert( "Please insert disk no " + Str( nDisk, 3 ) ) } )
 *
 *     IF HB_ZIPFILEBYPKSPAN( "A:\test.zip", "test.prg" )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     IF HB_ZIPFILEBYPKSPAN( "A:\test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     IF HB_ZIPFILEBYPKSPAN( "test2.zip", { "test.prg", "C:\windows\win.ini"}, 9, {|nPos,cFile| qout(cFile) } )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     aFiles := { "test.prg", "C:\windows\win.ini" }
 *     nLen   := Len( aFiles )
 *     aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "." )
 *     GaugeDisplay( aGauge )
 *     HB_ZIPFILEBYPKSPAN( "F:\test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen ) },, "hello" )
 *     // assuming F:\ is a Zip Drive
 *     Return NIL
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPFILEBYPKSPAN )
{
   BOOL bReturn = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pProcFiles = ZipCreateArray( hb_param( 2, HB_IT_STRING | HB_IT_ARRAY ),
                                            ISLOG( 11 ) ? hb_parl( 11 ) : TRUE,
                                            hb_param( 10, HB_IT_STRING | HB_IT_ARRAY ) );

      if( hb_arrayLen( pProcFiles ) )
      {
         bReturn = hb_CompressFile( hb_parc( 1 ),
                                    pProcFiles,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : -1,
                                    hb_param( 4, HB_IT_BLOCK ),
                                    hb_parl( 5 ),
                                    hb_parc( 6 ),
                                    0,
                                    hb_parl( 7 ),
                                    hb_parl( 8 ),
                                    hb_param( 9, HB_IT_BLOCK ),
                                    TRUE );
      }

      hb_itemRelease( pProcFiles );
   }

   hb_retl( bReturn );
}

/*
 * $DOC$
 * $FUNCNAME$
 *     HB_ZIPFILEBYTDSPAN()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Create a zip file
 * $SYNTAX$
 *     HB_ZIPFILEBYTDSPAN( <cFile> ,<cFileToCompress> | <aFiles>, <nLevel>,
 *     <bBlock>, <lOverWrite>, <cPassword>, <iSize>, <lWithPath>, <lWithDrive>,
 *     <pFileProgress>) ---> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file
 *
 *     <cFileToCompress>  Name of a file to Compress, Drive and/or path
 *     can be used
 *         _or_
 *     <aFiles>  An array containing files to compress, Drive and/or path
 *     can be used
 *
 *     <nLevel>  Compression level ranging from 0 to 9
 *
 *     <bBlock>  Code block to execute while compressing
 *
 *     <lOverWrite>  Toggle to overwrite the file if exists
 *
 *     <cPassword> Password to encrypt the files
 *
 *     <iSize> Size of the archive, in bytes. Default is 1457664 bytes
 *
 *     <lWithPath> Toggle to store the path or not
 *
 *     <lWithDrive> Toggle to store the Drive letter and path or not
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if file was create, otherwise .F.
 * $DESCRIPTION$
 *     This function creates a zip file named <cFile>. If the extension
 *     is omitted, .zip will be assumed. If the second parameter is a
 *     character string, this file will be added to the zip file. If the
 *     second parameter is an array, all file names contained in <aFiles>
 *     will be compressed.
 *
 *     If <nLevel> is used, it determines the compression type where 0 means
 *     no compression and 9 means best compression.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     If <lOverWrite> is used, it toggles to overwrite or not the existing
 *     file. Default is to overwrite the file, otherwise if <lOverWrite> is
 *     false the new files are added to the <cFile>.
 *
 *     If <lWithPath> is used, it tells thats the path should also be stored '
 *     with the file name. Default is false.
 *
 *     If <lWithDrive> is used, it tells thats the Drive and path should also
 *     be stored with the file name. Default is false.
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 * $EXAMPLES$
 *     FUNCTION MAIN()
 *
 *     IF HB_ZIPFILEBYTDSPAN( "test.zip", "test.prg" )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     IF HB_ZIPFILEBYTDSPAN( "test1.zip", { "test.prg", "C:\windows\win.ini" } )
 *        qout( "File was successfully created" )
 *     ENDIF
 *
 *     IF HB_ZIPFILEBYTDSPAN( "test2.zip", { "test.prg", "C:\windows\win.ini" }, 9, {|nPos,cFile| qout(cFile) }, "hello",, 521421 )
 *        qout("File was successfully created" )
 *     ENDIF
 *
 *     aFiles := { "test.prg", "C:\windows\win.ini" }
 *     nLen   := Len( aFiles )
 *     aGauge := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "." )
 *     GaugeDisplay( aGauge )
 *     HB_ZIPFILEBYTDSPAN( "test33.zip", aFiles, 9, {|cFile,nPos| GaugeUpdate( aGauge, nPos/nLen) },, "hello",, 6585452 )
 *     Return NIL
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPFILEBYTDSPAN )
{
   BOOL bReturn = FALSE;

   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pProcFiles = ZipCreateArray( hb_param( 2, HB_IT_STRING | HB_IT_ARRAY ),
                                            ISLOG( 12 ) ? hb_parl( 12 ) : TRUE,
                                            hb_param( 11, HB_IT_STRING | HB_IT_ARRAY ) );

      if( hb_arrayLen( pProcFiles ) )
      {
         bReturn = hb_CompressFile( hb_parc( 1 ),
                                    pProcFiles,
                                    ISNUM( 3 ) ? hb_parni( 3 ) : -1,
                                    hb_param( 4, HB_IT_BLOCK ),
                                    hb_parl( 5 ),
                                    hb_parc( 6 ),
                                    ISNUM( 7 ) ? hb_parnl( 7 ) : 1457664,
                                    hb_parl( 8 ),
                                    hb_parl( 9 ),
                                    hb_param( 10, HB_IT_BLOCK ),
                                    TRUE );
      }

      hb_itemRelease( pProcFiles );
   }

   hb_retl( bReturn );
}

/*
 * $DOC$
 * $FUNCNAME$
 *     HB_UNZIPFILE()
 * $CATEGORY$
 *     Zip Functions
 * $ONELINER$
 *     Unzip a compressed file
 * $SYNTAX$
 *     HB_UNZIPFILE( <cFile>, <bBlock>, <lWithPath>, <cPassWord>, <cPath>,
 *                   <cFile> | <aFile>, <pFileProgress> ) ---> lCompress
 * $ARGUMENTS$
 *     <cFile>   Name of the zip file to extract
 *
 *     <bBlock>  Code block to execute while extracting
 *
 *     <lWithPath> Toggle to create directory if needed
 *
 *     <cPassWord> Password to use to extract files
 *
 *     <cPath>    Path to extract the files to - mandatory
 *
 *     <cFile> | <aFiles> A File or Array of files to extract - mandatory
 *
 *     <pFileProgress> Code block for File Progress
 * $RETURNS$
 *     <lCompress>  .T. if all file was successfully restored, otherwise .F.
 * $DESCRIPTION$
 *     This function restores all files contained inside the <cFile>.
 *     If the extension is omitted, .zip will be assumed. If a file already
 *     exists, it will be overwritten.
 *
 *     If <bBlock> is used, every time the file is opened to compress it
 *     will evaluate bBlock. Parameters of bBlock are cFile and nPos.
 *
 *     The <cPath> is a mandatory parameter. Set to ".\" to extract to the
 *     current directory
 *
 *     If <cFile> or <aFiles> are not provided, no files will be extracted!
 *     Make sure you provide the file or files you want extracted
 *
 *     If <pFileProgress> is used, an Code block is evaluated, showing the total
 *     of that file has being processed.
 *     The codeblock must be defined as follow {|nPos,nTotal| GaugeUpdate(aGauge1,(nPos/nTotal))}
 * $EXAMPLES$
 *     FUNCTION MAIN()
 *
 *     aExtract := hb_GetFilesInZip( "test.zip" )  // extract all files in zip
 *     IF HB_UNZIPFILE( "test.zip",,,, ".\", aExtract )
 *        qout("File was successfully extracted")
 *     ENDIF
 *
 *     aExtract := hb_GetFilesInZip( "test2.zip" )  // extract all files in zip
 *     IF HB_UNZIPFILE( "test2.zip", {|cFile| qout( cFile ) },,, ".\", aExtract )
 *        qout("File was successfully extracted")
 *     ENDIF
 *     Return NIL
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

static void UnzipAddFileMask( const char * pszMask, PHB_ITEM pProcFiles, PHB_ITEM pZipFiles )
{
   ULONG nPos, nLen = hb_arrayLen( pZipFiles );

   for( nPos = 0; nPos < nLen; nPos++ )
   {
      const char * szEntry = hb_arrayGetCPtr( pZipFiles, nPos + 1 );
      BOOL bAdd = TRUE;

      if( pszMask )
         bAdd = hb_strMatchFile( szEntry, ( const char * ) pszMask );

      if( ! bAdd )
      {
         PHB_FNAME pFileName = hb_fsFNameSplit( szEntry );

         if( pFileName->szName )
         {
            char * pszFile = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
            pFileName->szPath = "";
            hb_fsFNameMerge( pszFile, pFileName );
            bAdd = hb_strMatchFile( pszMask, pszFile );
            hb_xfree( pszFile );

            if( ! bAdd )
               bAdd = hb_strMatchFile( pszMask, szEntry );
         }

         hb_xfree( pFileName );
      }

      if( bAdd )
      {
         PHB_ITEM pTemp = hb_itemPutC( NULL, szEntry );
         hb_arrayAddForward( pProcFiles, pTemp );
         hb_itemRelease( pTemp );
      }
   }
}

static void hb_procexistingzip( BOOL bUnzip )
{
   BOOL bReturn = FALSE;

   if( ISCHAR( 1 ) )
   {
      char * pszZipFileName = hb_FNAddZipExt( hb_parc( 1 ) );
      PHB_ITEM pParamFiles = hb_param( bUnzip ? 6 : 2, HB_IT_STRING | HB_IT_NUMERIC | HB_IT_ARRAY );
      PHB_ITEM pProcFiles = hb_itemArrayNew( 0 );

      if( pParamFiles )
      {
         PHB_ITEM pZipFiles = hb_GetFileNamesFromZip( pszZipFileName, FALSE );
         ULONG nZipLen = hb_arrayLen( pZipFiles );

         if( HB_IS_ARRAY( pParamFiles ) )
         {
            ULONG nPos, nLen = hb_arrayLen( pParamFiles );
         
            for( nPos = 0; nPos < nLen; nPos++ )
            {
               HB_TYPE type = hb_arrayGetType( pParamFiles, nPos + 1 );
         
               if( type & HB_IT_NUMERIC )
               {
                  ULONG nZipPos = hb_arrayGetNL( pParamFiles, nPos + 1 );
         
                  if( nZipPos > 0 && nZipPos <= nZipLen )
                  {
                     PHB_ITEM pTemp = hb_itemPutC( NULL, hb_arrayGetCPtr( pZipFiles, nZipPos ) );
                     hb_arrayAddForward( pProcFiles, pTemp );
                     hb_itemRelease( pTemp );
                  }
               }
               else if( type & HB_IT_STRING )
                  UnzipAddFileMask( hb_arrayGetCPtr( pParamFiles, nPos + 1 ), pProcFiles, pZipFiles );
            }
         }
         else if( HB_IS_NUMERIC( pParamFiles ) )
         {
            ULONG nZipPos = hb_itemGetNL( pParamFiles );
         
            if( nZipPos > 0 && nZipPos <= nZipLen )
            {
               PHB_ITEM pTemp = hb_itemPutC( NULL, hb_arrayGetCPtr( pZipFiles, nZipPos ) );
               hb_arrayAddForward( pProcFiles, pTemp );
               hb_itemRelease( pTemp );
            }
         }
         else
            UnzipAddFileMask( hb_itemGetCPtr( pParamFiles ), pProcFiles, pZipFiles );

         hb_itemRelease( pZipFiles );
      }

      if( bUnzip )
      {
         if( ! pParamFiles || hb_arrayLen( pProcFiles ) )
         {
            bReturn = hb_Unzip( pszZipFileName,
                                hb_param( 2, HB_IT_BLOCK ),
                                hb_parl( 3 ),
                                hb_parc( 4 ),
                                hb_parc( 5 ),
                                pParamFiles ? pProcFiles : NULL,
                                hb_param( 7, HB_IT_BLOCK ) );
         }
      }
      else
      {
         if( hb_arrayLen( pProcFiles ) )
            bReturn = hb_DeleteSel( pszZipFileName,
                                    pProcFiles );
      }

      hb_itemRelease( pProcFiles );
      hb_xfree( pszZipFileName );
   }

   hb_retl( bReturn );
}

HB_FUNC( HB_UNZIPFILE )
{
   hb_procexistingzip( TRUE );
}

HB_FUNC( HB_UNZIPFILEINDEX )
{
   HB_FUNC_EXEC( HB_UNZIPFILE );
}

HB_FUNC( HB_UNZIPALLFILE )
{
   HB_FUNC_EXEC( HB_UNZIPFILE );
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
 *     ? "has the file zipnew.i been deleted ", iif( HB_ZIPDELETEFILES( "\test23.zip", "zipnew.i" ), "Yes", "No" )
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPDELETEFILES )
{
   hb_procexistingzip( FALSE );
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
 *     HB_SETDISKZIP( {|nDisk| Alert( "Please insert disk no " + Str( nDisk, 3 ) ) } )
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_SETDISKZIP )
{
   hb_retl( hb_SetCallbackFunc( hb_param( 1, HB_IT_BLOCK ) ) );
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
 *     if HB_ZIPTESTPK( "A:\test22.zip" ) == 114
 *        ? "Invalid Diskette"
 *     endif
 * $STATUS$
 *     R
 * $COMPLIANCE$
 *     This function is a Harbour extension
 * $PLATFORMS$
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_ZIPTESTPK )
{
   char * pszZipFileName = hb_FNAddZipExt( hb_parc( 1 ) );

   hb_retni( hb_CheckSpanMode( pszZipFileName ) );

   hb_xfree( pszZipFileName );
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
 *     All
 * $FILES$
 *     Library is hbziparch.lib
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
 *     All
 * $FILES$
 *     Library is hbziparch.lib
 * $END$
 */

HB_FUNC( HB_GETZIPCOMMENT )
{
   hb_retc_buffer( hb_GetZipComment( hb_parc( 1 ) ) );
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

HB_FUNC( HB_ZIPWITHPASSWORD )
{
   hb_retl( hb_IsPassWord( hb_parc( 1 ) ) );
}

HB_FUNC( HB_GETFILECOUNT )
{
   ULONG nCount = 0;

   if( ISCHAR( 1 ) )
   {
      char * pszZipFileName = hb_FNAddZipExt( hb_parc( 1 ) );

      nCount = hb_GetNumberofFilestoUnzip( pszZipFileName );

      hb_xfree( pszZipFileName );
   }

   hb_retnl( ( long ) nCount );
}

HB_FUNC( HB_GETFILESINZIP )
{
   if( ISCHAR( 1 ) )
   {
      char * pszZipFileName = hb_FNAddZipExt( hb_parc( 1 ) );

      hb_itemReturnRelease( hb_GetFileNamesFromZip( pszZipFileName, hb_parl( 2 ) ) );

      hb_xfree( pszZipFileName );
   }
   else
      hb_reta( 0 );
}
