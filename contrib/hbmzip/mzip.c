/*
 * Harbour Project source code:
 *    Wrapper functions for minizip library
 *    Some higher level zip archive functions
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas.at.dbtopas.lt>
 * Copyright 2011-2013 Viktor Szakats (vszakats.net/harbour) (codepage/unicode)
 * www - http://harbour-project.org
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
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#if ! defined( _LARGEFILE64_SOURCE )
#  define _LARGEFILE64_SOURCE  1
#endif

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapistr.h"
#include "hbdate.h"
#include "hbset.h"
#include "zip.h"
#include "unzip.h"

#if defined( HB_OS_UNIX )
   #include <sys/types.h>
   #include <sys/stat.h>
   #include <unistd.h>
   #include <time.h>
   #include <utime.h>
#elif defined( HB_OS_DOS )
   #if defined( __DJGPP__ ) || defined( __RSX32__ ) || defined( __GNUC__ )
      #include "hb_io.h"
      #include <sys/param.h>
      #if defined( HB_OS_DOS )
         #include <time.h>
         #include <utime.h>
      #endif
   #endif
#elif defined( HB_OS_WIN )
   #include <windows.h>
   #if ! defined( INVALID_FILE_ATTRIBUTES )
      #define INVALID_FILE_ATTRIBUTES  ( ( DWORD ) -1 )
   #endif
   #include "hbwinuni.h"
#elif defined( HB_OS_OS2 )
   #define INCL_DOSFILEMGR
   #define INCL_ERRORS
   #include <os2.h>
#endif

#if ! defined( HB_USE_LARGEFILE64 ) && defined( HB_OS_UNIX )
   #if defined( __USE_LARGEFILE64 )
      /*
       * The macro: __USE_LARGEFILE64 is set when _LARGEFILE64_SOURCE is
       * define and efectively enables lseek64/flock64/ftruncate64 functions
       * on 32bit machines.
       */
      #define HB_USE_LARGEFILE64
   #elif defined( HB_OS_UNIX ) && defined( O_LARGEFILE )
      #define HB_USE_LARGEFILE64
   #endif
#endif

#define _ZIP_FLAG_UNICODE  ( 1 << 11 ) /* Language encoding flag (EFS) */

#if defined( HB_OS_UNIX )
   #define _VER_PLATFORM   0x03 /* it's necessary for file attributes in unzip */
#else
   #define _VER_PLATFORM   0x00
#endif

static int _version_made_by( HB_BOOL fUnicode )
{
   return ( fUnicode ? 0x3F /* 6.3.x */ : 0x14 /* 2.0.x */ ) | ( _VER_PLATFORM << 8 );
}

#define HB_Z_IOBUF_SIZE    ( 1024 * 16 )

static HB_GARBAGE_FUNC( hb_zipfile_destructor )
{
   zipFile * phZip = ( zipFile * ) Cargo;

   if( *phZip )
   {
      zipClose( *phZip, NULL );
      *phZip = NULL;
   }
}

static const HB_GC_FUNCS s_gcZipFileFuncs =
{
   hb_zipfile_destructor,
   hb_gcDummyMark
};


static zipFile hb_zipfileParam( int iParam )
{
   zipFile * phZip = ( zipFile * ) hb_parptrGC( &s_gcZipFileFuncs, iParam );

   if( phZip && *phZip )
      return *phZip;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}


static HB_GARBAGE_FUNC( hb_unzipfile_destructor )
{
   unzFile * phUnzip = ( unzFile * ) Cargo;

   if( *phUnzip )
   {
      unzClose( *phUnzip );
      *phUnzip = NULL;
   }
}

static const HB_GC_FUNCS s_gcUnZipFileFuncs =
{
   hb_unzipfile_destructor,
   hb_gcDummyMark
};


static unzFile hb_unzipfileParam( int iParam )
{
   unzFile * phUnzip = ( unzFile * ) hb_parptrGC( &s_gcUnZipFileFuncs, iParam );

   if( phUnzip && *phUnzip )
      return *phUnzip;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static PHB_FILE hb_fileHandleParam( int iParam, HB_BOOL * pfFree )
{
   PHB_FILE pFile = NULL;

   * pfFree = HB_FALSE;
   if( HB_ISNUM( iParam ) )
   {
      HB_FHANDLE hFile = hb_numToHandle( hb_parnint( iParam ) );
      if( hFile != FS_ERROR )
      {
         pFile = hb_fileFromHandle( hFile );
         * pfFree = HB_TRUE;
      }
   }
   else
      pFile = hb_fileParam( iParam );

   if( pFile != NULL )
      return pFile;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

static HB_FATTR hb_translateExtAttr( const char * szFileName, HB_FATTR ulExtAttr )
{
   int iLen;

   iLen = ( int ) strlen( szFileName );
   if( ( iLen > 4 && ( hb_stricmp( szFileName + iLen - 4, ".exe" ) == 0 ||
                       hb_stricmp( szFileName + iLen - 4, ".com" ) == 0 ||
                       hb_stricmp( szFileName + iLen - 4, ".bat" ) == 0 ||
                       hb_stricmp( szFileName + iLen - 4, ".cmd" ) == 0 ) ) ||
       ( iLen > 3 && hb_stricmp( szFileName + iLen - 3, ".sh" ) == 0 ) )
   {
      ulExtAttr |= 0x00490000; /* --x--x--x */
   }

   if( ulExtAttr & HB_FA_READONLY )
      ulExtAttr |= 0x01240000;  /* r--r--r-- */
   else
      ulExtAttr |= 0x01B60000;  /* rw-rw-rw- */

   if( ulExtAttr & HB_FA_DIRECTORY )
      ulExtAttr |= 0x40000000;
   else
      ulExtAttr |= 0x80000000;

   return ulExtAttr;
}


/* hb_zipOpen( cFileName, [ iMode = HB_ZIP_CREATE ], [ @cGlobalComment ] ) --> hZip */
HB_FUNC( HB_ZIPOPEN )
{
   const char * szFileName = hb_parc( 1 );

   if( szFileName )
   {
      zipcharpc pszGlobalComment = NULL;
      char *    pszFree;
      zipFile   hZip = zipOpen2( hb_fsNameConv( szFileName, &pszFree ), hb_parnidef( 2, APPEND_STATUS_CREATE ),
                                 &pszGlobalComment, NULL );

      if( pszFree )
         hb_xfree( pszFree );

      if( hZip )
      {
         zipFile * phZip = ( zipFile * ) hb_gcAllocate( sizeof( zipFile ), &s_gcZipFileFuncs );

         *phZip = hZip;
         hb_retptrGC( phZip );

         if( pszGlobalComment )
            hb_storc( ( const char * ) pszGlobalComment, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* hb_zipClose( hZip, [ cGlobalComment ] ) --> nError */
HB_FUNC( HB_ZIPCLOSE )
{
   zipFile * phZip = ( zipFile * ) hb_parptrGC( &s_gcZipFileFuncs, 1 );

   if( phZip && *phZip )
   {
      zipFile hZip = *phZip;

      *phZip = NULL;
      hb_retni( zipClose( hZip, hb_parc( 2 ) ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* hb_zipFileCreate( hZip, cZipName, dDate, cTime, nInternalAttr, nExternalAttr,
                     [ nMethod = HB_ZLIB_METHOD_DEFLATE ],
                     [ nLevel = HB_ZLIB_COMPRESSION_DEFAULT ],
                     [ cPassword, ulFileCRC32 ], [ cComment ], [ lUnicode ] ) --> nError */
HB_FUNC( HB_ZIPFILECREATE )
{
   const char * szZipName = hb_parc( 2 );

   if( szZipName )
   {
      zipFile hZip = hb_zipfileParam( 1 );

      if( hZip )
      {
         int   iMethod = hb_parnidef( 7, Z_DEFLATED );
         int   iLevel  = hb_parnidef( 8, Z_DEFAULT_COMPRESSION );
         long  lJulian, lMillisec;
         int   iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
         uLong flags = 0;

         HB_BOOL      fUnicode = hb_parl( 12 );
         void *       hZipName = NULL;
         void *       hComment = NULL;
         const char * szComment;

         zip_fileinfo zfi;

         memset( &zfi, 0, sizeof( zfi ) );

         if( HB_ISTIMESTAMP( 3 ) )
         {
            hb_partdt( &lJulian, &lMillisec, 3 );
            hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
            hb_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );
         }
         else
         {
            hb_dateDecode( hb_pardl( 3 ), &iYear, &iMonth, &iDay );
            hb_timeStrGet( hb_parc( 4 ), &iHour, &iMinute, &iSecond, &iMSec );
         }

         zfi.tmz_date.tm_hour = iHour;
         zfi.tmz_date.tm_min  = iMinute;
         zfi.tmz_date.tm_sec  = iSecond;

         zfi.tmz_date.tm_year = iYear;
         zfi.tmz_date.tm_mon  = iMonth - 1;
         zfi.tmz_date.tm_mday = iDay;

         zfi.internal_fa = hb_parnl( 5 );
         zfi.external_fa = hb_parnl( 6 );
#if ! defined( HB_OS_UNIX )
         if( ( zfi.external_fa & 0xFFFF0000 ) == 0 )
            zfi.external_fa = hb_translateExtAttr( szZipName, zfi.external_fa );
#endif

         if( fUnicode )
         {
            szZipName = hb_parstr_utf8( 2, &hZipName, NULL );
            szComment = hb_parstr_utf8( 11, &hComment, NULL );
            flags    |= _ZIP_FLAG_UNICODE;
         }
         else
            szComment = hb_parc( 11 );

         hb_retni( zipOpenNewFileInZip4( hZip, szZipName, &zfi,
                                         NULL, 0, NULL, 0,
                                         szComment, iMethod, iLevel, 0,
                                         -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                         hb_parc( 9 ), hb_parnl( 10 ), _version_made_by( fUnicode ), flags ) );

         if( fUnicode )
         {
            hb_strfree( hZipName );
            hb_strfree( hComment );
         }
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* hb_zipFileWrite( hZip, cData [, nLen ] ) --> nError */
HB_FUNC( HB_ZIPFILEWRITE )
{
   const char * pData = hb_parc( 2 );

   if( pData )
   {
      zipFile hZip = hb_zipfileParam( 1 );
      if( hZip )
      {
         HB_SIZE nLen = hb_parclen( 2 );

         if( HB_ISNUM( 3 ) )
         {
            HB_SIZE nWrite = hb_parns( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         hb_retni( zipWriteInFileInZip( hZip, pData, ( unsigned ) nLen ) );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* hb_zipFileClose( hZip ) --> nError */
HB_FUNC( HB_ZIPFILECLOSE )
{
   zipFile hZip = hb_zipfileParam( 1 );

   if( hZip )
      hb_retni( zipCloseFileInZip( hZip ) );
}


/* hb_unzipOpen( cFileName ) --> hUnzip */
HB_FUNC( HB_UNZIPOPEN )
{
   const char * szFileName = hb_parc( 1 );

   if( szFileName )
   {
      char *  pszFree;
      unzFile hUnzip = unzOpen( hb_fsNameConv( szFileName, &pszFree ) );

      if( pszFree )
         hb_xfree( pszFree );

      if( hUnzip )
      {
         unzFile * phUnzip = ( unzFile * ) hb_gcAllocate( sizeof( unzFile ), &s_gcUnZipFileFuncs );

         *phUnzip = hUnzip;
         hb_retptrGC( phUnzip );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* hb_unzipClose( hUnzip ) --> nError */
HB_FUNC( HB_UNZIPCLOSE )
{
   unzFile * phUnzip = ( unzFile * ) hb_parptrGC( &s_gcUnZipFileFuncs, 1 );

   if( phUnzip && *phUnzip )
   {
      unzFile hUnzip = *phUnzip;

      *phUnzip = NULL;
      hb_retni( unzClose( hUnzip ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* hb_unzipGlobalInfo( hUnzip, @nEntries, @cGlobalComment ) --> nError */
HB_FUNC( HB_UNZIPGLOBALINFO )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
   {
      unz_global_info ugi;
      int iResult;

      iResult = unzGetGlobalInfo( hUnzip, &ugi );

      hb_retni( iResult );

      if( iResult == UNZ_OK )
      {
         hb_storni( ugi.number_entry, 2 );
         if( HB_ISBYREF( 3 ) )
         {
            if( ugi.size_comment > 0 )
            {
               char * pszComment = ( char * ) hb_xgrab( ugi.size_comment + 1 );

               iResult = unzGetGlobalComment( hUnzip, pszComment, ugi.size_comment );
               if( iResult < 0 )
               {
                  hb_xfree( pszComment );
                  hb_storc( NULL, 3 );
                  hb_retni( iResult );
               }
               else
               {
                  pszComment[ iResult ] = '\0';
                  if( ! hb_storclen_buffer( pszComment, ugi.size_comment, 3 ) )
                     hb_xfree( pszComment );
               }
            }
         }
      }
      else
      {
         hb_storni( 0, 2 );
         hb_storc( NULL, 3 );
      }
   }
}


/* hb_unzipFileFirst( hUnzip ) --> nError */
HB_FUNC( HB_UNZIPFILEFIRST )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
      hb_retni( unzGoToFirstFile( hUnzip ) );
}


/* hb_unzipFileNext( hUnzip ) --> nError */
HB_FUNC( HB_UNZIPFILENEXT )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
      hb_retni( unzGoToNextFile( hUnzip ) );
}


/* hb_unzipFilePos( hUnzip ) --> nPosition */
HB_FUNC( HB_UNZIPFILEPOS )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
      hb_retnint( unzGetOffset( hUnzip ) );
}


/* hb_unzipFileGoto( hUnzip, nPosition ) --> nError */
HB_FUNC( HB_UNZIPFILEGOTO )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
      hb_retni( unzSetOffset( hUnzip, ( uLong ) hb_parnint( 2 ) ) );
}


/* hb_unzipFileInfo( hUnzip, @cZipName, @dDate, @cTime,
                     @nInternalAttr, @nExternalAttr,
                     @nMethod, @nSize, @nCompressedSize,
                     @lCrypted, @cComment, @nCRC ) --> nError */
HB_FUNC( HB_UNZIPFILEINFO )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
   {
      char szFileName[ HB_PATH_MAX * 3 ];
      unz_file_info ufi;
      int  iResult;
      char buf[ 16 ];
      long lJulian, lMillisec;

      iResult = unzGetCurrentFileInfo( hUnzip, &ufi, szFileName, sizeof( szFileName ) - 1,
                                       NULL, 0, NULL, 0 );
      hb_retni( iResult );

      if( iResult == UNZ_OK )
      {
         HB_BOOL fUnicode = ( ufi.flag & _ZIP_FLAG_UNICODE ) != 0;

         szFileName[ sizeof( szFileName ) - 1 ] = '\0';

         if( fUnicode )
            hb_storstr_utf8( szFileName, 2 );
         else
            hb_storc( szFileName, 2 );

         lJulian = hb_dateEncode( ufi.tmu_date.tm_year, ufi.tmu_date.tm_mon + 1,
                                  ufi.tmu_date.tm_mday );
         lMillisec = hb_timeEncode( ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min,
                                    ufi.tmu_date.tm_sec, 0 );

         hb_stortdt( lJulian, lMillisec, 3 );
         if( HB_ISBYREF( 4 ) )
         {
            hb_snprintf( buf, sizeof( buf ), "%02d:%02d:%02d",
                         ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min,
                         ufi.tmu_date.tm_sec );
            hb_storc( buf, 4 );
         }
         hb_stornl( ufi.internal_fa, 5 );
         hb_stornl( ufi.external_fa, 6 );
         hb_stornl( ufi.compression_method, 7 );
         hb_storns( ufi.uncompressed_size, 8 );
         hb_storns( ufi.compressed_size, 9 );
         hb_storl( ( ufi.flag & 1 ) != 0, 10 );
         hb_stornint( ufi.crc, 12 );

         if( ufi.size_file_comment > 0 && HB_ISBYREF( 11 ) )
         {
            char * pszComment = ( char * ) hb_xgrab( ufi.size_file_comment + 1 );

            iResult = unzGetCurrentFileInfo( hUnzip, &ufi, NULL, 0, NULL, 0,
                                             pszComment, ufi.size_file_comment );
            pszComment[ ufi.size_file_comment ] = '\0';
            if( iResult != UNZ_OK )
            {
               hb_xfree( pszComment );
               hb_storc( NULL, 11 );
            }
            else if( fUnicode )
            {
               hb_storstrlen_utf8( pszComment, ufi.size_file_comment, 11 );
               hb_xfree( pszComment );
            }
            else if( ! hb_storclen_buffer( pszComment, ufi.size_file_comment, 11 ) )
               hb_xfree( pszComment );
         }
      }
      else
      {
         hb_storc( NULL, 2 );
         hb_stortdt( 0, 0, 3 );
         hb_storc( NULL, 4 );
         hb_stornl( 0, 5 );
         hb_stornl( 0, 6 );
         hb_stornl( 0, 7 );
         hb_storns( 0, 8 );
         hb_storns( 0, 9 );
         hb_storl( HB_FALSE, 10 );
         hb_storc( NULL, 11 );
      }
   }
}


/* hb_unzipFileOpen( hUnzip, [ cPassword ] ) --> nError */
HB_FUNC( HB_UNZIPFILEOPEN )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
      hb_retni( unzOpenCurrentFilePassword( hUnzip, hb_parc( 2 ) ) );
}


/* hb_unzipFileRead( hUnzip, @cBuf [, nLen ] ) --> nRead */
HB_FUNC( HB_UNZIPFILEREAD )
{
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   char *   buffer;
   HB_SIZE  nSize;

   if( pBuffer && HB_ISBYREF( 2 ) &&
       hb_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
   {
      unzFile hUnzip = hb_unzipfileParam( 1 );

      if( hUnzip )
      {
         if( HB_ISNUM( 3 ) )
         {
            HB_SIZE nRead = hb_parns( 3 );
            if( nRead < nSize )
               nSize = nRead;
         }

         hb_retns( unzReadCurrentFile( hUnzip, buffer, ( unsigned ) nSize ) );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* hb_unzipFileClose( hUnzip ) --> nError */
HB_FUNC( HB_UNZIPFILECLOSE )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
      hb_retni( unzCloseCurrentFile( hUnzip ) );
}


/*
 *
 * Higher level functions - not a wrappers of minizip code
 *
 */

static HB_BOOL hb_zipGetFileInfoFromHandle( PHB_FILE pFile, HB_U32 * pulCRC, HB_BOOL * pfText )
{
   HB_BOOL fText = pfText != NULL, fResult = HB_FALSE;
   HB_U32  ulCRC = 0;

   if( pFile != NULL )
   {
      unsigned char * pString = ( unsigned char * ) hb_xgrab( HB_Z_IOBUF_SIZE );
      HB_SIZE         nRead, u;

      do
      {
         nRead = hb_fileRead( pFile, pString, HB_Z_IOBUF_SIZE, -1 );
         if( nRead > 0 )
         {
            ulCRC = crc32( ulCRC, pString, ( uInt ) nRead );
            if( fText )
            {
               for( u = 0; u < nRead; ++u )
               {
                  if( pString[ u ] < 0x20 ?
                      ( pString[ u ] != HB_CHAR_HT &&
                        pString[ u ] != HB_CHAR_LF &&
                        pString[ u ] != HB_CHAR_CR &&
                        pString[ u ] != HB_CHAR_EOF ) :
                      ( pString[ u ] >= 0x7F && pString[ u ] < 0xA0 &&
                        pString[ u ] != ( unsigned char ) HB_CHAR_SOFT1 ) )
                  {
                     fText = HB_FALSE;
                     break;
                  }
               }
            }
         }
      }
      while( nRead == HB_Z_IOBUF_SIZE );

      fResult = ( hb_fsError() == 0 );

      hb_xfree( pString );
   }

   if( pulCRC )
      *pulCRC = ulCRC;
   if( pfText )
      *pfText = fText;

   return fResult;
}

static HB_BOOL hb_zipGetFileInfo( const char * pszFileName, HB_U32 * pulCRC, HB_BOOL * pfText )
{
   PHB_FILE pFile;
   HB_BOOL  fResult;

   pFile = hb_fileExtOpen( pszFileName, NULL,
                           FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK,
                           NULL, NULL );
   fResult = hb_zipGetFileInfoFromHandle( pFile, pulCRC, pfText );
   if( pFile != NULL )
      hb_fileClose( pFile );

   return fResult;
}


/* hb_zipFileCRC32( cFileName ) --> nError */
HB_FUNC( HB_ZIPFILECRC32 )
{
   const char * szFileName = hb_parc( 1 );

   if( szFileName )
   {
      HB_U32 ulCRC = 0;
      if( ! hb_zipGetFileInfo( szFileName, &ulCRC, NULL ) )
         ulCRC = 0;
      hb_retnint( ulCRC );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static int hb_zipStoreFile( zipFile hZip, int iParamFileName, int iParamZipName, const char * szPassword, int iParamComment, HB_BOOL fUnicode )
{
   const char * szFileName = hb_parc( iParamFileName );
   const char * szName     = hb_parc( iParamZipName );
   char *       pString;
   PHB_FILE     pFile;
   HB_SIZE      nLen;
   HB_FATTR     ulExtAttr;
   zip_fileinfo zfi;
   int          iResult;
   HB_BOOL      fError;
   HB_BOOL      fText;
   HB_U32       ulCRC;
   uLong        flags = 0;
   void *       hZipName = NULL;
   void *       hComment = NULL;
   char *       szZipName;
   const char * szComment;

   memset( &zfi, 0, sizeof( zfi ) );
   fError    = HB_FALSE;
   ulExtAttr = 0;

#if defined( HB_OS_WIN )
   if( hb_fileIsLocalName( szFileName ) )
   {
      LPTSTR  lpFileNameFree;
      LPCTSTR lpFileName = HB_FSNAMECONV( szFileName, &lpFileNameFree );
      DWORD   attr       = GetFileAttributes( lpFileName );

      if( attr != INVALID_FILE_ATTRIBUTES )
      {
         ulExtAttr = attr & ( FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_HIDDEN |
                              FILE_ATTRIBUTE_SYSTEM | FILE_ATTRIBUTE_DIRECTORY |
                              FILE_ATTRIBUTE_ARCHIVE );
      }
      else
         fError = HB_TRUE;

      if( lpFileNameFree )
         hb_xfree( lpFileNameFree );
   }
   else
#elif defined( HB_OS_OS2 )
   if( hb_fileIsLocalName( szFileName ) )
   {
      FILESTATUS3 fs3;
      APIRET      ulrc;
      char *      pszFree;

      ulrc = DosQueryPathInfo( ( PCSZ ) hb_fsNameConv( szFileName, &pszFree ), FIL_STANDARD, &fs3, sizeof( fs3 ) );

      if( pszFree )
         hb_xfree( pszFree );

      if( ulrc == NO_ERROR )
      {
         if( fs3.attrFile & FILE_READONLY )
            ulExtAttr |= HB_FA_READONLY;
         if( fs3.attrFile & FILE_HIDDEN )
            ulExtAttr |= HB_FA_HIDDEN;
         if( fs3.attrFile & FILE_SYSTEM )
            ulExtAttr |= HB_FA_SYSTEM;
         if( fs3.attrFile & FILE_DIRECTORY )
            ulExtAttr |= HB_FA_DIRECTORY;
         if( fs3.attrFile & FILE_ARCHIVED )
            ulExtAttr |= HB_FA_ARCHIVE;

         zfi.tmz_date.tm_sec  = fs3.ftimeLastWrite.twosecs * 2;
         zfi.tmz_date.tm_min  = fs3.ftimeLastWrite.minutes;
         zfi.tmz_date.tm_hour = fs3.ftimeLastWrite.hours;
         zfi.tmz_date.tm_mday = fs3.fdateLastWrite.day;
         zfi.tmz_date.tm_mon  = fs3.fdateLastWrite.month;
         zfi.tmz_date.tm_year = fs3.fdateLastWrite.year + 1980;
      }
      else
         fError = HB_TRUE;
   }
   else
#elif defined( HB_OS_UNIX )
   if( hb_fileIsLocalName( szFileName ) )
   {
      struct tm   st;
      time_t      ftime;
      char *      pszFree;
#  if defined( HB_USE_LARGEFILE64 )
      struct stat64 statbuf;
      if( stat64( hb_fsNameConv( szFileName, &pszFree ), &statbuf ) == 0 )
#  else
      struct stat statbuf;
      if( stat( hb_fsNameConv( szFileName, &pszFree ), &statbuf ) == 0 )
#  endif
      {
         if( S_ISDIR( statbuf.st_mode ) )
         {
            ulExtAttr |= 0x40000000;
            ulExtAttr |= 0x10; /* FILE_ATTRIBUTE_DIRECTORY */
         }
         else
         {
            ulExtAttr |= 0x80000000;
            ulExtAttr |= 0x20; /* FILE_ATTRIBUTE_ARCHIVE */
         }

         ulExtAttr |= ( ( statbuf.st_mode & S_IXOTH ) ? 0x00010000 : 0 ) |
                      ( ( statbuf.st_mode & S_IWOTH ) ? 0x00020000 : 0 ) |
                      ( ( statbuf.st_mode & S_IROTH ) ? 0x00040000 : 0 ) |
                      ( ( statbuf.st_mode & S_IXGRP ) ? 0x00080000 : 0 ) |
                      ( ( statbuf.st_mode & S_IWGRP ) ? 0x00100000 : 0 ) |
                      ( ( statbuf.st_mode & S_IRGRP ) ? 0x00200000 : 0 ) |
                      ( ( statbuf.st_mode & S_IXUSR ) ? 0x00400000 : 0 ) |
                      ( ( statbuf.st_mode & S_IWUSR ) ? 0x00800000 : 0 ) |
                      ( ( statbuf.st_mode & S_IRUSR ) ? 0x01000000 : 0 );

         ftime = statbuf.st_mtime;
#  if defined( HB_HAS_LOCALTIME_R )
         localtime_r( &ftime, &st );
#  else
         st = *localtime( &ftime );
#  endif

         zfi.tmz_date.tm_sec  = st.tm_sec;
         zfi.tmz_date.tm_min  = st.tm_min;
         zfi.tmz_date.tm_hour = st.tm_hour;
         zfi.tmz_date.tm_mday = st.tm_mday;
         zfi.tmz_date.tm_mon  = st.tm_mon;
         zfi.tmz_date.tm_year = st.tm_year;
      }
      else
         fError = HB_TRUE;

      if( pszFree )
         hb_xfree( pszFree );
   }
   else
#endif
   {
      HB_FATTR attr;
      long lJulian, lMillisec;

      if( ! hb_fileAttrGet( szFileName, &attr ) )
         ulExtAttr = 0x81B60020;  /* HB_FA_ARCHIVE | rw-rw-rw- */
      else
      {
#if defined( HB_OS_UNIX )
         if( attr & HB_FA_DIRECTORY )
            ulExtAttr |= 0x40000000;
         else
         {
            ulExtAttr |= 0x80000000;
            ulExtAttr |= HB_FA_ARCHIVE;
         }
         /* Harbour uses the same binary values for unix access rights and
          * DOS/WIN/OS2 attributes so we can use them directly
          */
         ulExtAttr |= attr & ( HB_FA_RWXU | HB_FA_RWXG | HB_FA_RWXO );
#endif
         ulExtAttr |= attr & ( HB_FA_READONLY | HB_FA_HIDDEN | HB_FA_SYSTEM |
                               HB_FA_DIRECTORY | HB_FA_ARCHIVE );
      }

      if( hb_fileTimeGet( szFileName, &lJulian, &lMillisec ) )
      {
         int iYear, iMonth, iDay;
         int iHour, iMinute, iSecond, iMSec;

         hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
         hb_timeDecode( lMillisec, &iHour, &iMinute, &iSecond, &iMSec );

         zfi.tmz_date.tm_sec  = iSecond;
         zfi.tmz_date.tm_min  = iMinute;
         zfi.tmz_date.tm_hour = iHour;
         zfi.tmz_date.tm_mday = iDay;
         zfi.tmz_date.tm_mon  = iMonth - 1;
         zfi.tmz_date.tm_year = iYear;
      }
   }

   if( fError )
      return -200;

#if ! defined( HB_OS_UNIX )
   ulExtAttr = hb_translateExtAttr( szFileName, ulExtAttr );
#endif

   if( ! HB_ISCHAR( iParamZipName ) )
      iParamZipName = iParamFileName;

   if( fUnicode )
   {
      szZipName = hb_strdup( hb_parstr_utf8( iParamZipName, &hZipName, NULL ) );
      szComment = hb_parstr_utf8( iParamComment, &hComment, NULL );
      flags    |= _ZIP_FLAG_UNICODE;
   }
   else
   {
      szZipName = hb_strdup( hb_parc( iParamZipName ) );
      szComment = hb_parc( iParamComment );
   }

   if( szName )
   {
      /* change path separators to '/' */
      nLen    = strlen( szZipName );
      pString = szZipName;
      while( nLen-- )
      {
         if( pString[ nLen ] == '\\' )
            pString[ nLen ] = '/';
      }
   }
   else
   {
      /* get file name */
      szZipName = hb_strdup( szFileName );

      nLen    = strlen( szZipName );
      pString = szZipName;

      while( nLen-- )
      {
         if( pString[ nLen ] == '/' || pString[ nLen ] == '\\' )
         {
            memmove( szZipName, &pString[ nLen + 1 ], strlen( szZipName ) - nLen );
            break;
         }
      }
   }

   fText = HB_FALSE;
   ulCRC = 0;

   zfi.external_fa = ulExtAttr;
   /* TODO: zip.exe test: 0 for binary file, 1 for text. Does not depend on
      extension. We should analyse content of file to determine this??? */
   zfi.internal_fa = 0;

   if( ulExtAttr & 0x40000000 )
   {
      iResult = zipOpenNewFileInZip4( hZip, szZipName, &zfi, NULL, 0, NULL, 0, szComment,
                                      Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                      -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                      szPassword, ulCRC, _version_made_by( fUnicode ), flags );
      if( iResult == 0 )
         zipCloseFileInZip( hZip );
   }
   else
   {
      pFile = hb_fileExtOpen( szFileName, NULL,
                              FO_READ | FO_SHARED | FO_PRIVATE | FXO_SHARELOCK,
                              NULL, NULL );
      if( pFile != NULL )
      {
#if defined( HB_OS_WIN )
         if( hb_fileIsLocal( pFile ) )
         {
            FILETIME   ftutc, ft;
            SYSTEMTIME st;

            if( GetFileTime( ( HANDLE ) hb_fileHandle( pFile ), NULL, NULL, &ftutc ) &&
                FileTimeToLocalFileTime( &ftutc, &ft ) &&
                FileTimeToSystemTime( &ft, &st ) )
            {
               zfi.tmz_date.tm_sec  = st.wSecond;
               zfi.tmz_date.tm_min  = st.wMinute;
               zfi.tmz_date.tm_hour = st.wHour;
               zfi.tmz_date.tm_mday = st.wDay;
               zfi.tmz_date.tm_mon  = st.wMonth - 1;
               zfi.tmz_date.tm_year = st.wYear;
            }
         }
#endif
         if( szPassword )
         {
            if( hb_zipGetFileInfo( szFileName, &ulCRC, &fText ) )
               zfi.internal_fa = fText ? 1 : 0;
         }

         iResult = zipOpenNewFileInZip4( hZip, szZipName, &zfi, NULL, 0, NULL, 0, szComment,
                                         Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                         -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                         szPassword, ulCRC, _version_made_by( fUnicode ), flags );
         if( iResult == 0 )
         {
            pString = ( char * ) hb_xgrab( HB_Z_IOBUF_SIZE );
            while( ( nLen = hb_fileRead( pFile, pString, HB_Z_IOBUF_SIZE, -1 ) ) > 0 )
               zipWriteInFileInZip( hZip, pString, ( unsigned ) nLen );

            hb_xfree( pString );

            zipCloseFileInZip( hZip );
         }
         hb_fileClose( pFile );
      }
      else
         iResult = -200 - hb_fsError();
   }

   hb_xfree( szZipName );

   if( fUnicode )
   {
      hb_strfree( hZipName );
      hb_strfree( hComment );
   }

   return iResult;
}


/* hb_zipStoreFile( hZip, cFileName, [ cZipName ], [ cPassword ], [ cComment ], [ lUnicode ] ) --> nError */
HB_FUNC( HB_ZIPSTOREFILE )
{
   if( hb_parc( 2 ) )
   {
      zipFile hZip = hb_zipfileParam( 1 );

      if( hZip )
         hb_retni( hb_zipStoreFile( hZip, 2, 3, hb_parc( 4 ), 5, hb_parl( 6 ) ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


static int hb_zipStoreFileHandle( zipFile hZip, PHB_FILE pFile, int iParamZipName, const char * szPassword, int iParamComment, HB_BOOL fUnicode )
{
   HB_SIZE      nLen;
   zip_fileinfo zfi;
   int          iResult;
   HB_BOOL      fText;
   HB_U32       ulCRC;

   uLong flags = 0;

   void *       hZipName = NULL;
   void *       hComment = NULL;
   char *       szZipName;
   const char * szComment;

   if( pFile == NULL )
      return -200;

   if( fUnicode )
   {
      szZipName = hb_strdup( hb_parstr_utf8( iParamZipName, &hZipName, NULL ) );
      szComment = hb_parstr_utf8( iParamComment, &hComment, NULL );
      flags    |= _ZIP_FLAG_UNICODE;
   }
   else
   {
      szZipName = hb_strdup( hb_parc( iParamZipName ) );
      szComment = hb_parc( iParamComment );
   }

   /* change path separators to '/' */

   nLen = strlen( szZipName );
   while( nLen-- )
   {
      if( szZipName[ nLen ] == '\\' )
         szZipName[ nLen ] = '/';
   }

   memset( &zfi, 0, sizeof( zfi ) );

   zfi.external_fa      = 0x81B60020;
   zfi.tmz_date.tm_sec  = 0;
   zfi.tmz_date.tm_min  = 0;
   zfi.tmz_date.tm_hour = 0;
   zfi.tmz_date.tm_mday = 1;
   zfi.tmz_date.tm_mon  = 0;
   zfi.tmz_date.tm_year = 0;

   ulCRC = 0;
   fText = HB_FALSE;
   if( szPassword && hb_zipGetFileInfoFromHandle( pFile, &ulCRC, &fText ) )
      zfi.internal_fa = fText ? 1 : 0;
   else
      /* TODO: zip.exe test: 0 for binary file, 1 for text. Does not depend on
         extension. We should analyse content of file to determine this??? */
      zfi.internal_fa = 0;

   iResult = zipOpenNewFileInZip4( hZip, szZipName, &zfi, NULL, 0, NULL, 0, szComment,
                                   Z_DEFLATED, Z_DEFAULT_COMPRESSION, 0,
                                   -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                   szPassword, ulCRC, _version_made_by( fUnicode ), flags );
   if( iResult == 0 )
   {
      char * pString = ( char * ) hb_xgrab( HB_Z_IOBUF_SIZE );
      hb_fileSeek( pFile, 0, FS_SET );
      while( ( nLen = hb_fileRead( pFile, pString, HB_Z_IOBUF_SIZE, -1 ) ) > 0 )
         zipWriteInFileInZip( hZip, pString, ( unsigned ) nLen );
      hb_xfree( pString );

      zipCloseFileInZip( hZip );
   }

   hb_xfree( szZipName );

   if( fUnicode )
   {
      hb_strfree( hZipName );
      hb_strfree( hComment );
   }

   return iResult;
}


/* hb_zipStoreFileHandle( hZip, fhnd, cZipName, [ cPassword ], [ cComment ], [ lUnicode ] ) --> nError */
HB_FUNC( HB_ZIPSTOREFILEHANDLE )
{
   if( HB_ISCHAR( 3 ) )
   {
      zipFile hZip = hb_zipfileParam( 1 );

      if( hZip )
      {
         HB_BOOL fFree;
         PHB_FILE pFile = hb_fileHandleParam( 2, &fFree );

         if( pFile != NULL )
         {
            hb_retni( hb_zipStoreFileHandle( hZip, pFile, 3, hb_parc( 4 ), 5, hb_parl( 6 ) ) );
            if( fFree )
               hb_fileDetach( pFile );
         }
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


static int hb_unzipExtractCurrentFile( unzFile hUnzip, const char * szFileName, const char * szPassword )
{
   char          szNameRaw[ HB_PATH_MAX * 3 ];
   char *        szName;
   HB_SIZE       nPos, nLen;
   char          cSep, * pString;
   unz_file_info ufi;
   int           iResult;
   PHB_FILE      pFile;

   iResult = unzGetCurrentFileInfo( hUnzip, &ufi, szNameRaw, sizeof( szNameRaw ) - 1,
                                    NULL, 0, NULL, 0 );
   if( iResult != UNZ_OK )
      return iResult;

   iResult = unzOpenCurrentFilePassword( hUnzip, szPassword );

   if( iResult != UNZ_OK )
      return iResult;

   if( szFileName )
      szName = hb_strdup( szFileName );
   else
   {
      HB_BOOL fUnicode = ( ufi.flag & _ZIP_FLAG_UNICODE ) != 0;

      if( fUnicode )
      {
         PHB_ITEM pTemp = hb_itemPutStrUTF8( NULL, szNameRaw );

         szName = hb_strdup( hb_itemGetCPtr( pTemp ) );

         hb_itemRelease( pTemp );
      }
      else
         szName = hb_strdup( szNameRaw );
   }

   nLen = strlen( szName );

   /* Test shows that files in subfolders can be stored to zip file without
      explicitly adding folder. So, let's create a required path */

   nPos = 1;
   while( nPos < nLen )
   {
      cSep = szName[ nPos ];

      /* allow both path separators, ignore terminating path separator */
      if( ( cSep == '\\' || cSep == '/' ) && nPos < nLen - 1 )
      {
         szName[ nPos ] = '\0';
         hb_fileDirMake( szName );
         szName[ nPos ] = cSep;
      }
      nPos++;
   }

   if( ufi.external_fa & 0x40000000 ) /* DIRECTORY */
   {
      if( ! hb_fileDirMake( szName ) )
         iResult = -200 - hb_fsError();
   }
   else
   {
      pFile = hb_fileExtOpen( szName, NULL,
                              FO_READWRITE | FO_EXCLUSIVE | FO_PRIVATE |
                              FXO_TRUNCATE | FXO_SHARELOCK, NULL, NULL );
      if( pFile != NULL )
      {
         pString = ( char * ) hb_xgrab( HB_Z_IOBUF_SIZE );

         while( ( iResult = unzReadCurrentFile( hUnzip, pString, HB_Z_IOBUF_SIZE ) ) > 0 )
            hb_fileWrite( pFile, pString, ( HB_SIZE ) iResult, -1 );

         hb_xfree( pString );

#if defined( HB_OS_WIN )
         if( hb_fileIsLocal( pFile ) )
         {
            FILETIME   ftutc, ft;
            SYSTEMTIME st;

            st.wSecond       = ( WORD ) ufi.tmu_date.tm_sec;
            st.wMinute       = ( WORD ) ufi.tmu_date.tm_min;
            st.wHour         = ( WORD ) ufi.tmu_date.tm_hour;
            st.wDay          = ( WORD ) ufi.tmu_date.tm_mday;
            st.wMonth        = ( WORD ) ufi.tmu_date.tm_mon + 1;
            st.wYear         = ( WORD ) ufi.tmu_date.tm_year;
            st.wMilliseconds = 0;

            if( SystemTimeToFileTime( &st, &ft ) &&
                LocalFileTimeToFileTime( &ft, &ftutc ) )
            {
               SetFileTime( ( HANDLE ) hb_fileHandle( pFile ), &ftutc, &ftutc, &ftutc );
            }
         }
#endif

         hb_fileClose( pFile );
      }
      else
         iResult = -200 - hb_fsError();
   }
   unzCloseCurrentFile( hUnzip );

#if defined( HB_OS_WIN )
   if( hb_fileIsLocalName( szName ) )
   {
      LPTSTR  lpFileNameFree;
      LPCTSTR lpFileName = HB_FSNAMECONV( szName, &lpFileNameFree );

      SetFileAttributes( ( LPCTSTR ) lpFileName, ufi.external_fa & 0xFF );

      if( lpFileNameFree )
         hb_xfree( lpFileNameFree );
   }
   else
#elif defined( HB_OS_OS2 )
   if( hb_fileIsLocalName( szName ) )
   {
      FILESTATUS3 fs3;
      APIRET      ulrc;
      HB_FATTR    ulAttr = FILE_NORMAL;
      int         iAttr  = ufi.external_fa & 0xFF;

      char *       pszFree;
      const char * szNameOS = hb_fsNameConv( szName, &pszFree );

      if( iAttr & HB_FA_READONLY )
         ulAttr |= FILE_READONLY;
      if( iAttr & HB_FA_HIDDEN )
         ulAttr |= FILE_HIDDEN;
      if( iAttr & HB_FA_SYSTEM )
         ulAttr |= FILE_SYSTEM;
      if( iAttr & HB_FA_ARCHIVE )
         ulAttr |= FILE_ARCHIVED;

      ulrc = DosQueryPathInfo( ( PCSZ ) szNameOS, FIL_STANDARD, &fs3, sizeof( fs3 ) );

      if( ulrc == NO_ERROR )
      {
         FDATE fdate;
         FTIME ftime;

         fdate.year    = ufi.tmu_date.tm_year - 1980;
         fdate.month   = ufi.tmu_date.tm_mon;
         fdate.day     = ufi.tmu_date.tm_mday;
         ftime.hours   = ufi.tmu_date.tm_hour;
         ftime.minutes = ufi.tmu_date.tm_min;
         ftime.twosecs = ufi.tmu_date.tm_sec / 2;

         fs3.attrFile = ulAttr;

         fs3.fdateCreation = fs3.fdateLastAccess = fs3.fdateLastWrite = fdate;
         fs3.ftimeCreation = fs3.ftimeLastAccess = fs3.ftimeLastWrite = ftime;
         ulrc = DosSetPathInfo( ( PCSZ ) szNameOS, FIL_STANDARD,
                                &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
      }

      if( pszFree )
         hb_xfree( pszFree );
   }
   else
#elif defined( HB_OS_UNIX )
   if( hb_fileIsLocalName( szName ) )
   {
      struct utimbuf utim;
      struct tm      st;

      char *       pszFree;
      const char * szNameOS = hb_fsNameConv( szName, &pszFree );

      HB_FATTR ulAttr = ufi.external_fa;

      if( ( ulAttr & 0xFFFF0000 ) == 0 )
         ulAttr = hb_translateExtAttr( szName, ulAttr );

      ( void ) chmod( szNameOS,
                      ( ( ulAttr & 0x00010000 ) ? S_IXOTH : 0 ) |
                      ( ( ulAttr & 0x00020000 ) ? S_IWOTH : 0 ) |
                      ( ( ulAttr & 0x00040000 ) ? S_IROTH : 0 ) |
                      ( ( ulAttr & 0x00080000 ) ? S_IXGRP : 0 ) |
                      ( ( ulAttr & 0x00100000 ) ? S_IWGRP : 0 ) |
                      ( ( ulAttr & 0x00200000 ) ? S_IRGRP : 0 ) |
                      ( ( ulAttr & 0x00400000 ) ? S_IXUSR : 0 ) |
                      ( ( ulAttr & 0x00800000 ) ? S_IWUSR : 0 ) |
                      ( ( ulAttr & 0x01000000 ) ? S_IRUSR : 0 ) );
      memset( &st, 0, sizeof( st ) );

      st.tm_sec  = ufi.tmu_date.tm_sec;
      st.tm_min  = ufi.tmu_date.tm_min;
      st.tm_hour = ufi.tmu_date.tm_hour;
      st.tm_mday = ufi.tmu_date.tm_mday;
      st.tm_mon  = ufi.tmu_date.tm_mon;
      st.tm_year = ufi.tmu_date.tm_year - 1900;

      utim.actime = utim.modtime = mktime( &st );
      ( void ) utime( szNameOS, &utim );

      if( pszFree )
         hb_xfree( pszFree );
   }
   else
#endif
   {
      long lJulian, lMillisec;
      HB_FATTR ulAttr = ufi.external_fa;

      lJulian = hb_dateEncode( ufi.tmu_date.tm_year, ufi.tmu_date.tm_mon + 1,
                               ufi.tmu_date.tm_mday );
      lMillisec = hb_timeEncode( ufi.tmu_date.tm_hour, ufi.tmu_date.tm_min,
                                 ufi.tmu_date.tm_sec, 0 );
      hb_fileTimeSet( szName, lJulian, lMillisec );

#if defined( HB_OS_UNIX )
      if( ( ulAttr & 0xFFFF0000 ) == 0 )
         ulAttr = hb_translateExtAttr( szName, ulAttr );
      ulAttr &= 0x01FF0000;
#else
      ulAttr &= 0xFF;
#endif
      hb_fileAttrSet( szName, ulAttr );
   }

   hb_xfree( szName );

   return iResult;
}


/* hb_unzipExtractCurrentFile( hZip, [ cFileName ], [ cPassword ] ) --> nError */
HB_FUNC( HB_UNZIPEXTRACTCURRENTFILE )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
      hb_retni( hb_unzipExtractCurrentFile( hUnzip, hb_parc( 2 ), hb_parc( 3 ) ) );
}


static int hb_unzipExtractCurrentFileToHandle( unzFile hUnzip, PHB_FILE pFile, const char * szPassword )
{
   unz_file_info ufi;
   int iResult;

   if( pFile == NULL )
      return -200;

   iResult = unzGetCurrentFileInfo( hUnzip, &ufi, NULL, 0,
                                    NULL, 0, NULL, 0 );
   if( iResult != UNZ_OK )
      return iResult;

   iResult = unzOpenCurrentFilePassword( hUnzip, szPassword );

   if( iResult != UNZ_OK )
      return iResult;

   if( ! ( ufi.external_fa & 0x40000000 ) ) /* DIRECTORY */
   {
      char * pString = ( char * ) hb_xgrab( HB_Z_IOBUF_SIZE );

      while( ( iResult = unzReadCurrentFile( hUnzip, pString, HB_Z_IOBUF_SIZE ) ) > 0 )
         hb_fileWrite( pFile, pString, ( HB_SIZE ) iResult, -1 );

      hb_xfree( pString );

#if defined( HB_OS_WIN )
      if( hb_fileIsLocal( pFile ) )
      {
         FILETIME   ftutc, ft;
         SYSTEMTIME st;

         st.wSecond       = ( WORD ) ufi.tmu_date.tm_sec;
         st.wMinute       = ( WORD ) ufi.tmu_date.tm_min;
         st.wHour         = ( WORD ) ufi.tmu_date.tm_hour;
         st.wDay          = ( WORD ) ufi.tmu_date.tm_mday;
         st.wMonth        = ( WORD ) ufi.tmu_date.tm_mon + 1;
         st.wYear         = ( WORD ) ufi.tmu_date.tm_year;
         st.wMilliseconds = 0;

         if( SystemTimeToFileTime( &st, &ft ) &&
             LocalFileTimeToFileTime( &ft, &ftutc ) )
         {
            SetFileTime( ( HANDLE ) hb_fileHandle( pFile ), &ftutc, &ftutc, &ftutc );
         }
      }
#endif
   }
   unzCloseCurrentFile( hUnzip );

   return iResult;
}


/* hb_unzipExtractCurrentFileToHandle( hZip, fhnd, [ cPassword ] ) --> nError */
HB_FUNC( HB_UNZIPEXTRACTCURRENTFILETOHANDLE )
{
   unzFile hUnzip = hb_unzipfileParam( 1 );

   if( hUnzip )
   {
      HB_BOOL fFree;
      PHB_FILE pFile = hb_fileHandleParam( 2, &fFree );

      if( pFile != NULL )
      {
         hb_retni( hb_unzipExtractCurrentFileToHandle( hUnzip, pFile, hb_parc( 3 ) ) );
         if( fFree )
            hb_fileDetach( pFile );
      }
   }
}


static int hb_zipDeleteFile( const char * szZipFile, const char * szFileMask )
{
   char            szTempFile[ HB_PATH_MAX ];
   char            szCurrFile[ HB_PATH_MAX * 3 ];
   PHB_FNAME       pFileName;
   PHB_FILE        pFile;
   unzFile         hUnzip;
   zipFile         hZip;
   unz_global_info ugi;
   unz_file_info   ufi;
   zip_fileinfo    zfi;
   char *          pszGlobalComment = NULL;
   char *          pszFileComment   = NULL;
   void *          pExtraField      = NULL;
   void *          pLocalExtraField = NULL;
   int    iFilesLeft = 0;
   int    iFilesDel  = 0;
   int    iExtraFieldLen;
   int    method;
   int    level;
   int    iResult;
   char * pszFree;

   /* open source file */
   hUnzip = unzOpen( hb_fsNameConv( szZipFile, &pszFree ) );

   if( pszFree )
      hb_xfree( pszFree );

   if( hUnzip == NULL )
      return UNZ_ERRNO;

   pFileName = hb_fsFNameSplit( szZipFile );
   pFile     = hb_fileCreateTemp( pFileName->szPath, NULL, FC_NORMAL, szTempFile );
   hZip      = NULL;
   if( pFile != NULL )
   {
      hb_fileClose( pFile );
      hZip = zipOpen( szTempFile, APPEND_STATUS_CREATE );
   }
   hb_xfree( pFileName );

   if( hZip == NULL )
   {
      unzClose( hUnzip );
      return UNZ_ERRNO;
   }

   iResult = unzGetGlobalInfo( hUnzip, &ugi );
   if( iResult == UNZ_OK )
   {
      if( ugi.size_comment > 0 )
      {
         pszGlobalComment = ( char * ) hb_xgrab( ugi.size_comment + 1 );
         if( ( uLong ) unzGetGlobalComment( hUnzip, pszGlobalComment,
                                            ugi.size_comment ) != ugi.size_comment )
            iResult = UNZ_ERRNO;
         pszGlobalComment[ ugi.size_comment ] = '\0';
      }
      if( iResult == UNZ_OK )
         iResult = unzGoToFirstFile( hUnzip );
   }

   while( iResult == UNZ_OK )
   {
      iResult = unzGetCurrentFileInfo( hUnzip, &ufi, szCurrFile, sizeof( szCurrFile ) - 1, NULL, 0, NULL, 0 );
      if( iResult != UNZ_OK )
         break;

      if( hb_strMatchFile( szCurrFile, szFileMask ) )
         iFilesDel++;
      else
      {
         HB_BOOL fUnicode;

         if( ufi.size_file_extra )
            pExtraField = ( char * ) hb_xgrab( ufi.size_file_extra );
         if( ufi.size_file_comment )
            pszFileComment = ( char * ) hb_xgrab( ufi.size_file_comment + 1 );

         iResult = unzGetCurrentFileInfo( hUnzip, &ufi, NULL, 0,
                                          pExtraField, ufi.size_file_extra,
                                          pszFileComment, ufi.size_file_comment );
         if( pszFileComment )
            pszFileComment[ ufi.size_file_comment ] = '\0';
         if( iResult != UNZ_OK )
            break;

         iResult = unzOpenCurrentFile2( hUnzip, &method, &level, 1 );
         if( iResult != UNZ_OK )
            break;

         iExtraFieldLen = unzGetLocalExtrafield( hUnzip, NULL, 0 );
         if( iExtraFieldLen < 0 )
         {
            iResult = UNZ_ERRNO;
            break;
         }
         else if( iExtraFieldLen > 0 )
         {
            pLocalExtraField = hb_xgrab( iExtraFieldLen );
            if( unzGetLocalExtrafield( hUnzip, pLocalExtraField, iExtraFieldLen ) != iExtraFieldLen )
            {
               iResult = UNZ_ERRNO;
               break;
            }
         }

         fUnicode = ( ufi.flag & _ZIP_FLAG_UNICODE ) != 0;

         memset( &zfi, 0, sizeof( zfi ) );
         memcpy( &zfi.tmz_date, &ufi.tmu_date, sizeof( tm_unz ) );
         zfi.dosDate     = ufi.dosDate;
         zfi.internal_fa = ufi.internal_fa;
         zfi.external_fa = ufi.external_fa;

         iResult = zipOpenNewFileInZip4( hZip, szCurrFile, &zfi, pLocalExtraField, iExtraFieldLen, pExtraField, ufi.size_file_extra, pszFileComment,
                                         method, level, 1,
                                         -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY,
                                         NULL, 0, _version_made_by( fUnicode ), ufi.flag );
         if( iResult != UNZ_OK )
            break;

         if( ufi.compressed_size )
         {
            void * buffer = hb_xgrab( HB_Z_IOBUF_SIZE );
            uLong  ulLeft = ufi.compressed_size;

            while( ulLeft > 0 )
            {
               int iRead = HB_MIN( ulLeft, HB_Z_IOBUF_SIZE );
               iResult = unzReadCurrentFile( hUnzip, ( voidp ) buffer, iRead );
               if( iResult < 0 )
                  break;
               if( iResult != iRead )
               {
                  iResult = UNZ_ERRNO;
                  break;
               }
               iResult = zipWriteInFileInZip( hZip, ( voidp ) buffer, iRead );
               if( iResult != UNZ_OK )
                  break;
               ulLeft -= iRead;
            }
            hb_xfree( buffer );
            if( iResult != UNZ_OK )
               break;
         }

         iResult = zipCloseFileInZipRaw( hZip, ufi.uncompressed_size, ufi.crc );
         if( iResult != UNZ_OK )
            break;

         iResult = unzCloseCurrentFile( hUnzip );
         if( iResult != UNZ_OK )
            break;

         if( pExtraField )
         {
            hb_xfree( pExtraField );
            pExtraField = NULL;
         }
         if( pszFileComment )
         {
            hb_xfree( pszFileComment );
            pszFileComment = NULL;
         }
         if( pLocalExtraField )
         {
            hb_xfree( pLocalExtraField );
            pLocalExtraField = NULL;
         }
         iFilesLeft++;
      }
      iResult = unzGoToNextFile( hUnzip );
   }

   if( pExtraField )
      hb_xfree( pExtraField );
   if( pszFileComment )
      hb_xfree( pszFileComment );
   if( pLocalExtraField )
      hb_xfree( pLocalExtraField );

   if( iFilesDel == 0 )
      iResult = UNZ_ERRNO;
   else if( iResult == UNZ_END_OF_LIST_OF_FILE )
      iResult = UNZ_OK;

   if( iResult != UNZ_OK )
      zipClose( hZip, NULL );
   else
      iResult = zipClose( hZip, pszGlobalComment );
   unzClose( hUnzip );
   if( pszGlobalComment )
      hb_xfree( pszGlobalComment );

   if( iResult != UNZ_OK )
      hb_fileDelete( szTempFile );
   else
   {
      hb_fileDelete( szZipFile );

      if( iFilesLeft == 0 )
         hb_fileDelete( szTempFile );
      else if( ! hb_fileRename( szTempFile, szZipFile ) )
         iResult = UNZ_ERRNO;
   }

   return iResult;
}

/* hb_zipDeleteFile( cZipFile, cFileMask ) --> nError */
HB_FUNC( HB_ZIPDELETEFILE )
{
   const char * szZipFile  = hb_parc( 1 );
   const char * szFileMask = hb_parc( 2 );

   if( szZipFile && szFileMask )
      hb_retni( hb_zipDeleteFile( szZipFile, szFileMask ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
