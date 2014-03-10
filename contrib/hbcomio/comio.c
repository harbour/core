/*
 * Harbour Project source code:
 *   I/O driver for serial port streams
 *
 * Copyright 2014 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_IMPLEMENTATION_

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbinit.h"

#include "hbapicom.h"

typedef struct _HB_FILE
{
   const HB_FILE_FUNCS * pFuncs;
   int                   port;
   int                   timeout;
   HB_BOOL               fRead;
   HB_BOOL               fWrite;
} HB_FILE;

static PHB_FILE s_fileNew( int port, int timeout, HB_BOOL fRead, HB_BOOL fWrite );

static int s_fileGetValue( const char * pszName, int * piLen )
{
   int iLen = 0, iValue = 0;

   while( HB_ISDIGIT( pszName[ iLen ] ) )
      iValue = iValue * 10 + ( pszName[ iLen++ ] - '0' );

   *piLen = iLen;
   return iValue;
}

static int s_filePortParams( const char * pszName, int * piTimeout,
                             int * piBaud, int * piParity,
                             int * piSize, int * piStop,
                             int * piFlow )
{
   int iPort = 0, iLen, iValue;

   *piTimeout = -1;
   *piBaud = *piParity = *piSize = *piStop = *piFlow = 0;

   pszName += 3;
   if( *pszName == '$' )
   {
      const char * pszParams = strchr( pszName, ':' );

      if( pszParams != NULL && pszParams - pszName > 1 )
      {
         char * pszPort = hb_strndup( pszName + 1, pszParams - pszName - 1 );

         iPort = hb_comFindPort( pszPort, HB_TRUE );
         hb_xfree( pszPort );
         pszName = pszParams;
      }
   }
   else
   {
      while( HB_ISDIGIT( *pszName ) )
         iPort = iPort * 10 + ( *pszName++ - '0' );
   }

   while( iPort > 0 && *pszName )
   {
      if( HB_ISDIGIT( *pszName ) )
      {
         iValue = s_fileGetValue( pszName, &iLen );
         if( iLen == 1 )
         {
            if( iValue >= 1 && iValue <= 2 && *piStop == 0 )
               *piStop = iValue;
            else if( iValue >= 5 && iValue <= 8 && *piSize == 0 )
               *piSize = iValue;
            else
               iPort = -1;
         }
         else if( iLen == 2 && *piStop == 0 && *piSize == 0 )
         {
            if( pszName[ 0 ] >= '1' && pszName[ 0 ] <= '2' &&
                pszName[ 1 ] >= '5' && pszName[ 1 ] <= '8' )
            {
               *piStop = pszName[ 0 ] - '0';
               *piSize = pszName[ 1 ] - '0';
            }
            else if( pszName[ 0 ] >= '5' && pszName[ 0 ] <= '8' &&
                     pszName[ 1 ] >= '1' && pszName[ 1 ] <= '2' )
            {
               *piStop = pszName[ 1 ] - '0';
               *piSize = pszName[ 0 ] - '0';
            }
            else if( *piBaud )
               iPort = -1;
            else
               *piBaud = iValue;
         }
         else if( *piBaud )
            iPort = -1;
         else
            *piBaud = iValue;
         pszName += iLen;
      }
      else if( HB_ISALPHA( *pszName ) )
      {
         if( hb_strnicmp( pszName, "RTS", 3 ) == 0 )
         {
            *piFlow |= HB_COM_FLOW_IRTSCTS;
            pszName += 3;
         }
         else if( hb_strnicmp( pszName, "CTS", 3 ) == 0 )
         {
            *piFlow |= HB_COM_FLOW_ORTSCTS;
            pszName += 3;
         }
         else if( hb_strnicmp( pszName, "DTR", 3 ) == 0 )
         {
            *piFlow |= HB_COM_FLOW_IDTRDSR;
            pszName += 3;
         }
         else if( hb_strnicmp( pszName, "DSR", 3 ) == 0 )
         {
            *piFlow |= HB_COM_FLOW_ODTRDSR;
            pszName += 3;
         }
         else if( hb_strnicmp( pszName, "DCD", 3 ) == 0 )
         {
            *piFlow |= HB_COM_FLOW_DCD;
            pszName += 3;
         }
         else if( hb_strnicmp( pszName, "XOFF", 4 ) == 0 )
         {
            *piFlow |= HB_COM_FLOW_XOFF;
            pszName += 4;
         }
         else if( hb_strnicmp( pszName, "XON", 3 ) == 0 )
         {
            *piFlow |= HB_COM_FLOW_XON;
            pszName += 3;
         }
         else if( *piParity == 0 && ! HB_ISALPHA( pszName[ 1 ] ) )
         {
            switch( *pszName )
            {
               case 'N':
               case 'n':
               case 'E':
               case 'e':
               case 'O':
               case 'o':
               case 'S':
               case 's':
               case 'M':
               case 'm':
                  *piParity = HB_TOUPPER( *pszName );
                  pszName++;
                  break;
               default:
                  iPort = -1;
                  break;
            }
         }
         else
            iPort = -1;
      }
      else if( *pszName == ':' || *pszName == ',' || *pszName == ' ' )
         pszName++;
      else
         iPort = -1;
   }

   if( *piBaud == 0 )
      *piBaud = 9600;
   if( *piParity == 0 )
      *piParity = 'N';
   if( *piSize == 0 )
      *piSize = 8;
   if( *piStop == 0 )
      *piStop = 1;

   return iPort;
}

static HB_BOOL s_fileAccept( const char * pszFileName )
{
   if( HB_TOUPPER( pszFileName[ 0 ] ) == 'C' &&
       HB_TOUPPER( pszFileName[ 1 ] ) == 'O' &&
       HB_TOUPPER( pszFileName[ 2 ] ) == 'M' )
   {
      if( pszFileName[ 3 ] == '$' )
         return strchr( pszFileName + 4, ':' ) != NULL;
      else if( pszFileName[ 3 ] >= '1' && pszFileName[ 3 ] <= '9' )
      {
         pszFileName += 4;
         while( HB_ISDIGIT( *pszFileName ) )
            ++pszFileName;
         return *pszFileName == ':';
      }
   }
   return HB_FALSE;
}

static PHB_FILE s_fileOpen( const char * pszName, const char * pszDefExt,
                            HB_USHORT uiExFlags,
                            const char * pPaths, PHB_ITEM pError )
{
   PHB_FILE pFile = NULL;
   HB_ERRCODE errcode = 0;
   int iPort, iTimeout, iBaud, iParity, iSize, iStop, iFlow;
   HB_BOOL fRead, fWrite;

   HB_SYMBOL_UNUSED( pszDefExt );
   HB_SYMBOL_UNUSED( pPaths );

   fRead = fWrite = HB_TRUE;
   iPort = s_filePortParams( pszName, &iTimeout,
                             &iBaud, &iParity, &iSize, &iStop, &iFlow );
   if( iPort > 0 )
   {
      if( hb_comOpen( iPort ) == 0 &&
          hb_comInit( iPort, iBaud, iParity, iSize, iStop ) == 0 &&
          hb_comFlowControl( iPort, NULL, iFlow ) == 0 )
      {
         switch( uiExFlags & ( FO_READ | FO_WRITE | FO_READWRITE ) )
         {
            case FO_READ:
               fWrite = HB_FALSE;
               break;
            case FO_WRITE:
               fRead = HB_FALSE;
               break;
         }
         pFile = s_fileNew( iPort, iTimeout, fRead, fWrite );
      }
      else
         errcode = hb_comGetError( iPort );
   }
   else
      errcode = HB_COM_ERR_WRONGPORT;

   hb_fsSetError( errcode );

   if( pError )
   {
      hb_errPutFileName( pError, pszName );
      if( pFile == NULL )
      {
         hb_errPutOsCode( pError, errcode );
         hb_errPutGenCode( pError, ( HB_ERRCODE ) EG_OPEN );
      }
   }

   return pFile;
}

static void s_fileClose( PHB_FILE pFile )
{
   hb_comClose( pFile->port );
   hb_fsSetError( hb_comGetError( pFile->port ) );
   hb_xfree( pFile );
}

static HB_SIZE s_fileRead( PHB_FILE pFile, void * data,
                           HB_SIZE nSize, HB_MAXINT timeout )
{
   HB_ERRCODE errcode;
   long lRead = 0;

   if( pFile->fRead )
   {
      lRead = nSize > LONG_MAX ? LONG_MAX : ( long ) nSize;
      if( timeout == -1 )
         timeout = pFile->timeout;
      lRead = hb_comRecv( pFile->port, data, lRead, timeout );
      errcode = hb_comGetError( pFile->port );
   }
   else
      errcode = HB_COM_ERR_ACCESS;

   hb_fsSetError( errcode );

   return HB_MAX( lRead, 0 );
}

static HB_SIZE s_fileWrite( PHB_FILE pFile, const void * data,
                            HB_SIZE nSize, HB_MAXINT timeout )
{
   HB_ERRCODE errcode;
   long lSent = 0;

   if( pFile->fWrite )
   {
      lSent = nSize > LONG_MAX ? LONG_MAX : ( long ) nSize;
      if( timeout == -1 )
         timeout = pFile->timeout;
      lSent = hb_comSend( pFile->port, data, lSent, timeout );
      errcode = hb_comGetError( pFile->port );
   }
   else
      errcode = HB_COM_ERR_ACCESS;

   hb_fsSetError( errcode );

   return HB_MAX( 0, lSent );
}

static HB_BOOL s_fileConfigure( PHB_FILE pFile, int iIndex, PHB_ITEM pValue )
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( iIndex );
   HB_SYMBOL_UNUSED( pValue );

   return HB_FALSE;
}

static HB_FHANDLE s_fileHandle( PHB_FILE pFile )
{
   return pFile ? hb_comGetDeviceHandle( pFile->port ) : FS_ERROR;
}

static HB_FILE_FUNCS s_fileFuncs =
{
   s_fileAccept,

   NULL, /* s_fileExists */
   NULL, /* s_fileDelete */
   NULL, /* s_fileRename */
   NULL, /* s_fileCopy */

   NULL, /* s_fileDirExists */
   NULL, /* s_fileDirMake */
   NULL, /* s_fileDirRemove */
   NULL, /* s_fileDirSpace */
   NULL, /* s_fileDirectory */

   NULL, /* s_fileTimeGet */
   NULL, /* s_fileTimeSet */
   NULL, /* s_fileAttrGet */
   NULL, /* s_fileAttrSet */

   NULL, /* s_fileLink */
   NULL, /* s_fileLinkSym */
   NULL, /* s_fileLinkRead */

   s_fileOpen,
   s_fileClose,
   NULL, /* s_fileLock */
   NULL, /* s_fileLockTest */
   s_fileRead,
   s_fileWrite,
   NULL, /* s_fileReadAt */
   NULL, /* s_fileWriteAt */
   NULL, /* s_fileTruncAt */
   NULL, /* s_fileSeek */
   NULL, /* s_fileSize */
   NULL, /* s_fileEof */
   NULL, /* s_fileFlush */
   NULL, /* s_fileCommit */
   s_fileConfigure,
   s_fileHandle
};

static PHB_FILE s_fileNew( int port, int timeout, HB_BOOL fRead, HB_BOOL fWrite )
{
   PHB_FILE pFile = ( PHB_FILE ) hb_xgrab( sizeof( HB_FILE ) );

   pFile->pFuncs  = &s_fileFuncs;
   pFile->port    = port;
   pFile->timeout = timeout;
   pFile->fRead   = fRead;
   pFile->fWrite  = fWrite;

   return pFile;
}

HB_FUNC( HB_COMIO ) { ; }


HB_CALL_ON_STARTUP_BEGIN( _hb_file_comio_init_ )
   hb_fileRegisterPart( &s_fileFuncs );
HB_CALL_ON_STARTUP_END( _hb_file_comio_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_file_comio_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_file_comio_init_ )
   #include "hbiniseg.h"
#endif
