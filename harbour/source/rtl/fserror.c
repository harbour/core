/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 * www - http://www.xharbour.org
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
 * along with this software; see the file COPYING.  If not, write to
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

#ifndef HB_OS_WIN_32_USED
   #define HB_OS_WIN_32_USED
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hb_io.h"
#include <errno.h>

static USHORT s_uiErrorLast = 0;
static USHORT s_uiOsErrorLast = 0;


/* Try to translate C errno into DOS error code */

#if !defined(HB_WIN32_IO)
static int hb_errnoToDosError( int ErrCode )
{
   int iResult;

#if defined(__BORLANDC__)
   /* These C compilers use DOS error codes in errno */
   iResult = ErrCode;
#else
   switch ( ErrCode )
   {
#if defined( ENMFILE )
      case ENMFILE:
#endif
      case ENOENT:
         iResult = 2;   /* File not found */
         break;
#if defined( ENOTDIR )
      case ENOTDIR:
         iResult = 3;   /* Path not found */
         break;
#endif
#if defined( ENFILE )
      case ENFILE:
#endif
      case EMFILE:
         iResult = 4;   /* Too many open files */
         break;
      case EACCES:
#if defined( ETXTBSY )
      case ETXTBSY:
#endif
         iResult = 5;   /* Access denied */
         break;
      case EBADF:
         iResult = 6;   /* Invalid handle */
         break;
      case ENOMEM:
         iResult = 8;   /* Insufficient memory */
         break;
#if defined( EFAULT )
      case EFAULT:
         iResult = 9;   /* Invalid memory block address */
         break;
#endif
      case EINVAL:
         iResult = 13;  /* Invalid data */
         break;
#if defined( EROFS )
      case EROFS:
         iResult = 19;  /* Attempt to write on write-protected diskette */
         break;
#endif
#if defined( ESPIPE )
      case ESPIPE:
         iResult = 25;  /* Seek error */
         break;
#endif
      case EPIPE:
         iResult = 29;  /* Write fault */
         break;
      case EEXIST:
         iResult = 32;  /* Sharing violation */
         break;
      case EAGAIN:
         iResult = 33;  /* Lock violation */
         break;
      default:
         iResult = ErrCode;
         break;
   }
#endif

   return iResult;
}
#endif

#if defined(HB_WIN32_IO) || defined(HB_OS_WIN_32)
static int hb_WinToDosError( ULONG ulError )
{
   int iResult;

   switch ( ulError )
   {
      case ERROR_ALREADY_EXISTS:
         iResult = 5;
         break;
      case ERROR_FILE_NOT_FOUND:
         iResult = 2;
         break;
      case ERROR_PATH_NOT_FOUND:
         iResult = 3;
         break;
      case  ERROR_TOO_MANY_OPEN_FILES:
         iResult = 4;
         break;
      case ERROR_INVALID_HANDLE:
         iResult = 6;
         break;

      default:
         iResult = ( int ) ulError ;
         break;
   }

   return iResult;
}
#endif

/* return DOS error code of last operation */
HB_EXPORT USHORT hb_fsError( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsError()"));

   return s_uiErrorLast;
}

/* return real error code of last operation */
HB_EXPORT USHORT hb_fsOsError( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsOsError()"));

   return s_uiOsErrorLast;
}

/* set DOS error code for last operation */
HB_EXPORT void  hb_fsSetError( USHORT uiError )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_fsSetError(%hu)", uiError));

   /* TODO: untranslate uiError into errno */
   s_uiOsErrorLast = s_uiErrorLast = uiError;
}

/* set error code for last operation */
HB_EXPORT void  hb_fsSetIOError( BOOL fResult, USHORT uiOperation )
{

   /* TODO: implement it */
   HB_SYMBOL_UNUSED( uiOperation );

   if ( fResult )
   {
      s_uiOsErrorLast = s_uiErrorLast = 0;
   }
   else
   {
#if defined(HB_WIN32_IO) || defined(HB_OS_WIN_32)
      s_uiOsErrorLast = (USHORT) GetLastError();
      s_uiErrorLast = hb_WinToDosError( s_uiOsErrorLast );
#elif defined(_MSC_VER) || defined(__DMC__)
      #ifdef __XCC__
         extern unsigned long _doserrno;
         extern void __cdecl _dosmaperr( unsigned long oserrno );
         _dosmaperr( GetLastError() );
      #endif
      if ( _doserrno != 0 )
      {
         s_uiOsErrorLast = s_uiErrorLast = _doserrno;
      }
      else
      {
         s_uiOsErrorLast = errno;
         s_uiErrorLast = hb_errnoToDosError( errno );
      }
#else
      s_uiOsErrorLast = errno;
      s_uiErrorLast = hb_errnoToDosError( s_uiOsErrorLast );
#endif
   }
}
