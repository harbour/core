/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hb_fsCopy(), HB_FCOPY() functions
 *
 * Copyright 1991-2008 Viktor Szakats (harbour.01 syenar.hu)
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined( HB_OS_UNIX )
   #include <sys/stat.h>
   #include <unistd.h>
#endif

#define HB_FSCOPY_BUFFERSIZE    16384

HB_BOOL hb_fsCopy( const char * pszSource, const char * pszDest )
{
   HB_ERRCODE errCode;
   HB_BOOL bRetVal;
   HB_FHANDLE fhndSource;
   HB_FHANDLE fhndDest;

   if( ( fhndSource = hb_fsExtOpen( pszSource, NULL, FO_READ | FXO_SHARELOCK, NULL, NULL ) ) != FS_ERROR )
   {
      if( ( fhndDest = hb_fsExtOpen( pszDest, NULL, FXO_TRUNCATE | FO_READWRITE | FO_EXCLUSIVE | FXO_SHARELOCK, NULL, NULL ) ) != FS_ERROR )
      {
#if defined( HB_OS_UNIX )
         struct stat struFileInfo;
         int iSuccess = fstat( fhndSource, &struFileInfo );
#endif
         HB_USHORT nBytesRead;
         void * pbyBuffer = hb_xgrab( HB_FSCOPY_BUFFERSIZE );

         for( ;; )
         {
            if( ( nBytesRead = hb_fsRead( fhndSource, pbyBuffer, HB_FSCOPY_BUFFERSIZE ) ) > 0 )
            {
               if( nBytesRead != hb_fsWrite( fhndDest, pbyBuffer, nBytesRead ) )
               {
                  errCode = hb_fsError();
                  bRetVal = HB_FALSE;
                  break;
               }
            }
            else
            {
               errCode = hb_fsError();
               bRetVal = errCode == 0;
               break;
            }
         }

         hb_xfree( pbyBuffer );

#if defined( HB_OS_UNIX )
         if( iSuccess == 0 )
            fchmod( fhndDest, struFileInfo.st_mode );
#endif

         hb_fsClose( fhndDest );
      }
      else
      {
         errCode = hb_fsError();
         bRetVal = HB_FALSE;
      }

      hb_fsClose( fhndSource );
   }
   else
   {
      errCode = hb_fsError();
      bRetVal = HB_FALSE;
   }

   hb_fsSetFError( errCode );

   return bRetVal;
}

HB_FUNC( HB_FCOPY )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * pszSource = hb_parc( 1 ), * pszDest = hb_parc( 2 );

      if( *pszSource && *pszDest )
         hb_retni( hb_fsCopy( pszSource, pszDest ) ? 0 : F_ERROR );
      else
      {
         hb_fsSetFError( 2 /* file not found */ );
         hb_retni( F_ERROR );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
