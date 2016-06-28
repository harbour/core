/*
 * hb_fsCopy(), hb_FCopy() functions
 *
 * Copyright 1991-2008 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.h"
#include "hbapifs.h"

#define HB_FSCOPY_BUFFERSIZE  65536

HB_BOOL hb_fsCopy( const char * pszSource, const char * pszDest )
{
   HB_BOOL fResult = HB_FALSE;
   PHB_FILE pSrcFile;

   if( ( pSrcFile = hb_fileExtOpen( pszSource, NULL, FO_READ | FO_SHARED | FXO_SHARELOCK, NULL, NULL ) ) != NULL )
   {
      PHB_FILE pDstFile;
      HB_ERRCODE errCode;

      if( ( pDstFile = hb_fileExtOpen( pszDest, NULL, FXO_TRUNCATE | FO_WRITE | FO_EXCLUSIVE | FXO_SHARELOCK, NULL, NULL ) ) != NULL )
      {
         void * pbyBuffer = hb_xgrab( HB_FSCOPY_BUFFERSIZE );

         for( ;; )
         {
            HB_SIZE nBytesRead;
            if( ( nBytesRead = hb_fileRead( pSrcFile, pbyBuffer, HB_FSCOPY_BUFFERSIZE, -1 ) ) > 0 &&
                nBytesRead != ( HB_SIZE ) FS_ERROR )
            {
               if( nBytesRead != hb_fileWrite( pDstFile, pbyBuffer, nBytesRead, -1 ) )
               {
                  errCode = hb_fsError();
                  break;
               }
            }
            else
            {
               errCode = hb_fsError();
               fResult = errCode == 0;
               break;
            }
         }

         hb_xfree( pbyBuffer );

         hb_fileClose( pDstFile );
      }
      else
         errCode = hb_fsError();

      hb_fileClose( pSrcFile );

      if( fResult )
      {
         HB_FATTR ulAttr;

         if( hb_fileAttrGet( pszSource, &ulAttr ) )
            hb_fileAttrSet( pszDest, ulAttr );
      }
      hb_fsSetError( errCode );
   }

   return fResult;
}

HB_FUNC( HB_FCOPY )
{
   HB_ERRCODE errCode = 2; /* file not found */
   HB_BOOL fResult = HB_FALSE;
   const char * pszSource = hb_parc( 1 ), * pszDest = hb_parc( 2 );

   if( pszSource && pszDest )
   {
      fResult = hb_fsCopy( pszSource, pszDest );
      errCode = hb_fsError();
   }
   else
   {
      hb_fsSetFError( 2 /* file not found */ );
      hb_retni( F_ERROR );
   }
   hb_fsSetFError( errCode );
   hb_retni( fResult ? 0 : F_ERROR );
}
