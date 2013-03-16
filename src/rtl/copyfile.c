/*
 * Harbour Project source code:
 * __CopyFile() function
 *
 * Copyright 1999 Andi Jahja <andij@aonlippo.co.id>
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapifs.h"

#if defined( HB_OS_UNIX )
   #include <sys/stat.h>
   #include <unistd.h>
#endif

#define BUFFER_SIZE  8192

static HB_BOOL hb_copyfile( const char * szSource, const char * szDest )
{
   HB_BOOL bRetVal = HB_FALSE;
   HB_FHANDLE fhndSource;
   PHB_ITEM pError = NULL;

   HB_TRACE( HB_TR_DEBUG, ( "hb_copyfile(%s, %s)", szSource, szDest ) );

   do
   {
      fhndSource = hb_fsExtOpen( szSource, NULL,
                                 FO_READ | FXO_DEFAULTS | FXO_SHARELOCK,
                                 NULL, pError );
      if( fhndSource == FS_ERROR )
      {
         pError = hb_errRT_FileError( pError, NULL, EG_OPEN, 2012, szSource );
         if( hb_errLaunch( pError ) != E_RETRY )
            break;
      }
   }
   while( fhndSource == FS_ERROR );

   if( fhndSource != FS_ERROR )
   {
      HB_FHANDLE fhndDest;

      do
      {
         fhndDest = hb_fsExtOpen( szDest, NULL,
                                  FXO_TRUNCATE | FO_READWRITE | FO_EXCLUSIVE |
                                  FXO_DEFAULTS | FXO_SHARELOCK,
                                  NULL, pError );
         if( fhndDest == FS_ERROR )
         {
            pError = hb_errRT_FileError( pError, NULL, EG_CREATE, 2012, szDest );
            if( hb_errLaunch( pError ) != E_RETRY )
               break;
         }
      }
      while( fhndDest == FS_ERROR );

      if( fhndDest != FS_ERROR )
      {
#if defined( HB_OS_UNIX )
         struct stat struFileInfo;
         int iSuccess = fstat( fhndSource, &struFileInfo );
#endif
         void * buffer;
         HB_USHORT usRead;

         buffer = hb_xgrab( BUFFER_SIZE );

         bRetVal = HB_TRUE;

         while( ( usRead = hb_fsRead( fhndSource, buffer, BUFFER_SIZE ) ) != 0 )
         {
            while( hb_fsWrite( fhndDest, buffer, usRead ) != usRead )
            {
               pError = hb_errRT_FileError( pError, NULL, EG_WRITE, 2016, szDest );
               if( hb_errLaunch( pError ) != E_RETRY )
               {
                  bRetVal = HB_FALSE;
                  break;
               }
            }
         }

         hb_xfree( buffer );

#if defined( HB_OS_UNIX )
         if( iSuccess == 0 )
            fchmod( fhndDest, struFileInfo.st_mode );
#endif

         hb_fsClose( fhndDest );
      }

      hb_fsClose( fhndSource );
   }

   if( pError )
      hb_itemRelease( pError );

   return bRetVal;
}

/* Clipper returns .F. on failure and NIL on success */

HB_FUNC( __COPYFILE )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      if( ! hb_copyfile( hb_parc( 1 ), hb_parc( 2 ) ) )
         hb_retl( HB_FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );  /* NOTE: Undocumented but existing Clipper Run-time error */
}
