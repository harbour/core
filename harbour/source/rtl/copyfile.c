/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __COPYFILE() function
 *
 * Copyright 1999 Andi Jahja <andij@aonlippo.co.id>
 * www - http://www.harbour-project.org
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

#if defined(OS_UNIX_COMPATIBLE)
   #include <sys/stat.h>
   #include <unistd.h>
#endif

#define BUFFER_SIZE 8192

static BOOL hb_fsCopy( char * szSource, char * szDest )
{
   BOOL bRetVal = FALSE;
   FHANDLE fhndSource;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsCopy(%s, %s)", szSource, szDest));

   while( ( fhndSource = hb_fsOpen( ( BYTE * ) szSource, FO_READ | FO_SHARED | FO_PRIVATE ) ) == FS_ERROR )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2012, NULL, szSource, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
         break;
   }

   if( fhndSource != FS_ERROR )
   {
      FHANDLE fhndDest;

      while( ( fhndDest = hb_fsCreate( ( BYTE * ) szDest, FC_NORMAL ) ) == FS_ERROR )
      {
         USHORT uiAction = hb_errRT_BASE_Ext1( EG_CREATE, 2012, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }

      if( fhndDest != FS_ERROR )
      {
#if defined(OS_UNIX_COMPATIBLE)
         struct stat struFileInfo;
         int iSuccess = fstat( fhndSource, &struFileInfo );
#endif
         BYTE * buffer;
         USHORT usRead;

         buffer = ( BYTE * ) hb_xgrab( BUFFER_SIZE );

         bRetVal = TRUE;

         while( ( usRead = hb_fsRead( fhndSource, buffer, BUFFER_SIZE ) ) != 0 )
         {
            while( hb_fsWrite( fhndDest, buffer, usRead ) != usRead )
            {
               USHORT uiAction = hb_errRT_BASE_Ext1( EG_WRITE, 2016, NULL, szDest, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 0 );

               if( uiAction == E_DEFAULT || uiAction == E_BREAK )
               {
                  bRetVal = FALSE;
                  break;
               }
            }
         }

         hb_xfree( buffer );

#if defined(OS_UNIX_COMPATIBLE)
         if( iSuccess == 0 )
            fchmod( fhndDest, struFileInfo.st_mode );
#endif

         hb_fsClose( fhndDest );
      }

      hb_fsClose( fhndSource );
   }

   return bRetVal;
}

/* Clipper returns .F. on failure and NIL on success */

HB_FUNC( __COPYFILE )
{
   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
   {
      if( ! hb_fsCopy( hb_parc( 1 ), hb_parc( 2 ) ) )
         hb_retl( FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, "__COPYFILE", 2, hb_paramError( 1 ), hb_paramError( 2 ) ); /* NOTE: Undocumented but existing Clipper Run-time error */
}
