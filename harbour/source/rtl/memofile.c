/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MEMOWRIT()/MEMOREAD() functions
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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
#include "hbapiitm.h"
#include "hbapifs.h"

/* NOTE: CA-Clipper has 64K (65516 bytes exactly) limit on read, in Harbour
         this limit is extended, so we are not *strictly* compatible here.
         [vszakats] */

HB_FUNC( MEMOREAD )
{
   PHB_ITEM pFileName = hb_param( 1, HB_IT_STRING );

   if( pFileName )
   {
      FHANDLE fhnd = hb_fsOpen( ( BYTE * ) hb_itemGetCPtr( pFileName ), FO_READ | FO_SHARED | FO_PRIVATE );

      if( fhnd != FS_ERROR )
      {
         ULONG ulSize = hb_fsSeek( fhnd, 0, FS_END );

         if( ulSize != 0 )
         {
            BYTE * pbyBuffer;

            /* Don't read the file terminating EOF character */

            #if ! defined(OS_UNIX_COMPATIBLE)
            {
               BYTE byEOF = HB_CHAR_NUL;

               hb_fsSeek( fhnd, -1, FS_END );
               hb_fsRead( fhnd, &byEOF, sizeof( BYTE ) );

               if( byEOF == HB_CHAR_EOF )
                  ulSize--;
            }
            #endif

            pbyBuffer = ( BYTE * ) hb_xgrab( ulSize + sizeof( char ) );

            hb_fsSeek( fhnd, 0, FS_SET );
            hb_fsReadLarge( fhnd, pbyBuffer, ulSize );

            hb_fsClose( fhnd );

            hb_itemPutCPtr( hb_itemReturnPtr(), ( char * ) pbyBuffer, ulSize );
         }
         else
            hb_retc( "" );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}

HB_FUNC( MEMOWRIT )
{
   PHB_ITEM pFileName = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pString = hb_param( 2, HB_IT_STRING );
   BOOL bRetVal = FALSE;

   if( pFileName && pString )
   {
      FHANDLE fhnd = hb_fsCreate( ( BYTE * ) hb_itemGetCPtr( pFileName ), FC_NORMAL );

      if( fhnd != FS_ERROR )
      {
         ULONG ulSize = hb_itemGetCLen( pString );

         bRetVal = ( hb_fsWriteLarge( fhnd, ( BYTE * ) hb_itemGetCPtr( pString ), ulSize ) == ulSize );

         /* NOTE: CA-Clipper will add the EOF even if the write failed. [vszakats] */
         /* NOTE: CA-Clipper will not return .F. when the EOF could not be written. [vszakats] */
         #if ! defined(OS_UNIX_COMPATIBLE)
         {
            BYTE byEOF = HB_CHAR_EOF;

            hb_fsWrite( fhnd, &byEOF, sizeof( BYTE ) );
         }
         #endif

         hb_fsClose( fhnd );
      }
   }

   hb_retl( bRetVal );
}
