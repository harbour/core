/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MEMOWRIT()/MEMOREAD() functions
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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
         ULONG ulSize = hb_fsSeek( fhnd, -( ( LONG ) sizeof( BYTE ) ), FS_END ) + sizeof( BYTE );
         BYTE * pbyBuffer;

         /* Don't read the file terminating EOF character */

         #if ! defined(OS_UNIX_COMPATIBLE)
         {
            BYTE byEOF = HB_CHAR_NUL;

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

