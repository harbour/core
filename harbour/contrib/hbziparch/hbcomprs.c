/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DIRECTORY() related functions
 *
 * Copyright 1999 Leslee Griffith <les.griffith@vantagesystems.ca>
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
#include "hbapierr.h"

#include "directry.ch"

static void hb_fsGrabDirectory( PHB_ITEM pDir, const char * szDirSpec, USHORT uiMask, PHB_FNAME fDirSpec, BOOL bFullPath, BOOL bDirOnly )
{
   PHB_FFIND ffind;

   /* Get the file list */
   if( ( ffind = hb_fsFindFirst( szDirSpec, uiMask ) ) != NULL )
   {
      PHB_ITEM pSubarray;

      pSubarray = hb_itemNew( NULL );

      do
      {
         if( ( !bDirOnly || ( ffind->attr & HB_FA_DIRECTORY ) != 0 ) &&
             !( ( ( uiMask & HB_FA_HIDDEN    ) == 0 && ( ffind->attr & HB_FA_HIDDEN    ) != 0 ) ||
                ( ( uiMask & HB_FA_SYSTEM    ) == 0 && ( ffind->attr & HB_FA_SYSTEM    ) != 0 ) ||
                ( ( uiMask & HB_FA_LABEL     ) == 0 && ( ffind->attr & HB_FA_LABEL     ) != 0 ) ||
                ( ( uiMask & HB_FA_DIRECTORY ) == 0 && ( ffind->attr & HB_FA_DIRECTORY ) != 0 ) ) )
         {
            char buffer[ 32 ];

            hb_arrayNew( pSubarray, 5 );
            if ( bFullPath )
            {
               char *szFullName = hb_xstrcpy(NULL,fDirSpec->szPath?fDirSpec->szPath:"",ffind->szName,NULL);
               hb_itemPutC( hb_arrayGetItemPtr( pSubarray, F_NAME), szFullName );
               hb_xfree( szFullName );
            }
            else
               hb_itemPutC( hb_arrayGetItemPtr( pSubarray, F_NAME), ffind->szName );

            hb_itemPutNInt( hb_arrayGetItemPtr( pSubarray, F_SIZE ), ffind->size );
            hb_itemPutDL( hb_arrayGetItemPtr( pSubarray, F_DATE ), ffind->lDate );
            hb_itemPutC( hb_arrayGetItemPtr( pSubarray, F_TIME ), ffind->szTime );
            hb_itemPutC( hb_arrayGetItemPtr( pSubarray, F_ATTR ), hb_fsAttrDecode( ffind->attr, buffer ) );

            if( !bDirOnly || ( ffind->attr & HB_FA_DIRECTORY ) != 0 )
               hb_arrayAddForward( pDir, pSubarray );
         }
      }
      while( hb_fsFindNext( ffind ) );

      hb_itemRelease( pSubarray );
      hb_fsFindClose( ffind );
   }
}

void HB_EXPORT hb_fsDirectory( PHB_ITEM pDir, char* szSkleton, char* szAttributes, BOOL bDirOnly, BOOL bFullPath )
{
   USHORT uiMask;
   USHORT uiMaskNoLabel;
   BYTE      *szDirSpec;

/*
#if defined(__MINGW32__) || ( defined(_MSC_VER) && _MSC_VER >= 910 )
   PHB_ITEM pEightDotThree = hb_param( 3, HB_IT_LOGICAL );
   BOOL     bEightDotThree = pEightDotThree ? hb_itemGetL( pEightDotThree ) : FALSE; // Do we want 8.3 support?
#endif
*/

   PHB_FNAME pDirSpec = NULL;
   BOOL bAlloc = FALSE;

   /* Get the passed attributes and convert them to Harbour Flags */
   uiMask = HB_FA_ARCHIVE
          | HB_FA_READONLY
          | HB_FA_NORMAL
          | HB_FA_DEVICE
          | HB_FA_TEMPORARY
          | HB_FA_SPARSE
          | HB_FA_REPARSE
          | HB_FA_COMPRESSED
          | HB_FA_OFFLINE
          | HB_FA_NOTINDEXED
          | HB_FA_ENCRYPTED
          | HB_FA_VOLCOMP;

   uiMaskNoLabel = uiMask;

   hb_arrayNew( pDir, 0 );

   if ( bDirOnly )
      szAttributes = "D";

   if( szAttributes && strlen( szAttributes ) > 0 )
   {
      if ( ( uiMask |= hb_fsAttrEncode( szAttributes ) ) & HB_FA_LABEL )
      {
         /* NOTE: This is Clipper Doc compatible. (not operationally) */
         uiMask = HB_FA_LABEL;
      }
   }

   if ( szSkleton && strlen( szSkleton ) > 0 )
      szDirSpec = hb_fsNameConv( ( BYTE * ) szSkleton, &bAlloc );
   else
      szDirSpec = (BYTE *) OS_FILE_MASK;

   if( bDirOnly || bFullPath )
   {
      if ( ( pDirSpec = hb_fsFNameSplit( (char*) szDirSpec ) ) !=NULL )
      {
         if( pDirSpec->szDrive )
            hb_fsChDrv( (BYTE) ( pDirSpec->szDrive[0] - 'A' ) );

         if( pDirSpec->szPath )
            hb_fsChDir( (BYTE *) pDirSpec->szPath );
      }
   }

   /* Get the file list */
   hb_fsGrabDirectory( pDir, (const char*) szDirSpec, uiMask, pDirSpec, bFullPath, bDirOnly );

   if ( uiMask == HB_FA_LABEL )
   {
      uiMaskNoLabel |= hb_fsAttrEncode( szAttributes );
      uiMaskNoLabel &= ~HB_FA_LABEL;
      hb_fsGrabDirectory( pDir, (const char*) szDirSpec, uiMaskNoLabel, pDirSpec, bFullPath, bDirOnly );
   }

   if( pDirSpec )
      hb_xfree( pDirSpec );

   if( bAlloc )
      hb_xfree( szDirSpec );
}
