/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DIRECTORY() function
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

/*
 * Notes from the fringe... <ptucker@sympatico.ca>
 *
 * Clipper is a bit schizoid with the treatment of file attributes, but we've
 * emulated that weirdness here for your viewing amusement.
 *
 * In Clippers' homeworld of DOS, there are 5 basic attributes: 'A'rchive,
 * 'H'idden, 'S'ystem, 'R'eadonly and 'D'irectory.  In addition, a file can
 * have no attributes, and only 1 file per physical partition can have the
 * 'V'olume label.
 *
 * For a given file request, it is implied that the attribute mask includes
 * all attributes except 'H'idden, 'S'ystem and 'D'irectory and 'V'olume.
 * The returned file list will always include (for instance) 'R'eadOnly files
 * unless they also happen to be 'H'idden and that attribute was not requested.
 *
 * The exception is Directory entries - these will always be excluded
 * even if they have the requested bit set. (Unless of course, you request "D"
 * as an attribute as well)
 *
 * The only valid characters that can be passed as an attribute request then,
 * are any of "DHSV". Anything else is already implied, so it is ignored. "V"
 * is a special case - you will get back the entry that describes the volume
 * label for the drive implied by the filemask.
 *
 * Differences from the 'standard':
 * - Where supported, filenames will be returned in the same case as they
 *   are stored in the directory.  Clipper (and VO too) will convert the
 *   names to upper case.
 * - Where supported, filenames will be the full filename as supported by
 *   the OS in use.
 * - Where supported, there are a number of additional file attributes
 *   returned.  They are:
 *       'I' - DEVICE      File is a device
 *       'T' - TEMPORARY   File is a Temporary file
 *       'P' - SPARSE      File is Sparse
 *       'L' - REPARSE     File/Dir is a reparse point
 *       'C' - COMPRESSED  File/Dir is compressed
 *       'O' - OFFLINE     File/Dir is not online
 *       'X' - NOTINDEXED  Exclude File/Dir from Indexing Service
 *       'E' - ENCRYPTED   File/Dir is Encrypted
 *       'M' - VOLCOMP     Volume Supports Compression
 *
 * TODO: - Under an MS Windows implimentation, an optional 3rd parameter to
 *         Directory to allow you to receive the compatible '8.3' filename.
 *       - check that path support vis stat works on all platforms
 *       - UNC Support? ie: dir \\myserver\root
 *
 * TOFIX:- Volume label support
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#include "directry.ch"

#if defined( HB_OS_UNIX )
   #define HB_DIR_ALL_FILES_MASK        "*"
#else
   #define HB_DIR_ALL_FILES_MASK        "*.*"
#endif

HB_FUNC( DIRECTORY )
{
   PHB_ITEM  pDirSpec = hb_param( 1, HB_IT_STRING );
   PHB_ITEM  pAttributes = hb_param( 2, HB_IT_STRING );
   USHORT    uiMask;

/*
#if defined(__MINGW32__) || ( defined(_MSC_VER) && _MSC_VER >= 910 )
   PHB_ITEM pEightDotThree = hb_param( 3, HB_IT_LOGICAL );
   BOOL     bEightDotThree;

   // Do we want 8.3 support?
   bEightDotThree = ( pEightDotThree ? hb_itemGetL( pEightDotThree ) : FALSE );
#endif
*/

   PHB_ITEM  pDir = hb_itemArrayNew( 0 );

   PHB_FFIND ffind;

   /* Get the passed attributes and convert them to Harbour Flags */

   uiMask = HB_FA_ARCHIVE
          | HB_FA_DEVICE
          | HB_FA_TEMPORARY
          | HB_FA_SPARSE
          | HB_FA_REPARSE
          | HB_FA_COMPRESSED
          | HB_FA_OFFLINE
          | HB_FA_NOTINDEXED
          | HB_FA_ENCRYPTED
          | HB_FA_VOLCOMP;

   if( pAttributes && hb_itemGetCLen( pAttributes ) > 0 )
      if ( ( uiMask |= hb_fsAttrEncode( hb_itemGetCPtr( pAttributes ) ) ) & HB_FA_LABEL )
         uiMask = HB_FA_LABEL;

   /* Get the file list */

   if( ( ffind = hb_fsFindFirst( pDirSpec ? hb_itemGetCPtr( pDirSpec ) : HB_DIR_ALL_FILES_MASK, uiMask ) ) != NULL )
   {
      PHB_ITEM pFilename = hb_itemNew( NULL );
      PHB_ITEM pSize = hb_itemNew( NULL );
      PHB_ITEM pDate = hb_itemNew( NULL );
      PHB_ITEM pTime = hb_itemNew( NULL );
      PHB_ITEM pAttr = hb_itemNew( NULL );

      do
      {
         if( !( ( ( uiMask & HB_FA_HIDDEN    ) == 0 && ( ffind->attr & HB_FA_HIDDEN    ) != 0 ) ||
                ( ( uiMask & HB_FA_SYSTEM    ) == 0 && ( ffind->attr & HB_FA_SYSTEM    ) != 0 ) ||
                ( ( uiMask & HB_FA_LABEL     ) == 0 && ( ffind->attr & HB_FA_LABEL     ) != 0 ) ||
                ( ( uiMask & HB_FA_DIRECTORY ) == 0 && ( ffind->attr & HB_FA_DIRECTORY ) != 0 ) ) )
         {
            PHB_ITEM pSubarray = hb_itemArrayNew( F_LEN );
            char buffer[ 32 ];

            hb_arraySet( pSubarray, F_NAME, hb_itemPutC( pFilename, ffind->szName ) );
            hb_arraySet( pSubarray, F_SIZE, hb_itemPutNL( pSize, ffind->size ) );
            hb_arraySet( pSubarray, F_DATE, hb_itemPutDL( pDate, ffind->lDate ) );
            hb_arraySet( pSubarray, F_TIME, hb_itemPutC( pTime, ffind->szTime ) );
            hb_arraySet( pSubarray, F_ATTR, hb_itemPutC( pAttr, hb_fsAttrDecode( ffind->attr, buffer ) ) );

            /* Don't exit when array limit is reached */
            hb_arrayAdd( pDir, pSubarray );

            hb_itemRelease( pSubarray );
         }
      }
      while( hb_fsFindNext( ffind ) );

      hb_itemRelease( pFilename );
      hb_itemRelease( pSize );
      hb_itemRelease( pDate );
      hb_itemRelease( pTime );
      hb_itemRelease( pAttr );

      hb_fsFindClose( ffind );
   }

   hb_itemRelease( hb_itemReturn( pDir ) );
}

