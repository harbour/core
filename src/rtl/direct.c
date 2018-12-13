/*
 * Directory() function
 *
 * Copyright 1999 Leslee Griffith <les.griffith@vantagesystems.ca>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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
 * In Clippers' home world of MS-DOS, there are 5 basic attributes: 'A'rchive,
 * 'H'idden, 'S'ystem, 'R'eadonly and 'D'irectory.  In addition, a file can
 * have no attributes, and only 1 file per physical partition can have the
 * 'V'olume label.
 *
 * For a given file request, it is implied that the attribute mask includes
 * all attributes except 'H'idden, 'S'ystem, 'D'irectory and 'V'olume.
 * The returned file list will always include (for instance) 'R'eadOnly files
 * unless they also happen to be 'H'idden and that attribute was not requested.
 *
 * "V" is a special case - you will get back the entry that describes the
 * volume label for the drive implied by the file mask.
 *
 * Differences from the 'standard' (where supported):
 * - Filenames will be returned in the same case as they are stored in the
 *   directory.  Clipper (and VO too) will convert the names to upper case
 * - Filenames will be the full filename as supported by the OS in use.
 * - There are a number of additional file attributes returned.
 *   They are:
 *       'I' - DEVICE      File is a device
 *       'T' - TEMPORARY   File is a Temporary file
 *       'P' - SPARSE      File is Sparse
 *       'L' - REPARSE     File/Dir is a reparse point
 *       'C' - COMPRESSED  File/Dir is compressed
 *       'O' - OFFLINE     File/Dir is not online
 *       'X' - NOTINDEXED  Exclude File/Dir from Indexing Service
 *       'E' - ENCRYPTED   File/Dir is Encrypted
 *       'M' - VOLCOMP     Volume Supports Compression
 * - Clipper can sometimes drop the ReadOnly indication of directories.
 *   Harbour detects this correctly.
 *
 * TODO: - check that path support vis stat works on all platforms
 *       - UNC Support? ie: dir \\myserver\root
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#include "directry.ch"

/* NOTE: 8.3 support should be added in a separate way, like
         as a function which converts full names to 8.3 names, since
         this issue is very much platform specific, and this is
         not the only place which may need the conversion [vszakats]. */

PHB_ITEM hb_fsDirectory( const char * pszDirSpec, const char * pszAttributes, HB_BOOL fDateTime )
{
   PHB_ITEM  pDir = hb_itemArrayNew( 0 );
   char *    pszFree = NULL;
   PHB_FFIND ffind;
   HB_FATTR  ulMask;

   /* Get the passed attributes and convert them to Harbour Flags */

   ulMask = HB_FA_ARCHIVE | HB_FA_READONLY;

   if( pszAttributes && *pszAttributes )
      ulMask |= hb_fsAttrEncode( pszAttributes );

   if( pszDirSpec && *pszDirSpec )
   {
      if( ulMask != HB_FA_LABEL )
      {
         /* CA-Cl*pper compatible behavior - add all file mask when
          * last character is directory or drive separator
          */
         HB_SIZE nLen = strlen( pszDirSpec ) - 1;
#ifdef HB_OS_HAS_DRIVE_LETTER
         if( pszDirSpec[ nLen ] == HB_OS_PATH_DELIM_CHR ||
             pszDirSpec[ nLen ] == HB_OS_DRIVE_DELIM_CHR )
#else
         if( pszDirSpec[ nLen ] == HB_OS_PATH_DELIM_CHR )
#endif
            pszDirSpec = pszFree =
                           hb_xstrcpy( NULL, pszDirSpec, HB_OS_ALLFILE_MASK, NULL );
      }
   }
   else
      pszDirSpec = HB_OS_ALLFILE_MASK;

   /* Get the file list */

   if( ( ffind = hb_fsFindFirst( pszDirSpec, ulMask ) ) != NULL )
   {
      PHB_ITEM pSubarray = hb_itemNew( NULL );

      do
      {
         char buffer[ 32 ];

         hb_arrayNew    ( pSubarray, F_LEN );
         hb_arraySetC   ( pSubarray, F_NAME, ffind->szName );
         hb_arraySetNInt( pSubarray, F_SIZE, ffind->size );
         hb_arraySetC   ( pSubarray, F_TIME, ffind->szTime );
         hb_arraySetC   ( pSubarray, F_ATTR, hb_fsAttrDecode( ffind->attr, buffer ) );

         if( fDateTime )
            hb_arraySetTDT( pSubarray, F_DATE, ffind->lDate, ffind->lTime );
         else
            hb_arraySetDL ( pSubarray, F_DATE, ffind->lDate );

         /* Don't exit when array limit is reached */
         hb_arrayAddForward( pDir, pSubarray );
      }
      while( hb_fsFindNext( ffind ) );

      hb_itemRelease( pSubarray );

      hb_fsFindClose( ffind );
   }

   if( pszFree )
      hb_xfree( pszFree );

   return pDir;
}

HB_FUNC( DIRECTORY )
{
   hb_itemReturnRelease( hb_fsDirectory( hb_parc( 1 ), hb_parc( 2 ), HB_FALSE ) );
}

HB_FUNC( HB_DIRECTORY )
{
   hb_itemReturnRelease( hb_fsDirectory( hb_parc( 1 ), hb_parc( 2 ), HB_TRUE ) );
}
