/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_I18N low-level Harbour functions
 *
 * Copyright 2008 Viktor Szakats <harbour.01 syenar.hu>
 * www - http://www.harbour-project.org
 * Loosely based on work by:
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapierr.h"

static const BYTE s_szHead[ 5 ] = { 193, 'H', 'B', 'L', '\0' };

#define HB_I18N_COMMENT_SIZE 64 /* NOTE: Must be larger than 4 bytes. */

static BOOL hb_i18n_save( HB_FHANDLE handle, char * pszComment, PHB_ITEM pTable )
{
   ULONG i, j;
   BYTE buffer[ HB_I18N_COMMENT_SIZE ];

   memset( buffer, 0, sizeof( buffer ) );

   hb_strncpy( ( char * ) buffer, pszComment, sizeof( buffer ) - 1 );

   if( hb_fsWrite( handle, ( BYTE * ) s_szHead, sizeof( s_szHead ) ) != sizeof( s_szHead ) || 
       hb_fsWrite( handle, buffer, sizeof( buffer ) ) != sizeof( buffer ) )
      return FALSE;

   HB_PUT_LE_UINT32( buffer, ( UINT32 ) hb_arrayLen( pTable ) );

   if( hb_fsWrite( handle, buffer, 4 ) != 4 )
      return FALSE;

   for( i = 1; i <= hb_arrayLen( pTable ); i++ )
   {
      PHB_ITEM pRow = hb_arrayGetItemPtr( pTable, i );

      for( j = 1; j <= 2; j++ )
      {
         USHORT nStrLen = ( USHORT ) hb_arrayGetCLen( pRow, j ) + 1; /* including trailing 0 */

         HB_PUT_LE_UINT16( buffer, ( UINT16 ) nStrLen );

         if( hb_fsWrite( handle, ( BYTE * ) buffer, 2 ) != 2 || 
             hb_fsWrite( handle, ( BYTE * ) hb_arrayGetCPtr( pRow, j ), nStrLen ) != nStrLen )
            return FALSE;
      }
   }

   return TRUE;
}

/* Saves a table to disk. aSortedTable must be sorted by original text.
   __I18N_SAVE( cFileName | nHandle, aSortedTable [, cComment ] ) => lSuccess */
HB_FUNC( __I18N_SAVE )
{
   if( ISCHAR( 1 ) )
   {
      HB_FHANDLE handle = hb_fsCreate( ( BYTE * ) hb_parc( 1 ), FC_NORMAL );
   
      if( handle != FS_ERROR )
      {
         hb_retl( hb_i18n_save( handle, hb_parcx( 3 ), hb_param( 2, HB_IT_ARRAY ) ) );
         hb_fsClose( handle );
      }
      else
         hb_retl( FALSE );
   }
   else if( ISNUM( 1 ) )
      hb_retl( hb_i18n_save( hb_numToHandle( hb_parnint( 1 ) ), hb_parcx( 3 ), hb_param( 2, HB_IT_ARRAY ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static BOOL hb_i18n_memread( BYTE * memory, ULONG memsize, BYTE ** buffer, ULONG nSize, ULONG * offset )
{
   if( *offset + nSize <= memsize )
   {
      *buffer = memory + *offset;
      *offset += nSize;
      return TRUE;
   }

   return FALSE;
}

static PHB_ITEM hb_i18n_load_from_memory( BYTE * memory, ULONG memsize )
{
   PHB_ITEM pTable = NULL;
   BYTE * buffer;
   ULONG offset = 0;

   if( hb_i18n_memread( memory, memsize, &buffer, sizeof( s_szHead ), &offset ) &&
       memcmp( buffer, s_szHead, sizeof( s_szHead ) ) == 0 &&
       hb_i18n_memread( memory, memsize, &buffer, HB_I18N_COMMENT_SIZE, &offset ) &&
       hb_i18n_memread( memory, memsize, &buffer, 4, &offset ) )
   {
      ULONG count = ( ULONG ) HB_GET_LE_UINT32( buffer );
      ULONG i, j;
      
      pTable = hb_itemArrayNew( count << 1 );

      for( i = 0; i < count; i++ )
      {
         for( j = 1; j <= 2; j++ )
         {
            if( hb_i18n_memread( memory, memsize, &buffer, 2, &offset ) )
            {
               USHORT nStrLen = ( USHORT ) HB_GET_LE_UINT16( buffer );
              
               if( nStrLen > 0 && hb_i18n_memread( memory, memsize, &buffer, nStrLen, &offset ) && buffer[ nStrLen - 1 ] == '\0' )
               {
                  hb_arraySetCL( pTable, ( i << 1 ) + j, ( char * ) buffer, nStrLen - 1 );
                  continue;
               }
            }

            hb_itemRelease( pTable );
            pTable = NULL;
         }
      }
   }

   return pTable;
}

/* Loads a table from memory into a flat array.
   __I18N_LOADFROMMEMORY( cBuffer ) => trs */
HB_FUNC( __I18N_LOADFROMMEMORY )
{
   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pTable = hb_i18n_load_from_memory( ( BYTE * ) hb_parc( 1 ), hb_parclen( 1 ) );

      if( pTable )
         hb_itemReturnRelease( pTable );
      else
         hb_reta( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static PHB_ITEM hb_i18n_load( PHB_FILE file )
{
   PHB_ITEM pTable = NULL;
   BYTE buffer[ HB_I18N_COMMENT_SIZE ];
   HB_FOFFSET offset = 0;

   if( hb_fileReadAt( file, buffer, sizeof( s_szHead ), offset ) == sizeof( s_szHead ) &&
       memcmp( buffer, s_szHead, sizeof( s_szHead ) ) == 0 &&
       hb_fileReadAt( file, buffer, sizeof( buffer ), offset += sizeof( s_szHead ) ) == sizeof( buffer ) &&
       hb_fileReadAt( file, buffer, 4, offset += sizeof( buffer ) ) == 4 )
   {
      ULONG count = ( ULONG ) HB_GET_LE_UINT32( buffer );
      ULONG i, j;

      offset += 4;
      
      pTable = hb_itemArrayNew( count << 1 );
      
      for( i = 0; i < count; i++ )
      {
         for( j = 1; j <= 2; j++ )
         {
            if( hb_fileReadAt( file, buffer, 2, offset ) == 2 )
            {
               USHORT nStrLen = ( USHORT ) HB_GET_LE_UINT16( buffer );

               offset += 2;
              
               if( nStrLen > 0 )
               {
                  BYTE * string = ( BYTE * ) hb_xgrab( nStrLen );
              
                  if( hb_fileReadAt( file, string, nStrLen, offset ) == nStrLen && string[ nStrLen - 1 ] == '\0' )
                  {
                     offset += nStrLen;
                     hb_arraySetCPtr( pTable, ( i << 1 ) + j, ( char * ) string, nStrLen - 1 );
                     continue;
                  }
                  else
                     hb_xfree( string );
               }
            }
      
            hb_itemRelease( pTable );
            pTable = NULL;
         }
      }
   }

   return pTable;
}

/* Loads a table in a flat array.
   __I18N_LOAD( cFileName ) => trs */
HB_FUNC( __I18N_LOAD )
{
   if( ISCHAR( 1 ) )
   {
      PHB_ITEM pTable = NULL;
      PHB_FILE file = hb_fileExtOpen( ( BYTE * ) hb_parc( 1 ), NULL, FO_READ | FO_DENYNONE, NULL, NULL );

      if( file )
      {
         pTable = hb_i18n_load( file );
         hb_fileClose( file );
      }

      if( pTable )
         hb_itemReturnRelease( pTable );
      else
         hb_reta( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Translate an Harbour string or return it untranslated
   __I18N_GETTEXT( @cText, trs ) => NIL */
HB_FUNC( __I18N_GETTEXT )
{
   PHB_ITEM pFind = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pTable = hb_param( 2, HB_IT_ARRAY );

   if( pFind && pTable )
   {
      char * pszFind = hb_itemGetCPtr( pFind );

      ULONG nLow = 1;
      ULONG nHigh = ( hb_arrayLen( pTable ) >> 1 );
      ULONG nMiddle;

      while( nLow <= nHigh )
      {
         int result = strcmp( hb_arrayGetCPtr( pTable, ( ( nMiddle = ( nLow + nHigh ) / 2 ) << 1 ) - 1 ), pszFind );

         if( result == 0 )
         {
            hb_itemParamStore( 1, hb_arrayGetItemPtr( pTable, nMiddle << 1 ) );
            break;
         }
         else if( result > 0 )
            nHigh = nMiddle - 1;
         else
            nLow = nMiddle + 1;
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
