/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Set functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapilng.h"
#include "hbset.h"

extern hb_vm_bIdleRepeat;

HB_SET_STRUCT hb_set;

static BOOL set_logical( PHB_ITEM pItem )
{
   BOOL bLogical = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("set_logical(%p)", pItem));

   if( HB_IS_LOGICAL( pItem ) )
      bLogical = hb_itemGetL( pItem );
   else if( HB_IS_STRING( pItem ) )
   {
      char * szString = hb_itemGetCPtr( pItem );
      ULONG ulLen = hb_itemGetCLen( pItem );

      if( ulLen >= 2
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'N' )
         bLogical = TRUE;
      else if( ulLen >= 3
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'F'
       && toupper( szString[ 2 ] ) == 'F' )
         bLogical = FALSE;
   }

   return bLogical;
}

static int set_number( PHB_ITEM pItem, int iOldValue )
{
   HB_TRACE(HB_TR_DEBUG, ("set_number(%p, %d)", pItem, iOldValue));

   if( HB_IS_NUMERIC( pItem ) )
      return hb_itemGetNI( pItem );
   else
      return iOldValue;
}

static char * set_string( PHB_ITEM pItem, char * szOldString )
{
   char * szString;

   HB_TRACE(HB_TR_DEBUG, ("set_string(%p, %s)", pItem, szOldString));

   if( HB_IS_STRING( pItem ) )
   {
      /* Limit size of SET strings to 64K, truncating if source is longer */
      ULONG ulLen = hb_itemGetCLen( pItem );

      if( ulLen > USHRT_MAX ) ulLen = USHRT_MAX;

      if( szOldString ) szString = ( char * ) hb_xrealloc( szOldString, ulLen + 1 );
      else szString = ( char * ) hb_xgrab( ulLen + 1 );

      memcpy( szString, hb_itemGetCPtr( pItem ), ulLen );
      szString[ ulLen ] = '\0';
   }
   else
      szString = szOldString;

   return szString;
}

static void close_binary( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_binary(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();
      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
   }
}

static void close_text( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_text(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();
      #if ! defined(OS_UNIX_COMPATIBLE)
         hb_fsWrite( handle, ( BYTE * ) "\x1A", 1 );
      #endif
      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
   }
}

static FHANDLE open_handle( char * file_name, BOOL bAppend, char * def_ext, HB_set_enum set_specifier )
{
   USHORT user_ferror;
   FHANDLE handle;
   PHB_FNAME pFilename;
   char path[ _POSIX_PATH_MAX + 1 ];

   HB_TRACE(HB_TR_DEBUG, ("open_handle(%s, %d, %s, %d)", file_name, (int) bAppend, def_ext, (int) set_specifier));

   user_ferror = hb_fsError(); /* Save the current user file error code */
   /* Create full filename */

   pFilename = hb_fsFNameSplit( file_name );

   if( ! pFilename->szPath && hb_set.HB_SET_DEFAULT )
      pFilename->szPath = hb_set.HB_SET_DEFAULT;
   if( ! pFilename->szExtension && def_ext )
      pFilename->szExtension = def_ext;

   hb_fsFNameMerge( path, pFilename );
   hb_xfree( pFilename );

   /* Open the file either in append (bAppend) or truncate mode (!bAppend), but
      always use binary mode */

   /* QUESTION: What sharing mode does Clipper use ? [vszakats] */

   handle = FS_ERROR;
   while( handle == FS_ERROR )
   {
      BOOL bCreate = FALSE;

      if( bAppend )
      {  /* Append mode */
         if( hb_fsFile( ( BYTE * ) path ) )
         {  /* If the file already exists, open it (in read-write mode, in
               case of non-Unix and text modes). */
            handle = hb_fsOpen( ( BYTE * ) path, FO_READWRITE | FO_DENYWRITE );
            if( handle != FS_ERROR )
            {  /* Position to EOF */
            #if ! defined(OS_UNIX_COMPATIBLE)
               /* Non-Unix needs special binary vs. text file handling */
               if( set_specifier == HB_SET_PRINTFILE )
               {  /* PRINTFILE is binary and needs no special handling. */
            #endif
                  hb_fsSeek( handle, 0, FS_END );
            #if ! defined(OS_UNIX_COMPATIBLE)
               }
               else
               {  /* All other files are text files and may have an EOF
                     ('\x1A') character at the end (non-UNIX only). */
                  char cEOF = '\0';
                  hb_fsSeek( handle, -1, FS_END ); /* Position to last char. */
                  hb_fsRead( handle, ( BYTE * ) &cEOF, 1 );   /* Read the last char. */
                  if( cEOF == '\x1A' )             /* If it's an EOF, */
                     hb_fsSeek( handle, -1, FS_END ); /* Then write over it. */
               }
            #endif
            }
         }
         else bCreate = TRUE; /* Otherwise create a new file. */
      }
      else bCreate = TRUE; /* Always create a new file for overwrite mode. */

      if( bCreate )
         handle = hb_fsCreate( ( BYTE * ) path, FC_NORMAL );

      if( handle == FS_ERROR )
      {
         USHORT uiAction;

         /* NOTE: using switch() here will result in a compiler warning.
                  [vszakats] */
         if( set_specifier == HB_SET_ALTFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2013, NULL, path, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else if( set_specifier == HB_SET_PRINTFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2014, NULL, path, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else if( set_specifier == HB_SET_EXTRAFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2015, NULL, path, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else
            uiAction = E_DEFAULT;

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }
   }
   hb_fsSetError( user_ferror ); /* Restore the current user file error code */
   return handle;
}

HB_FUNC( SETCANCEL )
{
   hb_retl( hb_set.HB_SET_CANCEL );

   if( ISLOG( 1 ) )
      hb_set.HB_SET_CANCEL = hb_parl( 1 );
}

HB_FUNC( __SETCENTURY )
{
   BOOL old_century_setting = hb_set.hb_set_century;

   /*
    * Change the setting if the parameter is a logical value, or is
    * either "ON" or "OFF" (regardless of case)
    */
   if( ISLOG( 1 ) )
      hb_set.hb_set_century = hb_parl( 1 );
   else if( ISCHAR( 1 ) )
   {
      char * szString = hb_parc( 1 );
      ULONG ulLen = hb_parclen( 1 );

      if( ulLen >= 2
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'N' )
         hb_set.hb_set_century = TRUE;
      else if( ulLen >= 3
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'F'
       && toupper( szString[ 2 ] ) == 'F' )
         hb_set.hb_set_century = FALSE;
   }

   /*
    * Finally, if the setting changed, adjust the current date format to use
    * the correct number of year digits.
    */
   if( old_century_setting != hb_set.hb_set_century )
   {
      int count, digit, size, y_size, y_start, y_stop;
      char * szDateFormat, * szNewFormat;

      /* Convert to upper case and determine where year is */
      y_start = y_stop = -1;
      szDateFormat = hb_set.HB_SET_DATEFORMAT;
      size = strlen( szDateFormat );
      for( count = 0; count < size; count++ )
      {
         digit = toupper( szDateFormat[ count ] );
         if( digit == 'Y' )
         {
            if( y_start == -1 ) y_start = count;
         }
         else if( y_start > -1 && y_stop == -1 ) y_stop = count;
         szDateFormat[ count ] = digit;
      }
      /* Determine size of year in current format */
      if( y_start < 0 )
      {
         y_start = 0; /* There is no year in the current format */
         y_stop = 0;
      }
      else if( y_stop < 0 ) y_stop = size; /* All digits are year digits */
      y_size = y_stop - y_start;
      /* Calculate size of new format */
      size -= y_size;
      if( hb_set.hb_set_century ) size += 4;
      else size += 2;

      /* Create the new date format */
      szNewFormat = ( char * ) hb_xgrab( size + 1 );

      {
         int format_len;
         if( y_start > 0 ) memcpy( szNewFormat, szDateFormat, y_start );
         szNewFormat[ y_start ] = '\0';
         strcat( szNewFormat, "YY" );
         if( hb_set.hb_set_century ) strcat( szNewFormat, "YY" );
         format_len = strlen( szDateFormat );
         if( y_stop < format_len ) strcat( szNewFormat, szDateFormat + y_stop );
         hb_xfree( szDateFormat );
         hb_set.HB_SET_DATEFORMAT = szNewFormat;
      }
   }

   /* Return the previous setting */
   hb_retl( old_century_setting );
}

HB_FUNC( SET )
{
   BOOL bFlag;
   int args = hb_pcount();

   HB_set_enum set_specifier = ( args > 0 ) ? ( HB_set_enum ) hb_parni( 1 ) : HB_SET_INVALID_;
   PHB_ITEM pArg2 = ( args > 1 ) ? hb_param( 2, HB_IT_ANY ) : NULL;
   PHB_ITEM pArg3 = ( args > 2 ) ? hb_param( 3, HB_IT_ANY ) : NULL;

   switch ( set_specifier )
   {
      case HB_SET_ALTERNATE  :
         hb_retl( hb_set.HB_SET_ALTERNATE );
         if( args > 1 ) hb_set.HB_SET_ALTERNATE = set_logical( pArg2 );
         break;
      case HB_SET_ALTFILE    :
         if( hb_set.HB_SET_ALTFILE ) hb_retc( hb_set.HB_SET_ALTFILE );
         else hb_retc( "" );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) ) hb_set.HB_SET_ALTFILE = set_string( pArg2, hb_set.HB_SET_ALTFILE );
         if( args > 2 ) bFlag = set_logical( pArg3 );
         else bFlag = FALSE;
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            close_text( hb_set.hb_set_althan );
            if( hb_set.HB_SET_ALTFILE && strlen( hb_set.HB_SET_ALTFILE ) > 0 )
               hb_set.hb_set_althan = open_handle( hb_set.HB_SET_ALTFILE, bFlag, ".txt", HB_SET_ALTFILE );
         }
         break;
      case HB_SET_AUTOPEN    :
         hb_retl( hb_set.HB_SET_AUTOPEN );
         if( args > 1 ) hb_set.HB_SET_AUTOPEN = set_logical( pArg2 );
         break;
      case HB_SET_AUTORDER   :
         hb_retl( hb_set.HB_SET_AUTORDER );
         if( args > 1 ) hb_set.HB_SET_AUTORDER = set_logical( pArg2 );
         break;
      case HB_SET_AUTOSHARE  :
         hb_retni( hb_set.HB_SET_AUTOSHARE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_AUTOSHARE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_MARGIN = set_number( pArg2, hb_set.HB_SET_AUTOSHARE );
         }
         break;
      case HB_SET_BELL       :
         hb_retl( hb_set.HB_SET_BELL );
         if( args > 1 ) hb_set.HB_SET_BELL = set_logical( pArg2 );
         break;
      case HB_SET_CANCEL     :
         hb_retl( hb_set.HB_SET_CANCEL );
         if( args > 1 ) hb_set.HB_SET_CANCEL = set_logical( pArg2 );
         break;
      case HB_SET_COLOR      :
         hb_retc( hb_conSetColor( args >= 2 && HB_IS_STRING( pArg2 ) ? hb_itemGetCPtr( pArg2 ) : ( char * ) NULL ) );
         break;
      case HB_SET_CONFIRM    :
         hb_retl( hb_set.HB_SET_CONFIRM );
         if( args > 1 ) hb_set.HB_SET_CONFIRM = set_logical( pArg2 );
         break;
      case HB_SET_CONSOLE    :
         hb_retl( hb_set.HB_SET_CONSOLE );
         if( args > 1 ) hb_set.HB_SET_CONSOLE = set_logical( pArg2 );
         break;
      case HB_SET_CURSOR     :
         if( args >= 2 && HB_IS_NUMERIC( pArg2 ) )
            hb_retni( hb_conSetCursor( TRUE, hb_itemGetNI( pArg2 ) ) );
         else
            hb_retni( hb_conSetCursor( FALSE, 0 ) );
         break;
      case HB_SET_DATEFORMAT :
         if( hb_set.HB_SET_DATEFORMAT ) hb_retc( hb_set.HB_SET_DATEFORMAT );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_DATEFORMAT = set_string( pArg2, hb_set.HB_SET_DATEFORMAT );
         break;
      case HB_SET_DEBUG      :
         hb_retl( hb_set.HB_SET_DEBUG );
         if( args > 1 ) hb_set.HB_SET_DEBUG = set_logical( pArg2 );
         break;
      case HB_SET_DECIMALS   :
         hb_retni( hb_set.HB_SET_DECIMALS );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_DECIMALS ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_DECIMALS = set_number( pArg2, hb_set.HB_SET_DECIMALS );
         }
         break;
      case HB_SET_DEFAULT    :
         if( hb_set.HB_SET_DEFAULT ) hb_retc( hb_set.HB_SET_DEFAULT );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_DEFAULT = set_string( pArg2, hb_set.HB_SET_DEFAULT );
         break;
      case HB_SET_DELETED    :
         hb_retl( hb_set.HB_SET_DELETED );
         if( args > 1 ) hb_set.HB_SET_DELETED = set_logical( pArg2 );
         break;
      case HB_SET_DELIMCHARS :
         if( hb_set.HB_SET_DELIMCHARS ) hb_retc( hb_set.HB_SET_DELIMCHARS );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_DELIMCHARS = set_string( pArg2, hb_set.HB_SET_DELIMCHARS );
         break;
      case HB_SET_DELIMITERS :
         hb_retl( hb_set.HB_SET_DELIMITERS );
         if( args > 1 ) hb_set.HB_SET_DELIMITERS = set_logical( pArg2 );
         break;
      case HB_SET_DEVICE     :
         if( hb_set.HB_SET_DEVICE ) hb_retc( hb_set.HB_SET_DEVICE );
         else hb_retc( "" );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            /* If the print file is not already open, open it in overwrite mode. */
            hb_set.HB_SET_DEVICE = set_string( pArg2, hb_set.HB_SET_DEVICE );
            if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set.hb_set_printhan == FS_ERROR
            && hb_set.HB_SET_PRINTFILE && strlen( hb_set.HB_SET_PRINTFILE ) > 0 )
               hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, FALSE, ".prn", HB_SET_PRINTFILE );
         }
         break;
      case HB_SET_EPOCH      :
         hb_retni( hb_set.HB_SET_EPOCH );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_EPOCH ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_EPOCH = set_number( pArg2, hb_set.HB_SET_EPOCH );
         }
         break;
      case HB_SET_ESCAPE     :
         hb_retl( hb_set.HB_SET_ESCAPE );
         if( args > 1 ) hb_set.HB_SET_ESCAPE = set_logical( pArg2 );
         break;
      case HB_SET_EVENTMASK  :
         hb_retni( hb_set.HB_SET_EVENTMASK );
         if( args > 1 ) hb_set.HB_SET_EVENTMASK = ( HB_inkey_enum ) set_number( pArg2, hb_set.HB_SET_EVENTMASK );
         break;
      case HB_SET_EXACT      :
         hb_retl( hb_set.HB_SET_EXACT );
         if( args > 1 ) hb_set.HB_SET_EXACT = set_logical( pArg2 );
         break;
      case HB_SET_EXCLUSIVE  :
         hb_retl( hb_set.HB_SET_EXCLUSIVE );
         if( args > 1 ) hb_set.HB_SET_EXCLUSIVE = set_logical( pArg2 );
         break;
      case HB_SET_EXIT       :
         hb_retl( hb_set.HB_SET_EXIT );
         if( args > 1 ) hb_set.HB_SET_EXIT = set_logical( pArg2 );
         break;
      case HB_SET_EXTRA      :
         hb_retl( hb_set.HB_SET_EXTRA );
         if( args > 1 ) hb_set.HB_SET_EXTRA = set_logical( pArg2 );
         break;
      case HB_SET_EXTRAFILE  :
         if( hb_set.HB_SET_EXTRAFILE ) hb_retc( hb_set.HB_SET_EXTRAFILE );
         else hb_retc( "" );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) ) hb_set.HB_SET_EXTRAFILE = set_string( pArg2, hb_set.HB_SET_EXTRAFILE );
         if( args > 2 ) bFlag = set_logical( pArg3 );
         else bFlag = FALSE;
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            close_text( hb_set.hb_set_extrahan );
            if( hb_set.HB_SET_EXTRAFILE && strlen( hb_set.HB_SET_EXTRAFILE ) > 0 )
               hb_set.hb_set_extrahan = open_handle( hb_set.HB_SET_EXTRAFILE, bFlag, ".prn", HB_SET_EXTRAFILE );
         }
         break;
      case HB_SET_FIXED      :
         hb_retl( hb_set.HB_SET_FIXED );
         if( args > 1 ) hb_set.HB_SET_FIXED = set_logical( pArg2 );
         break;
      case HB_SET_INSERT     :
         hb_retl( hb_set.HB_SET_INSERT );
         if( args > 1 ) hb_set.HB_SET_INSERT = set_logical( pArg2 );
         break;
      case HB_SET_INTENSITY  :
         hb_retl( hb_set.HB_SET_INTENSITY );
         if( args > 1 ) hb_set.HB_SET_INTENSITY = set_logical( pArg2 );
         break;
      case HB_SET_MARGIN     :
         hb_retni( hb_set.HB_SET_MARGIN );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MARGIN ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_MARGIN = set_number( pArg2, hb_set.HB_SET_MARGIN );
         }
         break;
      case HB_SET_MBLOCKSIZE :
         hb_retni( hb_set.HB_SET_MBLOCKSIZE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_MBLOCKSIZE = set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE );
         }
         break;
      case HB_SET_MCENTER    :
         hb_retl( hb_set.HB_SET_MCENTER );
         if( args > 1 ) hb_set.HB_SET_MCENTER = set_logical( pArg2 );
         break;
      case HB_SET_MESSAGE    :
         hb_retni( hb_set.HB_SET_MESSAGE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MESSAGE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_MESSAGE = set_number( pArg2, hb_set.HB_SET_MESSAGE );
         }
         break;
      case HB_SET_MFILEEXT   :
         if( hb_set.HB_SET_MFILEEXT ) hb_retc( hb_set.HB_SET_MFILEEXT );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_MFILEEXT = set_string( pArg2, hb_set.HB_SET_MFILEEXT );
         break;
      case HB_SET_OPTIMIZE   :
         hb_retl( hb_set.HB_SET_OPTIMIZE );
         if( args > 1 ) hb_set.HB_SET_OPTIMIZE = set_logical( pArg2 );
         break;
      case HB_SET_STRICTREAD :
         hb_retl( hb_set.HB_SET_STRICTREAD );
         if( args > 1 ) hb_set.HB_SET_STRICTREAD = set_logical( pArg2 );
         break;
      case HB_SET_PATH       :
         if( hb_set.HB_SET_PATH ) hb_retc( hb_set.HB_SET_PATH );
         else hb_retc( "" );
         if( args > 1 ) hb_set.HB_SET_PATH = set_string( pArg2, hb_set.HB_SET_PATH );
         break;
      case HB_SET_PRINTER    :
         hb_retl( hb_set.HB_SET_PRINTER );
         if( args > 1 ) hb_set.HB_SET_PRINTER = set_logical( pArg2 );
         break;
      case HB_SET_PRINTFILE  :
         if( hb_set.HB_SET_PRINTFILE ) hb_retc( hb_set.HB_SET_PRINTFILE );
         else hb_retc( "" );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) ) hb_set.HB_SET_PRINTFILE = set_string( pArg2, hb_set.HB_SET_PRINTFILE );
         if( args > 2 ) bFlag = set_logical( pArg3 );
         else bFlag = FALSE;
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            close_binary( hb_set.hb_set_printhan );
            if( hb_set.HB_SET_PRINTFILE && strlen( hb_set.HB_SET_PRINTFILE ) > 0 )
               hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, bFlag, ".prn", HB_SET_PRINTFILE );
         }
         break;
      case HB_SET_SCOREBOARD :
         hb_retl( hb_set.HB_SET_SCOREBOARD );
         if( args > 1 ) hb_set.HB_SET_SCOREBOARD = set_logical( pArg2 );
         break;
      case HB_SET_SCROLLBREAK:
         hb_retl( hb_set.HB_SET_SCROLLBREAK );
         if( args > 1 ) hb_set.HB_SET_SCROLLBREAK = set_logical( pArg2 );
         break;
      case HB_SET_SOFTSEEK   :
         hb_retl( hb_set.HB_SET_SOFTSEEK );
         if( args > 1 ) hb_set.HB_SET_SOFTSEEK = set_logical( pArg2 );
         break;
      case HB_SET_TYPEAHEAD  :
         hb_retni( hb_set.HB_SET_TYPEAHEAD );
         if( args > 1 )
         {
            /* Set the value and limit the range */
            int old = hb_set.HB_SET_TYPEAHEAD;
            hb_set.HB_SET_TYPEAHEAD = set_number( pArg2, old );
            if( hb_set.HB_SET_TYPEAHEAD == 0 ) /* Do nothing */ ;
            else if( hb_set.HB_SET_TYPEAHEAD < 16 ) hb_set.HB_SET_TYPEAHEAD = 16;
            else if( hb_set.HB_SET_TYPEAHEAD > 4096 ) hb_set.HB_SET_TYPEAHEAD = 4096;
            /* Always reset the buffer, but only reallocate if the size changed */
            hb_inkeyReset( old == hb_set.HB_SET_TYPEAHEAD ? FALSE : TRUE );
         }
         break;
      case HB_SET_UNIQUE     :
         hb_retl( hb_set.HB_SET_UNIQUE );
         if( args > 1 ) hb_set.HB_SET_UNIQUE = set_logical( pArg2 );
         break;
      case HB_SET_VIDEOMODE  :
         hb_retni( hb_set.HB_SET_VIDEOMODE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_VIDEOMODE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET" );
            else
               hb_set.HB_SET_VIDEOMODE = set_number( pArg2, hb_set.HB_SET_VIDEOMODE );
         }
         break;
      case HB_SET_WRAP       :
         hb_retl( hb_set.HB_SET_WRAP );
         if( args > 1 ) hb_set.HB_SET_WRAP = set_logical( pArg2 );
         break;
      case HB_SET_LANGUAGE   :
         hb_retc( hb_langID() );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            hb_langSelectID( hb_itemGetCPtr( pArg2 ) );
         }
         break;
      case HB_SET_IDLEREPEAT :
         hb_retl( hb_vm_bIdleRepeat );
         if( args > 1 ) hb_vm_bIdleRepeat = set_logical( pArg2 );
         break;
      default                :
         /* Return NIL if called with invalid SET specifier */
         break;
   }
}

void hb_setInitialize( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setInitialize()"));

   hb_set.HB_SET_ALTERNATE = FALSE;
   hb_set.HB_SET_ALTFILE = NULL;
   hb_set.hb_set_althan = FS_ERROR;
   hb_set.HB_SET_AUTOPEN = FALSE;
   hb_set.HB_SET_AUTORDER = FALSE;
   hb_set.HB_SET_AUTOSHARE = 0;
   hb_set.HB_SET_BELL = FALSE;
   hb_set.HB_SET_CANCEL = TRUE;
   hb_set.hb_set_century = FALSE;
   strncpy( hb_set.HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", sizeof( hb_set.HB_SET_COLOR ) );
   hb_set.HB_SET_COLOR[ sizeof( hb_set.HB_SET_COLOR ) - 1 ] = '\0';
   hb_set.HB_SET_CONFIRM = FALSE;
   hb_set.HB_SET_CONSOLE = TRUE;
   hb_set.HB_SET_DATEFORMAT = ( char * ) hb_xgrab( 9 );
   memcpy( hb_set.HB_SET_DATEFORMAT, "mm/dd/yy", 9 );
   hb_set.HB_SET_DEBUG = FALSE;
   hb_set.HB_SET_DECIMALS = 2;
   hb_set.HB_SET_DEFAULT = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_DEFAULT[ 0 ] = '\0';
   hb_set.HB_SET_DELETED = FALSE;
   hb_set.HB_SET_DELIMCHARS = ( char * ) hb_xgrab( 3 );
   memcpy( hb_set.HB_SET_DELIMCHARS, "::", 3 );
   hb_set.HB_SET_DELIMITERS = FALSE;
   hb_set.HB_SET_DEVICE = ( char * ) hb_xgrab( 7 );
   memcpy( hb_set.HB_SET_DEVICE, "SCREEN", 7 );
   hb_set.HB_SET_EPOCH = 1900;
   hb_set.HB_SET_ESCAPE = TRUE;
   hb_set.HB_SET_EVENTMASK = INKEY_KEYBOARD;
   hb_set.HB_SET_EXACT = FALSE;
   hb_set.HB_SET_EXCLUSIVE = TRUE;
   hb_set.HB_SET_EXIT = FALSE;
   hb_set.HB_SET_EXTRA = FALSE;
   hb_set.HB_SET_EXTRAFILE = NULL;
   hb_set.hb_set_extrahan = FS_ERROR;
   hb_set.HB_SET_FIXED = FALSE;
   hb_set.HB_SET_INSERT = FALSE;
   hb_set.HB_SET_INTENSITY = TRUE;
   hb_set.HB_SET_MARGIN = 0;
   hb_set.HB_SET_MBLOCKSIZE = 0;
   hb_set.HB_SET_MCENTER = FALSE;
   hb_set.HB_SET_MESSAGE = 0;
   hb_set.HB_SET_MFILEEXT = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_MFILEEXT[ 0 ] = '\0';
   hb_set.HB_SET_OPTIMIZE = FALSE;
   hb_set.HB_SET_PATH = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_PATH[ 0 ] = '\0';
   hb_set.HB_SET_PRINTER = FALSE;
   hb_set.HB_SET_PRINTFILE = ( char * ) hb_xgrab( 4 );
   memcpy( hb_set.HB_SET_PRINTFILE, "PRN", 4 ); /* Default printer device */
   hb_set.hb_set_printhan = FS_ERROR;
   hb_set.HB_SET_SCOREBOARD = TRUE;
   hb_set.HB_SET_SCROLLBREAK = TRUE;
   hb_set.HB_SET_SOFTSEEK = FALSE;
   hb_set.HB_SET_STRICTREAD = FALSE;
   hb_set.HB_SET_TYPEAHEAD = 50; hb_inkeyReset( TRUE ); /* Allocate keyboard typeahead buffer */
   hb_set.HB_SET_UNIQUE = FALSE;
   hb_set.HB_SET_VIDEOMODE = 0;
   hb_set.HB_SET_WRAP = FALSE;
}

void hb_setRelease( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setRelease()"));

   close_text( hb_set.hb_set_althan );
   close_text( hb_set.hb_set_extrahan );
   close_binary( hb_set.hb_set_printhan );

   if( hb_set.HB_SET_ALTFILE )    hb_xfree( hb_set.HB_SET_ALTFILE );
   if( hb_set.HB_SET_DATEFORMAT ) hb_xfree( hb_set.HB_SET_DATEFORMAT );
   if( hb_set.HB_SET_DEFAULT )    hb_xfree( hb_set.HB_SET_DEFAULT );
   if( hb_set.HB_SET_DELIMCHARS ) hb_xfree( hb_set.HB_SET_DELIMCHARS );
   if( hb_set.HB_SET_DEVICE )     hb_xfree( hb_set.HB_SET_DEVICE );
   if( hb_set.HB_SET_EXTRAFILE )  hb_xfree( hb_set.HB_SET_EXTRAFILE );
   if( hb_set.HB_SET_MFILEEXT  )  hb_xfree( hb_set.HB_SET_MFILEEXT );
   if( hb_set.HB_SET_PATH )       hb_xfree( hb_set.HB_SET_PATH );
   if( hb_set.HB_SET_PRINTFILE )  hb_xfree( hb_set.HB_SET_PRINTFILE );

   hb_set.HB_SET_TYPEAHEAD = -1; hb_inkeyReset( TRUE ); /* Free keyboard typeahead buffer */
}
