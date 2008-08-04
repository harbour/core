/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Set functions
 *
 * Copyright 1999-2003 David G. Holm <dholm@jsd-llc.com>
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

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapilng.h"
#include "hbset.h"

HB_SET_STRUCT hb_set;

typedef struct HB_SET_LISTENER_
{
   int listener;
   HB_SET_LISTENER_CALLBACK * callback;
   struct HB_SET_LISTENER_ * next;
} HB_SET_LISTENER, * PHB_SET_LISTENER;

static PHB_SET_LISTENER sp_sl_first;
static PHB_SET_LISTENER sp_sl_last;
static int s_next_listener;

static HB_PATHNAMES * sp_set_path;

static void hb_setFreeSetPath( void )
{
   if( sp_set_path )
   {
      hb_fsFreeSearchPath( sp_set_path );
      sp_set_path = NULL;
   }
}

static char set_char( PHB_ITEM pItem, char oldChar )
{
   char newChar = oldChar;

   HB_TRACE(HB_TR_DEBUG, ("set_char(%p, %c)", pItem, oldChar));

   if( HB_IS_STRING( pItem ) )
   {
      /* Only replace if string has at least one character. */
      ULONG ulLen = hb_itemGetCLen( pItem );
      if( ulLen > 0 )
      {
         newChar = *hb_itemGetCPtr( pItem );
      }
   }
   return newChar;
}

static BOOL set_logical( PHB_ITEM pItem, BOOL bDefault )
{
   BOOL bLogical = bDefault;

   HB_TRACE(HB_TR_DEBUG, ("set_logical(%p)", pItem));

   if( HB_IS_LOGICAL( pItem ) )
      bLogical = hb_itemGetL( pItem );
   else if( HB_IS_STRING( pItem ) )
   {
      char * szString = hb_itemGetCPtr( pItem );
      ULONG ulLen = hb_itemGetCLen( pItem );

      if( ulLen >= 2
       && toupper( ( UCHAR ) szString[ 0 ] ) == 'O'
       && toupper( ( UCHAR ) szString[ 1 ] ) == 'N' )
         bLogical = TRUE;
      else if( ulLen >= 3
       && toupper( ( UCHAR ) szString[ 0 ] ) == 'O'
       && toupper( ( UCHAR ) szString[ 1 ] ) == 'F'
       && toupper( ( UCHAR ) szString[ 2 ] ) == 'F' )
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
   else if( HB_IS_NIL( pItem ) )
   {
      if( szOldString ) szString = ( char * ) hb_xrealloc( szOldString, 1 );
      else szString = ( char * ) hb_xgrab( 1 );

      szString[ 0 ] = '\0';
   }
   else
   {
      szString = szOldString;
   }

   return szString;
}

static void close_binary( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_binary(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      hb_fsClose( handle );
   }
}

static void close_text( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_text(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      if( hb_set.HB_SET_EOF )
         hb_fsWrite( handle, ( BYTE * ) "\x1A", 1 );
      hb_fsClose( handle );
   }
}

static FHANDLE open_handle( char * file_name, BOOL bAppend, char * def_ext, HB_set_enum set_specifier )
{
   FHANDLE handle;
   char path[ _POSIX_PATH_MAX + 1 ];
   BOOL bPipe = FALSE;
   HB_TRACE(HB_TR_DEBUG, ("open_handle(%s, %d, %s, %d)", file_name, (int) bAppend, def_ext, (int) set_specifier));

   /* Create full filename */
#if defined(OS_UNIX_COMPATIBLE)
   bPipe = set_specifier == HB_SET_PRINTFILE && file_name[ 0 ] == '|';
   if( bPipe )
   {
      file_name++;
      bAppend = FALSE;
   }
#endif
   if( ! bPipe )
   {
      PHB_FNAME pFilename = hb_fsFNameSplit( file_name );

      if( hb_set.HB_SET_DEFEXTENSIONS && pFilename->szExtension == NULL && def_ext )
      {
#if defined(HB_OS_OS2) || defined(HB_OS_W32) || defined(HB_OS_DOS)
         if( pFilename->szName )
         {
            int iLen = ( int ) strlen( pFilename->szName );
            if( ( iLen == 3 && 
                  ( hb_stricmp( pFilename->szName, "PRN" ) == 0 ||
                    hb_stricmp( pFilename->szName, "CON" ) == 0 ) ) ||
                ( iLen == 4 &&
                  ( ( hb_strnicmp( pFilename->szName, "LPT", 3 ) == 0 &&
                      pFilename->szName[3] >= '1' && pFilename->szName[3] <= '3' ) ||
                    ( hb_strnicmp( pFilename->szName, "COM", 3 ) == 0 &&
                      pFilename->szName[3] >= '1' && pFilename->szName[3] <= '9' ) ) ) )
            {
               hb_strupr( pFilename->szName );
               def_ext = NULL;
            }
         }
#endif
         pFilename->szExtension = def_ext;
      }
      if( pFilename->szPath == NULL && hb_set.HB_SET_DEFAULT )
         pFilename->szPath = hb_set.HB_SET_DEFAULT;

      hb_fsFNameMerge( path, pFilename );
      hb_xfree( pFilename );
   }

   /* Open the file either in append (bAppend) or truncate mode (!bAppend), but
      always use binary mode */

   /* QUESTION: What sharing mode does Clipper use ? [vszakats] */

   handle = FS_ERROR;
   while( handle == FS_ERROR )
   {
      BOOL bCreate = FALSE;

      if( bPipe )
         handle = hb_fsPOpen( ( BYTE * ) file_name, ( BYTE * ) "w" );
      else
      {
         if( bAppend )
         {  /* Append mode */
            if( hb_fsFile( ( BYTE * ) path ) )
            {  /* If the file already exists, open it (in read-write mode, in
                  case of non-Unix and text modes). */
               handle = hb_fsOpen( ( BYTE * ) path, FO_READWRITE | FO_DENYWRITE );
               if( handle != FS_ERROR )
               {  /* Position to EOF */
                  /* Special binary vs. text file handling - even for UN*X, now
                     that there's an HB_SET_EOF flag. */
                  if( set_specifier == HB_SET_PRINTFILE )
                  {  /* PRINTFILE is always binary and needs no special handling. */
                     hb_fsSeek( handle, 0, FS_END );
                  }
                  else
                  {  /* All other files are text files and may have an EOF
                        ('\x1A') character at the end (both UN*X and non-UN*X,
                        now that theres an HB_SET_EOF flag). */
                     char cEOF = '\0';
                     hb_fsSeek( handle, -1, FS_END ); /* Position to last char. */
                     hb_fsRead( handle, ( BYTE * ) &cEOF, 1 );   /* Read the last char. */
                     if( cEOF == '\x1A' )             /* If it's an EOF, */
                     {
                        hb_fsSeek( handle, -1, FS_END ); /* Then write over it. */
                     }
                  }
               }
            }
            else bCreate = TRUE; /* Otherwise create a new file. */
         }
         else bCreate = TRUE; /* Always create a new file for overwrite mode. */

         if( bCreate )
            handle = hb_fsCreate( ( BYTE * ) path, FC_NORMAL );
      }

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

         if( uiAction != E_RETRY )
            break;
      }
   }
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
       && toupper( ( UCHAR ) szString[ 0 ] ) == 'O'
       && toupper( ( UCHAR ) szString[ 1 ] ) == 'N' )
         hb_set.hb_set_century = TRUE;
      else if( ulLen >= 3
       && toupper( ( UCHAR ) szString[ 0 ] ) == 'O'
       && toupper( ( UCHAR ) szString[ 1 ] ) == 'F'
       && toupper( ( UCHAR ) szString[ 2 ] ) == 'F' )
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
         digit = toupper( ( UCHAR ) szDateFormat[ count ] );
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
         hb_strncat( szNewFormat, "YY", size );
         if( hb_set.hb_set_century ) hb_strncat( szNewFormat, "YY", size );
         format_len = strlen( szDateFormat );
         if( y_stop < format_len ) hb_strncat( szNewFormat, szDateFormat + y_stop, size );
         /* DATE FORMAT is under direct control of SET, so notify when it
            it is changed indirectly via __SETCENTURY() */
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_BEFORE );
         hb_xfree( szDateFormat );
         hb_set.HB_SET_DATEFORMAT = szNewFormat;
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_AFTER );
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

   if( args > 1 ) hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );

   switch( set_specifier )
   {
      case HB_SET_ALTERNATE:
         hb_retl( hb_set.HB_SET_ALTERNATE );
         if( args > 1 ) hb_set.HB_SET_ALTERNATE = set_logical( pArg2, hb_set.HB_SET_ALTERNATE );
         break;
      case HB_SET_ALTFILE:
         if( hb_set.HB_SET_ALTFILE ) hb_retc( hb_set.HB_SET_ALTFILE );
         else hb_retc( NULL );
         if( args > 1 )
         {
            if( HB_IS_NIL( pArg2 ) )
            {
               if( hb_set.HB_SET_ALTFILE )
               {
                  hb_xfree( hb_set.HB_SET_ALTFILE );
                  hb_set.HB_SET_ALTFILE = NULL;
               }
            }
            else
            {
               hb_set.HB_SET_ALTFILE = set_string( pArg2, hb_set.HB_SET_ALTFILE );
            }
         }
         if( args > 2 ) bFlag = set_logical( pArg3, FALSE );
         else bFlag = FALSE;
         if( args > 1 )
         {
            close_text( hb_set.hb_set_althan );
            if( hb_set.HB_SET_ALTFILE && hb_set.HB_SET_ALTFILE[0] != '\0' )
               hb_set.hb_set_althan = open_handle( hb_set.HB_SET_ALTFILE, bFlag, ".txt", HB_SET_ALTFILE );
            else
               hb_set.hb_set_althan = FS_ERROR;
         }
         break;
      case HB_SET_AUTOPEN:
         hb_retl( hb_set.HB_SET_AUTOPEN );
         if( args > 1 ) hb_set.HB_SET_AUTOPEN = set_logical( pArg2, hb_set.HB_SET_AUTOPEN );
         break;
      case HB_SET_AUTORDER:
         hb_retni( hb_set.HB_SET_AUTORDER );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_AUTORDER ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_AUTORDER = set_number( pArg2, hb_set.HB_SET_AUTORDER );
         }
         break;
      case HB_SET_AUTOSHARE:
         hb_retni( hb_set.HB_SET_AUTOSHARE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_AUTOSHARE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_AUTOSHARE = set_number( pArg2, hb_set.HB_SET_AUTOSHARE );
         }
         break;
      case HB_SET_BELL:
         hb_retl( hb_set.HB_SET_BELL );
         if( args > 1 ) hb_set.HB_SET_BELL = set_logical( pArg2, hb_set.HB_SET_BELL );
         break;
      case HB_SET_CANCEL:
         hb_retl( hb_set.HB_SET_CANCEL );
         if( args > 1 ) hb_set.HB_SET_CANCEL = set_logical( pArg2, hb_set.HB_SET_CANCEL );
         break;
      case HB_SET_COLOR:
         hb_retc( hb_conSetColor( args >= 2 && HB_IS_STRING( pArg2 ) ? hb_itemGetCPtr( pArg2 ) : ( char * ) NULL ) );
         break;
      case HB_SET_CONFIRM:
         hb_retl( hb_set.HB_SET_CONFIRM );
         if( args > 1 ) hb_set.HB_SET_CONFIRM = set_logical( pArg2, hb_set.HB_SET_CONFIRM );
         break;
      case HB_SET_CONSOLE:
         hb_retl( hb_set.HB_SET_CONSOLE );
         if( args > 1 ) hb_set.HB_SET_CONSOLE = set_logical( pArg2, hb_set.HB_SET_CONSOLE );
         break;
      case HB_SET_CURSOR:
         if( args >= 2 && HB_IS_NUMERIC( pArg2 ) )
            hb_retni( hb_conSetCursor( TRUE, hb_itemGetNI( pArg2 ) ) );
         else
            hb_retni( hb_conSetCursor( FALSE, 0 ) );
         break;
      case HB_SET_DATEFORMAT:
         if( hb_set.HB_SET_DATEFORMAT )
            hb_retc( hb_set.HB_SET_DATEFORMAT );
         else
            hb_retc( NULL );

         if( args > 1 )
         {
            BOOL flag = FALSE;
            unsigned int i, iLen;
            int ch, year = 0;

            hb_set.HB_SET_DATEFORMAT = set_string( pArg2, hb_set.HB_SET_DATEFORMAT );
            iLen = ( unsigned int ) strlen( hb_set.HB_SET_DATEFORMAT );
            for( i = 0; i < iLen; i++ )
            {
               ch = hb_set.HB_SET_DATEFORMAT[i];
               if( !flag && ( ch == 'Y' || ch == 'y' ) )
                  year++;   /* Only count the first set of consecutive "Y"s. */
               else if( year )
                  flag = TRUE; /* Indicate non-consecutive. */
            }

            flag = ( year >= 4 );

            if( flag != hb_set.hb_set_century )
            {
               /* CENTURY is not controlled directly by SET, so there is no
                  notification for changing it indirectly via DATE FORMAT. */
               hb_set.hb_set_century = flag;
            }
         }
         break;
      case HB_SET_DEBUG:
         hb_retl( hb_set.HB_SET_DEBUG );
         if( args > 1 ) hb_set.HB_SET_DEBUG = set_logical( pArg2, hb_set.HB_SET_DEBUG );
         break;
      case HB_SET_DECIMALS:
         hb_retni( hb_set.HB_SET_DECIMALS );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_DECIMALS ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_DECIMALS = set_number( pArg2, hb_set.HB_SET_DECIMALS );
         }
         break;
      case HB_SET_DEFAULT:
         if( hb_set.HB_SET_DEFAULT ) hb_retc( hb_set.HB_SET_DEFAULT );
         else hb_retc( NULL );
         if( args > 1 ) hb_set.HB_SET_DEFAULT = set_string( pArg2, hb_set.HB_SET_DEFAULT );
         break;
      case HB_SET_DELETED:
         hb_retl( hb_set.HB_SET_DELETED );
         if( args > 1 ) hb_set.HB_SET_DELETED = set_logical( pArg2, hb_set.HB_SET_DELETED );
         break;
      case HB_SET_DELIMCHARS:
         if( hb_set.HB_SET_DELIMCHARS ) hb_retc( hb_set.HB_SET_DELIMCHARS );
         else hb_retc( NULL );
         if( args > 1 ) hb_set.HB_SET_DELIMCHARS = set_string( pArg2, hb_set.HB_SET_DELIMCHARS );
         break;
      case HB_SET_DELIMITERS:
         hb_retl( hb_set.HB_SET_DELIMITERS );
         if( args > 1 ) hb_set.HB_SET_DELIMITERS = set_logical( pArg2, hb_set.HB_SET_DELIMITERS );
         break;
      case HB_SET_DEVICE:
         if( hb_set.HB_SET_DEVICE ) hb_retc( hb_set.HB_SET_DEVICE );
         else hb_retc( NULL );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            /* If the print file is not already open, open it in overwrite mode. */
            hb_set.HB_SET_DEVICE = set_string( pArg2, hb_set.HB_SET_DEVICE );
            if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 && hb_set.hb_set_printhan == FS_ERROR
            && hb_set.HB_SET_PRINTFILE && hb_set.HB_SET_PRINTFILE[0] != '\0' )
               hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, FALSE, NULL, HB_SET_PRINTFILE );
         }
         break;
      case HB_SET_EOF:
         hb_retl( hb_set.HB_SET_EOF );
         if( args > 1 ) hb_set.HB_SET_EOF = set_logical( pArg2, hb_set.HB_SET_EOF );
         break;
      case HB_SET_EPOCH:
         hb_retni( hb_set.HB_SET_EPOCH );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_EPOCH ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_EPOCH = set_number( pArg2, hb_set.HB_SET_EPOCH );
         }
         break;
      case HB_SET_ESCAPE:
         hb_retl( hb_set.HB_SET_ESCAPE );
         if( args > 1 ) hb_set.HB_SET_ESCAPE = set_logical( pArg2, hb_set.HB_SET_ESCAPE );
         break;
      case HB_SET_EVENTMASK:
         hb_retni( hb_set.HB_SET_EVENTMASK );
         if( args > 1 ) hb_set.HB_SET_EVENTMASK = set_number( pArg2, hb_set.HB_SET_EVENTMASK );
         break;
      case HB_SET_EXACT:
         hb_retl( hb_set.HB_SET_EXACT );
         if( args > 1 ) hb_set.HB_SET_EXACT = set_logical( pArg2, hb_set.HB_SET_EXACT );
         break;
      case HB_SET_EXCLUSIVE:
         hb_retl( hb_set.HB_SET_EXCLUSIVE );
         if( args > 1 ) hb_set.HB_SET_EXCLUSIVE = set_logical( pArg2, hb_set.HB_SET_EXCLUSIVE );
         break;
      case HB_SET_EXIT:
         hb_retl( hb_set.HB_SET_EXIT );
         /* NOTE: Otherwise ReadExit() will always set the value. [vszakats] */
         if( pArg2 != NULL && !HB_IS_NIL( pArg2 ) ) hb_set.HB_SET_EXIT = set_logical( pArg2, hb_set.HB_SET_EXIT );
         break;
      case HB_SET_EXTRA:
         hb_retl( hb_set.HB_SET_EXTRA );
         if( args > 1 ) hb_set.HB_SET_EXTRA = set_logical( pArg2, hb_set.HB_SET_EXTRA );
         break;
      case HB_SET_EXTRAFILE:
         if( hb_set.HB_SET_EXTRAFILE ) hb_retc( hb_set.HB_SET_EXTRAFILE );
         else hb_retc( NULL );
         if( args > 1 )
         {
            if( HB_IS_NIL( pArg2 ) )
            {
               if( hb_set.HB_SET_EXTRAFILE )
               {
                  hb_xfree( hb_set.HB_SET_EXTRAFILE );
                  hb_set.HB_SET_EXTRAFILE = NULL;
               }
            }
            else
            {
               hb_set.HB_SET_EXTRAFILE = set_string( pArg2, hb_set.HB_SET_EXTRAFILE );
            }
         }
         if( args > 2 ) bFlag = set_logical( pArg3, FALSE );
         else bFlag = FALSE;
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            close_text( hb_set.hb_set_extrahan );
            if( hb_set.HB_SET_EXTRAFILE && hb_set.HB_SET_EXTRAFILE[0] != '\0' )
               hb_set.hb_set_extrahan = open_handle( hb_set.HB_SET_EXTRAFILE, bFlag, ".prn", HB_SET_EXTRAFILE );
            else
               hb_set.hb_set_extrahan = FS_ERROR;
         }
         break;
      case HB_SET_FIXED:
         hb_retl( hb_set.HB_SET_FIXED );
         if( args > 1 ) hb_set.HB_SET_FIXED = set_logical( pArg2, hb_set.HB_SET_FIXED );
         break;
      case HB_SET_INSERT:
         hb_retl( hb_set.HB_SET_INSERT );
         if( args > 1 ) hb_set.HB_SET_INSERT = set_logical( pArg2, hb_set.HB_SET_INSERT );
         break;
      case HB_SET_INTENSITY:
         hb_retl( hb_set.HB_SET_INTENSITY );
         if( args > 1 ) hb_set.HB_SET_INTENSITY = set_logical( pArg2, hb_set.HB_SET_INTENSITY );
         break;
      case HB_SET_MARGIN:
         hb_retni( hb_set.HB_SET_MARGIN );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MARGIN ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_MARGIN = set_number( pArg2, hb_set.HB_SET_MARGIN );
         }
         break;
      case HB_SET_MBLOCKSIZE:
         hb_retni( hb_set.HB_SET_MBLOCKSIZE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_MBLOCKSIZE = set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE );
         }
         break;
      case HB_SET_MCENTER:
         hb_retl( hb_set.HB_SET_MCENTER );
         if( args > 1 ) hb_set.HB_SET_MCENTER = set_logical( pArg2, hb_set.HB_SET_MCENTER );
         break;
      case HB_SET_MESSAGE:
         hb_retni( hb_set.HB_SET_MESSAGE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MESSAGE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_MESSAGE = set_number( pArg2, hb_set.HB_SET_MESSAGE );
         }
         break;
      case HB_SET_MFILEEXT:
         hb_retc( hb_set.HB_SET_MFILEEXT );
         if( args > 1 ) hb_set.HB_SET_MFILEEXT = set_string( pArg2, hb_set.HB_SET_MFILEEXT );
         break;
      case HB_SET_OPTIMIZE:
         hb_retl( hb_set.HB_SET_OPTIMIZE );
         if( args > 1 ) hb_set.HB_SET_OPTIMIZE = set_logical( pArg2, hb_set.HB_SET_OPTIMIZE );
         break;
      case HB_SET_FORCEOPT:
         hb_retl( hb_set.HB_SET_FORCEOPT );
         if( args > 1 ) hb_set.HB_SET_FORCEOPT = set_logical( pArg2, hb_set.HB_SET_FORCEOPT );
         break;
      case HB_SET_STRICTREAD:
         hb_retl( hb_set.HB_SET_STRICTREAD );
         if( args > 1 ) hb_set.HB_SET_STRICTREAD = set_logical( pArg2, hb_set.HB_SET_STRICTREAD );
         break;
      case HB_SET_HARDCOMMIT:
         hb_retl( hb_set.HB_SET_HARDCOMMIT );
         if( args > 1 ) hb_set.HB_SET_HARDCOMMIT = set_logical( pArg2, hb_set.HB_SET_HARDCOMMIT );
         break;
      case HB_SET_PATH:
         if( hb_set.HB_SET_PATH ) hb_retc( hb_set.HB_SET_PATH );
         else hb_retc( NULL );
         if( args > 1 )
         {
            hb_setFreeSetPath();
            hb_set.HB_SET_PATH = set_string( pArg2, hb_set.HB_SET_PATH );
            hb_fsAddSearchPath( hb_set.HB_SET_PATH, &sp_set_path );
         }
         break;
      case HB_SET_PRINTER:
         hb_retl( hb_set.HB_SET_PRINTER );
         if( args > 1 ) hb_set.HB_SET_PRINTER = set_logical( pArg2, hb_set.HB_SET_PRINTER );
         break;
      case HB_SET_PRINTFILE:
         if( hb_set.HB_SET_PRINTFILE ) hb_retc( hb_set.HB_SET_PRINTFILE );
         else hb_retc( NULL );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) ) hb_set.HB_SET_PRINTFILE = set_string( pArg2, hb_set.HB_SET_PRINTFILE );
         if( args > 2 ) bFlag = set_logical( pArg3, FALSE );
         else bFlag = FALSE;
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            close_binary( hb_set.hb_set_printhan );
            hb_set.hb_set_printhan = FS_ERROR;
            if( hb_set.HB_SET_PRINTFILE && hb_set.HB_SET_PRINTFILE[0] != '\0' )
               hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, bFlag, ".prn", HB_SET_PRINTFILE );
         }
         break;
      case HB_SET_SCOREBOARD:
         hb_retl( hb_set.HB_SET_SCOREBOARD );
         if( args > 1 ) hb_set.HB_SET_SCOREBOARD = set_logical( pArg2, hb_set.HB_SET_SCOREBOARD );
         break;
      case HB_SET_SCROLLBREAK:
         hb_retl( hb_set.HB_SET_SCROLLBREAK );
         if( args > 1 ) hb_set.HB_SET_SCROLLBREAK = set_logical( pArg2, hb_set.HB_SET_SCROLLBREAK );
         break;
      case HB_SET_SOFTSEEK:
         hb_retl( hb_set.HB_SET_SOFTSEEK );
         if( args > 1 ) hb_set.HB_SET_SOFTSEEK = set_logical( pArg2, hb_set.HB_SET_SOFTSEEK );
         break;
      case HB_SET_TYPEAHEAD:
         hb_retni( hb_set.HB_SET_TYPEAHEAD );
         if( args > 1 )
         {
            /* Set the value and limit the range */
            int old = hb_set.HB_SET_TYPEAHEAD;
            hb_set.HB_SET_TYPEAHEAD = set_number( pArg2, old );
            if( hb_set.HB_SET_TYPEAHEAD == 0 ) /* Do nothing */ ;
            else if( hb_set.HB_SET_TYPEAHEAD < 16 ) hb_set.HB_SET_TYPEAHEAD = 16;
            else if( hb_set.HB_SET_TYPEAHEAD > 4096 ) hb_set.HB_SET_TYPEAHEAD = 4096;
            /* reset keyboard buffer */
            hb_inkeyReset();
         }
         break;
      case HB_SET_UNIQUE:
         hb_retl( hb_set.HB_SET_UNIQUE );
         if( args > 1 ) hb_set.HB_SET_UNIQUE = set_logical( pArg2, hb_set.HB_SET_UNIQUE );
         break;
      case HB_SET_VIDEOMODE:
         hb_retni( hb_set.HB_SET_VIDEOMODE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_VIDEOMODE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_VIDEOMODE = set_number( pArg2, hb_set.HB_SET_VIDEOMODE );
         }
         break;
      case HB_SET_WRAP:
         hb_retl( hb_set.HB_SET_WRAP );
         if( args > 1 ) hb_set.HB_SET_WRAP = set_logical( pArg2, hb_set.HB_SET_WRAP );
         break;
      case HB_SET_LANGUAGE:
         hb_retc( hb_langID() );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
            hb_langSelectID( hb_itemGetCPtr( pArg2 ) );
         break;
      case HB_SET_IDLEREPEAT:
         hb_retl( hb_set.HB_SET_IDLEREPEAT );
         if( args > 1 ) hb_set.HB_SET_IDLEREPEAT = set_logical( pArg2, hb_set.HB_SET_IDLEREPEAT );
         break;
      case HB_SET_FILECASE:
         hb_retni( hb_set.HB_SET_FILECASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "LOWER" ) )
                  hb_set.HB_SET_FILECASE = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "UPPER" ) )
                  hb_set.HB_SET_FILECASE = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "MIXED" ) )
                  hb_set.HB_SET_FILECASE = HB_SET_CASE_MIXED;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, hb_set.HB_SET_FILECASE ) < 0 )
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
               else
                  hb_set.HB_SET_FILECASE = set_number( pArg2, hb_set.HB_SET_FILECASE );
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
         }
         /*
         if( hb_set.HB_SET_FILECASE )
           hb_retc( hb_set.HB_SET_FILECASE );
         else hb_retc( NULL );
         if( hb_stricmp( hb_set.HB_SET_FILECASE, "LOWER" ) == 0 )
            if( args > 1 )
               hb_set.HB_SET_FILECASE = set_string( pArg2, hb_set.HB_SET_FILECASE );
         */
         break;
      case HB_SET_DIRCASE:
         hb_retni( hb_set.HB_SET_DIRCASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "LOWER" ) )
                  hb_set.HB_SET_DIRCASE = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "UPPER" ) )
                  hb_set.HB_SET_DIRCASE = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "MIXED" ) )
                  hb_set.HB_SET_DIRCASE = HB_SET_CASE_MIXED;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, hb_set.HB_SET_DIRCASE ) < 0 )
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
               else
                  hb_set.HB_SET_DIRCASE = set_number( pArg2, hb_set.HB_SET_DIRCASE );
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
         }
         break;
      case HB_SET_DIRSEPARATOR:
      {
         char szDirSep[ 2 ];
         szDirSep[ 0 ] = ( char ) hb_set.HB_SET_DIRSEPARATOR;
         szDirSep[ 1 ] = '\0';
         hb_retc( szDirSep );
         if( args > 1 ) hb_set.HB_SET_DIRSEPARATOR = set_char( pArg2, hb_set.HB_SET_DIRSEPARATOR );
         break;
      }
      case HB_SET_DBFLOCKSCHEME:
         hb_retni( hb_set.HB_SET_DBFLOCKSCHEME );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_DBFLOCKSCHEME ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               hb_set.HB_SET_DBFLOCKSCHEME = set_number( pArg2, hb_set.HB_SET_DBFLOCKSCHEME );
         }
         break;
      case HB_SET_DEFEXTENSIONS:
         hb_retl( hb_set.HB_SET_DEFEXTENSIONS );
         if( args > 1 ) hb_set.HB_SET_DEFEXTENSIONS = set_logical( pArg2, hb_set.HB_SET_DEFEXTENSIONS );
         break;
      case HB_SET_EOL:
         if( hb_set.HB_SET_EOL ) hb_retc( hb_set.HB_SET_EOL );
         else hb_retc( NULL );
         if( args > 1 ) hb_set.HB_SET_EOL = set_string( pArg2, hb_set.HB_SET_EOL );
         break;
      case HB_SET_TRIMFILENAME:
         hb_retl( hb_set.HB_SET_TRIMFILENAME );
         if( args > 1 ) hb_set.HB_SET_TRIMFILENAME = set_logical( pArg2, hb_set.HB_SET_TRIMFILENAME );
         break;
      case HB_SET_HBOUTLOG:
         if( hb_set.HB_SET_HBOUTLOG ) hb_retc( hb_set.HB_SET_HBOUTLOG );
         else hb_retc( NULL );
         if( args > 1 )
         {
            hb_set.HB_SET_HBOUTLOG = set_string( pArg2, hb_set.HB_SET_HBOUTLOG );
            hb_xsetfilename( hb_set.HB_SET_HBOUTLOG );
         }
         break;
      case HB_SET_HBOUTLOGINFO:
         if( hb_set.HB_SET_HBOUTLOGINFO ) hb_retc( hb_set.HB_SET_HBOUTLOGINFO );
         else hb_retc( NULL );
         if( args > 1 )
         {
            hb_set.HB_SET_HBOUTLOGINFO = set_string( pArg2, hb_set.HB_SET_HBOUTLOGINFO );
            hb_xsetinfo( hb_set.HB_SET_HBOUTLOGINFO );
         }
         break;

      case HB_SET_INVALID_:
         /* Return NIL if called with invalid SET specifier */
         break;

#if 0 
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }
   if( args > 1 ) hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
}

/* Listener test (1 of 2)
static void test_callback( HB_set_enum set, HB_set_listener_enum when )
{
   printf("\ntest_callback( %d, %d )", set, when);
}
End listener test (1 of 2) */

void hb_setInitialize( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setInitialize()"));

   hb_set.HB_SET_ALTERNATE = FALSE;
   hb_set.HB_SET_ALTFILE = NULL;
   hb_set.hb_set_althan = FS_ERROR;
   hb_set.HB_SET_AUTOPEN = TRUE;
   hb_set.HB_SET_AUTORDER = 0;
   hb_set.HB_SET_AUTOSHARE = 0;
   hb_set.HB_SET_BELL = FALSE;
   hb_set.HB_SET_CANCEL = TRUE;
   hb_set.hb_set_century = FALSE;
   hb_set.HB_SET_COLOR = ( char * ) hb_xgrab( CLR_STRLEN + 1 );
   hb_strncpy( hb_set.HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", CLR_STRLEN );
   hb_set.HB_SET_CONFIRM = FALSE;
   hb_set.HB_SET_CONSOLE = TRUE;
   hb_set.HB_SET_DATEFORMAT = hb_strdup( "mm/dd/yy" );
   /*
    * Tests shows that Clipper has two different flags to control ALT+D
    * and ALTD() behavior and on startup these flags are not synchronized.
    * When application starts _SET_DEBUG is set to FALSE but debugger
    * can be activated by hitting K_ALT_D or calling ALTD() function without
    * parameter. It means that some other internal flag enables these
    * operations.
    * Because Harbour is using _SET_DEBUG flag only then we have to
    * initialize it to TRUE when debugger is linked to keep real Clipper
    * behavior or we will have to add second flag too and try to replicate
    * exactly unsynchronized behavior of these flags which exists in Clipper.
    * IMHO it's a bug in Clipper (side effect of some internal solutions) and
    * we should not try to emulate it [druzus].
    */
   /* hb_set.HB_SET_DEBUG = FALSE; */
   hb_set.HB_SET_DEBUG = hb_dynsymFind( "__DBGENTRY" ) ? TRUE : FALSE;
   hb_set.HB_SET_DECIMALS = 2;
   hb_set.HB_SET_DEFAULT = hb_strdup( "" );
   hb_set.HB_SET_DELETED = FALSE;
   hb_set.HB_SET_DELIMCHARS = hb_strdup( "::" );
   hb_set.HB_SET_DELIMITERS = FALSE;
   hb_set.HB_SET_DEVICE = hb_strdup( "SCREEN" );
#if defined(OS_UNIX_COMPATIBLE)
   hb_set.HB_SET_EOF = FALSE;
#else
   hb_set.HB_SET_EOF = TRUE;
#endif
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
   hb_set.HB_SET_FORCEOPT = FALSE;
   hb_set.HB_SET_HARDCOMMIT = TRUE;
   hb_set.HB_SET_IDLEREPEAT = TRUE;
   hb_set.HB_SET_INSERT = FALSE;
   hb_set.HB_SET_INTENSITY = TRUE;
   hb_set.HB_SET_MARGIN = 0;
   hb_set.HB_SET_MBLOCKSIZE = 0;
   hb_set.HB_SET_MCENTER = FALSE;
   hb_set.HB_SET_MESSAGE = 0;
   hb_set.HB_SET_MFILEEXT = hb_strdup( "" );
   hb_set.HB_SET_OPTIMIZE = TRUE;
   hb_set.HB_SET_PATH = hb_strdup( "" );
   hb_set.HB_SET_PRINTER = FALSE;
   /* Default printer device */
#if defined(HB_OS_UNIX)
   hb_set.HB_SET_PRINTFILE = hb_strdup( "|lpr" );
#elif defined(HB_OS_DOS)
   hb_set.HB_SET_PRINTFILE = hb_strdup( "PRN" );
#elif defined(HB_OS_WIN_32) || defined(HB_OS_OS2)
   hb_set.HB_SET_PRINTFILE = hb_strdup( "LPT1" );
#else
   hb_set.HB_SET_PRINTFILE = hb_strdup( "PRN" ); /* TOFIX */
#endif
   hb_set.hb_set_printhan = FS_ERROR;
   hb_set.HB_SET_SCOREBOARD = TRUE;
   hb_set.HB_SET_SCROLLBREAK = TRUE;
   hb_set.HB_SET_SOFTSEEK = FALSE;
   hb_set.HB_SET_STRICTREAD = FALSE;
   hb_set.HB_SET_TYPEAHEAD = HB_DEFAULT_INKEY_BUFSIZE;
   hb_set.HB_SET_UNIQUE = FALSE;
   hb_set.HB_SET_FILECASE = HB_SET_CASE_MIXED;
   hb_set.HB_SET_DIRCASE = HB_SET_CASE_MIXED;
   hb_set.HB_SET_DIRSEPARATOR = OS_PATH_DELIMITER;
   hb_set.HB_SET_VIDEOMODE = 0;
   hb_set.HB_SET_WRAP = FALSE;
   hb_set.HB_SET_DBFLOCKSCHEME = 0;
   hb_set.HB_SET_DEFEXTENSIONS = TRUE;
   hb_set.HB_SET_EOL = hb_strdup( hb_conNewLine() );
   hb_set.HB_SET_TRIMFILENAME = FALSE;
   hb_set.HB_SET_HBOUTLOG = hb_strdup( "hb_out.log" );
   hb_set.HB_SET_HBOUTLOGINFO = hb_strdup( "" );

   hb_xsetfilename( hb_set.HB_SET_HBOUTLOG );
   hb_xsetinfo( hb_set.HB_SET_HBOUTLOGINFO );

   sp_sl_first = sp_sl_last = NULL;
   s_next_listener = 1;

   /* Listener test (2 of 2)
   {
      int temp = hb_setListenerAdd( test_callback );
      printf("\nSet listener test handle is %d", temp);
      {
         int temp2 = hb_setListenerAdd( test_callback );
         printf("\nSet listener test handle is %d", temp2);
         temp2 = hb_setListenerRemove( temp2 );
         printf("\nSet listener remove result is %d", temp2);
      }
   }
   End listener test (2 of 2) */
}

void hb_setRelease( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setRelease()"));

   close_text( hb_set.hb_set_althan );
   close_text( hb_set.hb_set_extrahan );
   close_binary( hb_set.hb_set_printhan );

   if( hb_set.HB_SET_ALTFILE )      hb_xfree( hb_set.HB_SET_ALTFILE );
   if( hb_set.HB_SET_DATEFORMAT )   hb_xfree( hb_set.HB_SET_DATEFORMAT );
   if( hb_set.HB_SET_DEFAULT )      hb_xfree( hb_set.HB_SET_DEFAULT );
   if( hb_set.HB_SET_DELIMCHARS )   hb_xfree( hb_set.HB_SET_DELIMCHARS );
   if( hb_set.HB_SET_DEVICE )       hb_xfree( hb_set.HB_SET_DEVICE );
   if( hb_set.HB_SET_EXTRAFILE )    hb_xfree( hb_set.HB_SET_EXTRAFILE );
   if( hb_set.HB_SET_MFILEEXT  )    hb_xfree( hb_set.HB_SET_MFILEEXT );
   if( hb_set.HB_SET_PATH )         hb_xfree( hb_set.HB_SET_PATH );
   if( hb_set.HB_SET_PRINTFILE )    hb_xfree( hb_set.HB_SET_PRINTFILE );
   if( hb_set.HB_SET_COLOR )        hb_xfree( hb_set.HB_SET_COLOR );
   if( hb_set.HB_SET_EOL )          hb_xfree( hb_set.HB_SET_EOL );
   if( hb_set.HB_SET_HBOUTLOG )     hb_xfree( hb_set.HB_SET_HBOUTLOG );
   if( hb_set.HB_SET_HBOUTLOGINFO ) hb_xfree( hb_set.HB_SET_HBOUTLOGINFO );

   hb_set.HB_SET_TYPEAHEAD = 0;     hb_inkeyReset(); /* reset keyboard buffer */

   while( sp_sl_first )
   {
      /* Free all set listeners */
      sp_sl_last = sp_sl_first->next;
      hb_xfree( sp_sl_first );
      sp_sl_first = sp_sl_last;
   }

   hb_setFreeSetPath();
}

int hb_setListenerAdd( HB_SET_LISTENER_CALLBACK * callback )
{
   PHB_SET_LISTENER p_sl = (PHB_SET_LISTENER) hb_xgrab( sizeof( HB_SET_LISTENER ) );
   p_sl->callback = callback;
   p_sl->listener = s_next_listener++;
   p_sl->next = NULL;
   if( sp_sl_last ) sp_sl_last->next = p_sl;
   else if( ! sp_sl_first ) sp_sl_first = p_sl;
   sp_sl_last = p_sl;
   return p_sl->listener;
}

void hb_setListenerNotify( HB_set_enum set, HB_set_listener_enum when )
{
   PHB_SET_LISTENER p_sl = sp_sl_first;
   while( p_sl )
   {
      (* p_sl->callback)( set, when );
      p_sl = p_sl->next;
   }
}

int hb_setListenerRemove( int listener )
{
   PHB_SET_LISTENER p_sl = sp_sl_first;
   PHB_SET_LISTENER p_sl_prev = NULL;
   while( p_sl )
   {
      if( listener == p_sl->listener )
      {
         listener = -listener;
         if( p_sl_prev ) p_sl_prev->next = p_sl->next;
         else sp_sl_first = p_sl->next;
         if( p_sl == sp_sl_last ) sp_sl_last = p_sl_prev;
         hb_xfree( p_sl );
         p_sl = NULL;
      }
      if( p_sl )
      {
         p_sl_prev = p_sl;
         p_sl = p_sl->next;
      }
   }
   return listener;
}

HB_EXPORT BOOL    hb_setGetL( HB_set_enum set_specifier )
{
   switch( set_specifier )
   {
      case HB_SET_ALTERNATE:
         return hb_set.HB_SET_ALTERNATE;
      case HB_SET_AUTOPEN:
         return hb_set.HB_SET_AUTOPEN;
      case HB_SET_BELL:
         return hb_set.HB_SET_BELL;
      case HB_SET_CANCEL:
         return hb_set.HB_SET_CANCEL;
      case HB_SET_CONFIRM:
         return hb_set.HB_SET_CONFIRM;
      case HB_SET_CONSOLE:
         return hb_set.HB_SET_CONSOLE;
      case HB_SET_DEBUG:
         return hb_set.HB_SET_DEBUG;
      case HB_SET_DELETED:
         return hb_set.HB_SET_DELETED;
      case HB_SET_DELIMITERS:
         return hb_set.HB_SET_DELIMITERS;
      case HB_SET_EOF:
         return hb_set.HB_SET_EOF;
      case HB_SET_ESCAPE:
         return hb_set.HB_SET_ESCAPE;
      case HB_SET_EXACT:
         return hb_set.HB_SET_EXACT;
      case HB_SET_EXCLUSIVE:
         return hb_set.HB_SET_EXCLUSIVE;
      case HB_SET_EXIT:
         return hb_set.HB_SET_EXIT;
      case HB_SET_EXTRA:
         return hb_set.HB_SET_EXTRA;
      case HB_SET_FIXED:
         return hb_set.HB_SET_FIXED;
      case HB_SET_IDLEREPEAT:
         return hb_set.HB_SET_IDLEREPEAT;
      case HB_SET_INSERT:
         return hb_set.HB_SET_INSERT;
      case HB_SET_INTENSITY:
         return hb_set.HB_SET_INTENSITY;
      case HB_SET_MCENTER:
         return hb_set.HB_SET_MCENTER;
      case HB_SET_OPTIMIZE:
         return hb_set.HB_SET_OPTIMIZE;
      case HB_SET_FORCEOPT:
         return hb_set.HB_SET_FORCEOPT;
      case HB_SET_PRINTER:
         return hb_set.HB_SET_PRINTER;
      case HB_SET_SCOREBOARD:
         return hb_set.HB_SET_SCOREBOARD;
      case HB_SET_SCROLLBREAK:
         return hb_set.HB_SET_SCROLLBREAK;
      case HB_SET_SOFTSEEK:
         return hb_set.HB_SET_SOFTSEEK;
      case HB_SET_STRICTREAD:
         return hb_set.HB_SET_STRICTREAD;
      case HB_SET_UNIQUE:
         return hb_set.HB_SET_UNIQUE;
      case HB_SET_WRAP:
         return hb_set.HB_SET_WRAP;
      case HB_SET_HARDCOMMIT:
         return hb_set.HB_SET_HARDCOMMIT;
      case HB_SET_DEFEXTENSIONS:
         return hb_set.HB_SET_DEFEXTENSIONS;
      case HB_SET_TRIMFILENAME:
         return hb_set.HB_SET_TRIMFILENAME;

      case HB_SET_ALTFILE:
      case HB_SET_AUTORDER:
      case HB_SET_AUTOSHARE:
      case HB_SET_COLOR:
      case HB_SET_CURSOR:
      case HB_SET_DATEFORMAT:
      case HB_SET_DECIMALS:
      case HB_SET_DEFAULT:
      case HB_SET_DELIMCHARS:
      case HB_SET_DEVICE:
      case HB_SET_EPOCH:
      case HB_SET_EVENTMASK:
      case HB_SET_EXTRAFILE:
      case HB_SET_MARGIN:
      case HB_SET_MBLOCKSIZE:
      case HB_SET_MESSAGE:
      case HB_SET_MFILEEXT:
      case HB_SET_PATH:
      case HB_SET_PRINTFILE:
      case HB_SET_TYPEAHEAD:
      case HB_SET_VIDEOMODE:
      case HB_SET_LANGUAGE:
      case HB_SET_FILECASE:
      case HB_SET_DIRCASE:
      case HB_SET_DIRSEPARATOR:
      case HB_SET_DBFLOCKSCHEME:
      case HB_SET_EOL:
      case HB_SET_HBOUTLOG:
      case HB_SET_HBOUTLOGINFO:
      case HB_SET_INVALID_:
         break;
#if 0 
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, 0 );
   return FALSE;
}

HB_EXPORT char *  hb_setGetCPtr( HB_set_enum set_specifier )
{
   switch( set_specifier )
   {
      case HB_SET_ALTFILE:
         return hb_set.HB_SET_ALTFILE;
      case HB_SET_COLOR:
         return hb_set.HB_SET_COLOR;
      case HB_SET_DATEFORMAT:
         return hb_set.HB_SET_DATEFORMAT;
      case HB_SET_DEFAULT:
         return hb_set.HB_SET_DEFAULT;
      case HB_SET_DELIMCHARS:
         return hb_set.HB_SET_DELIMCHARS;
      case HB_SET_DEVICE:
         return hb_set.HB_SET_DEVICE;
      case HB_SET_EXTRAFILE:
         return hb_set.HB_SET_EXTRAFILE;
      case HB_SET_PATH:
         return hb_set.HB_SET_PATH;
      case HB_SET_MFILEEXT:
         return hb_set.HB_SET_MFILEEXT;
      case HB_SET_PRINTFILE:
         return hb_set.HB_SET_PRINTFILE;
      case HB_SET_EOL:
         return hb_set.HB_SET_EOL;
      case HB_SET_HBOUTLOG:
         return hb_set.HB_SET_HBOUTLOG;
      case HB_SET_HBOUTLOGINFO:
         return hb_set.HB_SET_HBOUTLOGINFO;

      case HB_SET_ALTERNATE:
      case HB_SET_AUTOPEN:
      case HB_SET_AUTORDER:
      case HB_SET_AUTOSHARE:
      case HB_SET_BELL:
      case HB_SET_CANCEL:
      case HB_SET_CONFIRM:
      case HB_SET_CONSOLE:
      case HB_SET_CURSOR:
      case HB_SET_DEBUG:
      case HB_SET_DECIMALS:
      case HB_SET_DELETED:
      case HB_SET_DELIMITERS:
      case HB_SET_EOF:
      case HB_SET_EPOCH:
      case HB_SET_ESCAPE:
      case HB_SET_EVENTMASK:
      case HB_SET_EXACT:
      case HB_SET_EXCLUSIVE:
      case HB_SET_EXIT:
      case HB_SET_EXTRA:
      case HB_SET_FIXED:
      case HB_SET_INSERT:
      case HB_SET_INTENSITY:
      case HB_SET_MARGIN:
      case HB_SET_MBLOCKSIZE:
      case HB_SET_MCENTER:
      case HB_SET_MESSAGE:
      case HB_SET_OPTIMIZE:
      case HB_SET_FORCEOPT:
      case HB_SET_STRICTREAD:
      case HB_SET_HARDCOMMIT:
      case HB_SET_PRINTER:
      case HB_SET_SCOREBOARD:
      case HB_SET_SCROLLBREAK:
      case HB_SET_SOFTSEEK:
      case HB_SET_TYPEAHEAD:
      case HB_SET_UNIQUE:
      case HB_SET_VIDEOMODE:
      case HB_SET_WRAP:
      case HB_SET_LANGUAGE:
      case HB_SET_IDLEREPEAT:
      case HB_SET_FILECASE:
      case HB_SET_DIRCASE:
      case HB_SET_DIRSEPARATOR:
      case HB_SET_DBFLOCKSCHEME:
      case HB_SET_DEFEXTENSIONS:
      case HB_SET_TRIMFILENAME:
      case HB_SET_INVALID_:
         break;
#if 0 
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, 0 );
   return FALSE;
}

HB_EXPORT int     hb_setGetNI( HB_set_enum set_specifier )
{
   switch( set_specifier )
   {
      case HB_SET_AUTORDER:
         return hb_set.HB_SET_AUTORDER;
      case HB_SET_AUTOSHARE:
         return hb_set.HB_SET_AUTOSHARE;
      case HB_SET_DECIMALS:
         return hb_set.HB_SET_DECIMALS;
      case HB_SET_EPOCH:
         return hb_set.HB_SET_EPOCH;
      case HB_SET_EVENTMASK:
         return hb_set.HB_SET_EVENTMASK;
      case HB_SET_MARGIN:
         return hb_set.HB_SET_MARGIN;
      case HB_SET_MBLOCKSIZE:
         return hb_set.HB_SET_MBLOCKSIZE;
      case HB_SET_MESSAGE:
         return hb_set.HB_SET_MESSAGE;
      case HB_SET_TYPEAHEAD:
         return hb_set.HB_SET_TYPEAHEAD;
      case HB_SET_FILECASE:
         return hb_set.HB_SET_FILECASE;
      case HB_SET_DIRCASE:
         return hb_set.HB_SET_DIRCASE;
      case HB_SET_DIRSEPARATOR:
         return hb_set.HB_SET_DIRSEPARATOR;
      case HB_SET_VIDEOMODE:
         return hb_set.HB_SET_VIDEOMODE;
      case HB_SET_DBFLOCKSCHEME:
         return hb_set.HB_SET_DBFLOCKSCHEME;

      case HB_SET_ALTERNATE:
      case HB_SET_ALTFILE:
      case HB_SET_AUTOPEN:
      case HB_SET_BELL:
      case HB_SET_CANCEL:
      case HB_SET_COLOR:
      case HB_SET_CONFIRM:
      case HB_SET_CONSOLE:
      case HB_SET_CURSOR:
      case HB_SET_DATEFORMAT:
      case HB_SET_DEBUG:
      case HB_SET_DEFAULT:
      case HB_SET_DELETED:
      case HB_SET_DELIMCHARS:
      case HB_SET_DELIMITERS:
      case HB_SET_DEVICE:
      case HB_SET_EOF:
      case HB_SET_ESCAPE:
      case HB_SET_EXACT:
      case HB_SET_EXCLUSIVE:
      case HB_SET_EXIT:
      case HB_SET_EXTRA:
      case HB_SET_EXTRAFILE:
      case HB_SET_FIXED:
      case HB_SET_INSERT:
      case HB_SET_INTENSITY:
      case HB_SET_MCENTER:
      case HB_SET_MFILEEXT:
      case HB_SET_OPTIMIZE:
      case HB_SET_FORCEOPT:
      case HB_SET_STRICTREAD:
      case HB_SET_HARDCOMMIT:
      case HB_SET_PATH:
      case HB_SET_PRINTER:
      case HB_SET_PRINTFILE:
      case HB_SET_SCOREBOARD:
      case HB_SET_SCROLLBREAK:
      case HB_SET_SOFTSEEK:
      case HB_SET_UNIQUE:
      case HB_SET_WRAP:
      case HB_SET_LANGUAGE:
      case HB_SET_IDLEREPEAT:
      case HB_SET_EOL:
      case HB_SET_DEFEXTENSIONS:
      case HB_SET_TRIMFILENAME:
      case HB_SET_HBOUTLOG:
      case HB_SET_HBOUTLOGINFO:
      case HB_SET_INVALID_:
         break;
#if 0 
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, 0 );
   return FALSE;
}

HB_EXPORT long    hb_setGetNL( HB_set_enum set_specifier )
{
   return hb_setGetNI( set_specifier );
}

HB_EXPORT HB_PATHNAMES * hb_setGetFirstSetPath( void )
{
   return sp_set_path;
}


HB_EXPORT FHANDLE hb_setGetAltHan( void )
{
   return hb_set.hb_set_althan;
}

HB_EXPORT BOOL    hb_setGetCentury( void )
{
   return hb_set.hb_set_century;
}

HB_EXPORT FHANDLE hb_setGetExtraHan( void )
{
   return hb_set.hb_set_extrahan;
}

HB_EXPORT FHANDLE hb_setGetPrintHan( void )
{
   return hb_set.hb_set_printhan;
}

HB_EXPORT BOOL    hb_setGetAlternate( void )
{
   return hb_set.HB_SET_ALTERNATE;
}

HB_EXPORT char *  hb_setGetAltFile( void )
{
   return hb_set.HB_SET_ALTFILE;
}

HB_EXPORT BOOL    hb_setGetAutOpen( void )
{
   return hb_set.HB_SET_AUTOPEN;
}

HB_EXPORT int     hb_setGetAutOrder( void )
{
   return hb_set.HB_SET_AUTORDER;
}

HB_EXPORT int     hb_setGetAutoShare( void )
{
   return hb_set.HB_SET_AUTOSHARE;
}

HB_EXPORT BOOL    hb_setGetBell( void )
{
   return hb_set.HB_SET_BELL;
}

HB_EXPORT BOOL    hb_setGetCancel( void )
{
   return hb_set.HB_SET_CANCEL;
}

HB_EXPORT char *  hb_setGetColor( void )
{
   return hb_set.HB_SET_COLOR;
}

HB_EXPORT BOOL    hb_setGetConfirm( void )
{
   return hb_set.HB_SET_CONFIRM;
}

HB_EXPORT BOOL    hb_setGetConsole( void )
{
   return hb_set.HB_SET_CONSOLE;
}

HB_EXPORT char *  hb_setGetDateFormat( void )
{
   return hb_set.HB_SET_DATEFORMAT;
}

HB_EXPORT BOOL    hb_setGetDebug( void )
{
   return hb_set.HB_SET_DEBUG;
}

HB_EXPORT int     hb_setGetDecimals( void )
{
   return hb_set.HB_SET_DECIMALS;
}

HB_EXPORT char *  hb_setGetDefault( void )
{
   return hb_set.HB_SET_DEFAULT;
}

HB_EXPORT BOOL    hb_setGetDeleted( void )
{
   return hb_set.HB_SET_DELETED;
}

HB_EXPORT char *  hb_setGetDelimChars( void )
{
   return hb_set.HB_SET_DELIMCHARS;
}

HB_EXPORT BOOL    hb_setGetDelimiters( void )
{
   return hb_set.HB_SET_DELIMITERS;
}

HB_EXPORT char *  hb_setGetDevice( void )
{
   return hb_set.HB_SET_DEVICE;
}

HB_EXPORT BOOL    hb_setGetEOF( void )
{
   return hb_set.HB_SET_EOF;
}

HB_EXPORT int     hb_setGetEpoch( void )
{
   return hb_set.HB_SET_EPOCH;
}

HB_EXPORT BOOL    hb_setGetEscape( void )
{
   return hb_set.HB_SET_ESCAPE;
}

HB_EXPORT int     hb_setGetEventMask( void )
{
   return hb_set.HB_SET_EVENTMASK;
}

HB_EXPORT BOOL    hb_setGetExact( void )
{
   return hb_set.HB_SET_EXACT;
}

HB_EXPORT BOOL    hb_setGetExclusive( void )
{
   return hb_set.HB_SET_EXCLUSIVE;
}

HB_EXPORT BOOL    hb_setGetExit( void )
{
   return hb_set.HB_SET_EXIT;
}

HB_EXPORT BOOL    hb_setGetExtra( void )
{
   return hb_set.HB_SET_EXTRA;
}

HB_EXPORT char *  hb_setGetExtraFile( void )
{
   return hb_set.HB_SET_EXTRAFILE;
}

HB_EXPORT BOOL    hb_setGetFixed( void )
{
   return hb_set.HB_SET_FIXED;
}

HB_EXPORT BOOL    hb_setGetIdleRepeat( void )
{
   return hb_set.HB_SET_IDLEREPEAT;
}

HB_EXPORT BOOL    hb_setGetInsert( void )
{
   return hb_set.HB_SET_INSERT;
}

HB_EXPORT BOOL    hb_setGetIntensity( void )
{
   return hb_set.HB_SET_INTENSITY;
}

HB_EXPORT char *  hb_setGetPath( void )
{
   return hb_set.HB_SET_PATH;
}

HB_EXPORT int     hb_setGetMargin( void )
{
   return hb_set.HB_SET_MARGIN;
}

HB_EXPORT int     hb_setGetMBlockSize( void )
{
   return hb_set.HB_SET_MBLOCKSIZE;
}

HB_EXPORT BOOL    hb_setGetMCenter( void )
{
   return hb_set.HB_SET_MCENTER;
}

HB_EXPORT int     hb_setGetMessage( void )
{
   return hb_set.HB_SET_MESSAGE;
}

HB_EXPORT char *  hb_setGetMFileExt( void )
{
   return hb_set.HB_SET_MFILEEXT;
}

HB_EXPORT BOOL    hb_setGetOptimize( void )
{
   return hb_set.HB_SET_OPTIMIZE;
}

HB_EXPORT BOOL    hb_setGetPrinter( void )
{
   return hb_set.HB_SET_PRINTER;
}

HB_EXPORT char *  hb_setGetPrintFile( void )
{
   return hb_set.HB_SET_PRINTFILE;
}

HB_EXPORT BOOL    hb_setGetScoreBoard( void )
{
   return hb_set.HB_SET_SCOREBOARD;
}

HB_EXPORT BOOL    hb_setGetScrollBreak( void )
{
   return hb_set.HB_SET_SCROLLBREAK;
}

HB_EXPORT BOOL    hb_setGetSoftSeek( void )
{
   return hb_set.HB_SET_SOFTSEEK;
}

HB_EXPORT BOOL    hb_setGetStrictRead( void )
{
   return hb_set.HB_SET_STRICTREAD;
}

HB_EXPORT int     hb_setGetTypeAhead( void )
{
   return hb_set.HB_SET_TYPEAHEAD;
}

HB_EXPORT BOOL    hb_setGetUnique( void )
{
   return hb_set.HB_SET_UNIQUE;
}

HB_EXPORT int     hb_setGetFileCase( void )
{
   return hb_set.HB_SET_FILECASE;
}

HB_EXPORT int     hb_setGetDirCase( void )
{
   return hb_set.HB_SET_DIRCASE;
}

HB_EXPORT int     hb_setGetDirSeparator( void )
{
   return hb_set.HB_SET_DIRSEPARATOR;
}

HB_EXPORT int     hb_setGetVideoMode( void )
{
   return hb_set.HB_SET_VIDEOMODE;
}

HB_EXPORT BOOL    hb_setGetWrap( void )
{
   return hb_set.HB_SET_WRAP;
}

HB_EXPORT int     hb_setGetDBFLockScheme( void )
{
   return hb_set.HB_SET_DBFLOCKSCHEME;
}

HB_EXPORT BOOL    hb_setGetHardCommit( void )
{
   return hb_set.HB_SET_HARDCOMMIT;
}

HB_EXPORT BOOL    hb_setGetForceOpt( void )
{
   return hb_set.HB_SET_FORCEOPT;
}

HB_EXPORT BOOL    hb_setGetDefExtension( void )
{
   return hb_set.HB_SET_DEFEXTENSIONS;
}

HB_EXPORT char *  hb_setGetEOL( void )
{
   return hb_set.HB_SET_EOL;
}

HB_EXPORT BOOL    hb_setGetTrimFileName( void )
{
   return hb_set.HB_SET_TRIMFILENAME;
}

HB_EXPORT char *  hb_setGetHBOUTLOG( void )
{
   return hb_set.HB_SET_HBOUTLOG;
}

HB_EXPORT char *  hb_setGetHBOUTLOGINFO( void )
{
   return hb_set.HB_SET_HBOUTLOGINFO;
}
