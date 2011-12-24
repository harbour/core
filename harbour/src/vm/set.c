/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Set functions
 *
 * Copyright 1999-2003 David G. Holm <dholm@jsd-llc.com>
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
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2008-2009 Viktor Szakats (harbour syenar.hu)
 *    hb_osEncode()
 *    hb_osDecode()
 *
 * See COPYING for licensing terms.
 *
 */

#define _HB_SET_INTERNAL_

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapilng.h"
#include "hbapicdp.h"
#include "hbset.h"
#include "hbstack.h"

typedef struct HB_SET_LISTENER_
{
   int listener;
   HB_SET_LISTENER_CALLBACK * callback;
   struct HB_SET_LISTENER_ * next;
} HB_SET_LISTENER, * PHB_SET_LISTENER;

typedef struct
{
   PHB_SET_LISTENER  first;
   PHB_SET_LISTENER  last;
   int               counter;
} HB_SET_LISTENER_LST, * PHB_SET_LISTENER_LST;

static char set_char( PHB_ITEM pItem, char oldChar )
{
   char newChar = oldChar;

   HB_TRACE(HB_TR_DEBUG, ("set_char(%p, %c)", pItem, oldChar));

   if( HB_IS_STRING( pItem ) )
   {
      /* Only replace if string has at least one character. */
      HB_SIZE nLen = hb_itemGetCLen( pItem );
      if( nLen > 0 )
      {
         newChar = *hb_itemGetCPtr( pItem );
      }
   }
   return newChar;
}

/*
 * Change the setting if the parameter is a logical value, or is
 * either "ON" or "OFF" (regardless of case)
 */
static HB_BOOL set_logical( PHB_ITEM pItem, HB_BOOL bDefault )
{
   HB_BOOL bLogical = bDefault;

   HB_TRACE(HB_TR_DEBUG, ("set_logical(%p)", pItem));

   if( pItem )
   {
      if( HB_IS_LOGICAL( pItem ) )
         bLogical = hb_itemGetL( pItem );
      else if( HB_IS_STRING( pItem ) )
      {
         const char * szString = hb_itemGetCPtr( pItem );
         HB_SIZE nLen = hb_itemGetCLen( pItem );

         if( nLen >= 2
          && ( ( HB_UCHAR ) szString[ 0 ] == 'O' || ( HB_UCHAR ) szString[ 0 ] == 'o' )
          && ( ( HB_UCHAR ) szString[ 1 ] == 'N' || ( HB_UCHAR ) szString[ 1 ] == 'n' ) )
            bLogical = HB_TRUE;
         else if( nLen >= 3
          && ( ( HB_UCHAR ) szString[ 0 ] == 'O' || ( HB_UCHAR ) szString[ 0 ] == 'o' )
          && ( ( HB_UCHAR ) szString[ 1 ] == 'F' || ( HB_UCHAR ) szString[ 1 ] == 'f' )
          && ( ( HB_UCHAR ) szString[ 2 ] == 'F' || ( HB_UCHAR ) szString[ 2 ] == 'f' ) )
            bLogical = HB_FALSE;
      }
   }

   return bLogical;
}

static int set_number( PHB_ITEM pItem, int iOldValue )
{
   HB_TRACE(HB_TR_DEBUG, ("set_number(%p, %d)", pItem, iOldValue));

   return HB_IS_NUMERIC( pItem ) ? hb_itemGetNI( pItem ) : iOldValue;
}

static char * set_string( PHB_ITEM pItem, char * szOldString )
{
   char * szString;

   HB_TRACE(HB_TR_DEBUG, ("set_string(%p, %s)", pItem, szOldString));

   if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
   {
      if( szOldString )
         hb_xfree( szOldString );
      /* Limit size of SET strings to 64K, truncating if source is longer */
      szString = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
   }
   else
      szString = szOldString;

   return szString;
}

static void close_handle( PHB_SET_STRUCT pSet, HB_set_enum set_specifier )
{
   HB_FHANDLE * handle_ptr;

   HB_TRACE(HB_TR_DEBUG, ("close_handle(%p,%d)", pSet, ( int ) set_specifier));

   switch( set_specifier )
   {
      case HB_SET_ALTFILE:
         handle_ptr = &pSet->hb_set_althan;
         break;
      case HB_SET_PRINTFILE:
         handle_ptr = &pSet->hb_set_printhan;
         break;
      case HB_SET_EXTRAFILE:
         handle_ptr = &pSet->hb_set_extrahan;
         break;
      default:
         return;
   }

   if( *handle_ptr != FS_ERROR )
   {
      if( set_specifier != HB_SET_PRINTFILE && pSet->HB_SET_EOF )
         hb_fsWrite( *handle_ptr, "\x1A", 1 );
      hb_fsClose( *handle_ptr );
      *handle_ptr = FS_ERROR;
   }
}

static HB_BOOL is_devicename( const char * szFileName )
{
   if( szFileName && *szFileName )
   {
#if defined( HB_OS_OS2 ) || defined( HB_OS_WIN ) || defined( HB_OS_DOS )
      int iLen = ( int ) strlen( szFileName );
      if( ( iLen == 3 &&
            ( hb_stricmp( szFileName, "PRN" ) == 0 ||
              hb_stricmp( szFileName, "CON" ) == 0 ) ) ||
          ( iLen == 4 &&
            ( ( hb_strnicmp( szFileName, "LPT", 3 ) == 0 &&
                szFileName[3] >= '1' && szFileName[3] <= '3' ) ||
              ( hb_strnicmp( szFileName, "COM", 3 ) == 0 &&
                szFileName[3] >= '1' && szFileName[3] <= '9' ) ) ) )
      {
         return HB_TRUE;
      }
#elif defined( HB_OS_UNIX )
      if( strncmp( szFileName, "/dev/", 5 ) == 0 )
         return HB_TRUE;
      else
      {
         HB_FATTR ulAttr = 0;
         if( hb_fsGetAttr( szFileName, &ulAttr ) )
         {
            if( ulAttr & ( HB_FA_CHRDEVICE | HB_FA_BLKDEVICE | HB_FA_FIFO | HB_FA_SOCKET ) )
               return HB_TRUE;
         }
      }
#endif
   }
   return HB_FALSE;
}

static void open_handle( PHB_SET_STRUCT pSet, const char * file_name,
                         HB_BOOL bAppend, HB_set_enum set_specifier )
{
   PHB_ITEM pError = NULL;
   HB_FHANDLE handle, * handle_ptr;
   HB_ERRCODE uiError;
   char * szFileName = NULL, ** set_value;
   const char * def_ext;
   HB_BOOL bPipe = HB_FALSE;

   HB_TRACE(HB_TR_DEBUG, ("open_handle(%p, %s, %d, %d)", pSet, file_name, (int) bAppend, (int) set_specifier));

   switch( set_specifier )
   {
      case HB_SET_ALTFILE:
         uiError = 2013;
         set_value = &pSet->HB_SET_ALTFILE;
         handle_ptr = &pSet->hb_set_althan;
         def_ext = ".txt";
         break;
      case HB_SET_PRINTFILE:
         uiError = 2014;
         set_value = &pSet->HB_SET_PRINTFILE;
         handle_ptr = &pSet->hb_set_printhan;
         def_ext = ".prn";
         break;
      case HB_SET_EXTRAFILE:
         uiError = 2015;
         set_value = &pSet->HB_SET_EXTRAFILE;
         handle_ptr = &pSet->hb_set_extrahan;
         def_ext = ".prn";
         break;
      default:
         return;
   }

   close_handle( pSet, set_specifier );

   if( file_name && file_name[ 0 ] != '\0' )
   {
      /* Create full filename */
#if defined( HB_OS_UNIX )
      bPipe = file_name[ 0 ] == '|';
      if( bPipe )
      {
         szFileName = hb_strdup( file_name );
         bAppend = HB_FALSE;
      }
#endif
      if( ! bPipe )
      {
         char path[ HB_PATH_MAX ];
         PHB_FNAME pFilename = hb_fsFNameSplit( file_name );

         if( is_devicename( file_name ) )
         {
#if defined( HB_OS_OS2 ) || defined( HB_OS_WIN ) || defined( HB_OS_DOS )
            hb_strupr( ( char * ) pFilename->szName );
#endif
         }
         else
         {
            if( pFilename->szExtension == NULL && def_ext && pSet->HB_SET_DEFEXTENSIONS )
               pFilename->szExtension = def_ext;

            if( pFilename->szPath == NULL && pSet->HB_SET_DEFAULT )
               pFilename->szPath = pSet->HB_SET_DEFAULT;
         }
         hb_fsFNameMerge( path, pFilename );
         hb_xfree( pFilename );
         szFileName = hb_strdup( path );
      }
   }

   /* free the old value before setting the new one (CA-Cl*pper does it.
    * This code must be executed after setting szFileName, [druzus]
    */
   if( *set_value )
      hb_xfree( *set_value );
   *set_value = NULL;

   if( ! szFileName )
      return;

   /* Open the file either in append (bAppend) or truncate mode (!bAppend), but
      always use binary mode */

   /* QUESTION: What sharing mode does Clipper use ? [vszakats] */

   handle = FS_ERROR;
   while( handle == FS_ERROR )
   {
      if( bPipe )
         handle = hb_fsPOpen( szFileName + 1, "w" );
      else
      {
         HB_BOOL bCreate = HB_FALSE;

         if( bAppend )
         {  /* Append mode */
            if( hb_fsFileExists( szFileName ) )
            {  /* If the file already exists, open it (in read-write mode, in
                  case of non-Unix and text modes). */
               handle = hb_fsOpen( szFileName, FO_READWRITE | FO_DENYWRITE );
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
                     hb_fsRead( handle, &cEOF, 1 );   /* Read the last char. */
                     if( cEOF == '\x1A' )             /* If it's an EOF, */
                     {
                        hb_fsSeek( handle, -1, FS_END ); /* Then write over it. */
                     }
                  }
               }
            }
            else
               bCreate = HB_TRUE; /* Otherwise create a new file. */
         }
         else
            bCreate = HB_TRUE; /* Always create a new file for overwrite mode. */

         if( bCreate )
            handle = hb_fsCreate( szFileName, FC_NORMAL );
      }

      if( handle == FS_ERROR )
      {
         pError = hb_errRT_FileError( pError, HB_ERR_SS_TERMINAL, EG_CREATE, uiError, szFileName );
         if( hb_errLaunch( pError ) != E_RETRY )
            break;
      }
   }

   if( pError )
      hb_itemRelease( pError );

   if( handle == FS_ERROR )
   {
      hb_xfree( szFileName );
      szFileName = NULL;
   }

   /* user RT error handler can open it too so we have to
    * close it again if necessary
    */
   close_handle( pSet, set_specifier );
   * handle_ptr = handle;
   if( *set_value )
      hb_xfree( *set_value );
   *set_value = szFileName;

   return;
}

HB_BOOL hb_setSetCentury( HB_BOOL new_century_setting )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();
   HB_BOOL old_century_setting = pSet->hb_set_century;

   pSet->hb_set_century = new_century_setting;
   /*
    * if the setting changed, adjust the current date format to use
    * the correct number of year digits.
    */
   if( old_century_setting != new_century_setting )
   {
      int count, digit, size, y_size, y_start, y_stop;
      char * szDateFormat, * szNewFormat;

      /* Convert to upper case and determine where year is */
      y_start = y_stop = -1;
      szDateFormat = pSet->HB_SET_DATEFORMAT;
      size = ( int ) strlen( szDateFormat );
      for( count = 0; count < size; count++ )
      {
         digit = HB_TOUPPER( ( HB_UCHAR ) szDateFormat[ count ] );
         if( digit == 'Y' )
         {
            if( y_start == -1 )
               y_start = count;
         }
         else if( y_start > -1 && y_stop == -1 )
            y_stop = count;
         szDateFormat[ count ] = ( char ) digit;
      }
      /* Determine size of year in current format */
      if( y_start < 0 )
      {
         y_start = 0; /* There is no year in the current format */
         y_stop = 0;
      }
      else if( y_stop < 0 )
         y_stop = size; /* All digits are year digits */
      y_size = y_stop - y_start;
      /* Calculate size of new format */
      size -= y_size;
      if( new_century_setting )
         size += 4;
      else size += 2;

      /* Create the new date format */
      szNewFormat = ( char * ) hb_xgrab( size + 1 );

      {
         int format_len;
         if( y_start > 0 ) memcpy( szNewFormat, szDateFormat, y_start );
         szNewFormat[ y_start ] = '\0';
         hb_strncat( szNewFormat, "YY", size );
         if( new_century_setting )
            hb_strncat( szNewFormat, "YY", size );
         format_len = ( int ) strlen( szDateFormat );
         if( y_stop < format_len )
            hb_strncat( szNewFormat, szDateFormat + y_stop, size );
         /* DATE FORMAT is under direct control of SET, so notify when it
            it is changed indirectly via __SETCENTURY() */
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_BEFORE );
         hb_xfree( szDateFormat );
         pSet->HB_SET_DATEFORMAT = szNewFormat;
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_AFTER );
      }
   }

   /* Return the previous setting */
   return old_century_setting;
}

HB_FUNC( __SETCENTURY )
{
   HB_STACK_TLS_PRELOAD
   HB_BOOL old_century_setting = hb_setGetCentury();
   PHB_ITEM pNewVal = hb_param( 1, HB_IT_ANY );

   if( pNewVal )
      hb_setSetCentury( set_logical( pNewVal, old_century_setting ) );

   hb_retl( old_century_setting );
}

HB_FUNC( SETCANCEL )
{
   HB_STACK_TLS_PRELOAD
   hb_retl( hb_setGetCancel() );
   /* SETCANCEL() accepts only logical parameters */
   hb_setSetItem( HB_SET_CANCEL, hb_param( 1, HB_IT_LOGICAL ) );
}

/* return default printer device */
static char * hb_set_PRINTFILE_default( void )
{
#if defined( HB_OS_UNIX )
   return hb_strdup( "|lpr" );
#elif defined( HB_OS_DOS )
   return hb_strdup( "PRN" );
#elif defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
   return hb_strdup( "LPT1" );
#else
   return hb_strdup( "PRN" ); /* TOFIX */
#endif
}

HB_FUNC( SET )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();
   int args = hb_pcount();
   HB_set_enum set_specifier = ( args > 0 ) ? ( HB_set_enum ) hb_parni( 1 ) : HB_SET_INVALID_;
   PHB_ITEM pArg2 = ( args > 1 ) ? hb_param( 2, HB_IT_ANY ) : NULL;
   PHB_ITEM pArg3 = ( args > 2 ) ? hb_param( 3, HB_IT_ANY ) : NULL;

   if( args > 1 )
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );

   switch( set_specifier )
   {
      case HB_SET_ALTERNATE:
         hb_retl( pSet->HB_SET_ALTERNATE );
         if( args > 1 )
            pSet->HB_SET_ALTERNATE = set_logical( pArg2, pSet->HB_SET_ALTERNATE );
         break;
      case HB_SET_ALTFILE:
         hb_retc( pSet->HB_SET_ALTFILE );
         if( pArg2 && HB_IS_STRING( pArg2 ) )
            open_handle( pSet, hb_itemGetCPtr( pArg2 ), set_logical( pArg3, HB_FALSE ), HB_SET_ALTFILE );
         break;
      case HB_SET_AUTOPEN:
         hb_retl( pSet->HB_SET_AUTOPEN );
         if( args > 1 )
            pSet->HB_SET_AUTOPEN = set_logical( pArg2, pSet->HB_SET_AUTOPEN );
         break;
      case HB_SET_AUTORDER:
         hb_retni( pSet->HB_SET_AUTORDER );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_AUTORDER ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_AUTORDER = set_number( pArg2, pSet->HB_SET_AUTORDER );
         }
         break;
      case HB_SET_AUTOSHARE:
         hb_retni( pSet->HB_SET_AUTOSHARE );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_AUTOSHARE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_AUTOSHARE = set_number( pArg2, pSet->HB_SET_AUTOSHARE );
         }
         break;
      case HB_SET_BELL:
         hb_retl( pSet->HB_SET_BELL );
         if( args > 1 )
            pSet->HB_SET_BELL = set_logical( pArg2, pSet->HB_SET_BELL );
         break;
      case HB_SET_CANCEL:
         hb_retl( pSet->HB_SET_CANCEL );
         if( args > 1 )
            pSet->HB_SET_CANCEL = set_logical( pArg2, pSet->HB_SET_CANCEL );
         break;
      case HB_SET_COLOR:
         hb_retc( hb_conSetColor( args >= 2 && HB_IS_STRING( pArg2 ) ? hb_itemGetCPtr( pArg2 ) : NULL ) );
         break;
      case HB_SET_CONFIRM:
         hb_retl( pSet->HB_SET_CONFIRM );
         if( args > 1 )
            pSet->HB_SET_CONFIRM = set_logical( pArg2, pSet->HB_SET_CONFIRM );
         break;
      case HB_SET_CONSOLE:
         hb_retl( pSet->HB_SET_CONSOLE );
         if( args > 1 )
            pSet->HB_SET_CONSOLE = set_logical( pArg2, pSet->HB_SET_CONSOLE );
         break;
      case HB_SET_CURSOR:
         if( args >= 2 && HB_IS_NUMERIC( pArg2 ) )
            hb_retni( hb_conSetCursor( HB_TRUE, hb_itemGetNI( pArg2 ) ) );
         else
            hb_retni( hb_conSetCursor( HB_FALSE, 0 ) );
         break;
      case HB_SET_DATEFORMAT:
         hb_retc( pSet->HB_SET_DATEFORMAT );
         if( args > 1 )
         {
            char * value;
            int year = 0;

            value = pSet->HB_SET_DATEFORMAT = set_string( pArg2, pSet->HB_SET_DATEFORMAT );
            while( *value )
            {
               if( *value == 'Y' || *value == 'y' )
                  year++;
               else if( year )   /* Only count the first set of consecutive "Y"s. */
                  break;
               ++value;
            }
            /* CENTURY is not controlled directly by SET, so there is no
               notification for changing it indirectly via DATE FORMAT. */
            pSet->hb_set_century = year >= 4;
         }
         break;
      case HB_SET_TIMEFORMAT:
         hb_retc( pSet->HB_SET_TIMEFORMAT );
         if( args > 1 )
            pSet->HB_SET_TIMEFORMAT = set_string( pArg2, pSet->HB_SET_TIMEFORMAT );
         break;
      case HB_SET_DEBUG:
         hb_retl( pSet->HB_SET_DEBUG );
         if( args > 1 )
            pSet->HB_SET_DEBUG = set_logical( pArg2, pSet->HB_SET_DEBUG );
         break;
      case HB_SET_DECIMALS:
         hb_retni( pSet->HB_SET_DECIMALS );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_DECIMALS ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_DECIMALS = set_number( pArg2, pSet->HB_SET_DECIMALS );
         }
         break;
      case HB_SET_DEFAULT:
         hb_retc( pSet->HB_SET_DEFAULT );
         if( args > 1 )
            pSet->HB_SET_DEFAULT = set_string( pArg2, pSet->HB_SET_DEFAULT );
         break;
      case HB_SET_DELETED:
         hb_retl( pSet->HB_SET_DELETED );
         if( args > 1 )
            pSet->HB_SET_DELETED = set_logical( pArg2, pSet->HB_SET_DELETED );
         break;
      case HB_SET_DELIMCHARS:
         hb_retc( pSet->HB_SET_DELIMCHARS );
         if( args > 1 )
            pSet->HB_SET_DELIMCHARS = set_string( pArg2, pSet->HB_SET_DELIMCHARS );
         break;
      case HB_SET_DELIMITERS:
         hb_retl( pSet->HB_SET_DELIMITERS );
         if( args > 1 )
            pSet->HB_SET_DELIMITERS = set_logical( pArg2, pSet->HB_SET_DELIMITERS );
         break;
      case HB_SET_DEVICE:
         hb_retc( pSet->HB_SET_DEVICE );
         if( pArg2 && HB_IS_STRING( pArg2 ) )
         {
            /* If the print file is not already open, open it in overwrite mode. */
            pSet->HB_SET_DEVICE = set_string( pArg2, pSet->HB_SET_DEVICE );
            pSet->hb_set_prndevice = hb_stricmp( pSet->HB_SET_DEVICE, "PRINTER" ) == 0;
         }
         break;
      case HB_SET_EOF:
         hb_retl( pSet->HB_SET_EOF );
         if( args > 1 )
            pSet->HB_SET_EOF = set_logical( pArg2, pSet->HB_SET_EOF );
         break;
      case HB_SET_EPOCH:
         hb_retni( pSet->HB_SET_EPOCH );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_EPOCH ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_EPOCH = set_number( pArg2, pSet->HB_SET_EPOCH );
         }
         break;
      case HB_SET_ESCAPE:
         hb_retl( pSet->HB_SET_ESCAPE );
         if( args > 1 )
            pSet->HB_SET_ESCAPE = set_logical( pArg2, pSet->HB_SET_ESCAPE );
         break;
      case HB_SET_EVENTMASK:
         hb_retni( pSet->HB_SET_EVENTMASK );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_EVENTMASK ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_EVENTMASK = set_number( pArg2, pSet->HB_SET_EVENTMASK );
         }
         break;
      case HB_SET_EXACT:
         hb_retl( pSet->HB_SET_EXACT );
         if( args > 1 )
            pSet->HB_SET_EXACT = set_logical( pArg2, pSet->HB_SET_EXACT );
         break;
      case HB_SET_EXCLUSIVE:
         hb_retl( pSet->HB_SET_EXCLUSIVE );
         if( args > 1 )
            pSet->HB_SET_EXCLUSIVE = set_logical( pArg2, pSet->HB_SET_EXCLUSIVE );
         break;
      case HB_SET_EXIT:
         hb_retl( pSet->HB_SET_EXIT );
         /* NOTE: Otherwise ReadExit() will always set the value. [vszakats] */
         if( pArg2 != NULL && !HB_IS_NIL( pArg2 ) )
            pSet->HB_SET_EXIT = set_logical( pArg2, pSet->HB_SET_EXIT );
         break;
      case HB_SET_EXTRA:
         hb_retl( pSet->HB_SET_EXTRA );
         if( args > 1 )
            pSet->HB_SET_EXTRA = set_logical( pArg2, pSet->HB_SET_EXTRA );
         break;
      case HB_SET_EXTRAFILE:
         hb_retc( pSet->HB_SET_EXTRAFILE );
         if( pArg2 && HB_IS_STRING( pArg2 ) )
            open_handle( pSet, hb_itemGetCPtr( pArg2 ), set_logical( pArg3, HB_FALSE ), HB_SET_EXTRAFILE );
         break;
      case HB_SET_FIXED:
         hb_retl( pSet->HB_SET_FIXED );
         if( args > 1 )
            pSet->HB_SET_FIXED = set_logical( pArg2, pSet->HB_SET_FIXED );
         break;
      case HB_SET_INSERT:
         hb_retl( pSet->HB_SET_INSERT );
         if( args > 1 )
            pSet->HB_SET_INSERT = set_logical( pArg2, pSet->HB_SET_INSERT );
         break;
      case HB_SET_INTENSITY:
         hb_retl( pSet->HB_SET_INTENSITY );
         if( args > 1 )
            pSet->HB_SET_INTENSITY = set_logical( pArg2, pSet->HB_SET_INTENSITY );
         break;
      case HB_SET_MARGIN:
         hb_retni( pSet->HB_SET_MARGIN );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_MARGIN ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_MARGIN = set_number( pArg2, pSet->HB_SET_MARGIN );
         }
         break;
      case HB_SET_MBLOCKSIZE:
         hb_retni( pSet->HB_SET_MBLOCKSIZE );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_MBLOCKSIZE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_MBLOCKSIZE = set_number( pArg2, pSet->HB_SET_MBLOCKSIZE );
         }
         break;
      case HB_SET_MCENTER:
         hb_retl( pSet->HB_SET_MCENTER );
         if( args > 1 )
            pSet->HB_SET_MCENTER = set_logical( pArg2, pSet->HB_SET_MCENTER );
         break;
      case HB_SET_MESSAGE:
         hb_retni( pSet->HB_SET_MESSAGE );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_MESSAGE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_MESSAGE = set_number( pArg2, pSet->HB_SET_MESSAGE );
         }
         break;
      case HB_SET_MFILEEXT:
         hb_retc( pSet->HB_SET_MFILEEXT );
         if( args > 1 )
            pSet->HB_SET_MFILEEXT = set_string( pArg2, pSet->HB_SET_MFILEEXT );
         break;
      case HB_SET_OPTIMIZE:
         hb_retl( pSet->HB_SET_OPTIMIZE );
         if( args > 1 )
            pSet->HB_SET_OPTIMIZE = set_logical( pArg2, pSet->HB_SET_OPTIMIZE );
         break;
      case HB_SET_FORCEOPT:
         hb_retl( pSet->HB_SET_FORCEOPT );
         if( args > 1 )
            pSet->HB_SET_FORCEOPT = set_logical( pArg2, pSet->HB_SET_FORCEOPT );
         break;
      case HB_SET_STRICTREAD:
         hb_retl( pSet->HB_SET_STRICTREAD );
         if( args > 1 )
            pSet->HB_SET_STRICTREAD = set_logical( pArg2, pSet->HB_SET_STRICTREAD );
         break;
      case HB_SET_HARDCOMMIT:
         hb_retl( pSet->HB_SET_HARDCOMMIT );
         if( args > 1 )
            pSet->HB_SET_HARDCOMMIT = set_logical( pArg2, pSet->HB_SET_HARDCOMMIT );
         break;
      case HB_SET_PATH:
         hb_retc( pSet->HB_SET_PATH );
         if( args > 1 )
         {
            pSet->HB_SET_PATH = set_string( pArg2, pSet->HB_SET_PATH );
            hb_fsFreeSearchPath( pSet->hb_set_path );
            pSet->hb_set_path = NULL;
            hb_fsAddSearchPath( pSet->HB_SET_PATH, &pSet->hb_set_path );
         }
         break;
      case HB_SET_PRINTER:
         hb_retl( pSet->HB_SET_PRINTER );
         if( args > 1 )
            pSet->HB_SET_PRINTER = set_logical( pArg2, pSet->HB_SET_PRINTER );
         break;
      case HB_SET_PRINTFILE:
         hb_retc( pSet->HB_SET_PRINTFILE );
         if( pArg2 && HB_IS_STRING( pArg2 ) )
         {
            open_handle( pSet, hb_itemGetCPtr( pArg2 ), set_logical( pArg3, HB_FALSE ), HB_SET_PRINTFILE );
            /* With SET PRINTER TO or Set( _SET_PRINTFILE, "" ) are expected to activate the default printer [jarabal] */
            if( pSet->HB_SET_PRINTFILE == NULL )
               pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();
         }
         break;
      case HB_SET_SCOREBOARD:
         hb_retl( pSet->HB_SET_SCOREBOARD );
         if( args > 1 )
            pSet->HB_SET_SCOREBOARD = set_logical( pArg2, pSet->HB_SET_SCOREBOARD );
         break;
      case HB_SET_SCROLLBREAK:
         hb_retl( pSet->HB_SET_SCROLLBREAK );
         if( args > 1 )
            pSet->HB_SET_SCROLLBREAK = set_logical( pArg2, pSet->HB_SET_SCROLLBREAK );
         break;
      case HB_SET_SOFTSEEK:
         hb_retl( pSet->HB_SET_SOFTSEEK );
         if( args > 1 )
            pSet->HB_SET_SOFTSEEK = set_logical( pArg2, pSet->HB_SET_SOFTSEEK );
         break;
      case HB_SET_TYPEAHEAD:
         hb_retni( pSet->HB_SET_TYPEAHEAD );
         if( args > 1 )
         {
            /* Set the value and limit the range */
            pSet->HB_SET_TYPEAHEAD = set_number( pArg2, pSet->HB_SET_TYPEAHEAD );
            if( pSet->HB_SET_TYPEAHEAD == 0 )
               /* Do nothing */ ;
            else if( pSet->HB_SET_TYPEAHEAD < 16 )
               pSet->HB_SET_TYPEAHEAD = 16;
            else if( pSet->HB_SET_TYPEAHEAD > 4096 )
               pSet->HB_SET_TYPEAHEAD = 4096;
            /* reset keyboard buffer */
            hb_inkeyReset();
         }
         break;
      case HB_SET_UNIQUE:
         hb_retl( pSet->HB_SET_UNIQUE );
         if( args > 1 )
            pSet->HB_SET_UNIQUE = set_logical( pArg2, pSet->HB_SET_UNIQUE );
         break;
      case HB_SET_VIDEOMODE:
         hb_retni( pSet->HB_SET_VIDEOMODE );
         if( args > 1 )
            pSet->HB_SET_VIDEOMODE = set_number( pArg2, pSet->HB_SET_VIDEOMODE );
         break;
      case HB_SET_WRAP:
         hb_retl( pSet->HB_SET_WRAP );
         if( args > 1 )
            pSet->HB_SET_WRAP = set_logical( pArg2, pSet->HB_SET_WRAP );
         break;
      case HB_SET_LANGUAGE:
         hb_retc( hb_langID() );
         if( args > 1 && HB_IS_STRING( pArg2 ) )
            hb_langSelectID( hb_itemGetCPtr( pArg2 ) );
         break;
      case HB_SET_CODEPAGE:
         hb_retc( hb_cdpID() );
         if( args > 1 && HB_IS_STRING( pArg2 ) )
            hb_cdpSelectID( hb_itemGetCPtr( pArg2 ) );
         break;
      case HB_SET_IDLEREPEAT:
         hb_retl( pSet->HB_SET_IDLEREPEAT );
         if( args > 1 )
            pSet->HB_SET_IDLEREPEAT = set_logical( pArg2, pSet->HB_SET_IDLEREPEAT );
         break;
      case HB_SET_FILECASE:
         hb_retni( pSet->HB_SET_FILECASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "LOWER" ) )
                  pSet->HB_SET_FILECASE = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "UPPER" ) )
                  pSet->HB_SET_FILECASE = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "MIXED" ) )
                  pSet->HB_SET_FILECASE = HB_SET_CASE_MIXED;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               int iValue = set_number( pArg2, pSet->HB_SET_FILECASE );
               if( iValue == HB_SET_CASE_LOWER ||
                   iValue == HB_SET_CASE_UPPER ||
                   iValue == HB_SET_CASE_MIXED )
                  pSet->HB_SET_FILECASE = iValue;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
         }
         break;
      case HB_SET_DIRCASE:
         hb_retni( pSet->HB_SET_DIRCASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "LOWER" ) )
                  pSet->HB_SET_DIRCASE = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "UPPER" ) )
                  pSet->HB_SET_DIRCASE = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "MIXED" ) )
                  pSet->HB_SET_DIRCASE = HB_SET_CASE_MIXED;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               int iValue = set_number( pArg2, pSet->HB_SET_DIRCASE );
               if( iValue == HB_SET_CASE_LOWER ||
                   iValue == HB_SET_CASE_UPPER ||
                   iValue == HB_SET_CASE_MIXED )
                  pSet->HB_SET_DIRCASE = iValue;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
         }
         break;
      case HB_SET_DIRSEPARATOR:
      {
         char szDirSep[ 2 ];
         szDirSep[ 0 ] = ( char ) pSet->HB_SET_DIRSEPARATOR;
         szDirSep[ 1 ] = '\0';
         hb_retc( szDirSep );
         if( args > 1 )
            pSet->HB_SET_DIRSEPARATOR = set_char( pArg2, ( char ) pSet->HB_SET_DIRSEPARATOR );
         break;
      }
      case HB_SET_DBFLOCKSCHEME:
         hb_retni( pSet->HB_SET_DBFLOCKSCHEME );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_DBFLOCKSCHEME ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
            else
               pSet->HB_SET_DBFLOCKSCHEME = set_number( pArg2, pSet->HB_SET_DBFLOCKSCHEME );
         }
         break;
      case HB_SET_DEFEXTENSIONS:
         hb_retl( pSet->HB_SET_DEFEXTENSIONS );
         if( args > 1 )
            pSet->HB_SET_DEFEXTENSIONS = set_logical( pArg2, pSet->HB_SET_DEFEXTENSIONS );
         break;
      case HB_SET_EOL:
         hb_retc( pSet->HB_SET_EOL );
         if( args > 1 )
            pSet->HB_SET_EOL = set_string( pArg2, pSet->HB_SET_EOL );
         break;
      case HB_SET_TRIMFILENAME:
         hb_retl( pSet->HB_SET_TRIMFILENAME );
         if( args > 1 )
            pSet->HB_SET_TRIMFILENAME = set_logical( pArg2, pSet->HB_SET_TRIMFILENAME );
         break;
      case HB_SET_HBOUTLOG:
         hb_retc( pSet->HB_SET_HBOUTLOG );
         if( args > 1 && ( HB_IS_STRING( pArg2 ) || HB_IS_NIL( pArg2 ) ) )
         {
            if( pSet->HB_SET_HBOUTLOG )
               hb_xfree( pSet->HB_SET_HBOUTLOG );
            if( HB_IS_NIL( pArg2 ) )
               pSet->HB_SET_HBOUTLOG = NULL;
            else
               /* Limit size of SET strings to 64K, truncating if source is longer */
               pSet->HB_SET_HBOUTLOG = hb_strndup( hb_itemGetCPtr( pArg2 ), USHRT_MAX );
            hb_xsetfilename( pSet->HB_SET_HBOUTLOG );
         }
         break;
      case HB_SET_HBOUTLOGINFO:
         hb_retc( pSet->HB_SET_HBOUTLOGINFO );
         if( args > 1 )
         {
            pSet->HB_SET_HBOUTLOGINFO = set_string( pArg2, pSet->HB_SET_HBOUTLOGINFO );
            hb_xsetinfo( pSet->HB_SET_HBOUTLOGINFO );
         }
         break;
      case HB_SET_OSCODEPAGE:
         hb_retc( pSet->HB_SET_OSCODEPAGE );
         if( args > 1 && ( HB_IS_STRING( pArg2 ) || HB_IS_NIL( pArg2 ) ) )
         {
            PHB_CODEPAGE cdpOS = hb_cdpFindExt( hb_itemGetCPtr( pArg2 ) );
            char * szValue = cdpOS ? hb_strdup( cdpOS->id ) : NULL;
            if( pSet->HB_SET_OSCODEPAGE )
               hb_xfree( pSet->HB_SET_OSCODEPAGE );
            pSet->HB_SET_OSCODEPAGE = szValue;
            pSet->hb_set_oscp = ( void * ) cdpOS;
         }
         break;
      case HB_SET_DBCODEPAGE:
         if( pSet->HB_SET_DBCODEPAGE )
            hb_retc( pSet->HB_SET_DBCODEPAGE );
         else
            hb_ret();
         if( args > 1 && ( HB_IS_STRING( pArg2 ) || HB_IS_NIL( pArg2 ) ) )
         {
            PHB_CODEPAGE cdpOS = hb_cdpFindExt( hb_itemGetCPtr( pArg2 ) );
            char * szValue = cdpOS ? hb_strdup( cdpOS->id ) : NULL;
            if( pSet->HB_SET_DBCODEPAGE )
               hb_xfree( pSet->HB_SET_DBCODEPAGE );
            pSet->HB_SET_DBCODEPAGE = szValue;
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
   if( args > 1 )
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
}

void hb_setInitialize( PHB_SET_STRUCT pSet )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setInitialize(%p)", pSet));

   pSet->HB_SET_ALTERNATE = HB_FALSE;
   pSet->HB_SET_ALTFILE = NULL;
   pSet->hb_set_althan = FS_ERROR;
   pSet->HB_SET_AUTOPEN = HB_TRUE;
   pSet->HB_SET_AUTORDER = 0;
   pSet->HB_SET_AUTOSHARE = 0;
   pSet->HB_SET_BELL = HB_FALSE;
   pSet->HB_SET_CANCEL = HB_TRUE;
   pSet->hb_set_century = HB_FALSE;
   pSet->hb_set_prndevice = HB_FALSE;
   pSet->HB_SET_COLOR = ( char * ) hb_xgrab( HB_CLRSTR_LEN + 1 );
   hb_strncpy( pSet->HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", HB_CLRSTR_LEN );
   pSet->HB_SET_CONFIRM = HB_FALSE;
   pSet->HB_SET_CONSOLE = HB_TRUE;
   pSet->HB_SET_DATEFORMAT = hb_strdup( "mm/dd/yy" );
   pSet->HB_SET_TIMEFORMAT = hb_strdup( "hh:mm:ss.fff" );
   /*
    * Tests shows that Clipper has two different flags to control ALT+D
    * and ALTD() behavior and on startup these flags are not synchronized.
    * When application starts _SET_DEBUG is set to HB_FALSE but debugger
    * can be activated by hitting K_ALT_D or calling ALTD() function without
    * parameter. It means that some other internal flag enables these
    * operations.
    * Because Harbour is using _SET_DEBUG flag only then we have to
    * initialize it to HB_TRUE when debugger is linked to keep real Clipper
    * behavior or we will have to add second flag too and try to replicate
    * exactly unsynchronized behavior of these flags which exists in Clipper.
    * IMHO it's a bug in Clipper (side effect of some internal solutions) and
    * we should not try to emulate it [druzus].
    */
   /* pSet->HB_SET_DEBUG = HB_FALSE; */
   pSet->HB_SET_DEBUG = hb_dynsymFind( "__DBGENTRY" ) ? HB_TRUE : HB_FALSE;
   pSet->HB_SET_DECIMALS = 2;
   pSet->HB_SET_DEFAULT = hb_strdup( "" );
   pSet->HB_SET_DELETED = HB_FALSE;
   pSet->HB_SET_DELIMCHARS = hb_strdup( "::" );
   pSet->HB_SET_DELIMITERS = HB_FALSE;
   pSet->HB_SET_DEVICE = hb_strdup( "SCREEN" );
   pSet->HB_SET_EOF = HB_TRUE;
   pSet->HB_SET_EPOCH = 1900;
   pSet->HB_SET_ESCAPE = HB_TRUE;
   pSet->HB_SET_EVENTMASK = INKEY_KEYBOARD;
   pSet->HB_SET_EXACT = HB_FALSE;
   pSet->HB_SET_EXCLUSIVE = HB_TRUE;
   pSet->HB_SET_EXIT = HB_FALSE;
   pSet->HB_SET_EXTRA = HB_FALSE;
   pSet->HB_SET_EXTRAFILE = NULL;
   pSet->hb_set_extrahan = FS_ERROR;
   pSet->HB_SET_FIXED = HB_FALSE;
   pSet->HB_SET_FORCEOPT = HB_FALSE;
   pSet->HB_SET_HARDCOMMIT = HB_TRUE;
   pSet->HB_SET_IDLEREPEAT = HB_TRUE;
   pSet->HB_SET_INSERT = HB_FALSE;
   pSet->HB_SET_INTENSITY = HB_TRUE;
   pSet->HB_SET_MARGIN = 0;
   pSet->HB_SET_MBLOCKSIZE = 64;
   pSet->HB_SET_MCENTER = HB_FALSE;
   pSet->HB_SET_MESSAGE = 0;
   pSet->HB_SET_MFILEEXT = hb_strdup( "" );
   pSet->HB_SET_OPTIMIZE = HB_TRUE;
   pSet->HB_SET_PATH = hb_strdup( "" );
   pSet->hb_set_path = NULL;
   pSet->HB_SET_PRINTER = HB_FALSE;
   pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();
   pSet->hb_set_printhan = FS_ERROR;
   pSet->HB_SET_SCOREBOARD = HB_TRUE;
   pSet->HB_SET_SCROLLBREAK = HB_TRUE;
   pSet->HB_SET_SOFTSEEK = HB_FALSE;
   pSet->HB_SET_STRICTREAD = HB_FALSE;
   pSet->HB_SET_TYPEAHEAD = HB_DEFAULT_INKEY_BUFSIZE;
   pSet->HB_SET_UNIQUE = HB_FALSE;
   pSet->HB_SET_FILECASE = HB_SET_CASE_MIXED;
   pSet->HB_SET_DIRCASE = HB_SET_CASE_MIXED;
   pSet->HB_SET_DIRSEPARATOR = HB_OS_PATH_DELIM_CHR;
   pSet->HB_SET_VIDEOMODE = 0;
   pSet->HB_SET_WRAP = HB_FALSE;
   pSet->HB_SET_DBFLOCKSCHEME = 0;
   pSet->HB_SET_DEFEXTENSIONS = HB_TRUE;
   pSet->HB_SET_EOL = hb_strdup( hb_conNewLine() );
   pSet->HB_SET_TRIMFILENAME = HB_FALSE;
   pSet->HB_SET_HBOUTLOG = hb_strdup( "hb_out.log" );
   pSet->HB_SET_HBOUTLOGINFO = hb_strdup( "" );
   pSet->HB_SET_OSCODEPAGE = NULL;
   pSet->HB_SET_DBCODEPAGE = NULL;

   hb_xsetfilename( pSet->HB_SET_HBOUTLOG );
   hb_xsetinfo( pSet->HB_SET_HBOUTLOGINFO );

   pSet->hb_set_listener = NULL;
}

void hb_setRelease( PHB_SET_STRUCT pSet )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setRelease()"));

   close_handle( pSet, HB_SET_ALTFILE );
   close_handle( pSet, HB_SET_EXTRAFILE );
   close_handle( pSet, HB_SET_PRINTFILE );

   if( pSet->HB_SET_ALTFILE )       hb_xfree( pSet->HB_SET_ALTFILE );
   if( pSet->HB_SET_DATEFORMAT )    hb_xfree( pSet->HB_SET_DATEFORMAT );
   if( pSet->HB_SET_TIMEFORMAT )    hb_xfree( pSet->HB_SET_TIMEFORMAT );
   if( pSet->HB_SET_DEFAULT )       hb_xfree( pSet->HB_SET_DEFAULT );
   if( pSet->HB_SET_DELIMCHARS )    hb_xfree( pSet->HB_SET_DELIMCHARS );
   if( pSet->HB_SET_DEVICE )        hb_xfree( pSet->HB_SET_DEVICE );
   if( pSet->HB_SET_EXTRAFILE )     hb_xfree( pSet->HB_SET_EXTRAFILE );
   if( pSet->HB_SET_MFILEEXT  )     hb_xfree( pSet->HB_SET_MFILEEXT );
   if( pSet->HB_SET_PATH )          hb_xfree( pSet->HB_SET_PATH );
   if( pSet->HB_SET_PRINTFILE )     hb_xfree( pSet->HB_SET_PRINTFILE );
   if( pSet->HB_SET_COLOR )         hb_xfree( pSet->HB_SET_COLOR );
   if( pSet->HB_SET_EOL )           hb_xfree( pSet->HB_SET_EOL );
   if( pSet->HB_SET_HBOUTLOG )      hb_xfree( pSet->HB_SET_HBOUTLOG );
   if( pSet->HB_SET_HBOUTLOGINFO )  hb_xfree( pSet->HB_SET_HBOUTLOGINFO );
   if( pSet->HB_SET_OSCODEPAGE )    hb_xfree( pSet->HB_SET_OSCODEPAGE );
   if( pSet->HB_SET_DBCODEPAGE )    hb_xfree( pSet->HB_SET_DBCODEPAGE );

   hb_fsFreeSearchPath( pSet->hb_set_path );

   /* Free all set listeners */
   if( pSet->hb_set_listener )
   {
      PHB_SET_LISTENER pListener = ( ( PHB_SET_LISTENER_LST )
                                     pSet->hb_set_listener )->first;
      while( pListener )
      {
         PHB_SET_LISTENER pNext = pListener->next;
         hb_xfree( pListener );
         pListener = pNext;
      }
      hb_xfree( pSet->hb_set_listener );
   }
}

PHB_SET_STRUCT hb_setClone( PHB_SET_STRUCT pSrc )
{
   PHB_SET_STRUCT pSet = ( PHB_SET_STRUCT ) hb_xgrab( sizeof( HB_SET_STRUCT ) );

   memcpy( pSet, pSrc, sizeof( HB_SET_STRUCT ) );

   pSet->hb_set_althan = pSet->hb_set_extrahan = pSet->hb_set_printhan = FS_ERROR;
   pSet->hb_set_path = NULL;
   pSet->hb_set_listener = NULL;

   pSet->HB_SET_TYPEAHEAD = HB_DEFAULT_INKEY_BUFSIZE;

   pSet->HB_SET_COLOR = ( char * ) hb_xgrab( HB_CLRSTR_LEN + 1 );
   hb_strncpy( pSet->HB_SET_COLOR, pSrc->HB_SET_COLOR, HB_CLRSTR_LEN );

   if( pSet->HB_SET_ALTFILE )      pSet->HB_SET_ALTFILE      = hb_strdup( pSet->HB_SET_ALTFILE );
   if( pSet->HB_SET_DATEFORMAT )   pSet->HB_SET_DATEFORMAT   = hb_strdup( pSet->HB_SET_DATEFORMAT );
   if( pSet->HB_SET_TIMEFORMAT )   pSet->HB_SET_TIMEFORMAT   = hb_strdup( pSet->HB_SET_TIMEFORMAT );
   if( pSet->HB_SET_DEFAULT )      pSet->HB_SET_DEFAULT      = hb_strdup( pSet->HB_SET_DEFAULT );
   if( pSet->HB_SET_DELIMCHARS )   pSet->HB_SET_DELIMCHARS   = hb_strdup( pSet->HB_SET_DELIMCHARS );
   if( pSet->HB_SET_DEVICE )       pSet->HB_SET_DEVICE       = hb_strdup( pSet->HB_SET_DEVICE );
   if( pSet->HB_SET_EXTRAFILE )    pSet->HB_SET_EXTRAFILE    = hb_strdup( pSet->HB_SET_EXTRAFILE );
   if( pSet->HB_SET_MFILEEXT  )    pSet->HB_SET_MFILEEXT     = hb_strdup( pSet->HB_SET_MFILEEXT );
   if( pSet->HB_SET_PATH )         pSet->HB_SET_PATH         = hb_strdup( pSet->HB_SET_PATH );
   if( pSet->HB_SET_PRINTFILE )    pSet->HB_SET_PRINTFILE    = hb_strdup( pSet->HB_SET_PRINTFILE );
   if( pSet->HB_SET_EOL )          pSet->HB_SET_EOL          = hb_strdup( pSet->HB_SET_EOL );
   if( pSet->HB_SET_HBOUTLOG )     pSet->HB_SET_HBOUTLOG     = hb_strdup( pSet->HB_SET_HBOUTLOG );
   if( pSet->HB_SET_HBOUTLOGINFO ) pSet->HB_SET_HBOUTLOGINFO = hb_strdup( pSet->HB_SET_HBOUTLOGINFO );
   if( pSet->HB_SET_OSCODEPAGE )   pSet->HB_SET_OSCODEPAGE   = hb_strdup( pSet->HB_SET_OSCODEPAGE );
   if( pSet->HB_SET_DBCODEPAGE )   pSet->HB_SET_DBCODEPAGE   = hb_strdup( pSet->HB_SET_DBCODEPAGE );

   hb_fsAddSearchPath( pSet->HB_SET_PATH, &pSet->hb_set_path );

   return pSet;
}

int hb_setListenerAdd( HB_SET_LISTENER_CALLBACK * callback )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();
   PHB_SET_LISTENER p_sl = ( PHB_SET_LISTENER ) hb_xgrab( sizeof( HB_SET_LISTENER ) );
   PHB_SET_LISTENER_LST pList;

   if( ! pSet->hb_set_listener )
   {
      pSet->hb_set_listener = hb_xgrab( sizeof( HB_SET_LISTENER_LST ) );
      memset( pSet->hb_set_listener, 0, sizeof( HB_SET_LISTENER_LST ) );
   }
   pList = ( PHB_SET_LISTENER_LST ) pSet->hb_set_listener;

   p_sl->callback = callback;
   p_sl->listener = ++pList->counter;
   p_sl->next = NULL;

   if( pList->last )
      pList->last->next = p_sl;
   else if( ! pList->first )
      pList->first = p_sl;
   pList->last = p_sl;

   return p_sl->listener;
}

void hb_setListenerNotify( HB_set_enum set, HB_set_listener_enum when )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_LISTENER_LST pList = ( PHB_SET_LISTENER_LST ) hb_stackSetStruct()->hb_set_listener;
   if( pList )
   {
      PHB_SET_LISTENER p_sl = pList->first;
      while( p_sl )
      {
         ( * p_sl->callback )( set, when );
         p_sl = p_sl->next;
      }
   }
}

int hb_setListenerRemove( int listener )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_LISTENER_LST pList = ( PHB_SET_LISTENER_LST ) hb_stackSetStruct()->hb_set_listener;
   if( pList )
   {
      PHB_SET_LISTENER p_sl = pList->first;
      PHB_SET_LISTENER p_sl_prev = NULL;
      while( p_sl )
      {
         if( listener == p_sl->listener )
         {
            listener = -listener;
            if( p_sl_prev )
               p_sl_prev->next = p_sl->next;
            else
               pList->first = p_sl->next;
            if( p_sl == pList->last )
               pList->last = p_sl_prev;
            hb_xfree( p_sl );
            break;
         }
         p_sl_prev = p_sl;
         p_sl = p_sl->next;
      }
   }
   return listener;
}

HB_BOOL hb_setSetItem( HB_set_enum set_specifier, PHB_ITEM pItem )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();
   HB_BOOL fResult = HB_FALSE;
   char * szValue;
   int iValue;

   if( pItem )
   {
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );

      switch( set_specifier )
      {
         case HB_SET_ALTFILE:
         case HB_SET_EXTRAFILE:
         case HB_SET_PRINTFILE:
            /* This sets needs 3-rd parameter to indicate additive mode
             * so they cannot be fully supported by this function
             */
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               open_handle( pSet, hb_itemGetCPtr( pItem ), HB_FALSE, set_specifier );
               fResult = HB_TRUE;
               if( set_specifier == HB_SET_PRINTFILE && pSet->HB_SET_PRINTFILE == NULL )
                  pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();
            }
            break;

         case HB_SET_ALTERNATE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_ALTERNATE = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_AUTOPEN:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_AUTOPEN = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_BELL:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_BELL = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_CANCEL:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_CANCEL = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_CONFIRM:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_CONFIRM = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_CONSOLE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_CONSOLE = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DEBUG:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DEBUG = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DELETED:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DELETED = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DELIMITERS:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DELIMITERS = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_EOF:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EOF = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_ESCAPE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_ESCAPE = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_EXACT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXACT = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_EXCLUSIVE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXCLUSIVE = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_EXIT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXIT = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_EXTRA:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXTRA = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_FIXED:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_FIXED = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_IDLEREPEAT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_IDLEREPEAT = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_INSERT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_INSERT = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_INTENSITY:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_INTENSITY = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_MCENTER:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_MCENTER = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_OPTIMIZE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_OPTIMIZE = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_FORCEOPT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_FORCEOPT = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_PRINTER:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_PRINTER = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_SCOREBOARD:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_SCOREBOARD = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_SCROLLBREAK:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_SCROLLBREAK = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_SOFTSEEK:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_SOFTSEEK = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_STRICTREAD:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_STRICTREAD = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_UNIQUE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_UNIQUE = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_WRAP:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_WRAP = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_HARDCOMMIT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_HARDCOMMIT = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DEFEXTENSIONS:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DEFEXTENSIONS = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_TRIMFILENAME:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_TRIMFILENAME = hb_itemGetL( pItem );
               fResult = HB_TRUE;
            }
            break;

         case HB_SET_DECIMALS:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_DECIMALS = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_EPOCH:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_EPOCH = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_MBLOCKSIZE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_MBLOCKSIZE = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_DBFLOCKSCHEME:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_DBFLOCKSCHEME = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_AUTORDER:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_AUTORDER = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_AUTOSHARE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_AUTOSHARE = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_CURSOR:
            if( HB_IS_NUMERIC( pItem ) )
            {
               hb_conSetCursor( HB_TRUE, hb_itemGetNI( pItem ) );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_EVENTMASK:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_EVENTMASK = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_MARGIN:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_MARGIN = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_MESSAGE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_MESSAGE = iValue;
                  fResult = HB_TRUE;
               }
            }
            break;
         case HB_SET_TYPEAHEAD:
            if( HB_IS_NUMERIC( pItem ) )
            {
               /* Set the value and limit the range */
               pSet->HB_SET_TYPEAHEAD = hb_itemGetNI( pItem );
               if( pSet->HB_SET_TYPEAHEAD == 0 )
                  /* Do nothing */ ;
               else if( pSet->HB_SET_TYPEAHEAD < 16 )
                  pSet->HB_SET_TYPEAHEAD = 16;
               else if( pSet->HB_SET_TYPEAHEAD > 4096 )
                  pSet->HB_SET_TYPEAHEAD = 4096;
               /* reset keyboard buffer */
               hb_inkeyReset();
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_VIDEOMODE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               pSet->HB_SET_VIDEOMODE = hb_itemGetNI( pItem );
               fResult = HB_TRUE;
            }
            break;

         case HB_SET_COLOR:
            if( HB_IS_STRING( pItem ) )
            {
               hb_conSetColor( hb_itemGetCPtr( pItem ) );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_LANGUAGE:
            if( HB_IS_STRING( pItem ) )
            {
               hb_langSelectID( hb_itemGetCPtr( pItem ) );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_CODEPAGE:
            if( HB_IS_STRING( pItem ) )
            {
               hb_cdpSelectID( hb_itemGetCPtr( pItem ) );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_FILECASE:
         case HB_SET_DIRCASE:
            iValue = -1;
            if( HB_IS_STRING( pItem ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pItem ), "LOWER" ) )
                  iValue = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pItem ), "UPPER" ) )
                  iValue = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pItem ), "MIXED" ) )
                  iValue = HB_SET_CASE_MIXED;
            }
            else if( HB_IS_NUMERIC( pItem ) )
               iValue = hb_itemGetNI( pItem );

            if( iValue == HB_SET_CASE_LOWER ||
                iValue == HB_SET_CASE_UPPER ||
                iValue == HB_SET_CASE_MIXED )
            {
               if( set_specifier == HB_SET_FILECASE )
                  pSet->HB_SET_FILECASE = iValue;
               else
                  pSet->HB_SET_DIRCASE = iValue;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DATEFORMAT:
            if( HB_IS_STRING( pItem ) )
            {
               int iYear = 0;

               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DATEFORMAT )
                  hb_xfree( pSet->HB_SET_DATEFORMAT );
               pSet->HB_SET_DATEFORMAT = szValue;
               while( *szValue )
               {
                  if( *szValue == 'Y' || *szValue == 'y' )
                     ++iYear;
                  else if( iYear )   /* Only count the first set of consecutive "Y"s. */
                     break;
                  ++szValue;
               }
               /* CENTURY is not controlled directly by SET, so there is no
                  notification for changing it indirectly via DATE FORMAT. */
               pSet->hb_set_century = iYear >= 4;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_TIMEFORMAT:
            if( HB_IS_STRING( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_TIMEFORMAT )
                  hb_xfree( pSet->HB_SET_TIMEFORMAT );
               pSet->HB_SET_TIMEFORMAT = szValue;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DIRSEPARATOR:
            if( hb_itemGetCLen( pItem ) > 0 )
            {
               pSet->HB_SET_DIRSEPARATOR = hb_itemGetCPtr( pItem )[ 0 ];
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DEVICE:
            if( HB_IS_STRING( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DEVICE )
                  hb_xfree( pSet->HB_SET_DEVICE );
               pSet->HB_SET_DEVICE = szValue;
               pSet->hb_set_prndevice = hb_stricmp( szValue, "PRINTER" ) == 0;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_MFILEEXT:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_MFILEEXT )
                  hb_xfree( pSet->HB_SET_MFILEEXT );
               pSet->HB_SET_MFILEEXT = szValue;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DEFAULT:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DEFAULT )
                  hb_xfree( pSet->HB_SET_DEFAULT );
               pSet->HB_SET_DEFAULT = szValue;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_PATH:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_PATH )
                  hb_xfree( pSet->HB_SET_PATH );
               pSet->HB_SET_PATH = szValue;

               hb_fsFreeSearchPath( pSet->hb_set_path );
               pSet->hb_set_path = NULL;
               hb_fsAddSearchPath( pSet->HB_SET_PATH, &pSet->hb_set_path );

               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DELIMCHARS:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DELIMCHARS )
                  hb_xfree( pSet->HB_SET_DELIMCHARS );
               pSet->HB_SET_DELIMCHARS = szValue;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_EOL:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_EOL )
                  hb_xfree( pSet->HB_SET_EOL );
               pSet->HB_SET_EOL = szValue;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_HBOUTLOG:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               if( HB_IS_NIL( pItem ) )
                  szValue = NULL;
               else
                  szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_HBOUTLOG )
                  hb_xfree( pSet->HB_SET_HBOUTLOG );
               pSet->HB_SET_HBOUTLOG = szValue;
               hb_xsetfilename( pSet->HB_SET_HBOUTLOG );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_HBOUTLOGINFO:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_HBOUTLOGINFO )
                  hb_xfree( pSet->HB_SET_HBOUTLOGINFO );
               pSet->HB_SET_HBOUTLOGINFO = szValue;
               hb_xsetinfo( pSet->HB_SET_HBOUTLOGINFO );
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_OSCODEPAGE:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               PHB_CODEPAGE cdpOS = hb_cdpFindExt( hb_itemGetCPtr( pItem ) );
               szValue = cdpOS ? hb_strdup( cdpOS->id ) : NULL;
               if( pSet->HB_SET_OSCODEPAGE )
                  hb_xfree( pSet->HB_SET_OSCODEPAGE );
               pSet->HB_SET_OSCODEPAGE = szValue;
               pSet->hb_set_oscp = ( void * ) cdpOS;
               fResult = HB_TRUE;
            }
            break;
         case HB_SET_DBCODEPAGE:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               PHB_CODEPAGE cdpOS = hb_cdpFindExt( hb_itemGetCPtr( pItem ) );
               szValue = cdpOS ? hb_strdup( cdpOS->id ) : NULL;
               if( pSet->HB_SET_DBCODEPAGE )
                  hb_xfree( pSet->HB_SET_DBCODEPAGE );
               pSet->HB_SET_DBCODEPAGE = szValue;
               fResult = HB_TRUE;
            }
            break;

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
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
   }

   return fResult;
}

HB_BOOL hb_setSetItem2( HB_set_enum set_specifier, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_BOOL fResult = HB_FALSE;

   if( pItem1 )
   {
      switch( set_specifier )
      {
         case HB_SET_ALTFILE:
         case HB_SET_EXTRAFILE:
         case HB_SET_PRINTFILE:
            if( HB_IS_STRING( pItem1 ) || HB_IS_NIL( pItem1 ) )
            {
               HB_STACK_TLS_PRELOAD
               PHB_SET_STRUCT pSet = hb_stackSetStruct();

               hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );

               open_handle( pSet, hb_itemGetCPtr( pItem1 ),
                            set_logical( pItem2, HB_FALSE ), set_specifier );
               fResult = HB_TRUE;
               if( set_specifier == HB_SET_PRINTFILE && pSet->HB_SET_PRINTFILE == NULL )
                  pSet->HB_SET_PRINTFILE = hb_set_PRINTFILE_default();

               hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
            }
            break;
         default:
            fResult = hb_setSetItem( set_specifier, pItem1 );
      }
   }
   return fResult;
}

HB_BOOL hb_setGetL( HB_set_enum set_specifier )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   switch( set_specifier )
   {
      case HB_SET_ALTERNATE:
         return pSet->HB_SET_ALTERNATE;
      case HB_SET_AUTOPEN:
         return pSet->HB_SET_AUTOPEN;
      case HB_SET_BELL:
         return pSet->HB_SET_BELL;
      case HB_SET_CANCEL:
         return pSet->HB_SET_CANCEL;
      case HB_SET_CONFIRM:
         return pSet->HB_SET_CONFIRM;
      case HB_SET_CONSOLE:
         return pSet->HB_SET_CONSOLE;
      case HB_SET_DEBUG:
         return pSet->HB_SET_DEBUG;
      case HB_SET_DELETED:
         return pSet->HB_SET_DELETED;
      case HB_SET_DELIMITERS:
         return pSet->HB_SET_DELIMITERS;
      case HB_SET_EOF:
         return pSet->HB_SET_EOF;
      case HB_SET_ESCAPE:
         return pSet->HB_SET_ESCAPE;
      case HB_SET_EXACT:
         return pSet->HB_SET_EXACT;
      case HB_SET_EXCLUSIVE:
         return pSet->HB_SET_EXCLUSIVE;
      case HB_SET_EXIT:
         return pSet->HB_SET_EXIT;
      case HB_SET_EXTRA:
         return pSet->HB_SET_EXTRA;
      case HB_SET_FIXED:
         return pSet->HB_SET_FIXED;
      case HB_SET_IDLEREPEAT:
         return pSet->HB_SET_IDLEREPEAT;
      case HB_SET_INSERT:
         return pSet->HB_SET_INSERT;
      case HB_SET_INTENSITY:
         return pSet->HB_SET_INTENSITY;
      case HB_SET_MCENTER:
         return pSet->HB_SET_MCENTER;
      case HB_SET_OPTIMIZE:
         return pSet->HB_SET_OPTIMIZE;
      case HB_SET_FORCEOPT:
         return pSet->HB_SET_FORCEOPT;
      case HB_SET_PRINTER:
         return pSet->HB_SET_PRINTER;
      case HB_SET_SCOREBOARD:
         return pSet->HB_SET_SCOREBOARD;
      case HB_SET_SCROLLBREAK:
         return pSet->HB_SET_SCROLLBREAK;
      case HB_SET_SOFTSEEK:
         return pSet->HB_SET_SOFTSEEK;
      case HB_SET_STRICTREAD:
         return pSet->HB_SET_STRICTREAD;
      case HB_SET_UNIQUE:
         return pSet->HB_SET_UNIQUE;
      case HB_SET_WRAP:
         return pSet->HB_SET_WRAP;
      case HB_SET_HARDCOMMIT:
         return pSet->HB_SET_HARDCOMMIT;
      case HB_SET_DEFEXTENSIONS:
         return pSet->HB_SET_DEFEXTENSIONS;
      case HB_SET_TRIMFILENAME:
         return pSet->HB_SET_TRIMFILENAME;

      case HB_SET_ALTFILE:
      case HB_SET_AUTORDER:
      case HB_SET_AUTOSHARE:
      case HB_SET_COLOR:
      case HB_SET_CURSOR:
      case HB_SET_DATEFORMAT:
      case HB_SET_TIMEFORMAT:
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
      case HB_SET_CODEPAGE:
      case HB_SET_FILECASE:
      case HB_SET_DIRCASE:
      case HB_SET_DIRSEPARATOR:
      case HB_SET_DBFLOCKSCHEME:
      case HB_SET_EOL:
      case HB_SET_HBOUTLOG:
      case HB_SET_HBOUTLOGINFO:
      case HB_SET_OSCODEPAGE:
      case HB_SET_DBCODEPAGE:
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
   return HB_FALSE;
}

const char * hb_setGetCPtr( HB_set_enum set_specifier )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   switch( set_specifier )
   {
      case HB_SET_ALTFILE:
         return pSet->HB_SET_ALTFILE;
      case HB_SET_COLOR:
         return pSet->HB_SET_COLOR;
      case HB_SET_DATEFORMAT:
         return pSet->HB_SET_DATEFORMAT;
      case HB_SET_TIMEFORMAT:
         return pSet->HB_SET_TIMEFORMAT;
      case HB_SET_DEFAULT:
         return pSet->HB_SET_DEFAULT;
      case HB_SET_DELIMCHARS:
         return pSet->HB_SET_DELIMCHARS;
      case HB_SET_DEVICE:
         return pSet->HB_SET_DEVICE;
      case HB_SET_EXTRAFILE:
         return pSet->HB_SET_EXTRAFILE;
      case HB_SET_PATH:
         return pSet->HB_SET_PATH;
      case HB_SET_MFILEEXT:
         return pSet->HB_SET_MFILEEXT;
      case HB_SET_PRINTFILE:
         return pSet->HB_SET_PRINTFILE;
      case HB_SET_EOL:
         return pSet->HB_SET_EOL;
      case HB_SET_HBOUTLOG:
         return pSet->HB_SET_HBOUTLOG;
      case HB_SET_HBOUTLOGINFO:
         return pSet->HB_SET_HBOUTLOGINFO;
      case HB_SET_OSCODEPAGE:
         return pSet->HB_SET_OSCODEPAGE;
      case HB_SET_DBCODEPAGE:
         return pSet->HB_SET_DBCODEPAGE;
      case HB_SET_LANGUAGE:
         return hb_langID();
      case HB_SET_CODEPAGE:
         return hb_cdpID();
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
   return NULL;
}

int     hb_setGetNI( HB_set_enum set_specifier )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   switch( set_specifier )
   {
      case HB_SET_AUTORDER:
         return pSet->HB_SET_AUTORDER;
      case HB_SET_AUTOSHARE:
         return pSet->HB_SET_AUTOSHARE;
      case HB_SET_DECIMALS:
         return pSet->HB_SET_DECIMALS;
      case HB_SET_EPOCH:
         return pSet->HB_SET_EPOCH;
      case HB_SET_EVENTMASK:
         return pSet->HB_SET_EVENTMASK;
      case HB_SET_MARGIN:
         return pSet->HB_SET_MARGIN;
      case HB_SET_MBLOCKSIZE:
         return pSet->HB_SET_MBLOCKSIZE;
      case HB_SET_MESSAGE:
         return pSet->HB_SET_MESSAGE;
      case HB_SET_TYPEAHEAD:
         return pSet->HB_SET_TYPEAHEAD;
      case HB_SET_FILECASE:
         return pSet->HB_SET_FILECASE;
      case HB_SET_DIRCASE:
         return pSet->HB_SET_DIRCASE;
      case HB_SET_DIRSEPARATOR:
         return pSet->HB_SET_DIRSEPARATOR;
      case HB_SET_VIDEOMODE:
         return pSet->HB_SET_VIDEOMODE;
      case HB_SET_DBFLOCKSCHEME:
         return pSet->HB_SET_DBFLOCKSCHEME;

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
      case HB_SET_TIMEFORMAT:
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
      case HB_SET_CODEPAGE:
      case HB_SET_IDLEREPEAT:
      case HB_SET_EOL:
      case HB_SET_DEFEXTENSIONS:
      case HB_SET_TRIMFILENAME:
      case HB_SET_HBOUTLOG:
      case HB_SET_HBOUTLOGINFO:
      case HB_SET_OSCODEPAGE:
      case HB_SET_DBCODEPAGE:
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
   return 0;
}

long    hb_setGetNL( HB_set_enum set_specifier )
{
   return hb_setGetNI( set_specifier );
}

HB_PATHNAMES * hb_setGetFirstSetPath( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->hb_set_path;
}

HB_FHANDLE hb_setGetAltHan( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->hb_set_althan;
}

HB_BOOL hb_setGetCentury( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->hb_set_century;
}

HB_FHANDLE hb_setGetExtraHan( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->hb_set_extrahan;
}

HB_FHANDLE hb_setGetPrintHan( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->hb_set_printhan;
}

HB_BOOL hb_setGetAlternate( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_ALTERNATE;
}

const char *  hb_setGetAltFile( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_ALTFILE;
}

HB_BOOL hb_setGetAutOpen( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_AUTOPEN;
}

int     hb_setGetAutOrder( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_AUTORDER;
}

int     hb_setGetAutoShare( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_AUTOSHARE;
}

HB_BOOL hb_setGetBell( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_BELL;
}

HB_BOOL hb_setGetCancel( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_CANCEL;
}

char *  hb_setGetColor( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_COLOR;
}

HB_BOOL hb_setGetConfirm( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_CONFIRM;
}

HB_BOOL hb_setGetConsole( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_CONSOLE;
}

const char * hb_setGetDateFormat( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DATEFORMAT;
}

const char * hb_setGetTimeFormat( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_TIMEFORMAT;
}

HB_BOOL hb_setGetDebug( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DEBUG;
}

int     hb_setGetDecimals( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DECIMALS;
}

const char *  hb_setGetDefault( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DEFAULT;
}

HB_BOOL hb_setGetDeleted( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DELETED;
}

const char *  hb_setGetDelimChars( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DELIMCHARS;
}

HB_BOOL hb_setGetDelimiters( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DELIMITERS;
}

const char *  hb_setGetDevice( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DEVICE;
}

HB_BOOL hb_setGetEOF( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EOF;
}

int     hb_setGetEpoch( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EPOCH;
}

HB_BOOL hb_setGetEscape( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_ESCAPE;
}

int     hb_setGetEventMask( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EVENTMASK;
}

HB_BOOL hb_setGetExact( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EXACT;
}

HB_BOOL hb_setGetExclusive( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EXCLUSIVE;
}

HB_BOOL hb_setGetExit( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EXIT;
}

HB_BOOL hb_setGetExtra( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EXTRA;
}

const char *  hb_setGetExtraFile( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EXTRAFILE;
}

HB_BOOL hb_setGetFixed( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_FIXED;
}

HB_BOOL hb_setGetIdleRepeat( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_IDLEREPEAT;
}

HB_BOOL hb_setGetInsert( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_INSERT;
}

HB_BOOL hb_setGetIntensity( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_INTENSITY;
}

const char *  hb_setGetPath( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_PATH;
}

int     hb_setGetMargin( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_MARGIN;
}

int     hb_setGetMBlockSize( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_MBLOCKSIZE;
}

HB_BOOL hb_setGetMCenter( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_MCENTER;
}

int     hb_setGetMessage( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_MESSAGE;
}

const char *  hb_setGetMFileExt( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_MFILEEXT;
}

HB_BOOL hb_setGetOptimize( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_OPTIMIZE;
}

HB_BOOL hb_setGetPrinter( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_PRINTER;
}

const char *  hb_setGetPrintFile( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_PRINTFILE;
}

HB_BOOL hb_setGetScoreBoard( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_SCOREBOARD;
}

HB_BOOL hb_setGetScrollBreak( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_SCROLLBREAK;
}

HB_BOOL hb_setGetSoftSeek( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_SOFTSEEK;
}

HB_BOOL hb_setGetStrictRead( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_STRICTREAD;
}

int     hb_setGetTypeAhead( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_TYPEAHEAD;
}

HB_BOOL hb_setGetUnique( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_UNIQUE;
}

int     hb_setGetFileCase( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_FILECASE;
}

int     hb_setGetDirCase( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DIRCASE;
}

int     hb_setGetDirSeparator( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DIRSEPARATOR;
}

int     hb_setGetVideoMode( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_VIDEOMODE;
}

HB_BOOL hb_setGetWrap( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_WRAP;
}

int     hb_setGetDBFLockScheme( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DBFLOCKSCHEME;
}

HB_BOOL hb_setGetHardCommit( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_HARDCOMMIT;
}

HB_BOOL hb_setGetForceOpt( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_FORCEOPT;
}

HB_BOOL hb_setGetDefExtension( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DEFEXTENSIONS;
}

const char * hb_setGetEOL( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_EOL;
}

HB_BOOL hb_setGetTrimFileName( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_TRIMFILENAME;
}

const char * hb_setGetHBOUTLOG( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_HBOUTLOG;
}

const char * hb_setGetHBOUTLOGINFO( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_HBOUTLOGINFO;
}

const char * hb_setGetOSCODEPAGE( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_OSCODEPAGE;
}

void * hb_setGetOSCP( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->hb_set_oscp;
}

const char * hb_setGetDBCODEPAGE( void )
{
   HB_STACK_TLS_PRELOAD
   return hb_stackSetStruct()->HB_SET_DBCODEPAGE;
}

HB_BOOL hb_osUseCP( void )
{
   HB_STACK_TLS_PRELOAD

#if defined( HB_MT_VM )
   if( hb_stackId() )
#endif
   {
      PHB_CODEPAGE cdpOS = ( PHB_CODEPAGE ) hb_stackSetStruct()->hb_set_oscp;
      if( cdpOS )
      {
         PHB_CODEPAGE cdpHost = hb_vmCDP();
         return cdpHost && cdpHost != cdpOS;
      }
   }

   return HB_FALSE;
}

const char * hb_osEncodeCP( const char * szName, char ** pszFree, HB_SIZE * pnSize )
{
   HB_STACK_TLS_PRELOAD

#if defined( HB_MT_VM )
   if( hb_stackId() )
#endif
   {
      PHB_CODEPAGE cdpOS = ( PHB_CODEPAGE ) hb_stackSetStruct()->hb_set_oscp;
      if( cdpOS )
      {
         PHB_CODEPAGE cdpHost = hb_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
         {
            HB_SIZE nSize = 0;
            char * pszBuf;

            if( pszFree == NULL )
            {
               pszFree = ( char ** ) &szName;
               nSize = strlen( szName );
            }
            pszBuf = *pszFree;
            if( pnSize == NULL )
               pnSize = &nSize;
            else if( *pnSize > 0 )
               nSize = *pnSize - 1;

            szName = hb_cdpnDup3( szName, strlen( szName ),
                                  pszBuf, &nSize, pszFree, pnSize,
                                  cdpHost, cdpOS );
         }
      }
   }

   return szName;
}

const char * hb_osDecodeCP( const char * szName, char ** pszFree, HB_SIZE * pnSize )
{
   HB_STACK_TLS_PRELOAD

#if defined( HB_MT_VM )
   if( hb_stackId() )
#endif
   {
      PHB_CODEPAGE cdpOS = ( PHB_CODEPAGE ) hb_stackSetStruct()->hb_set_oscp;
      if( cdpOS )
      {
         PHB_CODEPAGE cdpHost = hb_vmCDP();
         if( cdpHost && cdpHost != cdpOS )
         {
            HB_SIZE nSize = 0;
            char * pszBuf;

            if( pszFree == NULL )
            {
               pszFree = ( char ** ) &szName;
               nSize = strlen( szName );
            }
            pszBuf = *pszFree;
            if( pnSize == NULL )
               pnSize = &nSize;
            else if( *pnSize > 0 )
               nSize = *pnSize - 1;

            szName = hb_cdpnDup3( szName, strlen( szName ),
                                  pszBuf, &nSize, pszFree, pnSize,
                                  cdpOS, cdpHost );
         }
      }
   }

   return szName;
}

HB_FHANDLE hb_setGetPrinterHandle( int iType )
{
   HB_STACK_TLS_PRELOAD
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   switch( iType )
   {
      case HB_SET_PRN_DEV:
         if( ! pSet->hb_set_prndevice )
            return FS_ERROR;
         break;
      case HB_SET_PRN_CON:
         if( ! pSet->HB_SET_PRINTER )
            return FS_ERROR;
         break;
      case HB_SET_PRN_ANY:
         break;
      default:
         return FS_ERROR;
   }

   if( pSet->hb_set_printhan == FS_ERROR && pSet->HB_SET_PRINTFILE )
      open_handle( pSet, pSet->HB_SET_PRINTFILE, HB_FALSE, HB_SET_PRINTFILE );

   return pSet->hb_set_printhan;
}
