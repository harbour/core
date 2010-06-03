/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * VistaSoftware's Apollo database driver. See http://www.VistaSoftware.com
 *
 * Copyright 2001 Patrick Mast <email@PatrickMast.com>
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

/* -----------------29/12/2001 19:23-----------------
 * NOTE: Functions are listed alfabetically
 * --------------------------------------------------*/

/* NOTE: This hack is needed to suppress 'non-ANSI
         keyword' warnings inside Sde61.h. */
#if defined( __BORLANDC__ ) || defined( __WATCOMC__ )
   #define _declspec __declspec
#endif

#include "hbapi.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

#if defined( HB_WITH_APOLLO_VER61 )
   #include "Sde61.h"
#else
   #include "sde7.h"
#endif

/* Uncomment for previous version */
/*  */

HB_FUNC( SX_APPENDBLANK )
{
   sx_AppendBlank();
}

HB_FUNC( SX_CLOSE )
{
   sx_Close();
}

HB_FUNC( SX_COMMIT )
{
   sx_Commit();
}

HB_FUNC( SX_CREATEEXEC )
{
   sx_CreateExec();
}

/* -----------------29/12/2001 19:18-----------------
 * sx_CreateField()
 * --------------------------------------------------*/
HB_FUNC( SX_CREATEFIELD )
{
   sx_CreateField( ( PBYTE ) hb_parc( 1 ),    /* Field name */
                   ( PBYTE ) hb_parc( 2 ),    /* Field type */
                   ( SHORT ) hb_parni( 3 ),   /* Field lenght */
                   ( SHORT ) hb_parni( 4 ) ); /* Field decimals */
}

/* -----------------29/12/2001 19:59-----------------
 * sx_CreateNew()
 *  => The work area number assigned to the database.
 * --------------------------------------------------*/
HB_FUNC( SX_CREATENEW )
{
   hb_retni(
      sx_CreateNew( ( PBYTE ) hb_parc( 1 ),      /* Field name */
                    ( PBYTE ) hb_parc( 2 ),      /* Alias */
                    ( SHORT ) hb_parni( 3 ),     /* RDE Type */
                    ( SHORT ) hb_parni( 4 ) ) ); /* The maximum number of fields to be added to the file structure */
}

HB_FUNC( SX_EOF )
{
   hb_retl( sx_Eof() );
}

/* -----------------29/12/2001 20:13-----------------
 * sx_GetDateJulian() => The date expressed as a long integer. Useful for date arithmetic.
 * Extracts the contents of a date field as a Julian number.
 * This number is equal to the number of days since January 1, 4713 BC.
 * However, only JULIAN dates equal or greater than January 1, 1000 are supported.
 * --------------------------------------------------*/
HB_FUNC( SX_GETDATEJULIAN )
{
   hb_retni( sx_GetDateJulian( ( PBYTE ) hb_parc( 1 ) ) );    /* Field name  */
}

/* -----------------30/12/2001 13:04-----------------
 * sx_GetLogical() => True if the field evaluates as True, and False if not.
 * Determines whether a logical field contains a True or False value.
 * --------------------------------------------------*/
HB_FUNC( SX_GETLOGICAL )
{
   hb_retl( sx_GetLogical( ( PBYTE ) hb_parc( 1 ) ) );       /* Field name  */
}

HB_FUNC( SX_GETSTRING )
{
   hb_retc( ( char * ) sx_GetString( ( PBYTE ) hb_parc( 1 ) ) );  /* Field name  */
}

/* -----------------30/12/2001 12:21-----------------
 * sx_GetVariant() => Character fields are returned as untrimmed strings.
 * --------------------------------------------------*/
HB_FUNC( SX_GETVARIANT )
{
   hb_retc( ( char * ) sx_GetVariant( ( PBYTE ) hb_parc( 1 ) ) );  /* Field name  */
}

HB_FUNC( SX_GO )
{
   sx_Go( hb_parni( 1 ) );
}

HB_FUNC( SX_GOBOTTOM )
{
   sx_GoBottom();
}

HB_FUNC( SX_GOTOP )
{
   sx_GoTop();
}

HB_FUNC( SX_INDEXTAG )
{
   hb_retni(
      sx_IndexTag( ( PBYTE ) hb_parc( 1 ),    /* Field name */
                   ( PBYTE ) hb_parc( 2 ),    /* Tag name */
                   ( PBYTE ) hb_parc( 3 ),    /* Index expression as a string */
                   ( SHORT ) hb_parni( 4 ),   /* Option (0=Standard) (1=Unique) (2=Roll-Your-Own) */
                   hb_parl( 5 ),              /* True for a descend index */
                   ( PBYTE ) hb_parc( 6) ) ); /* Condition */
}

HB_FUNC( SX_RECCOUNT )
{
   hb_retni( sx_RecCount() );
}

HB_FUNC( SX_RECNO )
{
   hb_retni( sx_RecNo() );
}

HB_FUNC( SX_REINDEX )
{
   sx_Reindex();
}

HB_FUNC( SX_REPLACE )
{
   switch( hb_parni( 2 ) )
   {
      case R_INTEGER :
      case R_JULIAN  : sx_Replace( ( PBYTE ) hb_parc( 1 ), ( SHORT ) hb_parni( 2 ), ( void * ) hb_parni( 3 ) ); break;
      case R_LOGICAL : sx_Replace( ( PBYTE ) hb_parc( 1 ), ( SHORT ) hb_parni( 2 ), ( void * ) hb_parni( 3 ) ); break; /* TODO: somthing is wrong here... */
      case R_LONG    : sx_Replace( ( PBYTE ) hb_parc( 1 ), ( SHORT ) hb_parni( 2 ), ( void * ) hb_parnl( 3 ) ); break;
      case R_DOUBLE  :
           {
              double d = hb_parnd( 3 );
              sx_Replace( ( PBYTE ) hb_parc( 1 ), ( SHORT ) hb_parni( 2 ), ( void * ) &d );
              break;
           }
      case R_CHAR    :
      case R_DATESTR :
      case R_MEMO    :
      case R_BITMAP  :
      case R_BLOBFILE:
      case R_BLOBPTR : sx_Replace( ( PBYTE ) hb_parc( 1 ), ( SHORT ) hb_parni( 2 ), ( void * ) hb_parc( 3 ) ); break;
      default:         sx_Replace( ( PBYTE ) hb_parc( 1 ), ( SHORT ) hb_parni( 2 ), ( void * ) hb_parc( 3 ) );
   }
}

HB_FUNC( SX_RLOCK )
{
   sx_Rlock( hb_parni( 1 ) );             /* The physical record number of the record to be locked. */
}

HB_FUNC( SX_SEEK )
{
   hb_retl( sx_Seek( ( PBYTE ) hb_parc( 1 ) ) );            /* The value to search for as a string */
}

/* -----------------20/01/2002 13:36-----------------
 * sx_Select() => The previous select area is returned.
 *                If there was no previous area active, zero is returned.
 * --------------------------------------------------*/
HB_FUNC( SX_SELECT )
{
   hb_retni( sx_Seek( ( PBYTE ) hb_parc( 1 ) ) );            /* The work area number returned when the file was opened.  */
}

/* -----------------30/12/2001 12:30-----------------
 * sx_SetCentury() => NILL
 * Indicates whether or not the two digits of the year designating
 * century are to be returned by sx_GetDateString as part of a date
 * string formatted according to the sx_SetDateFormat setting.
 * --------------------------------------------------*/
HB_FUNC( SX_SETCENTURY )
{
   sx_SetCentury( hb_parl( 1 ) ? 1 : 0 ); /* If True, the century digits will be returned.
                                           * If False, they will not. */
}

/* -----------------30/12/2001 12:32-----------------
 * sx_SetDateFormat() => NILL
 * Defines the format of date strings returned by sx_GetDateString.
 * --------------------------------------------------*/
HB_FUNC( SX_SETDATEFORMAT )
{
   sx_SetDateFormat( hb_parl( 1 ) ? 1 : 0 ); /* If True, the century digits will be returned.
                                              * If False, they will not. */
}

HB_FUNC( SX_SETMEMOBLOCKSIZE )
{
   sx_SetMemoBlockSize( ( WORD ) hb_parni( 1 ) ); /* The new default block size.
                                                   * The size must be a value from 1 through 1024. */
}

/* -----------------20/01/2002 13:33-----------------
 * sx_SetOrder() => The previous index order identifier in use is returned.
 *                  If zero, there was no previous index order in use.
 * --------------------------------------------------*/
HB_FUNC( SX_SETORDER )
{
   hb_retni( sx_SetOrder( ( SHORT ) hb_parni( 1 ) ) ); /* Selects an existing order as the controlling index order. */
}

HB_FUNC( SX_SETSOFTSEEK )
{
   sx_SetSoftSeek( hb_parl( 1 ) ? 1 : 0 ); /* True to set softseek ON */
}

HB_FUNC( SX_SKIP )
{
   sx_Skip( hb_parnl( 1 ) );              /* Number of records to skip */
}

/* -----------------20/01/2002 14:2-----------------
 * sx_SysProp() => Varies depending on the type of data being requested.
 * LONG  SDEAPI WINAPI sx_SysProp (WORD uiSysItem, PVOID vpData);
 * --------------------------------------------------*/
HB_FUNC( SX_SYSPROP )
{
   int i = hb_parni( 2 );
   hb_retni(
      sx_SysProp( ( WORD ) hb_parni( 1 ),            /* One of the predefined constant values. See Apollo.ch */
                  ( void * ) &i ) );
}
/*
HB_FUNC( SX_DISABLEAUTOOPEN )
{
   int i = 1;
   sx_SysProp( 1013, ( void * ) &i );
}
*/

HB_FUNC( SX_USE )
{
   hb_retni(
      sx_Use( ( PBYTE ) hb_parc( 1 ),      /* Filename */
              ( PBYTE ) hb_parc( 2 ),      /* Alias */
              ( SHORT ) hb_parni( 3 ),     /* OpenMode */
              ( SHORT ) hb_parni( 4 ) ) ); /* RDE Type */
}

HB_FUNC( SX_VERSION )
{
   hb_retc( ( char * ) sx_Version() );
}

HB_FUNC( SX_ZAP )
{
   sx_Zap();
}
