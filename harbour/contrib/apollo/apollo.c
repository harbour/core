/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * VistaSoftware's Apollo database driver. See http://www.VistaSoftware.com
 *
 * Copyright 2001 Patrick Mast <email@PatrickMast.com>
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

/* -----------------29/12/2001 19:23-----------------
 * NOTE: Functions are liste alfabeticly
 * --------------------------------------------------*/

#define HB_OS_WIN_32_USED
#define MAX_STR_LEN 255

#include "hbapi.h"
#include "sde60.h"


/* -----------------29/12/2001 19:21-----------------
 * sx_AppendBlank()
 * --------------------------------------------------*/
HB_FUNC( SX_APPENDBLANK )
{
   sx_AppendBlank( );
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Close()
 * --------------------------------------------------*/
HB_FUNC( SX_CLOSE )
{
    sx_Close( ) ;
}


/* -----------------29/12/2001 19:26-----------------
 * sx_Commit()
 * --------------------------------------------------*/
HB_FUNC( SX_COMMIT )
{
    sx_Commit( );
}


/* -----------------29/12/2001 19:59-----------------
 * sx_CreateExec()
 *  => NILL.
 * --------------------------------------------------*/
HB_FUNC( SX_CREATEEXEC )
{
    sx_CreateExec( );
}


/* -----------------29/12/2001 19:18-----------------
 * sx_CreateField()
 * --------------------------------------------------*/
HB_FUNC( SX_CREATEFIELD )
{
   sx_CreateField( hb_parc( 1 ),           /* Field name */
                   hb_parc( 2 ),           /* Field type */
                   hb_parni( 3 ),          /* Field lenght */
                   hb_parni( 4 ) );        /* Field decimals */
}


/* -----------------29/12/2001 19:59-----------------
 * sx_CreateNew()
 *  => The work area number assigned to the database.
 * --------------------------------------------------*/
HB_FUNC( SX_CREATENEW )
{
    hb_retni(
     sx_CreateNew( hb_parc( 1 ),           /* Field name */
                   hb_parc( 2 ),           /* Alias */
                   hb_parni( 3 ),          /* RDE Type */
                   hb_parni( 4 ) ) );      /* The maximum number of fields to be added to the file structure */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Eof()
 * --------------------------------------------------*/
HB_FUNC( SX_EOF )
{
    hb_retl( sx_Eof( ) );
}


/* -----------------29/12/2001 20:13-----------------
 * sx_GetDateJulian() => The date expressed as a long integer. Useful for date arithmetic.
 * Extracts the contents of a date field as a Julian number.
 * This number is equal to the number of days since January 1, 4713 BC.
 * However, only JULIAN dates equal or greater than January 1, 1000 are supported.
 * --------------------------------------------------*/
HB_FUNC( SX_GETDATEJULIAN )
{
   hb_retni(
    sx_GetDateJulian( hb_parc( 1 ) ) );    /* Field name  */
}


/* -----------------30/12/2001 13:04-----------------
 * sx_GetLogical() => True if the field evaluates as True, and False if not.
 * Determines whether a logical field contains a True or False value.
 * --------------------------------------------------*/
HB_FUNC( SX_GETLOGICAL )
{
   hb_retl(
    sx_GetLogical( hb_parc( 1 ) ) );       /* Field name  */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_GetString()
 * --------------------------------------------------*/
HB_FUNC( SX_GETSTRING )
{
   hb_retc(
    ( char * )sx_GetString( hb_parc( 1 ) ) );  /* Field name  */
}


/* -----------------30/12/2001 12:21-----------------
 * sx_GetVariant() => Character fields are returned as untrimmed strings.
 * --------------------------------------------------*/
HB_FUNC( SX_GETVARIANT )
{
   hb_retc(
    ( char * )sx_GetVariant( hb_parc( 1 ) ) );  /* Field name  */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Go()
 * --------------------------------------------------*/
HB_FUNC( SX_GO )
{
    sx_Go( hb_parni( 1 ) );                /* The record number to go */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_GoBottom()
 * --------------------------------------------------*/
HB_FUNC( SX_GOBOTTOM )
{
    sx_GoBottom( ) ;
}


/* -----------------29/12/2001 20:13-----------------
 * sx_GoTop()
 * --------------------------------------------------*/
HB_FUNC( SX_GOTOP )
{
    sx_GoTop( ) ;
}


/* -----------------29/12/2001 19:59-----------------
 * sx_IndexTag()
 * --------------------------------------------------*/
HB_FUNC( SX_INDEXTAG )
{
    hb_retni(
     sx_IndexTag( hb_parc( 1 ),            /* Field name */
                  hb_parc( 2 ),            /* Tag name */
                  hb_parc( 3 ),            /* Index expression as a string */
                  hb_parni( 4 ),           /* Option (0=Standard) (1=Unique) (2=Roll-Your-Own) */
                  hb_parl( 5 ),            /* True for a descend index */
                  hb_parc( 6) ) );         /* Condition */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_RecCOunt()
 * --------------------------------------------------*/
HB_FUNC( SX_RECCOUNT )
{
    hb_retni(
     sx_RecCount( ) );
}


/* -----------------29/12/2001 20:13-----------------
 * sx_RecNo()
 * --------------------------------------------------*/
HB_FUNC( SX_RECNO )
{
    hb_retni(
     sx_RecNo( ) );
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Reindex()
 * --------------------------------------------------*/
HB_FUNC( SX_REINDEX )
{
    sx_Reindex( );
}


/* -----------------29/12/2001 19:59-----------------
 * sx_Replace()
 * --------------------------------------------------*/
HB_FUNC( SX_REPLACE )
{

switch ( hb_parni( 2 ) )
{
 case R_INTEGER :
 case R_JULIAN  : sx_Replace( hb_parc( 1 ), hb_parni( 2 ), ( void * ) hb_parni( 3) ) ;  break;
 case R_LOGICAL : sx_Replace( hb_parc( 1 ), hb_parni( 2 ), ( void * ) hb_parni( 3) ) ;  break;  /* TODO: somthing is wrong here... */
 case R_LONG    : sx_Replace( hb_parc( 1 ), hb_parni( 2 ), ( void * ) hb_parnl( 3) ) ;  break;
 case R_DOUBLE  :
      {
        double d = hb_parnd( 3 );
        sx_Replace( hb_parc( 1 ), hb_parni( 2 ), ( void * ) &d );
        break;
      }
 case R_CHAR    :
 case R_DATESTR :
 case R_MEMO    :
 case R_BITMAP  :
 case R_BLOBFILE:
 case R_BLOBPTR : sx_Replace( hb_parc( 1 ), hb_parni( 2 ), ( void * ) hb_parc( 3)  ) ;  break;
 default:         sx_Replace( hb_parc( 1 ), hb_parni( 2 ), ( void * ) hb_parc( 3)  );
}

//    sx_Replace( hb_parc( 1 ),              /* Field name */
//                hb_parni( 2 ),             /* Data type */
//                hb_parc( 3) );             /* Data */

}


/* -----------------29/12/2001 20:13-----------------
 * sx_RLock()
 * --------------------------------------------------*/
HB_FUNC( SX_RLOCK )
{
    sx_Rlock( hb_parni( 1 ) );             /* The physical record number of the record to be locked. */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Seek()
 * --------------------------------------------------*/
HB_FUNC( SX_SEEK )
{
    hb_retl(
     sx_Seek( hb_parc( 1 ) ) );            /* The value to search for as a string */
}


/* -----------------30/12/2001 12:30-----------------
 * sx_SetCentury() => NILL
 * Indicates whether or not the two digits of the year designating
 * century are to be returned by sx_GetDateString as part of a date
 * string formatted according to the sx_SetDateFormat setting.
 * --------------------------------------------------*/
HB_FUNC( SX_SETCENTURY )
{
    sx_SetCentury( hb_parl( 1 ) );         /* If True, the century digits will be returned.
                                            * If False, they will not. */
}


/* -----------------30/12/2001 12:32-----------------
 * sx_SetDateFormat() => NILL
 * Defines the format of date strings returned by sx_GetDateString.
 * --------------------------------------------------*/
HB_FUNC( SX_SETDATEFORMAT )
{
    sx_SetDateFormat( hb_parni( 1 ) );     /* If True, the century digits will be returned.
                                            * If False, they will not. */
}


/* -----------------29/12/2001 19:59-----------------
 * sx_SetMemoBlockSize()
 * --------------------------------------------------*/
HB_FUNC( SX_SETMEMOBLOCKSIZE )
{
    sx_SetMemoBlockSize( hb_parni( 1 ) );  /* The new default block size.
                                            * The size must be a value from 1 through 1024. */
}


/* -----------------29/12/2001 19:59-----------------
 * sx_SetSoftSeek()
 * --------------------------------------------------*/
HB_FUNC( SX_SETSOFTSEEK )
{
    sx_SetSoftSeek( hb_parl( 1 ) );        /* True to set softseek ON */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Skip()
 * --------------------------------------------------*/
HB_FUNC( SX_SKIP )
{
    sx_Skip( hb_parni( 1 ) );              /* Number of records to skip */
}


/* -----------------29/12/2001 19:17-----------------
 * sx_Use()
 * --------------------------------------------------*/
HB_FUNC( SX_USE )
{
     hb_retni(
      sx_Use( hb_parc( 1 ),                /* Filename */
              hb_parc( 2 ),                /* Alias */
              hb_parni( 3 ),               /* OpenMode */
              hb_parni( 4) ));             /* RDE Type */
}


/* -----------------29/12/2001 19:17-----------------
 * sx_Version()
 * --------------------------------------------------*/
HB_FUNC( SX_VERSION )
{
   hb_retc(
    ( char * ) sx_Version() );
}


/* -----------------29/12/2001 19:18-----------------
 * sx_Zap()
 * --------------------------------------------------*/
HB_FUNC( SX_ZAP )
{
   sx_Zap( );
}



/*
2001-12-30 13:47 UTC+0100 Patrick Mast <email@patrickmast.com>
   * contrib/apollo/apollo.c
     + Added function sx_GetDateJulian()
     + Added function sx_GetVariant()
     + Added function sx_SetCentury()
     + Added function sx_SetDateFormat()
     + Added function sx_GetLogical()
     * modified function sx_Replace()
   Note: Replace a logical value with sx_Replace() does not work yet.
   * contrib/apollo/test/apollo.prg
     * Added added functions in the test application.
     * enhanced code
     * Added and changed defines

*/


/*
sx_AppendBlank()
sx_Close()
sx_Commit()
sx_CreateExec()
sx_CreateField()
sx_CreateNew()
sx_Eof()
sx_GetDateJulian()
sx_GetLogical()
sx_GetString()
sx_GetVariant()
sx_Go()
sx_GoBottom()
sx_GoTop()
sx_IndexTag()
sx_RecCOunt()
sx_RecNo()
sx_Reindex()
sx_Replace()
sx_RLock()
sx_Seek()
sx_SetDateFormat()
sx_SetCentury()
sx_SetMemoBlockSize()
sx_SetSoftSeek()
sx_Skip()
sx_Use()
sx_Version()
sx_Zap()
*/