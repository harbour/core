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


/* -----------------29/12/2001 19:18-----------------
 * sx_CreateField()
 * --------------------------------------------------*/
HB_FUNC( SX_CREATEFIELD )
{
   sx_CreateField( hb_parc( 1 ),        /* Field name */
                   hb_parc( 2 ),        /* Field type */
                   hb_parni( 3 ),       /* Field lenght */
                   hb_parni( 4 ) );     /* Field decimals */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Eof()
 * --------------------------------------------------*/
HB_FUNC( SX_EOF )
{
    hb_retl( sx_Eof( ) );
}


/* -----------------29/12/2001 20:13-----------------
 * sx_GetString()
 * --------------------------------------------------*/
HB_FUNC( SX_GETSTRING )
{
   char *szString=(char*)sx_GetString( hb_parc( 1 ) );  /* Field name  */
   hb_retc(szString);
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
     sx_IndexTag( hb_parc( 1 ),         /* Field name */
                  hb_parc( 2 ),         /* Tag name */
                  hb_parc( 3 ),         /* Index expression as a string */
                  hb_parni( 4 ),        /* Option (0=Standard) (1=Unique) (2=Roll-Your-Own) */
                  hb_parl( 5 ),         /* True for a descend index */
                  hb_parc( 6) ) );      /* Condition */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_RecNo()
 * --------------------------------------------------*/
HB_FUNC( SX_RECNO )
{
    hb_retni( sx_RecNo( ) );
}


/* -----------------29/12/2001 19:59-----------------
 * sx_Replace()
 * --------------------------------------------------*/
HB_FUNC( SX_REPLACE )
{
    sx_Replace( hb_parc( 1 ),           /* Field name */
                hb_parni( 2 ),          /* Data type */
                hb_parc( 3) );          /* Data */
}


/* -----------------29/12/2001 20:13-----------------
 * sx_Skip()
 * --------------------------------------------------*/
HB_FUNC( SX_SKIP )
{
    sx_Skip( hb_parni( 1 ) );           /* Number of records to skip */
}



/* -----------------29/12/2001 19:17-----------------
 * sx_Use()
 * --------------------------------------------------*/
HB_FUNC( SX_USE )
{
 hb_retni( sx_Use( hb_parc( 1 ),        /* Filename */
                   hb_parc( 2 ),        /* Alias */
                   hb_parni( 3 ),       /* OpenMode */
                   hb_parni( 4) ));     /* RDE Type */
}


/* -----------------29/12/2001 19:17-----------------
 * sx_Version()
 * --------------------------------------------------*/
HB_FUNC( SX_VERSION )
{
   char *szVersion=(char*)sx_Version();
   hb_retc(szVersion);
   /*hb_xfree(szVersion);*/
}


/* -----------------29/12/2001 19:18-----------------
 * sx_Zap()
 * --------------------------------------------------*/
HB_FUNC( SX_ZAP )
{
   sx_Zap( );
}
