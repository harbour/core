/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * NATION undocumented functions
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    _NATSORTVER()
 *    _NATMSGVER()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapilng.h"

/* NOTE: Ad-hoc names mostly taken from various Clipper source files.
         These should be named properly if exported outside this file.
         [vszakats] */

#define _DIR_HEADER             1       /* "Database Files    # Records    Last Update     Size" */
#define _LF_SAMPLES             2       /* "Do you want more samples?" */
#define _RF_PAGENO              3       /* "Page No." */
#define _RF_SUBTOTAL            4       /* "** Subtotal **" */
#define _RF_SUBSUBTOTAL         5       /* "* Subsubtotal *" */
#define _RF_TOTAL               6       /* "*** Total ***" */
#define _GET_INSERT_ON          7       /* "Ins" */
#define _GET_INSERT_OFF         8       /* "   " */
#define _GET_INVD_DATE          9       /* "Invalid Date" */
#define _GET_RANGE_FROM         10      /* "Range: " */
#define _GET_RANGE_TO           11      /* " - " */
#define _LF_YN                  12      /* "Y/N" */ /* NOTE: This must be in uppercase. [vszakats] */
#define _INVALID_EXPR           13      /* "INVALID EXPRESSION" */

char * hb_nationGetMsg( USHORT uiMsg )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_nationGetMsg(%hu)", uiMsg));

   return ( uiMsg >= 1 && uiMsg <= 13 ) ? ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_NATMSG + uiMsg - 1 ) : "";
}

#ifdef HB_C52_UNDOC

HB_FUNC( ISAFFIRM )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

   hb_retl( pItem && hb_itemGetCLen( pItem ) >= 1 && toupper( hb_itemGetCPtr( pItem )[ 0 ] ) == ( ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_NATMSG + _LF_YN - 1 ) )[ 0 ] );
}

HB_FUNC( ISNEGATIVE )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

   hb_retl( pItem && hb_itemGetCLen( pItem ) >= 1 && toupper( hb_itemGetCPtr( pItem )[ 0 ] ) == ( ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_NATMSG + _LF_YN - 1 ) )[ 2 ] );
}

HB_FUNC( NATIONMSG )
{
   if( hb_pcount() == 0 )
      /* TODO: Replace this with Language API call. */
      hb_retc( "Invalid argument" );
   else if( ISNUM( 1 ) )
      hb_retc( hb_nationGetMsg( hb_parni( 1 ) ) );
   else
      hb_retc( "" );
}

/* NOTE: Intentionally using one leading underscore, like in Clipper.
         [vszakats] */

HB_FUNC( _NATSORTVER )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATSORT v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATSORT v1.3i x19 06/Mar/95" */

   hb_retc( "NATSORT (Harbour)" );
}

/* NOTE: Intentionally using one leading underscore, like in Clipper.
         [vszakats] */

HB_FUNC( _NATMSGVER )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATMSGS v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATMSGS v1.3i x19 06/Mar/95" */

   hb_retc( "NATMSGS (Harbour)" );
}

#endif
