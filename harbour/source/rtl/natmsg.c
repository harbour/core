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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    _NATSORTVER()
 *    _NATMSGVER()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"

/* TODO: Use the Language API to retrieve these strings. */
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

static char * s_szMessages[] =
{
   "Database Files    # Records    Last Update     Size",
   "Do you want more samples?",
   "Page No.",
   "** Subtotal **",
   "* Subsubtotal *",
   "*** Total ***",
   "Ins",
   "   ",
   "Invalid date",
   "Range: ",
   " - ",
   "Y/N",
   "INVALID EXPRESSION"
};

char * hb_nationGetMsg( USHORT uiMsg )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_nationGetMsg(%hu)", uiMsg));

   return ( uiMsg >= 1 && uiMsg <= ( sizeof( s_szMessages ) / sizeof( char * ) ) ) ? s_szMessages[ uiMsg - 1 ] : "";
}

HB_FUNC( ISAFFIRM )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   hb_retl( pItem && hb_itemGetCLen( pItem ) >= 1 && toupper( hb_itemGetCPtr( pItem )[ 0 ] ) == s_szMessages[ _LF_YN - 1 ][ 0 ] );
}

HB_FUNC( ISNEGATIVE )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   hb_retl( pItem && hb_itemGetCLen( pItem ) >= 1 && toupper( hb_itemGetCPtr( pItem )[ 0 ] ) == s_szMessages[ _LF_YN - 1 ][ 2 ] );
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

