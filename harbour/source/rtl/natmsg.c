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
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    HB__NATSORTVER()
 *    HB__NATMSGVER()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "extend.h"
#include "itemapi.h"

/* TODO: Use the Language API to retrieve these strings. */
/* NOTE: Ad-hoc names mostly taken from various Clipper source files. 
         These should be named properly if exported outside this file. */

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
#define _LF_YN                  12      /* "Y/N" */ /* NOTE: This should be in uppercase. */
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

/*  $DOC$
 *  $FUNCNAME$
 *      ISAFFIRM()
 *  $CATEGORY$
 *      NATION
 *  $ONELINER$
 *      Checks if passed char is an affirmation char
 *  $SYNTAX$
 *      ISAFFIRM( <cChar> ) --> <lTrueOrFalse>
 *  $ARGUMENTS$
 *      <cChar> is a char or string of chars
 *  $RETURNS$
 *      True if passed char is an affirmation char, false otherwise
 *  $DESCRIPTION$
 *      This function it is used to check if a user input is true or not
 *      regarding of the msgxxx module used.
 *  $EXAMPLES$
 *      // Wait until user enters Y
 *      DO WHILE !ISAFFIRM( cYesNo )
 *        ACCEPT "Sure: " TO cYesNo
 *      END DO
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ISAFFIRM() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      ISNEGATIVE(),NATIONMSG()
 *  $END$
 */

HARBOUR HB_ISAFFIRM( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   hb_retl( pItem && hb_itemGetCLen( pItem ) >= 1 && toupper( hb_itemGetCPtr( pItem )[ 0 ] ) == s_szMessages[ _LF_YN - 1 ][ 0 ] );
}

/*  $DOC$
 *  $FUNCNAME$
 *      ISNEGATIVE()
 *  $CATEGORY$
 *      NATION
 *  $ONELINER$
 *      Checks if passed char is a negation char
 *  $SYNTAX$
 *      ISNEGATIVE( <cChar> ) --> <lTrueOrFalse>
 *  $ARGUMENTS$
 *      <cChar> is a char or string of chars
 *  $RETURNS$
 *      True if passed char is a negation char, false otherwise
 *  $DESCRIPTION$
 *      This function it is used to check if a user input is true or not
 *      regarding of the msgxxx module used.
 *  $EXAMPLES$
 *      // Wait until user enters N
 *      DO WHILE !ISNEGATIVE( cYesNo )
 *        ACCEPT "Sure: " TO cYesNo
 *      END DO
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      ISNEGATIVE() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      ISAFFIRM(),NATIONMSG()
 *  $END$
 */

HARBOUR HB_ISNEGATIVE( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   hb_retl( pItem && hb_itemGetCLen( pItem ) >= 1 && toupper( hb_itemGetCPtr( pItem )[ 0 ] ) == s_szMessages[ _LF_YN - 1 ][ 2 ] );
}

/*  $DOC$
 *  $FUNCNAME$
 *      NATIONMSG()
 *  $CATEGORY$
 *      NATION
 *  $ONELINER$
 *      Returns international strings messages.
 *  $SYNTAX$
 *      NATIONMSG( <nMsg> ) --> <cMessage>
 *  $ARGUMENTS$
 *      <nMsg> is the message number you want to get
 *  $RETURNS$
 *      If <nMsg> is a valid message selector return the message, if <nMsg>
 *      is nil returns "Invalid Argument" and if <nMsg> is any other type it
 *      returns an empty string.
 *  $DESCRIPTION$
 *      This functions returns international message descriptions.
 *  $EXAMPLES$
 *      // Displays "Sure Y/N: "  and waits until user enters Y
 *      // Y/N is the string for NATIONMSG( 12 ) with default natmsg module.
 *      DO WHILE !ISAFFIRM( cYesNo )
 *        ACCEPT "Sure " + NATIONMSG( 12 ) + ": " TO cYesNo
 *      END DO
 *  $STATUS$
 *      C
 *  $COMPLIANCE$
 *      NATIONMSG() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      ISAFFIRM(),ISNEGATIVE()
 *  $END$
 */

HARBOUR HB_NATIONMSG( void )
{
   if( hb_pcount() == 0 )
      /* TODO: Replace this with Language API call. */
      hb_retc( "Invalid argument" );
   else if( ISNUM( 1 ) )
      hb_retc( hb_nationGetMsg( hb_parni( 1 ) ) );
   else
      hb_retc( "" );
}

/* NOTE: Intentionally using one leading underscore, like in Clipper */

HARBOUR HB__NATSORTVER( void )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATSORT v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATSORT v1.3i x19 06/Mar/95" */

   hb_retc( "NATSORT (Harbour)" );
}

/* NOTE: Intentionally using one leading underscore, like in Clipper */

HARBOUR HB__NATMSGVER( void )
{
   /* NOTE: CA-Cl*pper 5.2e Intl. will return: "NATMSGS v1.2i x14 19/Mar/93" */
   /* NOTE: CA-Cl*pper 5.3  Intl. will return: "NATMSGS v1.3i x19 06/Mar/95" */

   hb_retc( "NATMSGS (Harbour)" );
}
