/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Some dbf structure related functions
 *
 * Copyright 2000 Alexander Kresin <alex@belacy.belgorod.su>
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

#include "extend.h"
#include "rddapi.h"

/*  $DOC$
 *  $FUNCNAME$
 *      FIELDTYPE()
 *  $CATEGORY$
 *      Data Base
 *  $ONELINER$
 *      Determines the type of a given field.
 *  $SYNTAX$
 *      FIELDTYPE(<nFieldNum>) --> cFieldType
 *  $ARGUMENTS$
 *      <nFieldNum> Data field , which type need to be determined.
 *  $RETURNS$
 *      FIELDTYPE() returns the character that designates the type of a given field:
 *      'C' - character string;
 *      'N' - numeric;
 *      'L' - logical;
 *      'D' - date;
 *      'M' - memo.
 *  $DESCRIPTION$
 *      This function determines the type of a field, designated by its number.
 *  $EXAMPLES$
 *      FUNCTION Main()
 *      LOCAL i
 *      USE Tests NEW
 *      FOR i = 1 TO FCOUNT()
 *        ? FieldType( i )
 *      NEXT
 *      USE
 *      RETURN NIL
 *  $TESTS$
 *
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is CA-CLIPPER TOOLS compatible
 *  $SEEALSO$
 *    FIELDSIZE(), FIELDDECI()
 *  $INCLUDE$
 *
 *  $END$
 */
 
HARBOUR HB_FIELDTYPE( void )
{
   USHORT uiField;
   LPFIELD pField;
   AREAP pArea;

   uiField = hb_parni( 1 );
   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   pField = pArea->lpFields + uiField - 1;

   hb_retc( ( char * ) &( pField->uiType ) );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FIELDSIZE()
 *  $CATEGORY$
 *      Data Base
 *  $ONELINER$
 *      Determines the size of a given field.
 *  $SYNTAX$
 *      FIELDSIZE(<nFieldNum>) --> nFieldSize
 *  $ARGUMENTS$
 *      <nFieldNum> Data field , which size need to be determined.
 *  $RETURNS$
 *      FIELDSIZE() returns the number that designates the size of a given field.
 *  $DESCRIPTION$
 *      This function determines the size of a field, designated by its number.
 *  $EXAMPLES$
 *      FUNCTION Main()
 *      LOCAL i
 *      USE Tests NEW
 *      FOR i = 1 TO FCOUNT()
 *        ? FieldSize( i )
 *      NEXT
 *      USE
 *      RETURN NIL
 *  $TESTS$
 *
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is CA-CLIPPER TOOLS compatible
 *  $SEEALSO$
 *    FIELDTYPE(), FIELDDECI()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FIELDSIZE( void )
{
   USHORT uiField;
   LPFIELD pField;
   AREAP pArea;

   uiField = hb_parni( 1 );
   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   pField = pArea->lpFields + uiField - 1;

   hb_retni( pField->uiLen );
}

/*  $DOC$
 *  $FUNCNAME$
 *      FIELDDECI()
 *  $CATEGORY$
 *      Data Base
 *  $ONELINER$
 *      Determines the number of decimal places of a given numeric field.
 *  $SYNTAX$
 *      FIELDDECI(<nFieldNum>) --> nFieldDeci
 *  $ARGUMENTS$
 *      <nFieldNum> Numeric data field , for which number of decimal
 *                  places need to be determined.
 *  $RETURNS$
 *      FIELDDECI() returns the numeric value that designates the number
 *                  of decimal places of a given field.
 *  $DESCRIPTION$
 *      This function determines the number of decimal places of a given numeric field.
 *  $EXAMPLES$
 *      FUNCTION Main()
 *      LOCAL i
 *      USE Tests NEW
 *      FOR i = 1 TO FCOUNT()
 *        ? FieldDeci( i )
 *      NEXT
 *      USE
 *      RETURN NIL
 *  $TESTS$
 *
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is CA-CLIPPER TOOLS compatible
 *  $SEEALSO$
 *    FIELDTYPE(), FIELDSIZE()
 *  $INCLUDE$
 *
 *  $END$
 */

HARBOUR HB_FIELDDECI( void )
{
   USHORT uiField;
   LPFIELD pField;
   AREAP pArea;

   uiField = hb_parni( 1 );
   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   pField = pArea->lpFields + uiField - 1;

   hb_retni( pField->uiDec );
}
