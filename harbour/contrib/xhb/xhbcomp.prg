/*
* $Id$
*/

/*
 * Harbour Project source code:
 * xhb compatibility functions
 *
 * Copyright 2007 Viktor Szakats <harbour.01 syenar.hu>
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

#include "common.ch"
#include "hbclass.ch"

ANNOUNCE XHB_LIB

INIT PROCEDURE xhb_Init()

   /* Add calls to do initial settings to Harbour to be more compatible with xhb. */

   RETURN

FUNCTION xhb_AIns( a, n, x )

   AIns( a, n )
   IF PCount() > 2
      a[ n ] := x
   ENDIF

   RETURN a

FUNCTION xhb_ADel( a, n, l )

   ADel( a, n )
   IF PCount() > 2 .AND. ISLOGICAL( l ) .AND. l
      ASize( a, Len( a ) - 1 )
   ENDIF

   RETURN a

/*
* Overloading of "$","[]" operators in scalar classes
* "$" is done for types:
*   HBCharacter
*   HBDate
*   HBLogical
*   HBNil
*   HBNumeric
*   // HBPointer
*   // HBSymbol
* "[]" is only for HBCharacter type
*   TODO: assign. in the form <string>[n] := <char>
* 2007 tfonrouge
*/
/*
  HBScalar
*/
CLASS HBScalar
  METHOD IsIn OPERATOR "$"
ENDCLASS

/*
  HBScalar:IsIn : "$" operator
*/
METHOD IsIn( itm ) CLASS HBScalar
  IF HB_IsArray( itm )
    RETURN AScan( itm, Self ) > 0
  ENDIF
  IF HB_IsHash( itm )
    RETURN HB_HHasKey( itm, Self )
  ENDIF
  /*
   * we need to raise a error here ? when ?
   */
RETURN .F. /*  */

/*
  HBArray
*/
CLASS HBArray FROM HBScalar
  METHOD IsIn OPERATOR "$" // <array> $ <any>  returns .F.
ENDCLASS

/*
  HBArray:IsIn : "$" operators
*/
METHOD IsIn CLASS HBArray
RETURN .F.

/*
  HBCharacter
*/
CLASS HBCharacter FROM HBScalar
  METHOD Index  OPERATOR "[]"
ENDCLASS

/*
  HBCharacter:Index
*/
METHOD Index( n/*, char*/ ) CLASS HBCharacter
  /*
  IF PCount()>1
    Self := Stuff( Self, n, Len( char ), char )
    RETURN Self
  ENDIF
  */
RETURN SubStr( Self, n , 1 )

/*
  HBDate
*/
CLASS HBDate FROM HBScalar
ENDCLASS

/*
  HBHash
*/
CLASS HBHash FROM HBScalar
  METHOD IsIn OPERATOR "$" // <hash> $ <any>  returns .F.
ENDCLASS

/*
  HBHash:IsIn : "$" operators
*/
METHOD IsIn CLASS HBHash
RETURN .F.

/*
  HBLogical
*/
CLASS HBLogical FROM HBScalar
ENDCLASS

/*
  HBNil
*/
CLASS HBNil FROM HBScalar
ENDCLASS

/*
  HBNumeric
*/
CLASS HBNumeric FROM HBScalar
ENDCLASS

/*
  HBPointer
*/
/*
CLASS HBPointer FROM HBScalar
ENDCLASS
*/

/*
  HBSymbol
*/
/*
CLASS HBSymbol FROM HBScalar
ENDCLASS
*/
