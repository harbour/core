/*
 * Harbour Project source code:
 * Document generator, templates
 *
 * Copyright 2009 April White <april users.sourceforge.net>
 * www - http://harbour-project.org
 *
 * Portions of this project are based on hbdoc
 *    Copyright 1999-2003 Luiz Rafael Culik <culikr@uol.com.br>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbdoc.ch"

#include "hbclass.ch"

/* a class that will hold one entry */
CREATE CLASS Entry

   EXPORTED:

   CLASS VAR Fields AS ARRAY INIT { ;
      { "DOC",          "Doc" }, ;
      { "TEMPLATE",     "Template" }, ;
      { "NAME",         "" }, ;
      { "CATEGORY",     "Category" }, ;
      { "SUBCATEGORY",  "Sub category" }, ;
      { "ONELINER",     "" }, ;
      { "SYNTAX",       "Syntax" }, ;
      { "ARGUMENTS",    "Argument(s)" }, ;
      { "RETURNS",      "Returns" }, ;
      { "DESCRIPTION",  "Description" }, ;
      { "DATALINK",     "Data link" }, ;
      { "DATANOLINK",   "Data no link" }, ;
      { "METHODSLINK",  "Methods link" }, ;
      { "METHODSNOLINK","Methods no link" }, ;
      { "EXAMPLES",     "Example(s)" }, ;
      { "TESTS",        "Test(s)" }, ;
      { "STATUS",       "Status" }, ;      /* ::hConstraint[ "status" ] is the constraint list */
      { "COMPLIANCE",   "Compliance" }, ;  /* ::hConstraint[ "compliance" ] is the constraint list */
      { "PLATFORMS",    "Platform(s)" }, ; /* ::hConstraint[ "platforms" ] is the constraint list */
      { "FILES",        "File(s)" }, ;
      { "SEEALSO",      "See also" }, ;
      { "END",          "End" } }

#define _S TPL_START
#define _E TPL_END
#define _T TPL_TEMPLATE
#define _R TPL_REQUIRED
#define _O TPL_OPTIONAL
#define _P TPL_PREFORMATTED
#define _U TPL_OUTPUT

   // this is best viewed with a fixed-width font
   // the columns of this array correspond to the elements of Fields
   CLASS VAR Templates AS ARRAY INIT { ;
      { "Template"      , { _S, _T,  0+_U,  0, _O   ,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U, _E } }, ;
      { "Document"      , { _S, _T, _R+_U, _R, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Function"      , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "C Function"    , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Procedure"     , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U,     0, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Command"       , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Class"         , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U, _R+_U, _R+_U, _O+_U, _O+_U, _O+_U, _O+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Class method"  , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _E } }, ;
      { "Class data"    , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _E } }, ;
      { "Run time error", { _S, _T, _R+_U, _R,  0   , _O+_U,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U, _O+_U,  0+_U,  0+_U, _O+_U, _E } } }

   METHOD New( cType, hConstraint ) CONSTRUCTOR
   METHOD IsField( c, nType )
   METHOD IsTemplate( cType )
   METHOD SetTemplate( cTemplate )
   METHOD IsConstraint( cSectionName, cSection )
   METHOD IsComplete( cIncompleteFielsList )
   METHOD IsPreformatted( cField )
   METHOD IsRequired( cField )
   METHOD IsOptional( cField )
   METHOD IsOutput( cField )
   METHOD FieldName( cField )
   METHOD CategoryIndex( cCategory )
   METHOD SubcategoryIndex( cCategory, cSubcategory )

   VAR Group AS ARRAY
   VAR filename AS STRING
   VAR type_ AS STRING
   VAR sourcefile_ AS STRING
   VAR sourcefileversion_ AS STRING
   VAR uid_ AS STRING
   VAR hConstraint AS HASH

   CLASS VAR uid__ AS INTEGER INIT 0

ENDCLASS

METHOD New( cType, hConstraint ) CLASS Entry

   ::hConstraint := hConstraint

   ::uid_ := hb_ntos( ++::uid__ )
   IF ! __objHasData( self, ::Fields[ 1 ][ 1 ] )
      AEval( ::Fields, {| a | __objAddData( self, a[ 1 ] ) } )
   ENDIF
   IF HB_ISSTRING( cType )
      ::Group := ::Templates[ AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) ][ 2 ]
   ENDIF

   RETURN self

METHOD IsField( c, nType ) CLASS Entry

   LOCAL idx
   LOCAL lResult

   IF ( lResult := ( idx := AScan( ::Fields, {| a | Upper( a[ 1 ] ) == Upper( c ) } ) ) > 0 )
      IF ::Group[ idx ] == 0
         lResult := .F.
      ELSEIF nType != NIL .AND. hb_bitAnd( ::Group[ idx ], nType ) != nType
         lResult := .F.
      ENDIF
   ENDIF

   RETURN lResult

METHOD IsTemplate( cType ) CLASS Entry
   RETURN AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) > 0

METHOD SetTemplate( cTemplate ) CLASS Entry

   LOCAL aData := Array( Len( ::Fields ) )
   LOCAL idx

   ::Group := ::Templates[ AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } ) ][ 2 ]
   FOR idx := 1 TO Len( aData )
      IF ::Fields[ idx ][ 1 ] == "TEMPLATE"
         aData[ idx ] := { ::Fields[ idx ][ 1 ], cTemplate }
      ELSE
         aData[ idx ] := { ::Fields[ idx ][ 1 ], iif( ::Group[ idx ] == TPL_REQUIRED, NIL, "" ) }
      ENDIF
   NEXT
   __objSetValueList( self, aData )

   RETURN self

METHOD IsConstraint( cSectionName, cSection ) CLASS Entry

   LOCAL lResult
   LOCAL idx := AScan( ::Fields, {| a | a[ 1 ] == cSectionName } )

   IF hb_bitAnd( ::Group[ idx ], hb_bitAnd( TPL_REQUIRED, TPL_OPTIONAL ) ) == 0
      lResult := .T.
   ELSEIF cSectionName $ ::hConstraint
      lResult := ;
         hb_AScan( ::hConstraint[ cSectionName ], cSection, , , .T. ) .OR. ;
         hb_AScan( ::hConstraint[ cSectionName ], Parse( cSection, "," ), , , .T. )
   ELSE
      lResult := .T.
   ENDIF

   RETURN lResult

METHOD IsComplete( cIncompleteFielsList ) CLASS Entry

   LOCAL lResult := .T.
   LOCAL idx

   cIncompleteFielsList := ""

   FOR idx := 1 TO Len( ::Fields )
      IF hb_bitAnd( ::Group[ idx ], TPL_REQUIRED ) != 0 .AND. Empty( ::&( ::Fields[ idx ][ 1 ] ) )
         cIncompleteFielsList += "," + ::Fields[ idx ][ 1 ]
         lResult := .F.
      ENDIF
   NEXT

   cIncompleteFielsList := SubStr( cIncompleteFielsList, 2 )

   RETURN lResult

METHOD IsPreformatted( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_PREFORMATTED ) != 0

METHOD IsRequired( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_REQUIRED ) != 0

METHOD IsOptional( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_OPTIONAL ) != 0

METHOD IsOutput( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_OUTPUT ) != 0

METHOD FieldName( cField ) CLASS Entry
   RETURN ::Fields[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ][ 2 ]

METHOD CategoryIndex( cCategory ) CLASS Entry
   RETURN AScan( ::hConstraint[ "categories" ], {| a | HB_ISARRAY( a ) .AND. Len( a ) >= 1 .AND. a[ 1 ] == cCategory } )

METHOD SubcategoryIndex( cCategory, cSubcategory ) CLASS Entry
   RETURN ::CategoryIndex( cCategory ) >= 1 .AND. ;
      hb_AScan( ::hConstraint[ "categories" ][ ::CategoryIndex( cCategory ) ][ 2 ], cSubcategory, , , .T. )
