/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DBCOPYSTRUCT(), __DBCOPYXSTRUCT(), __DBCREATE() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#include "hbsetup.ch"

#include "common.ch"
#include "dbstruct.ch"

FUNCTION __dbCopyStruct( cFileName, aFieldList )
   RETURN dbCreate( cFileName, __dbStructFilter( dbStruct(), aFieldList ) )

FUNCTION __dbCopyXStruct( cFileName )
   LOCAL nOldArea
   LOCAL oError
   LOCAL aStruct

   IF Empty( aStruct := dbStruct() )
      RETURN .F.
   ENDIF

   nOldArea := Select()

   BEGIN SEQUENCE

      dbSelectArea( 0 )
      __dbCreate( cFileName, NIL, NIL, .F., NIL )

      aEval( aStruct, {| aField | iif( aField[ DBS_TYPE ] == "C" .AND. aField[ DBS_LEN ] > 255, ;
         ( aField[ DBS_DEC ] := Int( aField[ DBS_LEN ] / 256 ), aField[ DBS_LEN ] := aField[ DBS_LEN ] % 256 ), NIL ) } )

      aEval( aStruct, {| aField | dbAppend(),;
                                  FIELD->FIELD_NAME := aField[ DBS_NAME ],;
                                  FIELD->FIELD_TYPE := aField[ DBS_TYPE ],;
                                  FIELD->FIELD_LEN := aField[ DBS_LEN ], ;
                                  FIELD->FIELD_DEC := aField[ DBS_DEC ] } )

   /* NOTE: CA-Cl*pper has a bug, where only a plain RECOVER statement is
            used here (without the USING keyword), so oError will always be 
            NIL. */
   RECOVER USING oError
   END SEQUENCE

   IF Select() != nOldArea
      dbCloseArea()
      dbSelectArea( nOldArea )
   ENDIF

   IF oError != NIL
      Break( oError )
   ENDIF

   RETURN .T.

FUNCTION __dbCreate( cFileName, cFileFrom, cRDDName, lNew, cAlias )
   LOCAL nOldArea := Select()
   LOCAL aStruct := {}
   LOCAL oError

   DEFAULT lNew TO .F.

   IF Used() .AND. ! lNew
      dbCloseArea()
   ENDIF

   BEGIN SEQUENCE

      IF Empty( cFileFrom )

         dbCreate( cFileName, {;
            { "FIELD_NAME", "C", 10, 0 },;
            { "FIELD_TYPE", "C",  1, 0 },;
            { "FIELD_LEN" , "N",  3, 0 },;
            { "FIELD_DEC" , "N",  3, 0 } }, cRDDName )
         dbUseArea( .F., cRDDName, cFileName, cAlias )

      ELSE

         dbUseArea( lNew,, cFileFrom ) 

         dbEval( {|| aAdd( aStruct, { FIELD->FIELD_NAME ,;
                                      FIELD->FIELD_TYPE ,;
                                      FIELD->FIELD_LEN ,;
                                      FIELD->FIELD_DEC } ) } )
         dbCloseArea()

         IF lNew
            dbSelectArea( nOldArea )
         ENDIF

         aEval( aStruct, {| aField | iif( aField[ DBS_TYPE ] == "C" .AND. aField[ DBS_DEC ] != 0, ;
            aField[ DBS_LEN ] := aField[ DBS_LEN ] + aField[ DBS_DEC ] * 256, NIL ) } )

         dbCreate( cFileName, aStruct, cRDDName )
         dbUseArea( lNew, cRDDName, cFileName, cAlias )

      ENDIF

   RECOVER USING oError
      dbCloseArea()
      Break( oError )
   END SEQUENCE

   RETURN Used()

/* NOTE: Undocumented, internal Clipper function */

#ifdef HB_C52_UNDOC

FUNCTION __FLEDIT( aStruct, aFieldList )
   RETURN __dbStructFilter( aStruct, aFieldList )

#endif

/* NOTE: Internal helper function, CA-Cl*pper name is: __FLEDIT() */

FUNCTION __dbStructFilter( aStruct, aFieldList )
   LOCAL aStructFiltered
   LOCAL bFindName
   LOCAL cName

   IF Empty( aFieldList )
      RETURN aStruct
   ENDIF

   /* Build a filtered list of the requested fields. */

   aStructFiltered := {}
   bFindName := {| aField | aField[ DBS_NAME ] == RTrim( Upper(cName ) ) }

   aEval( aFieldList, {| cFieldName, nIndex | cName := cFieldName,nIndex := aScan( aStruct, bFindName ),;
      iif( nIndex == 0, NIL, aAdd( aStructFiltered, aStruct[ nIndex] ) ) } )

   RETURN aStructFiltered

