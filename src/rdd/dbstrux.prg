/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __dbCopyStruct(), __dbCopyXStruct(), __dbCreate() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "dbstruct.ch"

FUNCTION __dbCopyStruct( cFileName, aFieldList )
   RETURN dbCreate( cFileName, __dbStructFilter( dbStruct(), aFieldList ) )

FUNCTION __dbCopyXStruct( cFileName )
   LOCAL nOldArea
   LOCAL aStruct

   LOCAL oError
   LOCAL lError := .F.

   IF Empty( aStruct := dbStruct() )
      RETURN .F.
   ENDIF

   nOldArea := Select()

   BEGIN SEQUENCE

      dbSelectArea( 0 )
      __dbCreate( cFileName, NIL, NIL, .F., NIL )

      AEval( aStruct, {| aField | ;
         iif( aField[ DBS_TYPE ] == "C" .AND. aField[ DBS_LEN ] > 255,;
            ( aField[ DBS_DEC ] := Int( aField[ DBS_LEN ] / 256 ), aField[ DBS_LEN ] := aField[ DBS_LEN ] % 256 ), ),;
         dbAppend(),;
         FIELD->FIELD_NAME := aField[ DBS_NAME ],;
         FIELD->FIELD_TYPE := aField[ DBS_TYPE ],;
         FIELD->FIELD_LEN := aField[ DBS_LEN ],;
         FIELD->FIELD_DEC := aField[ DBS_DEC ] } )

   /* NOTE: CA-Cl*pper has a bug, where only a plain RECOVER statement is
            used here (without the USING keyword), so oError will always be NIL. */
   RECOVER USING oError
      lError := .T.
   END SEQUENCE

   IF Select() != nOldArea
      dbCloseArea()
      dbSelectArea( nOldArea )
   ENDIF

   IF lError
      Break( oError )
   ENDIF

   RETURN .T.

/* NOTE: Compared to CA-Cl*pper, Harbour has two extra parameters
         (cCodePage, nConnection). */

FUNCTION __dbCreate( cFileName, cFileFrom, cRDD, lNew, cAlias, cCodePage, nConnection )
   LOCAL nOldArea := Select()
   LOCAL aStruct := {}

   LOCAL oError

   __defaultNIL( @lNew, .F. )

   IF cAlias == NIL
      hb_FNameSplit( cFileName, NIL, @cAlias )
   ENDIF

   IF Used() .AND. ! lNew
      dbCloseArea()
   ENDIF

   BEGIN SEQUENCE

      IF Empty( cFileFrom )

         dbCreate( cFileName, { ;
            { "FIELD_NAME", "C", 10, 0 }, ;
            { "FIELD_TYPE", "C",  1, 0 }, ;
            { "FIELD_LEN" , "N",  3, 0 }, ;
            { "FIELD_DEC" , "N",  3, 0 } }, ;
            cRDD, .F., cAlias, NIL, cCodePage, nConnection )
      ELSE
         dbUseArea( lNew, NIL, cFileFrom, "" )

         dbEval( {|| AAdd( aStruct, { ;
            FIELD->FIELD_NAME ,;
            FIELD->FIELD_TYPE ,;
            FIELD->FIELD_LEN ,;
            FIELD->FIELD_DEC } ) } )
         dbCloseArea()

         IF lNew
            dbSelectArea( nOldArea )
         ENDIF

         /* Type detection is more in sync with dbCreate() logic in Harbour, as lowercase "C"
            and padded/continued strings ("C ", "C...") are also accepted. */

         AEval( aStruct, {| aField | iif( Upper( Left( aField[ DBS_TYPE ], 1 ) ) == "C" .AND. aField[ DBS_DEC ] != 0,;
            ( aField[ DBS_LEN ] += aField[ DBS_DEC ] * 256,;
              aField[ DBS_DEC ] := 0 ), NIL ) } )

         dbCreate( cFileName, aStruct, cRDD, lNew, cAlias, NIL, cCodePage, nConnection )

      ENDIF

   RECOVER USING oError
      dbCloseArea()
      Break( oError )
   END SEQUENCE

   RETURN Used()

/* NOTE: Internal helper function, CA-Cl*pper name is: __FLedit() */

FUNCTION __dbStructFilter( aStruct, aFieldList )
   LOCAL aStructFiltered
   LOCAL bFindName
   LOCAL cName

   IF Empty( aFieldList )
      RETURN aStruct
   ENDIF

   /* Build a filtered list of the requested fields. */

   aStructFiltered := {}
   bFindName := {| aField | aField[ DBS_NAME ] == cName }

   AEval( aFieldList, {| cFieldName, nIndex | ;
         cName := RTrim( Upper( cFieldName ) ),;
         nIndex := AScan( aStruct, bFindName ),;
         iif( nIndex == 0, NIL, AAdd( aStructFiltered, aStruct[ nIndex ] ) ) } )

   RETURN aStructFiltered
