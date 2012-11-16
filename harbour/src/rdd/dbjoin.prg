/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __dbJoin() function
 *
 * Copyright 2005 Pavel Tsarenko <tpe2@mail.ru>
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

#include "dbstruct.ch"

/* NOTE: Compared to CA-Cl*pper, Harbour has three extra parameters
         (cRDD, nConnection, cCodePage). */

FUNCTION __dbJoin( cAlias, cFile, aFields, bFor, cRDD, nConnection, cCodePage )

   LOCAL nMaster := Select()
   LOCAL nDetail := Select( cAlias )
   LOCAL nResult
   LOCAL aStruct
   LOCAL aJoinList

   LOCAL oError
   LOCAL lError := .F.

   dbSelectArea( nMaster )
   IF Empty( aStruct := __FieldTwo( cAlias, aFields ) )
      RETURN .F.
   ENDIF

   BEGIN SEQUENCE

      dbCreate( cFile, aStruct, cRDD, .T., "", NIL, cCodePage, nConnection )
      nResult := Select()
      aJoinList := __JoinList( nMaster, nDetail, nResult, aStruct )

      dbSelectArea( nMaster )
      dbGoTop()
      DO WHILE ! Eof()

         dbSelectArea( nDetail )
         dbGoTop()
         DO WHILE ! Eof()

            dbSelectArea( nMaster )
            IF Eval( bFor )
               __doJoinList( aJoinList )
            ENDIF

            dbSelectArea( nDetail )
            dbSkip()
         ENDDO

         dbSelectArea( nMaster )
         dbSkip()
      ENDDO

   RECOVER USING oError
      lError := .T.
   END SEQUENCE

   IF nResult != NIL
      dbSelectArea( nResult )
      dbCloseArea()
   ENDIF

   dbSelectArea( nMaster )

   IF lError
      Break( oError )
   ENDIF

   RETURN .T.

STATIC FUNCTION __FieldTwo( cAlias, aFields )

   LOCAL aFldTemp
   LOCAL bFind
   LOCAL aStruct
   LOCAL cField

   IF Empty( aFields )
      RETURN dbStruct()
   ENDIF

   aFldTemp := {}
   AEval( aFields, {| cFld | AAdd( aFldTemp, RTrim( Upper( cFld ) ) ) } )

   aFields := aFldTemp

   aStruct := {}
   bFind := {| cFld | cFld == cField }
   AEval( dbStruct(), {| aFld | cField := aFld[ DBS_NAME ], ;
      iif( AScan( aFields, bFind ) == 0, NIL, AAdd( aStruct, aFld ) ) } )

   Select( cAlias )
   bFind := {| cFld | "->" $ cFld .AND. SubStr( cFld, At( "->", cFld ) + 2 ) == cField }
   AEval( dbStruct(), {| aFld | cField := aFld[ DBS_NAME ], ;
      iif( AScan( aFields, bFind ) == 0, NIL, AAdd( aStruct, aFld ) ) } )

   RETURN aStruct

STATIC FUNCTION __JoinList( nMaster, nDetail, nResult, aStruct )

   LOCAL aList := {}
   LOCAL nPos
   LOCAL i

   FOR i := 1 TO Len( aStruct )
      IF ( nPos := ( nMaster )->( FieldPos( aStruct[ i ][ DBS_NAME ] ) ) ) != 0
         AAdd( aList, { nResult, nMaster, nPos, i } )
      ELSEIF ( nPos := ( nDetail )->( FieldPos( aStruct[ i ][ DBS_NAME ] ) ) ) != 0
         AAdd( aList, { nResult, nDetail, nPos, i } )
      ENDIF
   NEXT

   RETURN aList

STATIC PROCEDURE __doJoinList( aList )

   LOCAL aJoin

   IF Len( aList ) > 0

      ( aList[ 1 ][ 1 ] )->( dbAppend() )

      FOR EACH aJoin IN aList
         ( aJoin[ 1 ] )->( FieldPut( aJoin[ 4 ], ( aJoin[ 2 ] )->( FieldGet( aJoin[ 3 ] ) ) ) )
      NEXT
   ENDIF

   RETURN
