/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DBTOTAL FUNCTION
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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
#include "error.ch"

/* NOTE: Compared to CA-Cl*pper, Harbour:
         - will accept character expressions for xKey, xFor and xWhile.
         - has three extra parameters (cRDD, nConnection, cCodePage).
         - will default to active index key for xKey parameter.
         - won't crash with "No exported method: EVAL" if xKey is not
           block and table is not indexed. */

FUNCTION __dbTotal( cFile, xKey, aFields,;
                    xFor, xWhile, nNext, nRec, lRest,;
                    cRDD, nConnection, cCodePage )

   LOCAL nOldArea
   LOCAL nNewArea

   LOCAL aNewDbStruct
   LOCAL aGetField
   LOCAL aFieldsSum
   LOCAL lDbTransRecord
   LOCAL xCurKey

   LOCAL bWhileBlock
   LOCAL bForBlock
   LOCAL bKeyBlock

   LOCAL oError
   LOCAL lError := .F.

   IF HB_ISSTRING( xWhile )
      bWhileBlock := hb_macroBlock( xWhile )
      lRest := .T.
   ELSEIF HB_ISBLOCK( xWhile )
      bWhileBlock := xWhile
      lRest := .T.
   ELSE
      bWhileBlock := {|| .T. }
   ENDIF

   IF HB_ISSTRING( xFor )
      bForBlock := hb_macroBlock( xFor )
   ELSEIF HB_ISBLOCK( xFor )
      bForBlock := xFor
   ELSE
      bForBlock := {|| .T. }
   ENDIF

   __defaultNIL( @lRest, .F. )

   IF nRec != NIL
      dbGoto( nRec )
      nNext := 1
   ELSE
      IF nNext == NIL
         nNext := -1
         IF ! lRest
            dbGoTop()
         ENDIF
      ELSE
         lRest := .T.
      ENDIF
   ENDIF

   nOldArea := Select()

   aNewDbStruct := {}
   AEval( dbStruct(), {| aField | iif( aField[ DBS_TYPE ] == "M", NIL, AAdd( aNewDbStruct, aField ) ) } )
   IF Empty( aNewDbStruct )
      RETURN .F.
   ENDIF

   BEGIN SEQUENCE

      IF Empty( xKey )
         xKey := ordKey()
      ENDIF

      IF HB_ISSTRING( xKey )
         bKeyBlock := hb_macroBlock( xKey )
      ELSEIF HB_ISBLOCK( xKey )
         bKeyBlock := xKey
      ELSE
         bKeyBlock := {|| .T. }
      ENDIF

      aGetField := {}
      AEval( aFields, {| cField | AAdd( aGetField, __GetField( cField ) ) } )
      aFieldsSum := Array( Len( aGetField ) )

      /* ; Keep it open after creating it. */
      dbCreate( cFile, aNewDbStruct, cRDD, .T., "", NIL, cCodePage, nConnection )
      nNewArea := Select()

      dbSelectArea( nOldArea )
      DO WHILE ! Eof() .AND. nNext != 0 .AND. Eval( bWhileBlock )

         lDbTransRecord := .F.

         AFill( aFieldsSum, 0 )

         xCurKey := Eval( bKeyBlock )

         DO WHILE ! Eof() .AND. nNext-- != 0 .AND. Eval( bWhileBlock ) .AND. ;
               xCurKey == Eval( bKeyBlock )

            IF Eval( bForBlock )
               IF ! lDbTransRecord
                  __dbTransRec( nNewArea, aNewDbStruct )
                  dbSelectArea( nOldArea )
                  lDbTransRecord := .T.
               ENDIF
               AEval( aGetField, {| bFieldBlock, nFieldPos | aFieldsSum[ nFieldPos ] += Eval( bFieldBlock ) } )
            ENDIF

            dbSkip()
         ENDDO

         IF lDbTransRecord
            dbSelectArea( nNewArea )
            AEval( aGetField, {| bFieldBlock, nFieldPos | Eval( bFieldBlock, aFieldsSum[ nFieldPos ] ) } )
            dbSelectArea( nOldArea )
         ENDIF

      ENDDO

   RECOVER USING oError
      lError := .T.
   END SEQUENCE

   IF nNewArea != NIL
      dbSelectArea( nNewArea )
      dbCloseArea()
   ENDIF

   dbSelectArea( nOldArea )

   IF lError
      Break( oError )
   ENDIF

   RETURN .T.

STATIC FUNCTION __GetField( cField )
   LOCAL nCurrArea := Select()
   LOCAL nPos
   LOCAL oError
   LOCAL lError

   /* ; Is the field aliased? */
   IF ( nPos := At( "->", cField ) ) > 0

      IF Select( Left( cField, nPos - 1 ) ) != nCurrArea

         oError := ErrorNew()
         oError:severity   := ES_ERROR
         oError:genCode    := EG_SYNTAX
         oError:subSystem  := "DBCMD"
         oError:canDefault := .T.
         oError:operation  := cField
         oError:subCode    := 1101

         lError := Eval( ErrorBlock(), oError )
         IF ! HB_ISLOGICAL( lError ) .OR. lError
            __ErrInHandler()
         ENDIF

         Break( oError )
      ENDIF

      cField := SubStr( cField, nPos + 2 )

   ENDIF

   RETURN FieldBlock( cField )

FUNCTION __dbTransRec( nDstArea, aFieldsStru )
   RETURN __dbTrans( nDstArea, aFieldsStru, NIL, NIL, 1 )
