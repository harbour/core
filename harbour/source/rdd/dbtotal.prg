/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DBTOTAL FUNCTION
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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


#include "set.ch"
#include "error.ch"

FUNCTION __DBTOTAL( cFile, xKey, aFields, ;
                    xFor, xWhile, nNext, nRec, lRest, rdd, ;
                    nConnection, cdpId )

   LOCAL CurSelect
   LOCAL NewSelect
   LOCAL aNewDbStruct
   LOCAL aGetField
   LOCAL aFieldsSum
   LOCAL lDbTransRecord
   LOCAL xCurKey
   LOCAL bKeyBlock
   LOCAL bForBlock
   LOCAL bWhileBlock
   LOCAL cset
   LOCAL flag_err
   LOCAL err_block
   LOCAL wRec
   LOCAL err

   err_block := Errorblock( { | x | Break( x ) } )
   flag_err  := .F.
   cset      := Set( _SET_CANCEL, .f. )

   IF ( Valtype( xWhile ) == "C" )
      bWhileBlock := "{||" + xWhile + "}"
      bWhileBlock := &bWhileBlock
   ELSEIF ( Valtype( xWhile ) != "B" )
      bWhileBlock := { || .t. }
   ELSE
      bWhileBlock := xWhile
      lRest    := .T.
   ENDIF

   IF ( Valtype( xFor ) == "C" )
      bForBlock := "{||" + xFor + "}"
      bForBlock := &xFor
   ELSEIF ( Valtype( xFor ) != "B" )
      bForBlock := { || .t. }
   ELSE
      bForBlock := xFor
   ENDIF

   IF ( lRest == NIL )
      lRest := .F.
   ENDIF
   IF ( nRec != NIL )
      GOTO nRec
      nNext := 1
   ELSE
      IF ( nNext == NIL )
         nNext := - 1
         IF ( !lRest )
            GOTO TOP
         ENDIF
      ELSE
         lRest := .T.
      ENDIF
   ENDIF

   CurSelect    := SELECT()
   aNewDbStruct := {}
   Aeval( Dbstruct(), { | _1 | IIF( _1[ 2 ] == "M", NIL, Aadd( aNewDbStruct, _1 ) ) } )
   IF ( Empty( aNewDbStruct ) )
      RETURN .F.
   ENDIF

   BEGIN SEQUENCE

      IF ( Empty( xKey ) )
         xKey := Indexkey()
      ENDIF

      IF ( Empty( xKey ) )
         Set( _SET_CANCEL, cset )
         err             := errorNew()
         err:description := "invalid argument"
         err:genCode     := EG_ARG
         Eval( Errorblock(), err )
      ENDIF

      IF ( Valtype( xKey ) == "C" )
         bKeyBlock := "{||" + xKey + "}"
         bKeyBlock := &bKeyBlock
      ELSEIF ( Valtype( xKey ) != "B" )
         bKeyBlock := { || .t. }
      ELSE
         bKeyBlock := xKey
      ENDIF

      aGetField := {}
      Aeval( aFields, { | _1 | Aadd( aGetField, getfield( _1 ) ) } )
      aFieldsSum := Array( Len( aGetField ) )

      dbCreate( cFile, aNewDbStruct, rdd, .T., "", , cdpId, nConnection )
      NewSelect := SELECT()
      SELECT( CurSelect )

      WHILE ( !Eof() .and. nNext != 0 .and. Eval( bWhileBlock ) )
         lDbTransRecord := .F.
         Afill( aFieldsSum, 0 )
         xCurKey := Eval( bKeyBlock )
         WHILE ( !Eof() .and. nNext -- != 0 .and. Eval( bWhileBlock ) .and. ;
                 xCurKey == Eval( bKeyBlock ) )
            IF ( Eval( bForBlock ) )
               IF ( !lDbTransRecord )
                  __dbTransRec( NewSelect, aNewDbStruct )
                  SELECT( CurSelect )
                  lDbTransRecord := .T.
               ENDIF
               Aeval( aGetField, { | _1, _2 | ;
                        aFieldsSum[ _2 ] := aFieldsSum[ _2 ] + Eval( _1 ) } )
            ENDIF

            SKIP
         ENDDO

         IF ( lDbTransRecord )
            SELECT( NewSelect )
            Aeval( aGetField, { | _1, _2 | Eval( _1, aFieldsSum[ _2 ] ) } )
            SELECT( CurSelect )
         ENDIF

      ENDDO

   RECOVER USING err
      flag_err := .t.

   ENDSEQUENCE

   IF ( NewSelect != NIL )
      SELECT( NewSelect )
      CLOSE
   ENDIF

   SELECT( CurSelect )
   Set( _SET_CANCEL, cset )
   Errorblock( err_block )

   IF ( flag_err )
      IF ( ValType( err:operation ) == "C" )
         err:operation += "/__DBTOTAL"
      ELSE
         err:operation := "__DBTOTAL"
      ENDIF
      Eval( Errorblock(), err )
   ENDIF

RETURN .T.

STATIC FUNCTION GETFIELD( cField )

    LOCAL nPos
    LOCAL SelectFromField
    LOCAL CurSelect
    LOCAL err
    LOCAL lErr
    CurSelect := SELECT()

    IF ( ( nPos := At( "->", cField ) ) > 0 )
        SelectFromField := Left( cField, nPos - 1 )
        IF ( Select( SelectFromField ) != CurSelect )
            err           := ErrorNew()
            err:severity  := ES_ERROR
            err:gencode   := EG_SYNTAX
            err:subsystem := "DBCMD"
            err:candefaul := .T.
            err:operation := cField
            err:subcode   := 1101
            lErr := Eval( Errorblock(), err )
            IF ( Valtype( lErr ) != "L" .or. lErr )
               __errInHandler()
            ENDIF

            BREAK( err )
        ENDIF

        cField := Substr( cField, nPos + 2 )
    ENDIF

RETURN Fieldblock( cField )

FUNCTION __dbTransRec( nDstArea, aFieldsStru )
   Return __dbTrans( nDstArea, aFieldsStru, , , 1 )
