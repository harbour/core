/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBEVAL() function
 *
 * Copyright 1999 Luiz Rafael Culik <Culik@sl.conex.net>
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

#include "common.ch"
#include "error.ch"

/* TODO: Optimize for speed (C rewrite?) */

/*  $DOC$
 *  $FUNCNAME$
 *      DBEVAL()
 *  $CATEGORY$
 *  $ONELINER$
 *      Performs a code block operation on the current data base
 *  $SYNTAX$
 *      DBEVAL( <bBlock>, [<bFor>], [<bWhile>], [<nNext>], [<nRecord>], [<lRest>] ) --> NIL
 *  $ARGUMENTS$
 *      <bBlock> Operation that is to be performed
 *      <bFor> Code block for the For condition
 *      <bWhile> Code block for the WHILE condition
 *      <nNext> Number of NEXT records  to process
 *      <nRecord> Record number to work on exactly
 *      <lRest> Toggle to rewind record pointer
 *  $RETURNS$
 *      NIL
 *  $DESCRIPTION$
 *      Performs a code block operation on the current data base
 *  $EXAMPLES$
 *      FUNCTION Main()
 *         LOCAL nCount
 *
 *         USE Test
 *
 *         dbGoto( 4 )
 *         ? RecNo()
 *         COUNT TO nCount
 *         ? RecNo(), nCount
 *         COUNT TO nCount NEXT 10
 *         ? RecNo(), nCount
 *
 *         RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      S
 *  $COMPLIANCE$
 *      DBEVAL is fully CA-Clipper compliant.
 *  $SEEALSO$
 *  $END$
 */

FUNCTION dbEval( bBlock, bFor, bWhile, nNext, nRecord, lRest )
   LOCAL oError
   LOCAL nCounter

   IF !Used()
      oError := ErrorNew()
      oError:severity := ES_ERROR
      oError:SubSystem := "DBCMD"
      oError:genCode := EG_ARG
      oError:subCode := 2019
      oError:operation := "DBEVAL"
      oError:canDefault := .T.
      Eval( ErrorBlock(), oError )
   ENDIF

   IF Used()

      IF !ISBLOCK( bBlock )
         oError := ErrorNew()
         oError:severity := ES_ERROR
         oError:SubSystem := "DBCMD"
         oError:genCode := EG_ARG
         oError:subCode := 2019
         oError:operation := "DBEVAL"
         Eval( ErrorBlock(), oError )
         RETURN NIL
      ENDIF

      IF ISNUMBER( nRecord )

         IF nRecord >= 1 .AND. nRecord <= LastRec()
            dbGoto( nRecord )
            IF bFor == NIL .OR. Eval( bFor )
               Eval( bBlock )
            ENDIF
         ENDIF

      ELSE

         nCounter := 0

         IF lRest == NIL .OR. !lRest
            dbGoTop()
         ENDIF

         DO CASE
         CASE bFor != NIL .AND. bWhile == NIL

            WHILE !Eof()
               IF nNext == NIL .OR. ++nCounter <= nNext
                  IF Eval( bFor )
                     Eval( bBlock )
                  ENDIF
               ENDIF
               dbSkip()
            ENDDO

         CASE bFor == NIL .AND. bWhile != NIL

            WHILE !Eof() .AND. Eval( bWhile )
               IF nNext == NIL .OR. ++nCounter <= nNext
                  Eval( bBlock )
               ENDIF
               dbSkip()
            ENDDO

         CASE bFor != NIL .AND. bWhile != NIL

            WHILE !Eof() .AND. Eval( bWhile )
               IF nNext == NIL .OR. ++nCounter <= nNext
                  IF Eval( bFor )
                     Eval( bBlock )
                  ENDIF
               ENDIF
               dbSkip()
            ENDDO

         OTHERWISE

            WHILE !Eof()
               IF nNext == NIL .OR. ++nCounter <= nNext
                  Eval( bBlock )
               ENDIF
               dbSkip()
            ENDDO

         ENDCASE
      ENDIF
   ENDIF

   RETURN NIL

