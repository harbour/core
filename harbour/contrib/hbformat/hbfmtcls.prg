/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour source code formatter
 *
 * Copyright 2009 Alexander S.Kresin <alex@belacy.belgorod.su>
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

#include "hbclass.ch"

#include "fileio.ch"

#define RF_STATE_FUNC   1
#define RF_STATE_VAR    2
#define RF_STATE_CODE   3
#define RF_STATE_RET    4

#define LEFTEQUAL( l, r )       ( Left( l, Len( r ) ) == r )

// TOFIX:
//   1. 'var ++'
//   2. '- 1' for numeric literals.
//   3. wrongly breaks line: 'FUNCTION Hello( /* comment */ )'
//   4. INIT PROCEDURE/EXIT PROCEDURE are not recognized as PROCEDURE statement
//   5. ".T.," / ".F.," gets wrongly corrected to ".T. ," / ".F. ,"
//   6. "end class" is converted to "end CLASS" instead of "END CLASS"
//   7. in PP commands "<var>" should not be converted to "< var >"
//   8. Certain lines outside any procs f.e. at beginning of file, sometimes
//      will be indented to one level:
//      ---
//      /* a1 */
//      REQUEST a1
//      ANNOUNCE a3
//      STATIC a4
//      THREAD STATIC a5
//      ---

CREATE CLASS HBFORMATCODE

   VAR cEol
   VAR nLineErr, nErr, cLineErr

   VAR nEol           INIT  0       // Eol: -1 - no change, 0 - OS default, 1 - DOS, 2 - UNIX
   VAR lFCaseLow      INIT .F.      // If true, convert file name to lower case
   VAR lNoTabs        INIT .T.      // If true, converts all tabs to spaces
   VAR lIndent        INIT .T.      // If true, indent code
   VAR lCase          INIT .T.      // If true, make case conversion
   VAR lSpaces        INIT .T.      // If true, reformat spaces
   VAR lIndFunc       INIT .F.      // If true, indent "Function", "Procedure", "Class", "Method"
   VAR lIndVar        INIT .T.      // If true, indent "Local", "Private", etc. in a function beginning
   VAR lIndDrt        INIT .F.      // If true, indent  directives
   VAR lIndRet        INIT .T.      // If true, indent  "Return"
   VAR nIndLeft       INIT   3      // Leftmost indent - amount of spaces
   VAR nIndNext       INIT   3      // indent - amount of spaces
   VAR nIndCont       INIT   3      // Indent for continuation ( after ';' ) lines - amount of spaces
   VAR lCnvAst        INIT .T.      // If true, convert asterisk '*' to '//'
   VAR lCnvAmp        INIT .T.      // If true, convert '&&' to '//'
   VAR lCnvNot        INIT .T.      // If true, convert .NOT. TO !
   VAR nCaseCmd       INIT   1      // Case of commands ( -1 - no change, 1 - upper, 2 - lower, 3 - title )
   VAR nCaseBoo       INIT   1      // Case of boolean operators ( -1 - no change, 1 - upper, 2 - lower, 3 - title )
   VAR nCaseFnc       INIT   4      // Case of functions ( -1 - no change, 1 - upper, 2 - lower, 3 - title, 4 - as in pattern )
   VAR nCaseUnk       INIT  -1      // Case of functions ( -1 - no change, 1 - upper, 2 - lower, 3 - title )
   VAR nCaseDrt       INIT   2      // Case of directives ( -1 - no change, 1 - upper, 2 - lower, 3 - title )
   VAR nSpaceDrt      INIT   0      // Number of spaces after # in directives ( -1 - no change )
   VAR nLineFnc       INIT   1      // -1 - no change, 1 - insert empty line before a function ( procedure, class ) declaration, 2 - remove it
   VAR nLineRet       INIT   1      // -1 - no change, 1 - insert empty line before return, 2 - remove it
   VAR nLineVar       INIT   1      // -1 - no change, 1 - insert empty line before variables declaration, 2 - remove it
   VAR nLineCode      INIT   1      // -1 - no change, 1 - insert empty line before code in function, 2 - remove it
   VAR nBr4Comma      INIT   1      // Max level of nesting in brackets, while space is added after a comma
   VAR nBr4Brac       INIT   2      // Max level of nesting in brackets, while space is added after/before a bracket
   VAR cHBXList       INIT ""

   VAR cExtSave       INIT ""       // Extension for a formatted file ( "" - replace original )
   VAR cExtBack       INIT ".bak"   // Extension for a backup file

   VAR cCommands      INIT ","
   VAR cClauses       INIT ","
   VAR cFunctions     INIT ","
   VAR aContr         INIT { { "if"    , ""        , "|else|elseif|"   , "endif"     },;
                             { "do"    , "while"   , ""                , "enddo"     },;
                             { "while" , ""        , ""                , "enddo"     },;
                             { "for"   , ""        , ""                , "next"      },;
                             { "do"    , "case"    , "|case|otherwise|", "endcase"   },;
                             { "with"  , "object"  , ""                , "end"       },;
                             { "begin" , "sequence", "|recover|always|", "end"       },;
                             { "switch", ""        , "|case|otherwise|", "endswitch" } }

   VAR bCallback

   METHOD New( aParams, cIniName )
   METHOD SetOption( cLine, i, aIni )
   METHOD ReadIni( cIniName )
   METHOD Reformat( aFile )
   METHOD FormatLine( cLine, lContinued )
   METHOD ConvertCmd( cLine, nBegin, nEnd, lFirstOnly )
   METHOD ConvertFnc( cLine, nBegin, nEnd )
   METHOD ConvertBool( cLine, nBegin, nEnd )
   METHOD File2Array( cFileName )
   METHOD Array2File( cFileName, aFile )

ENDCLASS

METHOD New( aParams, cIniName ) CLASS HBFORMATCODE

   LOCAL cParam, cHBX

   ::nErr := 0

   IF HB_ISSTRING( cIniName )
      IF ! ::ReadIni( cIniName )
         RETURN Self
      ENDIF
      FOR EACH cParam IN aParams
         IF Left( cParam, 1 ) == "@"
            IF ! ::ReadIni( SubStr( cParam, 2 ) )
               RETURN Self
            ENDIF
         ELSEIF Left( cParam, 1 ) $ "-/"
            IF ! ::SetOption( SubStr( cParam, 2 ), 0 )
               RETURN Self
            ENDIF
         ENDIF
      NEXT
   ENDIF

   IF !( Right( ::cCommands, 1 ) == "," )
      ::cCommands += ","
   ENDIF
   ::cCommands += "IF,ELSEIF,ELSE,ENDIF,END,DO,WHILE,ENDDO,WITH,CASE,OTHERWISE,ENDCASE,BEGIN," +;
                  "FUNCTION,PROCEDURE,RETURN,CLASS,ENDCLASS,METHOD,DATA,LOCAL,PRIVATE,PUBLIC,STATIC,FIELD,MEMVAR,PARAMETERS,DECLARE," +;
                  "ACCEPT,APPEND,AVERAGE,CLEAR,CLOSE,COMMIT,CONTINUE,COPY,COUNT,CREATE,DEFAULT," +;
                  "DELETE,DISPLAY,EJECT,ERASE,EXIT,FOR,GO,GOTO,INDEX,INPUT,JOIN,KEYBOARD,LABEL,LIST,LOCATE," +;
                  "LOOP,MENU,NEXT,PACK,PRINT,QUIT,READ,RECALL,REINDEX,RELEASE,RENAME,REQUEST,REPLACE,RESTORE," +;
                  "RUN,SAVE,SEEK,SELECT,SET,SKIP,SORT,STORE,SUM,TEXT,TOTAL,UNLOCK,USE,WAIT,ZAP,"

   IF !( Right( ::cClauses, 1 ) == "," )
      ::cClauses += ","
   ENDIF
   ::cClauses += "ADDITIVE,ALIAS,ALL,BLANK,BOTTOM,BOX,COLOR,DATE,DELETED,EACH,EXTENDED,EXCLUSIVE,FROM,GET," +;
                 "RANGE,READONLY,REST,SAY,SCREEN,ALTERNATE,BELL,CENTURY,CONFIRM,CONSOLE,CURSOR,DECIMALS,DELIMITERS,DEVICE,EPOCH,ESCAPE," +;
                 "EXACT,EXCLUSIVE,FILTER,FIXED,FORMAT,INTENSITY,KEY,LIKE,MARGIN,MESSAGE,NEW,OFF,ON,ORDER,PATH,PICTURE,PRINTER,PROMPT," +;
                 "PROTECTED,RELATION,SCOREBOARD,SEQUENCE,SOFTSEEK,STEP,STRUCTURE,TYPEAHEAD,UNIQUE,WRAP,TAG,TO,TOP,VALID,WHEN,"

   IF !( Right( ::cFunctions, 1 ) == "," )
      ::cFunctions += ","
   ENDIF
   IF !( ",STR," $ Upper( ::cFunctions ) )
      ::cFunctions += "iif,ISNIL,ISARRAY,ISBLOCK,ISCHARACTER,ISDATE,ISLOGICAL,ISMEMO,ISNUMBER,ISOBJECT,Main"
      HBXToFuncList( @::cFunctions, __harbour_hbx() )
      FOR EACH cHBX IN hb_ATokens( ::cHBXList )
         HBXToFuncList( @::cFunctions, hb_MemoRead( hb_PathJoin( hb_DirBase(), cHBX ) ) )
      NEXT
   ENDIF

   IF ::nEol == 2
      ::cEol := Chr( 10 )
   ELSEIF ::nEol == 1
      ::cEol := Chr( 13 ) + Chr( 10 )
   ELSEIF ::nEol == 0
      ::cEol := hb_eol()
   ENDIF
   IF ::lIndent
      ::lNoTabs := .T.
   ENDIF

   RETURN Self

STATIC PROCEDURE HBXToFuncList( /* @ */ cFuncList, cHBX )
   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( StrTran( cHBX, Chr( 13 ) ), Chr( 10 ) )
      IF Left( cLine, Len( "DYNAMIC " ) ) == "DYNAMIC "
         cFuncList += "," + SubStr( cLine, Len( "DYNAMIC " ) + 1 )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION __harbour_hbx()

#pragma __streaminclude "harbour.hbx" | RETURN %s

METHOD Reformat( aFile ) CLASS HBFORMATCODE

   LOCAL i, iDelta := 0, nLen := Len( aFile ), cToken1, cToken2, nLenToken, nPos
   LOCAL nPosSep, cLine, cLineAll, nLineSegment
   LOCAL nContrState, nIndent, nDeep := 0, aDeep := {}
   LOCAL lPragmaDump := .F., lClass := .F., lComment := .F., nPosComment, lContinue := .F.
   LOCAL nStatePrev, nState := 0

   ::nErr := 0
   FOR i := 1 TO nLen
      IF aFile[ i ] == NIL
         EXIT
      ENDIF
      IF Empty( aFile[ i ] )
         LOOP
      ENDIF
      IF ::bCallBack != NIL
         Eval( ::bCallBack, aFile, i )
      ENDIF
      nPosComment := 0
      IF ::lIndent
         aFile[ i ] := StrTran( RTrim( aFile[ i ] ), Chr( 9 ), " " )
      ELSE
         aFile[ i ] := RTrim( aFile[ i ] )
      ENDIF

      IF Empty( aFile[ i ] )
         aFile[ i ] := ""
         LOOP
      ENDIF
      IF lComment
         IF ( nPos := FindNotQuoted( "*/", aFile[ i ] ) ) != 0
            lComment := .F.
            IF ! Empty( cToken1 := SubStr( aFile[ i ], nPos + 2 ) )
               aFile[ i ] := Left( aFile[ i ], nPos + 1 )
               nLen := rf_AINS( aFile, i + 1, cToken1 )
               iDelta++
            ENDIF
         ENDIF
      ELSE
         cLineAll := LTrim( aFile[ i ] )
         IF Left( cLineAll, 1 ) == "#"
            cToken1 := Lower( hb_tokenGet( cLineAll, 1 ) )
            cToken2 := Lower( hb_tokenGet( cLineAll, 2 ) )
            IF Len( cToken1 ) == 1
               cToken1 += cToken2
               cToken2 := Lower( hb_tokenGet( cLineAll, 3 ) )
            ENDIF
            IF cToken1 == "#pragma"
               IF cToken2 == "begindump"
                  lPragmaDump := .T.
               ELSEIF cToken2 == "enddump"
                  lPragmaDump := .F.
               ENDIF
            ENDIF
         ENDIF
         IF Left( cLineAll, 1 ) == "*"
            nPosComment := 1
            IF ::lCnvAst
               cLineAll := "//" + SubStr( cLineAll, 2 )
            ENDIF
         ELSEIF ( nPos := FindNotQuoted( "//", cLineAll ) ) != 0
            nPosComment := nPos
         ELSEIF ( nPos := FindNotQuoted( "&&", cLineAll ) ) != 0
            nPosComment := nPos
            IF ::lCnvAmp
               cLineAll := Left( cLineAll, nPos - 1 ) + "//" + SubStr( cLineAll, nPos + 2 )
            ENDIF
         ENDIF
         IF ( nPos := FindNotQuoted( "/*", cLineAll ) ) != 0 .AND. ( nPosComment == 0 .OR. nPosComment > nPos )
            nPosComment := nPos
            IF ( nPos := FindNotQuoted( "*/", cLineAll, nPos + 2 ) ) != 0
               IF ! Empty( cToken1 := SubStr( cLineAll, nPos + 2 ) )
                  cLineAll := Left( cLineAll, nPos + 1 )
                  nLen := rf_AINS( aFile, i + 1, cToken1 )
                  iDelta++
               ENDIF
            ELSE
               lComment := .T.
            ENDIF
         ENDIF
         IF ! lPragmaDump .AND. ::lIndent .AND. ( ! lComment .OR. nPosComment > 1 )
            aFile[ i ] := cLineAll
            IF ! lContinue
               nPosSep := 1
               nLineSegment := 1
               DO WHILE .T.
                  nPos := nPosSep
                  IF !( Left( aFile[ i ], 1 ) == "#" ) .AND. ;
                        ( nPosSep := FindNotQuoted( ";", aFile[ i ], nPosSep ) ) != 0 .AND. ;
                        nPosSep < Len( aFile[ i ] ) .AND. ( nPosComment == 0 .OR. nPosSep < nPosComment )
                     cLine := SubStr( aFile[ i ], nPos, nPosSep - nPos + 1 )
                  ELSE
                     nPosSep := 0
                     cLine := SubStr( aFile[ i ], nPos, Len( aFile[ i ] ) - nPos + 1 )
                  ENDIF

                  nContrState := 0
                  nStatePrev := nState
                  cToken1 := Lower( hb_tokenGet( cLine, 1 ) )
                  nLenToken := Len( cToken1 )
                  nPos := 2
                  DO WHILE nPos <= nLenToken .AND. SubStr( cToken1, nPos, 1 ) >= "_"
                     nPos++
                  ENDDO
                  IF nPos <= nLenToken
                     nLenToken := nPos - 1
                     cToken1 := Left( cToken1, nLenToken )
                  ENDIF
                  cToken2 := Lower( hb_tokenGet( cLine, 2 ) )
                  IF Left( cToken1, 1 ) == "#"
                  ELSEIF nLenToken >= 4 .AND. ( ( LEFTEQUAL( "static", cToken1 ) .AND. ;
                        ( LEFTEQUAL( "function", cToken2 ) .OR. LEFTEQUAL( "procedure", cToken2 ) ) ) .OR. ;
                        LEFTEQUAL( "function", cToken1 ) .OR. LEFTEQUAL( "procedure", cToken1 ) ;
                        .OR. ( "method" == cToken1 .AND. ! lClass ) .OR. ;
                        ( "class" == cToken1 .AND. ! lClass ) .OR. ;
                        ( "create" == cToken1 .AND. "class" == cToken2 .AND. ! lClass ) )
                     IF nDeep == 0
                        nState := RF_STATE_FUNC
                        IF "class" == cToken1 .OR. ( "create" == cToken1 .AND. "class" == cToken2 )
                           lClass := .T.
                        ENDIF
                     ELSE
                        ::nLineErr := i - iDelta
                        ::nErr := 1
                        ::cLineErr := cLine
                        RETURN .F.
                     ENDIF
                  ELSEIF nLenToken >= 4 .AND. ( ;
                        LEFTEQUAL( "local", cToken1 ) .OR. ;
                        LEFTEQUAL( "private", cToken1 ) .OR. ;
                        LEFTEQUAL( "public", cToken1 ) .OR. ;
                        LEFTEQUAL( "field", cToken1 ) .OR. ;
                        LEFTEQUAL( "static", cToken1 ) .OR. ;
                        LEFTEQUAL( "memvar", cToken1 ) .OR. ;
                        LEFTEQUAL( "parameters", cToken1 ) .OR. ;
                        LEFTEQUAL( "declare", cToken1 ) )
                     IF nStatePrev == RF_STATE_FUNC
                        nState := RF_STATE_VAR
                     ENDIF
                  ELSEIF cToken1 == "return" .OR. cToken1 == "endclass" .OR. ;
                        ( cToken1 == "end" .AND. cToken2 == "class" )
                     IF nDeep == 0
                        nState := RF_STATE_RET
                     ENDIF
                  ELSE
                     IF !( cToken1 == "request" )
                        nState := RF_STATE_CODE
                     ENDIF
                     IF ( nContrState := Ascan( ::aContr, ;
                           {| a | a[ 1 ] == cToken1 .AND. ( Empty( a[ 2 ] ) .OR. a[ 2 ] == cToken2 ) } ) ) != 0
                        IF Len( aDeep ) < ++nDeep
                           AAdd( aDeep, NIL )
                        ENDIF
                        aDeep[ nDeep ] := nContrState
                     ELSEIF Len( cToken1 ) < 4 .OR. ( nContrState := AScan( ::aContr, {| a | "|" + cToken1 + "|" $ a[ 3 ] } ) ) == 0
                        IF ( nPos := AScan( ::aContr, {| a | a[ 4 ] == cToken1 } ) ) > 0 .OR. ;
                              cToken1 == "end"
                           IF nPos != 0 .AND. nDeep > 0 .AND. aDeep[ nDeep ] != nPos
                              DO WHILE ( nPos := Ascan( ::aContr, {| a | a[ 4 ] == cToken1 }, ;
                                    nPos + 1 ) ) != 0 .AND. aDeep[ nDeep ] != nPos
                              ENDDO
                           ENDIF
                           IF nDeep > 0 .AND. ( aDeep[ nDeep ] == nPos .OR. cToken1 == "end" )
                              nDeep--
                           ELSE
                              ::nLineErr := i - iDelta
                              ::nErr := 3
                              ::cLineErr := cLine
                              RETURN .F.
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
                  IF nLineSegment == 1
                     IF nState == 0
                        nIndent := 0
                     ELSEIF nState == RF_STATE_FUNC
                        nIndent := iif( ::lIndFunc, ::nIndLeft, 0 )
                     ELSEIF nState == RF_STATE_VAR
                        nIndent := iif( ::lIndVar, ::nIndLeft, 0 )
                     ELSEIF nState == RF_STATE_RET
                        nIndent := iif( ! lClass .AND. ::lIndRet, ::nIndLeft, 0 )
                     ELSE
                        nIndent := ::nIndLeft + ::nIndNext * iif( nContrState == 0, nDeep, nDeep - 1 )
                     ENDIF
                     IF Left( cLine, 1 ) == "#" .AND. ! ::lIndDrt
                        nIndent := 0
                     ENDIF
                     IF Left( cLine, 2 ) == "//" .AND. nDeep == 0
                        nIndent := 0
                     ENDIF
                     cLineAll := Space( nIndent ) + ::FormatLine( cLine )

                     IF i > 1 .AND. ( ( nState == RF_STATE_RET .AND. ::nLineRet > 0 ) .OR. ;
                           ( nState == RF_STATE_FUNC .AND. ::nLineFnc > 0 ) .OR. ;
                           ( nState == RF_STATE_VAR .AND. nStatePrev != nState .AND. ::nLineVar > 0 ) .OR. ;
                           ( nState == RF_STATE_CODE .AND. nStatePrev != nState .AND. ::nLineCode > 0 ) )
                        nPos := i - 1
                        IF ( nState == RF_STATE_RET  .AND. ::nLineRet == 1 ) .OR. ;
                           ( nState == RF_STATE_FUNC .AND. ::nLineFnc == 1 ) .OR. ;
                           ( nState == RF_STATE_VAR  .AND. ::nLineVar == 1 ) .OR. ;
                           ( nState == RF_STATE_CODE .AND. ::nLineCode == 1 )
                           IF ! Empty( aFile[ nPos ] )
                              nLen := rf_AINS( aFile, nPos + 1, "" )
                              iDelta++
                              i++
                           ELSE
                              nPos--
                           ENDIF
                        ENDIF
                        DO WHILE nPos > 1 .AND. Empty( aFile[ nPos ] )
                           rf_ADEL( aFile, nPos )
                           iDelta--
                           i--
                           nPos--
                        ENDDO
                     ENDIF
                  ELSE
                     cLineAll += ::FormatLine( cLine )
                  ENDIF
                  IF lClass .AND. nState == RF_STATE_RET
                     lClass := .F.
                  ENDIF
                  IF nPosSep == 0 .OR. nPosSep == Len( aFile[ i ] )
                     EXIT
                  ENDIF
                  nPosSep++
                  nLineSegment++
               ENDDO
               aFile[ i ] := cLineAll
            ELSE
               // This line is a continuation of previous
               aFile[ i ] := Space( ::nIndLeft + ::nIndNext * nDeep + ::nIndCont ) + ::FormatLine( aFile[ i ], .T. )
            ENDIF
            IF ( nPosComment > 0 .AND. Right( RTrim( Left( aFile[ i ], nPosComment - 1 ) ), 1 ) == ';' ) ;
                  .OR. ( nPosComment == 0 .AND. Right( aFile[ i ], 1 ) == ';' )
               lContinue := .T.
            ELSE
               lContinue := .F.
            ENDIF
         ELSEIF ! lPragmaDump
            aFile[ i ] := ::FormatLine( aFile[ i ] )
         ENDIF
      ENDIF
   NEXT

   RETURN .T.

#define FL_STATE_DIGIT   1
#define FL_STATE_ANY     2
#define FL_STATE_OP      3
#define FL_STATE_STRING  4
#define FL_STATE_QUOTED 11
#define FL_STATE_SQBR   12

METHOD FormatLine( cLine, lContinued ) CLASS HBFORMATCODE

   LOCAL i, nLen, c, nState := 0, cSymb, cToken, nPos := 1
   LOCAL lFirst, nBegin, nEnd, nB := 0, nA := 0, aBrackets[ 2 ]
   LOCAL cOperators := "+-*/%#=~^<>$!"

   IF ! ::lCase .AND. ! ::lSpaces
      RETURN cLine
   ENDIF

   hb_default( @lContinued, .F. )
   lFirst := ! lContinued

   nLen := Len( cLine )

   DO WHILE SubStr( cLine, nPos, 1 ) == " "
      nPos++
   ENDDO

   IF ! lContinued .AND. Left( cLine, 1 ) == "#"
      IF ::lSpaces .AND. ::nSpaceDrt != -1
         cLine := Left( cLine, nPos ) + Space( ::nSpaceDrt ) + LTrim( SubStr( cLine, nPos + 1 ) )
      ENDIF
      nLen := Len( cLine )
      IF ::lCase .AND. ::nCaseDrt != -1
         nPos++
         DO WHILE SubStr( cLine, nPos, 1 ) == " "
            nPos++
         ENDDO
         i := nPos
         DO WHILE nPos <= nLen .AND. SubStr( cLine, nPos, 1 ) >= "A"
            nPos++
         ENDDO
         IF SubStr( cLine, nPos, 1 ) >= "A"
            nPos++
         ENDIF
         cToken := SubStr( cLine, i, nPos - i )
         cToken := iif( ::nCaseDrt == 1, Upper( cToken ), iif( ::nCaseDrt == 2, Lower( cToken ), Upper( Left( cToken, 1 ) ) + Lower( SubStr( cToken, 2 ) ) ) )
         cLine := Left( cLine, i - 1 ) + cToken + iif( nPos > nLen, "", SubStr( cLine, nPos ) )
      ENDIF
   ELSE
      aBrackets[ 1 ] := aBrackets[ 2 ] := 0
      FOR i := nPos TO nLen
         c := SubStr( cLine, i, 1 )
         IF nState <= FL_STATE_STRING
            IF ( c >= "0" .AND. c <= "9" ) .OR. ( c >= "A" .AND. c <= "Z" ) ;
                  .OR. ( c >= "a" .AND. c <= "z" ) .OR. c == "_"
               IF nState < FL_STATE_STRING .OR. ( nState == FL_STATE_STRING .AND. nEnd > nBegin )
                  IF nState == FL_STATE_STRING
                     ::ConvertCmd( @cLine, nBegin, nEnd )
                  ENDIF
                  IF c >= "A"
                     nState := FL_STATE_STRING
                     nBegin := nEnd := i
                  ELSE
                     nState := FL_STATE_DIGIT
                  ENDIF
               ENDIF
            ELSEIF c == '"' .OR. c == "'"
               IF nState == FL_STATE_STRING
                  IF nEnd == nBegin
                     nEnd := i
                  ENDIF
                  ::ConvertCmd( @cLine, nBegin, nEnd )
               ENDIF
               cSymb := c
               nState := FL_STATE_QUOTED
            ELSEIF c == "["
               nState := FL_STATE_SQBR
            ELSEIF c == "/" .AND. ( ( c := SubStr( cLine, i + 1, 1 ) ) == "/" .OR. c == "*" )
               IF nState == FL_STATE_STRING
                  IF nEnd == nBegin
                     nEnd := i
                  ENDIF
                  ::ConvertCmd( @cLine, nBegin, nEnd )
               ENDIF
               EXIT
            ELSEIF c == " "
               nEnd := i
               i++
               DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                  i++
               ENDDO
               i--
            ELSEIF c == "(" .OR. c == "{"
               aBrackets[ iif( c == "(", 1, 2 ) ]++
               IF nState == FL_STATE_STRING
                  IF nEnd == nBegin
                     nEnd := i
                  ENDIF
                  IF ( ! lFirst .OR. ! ::ConvertCmd( @cLine, nBegin, nEnd, .T. ) ) .AND. c == "("
                     ::ConvertFnc( @cLine, nBegin, nEnd )
                  ENDIF
               ENDIF
               IF ::lSpaces .AND. aBrackets[ iif( c == "(", 1, 2 ) ] <= ::nBr4Brac .AND. ;
                     i < nLen .AND. !( SubStr( cLine, i + 1, 1 ) $ iif( c == "(", " )", " |} " ) )
                  nA := i
               ENDIF
               nState := FL_STATE_ANY
            ELSEIF c == "."
               IF nState == FL_STATE_STRING
                  IF nBegin > 1 .AND. SubStr( cLine, nBegin - 1, 1 ) == "." .AND. nEnd == nBegin
                     ::ConvertBool( @cLine, nBegin, i )
                     IF Len( cLine ) != nLen
                        /*  If .not. was converted to ! */
                        i -= ( nLen - Len( cLine ) )
                        nLen := Len( cLine )
                     ELSE
                        nB := nBegin - 1
                        nA := i
                     ENDIF
                  ELSE
                     IF nEnd == nBegin
                        nEnd := i
                     ENDIF
                     ::ConvertCmd( @cLine, nBegin, nEnd )
                  ENDIF
               ENDIF
               nState := FL_STATE_ANY
            ELSEIF c == ","
               IF aBrackets[ 1 ] <= ::nBr4Comma .AND. aBrackets[ 2 ] <= ::nBr4Comma
                  nA := i
               ENDIF
               nState := FL_STATE_ANY
            ELSEIF c == "!" .AND. !( SubStr( cLine, i + 1, 1 ) == "=" )
               IF nState == FL_STATE_STRING
                  IF nEnd == nBegin
                     nEnd := i
                  ENDIF
                  ::ConvertCmd( @cLine, nBegin, nEnd )
               ENDIF
               nState := iif( SubStr( cLine, i + 1, 1 ) == "=", FL_STATE_OP, FL_STATE_ANY )
            ELSEIF c $ cOperators .OR. ( c == ":" .AND. SubStr( cLine, i + 1, 1 ) == "=" )
               nB := i
               IF SubStr( cLine, i + 1, 1 ) $ cOperators
                  i++
               ENDIF
               nA := i
               nState := FL_STATE_OP
            ELSEIF c == ")" .OR. c == "}"
               IF ::lSpaces .AND. aBrackets[ iif( c == "(", 1, 2 ) ] <= ::nBr4Brac .AND. ;
                     i > 1 .AND. !( SubStr( cLine, i - 1, 1 ) $ " ({" )
                  nB := i
               ENDIF
               aBrackets[ iif( c == ")", 1, 2 ) ]--
               nState := FL_STATE_ANY
            ELSE
               nState := FL_STATE_ANY
            ENDIF
            IF lFirst .AND. nState != FL_STATE_STRING
               lFirst := .F.
            ENDIF
            IF !( "|" + SubStr( cLine, nB, 2 ) + "|" $ "|--|++|->|" )
               IF nA != 0 .AND. ::lSpaces .AND. nA < nLen .AND. !( SubStr( cLine, nA + 1, 1 ) == " " )
                  cLine := Left( cLine, nA ) + " " + SubStr( cLine, nA + 1 )
                  nLen++
                  i++
               ENDIF
               IF nB != 0 .AND. ::lSpaces .AND. nB > 1 .AND. !( SubStr( cLine, nB - 1, 1 ) == " " )
                  cLine := Left( cLine, nB - 1 ) + " " + SubStr( cLine, nB )
                  nLen++
                  i++
               ENDIF
            ENDIF
            nA := nB := 0
         ELSEIF ( nState == FL_STATE_QUOTED .AND. c == cSymb ) .OR. ;
               ( nState == FL_STATE_SQBR .AND. c == "]" )
            nState := FL_STATE_ANY
         ENDIF
         IF i == nLen .AND. lFirst .AND. nState == FL_STATE_STRING
            i++
            ::ConvertCmd( @cLine, nBegin, i )
         ENDIF
      NEXT
   ENDIF

   RETURN cLine

METHOD ConvertCmd( cLine, nBegin, nEnd, lFirstOnly ) CLASS HBFORMATCODE

   LOCAL nPos, cToken := Upper( SubStr( cLine, nBegin, nEnd - nBegin ) )

   IF ::lCase
      hb_default( @lFirstOnly, .F. )
      IF ( ( nPos := At( "," + cToken, ::cCommands ) ) != 0 .AND. ( Len( cToken ) >= 4 ;
            .OR. SubStr( ::cCommands, nPos + Len( cToken ) + 1, 1 ) == "," ) ) ;
            .OR. ;
            ( ! lFirstOnly .AND. ;
            ( nPos := At( "," + cToken, ::cClauses ) ) != 0 .AND. ( Len( cToken ) >= 4 ;
            .OR. SubStr( ::cClauses, nPos + Len( cToken ) + 1, 1 ) == "," ) )
         IF ::nCaseCmd > 0
            IF ::nCaseCmd > 1
               cToken := iif( ::nCaseCmd == 2, Lower( cToken ), Left( cToken, 1 ) + ;
                  Lower( SubStr( cToken, 2 ) ) )
            ENDIF
            cLine := iif( nBegin == 1, cToken + SubStr( cLine, nEnd ), ;
               Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd ) )
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

METHOD ConvertFnc( cLine, nBegin, nEnd ) CLASS HBFORMATCODE

   LOCAL nPos, cToken := Upper( SubStr( cLine, nBegin, nEnd - nBegin ) )

   IF ::lCase .AND. ::nCaseFnc > 0
      IF ( nPos := hb_AtI( "," + cToken + ",", ::cFunctions ) ) != 0

         IF ::nCaseFnc > 1
            nPos++
            cToken := iif( ::nCaseFnc == 2, Lower( cToken ), iif( ::nCaseFnc == 3, ;
               Left( cToken, 1 ) + Lower( SubStr( cToken, 2 ) ), ;
               SubStr( ::cFunctions, nPos, Len( cToken ) ) ) )
         ENDIF
         cLine := iif( nBegin == 1, cToken + SubStr( cLine, nEnd ), ;
            Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd ) )
      ELSEIF ::nCaseUnk > 0
         cToken := iif( ::nCaseUnk == 2, Lower( cToken ), ;
            Left( cToken, 1 ) + Lower( SubStr( cToken, 2 ) ) )
         cLine := iif( nBegin == 1, cToken + SubStr( cLine, nEnd ), ;
            Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd ) )
      ENDIF
   ENDIF

   RETURN .T.

METHOD ConvertBool( cLine, nBegin, nEnd ) CLASS HBFORMATCODE

   LOCAL cBool := ",NOT,AND,OR,F,T,"
   LOCAL nPos, cToken := Upper( SubStr( cLine, nBegin, nEnd - nBegin ) )

   IF ::lCase
      IF ( nPos := At( "," + cToken + ",", cBool ) ) != 0
         IF ::lCnvNot .AND. nPos == 1
            cLine := Left( cLine, nBegin - 2 ) + "!" + SubStr( cLine, nEnd + 1 )
         ELSE
            IF ::nCaseBoo > 0
               IF ::nCaseBoo > 1
                  cToken := iif( ::nCaseBoo == 2, Lower( cToken ), Left( cToken, 1 ) + ;
                     Lower( SubStr( cToken, 2 ) ) )
               ENDIF
               cLine := Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd )
            ENDIF
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

METHOD SetOption( cLine, i, aIni ) CLASS HBFORMATCODE

   LOCAL nPos, cToken1, cToken2, cTemp, xRes

   IF ( nPos := At( "=", cLine ) ) != 0
      cToken1 := Upper( RTrim( Left( cLine, nPos - 1 ) ) )
      cToken2 := LTrim( SubStr( cLine, nPos + 1 ) )
      IF __objHasMsg( Self, cToken1 )
         IF Empty( cToken2 )
            xRes := ""
         ELSEIF IsDigit( cToken2 ) .OR. ( Left( cToken2, 1 ) == "-" .AND. IsDigit( LTrim( SubStr( cToken2, 2 ) ) ) )
            xRes := Val( cToken2 )
         ELSEIF IsAlpha( cToken2 )
            IF ( cTemp := Upper( cToken2 ) ) == "ON" .OR. cTemp == "YES"
               xRes := .T.
            ELSEIF cTemp == "OFF" .OR. cTemp == "NO"
               xRes := .F.
            ELSE
               IF Right( cToken2, 1 ) == ";" .AND. aIni != NIL
                  xRes := RTrim( Left( cToken2, Len( cToken2 ) - 1 ) )
                  DO WHILE ++i < Len( aIni )
                     IF Right( aIni[ i ], 1 ) == ";"
                        xRes += AllTrim( Left( aIni[ i ], Len( aIni ) - 1 ) )
                     ELSE
                        xRes += AllTrim( aIni[ i ] )
                        EXIT
                     ENDIF
                  ENDDO
               ELSE
                  xRes := cToken2
               ENDIF
            ENDIF
         ELSE
            ::nErr := 3
         ENDIF
         IF ::nErr == 0 .AND. !( ValType( xRes ) == Left( cToken1, 1 ) )
            ::nErr := 4
         ENDIF
      ELSE
         ::nErr := 2
      ENDIF
   ELSE
      ::nErr := 1
   ENDIF
   IF ::nErr == 0
      __objSendMsg( Self, "_" + cToken1, xRes )
   ELSE
      ::nLineErr := i
      ::cLineErr := cLine
   ENDIF

   RETURN ::nErr == 0

METHOD ReadIni( cIniName ) CLASS HBFORMATCODE

   LOCAL i, nLen, aIni, c

   IF hb_FileExists( cIniName )
      aIni := hb_ATokens( StrTran( MemoRead( cIniName ), Chr( 13 ) + Chr( 10 ), Chr( 10 ) ), Chr( 10 ) )
      nLen := Len( aIni )
      FOR i := 1 TO nLen
         IF ! Empty( aIni[ i ] := AllTrim( aIni[ i ] ) ) .AND. ;
               !( ( c := Left( aIni[ i ], 1 ) ) == ";" ) .AND. !( c == "#" )
            IF ! ::SetOption( aIni[ i ], @i, aIni )
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN ::nErr == 0

METHOD File2Array( cFileName ) CLASS HBFORMATCODE

   LOCAL aFile

   IF hb_FileExists( cFileName )
      aFile := hb_ATokens( StrTran( MemoRead( cFileName ), Chr( 13 ) + Chr( 10 ), Chr( 10 ) ), Chr( 10 ) )
      IF ::nEol < 0
         IF Chr( 13 ) + Chr( 10 ) $ MemoRead( cFileName )
            ::cEol := Chr( 13 ) + Chr( 10 )
         ELSE
            ::cEol := Chr( 10 )
         ENDIF
      ENDIF
   ENDIF

   RETURN aFile

METHOD Array2File( cFileName, aFile ) CLASS HBFORMATCODE

   LOCAL i, nLen := Len( aFile ), cName, cBakName, cPath
   LOCAL cFile

   cName := iif( ( i := RAt( ".", cFileName ) ) == 0, cFileName, SubStr( cFileName, 1, i - 1 ) )
   IF Empty( ::cExtSave )
      cBakName := cName + iif( Left( ::cExtBack, 1 ) == ".", "", "." ) + ::cExtBack
      IF hb_FCopy( cFileName, cBakName ) == F_ERROR
         RETURN .F.
      ENDIF
   ENDIF

   IF ! Empty( ::cExtSave )
      cFileName := cName + iif( Left( ::cExtSave, 1 ) == ".", "", "." ) + ::cExtSave
   ENDIF
   IF ::lFCaseLow
      cPath := iif( ( i := RAt( "\", cFileName ) ) == 0, ;
         iif( ( i := RAt( "/", cFileName ) ) == 0, "", Left( cFileName, i ) ), ;
         Left( cFileName, i ) )
      cFileName := cPath + Lower( iif( i == 0, cFileName, SubStr( cFileName, i + 1 ) ) )
   ENDIF

   cFile := ""
   FOR i := 1 TO nLen
      IF aFile[ i ] == NIL
         EXIT
      ENDIF
      IF i < nLen .OR. ! Empty( aFile[ i ] )
         cFile += aFile[ i ] + ::cEol
      ENDIF
   NEXT

   DO WHILE Right( cFile, Len( ::cEol ) * 2 ) == Replicate( ::cEol, 2 )
      cFile := hb_StrShrink( cFile, Len( ::cEol ) )
   ENDDO

   RETURN hb_MemoWrit( cFileName, cFile )

STATIC FUNCTION rf_AINS( arr, nItem, cItem )

   IF ATail( arr ) != NIL
      AAdd( arr, NIL )
   ENDIF
   AIns( arr, nItem )
   arr[ nItem ] := cItem

   RETURN Len( arr )

STATIC FUNCTION rf_ADEL( arr, nItem )

   ADel( arr, nItem )

   RETURN NIL

STATIC FUNCTION FindNotQuoted( subs, stroka, nPos2 )

   LOCAL nPos1, i, c, nState := 0, cSymb

   hb_default( @nPos2, 1 )

   DO WHILE .T.
      IF ( nPos1 := hb_At( subs, stroka, nPos2 ) ) == 0
         EXIT
      ENDIF
      FOR i := nPos2 TO nPos1 - 1
         c := SubStr( stroka, i, 1 )
         IF nState == 0
            IF c == '"' .OR. c == "'"
               cSymb := c
               nState := 1
            ELSEIF c == "["
               nState := 2
            ENDIF
         ELSEIF ( nState == 1 .AND. c == cSymb ) .OR. ( nState == 2 .AND. c == "]" )
            nState := 0
         ENDIF
      NEXT
      IF nState == 0
         EXIT
      ENDIF
      nPos2 := nPos1 + 1
   ENDDO

   RETURN nPos1
