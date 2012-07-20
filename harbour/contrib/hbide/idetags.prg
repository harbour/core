/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Andy Wos
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                       Code Forwarded by Andy Wos
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               22Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"

#define LEFTEQUAL( l, r )       ( hb_BLeft( l, hb_BLen( r ) ) == r )

/*----------------------------------------------------------------------*/

FUNCTION UpdateTags( cModule, aSummary, aSumData, aFuncList, aLines, aText )
   LOCAL cType, cName, cSyntax, n, m, i, cSource, cExt
   LOCAL cClassName := ""
   LOCAL aTags      := {}
   LOCAL cGoodFuncNameChars := "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

   Hb_FNameSplit( cModule, , @cSource, @cExt )
   cSource += cExt

   aFuncList := {}
   aLines    := {}

   FOR i := 1 TO Len( aSummary )
      cSyntax := lTrim( Substr( aSummary[ i ], 8 ) )

      IF LEFT( cSyntax,14 ) == "HB_FUNC_STATIC"
         cType   := "HB_FUNC_STATIC"
         cSyntax := AllTrim( StrTran( StrTran( Substr( cSyntax, 15 ), "(", "" ), ")", "" ) )
         cName   := cSyntax
         cSyntax += "(void)"

      ELSEIF LEFT( cSyntax, 7 ) == "HB_FUNC"
         cType   := "HB_FUNC"
         cSyntax := AllTrim( StrTran( StrTran( Substr( cSyntax,8 ), "(", "" ), ")", "" ) )
         cName   := cSyntax
         cSyntax += "(void)"

      ELSE
         cType := Upper( Left( cSyntax, At( " ", cSyntax ) - 1 ) )
         IF LEFTEQUAL( cType, "INIT" ) .OR. LEFTEQUAL( cType, "EXIT" )
            cSyntax := LTrim( Substr( cSyntax, 6 ) )
            cType +=" "+Upper( Left( cSyntax, ( n := At( " ", cSyntax ) ) - 1 ) )
            cSyntax := LTrim( Substr( cSyntax, n + 1 ) )

         ELSEIF LEFTEQUAL( cType, "STATIC" )
            cSyntax := LTrim( Substr( cSyntax, 7 ) )
            cType += " " + Upper( Left( cSyntax, ( n := At( " ", cSyntax ) ) - 1 ) )
            cSyntax := LTrim( Substr( cSyntax, n + 1 ) )

         ELSEIF LEFTEQUAL( cType, "DLL" )
            cSyntax := LTrim( Substr( cSyntax, 4 ) )
            cType += " " + Upper( Left( cSyntax, ( n := At( " ", cSyntax ) ) - 1 ) )
            cSyntax := LTrim( Substr( cSyntax, n + 1 ) )

         ELSEIF LEFTEQUAL( cType, "DLL32" )
            cSyntax := LTrim( Substr( cSyntax, 6 ) )
            cType += " " + Upper( Left( cSyntax, ( n := At( " ", cSyntax ) ) - 1 ) )
            cSyntax := LTrim( Substr( cSyntax, n + 1 ) )

         ELSEIF cType == "CLASS"
            cSyntax := LTrim( SubStr( cSyntax, 7 ) )
         ELSE
            cSyntax := lTrim( SubStr( cSyntax, hb_BLen( cType ) + 1 ) )
         ENDIF

         IF ( n := RAt( "(", cSyntax ) ) > 0
            cName := Trim( hb_BLeft( cSyntax, n-1 ) )
            FOR m := Len( cName ) TO 1 STEP -1
               IF ( ! substr( cName, m, 1 ) $ cGoodFuncNameChars )
                  EXIT
               ENDIF
            NEXT

            IF m > 0
               cType   += " " + Left( cSyntax, m - 1 )
               cSyntax := SubStr( cSyntax, m + 1 )
               cName   := SubStr( cName, m + 1 )
            ENDIF

            IF LEFTEQUAL( cType, "METH" )
               IF ( n := Rat( " CLASS ", cSyntax ) ) > 0
                  cClassName := Upper( AllTrim( Substr( cSyntax, n + 7 ) ) )
               ELSE
                  cClassName := cClassName
               ENDIF
            ENDIF

         ELSEIF LEFTEQUAL( cType, "METH" )
            IF ( n := Rat( " CLASS ", cSyntax ) ) > 0
               cName      := Left( cSyntax, n - 1 )
               cClassName := Upper( AllTrim( Substr( cSyntax, n + 7 ) ) )
            ELSE
               cName      := cSyntax
               cClassName := cClassName
            ENDIF
         ELSE
            cName := cSyntax
         ENDIF

         IF cType == "CLASS"
            cClassName := Upper( cName )
            cClassName := Left( cClassName, At( " ", cClassName + " " ) - 1 ) // remove INHERIT
         ENDIF

      ENDIF

      IF !aSumData[ i,1 ]  // not commented out !
         aAdd( aTags, { Upper( Trim( cName ) ) ,;
                        iif( LEFTEQUAL( cType, "METH" ), iif( !Empty( cClassName ), cClassName + ":" + Upper( cType ), Upper( cType ) ), Upper( cType ) ),;
                        aSumData[ i,2 ],;
                        cModule        ,;
                        cSyntax        ,;
                        cType          ,;
                        Iif( LEFTEQUAL( cType, "METH" ), ":", "" ) + cSyntax, ;
                        aText[ aSumData[ i,2 ] ] ;
                     };
             )
      ENDIF

      AAdd( aFuncList, { Iif( LEFTEQUAL( cType, "METH" ), ":", "" ) + cSyntax, aSumData[ i, 2 ], aSumData[ i, 1 ] } )
      AAdd( aLines, i )
   NEXT

   RETURN aTags

/*----------------------------------------------------------------------*/

FUNCTION Summarize( aText, cComments, aSumData, nFileType )
   LOCAL cline, i,j, n, c, a, aSummary
   LOCAL cCLine     := ""
   LOCAL lInComment := .F.
   LOCAL lInClass   := .F.
   LOCAL nLine      := 1
   LOCAL nType      := nFileType
   LOCAL nNest      := 0

   aSummary := {}
   aSumData := {}
   n := Len( aText )

   FOR i := 1 TO n
      cline := Upper( AllTrim( aText[ i ] ) )

      IF nType == 9 .OR. nType == 10 // PRG code

         IF ! lInClass  .OR. LEFTEQUAL( cLine, "METH" )
            IF LEFTEQUAL( cLine, 'FUNCTION '         ) .OR. ;
               LEFTEQUAL( cLine, 'PROCEDURE '        ) .OR. ;
               LEFTEQUAL( cLine, 'STATIC PROCEDURE ' ) .OR. ;
               LEFTEQUAL( cLine, 'STATIC FUNCTION '  ) .OR. ;
               LEFTEQUAL( cLine, 'DLL FUNC'          ) .OR. ;
               LEFTEQUAL( cLine, 'DLL32 FUNC'        ) .OR. ;
               LEFTEQUAL( cLine, 'METHOD '           ) .OR. ;
               LEFTEQUAL( cLine, 'FUNC '             ) .OR. ;
               LEFTEQUAL( cLine, 'PROC '             ) .OR. ;
               LEFTEQUAL( cLine, 'METH '             ) .OR. ;
               LEFTEQUAL( cLine, 'STATIC PROC '      ) .OR. ;
               LEFTEQUAL( cLine, 'STATIC FUNC '      ) .OR. ;
               LEFTEQUAL( cLine, 'INIT FUNC'         ) .OR. ;
               LEFTEQUAL( cLine, 'INIT PROC'         ) .OR. ;
               LEFTEQUAL( cLine, 'EXIT FUNC'         ) .OR. ;
               LEFTEQUAL( cLine, 'EXIT PROC'         ) .OR. ;
               LEFTEQUAL( cLine, 'CLASS '            ) .OR. ;
               LEFTEQUAL( cLine, 'INIT CLASS '       )

               // check for multiline declaration
               a := ParsExpr( aText[ i ], .F., , , .F. )
               c := ""
               AEval( a, {|x| c += x } )
               c := AllTrim( c )
               nLine := i

               DO WHILE Right( c, 1 ) == ";"
                  i++
                  c := Left( c, Len( c ) - 1 )
                  a := ParsExpr( aText[ i ] + " ", .F. , , ,.F. )
                  AEval( a,{|x| c += x } )
                  c := AllTrim( c )
               ENDDO

               IF lInClass
                  IF LEFTEQUAL( cLine, 'METH' )
                     IF " INLINE " $ Upper( c )
                        c := Trim( Left( c, At( " INLINE ", Upper( c ) ) ) )
                     ELSE
                        LOOP
                     ENDIF
                  ENDIF
               ENDIF

               IF ( j := At( ";", c ) ) > 0
                  c := Left( c, j-1 )
               ENDIF

               lInComment := ( Asc( substr( cComments, nLine, 1 ) ) == 3 )
               aAdd( aSumData, { lInComment, nLine } )
               aAdd( aSummary, Str( nLine, 5, 0 ) + ': ' +  c )

               IF ! lInClass
                  lInClass := LEFTEQUAL( cLine, 'CLASS ' )
               ENDIF
            ELSEIF LEFTEQUAL( cLine, "#PRAGMA BEGINDUMP" )
               nType  := 1
               nNest  := 0
               ccLine := ""
            ENDIF
         ELSE
            lInClass := ( cline # 'END' )
         ENDIF

      ELSE   // C code

        IF LEFTEQUAL( cLine, "#PRAGMA ENDDUMP" )
            nType := nFileType
            ELSE
               IF nNest == 0
                  IF .F.
                     nLine := i
                     a := ParsExpr( aText[ i ], .F., @lInComment , , .F., .F. )
                     ccLine := ""
                     c := ""

                     AEval( a, {|x| c += x, nNest := Max( 0, nNest + IIf( x == "{", 1, IIf( x == "}", -1, 0 ) ) ) } )
                     ccLine := AllTrim( c )
                  ELSE
                     IF ! Empty( cLine )
                        a := ParsExpr( aText[ i ], .F. , @lInComment, , .F., .F. )
                        c := ""
                        IF Len( a ) > 0 .AND. a[ 1 ] == "#"
                           ccLine := ""
                           nLine  := i+1
                        ELSE
                           AEval( a, {|x| IIf( x == ";", ( nLine := i+1, ccLine :="",  c := "" ), c += x ), nNest := Max( 0, nNest + IIf( x == "{", 1, IIf( x == "}", -1, 0 ) ) ) } )
                        ENDIF
                        IF !lInComment .AND. ! Empty(c)
                           ccLine += AllTrim( c ) + " "
                           ccLine := StrTran( ccLine, ") {", "){" )
                        ENDIF
                     ELSE
                        nLine := i+1
                     ENDIF
                  ENDIF

                  IF "){" $ ccLine  // this is a function call
                     ccline := Left( ccline, At( "){", ccline ) )
                     aAdd( aSumData, { lInComment, nLine } )
                     aAdd( aSummary, Str( nLine, 5, 0 ) + ': ' + ccline ) //lTrim( ::aText[ i ] ) )
                     ccLine := ""
                  ENDIF

              ELSE
                 a := ParsExpr( aText[ i ], .F. , @lInComment, , .F., .F.)
                 AEval( a,{|x| nNest := Max( 0, nNest + IIf( x == "{", 1, IIf( x == "}", -1, 0 ) ) ) } )
                 ccLine := ""
                 nLine  := i+1
              ENDIF
          ENDIF
      ENDIF
   NEXT

   RETURN aSummary

/*----------------------------------------------------------------------*/
/*
   updates comments of the whole file or down from line nline (if supplied)
   comment nState codes:

   start commented   => 1
   leave commented   => 2
   unchanged but contains comment code => 4
*/
FUNCTION CheckComments( aText )
   LOCAL i, j, lChanged, nState, cText, nPos, cQuote, lInString
   LOCAL lInComment   := .F.
   LOCAL lLineComment := .F.
   LOCAL cComments    := ""
   LOCAL nLine        := 1
   LOCAL nLines       := Len( aText )

   cComments := Pad( cComments, nLines, chr( 0 ) )

   FOR i := nLine TO nLines

      nState   := iif( lInComment, 1, 0 )
      lChanged := .F.
      cText    := aText[ i ]

      IF "*" $ cText .OR. "/" $ cText  // quick test
         nPos  := 0

         DO WHILE .T.

            // check if terminated
            IF lInComment
               IF ( nPos := At( "*/", cText ) ) > 0
                  lChanged   := .T.
                  lInComment := .F.
                  cText      := SubStr( cText, nPos + 2 )
               ENDIF
            ENDIF

            // check for comment start
            IF !lInComment
               DO WHILE ( nPos := hb_at( "//", cText, nPos+1 ) ) > 0   // is the line commented out ?
                  IF ! IsInString( cText, nPos, 1 )                    // or is it just part of the string ?
                     cText := Left( cText, nPos - 1 )

                     IF Empty( cText )
                        lInComment   := .T.
                        lLineComment := .T.
                     ENDIF

                     EXIT
                  ENDIF
               ENDDO
            ENDIF

            IF ( nPos := At( "/*", cText ) ) > 0      // does start of comment exits on that line

               IF ( lInstring := IsInString( cText, nPos, 1, @cQuote ) )

                  // find the end of the string
                  FOR j := nPos+1 TO Len( cText )
                      IF substr( cText, j, 1 ) == cQuote
                         cText     := SubStr( cText, j + 1 )
                         nPos      := At( "/*", cText )
                         lInString := .F.
                         EXIT
                      ENDIF
                  NEXT
                  IF j > Len( cText ) .AND. lInString
                     cText := ""
                  ENDIF

               ELSE
                  lChanged   := .T.
                  lInComment := .T.
                  cText := Substr( cText, nPos + 2 )
               ENDIF

            ENDIF

            IF nPos == 0 .OR. Empty( cText )
               EXIT
            ENDIF
         ENDDO
      ENDIF

      cComments := substr( cComments, 1, i-1 ) + chr( nState + ( iif( lInComment, 2, 0 ) + iif( lChanged, 4, 0 ) ) ) + substr( cComments, i+1 )
      IF nState == 0 .AND. lLineComment
         lInComment := .F.
      ENDIF
      lLineComment := .F.

   NEXT

   RETURN cComments

/*----------------------------------------------------------------------*/

STATIC FUNCTION IsInString( cText, nPos, nStart, cQuote )
   LOCAL j, cTkn
   LOCAL lInString := .F.

   STATIC cAnyQuote  := '"' + "'"

   FOR j := nStart TO nPos-1          // check if string did not begin before it
       cTkn := substr( cText, j, 1 )
       IF cTkn $ cAnyQuote            // any quote characters present ?
          IF lInstring                // if we are already in string
             IF cTkn == cQuote        // is it a matching quote ?
                lInstring := .F.      // yes, we are no in string any more
             ENDIF
          ELSE                        // we are not in string yet
             cQuote    := cTkn        // this is the streing quote
             lInstring := .T.         // now we are in string
         ENDIF
      ENDIF
   NEXT

   RETURN lInString

/*----------------------------------------------------------------------*/
