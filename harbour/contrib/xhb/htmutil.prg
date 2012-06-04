/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Misc Suport Functions for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://harbour-project.org
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "html.ch"
#include "hbclass.ch"

STATIC s_aGreek := {}

/****
*
*     backButton()
*
*/

PROCEDURE BackButton( cImage, oHtm )

   DEFAULT cImage TO "back.gif"
   DEFAULT oHtm TO HtmlPageObject()

   IMAGE( cImage ) ;
          URL "" ;
          ONCLICK "history.back()" ;
          OF oHtm

   RETURN

/****
*
*     BackFormButton()
*
*/

PROCEDURE BackFormButton( cImage, oForm )

   LOCAL oBut

   DEFAULT oForm TO HtmlFormObject()

   IF cImage == NIL
      DEFINE BUTTON oBut ;
         NAME "BackButton" ;
         VALUE "go Back" ;
         ONCLICK "history.back()" ;
         IN oForm
   ELSE
      DEFINE IMAGE oBut ;
         NAME "BackButton" ;
         SOURCE( cImage ) ;
         ONCLICK "history.back()" ;
         IN oForm
   ENDIF

   RETURN

/****
*
*     PutCounter()
*
*/

FUNCTION PutCounter( oHtm, nNumber, cDir, nDigits, nWidth, bgColor, nBorder )

   LOCAL i
   LOCAL cStr    := ""

   DEFAULT oHtm TO HtmlPageObject()
   DEFAULT nNumber TO 0
   DEFAULT cDir TO "/images/counters/"
   DEFAULT nWidth TO 50
   DEFAULT nDigits TO Len( Alltrim( Str( nNumber ) ) )
   DEFAULT nBorder TO 1
   DEFAULT BGCOLOR TO "black"

   IF HB_ISNUMERIC( nNumber )
      cStr := Strzero( nNumber, nDigits )
   ENDIF

   oHtm:Write( "<center>" )
   DEFINE TABLE ;
      BORDER( nBorder ) ;
      WIDTH( nWidth ) ;
      COLORBG( bgColor ) ;
      OF oHtm

   oHtm:newTableRow()
   oHtm:newTableCell( "center" )

   FOR i := 1 TO Len( cStr )
      IMAGE cDir + Substr( cStr, i, 1 ) + ".gif" ;
         BORDER 0 ;
         OF oHtm
   NEXT

   oHtm:endTableCell()
   oHtm:endTableRow()
   oHtm:endTable()

   oHtm:Write( "</center>" )

RETURN Nil


PROC htmlBrowse( oHtm, cAction, lUseLinks )

   LOCAL i
   LOCAL n      := 0
   LOCAL aFlds  := Dbstruct()
   LOCAL cAlign

   DEFAULT cAction to "confirm('RECORD: '+this.name+'\nPlace your action here !!!')"
   DEFAULT lUseLinks to .F.

   /*
// browse caption...
oHtm:defineTable( 1, 1, 98 )
oHtm:newTableRow("black")
oHtm:newTableCell(,,,3,"white")
oHtm:Write( htmlSpace( 5 ) +"Browsing Table: <B>"+ALIAS()+"</B>" )
oHtm:endTableCell()
oHtm:endTableRow("black")
oHtm:endTable()
*/

   oHtm:defineTable( Fcount(), 1, 98 )

   oHtm:TableHead( " ? " )
   FOR i := 1 TO Fcount()
      oHtm:TableHead( aFlds[ i, 1 ] )
   NEXT

   WHILE !( Eof() )

      // each row has a different color...
      IF n == 0
         oHtm:newTableRow( "lightyellow" )
         n := 1
      ELSE
         oHtm:newTableRow( "#9196A0" )
         n := 0
      ENDIF

      // put an action pushbutton...
      oHtm:newTableCell( "center" )
      IF lUseLinks
         LINK ( cAction ) ;
               TEXT( hb_ntos( Recno() ) ) ;
               OF oHtm
      ELSE
         PUSH BUTTON ;
            NAME "'B" + hb_ntos( Recno() ) + "'" ;
            CAPTION "' ? '" ;
            ONCLICK ( cAction ) ;
            OF oHtm
      ENDIF
      oHtm:EndTableCell()

      // --> put the formatted fields data...
      FOR i := 1 TO Len( aFlds )
         cAlign := IF( aFlds[ i, 2 ] == "N", "RIGHT", "CENTER" )
         oHtm:newTableCell( cAlign,,,, "black" )
         oHtm:Write( greek2Html( htmlany2Str( Fieldget( i ) ) ) )
         oHtm:EndTableCell()
      NEXT
      oHtm:endTableRow()
      SKIP
   ENDDO

   oHtm:endTable()

   RETURN
#ifdef MYSQL
PROC htmlBrowseSql( oHtm, cAction, lUseLinks, cTarget, oServer, oQuery )

   LOCAL i
   LOCAL p
   LOCAL n       := 0
   LOCAL oCurRow

   LOCAL cAlign

   DEFAULT cAction to "confirm('RECORD: '+this.name+'\nPlace your action here !!!')"
   DEFAULT lUseLinks to .F.

   /*
// browse caption...
oHtm:defineTable( 1, 1, 98 )
oHtm:newTableRow("black")
oHtm:newTableCell(,,,3,"white")
oHtm:Write( htmlSpace( 5 ) +"Browsing Table: <B>"+ALIAS()+"</B>" )
oHtm:endTableCell()
oHtm:endTableRow("black")
oHtm:endTable()
*/
   oquery := oServer:query( 'Select * from rafael' )

   oHtm:defineTable( oQuery:FCount(), 1, 98 )
   oCurRow := oQuery:getRow( 1 )
   oHtm:TableHead( " ? " )
   FOR i := 1 TO oQuery:FCount()
      oHtm:TableHead( oCurRow:FieldName( i ) )
   NEXT

   FOR p := 1 TO oQuery:LastRec()
      oCurRow := oQuery:getRow( P )
      // each row has a different color...
      IF n == 0
         oHtm:newTableRow( "lightyellow" )
         n := 1
      ELSE
         oHtm:newTableRow( "#9196A0" )
         n := 0
      ENDIF

      // put an action pushbutton...
      oHtm:newTableCell( "center" )
      IF lUseLinks
         LINK( cAction ) ;
               TEXT( hb_ntos( oQuery:RECNO() ) ) ;
               OF oHtm
      ELSE
         PUSH BUTTON ;
            NAME "'B" + hb_ntos( oQuery:RECNO() ) + "'" ;
            CAPTION "' ? '" ;
            ONCLICK( cAction ) ;
            OF oHtm
      ENDIF
      oHtm:EndTableCell()

      // --> put the formatted fields data...

      FOR i := 1 TO oquery:fcount()

         cAlign := IF( oCurRow:FieldType( i ) == "N", "RIGHT", "CENTER" )
         oHtm:newTableCell( cAlign,,,, "black" )
         oHtm:Write( greek2Html( htmlany2Str( oCurRow:FieldGet( i ) ) ) )
         oHtm:EndTableCell()
      END
      oHtm:endTableRow()
      IF !oquery:eof()
         oquery:skip()
      ENDIF

   NEXT
   oHtm:endTable()

   RETURN
#endif

   //*** EOF ***//

Class JWindow

   DATA nH
   DATA Name INIT ""
   DATA oHtm
   DATA VarName INIT ""
   DATA URL INIT ""
   DATA Features INIT ""

   DATA ScreenX, ScreenY INIT 100
   DATA HEIGHT, WIDTH INIT 300
   DATA innerHeight, innerWidth, outerHeight INIT 0
   DATA alwaysRaised, alwaysLowered INIT .F.
   DATA Menubar, personalBar INIT .F.
   DATA location, directories, copyHistory INIT .F.
   DATA Toolbar INIT .F.
   DATA Status, TitleBar INIT .T.
   DATA Scrollbars, Resizable, dependent INIT .T.

   DATA TITLE
   DATA aScriptSRC
   DATA aServerSRC
   DATA BGIMAGE, BGCOLOR, fontColor
   DATA Style

   DATA onLoad
   DATA onUnLoad

   METHOD New( cVarName, cUrl, cName, x, y, w, h )

   METHOD setOnLoad( c ) INLINE ::onLoad := c

   METHOD setOnUnLoad( c ) INLINE ::onUnLoad := c

   METHOD Alert( c ) INLINE ::QOut( "alert('" + c + "')" )

   METHOD confirm( c ) INLINE ::QOut( "confirm('" + c + "')" )

   METHOD SetSize( x, y, h, w )

   METHOD Write( c )

   METHOD lineBreak() INLINE ::QOut( "<BR>" )

   METHOD Paragraph() INLINE ::QOut( "<P></P>" )

   METHOD CENTER( l ) INLINE ::QOut( If( l, "<CENTER>", "</CENTER>" ) )

   METHOD bold( l ) INLINE ::QOut( If( l, "<B>", "</B>" ) )

   METHOD Italic( l ) INLINE ::QOut( If( l, "<I>", "</I>" ) )

   METHOD ULine( l ) INLINE ::QOut( If( l, "<U>", "</U>" ) )

   METHOD Put()

   METHOD Begin()

   METHOD END ()

   METHOD Qout( c )

   METHOD WriteLN( c ) INLINE ::qOut( c )

   METHOD SetFeatures( alwaysRaised, alwaysLowered, ;
                       Resizable, Menubar, personalBar, ;
                       dependent, location, directories, ;
                       Scrollbars, Status, TitleBar, Toolbar, copyHistory )

   METHOD ImageURL( cImage, cUrl, nHeight, nBorder, ;
   cOnClick, cOnMsover, cOnMsout, ;
   cName, cAlt )

ENDCLASS

   /****
*
*     Start a new window definition
*
*
*
*/

METHOD New( cVarName, cUrl, cName, x, y, w, h ) Class JWindow

   DEFAULT cVarName to "newWin"
   DEFAULT cURL to " "
   DEFAULT cName to cVarName            //"newWin"
   DEFAULT x to 100
   DEFAULT y to 100
   DEFAULT h to 300
   DEFAULT w to 300

   ::nH      := HtmlPageHandle()
   ::oHtm    := HtmlPageObject()
   ::varName := cVarName
   ::URL     := cUrl
   ::Name    := cName

   ::ScreenX := x
   ::ScreenY := y
   ::height  := h
   ::width   := w

   // objectViewer( self )

RETURN Self

/****
*
*     Set the properties of the window
*
*
*
*/

METHOD SetFeatures( alwaysRaised, alwaysLowered, ;
                    Resizable, Menubar, personalBar, ;
                    dependent, location, directories, ;
                    Scrollbars, Status, TitleBar, Toolbar, copyHistory ) Class JWindow

   LOCAL cStr := ""

   DEFAULT alwaysRaised to ::alwaysRaised
   DEFAULT alwaysLowered to ::alwaysLowered
   DEFAULT Resizable to ::Resizable
   DEFAULT Menubar to ::Menubar
   DEFAULT personalBar to ::personalBar
   DEFAULT dependent to ::dependent
   DEFAULT location to ::location
   DEFAULT directories to ::directories
   DEFAULT Scrollbars to ::Scrollbars
   DEFAULT Status to ::Status
   DEFAULT TitleBar to ::TitleBar
   DEFAULT Toolbar to ::Toolbar
   DEFAULT copyHistory to ::copyHistory

   IF alwaysRaised
      cStr += "alwaysraised=yes,"
   ELSE
      cStr += "alwaysraised=no,"
   ENDIF
   IF alwaysLowered
      cStr += "alwayslowered=yes,"
   ELSE
      cStr += "alwayslowered=no,"
   ENDIF
   IF Resizable
      cStr += "resizable=yes,"
   ELSE
      cStr += "resizable=no,"
   ENDIF
   IF Menubar
      cStr += "menubar=yes,"
   ELSE
      cStr += "menubar=no,"
   ENDIF
   IF personalBar
      cStr += "personalbar=yes,"
   ELSE
      cStr += "personalbar=no,"
   ENDIF
   IF dependent
      cStr += "dependent=yes,"
   ELSE
      cStr += "dependent=no,"
   ENDIF
   IF location
      cStr += "location=yes,"
   ELSE
      cStr += "location=no,"
   ENDIF
   IF directories
      cStr += "directories=yes,"
   ELSE
      cStr += "directories=no,"
   ENDIF
   IF Scrollbars
      cStr += "scrollbars=yes,"
   ELSE
      cStr += "scrollbars=no,"
   ENDIF
   IF Status
      cStr += "status=yes,"
   ELSE
      cStr += "status=no,"
   ENDIF
   IF TitleBar
      cStr += "titlebar=yes,"
   ELSE
      cStr += "titlebar=no,"
   ENDIF
   IF Toolbar
      cStr += "toolbar=yes,"
   ELSE
      cStr += "toolbar=no,"
   ENDIF
   IF copyHistory
      cStr += "copyHistory=yes,"
   ELSE
      cStr += "copyHistory=no,"
   ENDIF

   ::features += IF( Empty( ::Features ), cStr + ",", cStr )

RETURN Self

/****
*
*     set the size for the window
*
*
*
*/

METHOD SetSize( x, y, h, w ) Class JWindow

   LOCAL cStr := ""
   DEFAULT x to ::ScreenX, ;
      y to ::ScreenY, ;
      h to ::height, ;
      w to ::width

   ::ScreenX := x
   ::ScreenY := y
   ::height  := h
   ::width   := w

   cStr := "screenX=" + hb_ntos( ::screenX ) + ","

   cStr += "screenY=" + hb_ntos( ::screenY ) + ","
   cStr += "height=" + hb_ntos( ::height ) + ","
   cStr += "width=" + hb_ntos( ::width )

   ::features += IF( Empty( ::Features ), cStr + ",", cStr )

RETURN Self

/****
*
*     Open the window from within the current document
*
*
*
*/

METHOD Put() Class JWindow

   LOCAL cStr := ""

   IF ::nH == NIL
      ::nH := HtmlPageHandle()
      IF ::nH == NIL
         RETURN Self
      ENDIF
   ENDIF

   IF Empty( ::features )
      ::setSize()
      ::setFeatures()
   ENDIF

   IF ! HB_ISSTRING( ::name )
      ::name := "newWin"
   ENDIF

   cStr += ::varName + " = window.open('" + ;
      ::URL + "', '" + ;
      ::varName + "', '" + ;
      ::features + "')"

   htmljscmd( ::nH, cStr )

RETURN Self

/****
*
*     Output stand alone Javascript code in the current document
*
*/

METHOD Write( c ) Class JWindow

   htmljscmd( ::nH, ::varName + ".document.write('" + c + "')" + CRLF() )
RETURN Self

/****
*
*     Output Javascript (or HTML) code in the current document and
*     in the current script
*
*/

METHOD Qout( c ) Class JWindow

   Fwrite( ::nH, ::varName + ".document.write('" + c + "')" + CRLF() )
RETURN Self

/****
*
*     Begin HTML output to the window from within the current document
*     and the current script
*
*
*/

METHOD Begin() Class JWindow

   LOCAL i

   Fwrite( ::nH, "<SCRIPT LANGUAGE=JavaScript 1.2>" + CRLF() )
   Fwrite( ::nH, "<!--" + CRLF() )
   ::QOut( "<HTML><HEAD>" )

   IF ::Title != NIL
      ::QOut( "<TITLE>" + ::Title + "</TITLE>" )
   ENDIF

   IF ::aScriptSrc != NIL
      FOR i := 1 TO Len( ::aScriptSrc )
         ::QOut( ;
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + ::aScriptSrc[ i ] + '"></SCRIPT>' )
      NEXT
   ENDIF

   IF ::aServerSrc != NIL
      FOR i := 1 TO Len( ::aServerSrc )
         ::QOut( ;
                 '<SCRIPT LANGUAGE=JavaScript SRC="' + ::aServerSrc[ i ] + '" RUNAT=SERVER></SCRIPT>' )
      NEXT
   ENDIF

   IF ::Style != NIL
      ::QOut( "<STYLE> " + ::Style + " </STYLE>" )
   ENDIF

   ::QOut( "</HEAD>" + "<BODY" )

   IF ::onLoad != NIL
      ::Qout( '   onLoad="' + ::onLoad + '"' )
   ENDIF

   IF ::onUnLoad != NIL
      ::QOut( ' onUnload="' + ::onUnLoad + '"' )
   ENDIF

   ::QOut( '>' )

   IF ::bgColor != NIL
      ::QOut( '<BODY BGCOLOR="' + ::bgColor + '">' )
   ENDIF

   IF ::fontColor != NIL
      ::QOut( '<BODY TEXT="' + ::fontColor + '">' )
   ENDIF

   IF ::bgImage != NIL
      ::QOut( '<BODY BACKGROUND="' + ::bgImage + '">' )
   ENDIF

   Fwrite( ::nH, "//-->" )
   Fwrite( ::nH, "</SCRIPT>" + CRLF() )

RETURN Self

/****
*
*     End HTML output to the window
*
*
*
*/

METHOD END () Class JWindow

   htmljscmd( ::nH, ::varName + ".document.write('</BODY></HTML>')" + CRLF() )

RETURN Self

/****
*
*     Place an image link to the window
*
*
*
*/

METHOD ImageURL( cImage, cUrl, nHeight, nBorder, ;
                    cOnClick, cOnMsover, cOnMsout, ;
                    cName, cAlt ) Class JWindow

   LOCAL cStr := ""

   DEFAULT cUrl to ""

   IF cName != NIL
      cStr += ' NAME= "' + cName + '"' + CRLF()
   ENDIF
   IF cAlt != NIL
      cStr += ' ALT= "' + cAlt + '"' + CRLF()
   ENDIF

   IF nBorder != NIL
      cStr += " BORDER = " + hb_ntos( nBorder ) + CRLF()
   ENDIF

   IF nHeight != NIL
      cStr += " HEIGHT = " + hb_ntos( nHeight ) + "% " + CRLF()
   ENDIF

   IF cOnClick != NIL
      cStr += ' onClick="' + cOnClick + '"' + CRLF()
   ENDIF
   IF cOnMsOver != NIL
      cStr += ' onMouseOver="' + cOnMsOver + '"' + CRLF()
   ENDIF
   IF cOnMsOut != NIL
      cStr += ' onMouseOut="' + cOnMsOut + '"' + CRLF()
   ENDIF

   IF cURL != NIL
      ::QOut( '<A HREF=' + cUrl + '><IMG SRC="' + cImage + '"' + ;
              cStr + '></A>' )
   ELSE
      ::QOut( '<IMG SRC="' + cImage + '"' + ;
              cStr + '></A>' )
   ENDIF
RETURN Self

//*** EOF ***//


#define GREEK_ALPHABET  {;
"Á", "Â", "Ã", "Ä", "Å", "Æ", "Ç", "È", "É", "Ê", "Ë", "Ì", "Í", "Î", "Ï", "Ð", "Ñ", "Ó", "Ô", "Õ", "Ö", "×", "Ø", "Ù", ;                   // A-—
   "á", "â", "ã", "ä", "å", "æ", "ç", "è", "é", "ê", "ë", "ì", "í", "î", "ï", "ð", "ñ", "ó", "ò", "ô", "õ", "ö", "÷", "ø", ;                // ˜-¯
   "ù", "Ü", "Ý", "Þ", "ú", "ß", "ü", "ý", "û", "þ", "¢", "¸", "¹", "º", "¼", "¾", "¿", "Ú", "Û" ;  // à-ð
   }

/****
*
*     InitGreek()
*
*     Initializes the international languages support.
*
*     Uses GREEK_ALPHABET array as a match pattern. Replace with your
*     own character set.
*/

FUNCTION initGreek()

   LOCAL i
   LOCAL n
   LOCAL aGreek := GREEK_ALPHABET
   LOCAL aArr   := Array( 255 )

   FOR i := 1 TO 255
      aArr[ i ] := Chr( i )
   NEXT

   n := 1
   FOR i := 128 TO 175
      aArr[ i ] := aGreek[ n ]
      n ++
   NEXT

   FOR i := 224 TO 240
      aArr[ i ] := aGreek[ n ]
      n ++
   NEXT
   aArr[ 244 ] := aGreek[ n ]
   n ++
   aArr[ 245 ] := aGreek[ n ]

RETURN aArr

/****
*
*     Greek2Html()
*
*     Converts International characters to HTML
*/

FUNCTION Greek2Html( cText )

   LOCAL i
   LOCAL cStr := ""

   IF Empty( s_aGreek )
      s_aGreek := initGreek()
   ENDIF
   FOR I := 1 TO Len( cText )
      cStr += s_aGreek[ Asc( Substr( cText, i, 1 ) ) ]
   NEXT

RETURN cStr
