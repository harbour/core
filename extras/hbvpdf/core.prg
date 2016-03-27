/* Optimized for Harbour, Unicode support, portability, cleanups
   and other Harbour tailoring by Viktor Szakats */

#include "hbvpdf.ch"

#include "fileio.ch"

#define CRLF  ( Chr( 13 ) + Chr( 10 ) )

THREAD STATIC t_aReport

// ---
FUNCTION pdfInit()

   t_aReport := Array( PARAMLEN )

   RETURN t_aReport

// ---
PROCEDURE pdfWidth( _nWidth )

   t_aReport[ REPORTWIDTH ] := _nWidth

   RETURN

// ---
FUNCTION pdfTextWidth( cStr )
   RETURN pdfLen( cStr ) / 25.4

// ---
FUNCTION pdfAtSay( cString, nRow, nCol, cUnits, lExact, cId )

   LOCAL _nFont, lReverse, nAt

   hb_default( @nRow, t_aReport[ REPORTLINE ] )
   hb_default( @cUnits, "R" )
   hb_default( @lExact, .F. )

   IF t_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFATSAY", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   IF ( nAt := At( "#pagenumber#", cString ) ) > 0
      cString := Stuff( cString, nAt, Len( "#pagenumber#" ), hb_ntos( pdfPageNumber() ) )
   ENDIF

   lReverse := .F.
   SWITCH cUnits
   CASE "M"
      nRow := pdfM2Y( nRow )
      nCol := pdfM2X( nCol )
      EXIT
   CASE "R"
      IF ! lExact
         pdfCheckLine( nRow )
         nRow += t_aReport[ PDFTOP ]
      ENDIF
      nRow := pdfR2D( nRow )
      nCol := pdfM2X( t_aReport[ PDFLEFT ] ) + ;
         nCol * 100.00 / t_aReport[ REPORTWIDTH ] * ;
         ( t_aReport[ PAGEX ] - pdfM2X( t_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
      EXIT
   ENDSWITCH
   IF ! Empty( cString )
      cString := pdfStringB( cString )

      SWITCH hb_BRight( cString, 1 )
      CASE hb_BChar( 255 )  // reverse
         cString := hb_BLeft( cString, hb_BLen( cString ) - 1 )
         pdfBox( t_aReport[ PAGEY ] - nRow - t_aReport[ FONTSIZE ] + 2.0, nCol, t_aReport[ PAGEY ] - nRow + 2.0, nCol + pdfM2X( pdfLen( cString ) ) + 1,, 100, "D" )
         t_aReport[ PAGEBUFFER ] += " 1 g "
         lReverse := .T.
         EXIT
      CASE hb_BChar( 254 )  // underline
         cString := hb_BLeft( cString, hb_BLen( cString ) - 1 )
         pdfBox( t_aReport[ PAGEY ] - nRow + 0.5,  nCol, t_aReport[ PAGEY ] - nRow + 1, nCol + pdfM2X( pdfLen( cString ) ) + 1,, 100, "D" )
         EXIT
      ENDSWITCH

      // version 0.01
      IF ( nAt := hb_BAt( hb_BChar( 253 ), cString ) ) > 0  // some color text inside
         t_aReport[ PAGEBUFFER ] += CRLF + ;
            Chr_RGB( hb_BSubStr( cString, nAt + 1, 1 ) ) + " " + ;
            Chr_RGB( hb_BSubStr( cString, nAt + 2, 1 ) ) + " " + ;
            Chr_RGB( hb_BSubStr( cString, nAt + 3, 1 ) ) + " rg "
         cString := hb_BLeft( cString, nAt - 1 ) + hb_BSubStr( cString, nAt + 4 )
      ENDIF
      // version 0.01

      _nFont := AScan( t_aReport[ FONTS ], {| arr | arr[ 1 ] == t_aReport[ FONTNAME ] } )
      IF !( t_aReport[ FONTNAME ] == t_aReport[ FONTNAMEPREV ] )
         t_aReport[ FONTNAMEPREV ] := t_aReport[ FONTNAME ]
         t_aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + hb_ntos( _nFont ) + " " + LTrim( Transform( t_aReport[ FONTSIZE ], "999.99" ) ) + " Tf " + LTrim( Transform( nCol, "9999.99" ) ) + " " + LTrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ELSEIF t_aReport[ FONTSIZE ] != t_aReport[ FONTSIZEPREV ]
         t_aReport[ FONTSIZEPREV ] := t_aReport[ FONTSIZE ]
         t_aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + hb_ntos( _nFont ) + " " + LTrim( Transform( t_aReport[ FONTSIZE ], "999.99" ) ) + " Tf " + LTrim( Transform( nCol, "9999.99" ) ) + " " + LTrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ELSE
         t_aReport[ PAGEBUFFER ] += CRLF + "BT " + LTrim( Transform( nCol, "9999.99" ) ) + " " + LTrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ENDIF
      IF lReverse
         t_aReport[ PAGEBUFFER ] += " 0 g "
      ENDIF
   ENDIF

   RETURN NIL

// ---
PROCEDURE pdfBold()

   SWITCH pdfGetFontInfo( "NAME" )
   CASE "Times"
      t_aReport[ FONTNAME ] := 2
      EXIT
   CASE "Helvetica"
      t_aReport[ FONTNAME ] := 6
      EXIT
   OTHERWISE
      t_aReport[ FONTNAME ] := 10  // Courier // 0.04
   ENDSWITCH
   AAdd( t_aReport[ PAGEFONTS ], t_aReport[ FONTNAME ] )
   IF AScan( t_aReport[ FONTS ], {| arr | arr[ 1 ] == t_aReport[ FONTNAME ] } ) == 0
      AAdd( t_aReport[ FONTS ], { t_aReport[ FONTNAME ], ++t_aReport[ NEXTOBJ ] } )
   ENDIF

   RETURN

// ---
PROCEDURE pdfBoldItalic()

   SWITCH pdfGetFontInfo( "NAME" )
   CASE "Times"
      t_aReport[ FONTNAME ] := 4
      EXIT
   CASE "Helvetica"
      t_aReport[ FONTNAME ] := 8
      EXIT
   OTHERWISE
      t_aReport[ FONTNAME ] := 12  // 0.04
   ENDSWITCH
   AAdd( t_aReport[ PAGEFONTS ], t_aReport[ FONTNAME ] )
   IF AScan( t_aReport[ FONTS ], {| arr | arr[ 1 ] == t_aReport[ FONTNAME ] } ) == 0
      AAdd( t_aReport[ FONTS ], { t_aReport[ FONTNAME ], ++t_aReport[ NEXTOBJ ] } )
   ENDIF

   RETURN

// ---
PROCEDURE pdfBookAdd( cTitle, nLevel, nPage, nLine )

   AAdd( t_aReport[ BOOKMARK ], { nLevel, AllTrim( cTitle ), 0, 0, 0, 0, 0, 0, nPage, iif( nLevel == 1, t_aReport[ PAGEY ], t_aReport[ PAGEY ] - nLine * 72 / t_aReport[ LPI ] ) } )

   RETURN

// ---
PROCEDURE pdfBookClose()

   t_aReport[ BOOKMARK ] := nil

   RETURN

// ---
STATIC FUNCTION pdfBookCount( nRecno, nCurLevel )

   LOCAL nTempLevel, nCount := 0, nLen := Len( t_aReport[ BOOKMARK ] )

   ++nRecno
   DO WHILE nRecno <= nLen
      nTempLevel := t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nTempLevel <= nCurLevel
         EXIT
      ELSE
         IF nCurLevel + 1 == nTempLevel
            ++nCount
         ENDIF
      ENDIF
      ++nRecno
   ENDDO

   RETURN -1 * nCount

// ---
STATIC FUNCTION pdfBookFirst( nRecno, nCurLevel, nObj )

   LOCAL nFirst := 0, nLen := Len( t_aReport[ BOOKMARK ] )

   ++nRecno
   IF nRecno <= nLen
      IF nCurLevel + 1 == t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         nFirst := nRecno
      ENDIF
   ENDIF

   RETURN iif( nFirst == 0, nFirst, nObj + nFirst )

// ---
STATIC FUNCTION pdfBookLast( nRecno, nCurLevel, nObj )

   LOCAL nLast := 0, nLen := Len( t_aReport[ BOOKMARK ] )

   ++nRecno
   IF nRecno <= nLen
      IF nCurLevel + 1 == t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         DO WHILE nRecno <= nLen .AND. nCurLevel + 1 <= t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
            IF nCurLevel + 1 == t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
               nLast := nRecno
            ENDIF
            ++nRecno
         ENDDO
      ENDIF
   ENDIF

   RETURN iif( nLast == 0, nLast, nObj + nLast )

// ---
STATIC FUNCTION pdfBookNext( nRecno, nCurLevel, nObj )

   LOCAL nTempLevel, nNext := 0, nLen := Len( t_aReport[ BOOKMARK ] )

   ++nRecno
   DO WHILE nRecno <= nLen
      nTempLevel := t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      DO CASE
      CASE nCurLevel > nTempLevel
         EXIT
      CASE nCurLevel == nTempLevel
         nNext := nRecno
         EXIT
      OTHERWISE
         // keep going
      ENDCASE
      ++nRecno
   ENDDO

   RETURN iif( nNext == 0, nNext, nObj + nNext )

// ---
PROCEDURE pdfBookOpen()

   t_aReport[ BOOKMARK ] := {}

   RETURN

// ---
STATIC FUNCTION pdfBookParent( nRecno, nCurLevel, nObj )

   LOCAL nTempLevel
   LOCAL nParent := 0

   --nRecno
   DO WHILE nRecno > 0
      nTempLevel := t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nTempLevel < nCurLevel
         nParent := nRecno
         EXIT
      ENDIF
      --nRecno
   ENDDO

   RETURN iif( nParent == 0, nObj - 1, nObj + nParent )

// ---
STATIC FUNCTION pdfBookPrev( nRecno, nCurLevel, nObj )

   LOCAL nTempLevel
   LOCAL nPrev := 0

   --nRecno
   DO WHILE nRecno > 0
      nTempLevel := t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      DO CASE
      CASE nCurLevel > nTempLevel
         EXIT
      CASE nCurLevel == nTempLevel
         nPrev := nRecno
         EXIT
      OTHERWISE
         // keep going
      ENDCASE
      --nRecno
   ENDDO

   RETURN iif( nPrev == 0, nPrev, nObj + nPrev )

// ---
FUNCTION pdfBox( x1, y1, x2, y2, nBorder, nShade, cUnits, cColor, cId )

   LOCAL cBoxColor

   hb_default( @nBorder, 0 )
   hb_default( @nShade, 0 )
   hb_default( @cUnits, "M" )
   hb_default( @cColor, "" )

   // version 0.02
   IF Empty( cColor )
      cBoxColor := ""
   ELSE
      cBoxColor := " " + ;
         Chr_RGB( hb_BSubStr( cColor, 2, 1 ) ) + " " + ;
         Chr_RGB( hb_BSubStr( cColor, 3, 1 ) ) + " " + ;
         Chr_RGB( hb_BSubStr( cColor, 4, 1 ) ) + " rg "
   ENDIF
   // version 0.02

   IF t_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFBOX", cId, { x1, y1, x2, y2, nBorder, nShade, cUnits } )
   ENDIF

   SWITCH cUnits
   CASE "M"
      y1 += 0.5
      y2 += 0.5

      IF nShade > 0
         // version 0.02
         t_aReport[ PAGEBUFFER ] += CRLF + Transform( 1.00 - nShade / 100.00, "9.99" ) + " g " + cBoxColor + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( y2 - y1 ) ) + " -" + hb_ntos( pdfM2X( x2 - x1 ) ) + " re f 0 g"
      ENDIF

      IF nBorder > 0
         t_aReport[ PAGEBUFFER ] += ;
            CRLF + "0 g " + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( y2 - y1 ) ) + " -" + hb_ntos( pdfM2X( nBorder ) ) + " re f" + ;
            CRLF + "0 g " + hb_ntos( pdfM2X( y2 - nBorder ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( nBorder ) ) + " -" + hb_ntos( pdfM2X( x2 - x1 ) ) + " re f" + ;
            CRLF + "0 g " + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x2 - nBorder ) ) + " " + hb_ntos( pdfM2X( y2 - y1 ) ) + " -" + hb_ntos( pdfM2X( nBorder ) ) + " re f" + ;
            CRLF + "0 g " + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( nBorder ) ) + " -" + hb_ntos( pdfM2X( x2 - x1 ) ) + " re f"
      ENDIF
      EXIT
   CASE "D"  // "Dots"
      // x1, y1, x2, y2 - nTop, nLeft, nBottom, nRight
      IF nShade > 0
         // version 0.02
         t_aReport[ PAGEBUFFER ] += CRLF + Transform( 1.00 - nShade / 100.00, "9.99" ) + " g " + cBoxColor + hb_ntos( y1 ) + " " + hb_ntos( t_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( y2 - y1 ) + " -" + hb_ntos( x2 - x1 ) + " re f 0 g"
      ENDIF

      IF nBorder > 0
         //      1
         //   +-----+
         // 4 |     | 2
         //   +-----+
         //      3
         t_aReport[ PAGEBUFFER ] += ;
            CRLF + "0 g " + hb_ntos( y1 ) + " " + hb_ntos( t_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( y2 - y1 ) + " -" + hb_ntos( nBorder ) + " re f" + ;
            CRLF + "0 g " + hb_ntos( y2 - nBorder ) + " " + hb_ntos( t_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( nBorder ) + " -" + hb_ntos( x2 - x1 ) + " re f" + ;
            CRLF + "0 g " + hb_ntos( y1 ) + " " + hb_ntos( t_aReport[ PAGEY ] - x2 + nBorder ) + " " + hb_ntos( y2 - y1 ) + " -" + hb_ntos( nBorder ) + " re f" + ;
            CRLF + "0 g " + hb_ntos( y1 ) + " " + hb_ntos( t_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( nBorder ) + " -" + hb_ntos( x2 - x1 ) + " re f"
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

// ---
PROCEDURE pdfBox1( nTop, nLeft, nBottom, nRight, nBorderWidth, cBorderColor, cBoxColor )

   hb_default( @nBorderWidth, 0.5 )
   hb_default( @cBorderColor, hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) )
   hb_default( @cBoxColor, hb_BChar( 255 ) + hb_BChar( 255 ) + hb_BChar( 255 ) )

   t_aReport[ PAGEBUFFER ] += CRLF + ;
      Chr_RGB( hb_BSubStr( cBorderColor, 1, 1 ) ) + " " + ;
      Chr_RGB( hb_BSubStr( cBorderColor, 2, 1 ) ) + " " + ;
      Chr_RGB( hb_BSubStr( cBorderColor, 3, 1 ) ) + ;
      " RG" + ;
      CRLF + ;
      Chr_RGB( hb_BSubStr( cBoxColor, 1, 1 ) ) + " " + ;
      Chr_RGB( hb_BSubStr( cBoxColor, 2, 1 ) ) + " " + ;
      Chr_RGB( hb_BSubStr( cBoxColor, 3, 1 ) ) + ;
      " rg" + ;
      CRLF + hb_ntos( nBorderWidth ) + " w" + ;
      CRLF + hb_ntos( nLeft + nBorderWidth / 2 ) + " " + ;
      CRLF + hb_ntos( t_aReport[ PAGEY ] - nBottom + nBorderWidth / 2 ) + " " + ;
      CRLF + hb_ntos( nRight - nLeft - nBorderWidth ) + ;
      CRLF + hb_ntos( nBottom - nTop - nBorderWidth ) + " " + ;
      " re" + ;
      CRLF + "B"

   RETURN

// ---
FUNCTION pdfCenter( cString, nRow, nCol, cUnits, lExact, cId )

   LOCAL nLen, nAt

   hb_default( @nRow, t_aReport[ REPORTLINE ] )
   hb_default( @cUnits, "R" )
   hb_default( @lExact, .F. )
   hb_default( @nCol, iif( cUnits == "R", t_aReport[ REPORTWIDTH ] / 2, t_aReport[ PAGEX ] / 72 * 25.4 / 2 ) )

   IF t_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFCENTER", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   IF ( nAt := At( "#pagenumber#", cString ) ) > 0
      cString := Stuff( cString, nAt, Len( "#pagenumber#" ), hb_ntos( pdfPageNumber() ) )
   ENDIF

   nLen := pdfLen( cString ) / 2

   IF cUnits == "R" .AND. ! lExact
      pdfCheckLine( nRow )
      nRow += t_aReport[ PDFTOP ]
   ENDIF
   pdfAtSay( cString, pdfR2M( nRow ), iif( cUnits == "R", t_aReport[ PDFLEFT ] + ( t_aReport[ PAGEX ] / 72 * 25.4 - 2 * t_aReport[ PDFLEFT ] ) * nCol / t_aReport[ REPORTWIDTH ], nCol ) - nLen, "M", lExact )

   RETURN NIL

// ---
STATIC PROCEDURE pdfCheckLine( nRow )

   IF nRow + t_aReport[ PDFTOP ] > t_aReport[ PDFBOTTOM ]
      pdfNewPage()
      nRow := t_aReport[ REPORTLINE ]
   ENDIF
   t_aReport[ REPORTLINE ] := nRow

   RETURN

// ---
PROCEDURE pdfClose()

   LOCAL nI, cTemp, nCurLevel, nObj1, nLast, nCount, nFirst, nRecno, nBooklen

   FIELD FIRST, PREV, NEXT, LAST, COUNT, PARENT, PAGE, COORD, TITLE, LEVEL

   pdfClosePage()

   // kids
   t_aReport[ REFS ][ 2 ] := t_aReport[ DOCLEN ]
   cTemp := ;
      "1 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/Type /Pages /Count " + hb_ntos( t_aReport[ REPORTPAGE ] ) + CRLF + ;
      "/Kids ["

   FOR nI := 1 TO t_aReport[ REPORTPAGE ]
      cTemp += " " + hb_ntos( t_aReport[ PAGES ][ nI ] ) + " 0 R"
   NEXT

   cTemp += ;
      " ]" + CRLF + ;
      ">>" + CRLF + ;
      "endobj" + CRLF

   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   // info
   ++t_aReport[ REPORTOBJ ]
   AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )
   cTemp := ;
      hb_ntos( t_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/Producer ()" + CRLF + ;
      "/Title ()" + CRLF + ;
      "/Author ()" + CRLF + ;
      "/Creator ()" + CRLF + ;
      "/Subject ()" + CRLF + ;
      "/Keywords ()" + CRLF + ;
      "/CreationDate (D:" + hb_BLeft( hb_TToS( hb_DateTime() ), 14 ) + ")" + CRLF + ;
      ">>" + CRLF + ;
      "endobj" + CRLF
   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   // root
   ++t_aReport[ REPORTOBJ ]
   AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )
   cTemp := ;
      hb_ntos( t_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      "<< /Type /Catalog /Pages 1 0 R /Outlines " + hb_ntos( t_aReport[ REPORTOBJ ] + 1 ) + " 0 R" + iif( ( nBookLen := Len( t_aReport[ BOOKMARK ] ) ) > 0, " /PageMode /UseOutlines", "" ) + " >>" + CRLF + "endobj" + CRLF
   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   ++t_aReport[ REPORTOBJ ]
   nObj1 := t_aReport[ REPORTOBJ ]

   IF nBookLen > 0

      nRecno := 1
      nFirst := t_aReport[ REPORTOBJ ] + 1
      nLast := 0
      nCount := 0
      DO WHILE nRecno <= nBookLen
         nCurLevel := t_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         t_aReport[ BOOKMARK ][ nRecno ][ BOOKPARENT ] := pdfBookParent( nRecno, nCurLevel, t_aReport[ REPORTOBJ ] )
         t_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ]   := pdfBookPrev( nRecno, nCurLevel, t_aReport[ REPORTOBJ ] )
         t_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ]   := pdfBookNext( nRecno, nCurLevel, t_aReport[ REPORTOBJ ] )
         t_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ]  := pdfBookFirst( nRecno, nCurLevel, t_aReport[ REPORTOBJ ] )
         t_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ]   := pdfBookLast( nRecno, nCurLevel, t_aReport[ REPORTOBJ ] )
         t_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ]  := pdfBookCount( nRecno, nCurLevel )
         IF nCurLevel == 1
            nLast := nRecno
            ++nCount
         ENDIF
         ++nRecno
      ENDDO

      nLast += t_aReport[ REPORTOBJ ]

      cTemp := hb_ntos( t_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + "<< /Type /Outlines /Count " + hb_ntos( nCount ) + " /First " + hb_ntos( nFirst ) + " 0 R /Last " + hb_ntos( nLast ) + " 0 R >>" + CRLF + "endobj" // + CRLF
      AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )
      t_aReport[ DOCLEN ] += hb_BLen( cTemp )
      hb_vfWrite( t_aReport[ HANDLE ], cTemp )

      ++t_aReport[ REPORTOBJ ]
      nRecno := 1
      FOR nI := 1 TO nBookLen
         cTemp := CRLF + hb_ntos( t_aReport[ REPORTOBJ ] + nI - 1 ) + " 0 obj" + CRLF + ;
            "<<" + CRLF + ;
            "/Parent " + hb_ntos( t_aReport[ BOOKMARK ][ nRecno ][ BOOKPARENT ] ) + " 0 R" + CRLF + ;
            "/Dest [" + hb_ntos( t_aReport[ PAGES ][ t_aReport[ BOOKMARK ][ nRecno ][ BOOKPAGE ] ] ) + " 0 R /XYZ 0 " + hb_ntos( t_aReport[ BOOKMARK ][ nRecno ][ BOOKCOORD ] ) + " 0]" + CRLF + ;
            "/Title (" + AllTrim( t_aReport[ BOOKMARK ][ nRecno ][ BOOKTITLE ] ) + ")" + CRLF + ;
            iif( t_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ] > 0, "/Prev " + hb_ntos( t_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ] ) + " 0 R" + CRLF, "" ) + ;
            iif( t_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ] > 0, "/Next " + hb_ntos( t_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ] ) + " 0 R" + CRLF, "" ) + ;
            iif( t_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ] > 0, "/First " + hb_ntos( t_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ] ) + " 0 R" + CRLF, "" ) + ;
            iif( t_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ] > 0, "/Last " + hb_ntos( t_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ] ) + " 0 R" + CRLF, "" ) + ;
            iif( t_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ] != 0, "/Count " + hb_ntos( t_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ] ) + CRLF, "" ) + ;
            ">>" + CRLF + "endobj" + CRLF

         AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] + 2 )
         t_aReport[ DOCLEN ] += hb_BLen( cTemp )
         hb_vfWrite( t_aReport[ HANDLE ], cTemp )
         ++nRecno
      NEXT
      pdfBookClose()

      t_aReport[ REPORTOBJ ] += nBookLen - 1
   ELSE
      cTemp := hb_ntos( t_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + "<< /Type /Outlines /Count 0 >>" + CRLF + "endobj" + CRLF
      AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )
      t_aReport[ DOCLEN ] += hb_BLen( cTemp )
      hb_vfWrite( t_aReport[ HANDLE ], cTemp )
   ENDIF

   cTemp := CRLF
   t_aReport[ DOCLEN ] += hb_BLen( cTemp )

   ++t_aReport[ REPORTOBJ ]

   cTemp += ;
      "xref" + CRLF + ;
      "0 " + hb_ntos( t_aReport[ REPORTOBJ ] ) + CRLF + ;
      StrZero( t_aReport[ REFS ][ 1 ], 10 ) + " 65535 f" + CRLF

   FOR nI := 2 TO Len( t_aReport[ REFS ] )
      cTemp += StrZero( t_aReport[ REFS ][ nI ], 10 ) + " 00000 n" + CRLF
   NEXT

   cTemp += ;
      "trailer << /Size " + hb_ntos( t_aReport[ REPORTOBJ ] ) + " /Root " + hb_ntos( nObj1 - 1 ) + " 0 R /Info " + hb_ntos( nObj1 - 2 ) + " 0 R >>" + CRLF + ;
      "startxref" + CRLF + ;
      hb_ntos( t_aReport[ DOCLEN ] ) + CRLF + ;
      "%%EOF" + CRLF
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )
#if 0
   IF t_aReport[ OPTIMIZE ]
      pdfOptimize( ) coming !
   ENDIF
#endif
   hb_vfClose( t_aReport[ HANDLE ] )

   t_aReport := NIL

   RETURN

// ---
STATIC PROCEDURE pdfClosePage()

   LOCAL cTemp, cBuffer, nBuffer, nRead, nI, k, nImage, nFont, hFileImage

   AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )

   AAdd( t_aReport[ PAGES ], t_aReport[ REPORTOBJ ] + 1 )

   cTemp := ;
      hb_ntos( ++t_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/Type /Page /Parent 1 0 R" + CRLF + ;
      "/Resources " + hb_ntos( ++t_aReport[ REPORTOBJ ] ) + " 0 R" + CRLF + ;
      "/MediaBox [ 0 0 " + LTrim( Transform( t_aReport[ PAGEX ], "9999.99" ) ) + " " + ;
      LTrim( Transform( t_aReport[ PAGEY ], "9999.99" ) ) + " ]" + CRLF + ;
      "/Contents " + hb_ntos( ++t_aReport[ REPORTOBJ ] ) + " 0 R" + CRLF + ;
      ">>" + CRLF + ;
      "endobj" + CRLF

   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )
   cTemp := ;
      hb_ntos( t_aReport[ REPORTOBJ ] - 1 ) + " 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/ColorSpace << /DeviceRGB /DeviceGray >>" + CRLF + ; // version 0.01
   "/ProcSet [ /PDF /Text /ImageB /ImageC ]"

   IF Len( t_aReport[ PAGEFONTS ] ) > 0
      cTemp += CRLF + ;
         "/Font" + CRLF + ;
         "<<"

      FOR EACH nI IN t_aReport[ PAGEFONTS ]
         nFont := AScan( t_aReport[ FONTS ], {| arr | arr[ 1 ] == nI } )
         cTemp += CRLF + "/Fo" + hb_ntos( nFont ) + " " + hb_ntos( t_aReport[ FONTS ][ nFont ][ 2 ] ) + " 0 R"
      NEXT

      cTemp += CRLF + ">>"
   ENDIF

   IF Len( t_aReport[ PAGEIMAGES ] ) > 0
      cTemp += CRLF + "/XObject" + CRLF + "<<"
      FOR EACH nI IN t_aReport[ PAGEIMAGES ]
         IF ( nImage := AScan( t_aReport[ IMAGES ], {| arr | arr[ 1 ] == nI[ 1 ] } ) ) == 0
            AAdd( t_aReport[ IMAGES ], { nI[ 1 ], ++t_aReport[ NEXTOBJ ], pdfImageInfo( nI[ 1 ] ) } )
            nImage := Len( t_aReport[ IMAGES ] )
         ENDIF
         cTemp += CRLF + "/Image" + hb_ntos( nImage ) + " " + hb_ntos( t_aReport[ IMAGES ][ nImage ][ 2 ] ) + " 0 R"
      NEXT
      cTemp += CRLF + ">>"
   ENDIF

   cTemp += CRLF + ">>" + CRLF + "endobj" + CRLF

   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )
   cTemp := hb_ntos( t_aReport[ REPORTOBJ ] ) + " 0 obj << /Length " + ;
      hb_ntos( t_aReport[ REPORTOBJ ] + 1 ) + " 0 R >>" + CRLF + ;
      "stream"

   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   IF Len( t_aReport[ PAGEIMAGES ] ) > 0
      cTemp := ""
      FOR EACH nI IN t_aReport[ PAGEIMAGES ]
         nImage := AScan( t_aReport[ IMAGES ], {| arr | arr[ 1 ] == nI[ 1 ] } )
         cTemp += ;
            CRLF + "q" + ;
            CRLF + hb_ntos( iif( nI[ 5 ] == 0, pdfM2X( t_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_WIDTH ] / t_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_XRES ] * 25.4 ), nI[ 5 ] ) ) + ;
            " 0 0 " + ;
            hb_ntos( iif( nI[ 4 ] == 0, pdfM2X( t_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_HEIGHT ] / t_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_YRES ] * 25.4 ), nI[ 4 ] ) ) + ;
            " " + hb_ntos( nI[ 3 ] ) + ;
            " " + hb_ntos( t_aReport[ PAGEY ] - nI[ 2 ] - ;
            iif( nI[ 4 ] == 0, pdfM2X( t_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_HEIGHT ] / t_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_YRES ] * 25.4 ), nI[ 4 ] ) ) + " cm" + ;
            CRLF + "/Image" + hb_ntos( nImage ) + " Do" + ;
            CRLF + "Q"
      NEXT
      t_aReport[ PAGEBUFFER ] := cTemp + t_aReport[ PAGEBUFFER ]
   ENDIF

   cTemp := ;
      t_aReport[ PAGEBUFFER ] + ;
      CRLF + "endstream" + CRLF + ;
      "endobj" + CRLF

   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )

   cTemp := ;
      hb_ntos( ++t_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      hb_ntos( hb_BLen( t_aReport[ PAGEBUFFER ] ) ) + CRLF + ;
      "endobj" + CRLF

   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   FOR EACH nI IN t_aReport[ FONTS ]
      IF nI[ 2 ] > t_aReport[ REPORTOBJ ]

         AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )

         cTemp := ;
            hb_ntos( nI[ 2 ] ) + " 0 obj" + CRLF + ;
            "<<" + CRLF + ;
            "/Type /Font" + CRLF + ;
            "/Subtype /Type1" + CRLF + ;
            "/Name /Fo" + hb_ntos( nI:__enumIndex() ) + CRLF + ;
            "/BaseFont /" + t_aReport[ TYPE1 ][ nI[ 1 ] ] + CRLF + ;
            "/Encoding /WinAnsiEncoding" + CRLF + ;
            ">>" + CRLF + ;
            "endobj" + CRLF

         t_aReport[ DOCLEN ] += hb_BLen( cTemp )
         hb_vfWrite( t_aReport[ HANDLE ], cTemp )
      ENDIF
   NEXT

   FOR EACH nI IN t_aReport[ IMAGES ]
      IF nI[ 2 ] > t_aReport[ REPORTOBJ ]

         AAdd( t_aReport[ REFS ], t_aReport[ DOCLEN ] )

         // "/Filter /CCITTFaxDecode" for black and white only ?
         cTemp := ;
            hb_ntos( nI[ 2 ] ) + " 0 obj" + CRLF + ;
            "<<" + CRLF + ;
            "/Type /XObject" + CRLF + ;
            "/Subtype /Image" + CRLF + ;
            "/Name /Image" + hb_ntos( nI:__enumIndex() ) + CRLF + ;
            "/Filter [" + iif( ".jpg" $ Lower( nI[ 1 ] ), " /DCTDecode", "" ) + " ]" + CRLF + ;
            "/Width " + hb_ntos( nI[ 3 ][ IMAGE_WIDTH ] ) + CRLF + ;
            "/Height " + hb_ntos( nI[ 3 ][ IMAGE_HEIGHT ] ) + CRLF + ;
            "/BitsPerComponent " + hb_ntos( nI[ 3 ][ IMAGE_BITS ] ) + CRLF + ;
            "/ColorSpace /" + iif( nI[ 3 ][ IMAGE_SPACE ] == 1, "DeviceGray", "DeviceRGB" ) + CRLF + ;
            "/Length " + hb_ntos( nI[ 3 ][ IMAGE_LENGTH ] ) + CRLF + ;
            ">>" + CRLF + ;
            "stream" + CRLF

         t_aReport[ DOCLEN ] += hb_BLen( cTemp )
         hb_vfWrite( t_aReport[ HANDLE ], cTemp )

         hFileImage := hb_vfOpen( nI[ 1 ], FO_READ )
         hb_vfSeek( hFileImage, nI[ 3 ][ IMAGE_FROM ] )

         nBuffer := 8192
         cBuffer := Space( nBuffer )
         k := 0
         DO WHILE k < nI[ 3 ][ IMAGE_LENGTH ]
            IF k + nBuffer <= nI[ 3 ][ IMAGE_LENGTH ]
               nRead := nBuffer
            ELSE
               nRead := nI[ 3 ][ IMAGE_LENGTH ] - k
            ENDIF
            hb_vfRead( hFileImage, @cBuffer, nRead )

            t_aReport[ DOCLEN ] += nRead
            hb_vfWrite( t_aReport[ HANDLE ], cBuffer, nRead )
            k += nRead
         ENDDO

         hb_vfClose( hFileImage )

         cTemp := CRLF + "endstream" + CRLF + ;
            "endobj" + CRLF

         t_aReport[ DOCLEN ] += hb_BLen( cTemp )
         hb_vfWrite( t_aReport[ HANDLE ], cTemp )
      ENDIF
   NEXT

   t_aReport[ REPORTOBJ ] := t_aReport[ NEXTOBJ ]

   t_aReport[ NEXTOBJ ] := t_aReport[ REPORTOBJ ] + 4

   t_aReport[ PAGEBUFFER ] := ""

   RETURN

// ---
STATIC FUNCTION pdfGetFontInfo( cParam )

   IF cParam == "NAME"
      DO CASE
      CASE hb_LeftEq( t_aReport[ TYPE1 ][ t_aReport[ FONTNAME ] ], "Times" )
         RETURN "Times"
      CASE hb_LeftEq( t_aReport[ TYPE1 ][ t_aReport[ FONTNAME ] ], "Helvetica" )
         RETURN "Helvetica"
      OTHERWISE
         RETURN "Courier"  // 0.04
      ENDCASE
   ENDIF

   // size
   RETURN Int( ( t_aReport[ FONTNAME ] - 1 ) % 4 )

// ---
FUNCTION pdfImage( cFile, nRow, nCol, cUnits, nHeight, nWidth, cId )

   hb_default( @nRow, t_aReport[ REPORTLINE ] )
   hb_default( @nCol, 0 )
   hb_default( @nHeight, 0 )
   hb_default( @nWidth, 0 )
   hb_default( @cUnits, "R" )

   IF t_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFIMAGE", cId, { cFile, nRow, nCol, cUnits, nHeight, nWidth } )
   ENDIF

   SWITCH cUnits
   CASE "M"
      nRow := t_aReport[ PAGEY ] - pdfM2Y( nRow )
      nCol := pdfM2X( nCol )
      nHeight := t_aReport[ PAGEY ] - pdfM2Y( nHeight )
      nWidth := pdfM2X( nWidth )
      EXIT
   CASE "R"
      nRow := t_aReport[ PAGEY ] - pdfR2D( nRow )
      nCol := pdfM2X( t_aReport[ PDFLEFT ] ) + ;
         nCol * 100.00 / t_aReport[ REPORTWIDTH ] * ;
         ( t_aReport[ PAGEX ] - pdfM2X( t_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
      nHeight := t_aReport[ PAGEY ] - pdfR2D( nHeight )
      nWidth := pdfM2X( t_aReport[ PDFLEFT ] ) + ;
         nWidth * 100.00 / t_aReport[ REPORTWIDTH ] * ;
         ( t_aReport[ PAGEX ] - pdfM2X( t_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
      EXIT
   CASE "D"
      EXIT
   ENDSWITCH

   AAdd( t_aReport[ PAGEIMAGES ], { cFile, nRow, nCol, nHeight, nWidth } )

   RETURN NIL

// ---
PROCEDURE pdfItalic()

   SWITCH pdfGetFontInfo( "NAME" )
   CASE "Times"
      t_aReport[ FONTNAME ] := 3
      EXIT
   CASE "Helvetica"
      t_aReport[ FONTNAME ] := 7
      EXIT
   OTHERWISE
      t_aReport[ FONTNAME ] := 11  // 0.04
   ENDSWITCH
   AAdd( t_aReport[ PAGEFONTS ], t_aReport[ FONTNAME ] )
   IF AScan( t_aReport[ FONTS ], {| arr | arr[ 1 ] == t_aReport[ FONTNAME ] } ) == 0
      AAdd( t_aReport[ FONTS ], { t_aReport[ FONTNAME ], ++t_aReport[ NEXTOBJ ] } )
   ENDIF

   RETURN

// ---
FUNCTION pdfLen( cString )

   LOCAL nWidth := 0.00, nI, nLen, nArr, nAdd

   IF ! Empty( t_aReport[ FONTWIDTH ] )

      IF hb_BRight( cString, 1 ) == hb_BChar( 255 ) .OR. ;
         hb_BRight( cString, 1 ) == hb_BChar( 254 )  // reverse or underline
         cString := hb_BLeft( cString, hb_BLen( cString ) - 1 )
      ENDIF

      SWITCH pdfGetFontInfo( "NAME" )
      CASE "Times"
         nArr := 1
         EXIT
      CASE "Helvetica"
         nArr := 2
         EXIT
      OTHERWISE
         nArr := 3  // 0.04
      ENDSWITCH

      nAdd := ( t_aReport[ FONTNAME ] - 1 ) % 4

      nLen := Len( cString )
      FOR nI := 1 TO nLen
         nWidth += t_aReport[ FONTWIDTH ][ nArr ][ ( Asc( SubStr( cString, nI, 1 ) ) - 32 ) * 4 + 1 + nAdd ] * 25.4 * t_aReport[ FONTSIZE ] / 720.00 / 100.00
      NEXT
   ENDIF

   RETURN nWidth

// ---
STATIC FUNCTION pdfM2R( mm )
   RETURN Int( t_aReport[ LPI ] * mm / 25.4 )

// ---
STATIC FUNCTION pdfM2X( n )
   RETURN n * 72 / 25.4

// ---
STATIC FUNCTION pdfM2Y( n )
   RETURN t_aReport[ PAGEY ] -  n * 72 / 25.4

// ---
FUNCTION pdfNewLine( n )

   hb_default( @n, 1 )

   IF t_aReport[ REPORTLINE ] + n + t_aReport[ PDFTOP ] > t_aReport[ PDFBOTTOM ]
      pdfNewPage()
      t_aReport[ REPORTLINE ] += 1
   ELSE
      t_aReport[ REPORTLINE ] += n
   ENDIF

   RETURN t_aReport[ REPORTLINE ]

// ---
PROCEDURE pdfNewPage( _cPageSize, _cPageOrient, _nLpi, _cFontName, _nFontType, _nFontSize )

   IF ! Empty( t_aReport[ PAGEBUFFER ] )
      pdfClosePage()
   ENDIF

   t_aReport[ PAGEFONTS ] := {}
   t_aReport[ PAGEIMAGES ] := {}

   ++t_aReport[ REPORTPAGE ]  // NEW !!!

   pdfPageSize( hb_defaultValue( _cPageSize, t_aReport[ PAGESIZE ] ) )
   pdfPageOrient( hb_defaultValue( _cPageOrient, t_aReport[ PAGEORIENT ] ) )
   pdfSetLPI( hb_defaultValue( _nLpi, t_aReport[ LPI ] ) )

   pdfSetFont( ;
      hb_defaultValue( _cFontName, pdfGetFontInfo( "NAME" ) ), ;
      hb_defaultValue( _nFontType, pdfGetFontInfo( "TYPE" ) ), ;
      hb_defaultValue( _nFontSize, t_aReport[ FONTSIZE ] ) )

   pdfDrawHeader()

   t_aReport[ REPORTLINE ] := 0  // 5
   t_aReport[ FONTNAMEPREV ] := 0
   t_aReport[ FONTSIZEPREV ] := 0

   RETURN

// ---
PROCEDURE pdfNormal()

   SWITCH pdfGetFontInfo( "NAME" )
   CASE "Times"
      t_aReport[ FONTNAME ] := 1
      EXIT
   CASE "Helvetica"
      t_aReport[ FONTNAME ] := 5
      EXIT
   OTHERWISE
      t_aReport[ FONTNAME ] := 9  // 0.04
   ENDSWITCH
   AAdd( t_aReport[ PAGEFONTS ], t_aReport[ FONTNAME ] )
   IF AScan( t_aReport[ FONTS ], {| arr | arr[ 1 ] == t_aReport[ FONTNAME ] } ) == 0
      AAdd( t_aReport[ FONTS ], { t_aReport[ FONTNAME ], ++t_aReport[ NEXTOBJ ] } )
   ENDIF

   RETURN

// ---
PROCEDURE pdfOpen( cFile, nLen, lOptimize )

   LOCAL cTemp, nI, nJ, n1, n2 := 896, n12

   t_aReport[ FONTNAME     ] := 1
   t_aReport[ FONTSIZE     ] := 10
   t_aReport[ LPI          ] := 6
   t_aReport[ PAGESIZE     ] := "LETTER"
   t_aReport[ PAGEORIENT   ] := "P"
   t_aReport[ PAGEX        ] := 8.5 * 72
   t_aReport[ PAGEY        ] := 11.0 * 72
   t_aReport[ REPORTWIDTH  ] := hb_defaultValue( nLen, 200 )  // should be as parameter
   t_aReport[ REPORTPAGE   ] := 0
   t_aReport[ REPORTLINE   ] := 0  // 5
   t_aReport[ FONTNAMEPREV ] := 0
   t_aReport[ FONTSIZEPREV ] := 0
   t_aReport[ PAGEBUFFER   ] := ""
   t_aReport[ REPORTOBJ    ] := 1  // 2
   t_aReport[ DOCLEN       ] := 0
   t_aReport[ TYPE1        ] := { "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic", "Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique", "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique"  } // 0.04
   t_aReport[ MARGINS      ] := .T.
   t_aReport[ HEADEREDIT   ] := .F.
   t_aReport[ NEXTOBJ      ] := 0
   t_aReport[ PDFTOP       ] := 1  // top
   t_aReport[ PDFLEFT      ] := 10  // left and right
   t_aReport[ PDFBOTTOM    ] := t_aReport[ PAGEY ] / 72 * t_aReport[ LPI ] - 1  // bottom, default "LETTER", "P", 6
   t_aReport[ HANDLE       ] := hb_vfOpen( cFile, FO_CREAT + FO_TRUNC + FO_WRITE )
   t_aReport[ PAGES        ] := {}
   t_aReport[ REFS         ] := { 0, 0 }
   t_aReport[ BOOKMARK     ] := {}
   t_aReport[ HEADER       ] := {}
   t_aReport[ FONTS        ] := {}
   t_aReport[ IMAGES       ] := {}
   t_aReport[ PAGEIMAGES   ] := {}
   t_aReport[ PAGEFONTS    ] := {}

   cTemp := __pdf_fontsdat()  // times, times-bold, times-italic, times-bolditalic, helvetica..., courier... // 0.04
   n1 := hb_BLen( cTemp ) / ( 2 * n2 )
   t_aReport[ FONTWIDTH ] := Array( n1, n2 )

   t_aReport[ OPTIMIZE ] := hb_defaultValue( lOptimize, .F. )

   t_aReport[ NEXTOBJ ] := t_aReport[ REPORTOBJ ] + 4

   n12 := 2 * n2  // 0.04
   FOR nI := 1 TO n1
      FOR nJ := 1 TO n2
         t_aReport[ FONTWIDTH ][ nI ][ nJ ] := Bin2I( hb_BSubStr( cTemp, ( nI - 1 ) * n12 + ( nJ - 1 ) * 2 + 1, 2 ) )
      NEXT
   NEXT

   t_aReport[ DOCLEN ] := 0
   cTemp := "%PDF-1.3" + CRLF
   t_aReport[ DOCLEN ] += hb_BLen( cTemp )
   hb_vfWrite( t_aReport[ HANDLE ], cTemp )

   RETURN

// ---
PROCEDURE pdfPageSize( _cPageSize, _nWidth, _nHeight )

   LOCAL nSize, nWidth, nHeight

   LOCAL aSize := { ;
      { "LETTER",     8.50, 11.00 }, ;
      { "LEGAL",      8.50, 14.00 }, ;
      { "LEDGER",    11.00, 17.00 }, ;
      { "EXECUTIVE",  7.25, 10.50 }, ;
      { "A4",         8.27, 11.69 }, ;
      { "A3",        11.69, 16.54 }, ;
      { "JIS B4",    10.12, 14.33 }, ;
      { "JIS B5",     7.16, 10.12 }, ;
      { "JPOST",      3.94,  5.83 }, ;
      { "JPOSTD",     5.83,  7.87 }, ;
      { "COM10",      4.12,  9.50 }, ;
      { "MONARCH",    3.87,  7.50 }, ;
      { "C5",         6.38,  9.01 }, ;
      { "DL",         4.33,  8.66 }, ;
      { "B5",         6.93,  9.84 }, ;
      { "USSTDFOLD", 14.87, 11.00 } }

   hb_default( @_nWidth, 0 )
   hb_default( @_nHeight, 0 )

   IF _nWidth == 0 .OR. _nHeight == 0

      hb_default( @_cPageSize, "LETTER" )

      IF ( nSize := AScan( aSize, {| arr | arr[ 1 ] == _cPageSize } ) ) == 0
         nSize := 1
      ENDIF

      t_aReport[ PAGESIZE ] := aSize[ nSize ][ 1 ]

      nWidth := aSize[ nSize ][ 2 ]
      nHeight := aSize[ nSize ][ 3 ]

   ELSE

      IF ( nSize := AScan( aSize, {| arr | ( arr[ 2 ] == _nWidth  ) .AND. ( arr[ 3 ] == _nHeight ) } ) ) == 0
         nSize := AScan( aSize, {| arr | ( arr[ 3 ] == _nWidth ) .AND. ( arr[ 2 ] == _nHeight ) } )
      ENDIF

      IF nSize == 0
         nSize := 1
      ENDIF

      t_aReport[ PAGESIZE ] := aSize[ nSize ][ 1 ]

      nWidth := _nWidth
      nHeight := _nHeight

   ENDIF

   IF t_aReport[ PAGEORIENT ] == "P"
      t_aReport[ PAGEX ] := nWidth * 72
      t_aReport[ PAGEY ] := nHeight * 72
   ELSE
      t_aReport[ PAGEX ] := nHeight * 72
      t_aReport[ PAGEY ] := nWidth * 72
   ENDIF

   RETURN

// ---
PROCEDURE pdfPageOrient( _cPageOrient )

   t_aReport[ PAGEORIENT ] := hb_defaultValue( _cPageOrient, "P" )
   pdfPageSize( t_aReport[ PAGESIZE ] )

   RETURN

// ---
STATIC FUNCTION pdfR2D( nRow )
   RETURN t_aReport[ PAGEY ] - nRow * 72 / t_aReport[ LPI ]

// ---
STATIC FUNCTION pdfR2M( nRow )
   RETURN 25.4 * nRow / t_aReport[ LPI ]

// ---
FUNCTION pdfPageNumber( n )

   hb_default( @n, 0 )

   IF n > 0
      t_aReport[ REPORTPAGE ] := n // NEW !!!
   ENDIF

   RETURN t_aReport[ REPORTPAGE ]

// ---
FUNCTION pdfReverse( cString )
   RETURN cString + hb_BChar( 255 )

// ---
FUNCTION pdfRJust( cString, nRow, nCol, cUnits, lExact, cId )

   LOCAL nLen, nAdj := 1.0, nAt

   hb_default( @nRow, t_aReport[ REPORTLINE ] )
   hb_default( @cUnits, "R" )
   hb_default( @lExact, .F. )

   IF t_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFRJUST", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   IF ( nAt := At( "#pagenumber#", cString ) ) > 0
      cString := Stuff( cString, nAt, Len( "#pagenumber#" ), hb_ntos( pdfPageNumber() ) )
   ENDIF

   nLen := pdfLen( cString )

   IF cUnits == "R" .AND. ! lExact
      pdfCheckLine( nRow )
      nRow += t_aReport[ PDFTOP ]
   ENDIF
   pdfAtSay( cString, pdfR2M( nRow ), iif( cUnits == "R", t_aReport[ PDFLEFT ] + ( t_aReport[ PAGEX ] / 72 * 25.4 - 2 * t_aReport[ PDFLEFT ] ) * nCol / t_aReport[ REPORTWIDTH ] - nAdj, nCol ) - nLen, "M", lExact )

   RETURN NIL

// ---
FUNCTION pdfSetFont( _cFont, _nType, _nSize, cId )

   hb_default( @_cFont, "Times" )
   hb_default( @_nType, 0 )
   hb_default( @_nSize, 10 )

   IF t_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFSETFONT", cId, { _cFont, _nType, _nSize } )
   ENDIF

   t_aReport[ FONTSIZE ] := _nSize

   SWITCH Upper( _cFont )
   CASE "TIMES"
      t_aReport[ FONTNAME ] := _nType + 1
      EXIT
   CASE "HELVETICA"
      t_aReport[ FONTNAME ] := _nType + 5
      EXIT
   OTHERWISE
      t_aReport[ FONTNAME ] := _nType + 9  // 0.04
   ENDSWITCH

   AAdd( t_aReport[ PAGEFONTS ], t_aReport[ FONTNAME ] )

   IF AScan( t_aReport[ FONTS ], {| arr | arr[ 1 ] == t_aReport[ FONTNAME ] } ) == 0
      AAdd( t_aReport[ FONTS ], { t_aReport[ FONTNAME ], ++t_aReport[ NEXTOBJ ] } )
   ENDIF

   RETURN NIL

// ---
PROCEDURE pdfSetLPI( _nLpi )

   LOCAL cLpi := hb_ntos( hb_defaultValue( _nLpi, 6 ) )

   t_aReport[ LPI ] := Val( iif( cLpi $ "1;2;3;4;6;8;12;16;24;48", cLpi, "6" ) )

   pdfPageSize( t_aReport[ PAGESIZE ] )

   RETURN

// ---
FUNCTION pdfStringB( cString )
   RETURN hb_StrReplace( cString, { ;
      "(" => "\(" , ;
      ")" => "\)" } )

// ---
FUNCTION pdfTextCount( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits )
   RETURN pdfText( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits, .F. )

// ---
FUNCTION pdfText( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits, cColor, lPrint )

   LOCAL cDelim := Chr( 0 ) + Chr( 9 ) + Chr( 10 ) + Chr( 13 ) + Chr( 26 ) + Chr( 32 ) + Chr( 138 ) + Chr( 141 )
   LOCAL nI, cTemp, cToken, k, nL, nRow, nLines, nLineLen, nStart
   LOCAL lParagraph, nSpace, nNew, nTokenLen, nCRLF, nTokens, nLen

   hb_default( @nTab, -1 )
   hb_default( @cUnits, "R" )
   hb_default( @nJustify, ALIGN_JUSTIFY )
   hb_default( @lPrint, .T. )
   hb_default( @cColor, "" )

   SWITCH cUnits
   CASE "M"
      nTop := pdfM2R( nTop )
      EXIT
   CASE "R"
      nLeft := pdfX2M( pdfM2X( t_aReport[ PDFLEFT ] ) + ;
         nLeft * 100.00 / t_aReport[ REPORTWIDTH ] * ;
         ( t_aReport[ PAGEX ] - pdfM2X( t_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00 )
      EXIT
   ENDSWITCH

   t_aReport[ REPORTLINE ] := nTop - 1

   nSpace := pdfLen( " " )
   nLines := 0
   nCRLF := 0

   nNew := nTab

   cString := AllTrim( cString )
   nTokens := NumToken( cString, cDelim )
   nStart := 1

   SWITCH nJustify
   CASE ALIGN_CENTER
      nLeft -= nLength / 2
      EXIT
   CASE ALIGN_RIGHT
      nLeft -= nLength
      EXIT
   ENDSWITCH

   nL := nLeft
   nL += nNew * nSpace  // first always paragraph
   nLineLen := nSpace * nNew - nSpace

   lParagraph := .T.
   nI := 1

   DO WHILE nI <= nTokens
      cToken := Token( cString, cDelim, nI )
      nTokenLen := pdfLen( cToken )
      nLen := Len( cToken )

      IF nLineLen + nSpace + nTokenLen > nLength
         IF nStart == nI  // single word > nLength
            k := 1
            DO WHILE k <= nLen
               cTemp := ""
               nLineLen := 0.00
               nL := nLeft
               IF lParagraph
                  nLineLen += nSpace * nNew
                  IF nJustify != ALIGN_CENTER
                     nL += nSpace * nNew
                  ENDIF
                  lParagraph := .F.
               ENDIF
               SWITCH nJustify
               CASE ALIGN_CENTER
                  nL := nLeft + ( nLength - pdfLen( cTemp ) ) / 2
                  EXIT
               CASE ALIGN_RIGHT
                  nL := nLeft + nLength - pdfLen( cTemp )
                  EXIT
               ENDSWITCH
               DO WHILE k <= nLen .AND. ( ( nLineLen += pdfLen( SubStr( cToken, k, 1 ) ) ) <= nLength )
                  nLineLen += pdfLen( SubStr( cToken, k, 1 ) )
                  cTemp += SubStr( cToken, k, 1 )
                  ++k
               ENDDO
               IF Empty( cTemp ) // single character > nlength
                  cTemp := SubStr( cToken, k, 1 )
                  ++k
               ENDIF
               ++nLines
               IF lPrint
                  nRow := pdfNewLine( 1 )
                  // version 0.02
                  pdfAtSay( cColor + cTemp, pdfR2M( nRow + t_aReport[ PDFTOP ] ), nL, "M" )
               ENDIF
            ENDDO
            ++nI
            nStart := nI
         ELSE
            pdfTextPrint( nI - 1, nLeft, @lParagraph, nJustify, nSpace, nNew, nLength, @nLineLen, @nLines, @nStart, cString, cDelim, cColor, lPrint )
         ENDIF
      ELSEIF nI == nTokens .OR. ( nI < nTokens .AND. ( nCRLF := pdfTextNextPara( cString, cDelim, nI ) ) > 0 )
         IF nI == nTokens
            nLineLen += nSpace + nTokenLen
         ENDIF
         pdfTextPrint( nI, nLeft, @lParagraph, nJustify, nSpace, nNew, nLength, @nLineLen, @nLines, @nStart, cString, cDelim, cColor, lPrint )
         ++nI

         IF nCRLF > 1
            nLines += nCRLF - 1
         ENDIF
         IF lPrint
            /* nRow := */ pdfNewLine( nCRLF - 1 )
         ENDIF
      ELSE
         nLineLen += nSpace + nTokenLen
         ++nI
      ENDIF
   ENDDO

   RETURN nLines

// ---
STATIC PROCEDURE pdfTextPrint( nI, nLeft, lParagraph, nJustify, nSpace, nNew, nLength, nLineLen, nLines, nStart, cString, cDelim, cColor, lPrint )

   LOCAL nL, nB, nJ, cToken, nRow
   LOCAL nFinish := nI

   nL := nLeft
   IF lParagraph .AND. nJustify != ALIGN_CENTER
      nL += nSpace * nNew
   ENDIF

   SWITCH nJustify
   CASE ALIGN_RIGHT
      nL += nLength - nLineLen
      EXIT
   CASE ALIGN_CENTER
      nL += ( nLength - nLineLen ) / 2
      EXIT
   ENDSWITCH

   ++nLines
   IF lPrint
      nRow := pdfNewLine( 1 )
   ENDIF
   nB := nSpace
   IF nJustify == ALIGN_JUSTIFY
      nB := ( nLength - nLineLen + ( nFinish - nStart ) * nSpace ) / ( nFinish - nStart )
   ENDIF
   FOR nJ := nStart TO nFinish
      cToken := Token( cString, cDelim, nJ )
      IF lPrint
         // version 0.02
         pdfAtSay( cColor + cToken, pdfR2M( nRow + t_aReport[ PDFTOP ] ), nL, "M" )
      ENDIF
      nL += pdfLen( cToken ) + nB
   NEXT

   nStart := nFinish + 1

   lParagraph := .F.

   nLineLen := 0.00
   nLineLen += nSpace * nNew

   RETURN

// ---
STATIC FUNCTION pdfTextNextPara( cString, cDelim, nI )

   LOCAL nAt, cAt, nCRLF, nNew, nRat, nRet := 0

   // check if next spaces paragraph(s)
   nAt := AtToken( cString, cDelim, nI ) + Len( Token( cString, cDelim, nI ) )
   cAt := SubStr( cString, nAt, AtToken( cString, cDelim, nI + 1 ) - nAt )
   nCRLF := NumAt( Chr( 13 ) + Chr( 10 ), cAt )
   nRat := RAt( Chr( 13 ) + Chr( 10 ), cAt )
   nNew := Len( cAt ) - nRat - iif( nRat > 0, 1, 0 )
   IF nCRLF > 1 .OR. ( nCRLF == 1 .AND. nNew > 0 )
      nRet := nCRLF
   ENDIF

   RETURN nRet

// ---
FUNCTION pdfUnderline( cString )
   RETURN cString + hb_BChar( 254 )

// ---
STATIC FUNCTION pdfX2M( n )
   RETURN n * 25.4 / 72

// ---
STATIC FUNCTION TimeAsAMPM( cTime )

   DO CASE
   CASE Val( cTime ) < 12
      cTime += " am"
   CASE Val( cTime ) == 12
      cTime += " pm"
   OTHERWISE
      cTime := Str( Val( cTime ) - 12, 2 ) + SubStr( cTime, 3 ) + " pm"
   ENDCASE
   cTime := Left( cTime, 5 ) + SubStr( cTime, 10 )

   RETURN cTime

PROCEDURE pdfOpenHeader( cFile )

   t_aReport[ HEADER ] := iif( HB_ISSTRING( cFile ), File2Array( AllTrim( cFile ) ), {} )
   t_aReport[ MARGINS ] := .T.

   RETURN

PROCEDURE pdfEditOnHeader()

   t_aReport[ HEADEREDIT ] := .T.
   t_aReport[ MARGINS ] := .T.

   RETURN

PROCEDURE pdfEditOffHeader()

   t_aReport[ HEADEREDIT ] := .F.
   t_aReport[ MARGINS ] := .T.

   RETURN

PROCEDURE pdfCloseHeader()

   t_aReport[ HEADER ] := {}
   t_aReport[ MARGINS ] := .F.

   RETURN

FUNCTION pdfDeleteHeader( cId )

   LOCAL nRet := -1, nId

   cId := Upper( cId )

   IF ( nId := AScan( t_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } ) ) > 0
      hb_ADel( t_aReport[ HEADER ], nId, .T. )
      nRet := Len( t_aReport[ HEADER ] )
      t_aReport[ MARGINS ] := .T.
   ENDIF

   RETURN nRet

PROCEDURE pdfEnableHeader( cId )

   LOCAL nId

   cId := Upper( cId )

   IF ( nId := AScan( t_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } ) ) > 0
      t_aReport[ HEADER ][ nId ][ 1 ] := .T.
      t_aReport[ MARGINS ] := .T.
   ENDIF

   RETURN

PROCEDURE pdfDisableHeader( cId )

   LOCAL nId

   cId := Upper( cId )

   IF ( nId := AScan( t_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } ) ) > 0
      t_aReport[ HEADER ][ nId ][ 1 ] := .F.
      t_aReport[ MARGINS ] := .T.
   ENDIF

   RETURN

PROCEDURE pdfSaveHeader( cFile )

   Array2File( "temp.tmp", t_aReport[ HEADER ] )
   hb_vfCopyFile( "temp.tmp", cFile )

   RETURN

FUNCTION pdfHeader( cFunction, cId, arr )

   LOCAL nId, nI, nIdLen

   hb_default( @cFunction, "" )
   hb_default( @cId, "" )

   nId := 0
   IF ! Empty( cId )
      cId := Upper( cId )
      nId := AScan( t_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } )
   ENDIF
   IF nId == 0
      IF Empty( cId )
         cId := cFunction
         nIdLen := Len( cId )
         FOR EACH nI IN t_aReport[ HEADER ]
            IF nI[ 2 ] == cId
               IF Val( SubStr( nI[ 3 ], nIdLen + 1 ) ) > nId
                  nId := Val( SubStr( nI[ 3 ], nIdLen + 1 ) )
               ENDIF
            ENDIF
         NEXT
         cId += hb_ntos( ++nId )
      ENDIF
      AAdd( t_aReport[ HEADER ], { .T., cFunction, cId } )
      FOR EACH nI IN arr
         AAdd( ATail( t_aReport[ HEADER ] ), nI )
      NEXT
   ELSE
      ASize( t_aReport[ HEADER ][ nId ], 3 )
      FOR EACH nI IN arr
         AAdd( t_aReport[ HEADER ][ nId ], nI )
      NEXT
   ENDIF

   RETURN cId

PROCEDURE pdfDrawHeader()

   LOCAL hdr, _nFont, _nSize

   IF Len( t_aReport[ HEADER ] ) > 0

      // save font
      _nFont := t_aReport[ FONTNAME ]
      _nSize := t_aReport[ FONTSIZE ]

      FOR EACH hdr IN t_aReport[ HEADER ]
         IF hdr[ 1 ]  // enabled
            SWITCH hdr[ 2 ]
            CASE "PDFATSAY"
               pdfAtSay( hdr[ 4 ], hdr[ 5 ], hdr[ 6 ], hdr[ 7 ], hdr[ 8 ], hdr[ 3 ] )
               EXIT
            CASE "PDFCENTER"
               pdfCenter( hdr[ 4 ], hdr[ 5 ], hdr[ 6 ], hdr[ 7 ], hdr[ 8 ], hdr[ 3 ] )
               EXIT
            CASE "PDFRJUST"
               pdfRJust( hdr[ 4 ], hdr[ 5 ], hdr[ 6 ], hdr[ 7 ], hdr[ 8 ], hdr[ 3 ] )
               EXIT
            CASE "PDFBOX"
               pdfBox( hdr[ 4 ], hdr[ 5 ], hdr[ 6 ], hdr[ 7 ], hdr[ 8 ], hdr[ 9 ], hdr[ 10 ], hdr[ 3 ] )
               EXIT
            CASE "PDFSETFONT"
               pdfSetFont( hdr[ 4 ], hdr[ 5 ], hdr[ 6 ], hdr[ 3 ] )
               EXIT
            CASE "PDFIMAGE"
               pdfImage( hdr[ 4 ], hdr[ 5 ], hdr[ 6 ], hdr[ 7 ], hdr[ 8 ], hdr[ 9 ], hdr[ 3 ] )
               EXIT
            ENDSWITCH
         ENDIF
      NEXT
      t_aReport[ FONTNAME ] := _nFont
      t_aReport[ FONTSIZE ] := _nSize

      IF t_aReport[ MARGINS ]
         pdfMargins()
      ENDIF

   ELSEIF t_aReport[ MARGINS ]

      t_aReport[ PDFTOP ] := 1  // top
      t_aReport[ PDFLEFT ] := 10  // left and right
      t_aReport[ PDFBOTTOM ] := t_aReport[ PAGEY ] / 72 * t_aReport[ LPI ] - 1  // bottom, default "LETTER", "P", 6

      t_aReport[ MARGINS ] := .F.

   ENDIF

   RETURN

PROCEDURE pdfMargins( nTop, nLeft, nBottom )

   LOCAL hdr, nTemp, aTemp, nHeight

   // version 0.07 begin

   t_aReport[ PDFTOP ] := hb_defaultValue( nTop, 1 )
   t_aReport[ PDFLEFT ] := hb_defaultValue( nLeft, 10 )
   // bottom, default "LETTER", "P", 6
   t_aReport[ PDFBOTTOM ] := hb_defaultValue( nBottom, t_aReport[ PAGEY ] / 72 * t_aReport[ LPI ] - 1 )

   // version 0.07 end

   FOR EACH hdr IN t_aReport[ HEADER ]
      IF hdr[ 1 ]  // enabled

         SWITCH hdr[ 2 ]
         CASE "PDFSETFONT"
            EXIT

         CASE "PDFIMAGE"
            IF hdr[ 8 ] == 0  // picture in header, first at all, not at any page yet
               aTemp := pdfImageInfo( hdr[ 4 ] )
               nHeight := aTemp[ IMAGE_HEIGHT ] / aTemp[ IMAGE_YRES ] * 25.4
               IF hdr[ 7 ] == "D"
                  nHeight := pdfM2X( nHeight )
               ENDIF
            ELSE
               nHeight := hdr[ 8 ]
            ENDIF

            SWITCH hdr[ 7 ]
            CASE "M"
               nTemp := t_aReport[ PAGEY ] / 72 * 25.4 / 2

               IF hdr[ 5 ] < nTemp
                  nTemp := ( hdr[ 5 ] + nHeight ) * t_aReport[ LPI ] / 25.4  // top
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSE
                  nTemp := hdr[ 5 ] * t_aReport[ LPI ] / 25.4  // top
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF
               EXIT

            CASE "D"
               nTemp := t_aReport[ PAGEY ] / 2

               IF hdr[ 5 ] < nTemp
                  nTemp := ( hdr[ 5 ] + nHeight ) * t_aReport[ LPI ] / 72  // top
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSE
                  nTemp := hdr[ 5 ] * t_aReport[ LPI ] / 72  // top
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF
               EXIT

            ENDSWITCH
            EXIT

         CASE "PDFBOX"

            SWITCH hdr[ 10 ]
            CASE "M"
               nTemp := t_aReport[ PAGEY ] / 72 * 25.4 / 2

               DO CASE
               CASE hdr[ 4 ] < nTemp .AND. ;
                    hdr[ 6 ] < nTemp

                  nTemp := hdr[ 6 ] * t_aReport[ LPI ] / 25.4  // top
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF

               CASE hdr[ 4 ] < nTemp .AND. ;
                    hdr[ 6 ] > nTemp

                  nTemp := ( hdr[ 4 ] + hdr[ 8 ] ) * t_aReport[ LPI ] / 25.4  // top
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF

                  nTemp := ( hdr[ 6 ] - hdr[ 8 ] ) * t_aReport[ LPI ] / 25.4  // top
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF

               CASE hdr[ 4 ] > nTemp .AND. ;
                    hdr[ 6 ] > nTemp

                  nTemp := hdr[ 4 ] * t_aReport[ LPI ] / 25.4  // top
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF

               ENDCASE
               EXIT

            CASE "D"
               nTemp := t_aReport[ PAGEY ] / 2

               DO CASE
               CASE hdr[ 4 ] < nTemp .AND. ;
                    hdr[ 6 ] < nTemp

                  nTemp := hdr[ 6 ] / t_aReport[ LPI ]  // top
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF

               CASE hdr[ 4 ] < nTemp .AND. ;
                    hdr[ 6 ] > nTemp

                  nTemp := ( hdr[ 4 ] + hdr[ 8 ] ) / t_aReport[ LPI ]  // top
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF

                  nTemp := ( hdr[ 6 ] - hdr[ 8 ] ) / t_aReport[ LPI ]  // top
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF

               CASE hdr[ 4 ] > nTemp .AND. ;
                    hdr[ 6 ] > nTemp

                  nTemp := hdr[ 4 ] / t_aReport[ LPI ]  // top
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF

               ENDCASE
               EXIT

            ENDSWITCH
            EXIT

         OTHERWISE

            SWITCH hdr[ 7 ]
            CASE "R"
               nTemp := hdr[ 5 ]  // top
               IF hdr[ 5 ] > t_aReport[ PAGEY ] / 72 * t_aReport[ LPI ] / 2
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
               EXIT
            CASE "M"
               nTemp := hdr[ 5 ] * t_aReport[ LPI ] / 25.4  // top
               IF hdr[ 5 ] > t_aReport[ PAGEY ] / 72 * 25.4 / 2
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
               EXIT
            CASE "D"
               nTemp := hdr[ 5 ] / t_aReport[ LPI ]  // top
               IF hdr[ 5 ] > t_aReport[ PAGEY ] / 2
                  IF nTemp < t_aReport[ PDFBOTTOM ]
                     t_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > t_aReport[ PDFTOP ]
                     t_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
               EXIT
            ENDSWITCH
         ENDSWITCH
      ENDIF
   NEXT

   t_aReport[ MARGINS ] := .F.

   RETURN

PROCEDURE pdfCreateHeader( _file, _size, _orient, _lpi, _width )

   LOCAL t_aReportStyle := { ;
      { 1    , 2  , 3  , 4  ,  5  ,   6    }, ;  // "Default"
      { 2.475, 4.0, 4.9, 6.4,  7.5,  64.0  }, ;  // "P6"
      { 3.3  , 5.4, 6.5, 8.6, 10.0,  85.35 }, ;  // "P8"
      { 2.475, 4.0, 4.9, 6.4,  7.5,  48.9  }, ;  // "L6"
      { 3.3  , 5.4, 6.5, 8.6, 10.0,  65.2  }, ;  // "L8"
      { 2.475, 4.0, 4.9, 6.4,  7.5,  82.0  }, ;  // "P6"
      { 3.3  , 5.4, 6.5, 8.6, 10.0, 109.35 } }   // "P8"

   LOCAL nStyle := 1, nAdd := 0.00

   hb_default( @_size, t_aReport[ PAGESIZE ] )
   hb_default( @_orient, t_aReport[ PAGEORIENT ] )
   hb_default( @_lpi, t_aReport[ LPI ] )

   HB_SYMBOL_UNUSED( _width )  /* default was set to 200 */

   SWITCH _size
   CASE "LETTER"
      SWITCH _orient
      CASE "P"
         SWITCH _lpi
         CASE 6 ; nStyle := 2 ; EXIT
         CASE 8 ; nStyle := 3 ; EXIT
         ENDSWITCH
         EXIT
      CASE "L"
         SWITCH _lpi
         CASE 6 ; nStyle := 4 ; EXIT
         CASE 8 ; nStyle := 5 ; EXIT
         ENDSWITCH
         EXIT
      ENDSWITCH
      EXIT
   CASE "LEGAL"
      SWITCH _orient
      CASE "P"
         SWITCH _lpi
         CASE 6 ; nStyle := 6 ; EXIT
         CASE 8 ; nStyle := 7 ; EXIT
         ENDSWITCH
         EXIT
      CASE "L"
         SWITCH _lpi
         CASE 6 ; nStyle := 4 ; EXIT
         CASE 8 ; nStyle := 5 ; EXIT
         ENDSWITCH
         EXIT
      ENDSWITCH
      EXIT
   ENDSWITCH

   pdfEditOnHeader()

   IF _size == "LEGAL"
      nAdd := 76.2
   ENDIF

   IF _orient == "P"
      pdfBox(  5.0, 5.0, 274.0 + nAdd, 210.0,  1.0 )
      pdfBox(  6.5, 6.5, 272.5 + nAdd, 208.5,  0.5 )

      pdfBox( 11.5, 9.5,  22.0, 205.5,  0.5, 5 )
      pdfBox( 23.0, 9.5,  33.5, 205.5,  0.5, 5 )
      pdfBox( 34.5, 9.5, 267.5 + nAdd, 205.5,  0.5 )
   ELSE
      pdfBox(  5.0, 5.0, 210.0, 274.0 + nAdd, 1.0 )
      pdfBox(  6.5, 6.5, 208.5, 272.5 + nAdd, 0.5 )

      pdfBox( 11.5, 9.5,  22.0, 269.5 + nAdd, 0.5, 5 )
      pdfBox( 23.0, 9.5,  33.5, 269.5 + nAdd, 0.5, 5 )
      pdfBox( 34.5, 9.5, 203.5, 269.5 + nAdd, 0.5 )
   ENDIF

   pdfSetFont( "Helvetica", BOLD, 10 )  // 0.04
   pdfAtSay( "Test Line 1", t_aReportStyle[ nStyle ][ 1 ], 1, "R", .T. )

   pdfSetFont( "Times", BOLD, 18 )
   pdfCenter( "Test Line 2", t_aReportStyle[ nStyle ][ 2 ],, "R", .T. )

   pdfSetFont( "Times", BOLD, 12 )
   pdfCenter( "Test Line 3", t_aReportStyle[ nStyle ][ 3 ],, "R", .T. )

   pdfSetFont( "Helvetica", BOLD, 10 ) // 0.04
   pdfAtSay( "Test Line 4", t_aReportStyle[ nStyle ][ 4 ], 1, "R", .T. )

   pdfSetFont( "Helvetica", BOLD, 10 ) // 0.04
   pdfAtSay( "Test Line 5", t_aReportStyle[ nStyle ][ 5 ], 1, "R", .T. )

   pdfAtSay( DToC( Date() ) + " " + TimeAsAMPM( Time() ), t_aReportStyle[ nStyle ][ 6 ], 1, "R", .T. )
   pdfRJust( "Page: #pagenumber#", t_aReportStyle[ nStyle ][ 6 ], t_aReport[ REPORTWIDTH ], "R", .T. )

   pdfEditOffHeader()
   pdfSaveHeader( _file )

   RETURN

FUNCTION pdfImageInfo( cFile )

   SWITCH Lower( hb_FNameExt( cFile ) )
   CASE ".tif"
   CASE ".tiff"
      RETURN pdfTIFFInfo( cFile )
   CASE ".jpg"
   CASE ".jpeg"
   CASE ".jpe"
      RETURN pdfJPEGInfo( cFile )
   ENDSWITCH

   RETURN {}

FUNCTION pdfTIFFInfo( cFile )

   LOCAL c40 := hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 )

#if 0
   LOCAL aType := { "BYTE", "ASCII", "SHORT", "LONG", "RATIONAL", "SBYTE", "UNDEFINED", "SSHORT", "SLONG", "SRATIONAL", "FLOAT", "DOUBLE" }
#endif
   LOCAL aCount := { 1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8 }
   LOCAL nTemp, cValues, c2, nFieldType, nCount, nPos, nTag
   LOCAL nOffset, cTemp, cIFDNext, nIFD, nFields, nn //, cTag, nPages

   LOCAL nWidth := 0, nHeight := 0, nBits := 0, nFrom := 0, nLength := 0, xRes := 0, yRes := 0

   LOCAL hFile := hb_vfOpen( cFile, FO_READ )

   c2 := Space( 2 )
   hb_vfRead( hFile, @c2, 2 )
   hb_vfRead( hFile, @c2, 2 )
   cIFDNext := Space( 4 )
   hb_vfRead( hFile, @cIFDNext, 4 )

   cTemp := Space( 12 )
   // nPages := 0

   DO WHILE !( cIFDNext == c40 )  // read IFD's

      nIFD := Bin2L( cIFDNext )

      hb_vfSeek( hFile, nIFD )
      // ? "*** IFD", hb_ntos( ++nPages )

      hb_vfRead( hFile, @c2, 2 )
      nFields := Bin2I( c2 )

      FOR nn := 1 TO nFields
         hb_vfRead( hFile, @cTemp, 12 )

         nTag := Bin2W( hb_BSubStr( cTemp, 1, 2 ) )
         nFieldType := Bin2W( hb_BSubStr( cTemp, 3, 2 ) )
         /*
         1 : BYTE       8-bit unsigned integer.
         2 : ASCII      8-bit byte that contains a 7-bit ASCII code; the last byte
                        must be NUL (binary zero).
         3 : SHORT      16-bit (2-byte) unsigned integer.
         4 : LONG       32-bit (4-byte) unsigned integer.
         5 : RATIONAL   Two LONGs: the first represents the numerator of a
                        fraction; the second, the denominator.

         In TIFF 6.0, some new field types have been defined:

         6 : SBYTE      An 8-bit signed (twos-complement) integer.
         7 : UNDEFINED  An 8-bit byte that may contain anything, depending on
                        the definition of the field.
         8 : SSHORT     A 16-bit (2-byte) signed (twos-complement) integer.
         9 : SLONG      A 32-bit (4-byte) signed (twos-complement) integer.
         10 : SRATIONAL Two SLONG's: the first represents the numerator of a
                        fraction, the second the denominator.
         11 : FLOAT     Single precision (4-byte) IEEE format.
         12 : DOUBLE    Double precision (8-byte) IEEE format.
         */
         nCount := Bin2L( hb_BSubStr( cTemp, 5, 4 ) )
         nOffset := Bin2L( hb_BSubStr( cTemp, 9, 4 ) )

         IF nCount > 1 .OR. nFieldType == RATIONAL .OR. nFieldType == SRATIONAL
            nPos := FilePos( hFile )
            hb_vfSeek( hFile, nOffset )

            cValues := Space( nCount * aCount[ nFieldType ] )
            hb_vfRead( hFile, @cValues, hb_BLen( cValues ) )
            hb_vfSeek( hFile, nPos )
         ELSE
            cValues := hb_BSubStr( cTemp, 9, 4 )
         ENDIF

         IF nFieldType == ASCII
            --nCount
         ENDIF
         // ? "Tag"
         // ?? " " + PadR( nTag, 10 )
         // cTag := ""
         SWITCH nTag
         CASE 256
               /*
               ImageWidth
               Tag : 256 (100.H)
               Type : SHORT or LONG
               The number of columns in the image, i.e., the number of pixels per scanline.
               */
            // ?? cTag := "ImageWidth"
#if 0
            IF nFieldType != SHORT .AND. nFieldType != LONG
               // Alert( "Wrong Type for ImageWidth" )
            ENDIF
#endif
            SWITCH nFieldType
            CASE SHORT
               nWidth := Bin2W( hb_BSubStr( cValues, 1, 2 ) )
               EXIT
            CASE LONG
               nWidth := Bin2L( hb_BSubStr( cValues, 1, 4 ) )
               EXIT
            ENDSWITCH
            EXIT

         CASE 257
               /*
               ImageLength
               Tag : 257 (101.H)
               Type : SHORT or LONG
               The number of rows (sometimes described as scanlines) in the image.
               */
            // ?? cTag := "ImageLength"
#if 0
            IF nFieldType != SHORT .AND. nFieldType != LONG
               // Alert( "Wrong Type for ImageLength" )
            ENDIF
#endif
            SWITCH nFieldType
            CASE SHORT
               nHeight := Bin2W( hb_BSubStr( cValues, 1, 2 ) )
               EXIT
            CASE LONG
               nHeight := Bin2L( hb_BSubStr( cValues, 1, 4 ) )
               EXIT
            ENDSWITCH
            EXIT

         CASE 258
               /*
               BitsPerSample
               Tag : 258 (102.H)
               Type : SHORT
               The number of bits per component.
               Allowable values for Baseline TIFF grayscale images are 4 and 8, allowing either
               16 or 256 distinct shades of gray.
               */
            // ?? cTag := "BitsPerSample"
            nTemp := 0
            IF nFieldType == SHORT
               nTemp := Bin2W( cValues )
            ELSE
               // Alert( "Wrong Type for BitsPerSample" )
            ENDIF
            nBits := nTemp
            IF nTemp != 4 .AND. nTemp != 8
               // Alert( "Wrong Value for BitsPerSample" )
            ENDIF
            EXIT

         CASE 259
               /*
               Compression
               Tag : 259 (103.H)
               Type : SHORT
               Values:
               1 : No compression, but pack data into bytes as tightly as possible, leaving no unused
               bits (except at the end of a row). The component values are stored as an array of
               type BYTE. Each scan line (row) is padded to the next BYTE boundary.
               2 : CCITT Group 3 1-Dimensional Modified Huffman run length encoding. See
               Section 10 for a description of Modified Huffman Compression.
               32773 : PackBits compression, a simple byte-oriented run length scheme. See the
               PackBits section for details.
               Data compression applies only to raster image data. All other TIFF fields are
               unaffected.
               Baseline TIFF readers must handle all three compression schemes.
               */
            // ?? cTag := "Compression"
#if 0
            nTemp := 0
            IF nFieldType == SHORT
               nTemp := Bin2W( cValues )
            ELSE
               // Alert( "Wrong Type for Compression" )
            ENDIF
            IF nTemp != 1 .AND. nTemp != 2 .AND. nTemp != 32773
               // Alert( "Wrong Value for Compression" )
            ENDIF
#endif
            EXIT

         CASE 262
               /*
               PhotometricInterpretation
               Tag : 262 (106.H)
               Type : SHORT
               Values:
               0 : WhiteIsZero. For bilevel and grayscale images: 0 is imaged as white. The maxi-mum
               value is imaged as black. This is the normal value for Compression=2.
               1 : BlackIsZero. For bilevel and grayscale images: 0 is imaged as black. The maxi-mum
               value is imaged as white. If this value is specified for Compression=2, the
               image should display and print reversed.
               */
            // ?? cTag := "PhotometricInterpretation"
            nTemp := -1
            IF nFieldType == SHORT
               nTemp := Bin2W( cValues )
            ELSE
               // Alert( "Wrong Type for PhotometricInterpretation" )
            ENDIF
            IF nTemp != 0 .AND. nTemp != 1 .AND. nTemp != 2 .AND. nTemp != 3
               // Alert( "Wrong Value for PhotometricInterpretation" )
            ENDIF
            EXIT

         CASE 264
               /*
               CellWidth
               The width of the dithering or halftoning matrix used to create a dithered or
               halftoned bilevel file.Tag : 264 (108.H)
               Type : SHORT
               N : 1
               No default. See also Threshholding.
               */
            // ?? cTag := "CellWidth"
            IF nFieldType != SHORT
               // Alert( "Wrong Type for CellWidth" )
            ENDIF
            EXIT

         CASE 265
               /*
               CellLength
               The length of the dithering or halftoning matrix used to create a dithered or
               halftoned bilevel file.
               Tag : 265 (109.H)
               Type : SHORT
               N : 1
               This field should only be present if Threshholding : 2
               No default. See also Threshholding.
               */
            // ?? cTag := "CellLength"
            IF nFieldType != SHORT
               // Alert( "Wrong Type for CellLength" )
            ENDIF
            EXIT

         CASE 266
               /*
               FillOrder
               The logical order of bits within a byte.
               Tag : 266 (10A.H)
               Type : SHORT
               N : 1
               */
            // ?? cTag := "FillOrder"
            IF nFieldType != SHORT
               // Alert( "Wrong Type for FillOrder" )
            ENDIF
            EXIT

         CASE 273
               /*
               StripOffsets
               Tag : 273 (111.H)
               Type : SHORT or LONG
               For each strip, the byte offset of that strip.
               */
            // ?? cTag := "StripOffsets"
            IF nFieldType != SHORT .AND. nFieldType != LONG
               // Alert( "Wrong Type for StripOffsets" )
            ENDIF

            SWITCH nFieldType
            CASE SHORT
               nFrom := Bin2W( hb_BSubStr( cValues, 1, 2 ) )
               EXIT
            CASE LONG
               nFrom := Bin2L( hb_BSubStr( cValues, 1, 4 ) )
               EXIT
            ENDSWITCH
            EXIT

         CASE 277
               /*
               SamplesPerPixel
               Tag : 277 (115.H)
               Type : SHORT
               The number of components per pixel. This number is 3 for RGB images, unless
               extra samples are present. See the ExtraSamples field for further information.
               */
            // ?? cTag := "SamplesPerPixel"
            IF nFieldType != SHORT
               // Alert( "Wrong Type for SamplesPerPixel" )
            ENDIF
            EXIT

         CASE 278
               /*
               RowsPerStrip
               Tag : 278 (116.H)
               Type : SHORT or LONG
               The number of rows in each strip (except possibly the last strip.)
               For example, if ImageLength is 24, and RowsPerStrip is 10, then there are 3
               strips, with 10 rows in the first strip, 10 rows in the second strip, and 4 rows in the
               third strip. (The data in the last strip is not padded with 6 extra rows of dummy
               data.)
               */
            // ?? cTag := "RowsPerStrip"
            IF nFieldType != SHORT .AND. nFieldType != LONG
               // Alert( "Wrong Type for RowsPerStrip" )
            ENDIF
            EXIT

         CASE 279
               /*
               StripByteCounts
               Tag : 279 (117.H)
               Type : SHORT or LONG
               For each strip, the number of bytes in that strip after any compression.
               */
            // ?? cTag := "StripByteCounts"
            IF nFieldType != SHORT .AND. nFieldType != LONG
               // Alert( "Wrong Type for StripByteCounts" )
            ENDIF

            SWITCH nFieldType
            CASE SHORT
               nLength := Bin2W( hb_BSubStr( cValues, 1, 2 ) )
               EXIT
            CASE LONG
               nLength := Bin2L( hb_BSubStr( cValues, 1, 4 ) )
               EXIT
            ENDSWITCH

            nLength *= nCount  // Count all strips !!!
            EXIT

         CASE 282
               /*
               XResolution
               Tag : 282 (11A.H)
               Type : RATIONAL
               The number of pixels per ResolutionUnit in the ImageWidth (typically, horizontal
               - see Orientation) direction.
               */
            // ?? cTag := "XResolution"
            IF nFieldType != RATIONAL
               // Alert( "Wrong Type for XResolution" )
            ENDIF
            xRes := Bin2L( hb_BSubStr( cValues, 1, 4 ) )
            EXIT

         CASE 283
               /*
               YResolution
               Tag : 283 (11B.H)
               Type : RATIONAL
               The number of pixels per ResolutionUnit in the ImageLength (typically, vertical)
               direction.
               */
            // ?? cTag := "YResolution"
            IF nFieldType != RATIONAL
               // Alert( "Wrong Type for YResolution" )
            ENDIF
            yRes := Bin2L( hb_BSubStr( cValues, 1, 4 ) )
            EXIT

         CASE 284
            // ?? cTag := "PlanarConfiguration"
            IF nFieldType != SHORT
               // Alert( "Wrong Type for PlanarConfiguration" )
            ENDIF
            EXIT

         CASE 288
               /*
               FreeOffsets
               For each string of contiguous unused bytes in a TIFF file, the byte offset of the
               string.
               Tag : 288 (120.H)
               Type : LONG
               Not recommended for general interchange.
               See also FreeByteCounts.
               */
            // ?? cTag := "FreeOffsets"
            IF nFieldType != LONG
               // Alert( "Wrong Type for FreeOffsets" )
            ENDIF
            EXIT

         CASE 289
               /*
               FreeByteCounts
               For each string of contiguous unused bytes in a TIFF file, the number of bytes in
               the string.
               Tag : 289 (121.H)
               Type : LONG
               Not recommended for general interchange.
               See also FreeOffsets.
               */
            // ?? cTag := "FreeByteCounts"
            IF nFieldType != LONG
               // Alert( "Wrong Type for FreeByteCounts" )
            ENDIF
            EXIT

         CASE 296
               /*
               ResolutionUnit
               Tag : 296 (128.H)
               Type : SHORT
               Values:
               1 : No absolute unit of measurement. Used for images that may have a non-square
               aspect ratio but no meaningful absolute dimensions.
               2 : Inch.
               3 : Centimeter.
               Default : 2 (inch).
               */
            // ?? cTag := "ResolutionUnit"
            nTemp := 0
            IF nFieldType == SHORT
               nTemp := Bin2W( cValues )
            ELSE
               // Alert( "Wrong Type for ResolutionUnit" )
            ENDIF
            IF nTemp != 1 .AND. nTemp != 2 .AND. nTemp != 3
               // Alert( "Wrong Value for ResolutionUnit" )
            ENDIF
            EXIT

         CASE 305
            // ?? cTag := "Software"
            IF nFieldType != ASCII
               // Alert( "Wrong Type for Software" )
            ENDIF
            EXIT

         CASE 306
               /*
               DateTime
               Date and time of image creation.
               Tag : 306 (132.H)
               Type : ASCII
               N : 2 0
               The format is: YYYY:MM:DD HH:MM:SS, with hours like those on a 24-hour
               clock, and one space character between the date and the time. The length of the
               string, including the terminating NUL, is 20 bytes.
               */
            // ?? cTag := "DateTime"
            IF nFieldType != ASCII
               // Alert( "Wrong Type for DateTime" )
            ENDIF
            EXIT

         CASE 315
               /*
               Artist
               Person who created the image.
               Tag : 315 (13B.H)
               Type : ASCII
               Note: some older TIFF files used this tag for storing Copyright information.
               */
            // ?? cTag := "Artist"
            IF nFieldType != ASCII
               // Alert( "Wrong Type for Artist" )
            ENDIF
            EXIT

         CASE 320
               /*
               ColorMap
               Tag : 320 (140.H)
               Type : SHORT
               N : 3 * (2**BitsPerSample)
               This field defines a Red-Green-Blue color map (often called a lookup table) for
               palette color images. In a palette-color image, a pixel value is used to index into an
               RGB-lookup table. For example, a palette-color pixel having a value of 0 would
               be displayed according to the 0th Red, Green, Blue triplet.
               In a TIFF ColorMap, all the Red values come first, followed by the Green values,
               then the Blue values. In the ColorMap, black is represented by 0,0,0 and white is
               represented by 65535, 65535, 65535.
               */
            // ?? cTag := "ColorMap"
            IF nFieldType != SHORT
               // Alert( "Wrong Type for ColorMap" )
            ENDIF
            EXIT

         CASE 338
               /*
               ExtraSamples
               Description of extra components.
               Tag : 338 (152.H)
               Type : SHORT
               N : m
               */
            // ?? cTag := "ExtraSamples"
            IF nFieldType != SHORT
               // Alert( "Wrong Type for ExtraSamples" )
            ENDIF
            EXIT

         CASE 33432
               /*
               Copyright
               Copyright notice.
               Tag : 33432 (8298.H)
               Type : ASCII
               Copyright notice of the person or organization that claims the copyright to the
               image. The complete copyright statement should be listed in this field including
               any dates and statements of claims. For example, Copyright, John Smith, 19xx.
               All rights reserved.
               */
            // ?? cTag := "Copyright"
            IF nFieldType != ASCII
               // Alert( "Wrong Type for Copyright" )
            ENDIF
            EXIT

         OTHERWISE
            // ?? cTag := "Unknown"
         ENDSWITCH
#if 0
         ?? PadR( cTag, 30 )
         ?? " type", PadR( aType[ nFieldType ], 10 ), "count", hb_ntos( nCount ), "<"

         SWITCH nFieldType
         CASE BYTE
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( hb_BCode( hb_BSubStr( cValues, nI, 1 ) ) )
            NEXT
            EXIT
         CASE ASCII
            ?? " "
            FOR nI := 1 TO nCount
               ?? hb_BSubStr( cValues, nI, 1 )
            NEXT
            EXIT
         CASE SHORT
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( Bin2W( hb_BSubStr( cValues, ( nI - 1 ) * 2 + 1, 2 ) ) )
            NEXT
            EXIT
         CASE LONG
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( Bin2L( hb_BSubStr( cValues, ( nI - 1 ) * 4 + 1, 4 ) ) )
            NEXT
            EXIT
         CASE RATIONAL
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( Bin2L( hb_BSubStr( cValues, ( nI - 1 ) * 8 + 1, 4 ) ) ) + "/" + hb_ntos( Bin2L( hb_BSubStr( cValues, nI + 4, 4 ) ) )
            NEXT
            EXIT
         CASE SBYTE
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( hb_BCode( hb_BSubStr( cValues, nI, 1 ) ) )
            NEXT
            EXIT
         CASE UNDEFINED
            FOR nI := 1 TO nCount
               ?? " " + hb_BSubStr( cValues, nI, 1 )
            NEXT
            EXIT
         CASE SSHORT
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( Bin2I( hb_BSubStr( cValues, ( nI - 1 ) * 2 + 1, 2 ) ) )
            NEXT
            EXIT
         CASE SLONG
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( Bin2L( hb_BSubStr( cValues, ( nI - 1 ) * 4 + 1, 4 ) ) )
            NEXT
            EXIT
         CASE SRATIONAL
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( Bin2L( hb_BSubStr( cValues, ( nI - 1 ) * 8 + 1, 4 ) ) ) + "/" + hb_ntos( Bin2L( hb_BSubStr( cValues, nI + 4, 4 ) ) )
            NEXT
            EXIT
         CASE FLOAT
            EXIT
         CASE DOUBLE
            FOR nI := 1 TO nCount
               ?? " " + hb_ntos( CToF( hb_BSubStr( cValues, ( nI - 1 ) * 8 + 1, 8 ) ) )
            NEXT
            EXIT
         ENDSWITCH
         ?? " >"
#endif
      NEXT
      hb_vfRead( hFile, @cIFDNext, 4 )
   ENDDO

   hb_vfClose( hFile )

   RETURN { ;
      nWidth, ;
      nHeight, ;
      xRes, ;
      yRes, ;
      nBits, ;
      nFrom, ;
      nLength, ;
      0 /* IMAGE_SPACE */ }

FUNCTION pdfJPEGInfo( cFile )

   LOCAL c255, nAt
   LOCAL nWidth, nHeight, nBits := 8, nFrom := 0, nLength, xRes, yRes
   LOCAL nSpace  // := 3 // 3 - RGB, 1 - GREY, 4 - CMYK

   LOCAL hFile := hb_vfOpen( cFile, FO_READ )

   c255 := Space( 20000 )
   hb_vfRead( hFile, @c255, hb_BLen( c255 ) )

   xRes := hb_BCode( hb_BSubStr( c255, 15, 1 ) ) * 256 + hb_BCode( hb_BSubStr( c255, 16, 1 ) )
   yRes := hb_BCode( hb_BSubStr( c255, 17, 1 ) ) * 256 + hb_BCode( hb_BSubStr( c255, 18, 1 ) )

   IF ( nAt := hb_BRAt( hb_BChar( 255 ) + hb_BChar( 0xC0 ), c255 ) ) == 0
      nAt := hb_BRAt( hb_BChar( 255 ) + hb_BChar( 0xC2 ), c255 )
   ENDIF
   nAt += 5
   nHeight := hb_BCode( hb_BSubStr( c255, nAt, 1 ) ) * 256 + hb_BCode( hb_BSubStr( c255, nAt + 1, 1 ) )
   nWidth := hb_BCode( hb_BSubStr( c255, nAt + 2, 1 ) ) * 256 + hb_BCode( hb_BSubStr( c255, nAt + 3, 1 ) )

   nSpace := hb_BCode( hb_BSubStr( c255, nAt + 4, 1 ) )

   nLength := FileSize( hFile )

   hb_vfClose( hFile )

   RETURN { ;
      nWidth, ;
      nHeight, ;
      xRes, ;
      yRes, ;
      nBits, ;
      nFrom, ;
      nLength, ;
      nSpace }

STATIC FUNCTION FilePos( hFile )
   RETURN hb_vfSeek( hFile, 0, FS_RELATIVE )

STATIC FUNCTION Chr_RGB( cChar )
   RETURN Str( hb_BCode( cChar ) / 255, 4, 2 )

STATIC FUNCTION NumToken( cString, cDelimiter )
   RETURN AllToken( cString, cDelimiter )

STATIC FUNCTION Token( cString, cDelimiter, nPointer )
   RETURN AllToken( cString, cDelimiter, nPointer, 1 )

STATIC FUNCTION AtToken( cString, cDelimiter, nPointer )
   RETURN AllToken( cString, cDelimiter, nPointer, 2 )

STATIC FUNCTION AllToken( cString, cDelimiter, nPointer, nAction )

   LOCAL nTokens := 0, nPos := 1, nLen := Len( cString ), nStart, cRet

   hb_default( @cDelimiter, Chr( 0 ) + Chr( 9 ) + Chr( 10 ) + Chr( 13 ) + Chr( 26 ) + Chr( 32 ) + Chr( 138 ) + Chr( 141 ) )
   hb_default( @nAction, 0 )

   // nAction == 0 - numtoken
   // nAction == 1 - token
   // nAction == 2 - attoken

   DO WHILE nPos <= nLen
      IF ! SubStr( cString, nPos, 1 ) $ cDelimiter
         nStart := nPos
         DO WHILE nPos <= nLen .AND. ! SubStr( cString, nPos, 1 ) $ cDelimiter
            ++nPos
         ENDDO
         ++nTokens
         IF nAction > 0
            IF nPointer == nTokens
               IF nAction == 1
                  cRet := SubStr( cString, nStart, nPos - nStart )
               ELSE
                  cRet := nStart
               ENDIF
               EXIT
            ENDIF
         ENDIF
      ENDIF
      IF SubStr( cString, nPos, 1 ) $ cDelimiter
         DO WHILE nPos <= nLen .AND. SubStr( cString, nPos, 1 ) $ cDelimiter
            ++nPos
         ENDDO
      ENDIF
      cRet := nTokens
   ENDDO

   RETURN cRet

STATIC FUNCTION NumAt( cSearch, cString )

   LOCAL n := 0, nAt, nPos := 0

   DO WHILE ( nAt := At( cSearch, SubStr( cString, nPos + 1 ) ) ) > 0
      nPos += nAt
      ++n
   ENDDO

   RETURN n

STATIC FUNCTION FileSize( hFile )

   LOCAL nCurrent := FilePos( hFile )   // Get file position
   LOCAL nLength := hb_vfSize( hFile )  // Get file length

   // Reset file position
   hb_vfSeek( hFile, nCurrent )

   RETURN nLength

// written by Peter Kulek
// modified DATE processing by V.K.
STATIC FUNCTION Array2File( cFile, aRay, nDepth, hFile )  /* TODO: replace with hb_Serialize() */

   LOCAL nBytes := 0
   LOCAL i

   IF PCount() < 4
      IF ( hFile := hb_vfOpen( cFile, FO_CREAT + FO_TRUNC + FO_WRITE ) ) == NIL
         RETURN nBytes
      ENDIF
   ENDIF

   hb_default( @nDepth, 0 )
   nDepth++
   nBytes += WriteData( hFile, aRay )
   IF HB_ISARRAY( aRay )
      FOR EACH i IN aRay
         nBytes += Array2File( cFile, i, nDepth, hFile )
      NEXT
   ENDIF
   nDepth--
   IF nDepth == 0
      hb_vfClose( hFile )
   ENDIF

   RETURN nBytes

// written by Peter Kulek
STATIC FUNCTION WriteData( hFile, xData )

   LOCAL cData

   SWITCH cData := ValType( xData )
   CASE "C"  ; cData += I2Bin( hb_BLen( xData ) ) + xData ; EXIT
   CASE "N"  ; cData += I2Bin( hb_BLen( hb_ntos( xData ) ) ) + hb_ntos( xData ) ; EXIT
   CASE "D"  ; cData += I2Bin( 8 ) + DToS( xData ) ; EXIT
   CASE "L"  ; cData += I2Bin( 1 ) + iif( xData, "T", "F" ) ; EXIT
   CASE "A"  ; cData += I2Bin( Len( xData ) ) ; EXIT
   OTHERWISE ; cData += I2Bin( 0 )  // NIL
   ENDSWITCH

   RETURN Max( hb_vfWrite( hFile, cData ), 0 )

// written by Peter Kulek
STATIC FUNCTION File2Array( cFile, nLen, hFile )  /* TODO: replace with hb_Deserialize() */

   LOCAL cData, cType, nDataLen, nBytes
   LOCAL nDepth := 0
   LOCAL aRay   := {}

   IF PCount() < 3
      IF ( hFile := hb_vfOpen( cFile, FO_READ ) ) == NIL
         RETURN aRay
      ENDIF
      cData := Space( 3 )
      hb_vfRead( hFile, @cData, 3 )
      IF ! hb_LeftEq( cData, "A" )
         RETURN aRay
      ENDIF
      nLen := Bin2I( hb_BRight( cData, 2 ) )
   ENDIF
   DO WHILE nDepth < nLen
      cData  := Space( 3 )
      nBytes := hb_vfRead( hFile, @cData, 3 )
      IF nBytes < 3
         EXIT
      ENDIF
      cType := hb_BLeft( cData, 1 )
      nDataLen := Bin2I( hb_BRight( cData, 2 ) )
      IF !( cType == "A" )
         cData := Space( nDataLen )
         nBytes := hb_vfRead( hFile, @cData, nDataLen )
         IF nBytes < nDataLen
            EXIT
         ENDIF
      ENDIF
      nDepth++
      AAdd( aRay, NIL )
      SWITCH cType
      CASE "C" ; aRay[ nDepth ] := cData ; EXIT
      CASE "N" ; aRay[ nDepth ] := Val( cData ) ; EXIT
      CASE "D" ; aRay[ nDepth ] := hb_SToD( cData ) ; EXIT
      CASE "L" ; aRay[ nDepth ] := ( cData == "T" ) ; EXIT
      CASE "A" ; aRay[ nDepth ] := File2Array( , nDataLen, hFile ) ; EXIT
      ENDSWITCH
   ENDDO
   IF cFile != NIL
      hb_vfClose( hFile )
   ENDIF

   RETURN aRay

STATIC FUNCTION __pdf_fontsdat()
#pragma __streaminclude "fonts.dat" | RETURN %s
