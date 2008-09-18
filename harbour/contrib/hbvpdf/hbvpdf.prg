/*
 * $Id$
 */

#include "hbvpdf.ch"

THREAD STATIC s_aReport
                                                                              /*
===========================================================                 */
function pdfInit()                                                            /*
=============================================================                 */

s_aReport := array( PARAMLEN )

return s_aReport
                                                                              /*              
=============================================================                 */
function pdfWidth( _nWidth )                                                  /*
=============================================================                 */

s_aReport[ REPORTWIDTH ] := _nWidth

return nil
                                                                              /*
=============================================================                 */
function pdfTextWidth( cStr )                                                 /*
=============================================================                 */

return pdfLen( cStr ) / 25.4
                                                                              /*
=============================================================                 */
function pdfAtSay( cString, nRow, nCol, cUnits, lExact, cId )                 /*
=============================================================                 */
local _nFont, lReverse, nAt

DEFAULT nRow to s_aReport[ REPORTLINE ]
DEFAULT cUnits to "R"
DEFAULT lExact to .f.
DEFAULT cId to ""

   IF s_aReport[ HEADEREDIT ]
      return pdfHeader( "PDFATSAY", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   IF ( nAt := at( "#pagenumber#", cString ) ) > 0
      cString := left( cString, nAt - 1 ) + ltrim(str( pdfPageNumber())) + substr( cString, nAt + 12 )
   ENDIF

   lReverse := .f.
   IF cUnits == "M"
      nRow := pdfM2Y( nRow )
      nCol := pdfM2X( nCol )
   ELSEIF cUnits == "R"
      IF !lExact
         pdfCheckLine( nRow )
         nRow := nRow + s_aReport[ PDFTOP ]
      ENDIF
      nRow := pdfR2D( nRow )
      nCol := pdfM2X( s_aReport[ PDFLEFT ] ) + ;
              nCol * 100.00 / s_aReport[ REPORTWIDTH ] * ;
              ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
   ENDIF
   IF !empty( cString )
      cString := pdfStringB( cString )
      IF right( cString, 1 ) == chr(255) //reverse
         cString := left( cString, len( cString ) - 1 )
         pdfBox( s_aReport[ PAGEY ] - nRow - s_aReport[ FONTSIZE ] + 2.0 , nCol, s_aReport[ PAGEY ] - nRow + 2.0, nCol + pdfM2X( pdfLen( cString )) + 1,,100, "D")
         s_aReport[ PAGEBUFFER ] += " 1 g "
         lReverse := .t.
      ELSEIF right( cString, 1 ) == chr(254) //underline
         cString := left( cString, len( cString ) - 1 )
         pdfBox( s_aReport[ PAGEY ] - nRow + 0.5,  nCol, s_aReport[ PAGEY ] - nRow + 1, nCol + pdfM2X( pdfLen( cString )) + 1,,100, "D")
      ENDIF

      // version 0.01
      IF ( nAt := at( chr(253), cString )) > 0 // some color text inside
         s_aReport[ PAGEBUFFER ] += CRLF + ;
         Chr_RGB( substr( cString, nAt + 1, 1 )) + " " + ;
         Chr_RGB( substr( cString, nAt + 2, 1 )) + " " + ;
         Chr_RGB( substr( cString, nAt + 3, 1 )) + " rg "
         cString := stuff( cString, nAt, 4, "")
      ENDIF
      // version 0.01

      _nFont := ascan( s_aReport[ FONTS ], {|arr| arr[1] == s_aReport[ FONTNAME ]} )
      IF !( s_aReport[ FONTNAME ] == s_aReport[ FONTNAMEPREV ] )
         s_aReport[ FONTNAMEPREV ] := s_aReport[ FONTNAME ]
         s_aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + ltrim(str( _nFont )) + " " + ltrim(transform( s_aReport[ FONTSIZE ], "999.99")) + " Tf " + ltrim(transform( nCol, "9999.99" )) + " " + ltrim(transform( nRow, "9999.99" )) + " Td (" + cString + ") Tj ET"
      ELSEIF s_aReport[ FONTSIZE ] != s_aReport[ FONTSIZEPREV ]
         s_aReport[ FONTSIZEPREV ] := s_aReport[ FONTSIZE ]
         s_aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + ltrim(str( _nFont )) + " " + ltrim(transform( s_aReport[ FONTSIZE ], "999.99")) + " Tf " + ltrim(transform( nCol, "9999.99" )) + " " + ltrim(transform( nRow, "9999.99" )) + " Td (" + cString + ") Tj ET"
      ELSE
         s_aReport[ PAGEBUFFER ] += CRLF + "BT " + ltrim(transform( nCol, "9999.99" )) + " " + ltrim(transform( nRow, "9999.99" )) + " Td (" + cString + ") Tj ET"
      ENDIF
      IF lReverse
         s_aReport[ PAGEBUFFER ] += " 0 g "
      ENDIF
   ENDIF
return nil
                                                                              /*
==================                                                            */
function pdfBold()                                                            /*
==================                                                            */
   IF pdfGetFontInfo("NAME") = "Times"
      s_aReport[ FONTNAME ] := 2
   ELSEIF pdfGetFontInfo("NAME") = "Helvetica"
      s_aReport[ FONTNAME ] := 6
   ELSE
      s_aReport[ FONTNAME ] := 10 // Courier // 0.04
   ENDIF
   aadd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF ascan( s_aReport[ FONTS ], { |arr| arr[1] == s_aReport[ FONTNAME ] } ) == 0
      aadd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF
return nil
                                                                              /*
========================                                                      */
function pdfBoldItalic()                                                      /*
========================                                                      */
   IF pdfGetFontInfo("NAME") = "Times"
      s_aReport[ FONTNAME ] := 4
   ELSEIF pdfGetFontInfo("NAME") = "Helvetica"
      s_aReport[ FONTNAME ] := 8
   ELSE
      s_aReport[ FONTNAME ] := 12 // 0.04
   ENDIF
   aadd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF ascan( s_aReport[ FONTS ], { |arr| arr[1] == s_aReport[ FONTNAME ] } ) == 0
      aadd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF
return nil
                                                                              /*
===================================================                           */
function pdfBookAdd( cTitle, nLevel, nPage, nLine )                           /*
===================================================                           */
   aadd( s_aReport[ BOOKMARK ], { nLevel, alltrim( cTitle ), 0, 0, 0, 0, 0, 0, nPage, IIF( nLevel == 1, s_aReport[ PAGEY ], s_aReport[ PAGEY ] - nLine * 72 / s_aReport[ LPI ] ) })
return Nil
                                                                              /*
========================                                                      */
function pdfBookClose( )                                                      /*
========================                                                      */
   s_aReport[ BOOKMARK ] := nil
return Nil
                                                                              /*
=================================================                             */
static function pdfBookCount( nRecno, nCurLevel )                             /*
=================================================                             */
local nTempLevel := 0, nCount := 0, nLen := len( s_aReport[ BOOKMARK ] )
   ++nRecno
   while nRecno <= nLen
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nTempLevel <= nCurLevel
         exit
      ELSE
         IF nCurLevel + 1 == nTempLevel
            ++nCount
         ENDIF
      ENDIF
      ++nRecno
   enddo
return -1 * nCount
                                                                              /*
=======================================================                       */
static function pdfBookFirst( nRecno, nCurLevel, nObj )                       /*
=======================================================                       */
local nFirst := 0, nLen := len( s_aReport[ BOOKMARK ] )
   ++nRecno
   IF nRecno <= nLen
      IF nCurLevel + 1 == s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         nFirst := nRecno
      ENDIF
   ENDIF
return IIF( nFirst == 0, nFirst, nObj + nFirst )
                                                                              /*
======================================================                        */
static function pdfBookLast( nRecno, nCurLevel, nObj )                        /*
======================================================                        */
local nLast := 0, nLen := len( s_aReport[ BOOKMARK ] )
   ++nRecno
   IF nRecno <= nLen
      IF nCurLevel + 1 == s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         while nRecno <= nLen .and. nCurLevel + 1 <= s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
            IF nCurLevel + 1 == s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
               nLast := nRecno
            ENDIF
            ++nRecno
         enddo
      ENDIF
   ENDIF
return IIF( nLast == 0, nLast, nObj + nLast )
                                                                              /*
======================================================                        */
static function pdfBookNext( nRecno, nCurLevel, nObj )                        /*
======================================================                        */
local nTempLevel := 0, nNext := 0, nLen := len( s_aReport[ BOOKMARK ] )
   ++nRecno
   while nRecno <= nLen
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nCurLevel > nTempLevel
         exit
      ELSEIF nCurLevel == nTempLevel
         nNext := nRecno
         exit
      ELSE
         // keep going
      ENDIF
      ++nRecno
   enddo
return IIF( nNext == 0, nNext, nObj + nNext )
                                                                              /*
=======================                                                       */
function pdfBookOpen( )                                                       /*
=======================                                                       */
   s_aReport[ BOOKMARK ] := {}
return Nil
                                                                              /*
========================================================                      */
static function pdfBookParent( nRecno, nCurLevel, nObj )                      /*
========================================================                      */
local nTempLevel := 0
local nParent := 0
   --nRecno
   while nRecno > 0
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nTempLevel < nCurLevel
         nParent := nRecno
         exit
      ENDIF
      --nRecno
   enddo
return IIF( nParent == 0, nObj - 1, nObj + nParent )
                                                                              /*
======================================================                        */
static function pdfBookPrev( nRecno, nCurLevel, nObj )                        /*
======================================================                        */
local nTempLevel := 0
local nPrev := 0
   --nRecno
   while nRecno > 0
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nCurLevel > nTempLevel
         exit
      ELSEIF nCurLevel == nTempLevel
         nPrev := nRecno
         exit
      ELSE
         // keep going
      ENDIF
      --nRecno
   enddo
return IIF( nPrev == 0, nPrev, nObj + nPrev )
                                                                              /*
===============================================================               */
function pdfBox( x1, y1, x2, y2, nBorder, nShade, cUnits, cColor, cId )       /*
===============================================================               */
local cBoxColor
DEFAULT nBorder to 0
DEFAULT nShade to 0
DEFAULT cUnits to "M"
DEFAULT cColor to ""

   // version 0.02
   cBoxColor := ""
   IF !empty( cColor )
      cBoxColor := " " + Chr_RGB( substr( cColor, 2, 1 )) + " " + ;
                         Chr_RGB( substr( cColor, 3, 1 )) + " " + ;
                         Chr_RGB( substr( cColor, 4, 1 )) + " rg "
      IF empty( alltrim( cBoxColor ) )
         cBoxColor := ""
      ENDIF
   ENDIF
   // version 0.02

   IF s_aReport[ HEADEREDIT ]
      return pdfHeader( "PDFBOX", cId, { x1, y1, x2, y2, nBorder, nShade, cUnits } )
   ENDIF

   IF cUnits == "M"
      y1 += 0.5
      y2 += 0.5

      IF nShade > 0
         // version 0.02
         s_aReport[ PAGEBUFFER ] += CRLF + transform( 1.00 - nShade / 100.00, "9.99") + " g " + cBoxColor + ltrim(str(pdfM2X( y1 ))) + " " + ltrim(str(pdfM2Y( x1 ))) + " " + ltrim(str(pdfM2X( y2 - y1 ))) + " -" + ltrim(str(pdfM2X( x2 - x1 ))) + " re f 0 g"
      ENDIF

      IF nBorder > 0
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str(pdfM2X( y1 ))) + " " + ltrim(str(pdfM2Y( x1 ))) + " " + ltrim(str(pdfM2X( y2 - y1 ))) + " -" + ltrim(str(pdfM2X( nBorder ))) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str(pdfM2X( y2 - nBorder ))) + " " + ltrim(str(pdfM2Y( x1 ))) + " " + ltrim(str(pdfM2X( nBorder ))) + " -" + ltrim(str(pdfM2X( x2 - x1 ))) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str(pdfM2X( y1 ))) + " " + ltrim(str(pdfM2Y( x2 - nBorder ))) + " " + ltrim(str(pdfM2X( y2 - y1 ))) + " -" + ltrim(str(pdfM2X( nBorder ))) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str(pdfM2X( y1 ))) + " " + ltrim(str(pdfM2Y( x1 ))) + " " + ltrim(str(pdfM2X( nBorder ))) + " -" + ltrim(str(pdfM2X( x2 - x1 ))) + " re f"
      ENDIF
   ELSEIF cUnits == "D"// "Dots"
      //x1, y1, x2, y2 - nTop, nLeft, nBottom, nRight
      IF nShade > 0
         // version 0.02
         s_aReport[ PAGEBUFFER ] += CRLF + transform( 1.00 - nShade / 100.00, "9.99") + " g " + cBoxColor + ltrim(str( y1 )) + " " + ltrim(str( s_aReport[ PAGEY ] - x1 )) + " " + ltrim(str( y2 - y1 )) + " -" + ltrim(str( x2 - x1 )) + " re f 0 g"
      ENDIF

      IF nBorder > 0
/*
            1
         ÚÄÄÄÄÄ¿
       4 ³     ³ 2
         ÀÄÄÄÄÄÙ
            3
*/
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str( y1 )) + " " + ltrim(str( s_aReport[ PAGEY ] - x1 )) + " " + ltrim(str( y2 - y1 )) + " -" + ltrim(str( nBorder )) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str( y2 - nBorder )) + " " + ltrim(str( s_aReport[ PAGEY ] - x1 )) + " " + ltrim(str( nBorder )) + " -" + ltrim(str( x2 - x1 )) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str( y1 )) + " " + ltrim(str( s_aReport[ PAGEY ] - x2 + nBorder )) + " " + ltrim(str( y2 - y1 )) + " -" + ltrim(str( nBorder )) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + ltrim(str( y1 )) + " " + ltrim(str( s_aReport[ PAGEY ] - x1 )) + " " + ltrim(str( nBorder )) + " -" + ltrim(str( x2 - x1 )) + " re f"
      ENDIF
   ENDIF

return nil

                                                                                              /*
===============================================================                               */
function pdfBox1( nTop, nLeft, nBottom, nRight, nBorderWidth, cBorderColor, cBoxColor )       /*
===============================================================                               */
DEFAULT nBorderWidth to 0.5
DEFAULT cBorderColor to chr(0) + chr(0) + chr(0)
DEFAULT cBoxColor to chr(255) + chr(255) + chr(255)

   s_aReport[ PAGEBUFFER ] +=  CRLF + ;
                         Chr_RGB( substr( cBorderColor, 1, 1 )) + " " + ;
                         Chr_RGB( substr( cBorderColor, 2, 1 )) + " " + ;
                         Chr_RGB( substr( cBorderColor, 3, 1 )) + ;
                         " RG" + ;
                         CRLF + ;
                         Chr_RGB( substr( cBoxColor, 1, 1 )) + " " + ;
                         Chr_RGB( substr( cBoxColor, 2, 1 )) + " " + ;
                         Chr_RGB( substr( cBoxColor, 3, 1 )) + ;
                         " rg" + ;
                         CRLF + ltrim(str( nBorderWidth )) + " w" + ;
                         CRLF + ltrim( str ( nLeft + nBorderWidth / 2 )) + " " + ;
                         CRLF + ltrim( str ( s_aReport[ PAGEY ] - nBottom + nBorderWidth / 2)) + " " + ;
                         CRLF + ltrim( str ( nRight - nLeft -  nBorderWidth )) + ;
                         CRLF + ltrim( str ( nBottom - nTop - nBorderWidth )) + " " + ;
                         " re" + ;
                         CRLF + "B"
return nil
                                                                              /*
==============================================================                */
function pdfCenter( cString, nRow, nCol, cUnits, lExact, cId )                /*
==============================================================                */
local nLen, nAt
DEFAULT nRow to s_aReport[ REPORTLINE ]
DEFAULT cUnits to "R"
DEFAULT lExact to .f.
DEFAULT nCol to IIF( cUnits == "R", s_aReport[ REPORTWIDTH ] / 2, s_aReport[ PAGEX ] / 72 * 25.4 / 2 )

   IF s_aReport[ HEADEREDIT ]
      return pdfHeader( "PDFCENTER", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   IF ( nAt := at( "#pagenumber#", cString ) ) > 0
      cString := left( cString, nAt - 1 ) + ltrim(str( pdfPageNumber())) + substr( cString, nAt + 12 )
   ENDIF

   nLen := pdfLen( cString ) / 2
   IF cUnits == "R"
      IF !lExact
         pdfCheckLine( nRow )
         nRow := nRow + s_aReport[ PDFTOP ]
      ENDIF
   ENDIF
   pdfAtSay( cString, pdfR2M( nRow ), IIF( cUnits == "R", s_aReport[ PDFLEFT ] + ( s_aReport[ PAGEX ] / 72 * 25.4 - 2 * s_aReport[ PDFLEFT ] ) * nCol / s_aReport[ REPORTWIDTH ], nCol ) - nLen, "M", lExact )
return nil
                                                                              /*
====================================                                          */
static function pdfCheckLine( nRow )                                          /*
====================================                                          */
   IF nRow + s_aReport[ PDFTOP ] > s_aReport[ PDFBOTTOM ]
      pdfNewPage()
      nRow := s_aReport[ REPORTLINE ]
   ENDIF
   s_aReport[ REPORTLINE ] := nRow
return nil
                                                                              /*
===================                                                           */
function pdfClose()                                                           /*
===================                                                           */
local nI, cTemp, nCurLevel, nObj1, nLast, nCount, nFirst, nRecno, nBooklen

   FIELD FIRST, PREV, NEXT, LAST, COUNT, PARENT, PAGE, COORD, TITLE, LEVEL

   pdfClosePage()

   // kids
   s_aReport[ REFS ][ 2 ] := s_aReport[ DOCLEN ]
   cTemp := ;
   "1 0 obj"+CRLF+;
   "<<"+CRLF+;
   "/Type /Pages /Count " + ltrim(str(s_aReport[ REPORTPAGE ])) + CRLF +;
   "/Kids ["

   for nI := 1 to s_aReport[ REPORTPAGE ] 
      cTemp += " " + ltrim(str( s_aReport[ PAGES ][ nI ] )) + " 0 R"
   next

   cTemp += " ]" + CRLF + ;
   ">>" + CRLF + ;
   "endobj" + CRLF

   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   // info
   ++s_aReport[ REPORTOBJ ]
   aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := ltrim(str( s_aReport[ REPORTOBJ ] )) + " 0 obj" + CRLF + ;
            "<<" + CRLF + ;
            "/Producer ()" + CRLF + ;
            "/Title ()" + CRLF + ;
            "/Author ()" + CRLF + ;
            "/Creator ()" + CRLF + ;
            "/Subject ()" + CRLF + ;
            "/Keywords ()" + CRLF + ;
            "/CreationDate (D:" + str(year(date()), 4) + padl( month(date()), 2, "0") + padl( day(date()), 2, "0") + substr( time(), 1, 2 ) + substr( time(), 4, 2 ) + substr( time(), 7, 2 ) + ")" + CRLF + ;
            ">>" + CRLF + ;
            "endobj" + CRLF
   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   // root
   ++s_aReport[ REPORTOBJ ]
   aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := ltrim(str( s_aReport[ REPORTOBJ ] )) + " 0 obj" + CRLF + ;
   "<< /Type /Catalog /Pages 1 0 R /Outlines " + ltrim(str( s_aReport[ REPORTOBJ ] + 1 )) + " 0 R" + IIF( ( nBookLen := len( s_aReport[ BOOKMARK ] )) > 0, " /PageMode /UseOutlines", "") + " >>" + CRLF + "endobj" + CRLF
   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   ++s_aReport[ REPORTOBJ ]
   nObj1 := s_aReport[ REPORTOBJ ]

   IF nBookLen > 0

      nRecno := 1
      nFirst := s_aReport[ REPORTOBJ ] + 1
      nLast := 0
      nCount := 0
      while nRecno <= nBookLen
         nCurLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKPARENT ] := pdfBookParent( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ]   := pdfBookPrev( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ]   := pdfBookNext( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ]  := pdfBookFirst( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ]   := pdfBookLast( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ]  := pdfBookCount( nRecno, nCurLevel )
         IF nCurLevel == 1
            nLast := nRecno
            ++nCount
         ENDIF
         ++nRecno
      enddo

      nLast += s_aReport[ REPORTOBJ ]

      cTemp := ltrim(str( s_aReport[ REPORTOBJ ] )) + " 0 obj" + CRLF + "<< /Type /Outlines /Count " + ltrim(str( nCount )) + " /First " + ltrim(str( nFirst )) + " 0 R /Last " + ltrim(str( nLast )) + " 0 R >>" + CRLF + "endobj" //+ CRLF
      aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
      s_aReport[ DOCLEN ] += len( cTemp )
      fwrite( s_aReport[ HANDLE ], cTemp )

      ++s_aReport[ REPORTOBJ ]
      nRecno := 1
      FOR nI := 1 to nBookLen
         cTemp := CRLF + ltrim(str( s_aReport[ REPORTOBJ ] + nI - 1 )) + " 0 obj" + CRLF + ;
                 "<<" + CRLF + ;
                 "/Parent " + ltrim(str( s_aReport[ BOOKMARK ][ nRecno ][ BOOKPARENT ])) + " 0 R" + CRLF + ;
                 "/Dest [" + ltrim(str( s_aReport[ PAGES ][ s_aReport[ BOOKMARK ][ nRecno ][ BOOKPAGE ] ] )) + " 0 R /XYZ 0 " + ltrim( str( s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOORD ])) + " 0]" + CRLF + ;
                 "/Title (" + alltrim( s_aReport[ BOOKMARK ][ nRecno ][ BOOKTITLE ]) + ")" + CRLF + ;
                 IIF( s_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ] > 0, "/Prev " + ltrim(str( s_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ])) + " 0 R" + CRLF, "") + ;
                 IIF( s_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ] > 0, "/Next " + ltrim(str( s_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ])) + " 0 R" + CRLF, "") + ;
                 IIF( s_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ] > 0, "/First " + ltrim(str( s_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ])) + " 0 R" + CRLF, "") + ;
                 IIF( s_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ] > 0, "/Last " + ltrim(str( s_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ])) + " 0 R" + CRLF, "") + ;
                 IIF( s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ] != 0, "/Count " + ltrim(str( s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ])) + CRLF, "") + ;
                 ">>" + CRLF + "endobj" + CRLF

         aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] + 2 )
         s_aReport[ DOCLEN ] += len( cTemp )
         fwrite( s_aReport[ HANDLE ], cTemp )
         ++nRecno
      NEXT
      pdfBookClose()

      s_aReport[ REPORTOBJ ] += nBookLen - 1
   ELSE
      cTemp := ltrim(str( s_aReport[ REPORTOBJ ] )) + " 0 obj" + CRLF + "<< /Type /Outlines /Count 0 >>" + CRLF + "endobj" + CRLF
      aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
      s_aReport[ DOCLEN ] += len( cTemp )
      fwrite( s_aReport[ HANDLE ], cTemp )
   ENDIF

   cTemp := CRLF
   s_aReport[ DOCLEN ] += len( cTemp )

   ++s_aReport[ REPORTOBJ ]

   cTemp += "xref" + CRLF + ;
   "0 " + ltrim(str( s_aReport[ REPORTOBJ ] )) + CRLF +;
   padl( s_aReport[ REFS ][ 1 ], 10, "0") + " 65535 f" + CRLF

   for nI := 2 to len( s_aReport[ REFS ] )
      cTemp += padl( s_aReport[ REFS ][ nI ], 10, "0") + " 00000 n" + CRLF
   next

   cTemp += "trailer << /Size " + ltrim(str( s_aReport[ REPORTOBJ ] )) + " /Root " + ltrim(str( nObj1 - 1 )) + " 0 R /Info " + ltrim(str( nObj1 - 2 )) + " 0 R >>" + CRLF + ;
            "startxref" + CRLF + ;
            ltrim(str( s_aReport[ DOCLEN ] )) + CRLF + ;
            "%%EOF" + CRLF
   fwrite( s_aReport[ HANDLE ], cTemp )
/*
   IF s_aReport[ OPTIMIZE ]
      pdfOptimize( ) coming !
   ENDIF
*/
   fclose( s_aReport[ HANDLE ] )

   s_aReport := nil

return nil
                                                                              /*
==============================                                                */
static function pdfClosePage()                                                /*
==============================                                                */
local cTemp, cBuffer, nBuffer, nRead, nI, k, nImage, nFont, nImageHandle

   aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )

   aadd( s_aReport[ PAGES ], s_aReport[ REPORTOBJ ] + 1 )

   cTemp := ;
   ltrim(str( ++s_aReport[ REPORTOBJ ] )) + " 0 obj" + CRLF + ;
   "<<" + CRLF + ;
   "/Type /Page /Parent 1 0 R" + CRLF + ;
   "/Resources " + ltrim(str( ++s_aReport[ REPORTOBJ ] )) + " 0 R" + CRLF + ;
   "/MediaBox [ 0 0 " + ltrim(transform( s_aReport[ PAGEX ], "9999.99")) + " " + ;
   ltrim(transform(s_aReport[ PAGEY ], "9999.99")) + " ]" + CRLF + ;
   "/Contents " + ltrim(str( ++s_aReport[ REPORTOBJ ] )) + " 0 R" + CRLF + ;
   ">>" + CRLF + ;
   "endobj" + CRLF

   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := ;
   ltrim(str(s_aReport[ REPORTOBJ ] - 1)) + " 0 obj" + CRLF + ;
   "<<"+CRLF+;
   "/ColorSpace << /DeviceRGB /DeviceGray >>" + CRLF + ; //version 0.01
   "/ProcSet [ /PDF /Text /ImageB /ImageC ]"

   IF len( s_aReport[ PAGEFONTS ] ) > 0
      cTemp += CRLF + ;
      "/Font" + CRLF + ;
      "<<"

      for nI := 1 to len( s_aReport[ PAGEFONTS ] )
         nFont := ascan( s_aReport[ FONTS ], { |arr| arr[1] == s_aReport[ PAGEFONTS ][ nI ] } )
         cTemp += CRLF + "/Fo" + ltrim(str( nFont )) + " " + ltrim(str( s_aReport[ FONTS ][ nFont ][ 2 ])) + " 0 R"
      next

      cTemp += CRLF + ">>"
   ENDIF

   IF len( s_aReport[ PAGEIMAGES ] ) > 0
      cTemp += CRLF + "/XObject" + CRLF + "<<"
      for nI := 1 to len( s_aReport[ PAGEIMAGES ] )
         nImage := ascan( s_aReport[ IMAGES ], { |arr| arr[1] == s_aReport[ PAGEIMAGES ][ nI ][ 1 ] } )
         IF nImage == 0
            aadd( s_aReport[ IMAGES ], { s_aReport[ PAGEIMAGES ][ nI ][ 1 ], ++s_aReport[ NEXTOBJ ], pdfImageInfo( s_aReport[ PAGEIMAGES ][ nI ][ 1 ] ) } )
            nImage := len( s_aReport[ IMAGES ] )
         ENDIF
         cTemp += CRLF + "/Image" + ltrim(str( nImage )) + " " + ltrim(str( s_aReport[ IMAGES ][ nImage ][ 2 ])) + " 0 R"
      next
      cTemp += CRLF + ">>"
   ENDIF

   cTemp += CRLF + ">>" + CRLF + "endobj" + CRLF

   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := ltrim(str( s_aReport[ REPORTOBJ ] )) + " 0 obj << /Length " + ;
   ltrim(str( s_aReport[ REPORTOBJ ] + 1 )) + " 0 R >>" + CRLF +;
   "stream"

   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   IF len( s_aReport[ PAGEIMAGES ] ) > 0
      cTemp := ""
      for nI := 1 to len( s_aReport[ PAGEIMAGES ] )
         cTemp += CRLF + "q"
         nImage := ascan( s_aReport[ IMAGES ], { |arr| arr[1] == s_aReport[ PAGEIMAGES ][ nI ][ 1 ] } )
         cTemp += CRLF + ltrim(str( IIF( s_aReport[ PAGEIMAGES ][ nI ][ 5 ] == 0, pdfM2X( s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_WIDTH ] / s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_XRES ] * 25.4 ), s_aReport[ PAGEIMAGES ][ nI ][ 5 ]))) + ;
         " 0 0 " + ;
         ltrim(str( IIF( s_aReport[ PAGEIMAGES ][ nI ][ 4 ] == 0, pdfM2X( s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_HEIGHT ] / s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_YRES ] * 25.4 ), s_aReport[ PAGEIMAGES ][ nI ][ 4 ]))) + ;
         " " + ltrim(str( s_aReport[ PAGEIMAGES ][ nI ][ 3 ] )) + ;
         " " + ltrim(str( s_aReport[ PAGEY ] - s_aReport[ PAGEIMAGES ][ nI ][ 2 ] - ;
         IIF( s_aReport[ PAGEIMAGES ][ nI ][ 4 ] == 0, pdfM2X( s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_HEIGHT ] / s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_YRES ] * 25.4 ), s_aReport[ PAGEIMAGES ][ nI ][ 4 ]))) + " cm"
         cTemp += CRLF + "/Image" + ltrim(str( nImage )) + " Do"
         cTemp += CRLF + "Q"
      next
      s_aReport[ PAGEBUFFER ] := cTemp + s_aReport[ PAGEBUFFER ]
   ENDIF

   cTemp := s_aReport[ PAGEBUFFER ]

   cTemp += CRLF + "endstream" + CRLF + ;
   "endobj" + CRLF

   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )

   cTemp := ltrim(str( ++s_aReport[ REPORTOBJ ] )) + " 0 obj" + CRLF + ;
   ltrim(str(len( s_aReport[ PAGEBUFFER ] ))) + CRLF + ;
   "endobj" + CRLF

   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

   for nI := 1 to len( s_aReport[ FONTS ] )
      IF s_aReport[ FONTS ][ nI ][ 2 ] > s_aReport[ REPORTOBJ ]

         aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )

         cTemp := ;
         ltrim(str( s_aReport[ FONTS ][ nI ][ 2 ] )) + " 0 obj" + CRLF + ;
         "<<" + CRLF + ;
         "/Type /Font" + CRLF + ;
         "/Subtype /Type1" + CRLF + ;
         "/Name /Fo" + ltrim(str( nI )) + CRLF + ;
         "/BaseFont /" + s_aReport[ TYPE1 ][ s_aReport[ FONTS ][ nI ][ 1 ] ] + CRLF + ;
         "/Encoding /WinAnsiEncoding" + CRLF + ;
         ">>" + CRLF + ;
         "endobj" + CRLF

         s_aReport[ DOCLEN ] += len( cTemp )
         fwrite( s_aReport[ HANDLE ], cTemp )

      ENDIF
   next

   for nI := 1 to len( s_aReport[ IMAGES ] )
      IF s_aReport[ IMAGES ][ nI ][ 2 ] > s_aReport[ REPORTOBJ ]

         aadd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )

         // "/Filter /CCITTFaxDecode" for B&W only ?
         cTemp :=  ;
         ltrim(str( s_aReport[ IMAGES ][ nI ][ 2 ] )) + " 0 obj" + CRLF + ;
         "<<" + CRLF + ;
         "/Type /XObject" + CRLF + ;
         "/Subtype /Image" + CRLF + ;
         "/Name /Image" + ltrim(str(nI)) + CRLF + ;
         "/Filter [" + IIF( at( ".jpg", lower( s_aReport[ IMAGES ][ nI ][ 1 ]) ) > 0, " /DCTDecode", "" ) + " ]" + CRLF + ;
         "/Width " + ltrim(str( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_WIDTH ] )) + CRLF + ;
         "/Height " + ltrim(str( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_HEIGHT ] )) + CRLF + ;
         "/BitsPerComponent " + ltrim(str( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_BITS ] )) + CRLF + ;
         "/ColorSpace /" + IIF( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_SPACE ] == 1, "DeviceGray", "DeviceRGB") + CRLF + ;
         "/Length " + ltrim(str( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ])) + CRLF + ;
         ">>" + CRLF + ;
         "stream" + CRLF

         s_aReport[ DOCLEN ] += len( cTemp )
         fwrite( s_aReport[ HANDLE ], cTemp )

         nImageHandle := fopen( s_aReport[ IMAGES ][ nI ][ 1 ] )
         fseek( nImageHandle, s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_FROM ] )

         nBuffer := 8192
         cBuffer := space( nBuffer )
         k := 0
         while k < s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ]
            IF k + nBuffer <= s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ]
               nRead := nBuffer
            ELSE
               nRead := s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ] - k
            ENDIF
            fread( nImageHandle, @cBuffer, nRead )

            s_aReport[ DOCLEN ] += nRead
            fwrite( s_aReport[ HANDLE ], cBuffer, nRead )
            k += nRead
         enddo

         fclose( nImageHandle )

         cTemp := CRLF + "endstream" + CRLF + ;
         "endobj" + CRLF

         s_aReport[ DOCLEN ] += len( cTemp )
         fwrite( s_aReport[ HANDLE ], cTemp )

      ENDIF
   next

   s_aReport[ REPORTOBJ ] := s_aReport[ NEXTOBJ ]

   s_aReport[ NEXTOBJ ] := s_aReport[ REPORTOBJ ] + 4

   s_aReport[ PAGEBUFFER ] := ""

return nil
                                                                              /*
========================================                                      */
static function pdfGetFontInfo( cParam )                                      /*
========================================                                      */
local cRet
   IF cParam == "NAME"
      IF left( s_aReport[ TYPE1 ][ s_aReport[ FONTNAME ] ], 5 ) == "Times"
         cRet := "Times"
      ELSEIF left( s_aReport[ TYPE1 ][ s_aReport[ FONTNAME ] ], 9 ) == "Helvetica"
         cRet := "Helvetica"
      ELSE
         cRet := "Courier" // 0.04
      ENDIF
   ELSE // size
      cRet := int(( s_aReport[ FONTNAME ] - 1 ) % 4)
   ENDIF
return cRet
                                                                              /*
====================================================================          */
function pdfImage( cFile, nRow, nCol, cUnits, nHeight, nWidth, cId )          /*
====================================================================          */

DEFAULT nRow to s_aReport[ REPORTLINE ]
DEFAULT nCol to 0
DEFAULT nHeight to 0
DEFAULT nWidth to 0
DEFAULT cUnits to "R"
DEFAULT cId to ""

   IF s_aReport[ HEADEREDIT ]
      return pdfHeader( "PDFIMAGE", cId, { cFile, nRow, nCol, cUnits, nHeight, nWidth } )
   ENDIF

   IF cUnits == "M"
      nRow := s_aReport[ PAGEY ] - pdfM2Y( nRow )
      nCol := pdfM2X( nCol )
      nHeight := s_aReport[ PAGEY ] - pdfM2Y( nHeight )
      nWidth := pdfM2X( nWidth )
   ELSEIF cUnits == "R"
      nRow := s_aReport[ PAGEY ] - pdfR2D( nRow )
      nCol := pdfM2X( s_aReport[ PDFLEFT ] ) + ;
              nCol * 100.00 / s_aReport[ REPORTWIDTH ] * ;
              ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
      nHeight := s_aReport[ PAGEY ] - pdfR2D( nHeight )
      nWidth := pdfM2X( s_aReport[ PDFLEFT ] ) + ;
              nWidth * 100.00 / s_aReport[ REPORTWIDTH ] * ;
              ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
   ELSEIF cUnits == "D"
   ENDIF

   aadd( s_aReport[ PAGEIMAGES ], { cFile, nRow, nCol, nHeight, nWidth } )

return nil
                                                                              /*
====================                                                          */
function pdfItalic()                                                          /*
====================                                                          */
   IF pdfGetFontInfo("NAME") = "Times"
      s_aReport[ FONTNAME ] := 3
   ELSEIF pdfGetFontInfo("NAME") = "Helvetica"
      s_aReport[ FONTNAME ] := 7
   ELSE
      s_aReport[ FONTNAME ] := 11 // 0.04
   ENDIF
   aadd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF ascan( s_aReport[ FONTS ], { |arr| arr[1] == s_aReport[ FONTNAME ] } ) == 0
      aadd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF
return nil
                                                                              /*
==========================                                                    */
function pdfLen( cString )                                                    /*
==========================                                                    */
local nWidth := 0.00, nI, nLen, nArr, nAdd := ( s_aReport[ FONTNAME ] - 1 ) % 4

   nLen := len( cString )
   IF right( cString, 1 ) == chr(255) .or. right( cString, 1 ) == chr(254 )// reverse or underline
      --nLen
   ENDIF
   IF pdfGetFontInfo("NAME") = "Times"
      nArr := 1
   ELSEIF pdfGetFontInfo("NAME") = "Helvetica"
      nArr := 2
   ELSE
      nArr := 3 // 0.04
   ENDIF
  
   if !empty( s_aReport[ FONTWIDTH ] )
      For nI:= 1 To nLen
         nWidth += s_aReport[ FONTWIDTH ][ nArr ][ ( asc( substr( cString, nI, 1 )) - 32 ) * 4 + 1 + nAdd ] * 25.4 * s_aReport[ FONTSIZE ] / 720.00 / 100.00
      Next
   endif

return nWidth
                                                                              /*
============================                                                  */
static function pdfM2R( mm )                                                  /*
============================                                                  */
return int( s_aReport[ LPI ] * mm / 25.4 )
                                                                              /*
===========================                                                   */
static function pdfM2X( n )                                                   /*
===========================                                                   */
return n * 72 / 25.4
                                                                              /*
===========================                                                   */
static function pdfM2Y( n )                                                   /*
===========================                                                   */
return s_aReport[ PAGEY ] -  n * 72 / 25.4
                                                                              /*
========================                                                      */
function pdfNewLine( n )                                                      /*
========================                                                      */
DEFAULT n to 1
   IF s_aReport[ REPORTLINE ] + n + s_aReport[ PDFTOP ] > s_aReport[ PDFBOTTOM ]
      pdfNewPage()
      s_aReport[ REPORTLINE ] += 1
   ELSE
      s_aReport[ REPORTLINE ] += n
   ENDIF
return s_aReport[ REPORTLINE ]
                                                                                          /*
==========================================================================================*/
function pdfNewPage( _cPageSize, _cPageOrient, _nLpi, _cFontName, _nFontType, _nFontSize )/*
==========================================================================================*/
local nAdd := 76.2
DEFAULT _cPageSize to s_aReport[ PAGESIZE ]
DEFAULT _cPageOrient to s_aReport[ PAGEORIENT ]
DEFAULT _nLpi to s_aReport[ LPI ]
DEFAULT _cFontName to pdfGetFontInfo("NAME")
DEFAULT _nFontType to pdfGetFontInfo("TYPE")
DEFAULT _nFontSize to s_aReport[ FONTSIZE ]

   IF !empty(s_aReport[ PAGEBUFFER ]) 
      pdfClosePage()
   ENDIF

   s_aReport[ PAGEFONTS ] := {}
   s_aReport[ PAGEIMAGES ] := {}

   ++s_aReport[ REPORTPAGE ] // NEW !!!

   pdfPageSize( _cPageSize )
   pdfPageOrient( _cPageOrient )
   pdfSetLPI( _nLpi )

   pdfSetFont( _cFontName, _nFontType, _nFontSize )

   pdfDrawHeader()

   s_aReport[ REPORTLINE ] := 0//5
   s_aReport[ FONTNAMEPREV ] := 0
   s_aReport[ FONTSIZEPREV ] := 0
return nil
                                                                              /*
====================                                                          */
function pdfNormal()                                                          /*
====================                                                          */
   IF pdfGetFontInfo("NAME") = "Times"
      s_aReport[ FONTNAME ] := 1
   ELSEIF pdfGetFontInfo("NAME") = "Helvetica"
      s_aReport[ FONTNAME ] := 5
   ELSE
      s_aReport[ FONTNAME ] := 9 // 0.04
   ENDIF
   aadd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF ascan( s_aReport[ FONTS ], { |arr| arr[1] == s_aReport[ FONTNAME ] } ) == 0
      aadd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF
return nil
                                                                              /*
==========================================                                    */
function pdfOpen( cFile, nLen, lOptimize )                                    /*
==========================================                                    */
local cTemp, nI, nJ, n1, n2 := 896, n12
DEFAULT nLen to 200
DEFAULT lOptimize to .f.

   s_aReport[ FONTNAME     ] := 1
   s_aReport[ FONTSIZE     ] := 10
   s_aReport[ LPI          ] := 6
   s_aReport[ PAGESIZE     ] := "LETTER"
   s_aReport[ PAGEORIENT   ] := "P"
   s_aReport[ PAGEX        ] := 8.5 * 72
   s_aReport[ PAGEY        ] := 11.0 * 72
   s_aReport[ REPORTWIDTH  ] := nLen // 200 // should be as parameter
   s_aReport[ REPORTPAGE   ] := 0
   s_aReport[ REPORTLINE   ] := 0//5
   s_aReport[ FONTNAMEPREV ] := 0
   s_aReport[ FONTSIZEPREV ] := 0
   s_aReport[ PAGEBUFFER   ] := ""
   s_aReport[ REPORTOBJ    ] := 1//2
   s_aReport[ DOCLEN       ] := 0
   s_aReport[ TYPE1        ] := { "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic", "Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique", "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique"  } // 0.04
   s_aReport[ MARGINS      ] := .t.
   s_aReport[ HEADEREDIT   ] := .f.
   s_aReport[ NEXTOBJ      ] := 0
   s_aReport[ PDFTOP       ] := 1 // top
   s_aReport[ PDFLEFT      ] := 10 // left & right
   s_aReport[ PDFBOTTOM    ] := s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] - 1 // bottom, default "LETTER", "P", 6
   s_aReport[ HANDLE       ] := fcreate( cFile )
   s_aReport[ PAGES        ] := {}
   s_aReport[ REFS         ] := { 0, 0 }
   s_aReport[ BOOKMARK     ] := {}
   s_aReport[ HEADER       ] := {}
   s_aReport[ FONTS        ] := {}
   s_aReport[ IMAGES       ] := {}
   s_aReport[ PAGEIMAGES   ] := {}
   s_aReport[ PAGEFONTS    ] := {}

   // TOFIX: This external file dependency should be removed.

   cTemp := vpdf_FontsDat() // times, times-bold, times-italic, times-bolditalic, helvetica..., courier... // 0.04
   n1 := len( cTemp ) / ( 2 * n2 )
   s_aReport[ FONTWIDTH ] := array( n1, n2 )

   s_aReport[ OPTIMIZE     ] := lOptimize

   s_aReport[ NEXTOBJ ] := s_aReport[ REPORTOBJ ] + 4

   n12 := 2 * n2 // 0.04
   for nI := 1 to n1
      for nJ := 1 to n2
         s_aReport[ FONTWIDTH ][ nI ][ nJ ] := bin2i(substr( cTemp, ( nI - 1 ) * n12 + ( nJ - 1 ) * 2 + 1, 2 ))
      next
   next

   s_aReport[ DOCLEN ] := 0
   cTemp := "%PDF-1.3" + CRLF
   s_aReport[ DOCLEN ] += len( cTemp )
   fwrite( s_aReport[ HANDLE ], cTemp )

return nil
                                                                              /*
==================================                                            */
function pdfPageSize( _cPageSize, _nWidth, _nHeight )                         /*
==================================                                            */
local nSize, aSize, nWidth, nHeight 

   aSize := { { "LETTER",    8.50, 11.00 }, ;
              { "LEGAL" ,    8.50, 14.00 }, ;
              { "LEDGER",   11.00, 17.00 }, ;
              { "EXECUTIVE", 7.25, 10.50 }, ;
              { "A4",        8.27, 11.69 }, ;
              { "A3",       11.69, 16.54 }, ;
              { "JIS B4",   10.12, 14.33 }, ;
              { "JIS B5",    7.16, 10.12 }, ;
              { "JPOST",     3.94,  5.83 }, ;
              { "JPOSTD",    5.83,  7.87 }, ;
              { "COM10",     4.12,  9.50 }, ;
              { "MONARCH",   3.87,  7.50 }, ;
              { "C5",        6.38,  9.01 }, ;
              { "DL",        4.33,  8.66 }, ;
              { "B5",        6.93,  9.84 }, ;
              { "USSTDFOLD", 14.87, 11.00 } }
             
   DEFAULT _cPageSize to "LETTER"

   if empty( _nWidth ) .or. empty( _nHeight )

      nSize := ascan( aSize, { |arr| arr[ 1 ] == _cPageSize } )

      IF nSize = 0
         nSize := 1
      ENDIF

      s_aReport[ PAGESIZE ] := aSize[ nSize ][ 1 ]

      nWidth := aSize[ nSize ][ 2 ] 
      nHeight := aSize[ nSize ][ 3 ]

   else

      _nWidth := val( str( _nWidth ) )
      _nHeight := val( str( _nHeight ) )

      nSize := ascan( aSize, { |arr| ( arr[ 2 ] == _nWidth  ) .and. ( arr[ 3 ] == _nHeight ) } )

      if nSize == 0
         nSize := ascan( aSize, { |arr| ( arr[ 3 ] == _nWidth ) .and. ( arr[ 2 ] == _nHeight ) } )
      endif   

      IF nSize = 0
         nSize := 1
      ENDIF

      s_aReport[ PAGESIZE ] := aSize[ nSize ][ 1 ]

      nWidth = _nWidth
      nHeight = _nHeight
   
   endif   

   IF s_aReport[ PAGEORIENT ] = "P"
      s_aReport[ PAGEX ] := nWidth * 72
      s_aReport[ PAGEY ] := nHeight * 72
   ELSE
      s_aReport[ PAGEX ] := nHeight * 72
      s_aReport[ PAGEY ] := nWidth * 72
   ENDIF
   
   return nil
                                                                              /*
======================================                                        */
function pdfPageOrient( _cPageOrient )                                        /*
======================================                                        */
DEFAULT _cPageOrient to "P"

   s_aReport[ PAGEORIENT ] := _cPageOrient
   pdfPageSize( s_aReport[ PAGESIZE ] )
return nil
                                                                              /*
==============================                                                */
static function pdfR2D( nRow )                                                /*
==============================                                                */
return s_aReport[ PAGEY ] - nRow * 72 / s_aReport[ LPI ]

                                                                              /*
==============================                                                */
static function pdfR2M( nRow )                                                /*
==============================                                                */
return 25.4 * nRow / s_aReport[ LPI ]
                                                                              /*
===========================                                                   */
function pdfPageNumber( n )                                                   /*
===========================                                                   */
DEFAULT n to 0
   IF n > 0
      s_aReport[ REPORTPAGE ] := n // NEW !!!
   ENDIF
return s_aReport[ REPORTPAGE ]
                                                                              /*
==============================                                                */
function pdfReverse( cString )                                                /*
==============================                                                */
return cString + chr(255)
                                                                              /*
=============================================================                 */
function pdfRJust( cString, nRow, nCol, cUnits, lExact, cId )                 /*
=============================================================                 */
local nLen, nAdj := 1.0, nAt
DEFAULT nRow to s_aReport[ REPORTLINE ]
DEFAULT cUnits to "R"
DEFAULT lExact to .f.

   IF s_aReport[ HEADEREDIT ]
      return pdfHeader( "PDFRJUST", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   IF ( nAt := at( "#pagenumber#", cString ) ) > 0
      cString := left( cString, nAt - 1 ) + ltrim(str( pdfPageNumber())) + substr( cString, nAt + 12 )
   ENDIF

   nLen := pdfLen( cString )

   IF cUnits == "R"
      IF !lExact
         pdfCheckLine( nRow )
         nRow := nRow + s_aReport[ PDFTOP ]
      ENDIF
   ENDIF
   pdfAtSay( cString, pdfR2M( nRow ), IIF( cUnits == "R", s_aReport[ PDFLEFT ] + ( s_aReport[ PAGEX ] / 72 * 25.4 - 2 * s_aReport[ PDFLEFT ] ) * nCol / s_aReport[ REPORTWIDTH ] - nAdj, nCol ) - nLen, "M", lExact )
return nil
                                                                              /*
==================================================                            */
function pdfSetFont( _cFont, _nType, _nSize, cId )                            /*
==================================================                            */

DEFAULT _cFont to "Times"
DEFAULT _nType to 0
DEFAULT _nSize to 10

   IF s_aReport[ HEADEREDIT ]
      return pdfHeader( "PDFSETFONT", cId, { _cFont, _nType, _nSize } )
   ENDIF

   _cFont := upper( _cFont )
   s_aReport[ FONTSIZE ] := _nSize

   IF _cFont == "TIMES"
      s_aReport[ FONTNAME ] := _nType + 1
   ELSEIF _cFont == "HELVETICA"
      s_aReport[ FONTNAME ] := _nType + 5
   ELSE
      s_aReport[ FONTNAME ] := _nType + 9 // 0.04
   ENDIF

   aadd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )

   IF ascan( s_aReport[ FONTS ], { |arr| arr[1] == s_aReport[ FONTNAME ] } ) == 0
      aadd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF
return nil
                                                                              /*
=========================                                                     */
function pdfSetLPI(_nLpi)                                                     /*
=========================                                                     */
local cLpi := alltrim(str(_nLpi))
DEFAULT _nLpi to 6

   cLpi := iif(cLpi$"1;2;3;4;6;8;12;16;24;48",cLpi,"6")
   s_aReport[ LPI ] := val( cLpi )

   pdfPageSize( s_aReport[ PAGESIZE ] )
return nil
                                                                              /*
==============================                                                */
function pdfStringB( cString )                                                /*
==============================                                                */
   cString := strtran( cString, "(", "\(" )
   cString := strtran( cString, ")", "\)" )
return cString
                                                                              /*
==============================================================================*/
function pdfTextCount( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits )/*
==============================================================================*/
return pdfText( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits, .f. )
                                                                                 /*
=================================================================================*/
function pdfText( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits, cColor, lPrint )/*
=================================================================================*/
local cDelim := chr(0)+chr(9)+chr(10)+chr(13)+chr(26)+chr(32)+chr(138)+chr(141)
local nI, cTemp, cToken, k, nL, nRow, nLines, nLineLen, nStart
local lParagraph, nSpace, nNew, nTokenLen, nCRLF, nTokens, nLen
DEFAULT nTab to -1
DEFAULT cUnits to 'R'
DEFAULT nJustify to 4 // justify
DEFAULT lPrint to .t.
DEFAULT cColor to ""

   IF cUnits == "M"
      nTop := pdfM2R( nTop )
   ELSEIF cUnits == "R"
      nLeft := pdfX2M( pdfM2X( s_aReport[ PDFLEFT ] ) + ;
              nLeft * 100.00 / s_aReport[ REPORTWIDTH ] * ;
              ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00 )
   ENDIF

   s_aReport[ REPORTLINE ] := nTop - 1

   nSpace := pdfLen( " " )
   nLines := 0
   nCRLF := 0

   nNew := nTab

   cString := alltrim( cString )
   nTokens := numtoken( cString, cDelim )
   nTokenLen := 0.00
   nStart := 1

   IF nJustify == 1 .or. nJustify == 4
      nLeft := nLeft
   ELSEIF nJustify == 2
      nLeft := nLeft - nLength / 2
   ELSEIF nJustify == 3
      nLeft := nLeft - nLength
   ENDIF

   nL := nLeft
   nL += nNew * nSpace // first always paragraph
   nLineLen := nSpace * nNew - nSpace

   lParagraph := .t.
   nI := 1

   while nI <= nTokens
      cToken := token( cString, cDelim, nI )
      nTokenLen := pdfLen( cToken )
      nLen := len( cToken )

      IF nLineLen + nSpace + nTokenLen > nLength
         IF nStart == nI // single word > nLength
            k := 1
            while k <= nLen
               cTemp := ""
               nLineLen := 0.00
               nL := nLeft
               IF lParagraph
                  nLineLen += nSpace * nNew
                  IF nJustify != 2
                     nL += nSpace * nNew
                  ENDIF
                  lParagraph := .f.
               ENDIF
               IF nJustify == 2
                  nL := nLeft + ( nLength - pdfLen( cTemp ) ) / 2
               ELSEIF nJustify == 3
                  nL := nLeft + nLength - pdfLen( cTemp )
               ENDIF
               while k <= nLen .and. ( ( nLineLen += pdfLen( substr( cToken, k, 1 ))) <= nLength )
                  nLineLen += pdfLen( substr( cToken, k, 1 ))
                  cTemp += substr( cToken, k, 1 )
                  ++k
               enddo
               IF empty( cTemp ) // single character > nlength
                  cTemp := substr( cToken, k, 1 )
                  ++k
               ENDIF
               ++nLines
               IF lPrint
                  nRow := pdfNewLine( 1 )
                  // version 0.02
                  pdfAtSay( cColor + cTemp, pdfR2M( nRow + s_aReport[ PDFTOP ] ), nL, "M" )
               ENDIF
            enddo
            ++nI
            nStart := nI
         ELSE
            pdfTextPrint( nI - 1, nLeft, @lParagraph, nJustify, nSpace, nNew, nLength, @nLineLen, @nLines, @nStart, cString, cDelim, cColor, lPrint )
         ENDIF
      ELSEIF ( nI == nTokens ) .or. ( nI < nTokens .and. ( nCRLF := pdfTextNextPara( cString, cDelim, nI ) ) > 0 )
         IF nI == nTokens
            nLineLen += nSpace + nTokenLen
         ENDIF
         pdfTextPrint( nI, nLeft, @lParagraph, nJustify, nSpace, nNew, nLength, @nLineLen, @nLines, @nStart, cString, cDelim, cColor, lPrint )
         ++nI

         IF nCRLF > 1
            nLines += nCRLF - 1
         ENDIF
         IF lPrint
            nRow := pdfNewLine( nCRLF - 1 )
         ENDIF

      ELSE
         nLineLen += nSpace + nTokenLen
         ++nI
      ENDIF
   enddo

return nLines
                                                                                                                                         /*
=========================================================================================================================================*/
static function pdfTextPrint( nI, nLeft, lParagraph, nJustify, nSpace, nNew, nLength, nLineLen, nLines, nStart, cString, cDelim, cColor, lPrint )/*
=========================================================================================================================================*/
local nFinish, nL, nB, nJ, cToken, nRow

   nFinish := nI

   nL := nLeft
   IF lParagraph
      IF nJustify != 2
         nL += nSpace * nNew
      ENDIF
   ENDIF

   IF nJustify == 3 // right
      nL += nLength - nLineLen
   ELSEIF nJustify == 2 // center
      nL += ( nLength - nLineLen ) / 2
   ENDIF

   ++nLines
   IF lPrint
      nRow := pdfNewLine( 1 )
   ENDIF
   nB := nSpace
   IF nJustify == 4
      nB := ( nLength - nLineLen + ( nFinish - nStart ) * nSpace ) / ( nFinish - nStart )
   ENDIF
   for nJ := nStart to nFinish
      cToken := token( cString, cDelim, nJ )
      IF lPrint
         // version 0.02
         pdfAtSay( cColor + cToken, pdfR2M( nRow + s_aReport[ PDFTOP ] ), nL, "M" )
      ENDIF
      nL += pdfLen ( cToken ) + nB
   next

   nStart := nFinish + 1

   lParagraph := .f.

   nLineLen := 0.00
   nLineLen += nSpace * nNew

return nil
                                                                              /*
======================================================                        */
static function pdfTextNextPara( cString, cDelim, nI )                        /*
======================================================                        */
local nAt, cAt, nCRLF, nNew, nRat, nRet := 0
   // check if next spaces paragraph(s)
   nAt := attoken( cString, cDelim, nI ) + len( token( cString, cDelim, nI ) )
   cAt := substr( cString, nAt, attoken( cString, cDelim, nI + 1 ) - nAt )
   nCRLF := numat( chr(13) + chr(10), cAt )
   nRat := rat( chr(13) + chr(10), cAt )
   nNew := len( cAt ) - nRat - IIF( nRat > 0, 1, 0 )
   IF nCRLF > 1 .or. ( nCRLF == 1 .and. nNew > 0 )
      nRet := nCRLF
   ENDIF
return nRet
                                                                              /*
================================                                              */
function pdfUnderLine( cString )                                              /*
================================                                              */
return cString + chr(254)
                                                                              /*
===========================                                                   */
static function pdfX2M( n )                                                   /*
===========================                                                   */
return n * 25.4 / 72
                                                                              /*
===================================                                           */
static function TimeAsAMPM( cTime )                                           /*
===================================                                           */
   IF VAL(cTime) < 12
      cTime += " am"
   ELSEIF VAL(cTime) == 12
      cTime += " pm"
   ELSE
      cTime := STR(VAL(cTime) - 12, 2) + SUBSTR(cTime, 3) + " pm"
   ENDIF
   cTime := left( cTime, 5 ) + substr( cTime, 10 )
return cTime

function pdfOpenHeader( cFile )
local nErrorCode := 0, nAt
DEFAULT cFile to ""
   IF !empty( cFile )
      cFile := alltrim( cFile )
      IF len( cFile ) > 12 .or. ;
         at( ' ', cFile ) > 0 .or. ;
         ( at( ' ', cFile ) == 0 .and. len( cFile ) > 8 ) .or. ;
         ( ( nAt := at( '.', cFile )) > 0 .and. len( substr( cFile, nAt + 1 )) > 3 )
         copy file (cFile) to temp.tmp
         cFile := "temp.tmp"
      ENDIF
      //s_aReport[ HEADER ] := FT_RestArr( cFile, @nErrorCode )
      s_aReport[ HEADER ] := File2Array( cFile )
   ELSE
      s_aReport[ HEADER ] := {}
   ENDIF
   s_aReport[ MARGINS ] := .t.
return nil

function pdfEditOnHeader()
   s_aReport[ HEADEREDIT ] := .t.
   s_aReport[ MARGINS ] := .t.
return nil

function pdfEditOffHeader()
   s_aReport[ HEADEREDIT ] := .f.
   s_aReport[ MARGINS ] := .t.
return nil

function pdfCloseHeader()
   s_aReport[ HEADER ] := {}
   s_aReport[ MARGINS ] := .f.
return nil

function pdfDeleteHeader( cId )
local nRet := -1, nId
   cId := upper( cId )
   nId := ascan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId })
   IF nId > 0
      nRet := len( s_aReport[ HEADER ] ) - 1
      aDel( s_aReport[ HEADER ], nId )
      aSize( s_aReport[ HEADER ], nRet )
      s_aReport[ MARGINS ] := .t.
   ENDIF
return nRet

function pdfEnableHeader( cId )
local nId
   cId := upper( cId )
   nId := ascan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId })
   IF nId > 0
      s_aReport[ HEADER ][ nId ][ 1 ] := .t.
      s_aReport[ MARGINS ] := .t.
   ENDIF
return nil

function pdfDisableHeader( cId )
local nId
   cId := upper( cId )
   nId := ascan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId })
   IF nId > 0
      s_aReport[ HEADER ][ nId ][ 1 ] := .f.
      s_aReport[ MARGINS ] := .t.
   ENDIF
return nil

function pdfSaveHeader( cFile )
local nErrorCode := 0
   Array2File( 'temp.tmp', s_aReport[ HEADER ] )
   copy file temp.tmp to (cFile)
return nil

function pdfHeader( cFunction, cId, arr )
local nId, nI, nLen, nIdLen
   nId := 0
   IF !empty( cId )
      cId := upper( cId )
      nId := ascan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId })
   ENDIF
   IF nId == 0
      nLen := len( s_aReport[ HEADER ] )
      IF empty( cId )
         cId := cFunction
         nIdLen := len( cId )
         for nI := 1 to nLen
            IF s_aReport[ HEADER ][ nI ][ 2 ] == cId
               IF val( substr( s_aReport[ HEADER ][ nI ][ 3 ], nIdLen + 1 ) ) > nId
                  nId := val( substr( s_aReport[ HEADER ][ nI ][ 3 ], nIdLen + 1 ) )
               ENDIF
            ENDIF
         next
         ++nId
         cId += ltrim(str(nId))
      ENDIF
      aadd( s_aReport[ HEADER ], { .t., cFunction, cId } )
      ++nLen
      for nI := 1 to len( arr )
         aadd( s_aReport[ HEADER ][ nLen ], arr[ nI ] )
      next
   ELSE
      aSize( s_aReport[ HEADER ][ nId ], 3 )
      for nI := 1 to len( arr )
         aadd( s_aReport[ HEADER ][ nId ], arr[ nI ] )
      next
   ENDIF
return cId

function pdfDrawHeader()
local nI, _nFont, _nSize, nLen := len( s_aReport[ HEADER ] )

   IF nLen > 0

      // save font
      _nFont := s_aReport[ FONTNAME ]
      _nSize := s_aReport[ FONTSIZE ]

      for nI := 1 to nLen
         IF s_aReport[ HEADER ][ nI ][ 1 ] // enabled
            do case
            case s_aReport[ HEADER ][ nI ][ 2 ] == "PDFATSAY"
               pdfAtSay( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 3 ] )

            case s_aReport[ HEADER ][ nI ][ 2 ] == "PDFCENTER"
               pdfCenter( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 3 ] )

            case s_aReport[ HEADER ][ nI ][ 2 ] == "PDFRJUST"
               pdfRJust( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 3 ] )

            case s_aReport[ HEADER ][ nI ][ 2 ] == "PDFBOX"
               pdfBox( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 9 ], s_aReport[ HEADER ][ nI ][ 10 ], s_aReport[ HEADER ][ nI ][ 3 ] )

            case s_aReport[ HEADER ][ nI ][ 2 ] == "PDFSETFONT"
               pdfSetFont( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 3 ] )

            case s_aReport[ HEADER ][ nI ][ 2 ] == "PDFIMAGE"
               pdfImage( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 9 ], s_aReport[ HEADER ][ nI ][ 3 ] )

            endcase
         ENDIF
      next
      s_aReport[ FONTNAME ] := _nFont
      s_aReport[ FONTSIZE ] := _nSize

      IF s_aReport[ MARGINS ]
         pdfMargins()
      ENDIF

   ELSE
      IF s_aReport[ MARGINS ]
         s_aReport[ PDFTOP ] := 1 // top
         s_aReport[ PDFLEFT ] := 10 // left & right
         s_aReport[ PDFBOTTOM ] := s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] - 1 // bottom, default "LETTER", "P", 6

         s_aReport[ MARGINS ] := .f.
      ENDIF
   ENDIF
return nil

function pdfMargins( nTop, nLeft, nBottom )
local nI, nLen := len( s_aReport[ HEADER ] ), nTemp, aTemp, nHeight

// version 0.07 begin

   DEFAULT nTop to 1 // top
   DEFAULT nLeft to 10 // left & right
   DEFAULT nBottom to s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] - 1 // bottom, default "LETTER", "P", 6

   s_aReport[ PDFTOP ] := nTop
   s_aReport[ PDFLEFT ] := nLeft
   s_aReport[ PDFBOTTOM ] := nBottom

// version 0.07 end

   for nI := 1 to nLen
      IF s_aReport[ HEADER ][ nI ][ 1 ] // enabled

         IF s_aReport[ HEADER ][ nI ][ 2 ] == "PDFSETFONT"

         ELSEIF s_aReport[ HEADER ][ nI ][ 2 ] == "PDFIMAGE"
            IF s_aReport[ HEADER ][ nI ][ 8 ] == 0 // picture in header, first at all, not at any page yet
               aTemp := pdfImageInfo( s_aReport[ HEADER ][ nI ][ 4 ] )
               nHeight := aTemp[ IMAGE_HEIGHT ] / aTemp[ IMAGE_YRES ] * 25.4
               IF s_aReport[ HEADER ][ nI ][ 7 ] == "D"
                  nHeight := pdfM2X( nHeight )
               ENDIF
            ELSE
               nHeight := s_aReport[ HEADER ][ nI ][ 8 ]
            ENDIF

            IF s_aReport[ HEADER ][ nI ][ 7 ] == "M"

               nTemp := s_aReport[ PAGEY ] / 72 * 25.4 / 2

               IF s_aReport[ HEADER ][ nI ][ 5 ] < nTemp
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 5 ] + nHeight ) * s_aReport[ LPI ] / 25.4 // top
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSE
                  nTemp := s_aReport[ HEADER ][ nI ][ 5 ] * s_aReport[ LPI ] / 25.4 // top
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF

            ELSEIF s_aReport[ HEADER ][ nI ][ 7 ] == "D"
               nTemp := s_aReport[ PAGEY ] / 2

               IF s_aReport[ HEADER ][ nI ][ 5 ] < nTemp
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 5 ] + nHeight ) * s_aReport[ LPI ] / 72 // top
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSE
                  nTemp := s_aReport[ HEADER ][ nI ][ 5 ] * s_aReport[ LPI ] / 72 // top
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF

               ENDIF

            ENDIF

         ELSEIF s_aReport[ HEADER ][ nI ][ 2 ] == "PDFBOX"

            IF s_aReport[ HEADER ][ nI ][ 10 ] == "M"

               nTemp := s_aReport[ PAGEY ] / 72 * 25.4 / 2

               IF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .and. ;
                  s_aReport[ HEADER ][ nI ][ 6 ] < nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 6 ] * s_aReport[ LPI ] / 25.4 // top
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .and. ;
                      s_aReport[ HEADER ][ nI ][ 6 ] > nTemp

                  nTemp := ( s_aReport[ HEADER ][ nI ][ 4 ] + s_aReport[ HEADER ][ nI ][ 8 ] ) * s_aReport[ LPI ] / 25.4 // top
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF

                  nTemp := ( s_aReport[ HEADER ][ nI ][ 6 ] - s_aReport[ HEADER ][ nI ][ 8 ] ) * s_aReport[ LPI ] / 25.4 // top
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF

               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] > nTemp .and. ;
                      s_aReport[ HEADER ][ nI ][ 6 ] > nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 4 ] * s_aReport[ LPI ] / 25.4 // top
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF

            ELSEIF s_aReport[ HEADER ][ nI ][ 10 ] == "D"
               nTemp := s_aReport[ PAGEY ] / 2

               IF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .and. ;
                  s_aReport[ HEADER ][ nI ][ 6 ] < nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 6 ] / s_aReport[ LPI ] // top
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .and. ;
                      s_aReport[ HEADER ][ nI ][ 6 ] > nTemp

                  nTemp := ( s_aReport[ HEADER ][ nI ][ 4 ] + s_aReport[ HEADER ][ nI ][ 8 ] ) / s_aReport[ LPI ] // top
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF

                  nTemp := ( s_aReport[ HEADER ][ nI ][ 6 ] - s_aReport[ HEADER ][ nI ][ 8 ] ) / s_aReport[ LPI ] // top
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF

               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] > nTemp .and. ;
                      s_aReport[ HEADER ][ nI ][ 6 ] > nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 4 ] / s_aReport[ LPI ] // top
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF

            ENDIF

         ELSE
            IF s_aReport[ HEADER ][ nI ][ 7 ] == "R"
               nTemp := s_aReport[ HEADER ][ nI ][ 5 ] // top
               IF s_aReport[ HEADER ][ nI ][ 5 ] > s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] / 2
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
            ELSEIF s_aReport[ HEADER ][ nI ][ 7 ] == "M"
               nTemp := s_aReport[ HEADER ][ nI ][ 5 ] * s_aReport[ LPI ] / 25.4 // top
               IF s_aReport[ HEADER ][ nI ][ 5 ] > s_aReport[ PAGEY ] / 72 * 25.4 / 2
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
            ELSEIF s_aReport[ HEADER ][ nI ][ 7 ] == "D"
               nTemp := s_aReport[ HEADER ][ nI ][ 5 ] / s_aReport[ LPI ] // top
               IF s_aReport[ HEADER ][ nI ][ 5 ] > s_aReport[ PAGEY ] / 2
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   next

   s_aReport[ MARGINS ] := .f.

return nil

function pdfCreateHeader( _file, _size, _orient, _lpi, _width )
local ;
   s_aReportStyle := {                                                  ;
                     { 1,     2,   3,   4,    5,     6    }, ; //"Default"
                     { 2.475, 4.0, 4.9, 6.4,  7.5,  64.0  }, ; //"P6"
                     { 3.3  , 5.4, 6.5, 8.6, 10.0,  85.35 }, ; //"P8"
                     { 2.475, 4.0, 4.9, 6.4,  7.5,  48.9  }, ; //"L6"
                     { 3.3  , 5.4, 6.5, 8.6, 10.0,  65.2  }, ; //"L8"
                     { 2.475, 4.0, 4.9, 6.4,  7.5,  82.0  }, ; //"P6"
                     { 3.3  , 5.4, 6.5, 8.6, 10.0, 109.35 }  ; //"P8"
                   }
local nStyle := 1, nAdd := 0.00

DEFAULT _size to s_aReport[ PAGESIZE ]
DEFAULT _orient to s_aReport[ PAGEORIENT ]
DEFAULT _lpi to s_aReport[ LPI ]
DEFAULT _width to 200

   IF _size == "LETTER"
      IF _orient == "P"
         IF _lpi == 6
            nStyle := 2
         ELSEIF _lpi == 8
            nStyle := 3
         ENDIF
      ELSEIF _orient == "L"
         IF _lpi == 6
            nStyle := 4
         ELSEIF _lpi == 8
            nStyle := 5
         ENDIF
      ENDIF
   ELSEIF _size == "LEGAL"
      IF _orient == "P"
         IF _lpi == 6
            nStyle := 6
         ELSEIF _lpi == 8
            nStyle := 7
         ENDIF
      ELSEIF _orient == "L"
         IF _lpi == 6
            nStyle := 4
         ELSEIF _lpi == 8
            nStyle := 5
         ENDIF
      ENDIF
   ENDIF

   pdfEditOnHeader()

   IF _size == "LEGAL"
      nAdd := 76.2
   ENDIF

   IF _orient == "P"
      pdfBox(   5.0, 5.0, 274.0 + nAdd, 210.0,  1.0 )
      pdfBox(   6.5, 6.5, 272.5 + nAdd, 208.5,  0.5 )

      pdfBox(  11.5, 9.5,  22.0       , 205.5,  0.5, 5 )
      pdfBox(  23.0, 9.5,  33.5       , 205.5,  0.5, 5 )
      pdfBox(  34.5, 9.5, 267.5 + nAdd, 205.5,  0.5 )

   ELSE
      pdfBox(  5.0, 5.0, 210.0, 274.0 + nAdd, 1.0 )
      pdfBox(  6.5, 6.5, 208.5, 272.5 + nAdd, 0.5 )

      pdfBox( 11.5, 9.5,  22.0, 269.5 + nAdd, 0.5, 5 )
      pdfBox( 23.0, 9.5,  33.5, 269.5 + nAdd, 0.5, 5 )
      pdfBox( 34.5, 9.5, 203.5, 269.5 + nAdd, 0.5 )
   ENDIF

   pdfSetFont("Helvetica", BOLD, 10) // 0.04
   pdfAtSay( "Test Line 1", s_aReportStyle[ nStyle ][ 1 ], 1, "R", .t. )

   pdfSetFont("Times", BOLD, 18)
   pdfCenter( "Test Line 2", s_aReportStyle[ nStyle ][ 2 ],,"R", .t. )

   pdfSetFont("Times", BOLD, 12)
   pdfCenter( "Test Line 3", s_aReportStyle[ nStyle ][ 3 ],,"R", .t. )

   pdfSetFont("Helvetica", BOLD, 10) // 0.04
   pdfAtSay( "Test Line 4", s_aReportStyle[ nStyle ][ 4 ], 1, "R", .t. )

   pdfSetFont("Helvetica", BOLD, 10) // 0.04
   pdfAtSay( "Test Line 5", s_aReportStyle[ nStyle ][ 5 ], 1, "R", .t. )

   pdfAtSay( dtoc( date()) + " " + TimeAsAMPM( time() ), s_aReportStyle[ nStyle ][ 6 ], 1, "R", .t. )
   pdfRJust( "Page: #pagenumber#", s_aReportStyle[ nStyle ][ 6 ], s_aReport[ REPORTWIDTH ], "R", .t. )

   pdfEditOffHeader()
   pdfSaveHeader( _file )

return nil

function pdfImageInfo( cFile )
local cTemp := upper(substr( cFile, rat('.', cFile) + 1 )), aTemp := {}
   do case
   case cTemp == "TIF"
      aTemp := pdfTIFFInfo( cFile )
   case cTemp == "JPG"
      aTemp := pdfJPEGInfo( cFile )
   endcase
return aTemp

function pdfTIFFInfo( cFile )
local c40 := chr(0)+chr(0)+chr(0)+chr(0)
local aType := {"BYTE","ASCII","SHORT","LONG","RATIONAL","SBYTE","UNDEFINED","SSHORT","SLONG","SRATIONAL","FLOAT","DOUBLE"}
local aCount := { 1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8 }
local nTemp, nHandle, cValues, c2, nFieldType, nCount, nPos, nTag, nValues
local nOffset, cTemp, cIFDNext, nIFD, nFields, cTag, nPages, nn

local nWidth := 0, nHeight := 0, nBits := 0, nFrom := 0, nLength := 0, xRes := 0, yRes := 0, aTemp := {}, nSpace

   nHandle := fopen( cFile )

   c2 := '  '
   fread( nHandle, @c2, 2 )
   fread( nHandle, @c2, 2 )
   cIFDNext := '    '
   fread( nHandle, @cIFDNext, 4 )

   cTemp := space(12)
   nPages := 0

   while !( cIFDNext == c40 ) //read IFD's

      nIFD := bin2l( cIFDNext )

      fseek( nHandle, nIFD )
      //?'*** IFD ' + ltrim(str( ++nPages ))

      fread( nHandle, @c2, 2 )
      nFields := bin2i( c2 )

      for nn := 1 to nFields
         fread( nHandle, @cTemp, 12 )

         nTag := bin2w( substr( cTemp, 1, 2 ) )
         nFieldType := bin2w(substr( cTemp, 3, 2 ))
      /*
      1 = BYTE       8-bit unsigned integer.
      2 = ASCII      8-bit byte that contains a 7-bit ASCII code; the last byte
                     must be NUL (binary zero).
      3 = SHORT      16-bit (2-byte) unsigned integer.
      4 = LONG       32-bit (4-byte) unsigned integer.
      5 = RATIONAL   Two LONGs: the first represents the numerator of a
                     fraction; the second, the denominator.

      In TIFF 6.0, some new field types have been defined:

      6 = SBYTE      An 8-bit signed (twos-complement) integer.
      7 = UNDEFINED  An 8-bit byte that may contain anything, depending on
                     the definition of the field.
      8 = SSHORT     A 16-bit (2-byte) signed (twos-complement) integer.
      9 = SLONG      A 32-bit (4-byte) signed (twos-complement) integer.
      10 = SRATIONAL Two SLONG’s: the first represents the numerator of a
                     fraction, the second the denominator.
      11 = FLOAT     Single precision (4-byte) IEEE format.
      12 = DOUBLE    Double precision (8-byte) IEEE format.
      */
         nCount := bin2l(substr( cTemp, 5, 4 ))
         nOffset := bin2l(substr( cTemp, 9, 4 ))

         IF nCount > 1 .or. nFieldType == RATIONAL .or. nFieldType == SRATIONAL
            nPos := filepos( nHandle )
            fseek( nHandle, nOffset)

            nValues := nCount * aCount[ nFieldType ]
            cValues := space( nValues )
            fread( nHandle, @cValues, nValues )
            fseek( nHandle, nPos )
         ELSE
            cValues := substr( cTemp, 9, 4 )
         ENDIF

         IF nFieldType ==  ASCII
            --nCount
         ENDIF
         //?'Tag'
         //??' ' + padr( nTag, 10 )
         cTag := ''
         do case
         case nTag == 256
               /*
               ImageWidth
               Tag = 256 (100.H)
               Type = SHORT or LONG
               The number of columns in the image, i.e., the number of pixels per scanline.
               */
            //??'ImageWidth'
            cTag := 'ImageWidth'
/*
               IF nFieldType != SHORT .and. nFieldType != LONG
                  alert('Wrong Type for ImageWidth')
               ENDIF
*/
            IF nFieldType ==  SHORT
               nWidth := bin2w(substr( cValues, 1, 2 ))
            ELSEIF nFieldType ==  LONG
               nWidth := bin2l(substr( cValues, 1, 4 ))
            ENDIF

         case nTag == 257
               /*
               ImageLength
               Tag = 257 (101.H)
               Type = SHORT or LONG
               The number of rows (sometimes described as scanlines) in the image.
               */
            //??'ImageLength'
            cTag := 'ImageLength'
/*
               IF nFieldType != SHORT .and. nFieldType != LONG
                  alert('Wrong Type for ImageLength')
               ENDIF
*/
            IF nFieldType ==  SHORT
               nHeight := bin2w(substr( cValues, 1, 2 ))
            ELSEIF nFieldType ==  LONG
               nHeight := bin2l(substr( cValues, 1, 4 ))
            ENDIF

         case nTag == 258
               /*
               BitsPerSample
               Tag = 258 (102.H)
               Type = SHORT
               The number of bits per component.
               Allowable values for Baseline TIFF grayscale images are 4 and 8, allowing either
               16 or 256 distinct shades of gray.
               */
            //??'BitsPerSample'
            cTag := 'BitsPerSample'
            nTemp := 0
            IF nFieldType == SHORT
               nTemp := bin2w( cValues )
            ELSE
               //alert('Wrong Type for BitsPerSample')
            ENDIF
            nBits := nTemp
            //IF nTemp != 4 .and. nTemp != 8
            //   alert('Wrong Value for BitsPerSample')
            //ENDIF
         case nTag == 259
               /*
               Compression
               Tag = 259 (103.H)
               Type = SHORT
               Values:
               1 = No compression, but pack data into bytes as tightly as possible, leaving no unused
               bits (except at the end of a row). The component values are stored as an array of
               type BYTE. Each scan line (row) is padded to the next BYTE boundary.
               2 = CCITT Group 3 1-Dimensional Modified Huffman run length encoding. See
               Section 10 for a description of Modified Huffman Compression.
               32773 = PackBits compression, a simple byte-oriented run length scheme. See the
               PackBits section for details.
               Data compression applies only to raster image data. All other TIFF fields are
               unaffected.
               Baseline TIFF readers must handle all three compression schemes.
               */
            //??'Compression'
            cTag := 'Compression'
            nTemp := 0
            IF nFieldType == SHORT
               nTemp := bin2w( cValues )
            ELSE
               //alert('Wrong Type for Compression')
            ENDIF
            //IF nTemp != 1 .and. nTemp != 2 .and. nTemp != 32773
            //   alert('Wrong Value for Compression')
            //ENDIF
         case nTag == 262
               /*
               PhotometricInterpretation
               Tag = 262 (106.H)
               Type = SHORT
               Values:
               0 = WhiteIsZero. For bilevel and grayscale images: 0 is imaged as white. The maxi-mum
               value is imaged as black. This is the normal value for Compression=2.
               1 = BlackIsZero. For bilevel and grayscale images: 0 is imaged as black. The maxi-mum
               value is imaged as white. If this value is specified for Compression=2, the
               image should display and print reversed.
               */
            //??'PhotometricInterpretation'
            cTag := 'PhotometricInterpretation'
            nTemp := -1
            IF nFieldType == SHORT
               nTemp := bin2w( cValues )
            ELSE
               //alert('Wrong Type for PhotometricInterpretation')
            ENDIF
            IF nTemp != 0 .and. nTemp != 1 .and. nTemp != 2 .and. nTemp != 3
               //alert('Wrong Value for PhotometricInterpretation')
            ENDIF
         case nTag == 264
               /*
               CellWidth
               The width of the dithering or halftoning matrix used to create a dithered or
               halftoned bilevel file.Tag = 264 (108.H)
               Type = SHORT
               N = 1
               No default. See also Threshholding.
               */
            //??'CellWidth'
            cTag := 'CellWidth'
            IF nFieldType != SHORT
               //alert('Wrong Type for CellWidth')
            ENDIF
         case nTag == 265
               /*
               CellLength
               The length of the dithering or halftoning matrix used to create a dithered or
               halftoned bilevel file.
               Tag = 265 (109.H)
               Type = SHORT
               N = 1
               This field should only be present if Threshholding = 2
               No default. See also Threshholding.
               */
            //??'CellLength'
            cTag := 'CellLength'
            IF nFieldType != SHORT
               //alert('Wrong Type for CellLength')
            ENDIF
         case nTag == 266
               /*
               FillOrder
               The logical order of bits within a byte.
               Tag = 266 (10A.H)
               Type = SHORT
               N = 1
               */
            //??'FillOrder'
            cTag := 'FillOrder'
            IF nFieldType != SHORT
               //alert('Wrong Type for FillOrder')
            ENDIF
         case nTag == 273
               /*
               StripOffsets
               Tag = 273 (111.H)
               Type = SHORT or LONG
               For each strip, the byte offset of that strip.
               */
            //??'StripOffsets'
            cTag := 'StripOffsets'
            IF nFieldType != SHORT .and. nFieldType != LONG
               //alert('Wrong Type for StripOffsets')
            ENDIF

            IF nFieldType ==  SHORT
               nFrom := bin2w(substr( cValues, 1, 2 ))
            ELSEIF nFieldType ==  LONG
               nFrom := bin2l(substr( cValues, 1, 4 ))
            ENDIF

         case nTag == 277
               /*
               SamplesPerPixel
               Tag = 277 (115.H)
               Type = SHORT
               The number of components per pixel. This number is 3 for RGB images, unless
               extra samples are present. See the ExtraSamples field for further information.
               */
            //??'SamplesPerPixel'
            cTag := 'SamplesPerPixel'
            IF nFieldType != SHORT
               //alert('Wrong Type for SamplesPerPixel')
            ENDIF
         case nTag == 278
               /*
               RowsPerStrip
               Tag = 278 (116.H)
               Type = SHORT or LONG
               The number of rows in each strip (except possibly the last strip.)
               For example, if ImageLength is 24, and RowsPerStrip is 10, then there are 3
               strips, with 10 rows in the first strip, 10 rows in the second strip, and 4 rows in the
               third strip. (The data in the last strip is not padded with 6 extra rows of dummy
               data.)
               */
            //??'RowsPerStrip'
            cTag := 'RowsPerStrip'
            IF nFieldType != SHORT .and. nFieldType != LONG
               //alert('Wrong Type for RowsPerStrip')
            ENDIF
         case nTag == 279
               /*
               StripByteCounts
               Tag = 279 (117.H)
               Type = SHORT or LONG
               For each strip, the number of bytes in that strip after any compression.
               */
            //??'StripByteCounts'
            cTag := 'StripByteCounts'
            IF nFieldType != SHORT .and. nFieldType != LONG
               //alert('Wrong Type for StripByteCounts')
            ENDIF

            IF nFieldType ==  SHORT
               nLength := bin2w(substr( cValues, 1, 2 ))
            ELSEIF nFieldType ==  LONG
               nLength := bin2l(substr( cValues, 1, 4 ))
            ENDIF

            nLength *= nCount // Count all strips !!!

         case nTag == 282
               /*
               XResolution
               Tag = 282 (11A.H)
               Type = RATIONAL
               The number of pixels per ResolutionUnit in the ImageWidth (typically, horizontal
               - see Orientation) direction.
               */
            //??'XResolution'
            cTag := 'XResolution'
            IF nFieldType != RATIONAL
               //alert('Wrong Type for XResolution')
            ENDIF
            xRes := bin2l(substr( cValues, 1, 4 ))
         case nTag == 283
               /*
               YResolution
               Tag = 283 (11B.H)
               Type = RATIONAL
               The number of pixels per ResolutionUnit in the ImageLength (typically, vertical)
               direction.
               */
            //??'YResolution'
            cTag := 'YResolution'
            IF nFieldType != RATIONAL
               //alert('Wrong Type for YResolution')
            ENDIF
            yRes := bin2l(substr( cValues, 1, 4 ))
         case nTag == 284
            //??'PlanarConfiguration'
            cTag := 'PlanarConfiguration'
            IF nFieldType != SHORT
               //alert('Wrong Type for PlanarConfiguration')
            ENDIF
         case nTag == 288
               /*
               FreeOffsets
               For each string of contiguous unused bytes in a TIFF file, the byte offset of the
               string.
               Tag = 288 (120.H)
               Type = LONG
               Not recommended for general interchange.
               See also FreeByteCounts.
               */
            //??'FreeOffsets'
            cTag := 'FreeOffsets'
            IF nFieldType != LONG
               //alert('Wrong Type for FreeOffsets')
            ENDIF
         case nTag == 289
               /*
               FreeByteCounts
               For each string of contiguous unused bytes in a TIFF file, the number of bytes in
               the string.
               Tag = 289 (121.H)
               Type = LONG
               Not recommended for general interchange.
               See also FreeOffsets.
               */
            //??'FreeByteCounts'
            cTag := 'FreeByteCounts'
            IF nFieldType != LONG
               //alert('Wrong Type for FreeByteCounts')
            ENDIF
         case nTag == 296
               /*
               ResolutionUnit
               Tag = 296 (128.H)
               Type = SHORT
               Values:
               1 = No absolute unit of measurement. Used for images that may have a non-square
               aspect ratio but no meaningful absolute dimensions.
               2 = Inch.
               3 = Centimeter.
               Default = 2 (inch).
               */
            //??'ResolutionUnit'
            cTag := 'ResolutionUnit'
            nTemp := 0
            IF nFieldType == SHORT
               nTemp := bin2w( cValues )
            ELSE
               //alert('Wrong Type for ResolutionUnit')
            ENDIF
            IF nTemp != 1 .and. nTemp != 2 .and. nTemp != 3
               //alert('Wrong Value for ResolutionUnit')
            ENDIF
         case nTag == 305
            //??'Software'
            cTag := 'Software'
            IF nFieldType != ASCII
               //alert('Wrong Type for Software')
            ENDIF
         case nTag == 306
               /*
               DateTime
               Date and time of image creation.
               Tag = 306 (132.H)
               Type = ASCII
               N = 2 0
               The format is: YYYY:MM:DD HH:MM:SS, with hours like those on a 24-hour
               clock, and one space character between the date and the time. The length of the
               string, including the terminating NUL, is 20 bytes.
               */
            //??'DateTime'
            cTag := 'DateTime'
            IF nFieldType != ASCII
               //alert('Wrong Type for DateTime')
            ENDIF
         case nTag == 315
               /*
               Artist
               Person who created the image.
               Tag = 315 (13B.H)
               Type = ASCII
               Note: some older TIFF files used this tag for storing Copyright information.
               */
            //??'Artist'
            cTag := 'Artist'
            IF nFieldType != ASCII
               //alert('Wrong Type for Artist')
            ENDIF
         case nTag == 320
               /*
               ColorMap
               Tag = 320 (140.H)
               Type = SHORT
               N = 3 * (2**BitsPerSample)
               This field defines a Red-Green-Blue color map (often called a lookup table) for
               palette color images. In a palette-color image, a pixel value is used to index into an
               RGB-lookup table. For example, a palette-color pixel having a value of 0 would
               be displayed according to the 0th Red, Green, Blue triplet.
               In a TIFF ColorMap, all the Red values come first, followed by the Green values,
               then the Blue values. In the ColorMap, black is represented by 0,0,0 and white is
               represented by 65535, 65535, 65535.
               */
            //??'ColorMap'
            cTag := 'ColorMap'
            IF nFieldType != SHORT
               //alert('Wrong Type for ColorMap')
            ENDIF
         case nTag == 338
               /*
               ExtraSamples
               Description of extra components.
               Tag = 338 (152.H)
               Type = SHORT
               N = m
               */
            //??'ExtraSamples'
            cTag := 'ExtraSamples'
            IF nFieldType != SHORT
               //alert('Wrong Type for ExtraSamples')
            ENDIF
         case nTag == 33432
               /*
               Copyright
               Copyright notice.
               Tag = 33432 (8298.H)
               Type = ASCII
               Copyright notice of the person or organization that claims the copyright to the
               image. The complete copyright statement should be listed in this field including
               any dates and statements of claims. For example, “Copyright, John Smith, 19xx.
               All rights reserved.
               */
            //??'Copyright'
            cTag := 'Copyright'
            IF nFieldType != ASCII
               //alert('Wrong Type for Copyright')
            ENDIF
         otherwise
            //??'Unknown'
            cTag := 'Unknown'
         endcase
      /*
      ??padr( cTag, 30 )
      ??' type ' + padr(aType[ nFieldType ], 10) + ' count ' + ltrim(str(nCount)) + ' <'
      do case
         case nFieldType ==  BYTE
              for nI := 1 to nCount
                  ??' ' + ltrim(str(asc( substr( cValues, nI, 1 ))))
              next
         case nFieldType ==  ASCII
              ??' '
              for nI := 1 to nCount
                  ??substr( cValues, nI, 1 )
              next
         case nFieldType ==  SHORT
              for nI := 1 to nCount
                  ??' ' + ltrim(str(bin2w(substr( cValues, ( nI - 1 ) * 2 + 1, 2 ))))
              next
         case nFieldType ==  LONG
              for nI := 1 to nCount
                  ??' ' + ltrim(str(bin2l(substr( cValues, ( nI - 1 ) * 4 + 1, 4 ))))
              next
         case nFieldType ==  RATIONAL
              for nI := 1 to nCount
                  ??' ' + ltrim(str(bin2l(substr( cValues, ( nI - 1 ) * 8 + 1, 4 )))) + '/' + ltrim(str(bin2l(substr( cValues, nI + 4, 4 ))))
              next
         case nFieldType ==  SBYTE
              for nI := 1 to nCount
                  ??' ' + ltrim(str(asc( substr( cValues, nI, 1 ))))
              next
         case nFieldType ==  UNDEFINED
              for nI := 1 to nCount
                  ??' ' + substr( cValues, nI, 1 )
              next
         case nFieldType ==  SSHORT
              for nI := 1 to nCount
                  ??' ' + ltrim(str(bin2i(substr( cValues, ( nI - 1 ) * 2 + 1, 2 ))))
              next
         case nFieldType ==  SLONG
              for nI := 1 to nCount
                  ??' ' + ltrim(str(bin2l(substr( cValues, ( nI - 1 ) * 4 + 1, 4 ))))
              next
         case nFieldType == SRATIONAL
              for nI := 1 to nCount
                  ??' ' + ltrim(str(bin2l(substr( cValues, ( nI - 1 ) * 8 + 1, 4 )))) + '/' + ltrim(str(bin2l(substr( cValues, nI + 4, 4 ))))
              next
         case nFieldType == FLOAT
         case nFieldType == DOUBLE
              for nI := 1 to nCount
                  ??' ' + ltrim(str(ctof(substr( cValues, ( nI - 1 ) * 8 + 1, 8 ))))
              next

      endcase
      ??' >'
      */
      next
      fread( nHandle, @cIFDNext, 4 )
   enddo

   fclose( nHandle )

   aadd( aTemp, nWidth )
   aadd( aTemp, nHeight )
   aadd( aTemp, xRes )
   aadd( aTemp, yRes )
   aadd( aTemp, nBits )
   aadd( aTemp, nFrom )
   aadd( aTemp, nLength )

   nSpace := 0
   aadd( aTemp, nSpace )
return aTemp

function pdfJPEGInfo( cFile )
local c255, nAt, nHandle
local nWidth := 0, nHeight := 0, nBits := 8, nFrom := 0, nLength := 0, xRes := 0, yRes := 0, aTemp := {}
local nBuffer := 20000
local nSpace := 3 // 3 - RGB, 1 - GREY, 4 - CMYK

   nHandle := fopen( cFile )

   c255 := space( nBuffer )
   fread( nHandle, @c255, nBuffer )

   xRes := asc(substr( c255, 15, 1 )) * 256 + asc(substr( c255, 16, 1 ))
   yRes := asc( substr( c255, 17, 1 )) * 256 + asc(substr( c255, 18, 1 ))

   nAt := rat( chr(255) + chr(192), c255 ) + 5
   nHeight := asc(substr( c255, nAt, 1 )) * 256 + asc(substr( c255, nAt + 1, 1 ))
   nWidth := asc( substr( c255, nAt + 2, 1 )) * 256 + asc(substr( c255, nAt + 3, 1 ))

   nSpace := asc( substr( c255, nAt + 4, 1 ))

   nLength := filesize( nHandle )

   fclose( nHandle )

   aadd( aTemp, nWidth )
   aadd( aTemp, nHeight )
   aadd( aTemp, xRes )
   aadd( aTemp, yRes )
   aadd( aTemp, nBits )
   aadd( aTemp, nFrom )
   aadd( aTemp, nLength )
   aadd( aTemp, nSpace )

return aTemp

STATIC FUNCTION FilePos( nHandle )
RETURN ( FSEEK( nHandle, 0, FS_RELATIVE ) )

STATIC FUNCTION Chr_RGB( cChar )
RETURN str(asc( cChar ) / 255, 4, 2)

STATIC FUNCTION NumToken( cString, cDelimiter )
RETURN AllToken( cString, cDelimiter )

STATIC FUNCTION Token( cString, cDelimiter, nPointer )
RETURN AllToken( cString, cDelimiter, nPointer, 1 )

STATIC FUNCTION AtToken( cString, cDelimiter, nPointer )
RETURN AllToken( cString, cDelimiter, nPointer, 2 )

STATIC FUNCTION AllToken( cString, cDelimiter, nPointer, nAction )
LOCAL nTokens := 0, nPos := 1, nLen := len( cString ), nStart := 0, cToken := "", cRet
DEFAULT cDelimiter to chr(0)+chr(9)+chr(10)+chr(13)+chr(26)+chr(32)+chr(138)+chr(141)
DEFAULT nAction to 0

// nAction == 0 - numtoken
// nAction == 1 - token
// nAction == 2 - attoken

      while nPos <= nLen
            if !substr( cString, nPos, 1 ) $ cDelimiter
               nStart := nPos
               while nPos <= nLen .and. !substr( cString, nPos, 1 ) $ cDelimiter
                     ++nPos
               enddo
               ++nTokens
               IF nAction > 0
                  IF nPointer == nTokens
                     IF nAction == 1
                        cRet := substr( cString, nStart, nPos - nStart )
                     ELSE
                        cRet := nStart
                     ENDIF
                     exit
                  ENDIF
               ENDIF
            endif
            if substr( cString, nPos, 1 ) $ cDelimiter
               while nPos <= nLen .and. substr( cString, nPos, 1 ) $ cDelimiter
                     ++nPos
               enddo
            endif
            cRet := nTokens
      ENDDO
RETURN cRet

STATIC FUNCTION NumAt( cSearch, cString )
   LOCAL n := 0, nAt := 0, nPos := 0
   WHILE ( nAt := at( cSearch, substr( cString, nPos + 1 ) )) > 0
           nPos += nAt
           ++n
   ENDDO
RETURN n

STATIC FUNCTION FileSize( nHandle )

   LOCAL nCurrent
   LOCAL nLength

   // Get file position
   nCurrent := FilePos( nHandle )

   // Get file length
   nLength := FSEEK( nHandle, 0, FS_END )

   // nLength := FilePos( nHandle )

   // Reset file position
   FSEEK( nHandle, nCurrent )

RETURN ( nLength )

// next 3 function written by Peter Kulek
//modified for compatibility with common.ch by V.K.
//modified DATE processing by V.K.
static function Array2File(cFile,aRay,nDepth,hFile)
local nBytes := 0
local i
nDepth := iif(ISNUMBER(nDepth),nDepth,0)
if hFile == NIL
   if (hFile := fCreate(cFile,FC_NORMAL)) == -1
      return(nBytes)
   endif
endif
nDepth++
nBytes += WriteData(hFile,aRay)
if ISARRAY(aRay)
   for i := 1 to len(aRay)
      nBytes += Array2File(cFile,aRay[i],nDepth,hFile)
   next
endif
nDepth--
if nDepth == 0
   fClose(hFile)
endif
return(nBytes)

static function WriteData(hFile,xData)
local cData  := valtype(xData)
   if ISCHARACTER(xData)
       cData += i2bin(len(xData))+xData
   elseif ISNUMBER(xData)
       cData += i2bin(len(alltrim(str(xData))) )+alltrim(str(xData))
   elseif ISDATE(xData)
       cData += i2bin(8)+dtos(xData)
   elseif ISLOGICAL(xData)
       cData += i2bin(1)+iif(xData,'T','F')
   elseif ISARRAY(xData)
       cData += i2bin(len(xData))
   else
       cData += i2bin(0)   // NIL
   endif
return( fWrite(hFile,cData,len(cData)) )

static function File2Array(cFile,nLen,hFile)
LOCAL cData,cType,nDataLen,nBytes
local nDepth := 0
local aRay   := {}
if hFile == NIL
     if (hFile:=fOpen(cFile,FO_READ)) == -1
         return(aRay)
     endif
     cData := space(3)
     fRead(hFile,@cData,3)
     if left(cData,1) != 'A'
         return( aRay)
     endif
     nLen := bin2i(right(cData,2))
endif
do while nDepth < nLen
    cData  := space(3)
    nBytes := fRead(hFile,@cData,3)
    if nBytes<3
       exit
    endif
    cType:= padl(cData,1)
    nDataLen:= bin2i(right(cData,2))
    if cType != 'A'
       cData := space(nDataLen)
       nBytes:= fRead(hFile,@cData,nDataLen)
       if nBytes<nDataLen
           exit
       endif
    endif
    nDepth++
    aadd(aRay,NIL)
    if cType=='C'
        aRay[nDepth] := cData
    elseif cType=='N'
        aRay[nDepth] := val(cData)
    elseif cType=='D'
        aRay[nDepth] := ctod( left( cData, 4 ) + "/" + substr( cData, 5, 2 ) + "/" + substr( cData, 7, 2 )) //stod(cData)
    elseif cType=='L'
        aRay[nDepth] := (cData=='T')
    elseif cType=='A'
        aRay[nDepth] := File2Array(,nDataLen,hFile)
    endif
enddo
if cFile!=NIL
    fClose(hFile)
endif
return(aRay)
// end of 3rd function written by Peter Kulek
