/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PDF api Api for multiple porpurse
 *
 * Copyright 2000-2004 Luiz Rafael Culik <culikr /at/ brturbo.com>
 *            2000-2004 Priptpal Bedi  <vouchcac /at/ hotmail.com>
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


#include "common.ch"
#include "pdfhbdoc.ch"
#include "pdf.ch"
#include "hbclass.ch"

/* TPdf class
based on  work of : Victor K.    . http://www.ihaveparts.com
+ many new itens that Victor code dont handle
*/

CLASS TPdf

   EXPORT:
      DATA aReport INIT Array( PARAMLEN )

      METHOD New( cFile, nLen, nHeigth, nWidth )
      METHOD Pdfnewpage( _cPageSize, _cPageOrient, _nLpi, _cFontName, _nFontType, _nFontSize )
      METHOD Pdfclosepage()
      METHOD Pdfclose()
      METHOD Pdfpagesize( _cPageSize )
      METHOD Pdfpageorient( _cPageOrient )
      METHOD Pdfsetlpi( _nLpi )
      METHOD Pdfsetfont( _cFont, _nType, _nSize, cId )
      METHOD Pdfdrawheader()
      METHOD Pdfmargins( nTop, nLeft, nBottom )
      METHOD Pdfimageinfo( cFile )
      METHOD Pdftiffinfo( cFile )

      METHOD Pdfjpeginfo( cFile )
      METHOD Pdfimage( cFile, nRow, nCol, nHeight, nWidth, cId, Scalex, Scaley )
      METHOD Pdfatsay( cString, nRow, nCol, lExact, cId )
      METHOD Pdfgetfontinfo( cParam )
      METHOD Pdfpagenum( n )
   PROTECTED:
      METHOD Pdfstringb( cString )

      /* New method added by Luiz Rafael Culik not exist on original code */
   export:
      METHOD Getpagetype( nWidth, nHeight )
      METHOD Settop( N ) INLINE ::aReport[ PDFTOP ] := N    // top
      METHOD Setleft( N ) INLINE ::aReport[ PDFLEFT ] := N  // left & right
      METHOD Setbottom( N ) INLINE ::aReport[ PDFBOTTOM ] := N
      METHOD Pdf_Rect( x, y, width, height )
      METHOD Pdf_Stroke()
      METHOD Setoverline( l )
      METHOD Setunderline( l )
      METHOD Pdfmoveto( X, Y )
      METHOD Pdflineto( X, Y )
      METHOD Pdfsetlinewidth( w )
      METHOD Pdfsetlinecap( w )
      METHOD Pdfsetdash( b, w )
      METHOD Pdfsave( lBuf )
      METHOD Pdfrestore( lBuf )
      METHOD Pdfunder( Text, Len, X, Y )
      METHOD Pdfover( Text, Len, x, y )
      METHOD Pdf_Str_Width( Text, Len, Font, Size )
      METHOD Pdfsetrgbcolor( R, G, B )
      METHOD Pdfscale( X, Y, lBuf )
      METHOD Pdftranslate( tX, tY )

      METHOD Pdfpnginfo( cFile )
      METHOD Pdfrotate( pHi )
      METHOD Pdfclosepath()
      METHOD Pdfendpath()

      METHOD Pdfcurveto( x1, y1, x2, y2, x3, y3 )
      METHOD Pdfcomplevel( x )
      METHOD Pdfinfo( cTitle, cAuthor, cKey, cCreator )
      METHOD PDF_setrgbcolor_fill( R, G, B )
ENDCLASS

   /* Original Victor Code (with some itens nowt needed removed*/

METHOD NEW( cFile, nLen, nHeigth, nWidth ) CLASS TPdf

LOCAL cTemp
LOCAL nI
LOCAL nJ
LOCAL n1
LOCAL n2    := 896
LOCAL n12

   DEFAULT nLen TO 200
   DEFAULT nHeigth TO a4_height, nWidth TO a4_width

   ::aReport[ HANDLE ] := Fcreate( cFile )
   IF ::aReport[ HANDLE ] <= 0
      RETURN NIL
   ENDIF

   //   if nHeigth < a4_height
   //       nHeigth  :=  a4_height
   //   endif

   //   if nWidth < a4_width
   //      nWidth :=    a4_width
   //   endif

   ::aReport[ FONTNAME ]   := 1
   ::aReport[ FONTSIZE ]   := 10
   ::aReport[ LPI ]        := 6
   ::aReport[ PAGESIZE ]   :=::GetPageType( nWidth, nHeigth )
   ::aReport[ PAGEORIENT ] := Iif( nWidth > nHeigth, "P", "L" )
   ::aReport[ PAGEX ]      := nWidth
   ::aReport[ PAGEY ]      := nHeigth

   ::aReport[ REPORTWIDTH ]  := nLen    // 200 // should be as parameter
   ::aReport[ REPORTPAGE ]   := 0
   ::aReport[ REPORTLINE ]   := 0       //5
   ::aReport[ FONTNAMEPREV ] := 0
   ::aReport[ FONTSIZEPREV ] := 0
   ::aReport[ PAGEBUFFER ]   := ""
   ::aReport[ REPORTOBJ ]    := 1       //2
   ::aReport[ DOCLEN ]       := 0
   ::aReport[ TYPE1 ]        := { "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic", "Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique", "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique" }               // 0.04
   ::aReport[ MARGINS ]      := .t.
   ::aReport[ HEADEREDIT ]   := .f.
   ::aReport[ NEXTOBJ ]      := 0
   ::aReport[ PDFTOP ]       := 1       // top
   ::aReport[ PDFLEFT ]      := 10      // Left & right
   ::aReport[ PDFBOTTOM ]    := ::aReport[ PAGEY ] * ::aReport[ LPI ] - 1       // bottom, DEFAULT "LETTER", "P", 6

   ::aReport[ PAGES ]      := {}
   ::aReport[ REFS ]       := { 0, 0 }
   ::aReport[ BOOKMARK ]   := {}
   ::aReport[ HEADER ]     := {}
   ::aReport[ FONTS ]      := {}
   ::aReport[ IMAGES ]     := {}
   ::aReport[ PAGEIMAGES ] := {}
   ::aReport[ PAGEFONTS ]  := {}

   cTemp                   := Memoread( "fonts.dat" )       // times, times-bold, times-italic, times-bolditalic, helvetica..., courier... // 0.04
   n1                      := Len( cTemp ) / ( 2 * n2 )
   ::aReport[ FONTWIDTH ]  := Array( n1, n2 )
   ::aReport[ PDFCOMPLVL ] := 0
   ::aReport[ NEXTOBJ ]    := ::aReport[ REPORTOBJ ] + 4

   n12 := 2 * n2    // 0.04
   FOR nI := 1 TO n1
      FOR nJ := 1 TO n2

         ::aReport[ FONTWIDTH, nI, nJ ] := Bin2i( Substr( cTemp, ( nI - 1 ) * n12 + ( nJ - 1 ) * 2 + 1, 2 ) )
      NEXT
   NEXT

   ::aReport[ CREATOR ]   := ""
   ::aReport[ KEYWORD ]   := ""
   ::aReport[ AUTHOR ]    := ""
   ::aReport[ SUBJECT ]   := ""
   ::aReport[ OVERLINE ]  := .f.
   ::aReport[ UNDERLINE ] := .f.

   ::aReport[ DOCLEN ] := 0
   cTemp               := "%PDF-1.3" + CRLF
   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

RETURN SELF

METHOD Pdfnewpage( _cPageSize, _cPageOrient, _nLpi, _cFontName, _nFontType, _nFontSize ) CLASS TPdf

LOCAL nAdd   := 76.2
   DEFAULT _cPageSize TO ::aReport[ PAGESIZE ]
   DEFAULT _cPageOrient TO ::aReport[ PAGEORIENT ]
   DEFAULT _nLpi TO ::aReport[ LPI ]
   DEFAULT _cFontName TO ::pdfGetFontInfo( "NAME" )
   DEFAULT _nFontType TO ::pdfGetFontInfo( "TYPE" )
   DEFAULT _nFontSize TO ::aReport[ FONTSIZE ]

   IF !Empty( ::aReport[ PAGEBUFFER ] )
      ::pdfClosePage()
   ENDIF

   ::aReport[ PAGEFONTS ]  := {}
   ::aReport[ PAGEIMAGES ] := {}

   ++ ::aReport[ REPORTPAGE ]           // NEW !!!

   ::pdfPageSize( _cPageSize )
   ::pdfPageOrient( _cPageOrient )
   ::pdfSetLPI( _nLpi )

   ::pdfSetFont( _cFontName, _nFontType, _nFontSize )

   ::pdfDrawHeader()

   ::aReport[ REPORTLINE ]   := 0
   ::aReport[ FONTNAMEPREV ] := 0
   ::aReport[ FONTSIZEPREV ] := 0
RETURN nil

METHOD Pdfclosepage() CLASS TPdf

LOCAL cTemp
LOCAL cBuffer
LOCAL nBuffer
LOCAL nRead
LOCAL nI
LOCAL k
LOCAL nImage
LOCAL nFont
LOCAL nImageHandle
LOCAL Row          := 0
LOCAL rowsperstrip
LOCAL res

   Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )

   Aadd( ::aReport[ PAGES ], ::aReport[ REPORTOBJ ] + 1 )

   cTemp := ;
           Ltrim( Str( ++ ::aReport[ REPORTOBJ ] ) ) + " 0 obj" + CRLF + ;
           "<<" + CRLF + ;
           "/Type /Page /Parent 1 0 R" + CRLF + ;
           "/Resources " + Ltrim( Str( ++ ::aReport[ REPORTOBJ ] ) ) + " 0 R" + CRLF + ;
           "/MediaBox [ 0 0 " + Ltrim( Transform( ::aReport[ PAGEX ], "9999.99" ) ) + " " + ;
           Ltrim( Transform( ::aReport[ PAGEY ], "9999.99" ) ) + " ]" + CRLF + ;
           "/Contents " + Ltrim( Str( ++ ::aReport[ REPORTOBJ ] ) ) + " 0 R" + CRLF + ;
           ">>" + CRLF + ;
           "endobj" + CRLF
   //   "/Contents [ " + LTrim(Str( ++::aReport[ REPORTOBJ ] )) + " 0 R ]" + CRLF + ;

   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )
   cTemp := ;
           Ltrim( Str( ::aReport[ REPORTOBJ ] - 1 ) ) + " 0 obj" + CRLF + ;
           "<<" + CRLF + ;
           "/ColorSpace << /DeviceRGB /DeviceGray >>" + CRLF + ;                //version 0.01
           "/ProcSet [ /PDF /Text /ImageB /ImageC ]"

   IF Len( ::aReport[ PAGEFONTS ] ) > 0
      cTemp += CRLF + ;
              "/Font" + CRLF + ;
              "<<"

      FOR nI := 1 TO Len( ::aReport[ PAGEFONTS ] )
         nFont := Ascan( ::aReport[ FONTS ], { | arr | arr[ 1 ] == ::aReport[ PAGEFONTS, nI ] } )
         //IF nFont == 0
         //   alert("New font after!!!")
         //ENDIF
         cTemp += CRLF + "/Fo" + Ltrim( Str( nFont ) ) + " " + Ltrim( Str( ::aReport[ FONTS, nFont, 2 ] ) ) + " 0 R"
      NEXT

      cTemp += CRLF + ">>"
   ENDIF

   IF Len( ::aReport[ PAGEIMAGES ] ) > 0

      cTemp += CRLF + "/XObject" + CRLF + "<<"
      FOR nI := 1 TO Len( ::aReport[ PAGEIMAGES ] )
         nImage := Ascan( ::aReport[ IMAGES ], { | arr | arr[ 1 ] == ::aReport[ PAGEIMAGES, nI, 1 ] } )
         IF nImage == 0
            Aadd( ::aReport[ IMAGES ], { ::aReport[ PAGEIMAGES, nI, 1 ], ++ ::aReport[ NEXTOBJ ], ::pdfImageInfo( ::aReport[ PAGEIMAGES, nI, 1 ] ) } )
            nImage := Len( ::aReport[ IMAGES ] )
         ENDIF
         cTemp += CRLF + "/Image" + Ltrim( Str( nImage ) ) + " " + Ltrim( Str( ::aReport[ IMAGES, nImage, 2 ] ) ) + " 0 R"
      NEXT
      cTemp += CRLF + ">>"
   ENDIF

   cTemp += CRLF + ">>" + CRLF + "endobj" + CRLF

   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )
   cTemp := Ltrim( Str( ::aReport[ REPORTOBJ ] ) ) + " 0 obj << /Length " + ;
                   Ltrim( Str( ::aReport[ REPORTOBJ ] + 1 ) ) + " 0 R " + CRLF + ;
                   Iif( ::aReport[ PDFCOMPLVL ] > 0, "/Filter/FlateDecode" + CRLF + ">>", CRLF + ">>" ) + CRLF + ;
                   "stream" + CRLF

   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   IF Len( ::aReport[ PAGEIMAGES ] ) > 0
      cTemp := ""
      FOR nI := 1 TO Len( ::aReport[ PAGEIMAGES ] )
         IF ::aReport[ PAGEIMAGES, nI, 6 ] != 0 .AND. ::aReport[ PAGEIMAGES, nI, 7 ] != 0
            cTemp +=::pdfsave( .f. )
            cTemp +=::pdfscale( ::aReport[ PAGEIMAGES, nI, 6 ], ::aReport[ PAGEIMAGES, nI, 7 ], .f. )
         ENDIF
         cTemp  +=::pdfsave( .f. )
         nImage := Ascan( ::aReport[ IMAGES ], { | arr | arr[ 1 ] == ::aReport[ PAGEIMAGES, nI, 1 ] } )
         cTemp  += CRLF + Ltrim( Str( Iif( ::aReport[ PAGEIMAGES, nI, 5 ] == 0, pdfM2X( ::aReport[ IMAGES, nImage, 3, IMAGE_HEIGHT ] ), ::aReport[ PAGEIMAGES, nI, 4 ] ) ) ) + ;
                                " 0 0 " + ;
                                Ltrim( Str( Iif( ::aReport[ PAGEIMAGES, nI, 4 ] == 0, pdfM2X( ::aReport[ IMAGES, nImage, 3, IMAGE_WIDTH ] ), ::aReport[ PAGEIMAGES, nI, 5 ] ) ) ) + ;
                                " " + Ltrim( Str( ::aReport[ PAGEIMAGES, nI, 3 ] ) )
         /*
         " " + LTrim(Str( ::aReport[ PAGEY ] - ::aReport[ PAGEIMAGES ][ nI ][ 2 ] - ;
         IIF( ::aReport[ PAGEIMAGES ][ nI ][ 4 ] == 0, pdfM2X( ::aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_HEIGHT ] / ::aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_YRES ] * 25.4 ), ::aReport[ PAGEIMAGES ][ nI ][ 4 ]))) + " cm"
         */
         rowsperstrip := ::aReport[ PAGEIMAGES, nI, 5 ]
         res          := calcdata( ::aReport[ PAGEIMAGES, nI, 5 ], ::aReport[ PAGEIMAGES, nI, 2 ], ::aReport[ PAGEIMAGES, nI, 5 ] )

         cTemp += " " + Ltrim( Str( res ) ) + " cm"

         cTemp += CRLF + "/Image" + Ltrim( Str( nImage ) ) + " Do"
         cTemp +=::pdfrestore( .f. )
         IF ::aReport[ PAGEIMAGES, nI, 6 ] != 0 .AND. ::aReport[ PAGEIMAGES, nI, 7 ] != 0
            cTemp +=::pdfrestore( .f. )
         ENDIF

      NEXT
      ::aReport[ PAGEBUFFER ] := cTemp + ::aReport[ PAGEBUFFER ]
   ENDIF

   IF ::aReport[ PDFCOMPLVL ] > 0

      cTemp := HB_COMPRESS( ::aReport[ PDFCOMPLVL ], ::aReport[ PAGEBUFFER ] )
   ELSE
      cTemp := ::aReport[ PAGEBUFFER ]
   ENDIF
   cTemp += CRLF + "endstream" + CRLF + ;
           "endobj" + CRLF

   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )
   cTemp := Ltrim( Str( ++ ::aReport[ REPORTOBJ ] ) ) + " 0 obj" + CRLF + ;
                   Ltrim( Str( Len( ::aReport[ PAGEBUFFER ] ) ) ) + CRLF + ;
                   "endobj" + CRLF

   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   FOR nI := 1 TO Len( ::aReport[ FONTS ] )
      IF ::aReport[ FONTS, nI, 2 ] > ::aReport[ REPORTOBJ ]

         Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )

         cTemp := ;
                 Ltrim( Str( ::aReport[ FONTS, nI, 2 ] ) ) + " 0 obj" + CRLF + ;
                 "<<" + CRLF + ;
                 "/Type /Font" + CRLF + ;
                 "/Subtype /Type1" + CRLF + ;
                 "/Name /Fo" + Ltrim( Str( nI ) ) + CRLF + ;
                 "/BaseFont /" + ::aReport[ TYPE1, ::aReport[ FONTS, nI, 1 ] ] + CRLF + ;
                 "/Encoding /WinAnsiEncoding" + CRLF + ;
                 ">>" + CRLF + ;
                 "endobj" + CRLF

         ::aReport[ DOCLEN ] += Len( cTemp )
         Fwrite( ::aReport[ HANDLE ], cTemp )

      ENDIF
   NEXT

   FOR nI := 1 TO Len( ::aReport[ IMAGES ] )
      IF ::aReport[ IMAGES, nI, 2 ] > ::aReport[ REPORTOBJ ]

         Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )

         // "/Filter /CCITTFaxDecode" FOR B&W only ?
         cTemp := ;
                 Ltrim( Str( ::aReport[ IMAGES, nI, 2 ] ) ) + " 0 obj" + CRLF + ;
                 "<<" + CRLF + ;
                 "/Type /XObject" + CRLF + ;
                 "/Subtype /Image" + CRLF + ;
                 "/Name /Image" + Ltrim( Str( nI ) ) + CRLF
         IF At( ".PNG", Upper( ::aReport[ IMAGES, nI, 1 ] ) ) > 0
            cTemp += "/Filter[/FlateDecode]" + CRLF
            cTemp += "/DecodeParms[<</Predictor 15/Columns " + Ltrim( Str( ::aReport[ IMAGES, nI, 3, IMAGE_WIDTH ] ) ) + Iif( ::aReport[ IMAGES, nI, 3, 8 ] == 2, "/Colors " + Ltrim( Str( ::areport[ IMAGES, nI, 3, 8 ] + 1 ) ), "" ) + ">>]" + CRLF
         ELSE
            cTemp += "/Filter [" + Iif( At( ".JPG", Upper( ::aReport[ IMAGES, nI, 1 ] ) ) > 0, " /DCTDecode", "" ) + " ]" + CRLF
         ENDIF
         cTemp += "/Width " + Ltrim( Str( ::aReport[ IMAGES, nI, 3, IMAGE_WIDTH ] ) ) + CRLF + ;
                                     "/Height " + Ltrim( Str( ::aReport[ IMAGES, nI, 3, IMAGE_HEIGHT ] ) ) + CRLF + ;
                                     "/BitsPerComponent " + Ltrim( Str( ::aReport[ IMAGES, nI, 3, IMAGE_BITS ] ) ) + CRLF
         IF ::aReport[ IMAGES, nI, 3, 8 ] == 2
            cTemp += "/ColorSpace/" + Iif( ::aReport[ IMAGES, nI, 3, IMAGE_BITS ] == 1, "DeviceGray", "DeviceRGB" ) + CRLF + ;
                                           "/Length " + Ltrim( Str( ::aReport[ IMAGES, nI, 3, IMAGE_LENGTH ] ) ) + CRLF + ;
                                           ">>" + CRLF + ;
                                           "stream" + CRLF
         ELSE
            cTemp += "/ColorSpace[/Indexed/DeviceRGB 255 " + Ltrim( Str( ::aReport[ IMAGES, nI, 2 ] + 1 ) ) + " 0 R]" + CRLF + ;
                                                                    "/Length " + Ltrim( Str( ::aReport[ IMAGES, nI, 3, IMAGE_LENGTH ] ) ) + CRLF + ;
                                                                    ">>" + CRLF + ;
                                                                    "stream" + CRLF

         ENDIF

         ::aReport[ DOCLEN ] += Len( cTemp )
         Fwrite( ::aReport[ HANDLE ], cTemp )

         nImageHandle := Fopen( ::aReport[ IMAGES, nI, 1 ] )

         Fseek( nImageHandle, ::aReport[ IMAGES, nI, 3, IMAGE_FROM ] )

         nBuffer := 8192
         cBuffer := Space( nBuffer )
         k       := 0
         WHILE k < ::aReport[ IMAGES, nI, 3, IMAGE_LENGTH ]
            IF k + nBuffer <= ::aReport[ IMAGES, nI, 3, IMAGE_LENGTH ]
               nRead := nBuffer
            ELSE
               nRead := ::aReport[ IMAGES, nI, 3, IMAGE_LENGTH ] - k
            ENDIF
            Fread( nImageHandle, @cBuffer, nRead )

            ::aReport[ DOCLEN ] += nRead
            Fwrite( ::aReport[ HANDLE ], cBuffer, nRead )
            k += nRead
         ENDDO

         cTemp := "endstream" + CRLF + ;
                  "endobj" + CRLF

         ::aReport[ DOCLEN ] += Len( cTemp )
         Fwrite( ::aReport[ HANDLE ], cTemp )
         IF ::aReport[ IMAGES, nI, 3, 8 ] == 3
            Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )
            cTemp := Ltrim( Str( ::aReport[ IMAGES, nI, 2 ] + 1 ) ) + " 0 obj" + CRLF + ;
                            "<</Length " + Ltrim( Str( ::aReport[ IMAGES, nI, 3, 10 ] ) ) + CRLF + ;
                            ">>" + CRLF + ;
                            "stream" + CRLF + ;
                            ::aReport[ IMAGES, nI, 3, 9 ] + "endstream" + CRLF + ;
                            "endobj" + CRLF
            ::aReport[ DOCLEN ] += Len( cTemp )
            Fwrite( ::aReport[ HANDLE ], cTemp )
            ++ ::aReport[ NEXTOBJ ]
         ENDIF

      ENDIF
   NEXT

   ::aReport[ REPORTOBJ ] := ::aReport[ NEXTOBJ ]

   ::aReport[ NEXTOBJ ] := ::aReport[ REPORTOBJ ] + 4

   ::aReport[ PAGEBUFFER ] := ""

RETURN nil

METHOD Pdfpagesize( _cPageSize ) CLASS TPdf

LOCAL nSize
LOCAL aSize := { { "LETTER", letter_width, letter_height }, { "LEGAL", legal_width, legal_height }, { "LEDGER", ledger_width, ledger_height }, { "A4", a4_width, a4_height }, { "A3", a3_width, a3_height } }

   DEFAULT _cPageSize TO "LETTER"

   nSize := Ascan( aSize, { | arr | arr[ 1 ] = _cPageSize } )

   IF nSize = 0     //.or. nSize > 2 //0.05
      nSize := 1
   ENDIF

   ::aReport[ PAGESIZE ] := aSize[ nSize, 1 ]

   IF ::aReport[ PAGEORIENT ] = "P"
      ::aReport[ PAGEX ] := aSize[ nSize, 2 ]
      ::aReport[ PAGEY ] := aSize[ nSize, 3 ]
   ELSE
      ::aReport[ PAGEX ] := aSize[ nSize, 3 ]
      ::aReport[ PAGEY ] := aSize[ nSize, 2 ]
   ENDIF

RETURN nil

METHOD Pdfpageorient( _cPageOrient ) CLASS TPdf

   DEFAULT _cPageOrient TO Iif( pdf_GetLandScape(), "L", "P" )

   ::aReport[ PAGEORIENT ] := _cPageOrient
   ::pdfPageSize( ::aReport[ PAGESIZE ] )

RETURN nil

METHOD Pdfsetlpi( _nLpi ) CLASS TPdf

LOCAL cLpi := Alltrim( Str( _nLpi ) )
   DEFAULT _nLpi TO 6

   cLpi             := Iif( cLpi $ "1;2;3;4;6;8;12;16;24;48", cLpi, "6" )
   ::aReport[ LPI ] := Val( cLpi )

   ::pdfPageSize( ::aReport[ PAGESIZE ] )
RETURN nil

METHOD Pdfsetfont( _cFont, _nType, _nSize, cId ) CLASS TPdf

   DEFAULT _cFont TO "Times"
   DEFAULT _nType TO 0
   DEFAULT _nSize TO 10

   HB_SYMBOL_UNUSED( cId )

   _cFont                := Upper( _cFont )
   ::aReport[ FONTSIZE ] := _nSize

   IF _cFont == "TIMES"
      ::aReport[ FONTNAME ] := _nType + 1
   ELSEIF _cFont == "HELVETICA"
      ::aReport[ FONTNAME ] := _nType + 5
   ELSE
      ::aReport[ FONTNAME ] := _nType + 9                   // 0.04
   ENDIF

   Aadd( ::aReport[ PAGEFONTS ], ::aReport[ FONTNAME ] )

   IF Ascan( ::aReport[ FONTS ], { | arr | arr[ 1 ] == ::aReport[ FONTNAME ] } ) == 0
      Aadd( ::aReport[ FONTS ], { ::aReport[ FONTNAME ], ++ ::aReport[ NEXTOBJ ] } )
   ENDIF

RETURN nil

METHOD Pdfdrawheader() CLASS TPdf

LOCAL _nFont
LOCAL _nSize
LOCAL nLen   := Len( ::aReport[ HEADER ] )

   IF nLen > 0

      // save font
      _nFont := ::aReport[ FONTNAME ]
      _nSize := ::aReport[ FONTSIZE ]

      ::aReport[ FONTNAME ] := _nFont
      ::aReport[ FONTSIZE ] := _nSize

      IF ::aReport[ MARGINS ]
         ::pdfMargins()
      ENDIF

   ELSE
      IF ::aReport[ MARGINS ]
         ::aReport[ PDFTOP ]    := 1    // top
         ::aReport[ PDFLEFT ]   := 10   // Left & right
         ::aReport[ PDFBOTTOM ] := ::aReport[ PAGEY ] * ::aReport[ LPI ] - 1    // bottom, default "LETTER", "P", 6

         ::aReport[ MARGINS ] := .f.
      ENDIF
   ENDIF
RETURN nil

METHOD Pdfmargins( nTop, nLeft, nBottom ) CLASS TPdf

   IF nTop <> NIL
      ::aReport[ PDFTOP ] := nTop
   ENDIF
   IF nLeft <> NIL
      ::aReport[ PDFLEFT ] := nLeft
   ENDIF
   IF nBottom <> NIL
      ::aReport[ PDFBOTTOM ] := nBottom
   ENDIF

   ::aReport[ MARGINS ] := .f.

RETURN nil

METHOD Pdfimageinfo( cFile ) CLASS TPdf

LOCAL cTemp := Upper( Substr( cFile, Rat( '.', cFile ) + 1 ) )
LOCAL aTemp := {}
   DO CASE
      CASE cTemp == "TIF"
         aTemp :=::pdfTIFFInfo( cFile )
      CASE cTemp == "JPG"
         aTemp :=::pdfJPEGInfo( cFile )
      CASE cTemp == "PNG"
         aTemp :=::pdfPngInfo( cFile )
   ENDCASE

RETURN aTemp

METHOD Pdftiffinfo( cFile ) CLASS TPdf

LOCAL c40        := Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
LOCAL aType      := { "BYTE", "ASCII", "SHORT", "LONG", "RATIONAL", "SBYTE", "UNDEFINED", "SSHORT", "SLONG", "SRATIONAL", "FLOAT", "DOUBLE" }
LOCAL aCount     := { 1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8 }
LOCAL nTemp
LOCAL nHandle
LOCAL cValues
LOCAL c2
LOCAL nFieldType
LOCAL nCount
LOCAL nPos
LOCAL nTag
LOCAL nValues
LOCAL nOffset
LOCAL cTemp
LOCAL cIFDNext
LOCAL nIFD
LOCAL nFields
LOCAL cTag
LOCAL nPages
LOCAL nn

LOCAL nWidth  := 0
LOCAL nHeight := 0
LOCAL nBits   := 0
LOCAL nFrom   := 0
LOCAL nLength := 0
LOCAL xRes    := 0
LOCAL yRes    := 0
LOCAL aTemp   := {}

   nHandle := Fopen( cFile )

   c2 := '  '
   Fread( nHandle, @c2, 2 )
   /*
IF c2 == 'II' .or. c2 == 'MM'
ELSE
   alert("Not II or MM")
ENDIF
*/
   Fread( nHandle, @c2, 2 )
   /*
IF c2 <> '*' + Chr(0)
   alert("Not *")
ENDIF
*/
   cIFDNext := '    '
   Fread( nHandle, @cIFDNext, 4 )

   cTemp  := Space( 12 )
   nPages := 0

   WHILE cIFDNext <> c40                //read IFD's

      nIFD := Bin2l( cIFDNext )

      Fseek( nHandle, nIFD )
      //?'*** IFD ' + LTrim(Str( ++nPages ))

      Fread( nHandle, @c2, 2 )
      nFields := Bin2i( c2 )

      FOR nn := 1 TO nFields
         Fread( nHandle, @cTemp, 12 )

         nTag       := Bin2w( Substr( cTemp, 1, 2 ) )
         nFieldType := Bin2w( Substr( cTemp, 3, 2 ) )
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
      10 = SRATIONAL Two SLONGís: the first represents the numerator of a
                     fraction, the second the denominator.
      11 = FLOAT     Single precision (4-byte) IEEE format.
      12 = DOUBLE    Double precision (8-byte) IEEE format.
      */
         nCount  := Bin2l( Substr( cTemp, 5, 4 ) )
         nOffset := Bin2l( Substr( cTemp, 9, 4 ) )

         IF nCount > 1 .OR. nFieldType == RATIONAL .OR. nFieldType == SRATIONAL
            nPos := filepos( nHandle )
            Fseek( nHandle, nOffset )

            nValues := nCount * aCount[ nFieldType ]
            cValues := Space( nValues )
            Fread( nHandle, @cValues, nValues )
            Fseek( nHandle, nPos )
         ELSE
            cValues := Substr( cTemp, 9, 4 )
         ENDIF

         IF nFieldType == ASCII
            -- nCount
         ENDIF
         //?'Tag'
         //??' ' + Padr( nTag, 10 )
         cTag := ''
         DO CASE
            CASE nTag == 256
               /*
               ImageWidth
               Tag = 256 (100.H)
               Type = SHORT or LONG
               The number of columns in the image, i.e., the number of pixels per scanline.
               */
               //??'ImageWidth'
               cTag := 'ImageWidth'
               /*
               IF nFieldType <> SHORT .and. nFieldType <> LONG
                  alert('Wrong Type FOR ImageWidth')
               ENDIF
*/
               IF nFieldType == SHORT
                  nWidth := Bin2w( Substr( cValues, 1, 2 ) )
               ELSEIF nFieldType == LONG
                  nWidth := Bin2l( Substr( cValues, 1, 4 ) )
               ENDIF

            CASE nTag == 257
               /*
               ImageLength
               Tag = 257 (101.H)
               Type = SHORT or LONG
               The number of rows (sometimes described as scanlines) in the image.
               */
               //??'ImageLength'
               cTag := 'ImageLength'
               /*
               IF nFieldType <> SHORT .and. nFieldType <> LONG
                  alert('Wrong Type FOR ImageLength')
               ENDIF
*/
               IF nFieldType == SHORT
                  nHeight := Bin2w( Substr( cValues, 1, 2 ) )
               ELSEIF nFieldType == LONG
                  nHeight := Bin2l( Substr( cValues, 1, 4 ) )
               ENDIF

            CASE nTag == 258
               /*
               BitsPerSample
               Tag = 258 (102.H)
               Type = SHORT
               The number of bits per component.
               Allowable values for Baseline TIFF grayscale images are 4 and 8, allowing either
               16 or 256 distinct shades of gray.
               */
               //??'BitsPerSample'
               cTag  := 'BitsPerSample'
               nTemp := 0
               IF nFieldType == SHORT
                  nTemp := Bin2w( cValues )
               ELSE
                  //alert('Wrong Type for BitsPerSample')
               ENDIF
               nBits := nTemp
               //IF nTemp <> 4 .and. nTemp <> 8
               //   alert('Wrong Value for BitsPerSample')
               //ENDIF
            CASE nTag == 259
               /*
               Compression
               Tag = 259 (103.H)
               Type = SHORT
               Values:
               1 = No compression, but pack data into bytes as tightly as possible, leaving no unused
               bits (except at the end of a row). The component values are stored as an array of
               type BYTE. Each scan line (row) is padded to the next BYTE boundary.
               2 = CCITT Group 3 1-Dimensional ModIFied Huffman run length encoding. See
               Section 10 for a description of ModIFied Huffman Compression.
               32773 = PackBits compression, a simple byte-oriented run length scheme. See the
               PackBits section for details.
               Data compression applies only to raster image data. All other TIFF fields are
               unaffected.
               Baseline TIFF readers must handle all three compression schemes.
               */
               //??'Compression'
               cTag  := 'Compression'
               nTemp := 0
               IF nFieldType == SHORT
                  nTemp := Bin2w( cValues )
               ELSE
                  //alert('Wrong Type for Compression')
               ENDIF
               //IF nTemp <> 1 .and. nTemp <> 2 .and. nTemp <> 32773
               //   alert('Wrong Value for Compression')
               //ENDIF
            CASE nTag == 262
               /*
               PhotometricInterpretation
               Tag = 262 (106.H)
               Type = SHORT
               Values:
               0 = WhiteIsZero. For bilevel and grayscale images: 0 is imaged as white. The maxi-mum
               value is imaged as black. This is the normal value for Compression=2.
               1 = BlackIsZero. For bilevel and grayscale images: 0 is imaged as black. The maxi-mum
               value is imaged as white. IF this value is specIFied for Compression=2, the
               image should display and print reversed.
               */
               //??'PhotometricInterpretation'
               cTag  := 'PhotometricInterpretation'
               nTemp := - 1
               IF nFieldType == SHORT
                  nTemp := Bin2w( cValues )
               ELSE
                  //alert('Wrong Type for PhotometricInterpretation')
               ENDIF
               IF nTemp <> 0 .AND. nTemp <> 1 .AND. nTemp <> 2 .AND. nTemp <> 3
                  //alert('Wrong Value for PhotometricInterpretation')
               ENDIF
            CASE nTag == 264
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
               IF nFieldType <> SHORT
                  //alert('Wrong Type for CellWidth')
               ENDIF
            CASE nTag == 265
               /*
               CellLength
               The length of the dithering or halftoning matrix used to create a dithered or
               halftoned bilevel file.
               Tag = 265 (109.H)
               Type = SHORT
               N = 1
               This field should only be present IF Threshholding = 2
               No default. See also Threshholding.
               */
               //??'CellLength'
               cTag := 'CellLength'
               IF nFieldType <> SHORT
                  //alert('Wrong Type for CellLength')
               ENDIF
            CASE nTag == 266
               /*
               FillOrder
               The logical order of bits within a byte.
               Tag = 266 (10A.H)
               Type = SHORT
               N = 1
               */
               //??'FillOrder'
               cTag := 'FillOrder'
               IF nFieldType <> SHORT
                  //alert('Wrong Type for FillOrder')
               ENDIF
            CASE nTag == 273
               /*
               StripOffsets
               Tag = 273 (111.H)
               Type = SHORT or LONG
               For each strip, the byte offset of that strip.
               */
               //??'StripOffsets'
               cTag := 'StripOffsets'
               IF nFieldType <> SHORT .AND. nFieldType <> LONG
                  //alert('Wrong Type for StripOffsets')
               ENDIF

               IF nFieldType == SHORT
                  nFrom := Bin2w( Substr( cValues, 1, 2 ) )
               ELSEIF nFieldType == LONG
                  nFrom := Bin2l( Substr( cValues, 1, 4 ) )
               ENDIF

            CASE nTag == 277
               /*
               SamplesPerPixel
               Tag = 277 (115.H)
               Type = SHORT
               The number of components per pixel. This number is 3 for RGB images, unless
               extra samples are present. See the ExtraSamples field for further information.
               */
               //??'SamplesPerPixel'
               cTag := 'SamplesPerPixel'
               IF nFieldType <> SHORT
                  //alert('Wrong Type for SamplesPerPixel')
               ENDIF
            CASE nTag == 278
               /*
               RowsPerStrip
               Tag = 278 (116.H)
               Type = SHORT or LONG
               The number of rows in each strip (except possibly the last strip.)
               For example, IF ImageLength is 24, and RowsPerStrip is 10, then there are 3
               strips, with 10 rows in the first strip, 10 rows in the second strip, and 4 rows in the
               third strip. (The data in the last strip is not padded with 6 extra rows of dummy
               data.)
               */
               //??'RowsPerStrip'
               cTag := 'RowsPerStrip'
               IF nFieldType <> SHORT .AND. nFieldType <> LONG
                  //alert('Wrong Type for RowsPerStrip')
               ENDIF
            CASE nTag == 279
               /*
               StripByteCounts
               Tag = 279 (117.H)
               Type = SHORT or LONG
               For each strip, the number of bytes in that strip after any compression.
               */
               //??'StripByteCounts'
               cTag := 'StripByteCounts'
               IF nFieldType <> SHORT .AND. nFieldType <> LONG
                  //alert('Wrong Type for StripByteCounts')
               ENDIF

               IF nFieldType == SHORT
                  nLength := Bin2w( Substr( cValues, 1, 2 ) )
               ELSEIF nFieldType == LONG
                  nLength := Bin2l( Substr( cValues, 1, 4 ) )
               ENDIF

               nLength *= nCount        // Count all strips !!!

            CASE nTag == 282
               /*
               XResolution
               Tag = 282 (11A.H)
               Type = RATIONAL
               The number of pixels per ResolutionUnit in the ImageWidth (typically, horizontal
               - see Orientation) direction.
               */
               //??'XResolution'
               cTag := 'XResolution'
               IF nFieldType <> RATIONAL
                  //alert('Wrong Type for XResolution')
               ENDIF
               xRes := Bin2l( Substr( cValues, 1, 4 ) )
            CASE nTag == 283
               /*
               YResolution
               Tag = 283 (11B.H)
               Type = RATIONAL
               The number of pixels per ResolutionUnit in the ImageLength (typically, vertical)
               direction.
               */
               //??'YResolution'
               cTag := 'YResolution'
               IF nFieldType <> RATIONAL
                  //alert('Wrong Type for YResolution')
               ENDIF
               yRes := Bin2l( Substr( cValues, 1, 4 ) )
            CASE nTag == 284
               //??'PlanarConfiguration'
               cTag := 'PlanarConfiguration'
               IF nFieldType <> SHORT
                  //alert('Wrong Type for PlanarConfiguration')
               ENDIF
            CASE nTag == 288
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
               IF nFieldType <> LONG
                  //alert('Wrong Type FOR FreeOffsets')
               ENDIF
            CASE nTag == 289
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
               IF nFieldType <> LONG
                  //alert('Wrong Type for FreeByteCounts')
               ENDIF
            CASE nTag == 296
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
               cTag  := 'ResolutionUnit'
               nTemp := 0
               IF nFieldType == SHORT
                  nTemp := Bin2w( cValues )
               ELSE
                  //alert('Wrong Type for ResolutionUnit')
               ENDIF
               IF nTemp <> 1 .AND. nTemp <> 2 .AND. nTemp <> 3
                  //alert('Wrong Value for ResolutionUnit')
               ENDIF
            CASE nTag == 305
               //??'Software'
               cTag := 'Software'
               IF nFieldType <> ASCII
                  //alert('Wrong Type for Software')
               ENDIF
            CASE nTag == 306
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
               IF nFieldType <> ASCII
                  //alert('Wrong Type for DateTime')
               ENDIF
            CASE nTag == 315
               /*
               Artist
               Person who created the image.
               Tag = 315 (13B.H)
               Type = ASCII
               Note: some older TIFF files used this tag FOR storing Copyright information.
               */
               //??'Artist'
               cTag := 'Artist'
               IF nFieldType <> ASCII
                  //alert('Wrong Type for Artist')
               ENDIF
            CASE nTag == 320
               /*
               ColorMap
               Tag = 320 (140.H)
               Type = SHORT
               N = 3 * (2**BitsPerSample)
               This field defines a Red-Green-Blue color map (often called a lookup table) for
               palette color images. In a palette-color image, a pixel value is used to index into an
               RGB-lookup table. FOR example, a palette-color pixel having a value of 0 would
               be displayed according to the 0th Red, Green, Blue triplet.
               In a TIFF ColorMap, all the Red values come first, followed by the Green values,
               then the Blue values. In the ColorMap, black is represented by 0,0,0 and white is
               represented by 65535, 65535, 65535.
               */
               //??'ColorMap'
               cTag := 'ColorMap'
               IF nFieldType <> SHORT
                  //alert('Wrong Type for ColorMap')
               ENDIF
            CASE nTag == 338
               /*
               ExtraSamples
               Description of extra components.
               Tag = 338 (152.H)
               Type = SHORT
               N = m
               */
               //??'ExtraSamples'
               cTag := 'ExtraSamples'
               IF nFieldType <> SHORT
                  //alert('Wrong Type for ExtraSamples')
               ENDIF
            CASE nTag == 33432
               /*
               Copyright
               Copyright notice.
               Tag = 33432 (8298.H)
               Type = ASCII
               Copyright notice of the person or organization that claims the copyright to the
               image. The complete copyright statement should be listed in this field including
               any dates and statements of claims. For example, ìCopyright, John Smith, 19xx.
               All rights reserved.
               */
               //??'Copyright'
               cTag := 'Copyright'
               IF nFieldType <> ASCII
                  //alert('Wrong Type for Copyright')
               ENDIF
            OTHERWISE
               //??'Unknown'
               cTag := 'Unknown'
         ENDCASE
         /*
      ??Padr( cTag, 30 )
      ??' type ' + Padr(aType[ nFieldType ], 10) + ' count ' + LTrim(Str(nCount)) + ' <'
      DO CASE
         CASE nFieldType ==  BYTE
              for nI := 1 to nCount
                  ??' ' + LTrim(Str(asc( SubStr( cValues, nI, 1 ))))
              NEXT
         CASE nFieldType ==  ASCII
              ??' '
              FOR nI := 1 to nCount
                  ??SubStr( cValues, nI, 1 )
              NEXT
         CASE nFieldType ==  SHORT
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(bin2w(SubStr( cValues, ( nI - 1 ) * 2 + 1, 2 ))))
              NEXT
         CASE nFieldType ==  LONG
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(bin2l(SubStr( cValues, ( nI - 1 ) * 4 + 1, 4 ))))
              NEXT
         CASE nFieldType ==  RATIONAL
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(bin2l(SubStr( cValues, ( nI - 1 ) * 8 + 1, 4 )))) + '/' + LTrim(Str(bin2l(SubStr( cValues, nI + 4, 4 ))))
              NEXT
         CASE nFieldType ==  SBYTE
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(asc( SubStr( cValues, nI, 1 ))))
              NEXT
         CASE nFieldType ==  UNDEFINED
              FOR nI := 1 to nCount
                  ??' ' + SubStr( cValues, nI, 1 )
              NEXT
         CASE nFieldType ==  SSHORT
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(Bin2I(SubStr( cValues, ( nI - 1 ) * 2 + 1, 2 ))))
              NEXT
         CASE nFieldType ==  SLONG
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(bin2l(SubStr( cValues, ( nI - 1 ) * 4 + 1, 4 ))))
              NEXT
         CASE nFieldType == SRATIONAL
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(bin2l(SubStr( cValues, ( nI - 1 ) * 8 + 1, 4 )))) + '/' + LTrim(Str(bin2l(SubStr( cValues, nI + 4, 4 ))))
              NEXT
         CASE nFieldType == FLOAT
         CASE nFieldType == DOUBLE
              FOR nI := 1 to nCount
                  ??' ' + LTrim(Str(ctof(SubStr( cValues, ( nI - 1 ) * 8 + 1, 8 ))))
              NEXT

      ENDCASE
      ??' >'
      */
      NEXT
      Fread( nHandle, @cIFDNext, 4 )
   ENDDO

   Fclose( nHandle )

   Aadd( aTemp, nWidth )
   Aadd( aTemp, nHeight )
   Aadd( aTemp, xRes )
   Aadd( aTemp, yRes )
   Aadd( aTemp, nBits )
   Aadd( aTemp, nFrom )
   Aadd( aTemp, nLength )
   Aadd( aTemp, 0 )

RETURN aTemp

METHOD Pdfjpeginfo( cFile ) CLASS TPdf

LOCAL c255
LOCAL nAt
LOCAL nHandle
LOCAL nWidth  := 0
LOCAL nHeight := 0
LOCAL nBits   := 8
LOCAL nFrom   := 0
LOCAL nLength := 0
LOCAL xRes    := 0
LOCAL yRes    := 0
LOCAL aTemp   := {}

   nHandle := Fopen( cFile )

   c255 := Space( 1024 )
   Fread( nHandle, @c255, 1024 )

   xRes := Asc( Substr( c255, 15, 1 ) ) * 256 + Asc( Substr( c255, 16, 1 ) )
   yRes := Asc( Substr( c255, 17, 1 ) ) * 256 + Asc( Substr( c255, 18, 1 ) )

   nAt     := At( Chr( 255 ) + Chr( 192 ), c255 ) + 5
   nHeight := Asc( Substr( c255, nAt, 1 ) ) * 256 + Asc( Substr( c255, nAt + 1, 1 ) )
   nWidth  := Asc( Substr( c255, nAt + 2, 1 ) ) * 256 + Asc( Substr( c255, nAt + 3, 1 ) )

   Fclose( nHandle )

   nLength := filesize( cFile )

   Aadd( aTemp, nWidth )
   Aadd( aTemp, nHeight )
   Aadd( aTemp, xRes )
   Aadd( aTemp, yRes )
   Aadd( aTemp, nBits )
   Aadd( aTemp, nFrom )
   Aadd( aTemp, nLength )
   aadd( aTemp,2)

RETURN aTemp

METHOD Pdfimage( cFile, nRow, nCol, nHeight, nWidth, cId, Scalex, Scaley )

   DEFAULT nRow TO ::aReport[ REPORTLINE ]
   DEFAULT nCol TO 0
   DEFAULT nHeight TO 0
   DEFAULT nWidth TO 0

   DEFAULT cId TO ""
   DEFAULT scalex TO 0, sCaley TO 0

   Aadd( ::aReport[ PAGEIMAGES ], { cFile, nRow, nCol, nHeight, nWidth, scalex, scaley } )

RETURN nil

METHOD Pdfatsay( cString, nRow, nCol, lExact, cId ) CLASS TPdf

LOCAL _nFont
LOCAL lReverse
LOCAL nAt

   DEFAULT nRow TO ::aReport[ REPORTLINE ]
   DEFAULT lExact TO .f.
   DEFAULT cId TO ""

   IF ( nAt := At( "#pagenumber#", cString ) ) > 0
      cString := Left( cString, nAt - 1 ) + Ltrim( Str( ::pdfPageNum() ) ) + Substr( cString, nAt + 12 )
   ENDIF

   lReverse := .f.
   IF !Empty( cString )

      IF ::aReport[ UNDERLINE ]
         ::Pdfunder( cString, Len( cString ), nCol, nRow )
      ENDIF

      IF ::aReport[ OVERLINE ]
         ::PdfOver( cString, Len( cString ), nCol, nRow )
      ENDIF

      cString :=::pdfStringB( cString )

      _nFont := Ascan( ::aReport[ FONTS ], { | arr | arr[ 1 ] == ::aReport[ FONTNAME ] } )

      IF ::aReport[ FONTNAME ] <> ::aReport[ FONTNAMEPREV ]
         ::aReport[ FONTNAMEPREV ] := ::aReport[ FONTNAME ]
         ::aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + Ltrim( Str( _nFont ) ) + " " + Ltrim( Transform( ::aReport[ FONTSIZE ], "999.99" ) ) + " Tf " + Ltrim( Transform( nCol, "9999.99" ) ) + " " + Ltrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ELSEIF ::aReport[ FONTSIZE ] <> ::aReport[ FONTSIZEPREV ]
         ::aReport[ FONTSIZEPREV ] := ::aReport[ FONTSIZE ]
         ::aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + Ltrim( Str( _nFont ) ) + " " + Ltrim( Transform( ::aReport[ FONTSIZE ], "999.99" ) ) + " Tf " + Ltrim( Transform( nCol, "9999.99" ) ) + " " + Ltrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ELSE

         ::aReport[ PAGEBUFFER ] += CRLF + "BT " + Ltrim( Transform( nCol, "9999.99" ) ) + " " + Ltrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ENDIF
      IF lReverse
         ::aReport[ PAGEBUFFER ] += " 0 g "
      ENDIF
   ENDIF
RETURN nil

METHOD Pdfstringb( cString ) CLASS TPdf

   cString := Strtran( cString, "(", "\(" )
   cString := Strtran( cString, ")", "\)" )

RETURN cString

METHOD Pdfgetfontinfo( cParam ) CLASS TPdf

LOCAL cRet
   IF cParam == "NAME"
      IF Left( ::aReport[ TYPE1, ::aReport[ FONTNAME ] ], 5 ) == "Times"
         cRet := "Times"
      ELSEIF Left( ::aReport[ TYPE1, ::aReport[ FONTNAME ] ], 9 ) == "Helvetica"
         cRet := "Helvetica"
      ELSE
         cRet := "Courier"              // 0.04
      ENDIF
   ELSE             // size
      cRet := Int( ( ::aReport[ FONTNAME ] - 1 ) % 4 )
   ENDIF
RETURN cRet

METHOD Pdfpagenum( n ) CLASS TPdf

   DEFAULT n TO 0
   IF n > 0
      ::aReport[ REPORTPAGE ] := n      // NEW !!!
   ENDIF
RETURN ::aReport[ REPORTPAGE ]

METHOD Pdfclose() CLASS TPdf

LOCAL nI
LOCAL cTemp
LOCAL nObj1
LOCAL nBooklen

FIELD FIRST, PREV, NEXT, LAST, COUNT, PARENT, PAGE, COORD, TITLE, LEVEL

   ::pdfClosePage()

   // kids
   ::aReport[ REFS, 2 ] := ::aReport[ DOCLEN ]
   cTemp                := ;
                          "1 0 obj" + CRLF + ;
                          "<<" + CRLF + ;
                          "/Type /Pages /Count " + Ltrim( Str( ::aReport[ REPORTPAGE ] ) ) + CRLF + ;
                          "/Kids ["

   FOR nI := 1 TO ::aReport[ REPORTPAGE ]
      cTemp += " " + Ltrim( Str( ::aReport[ PAGES, nI ] ) ) + " 0 R"
   NEXT

   cTemp += " ]" + CRLF + ;
           ">>" + CRLF + ;
           "endobj" + CRLF

   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   // info
   ++ ::aReport[ REPORTOBJ ]
   Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )
   cTemp := Ltrim( Str( ::aReport[ REPORTOBJ ] ) ) + " 0 obj" + CRLF + ;
                   "<< /Title ()" + CRLF + ;
                   "/Producer ()" + CRLF + ;
                   Iif( !Empty( ::aReport[ AUTHOR ] ), "/Author (" + ::aReport[ AUTHOR ] + ')', "/Author ()" ) + CRLF + ;
                   "/Creator ()" + CRLF + ;
                   Iif( !Empty( ::aReport[ SUBJECT ] ), "/Subject (" + ::aReport[ SUBJECT ] + ')', "/Subject ()" ) + CRLF + ;
                   Iif( !Empty( ::aReport[ KEYWORD ] ), "/Keywords (" + ::aReport[ KEYWORD ] + ')', "/Keywords ()" ) + CRLF + ;
                   "/CreationDate (D:" + Str( Year( Date() ), 4 ) + Padl( Month( Date() ), 2, "0" ) + Padl( Day( Date() ), 2, "0" ) + Substr( Time(), 1, 2 ) + Substr( Time(), 4, 2 ) + Substr( Time(), 7, 2 ) + ")" + CRLF + ;
                   ">>" + CRLF + ;
                   "endobj" + CRLF
   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   // root
   ++ ::aReport[ REPORTOBJ ]
   Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )
   cTemp := Ltrim( Str( ::aReport[ REPORTOBJ ] ) ) + " 0 obj" + CRLF + ;
                   "<< /Type /Catalog /Pages 1 0 R /Outlines " + Ltrim( Str( ::aReport[ REPORTOBJ ] + 1 ) ) + " 0 R" + Iif( ( nBookLen := Len( ::aReport[ BOOKMARK ] ) ) > 0, " /PageMode /UseOutlines", "" ) + " >>" + CRLF + "endobj" + CRLF
   ::aReport[ DOCLEN ] += Len( cTemp )
   Fwrite( ::aReport[ HANDLE ], cTemp )

   ++ ::aReport[ REPORTOBJ ]
   nObj1 := ::aReport[ REPORTOBJ ]

   IF nBookLen == 0
      cTemp := Ltrim( Str( ::aReport[ REPORTOBJ ] ) ) + " 0 obj" + CRLF + "<< /Type /Outlines /Count 0 >>" + CRLF + "endobj" + CRLF
      Aadd( ::aReport[ REFS ], ::aReport[ DOCLEN ] )
      ::aReport[ DOCLEN ] += Len( cTemp )
      Fwrite( ::aReport[ HANDLE ], cTemp )
   ENDIF

   cTemp := CRLF
   ::aReport[ DOCLEN ] += Len( cTemp )

   ++ ::aReport[ REPORTOBJ ]
   cTemp += "xref" + CRLF + ;
           "0 " + Ltrim( Str( ::aReport[ REPORTOBJ ] ) ) + CRLF + ;
           Padl( ::aReport[ REFS, 1 ], 10, "0" ) + " 65535 f" + CRLF

   FOR nI := 2 TO Len( ::aReport[ REFS ] )
      cTemp += Padl( ::aReport[ REFS, nI ], 10, "0" ) + " 00000 n" + CRLF
   NEXT

   cTemp += "trailer << /Size " + Ltrim( Str( ::aReport[ REPORTOBJ ] ) ) + " /Root " + Ltrim( Str( nObj1 - 1 ) ) + " 0 R /Info " + Ltrim( Str( nObj1 - 2 ) ) + " 0 R >>" + CRLF + ;
                                         "startxref" + CRLF + ;
                                         Ltrim( Str( ::aReport[ DOCLEN ] ) ) + CRLF + ;
                                         "%%EOF" + CRLF
   Fwrite( ::aReport[ HANDLE ], cTemp )
   /*
   IF ::aReport[ OPTIMIZE ]
      pdfOptimize( ) coming !
   ENDIF
*/
   Fclose( ::aReport[ HANDLE ] )

   ::aReport := nil

RETURN nil
/*

/* new methods added by Luiz Rafael Culik */

METHOD Getpagetype( nWidth, nHeight ) CLASS TPdf

LOCAL nPos
LOCAL aSize := { { "LETTER", letter_width, letter_height }, { "LEGAL", legal_width, legal_height }, { "LEDGER", ledger_width, ledger_height }, { "A4", a4_width, a4_height }, { "A3", a3_width, a3_height } }

   nPos := Ascan( aSize, { | x | x[ 2 ] == nWidth .AND. x[ 3 ] == nHeight } )

   IF nPos == 0
      nPos := Ascan( aSize, { | x | x[ 3 ] == nWidth .AND. x[ 2 ] == nHeight } )
   ENDIF

RETURN aSize[ nPos, 1 ]

METHOD Pdf_Rect( x, y, width, height ) CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Str( x ) ) + " " + ;
           Ltrim( Str( y ) ) + " " + ;
           Ltrim( Str( width ) ) + " " + ;
           Ltrim( Str( height ) ) + " " + ;
           " re" + ;
           CRLF
RETURN nil

METHOD Pdf_Stroke() CLASS TPdf

   ::aReport[ PAGEBUFFER ] += "S" + CRLF

RETURN nil

METHOD Setoverline( l ) CLASS TPdf

LOCAL Res := ::aReport[ OVERLINE ]
   ::aReport[ OVERLINE ] := l

RETURN Res

METHOD Setunderline( l ) CLASS TPdf

LOCAL Res := ::aReport[ UNDERLINE ]
   ::aReport[ UNDERLINE ] = l

RETURN Res

METHOD Pdfmoveto( X, Y ) CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Str( x ) ) + " " + ;
           Ltrim( Str( y ) ) + " " + ;
           "m" + CRLF
RETURN nil

METHOD Pdflineto( X, Y ) CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Str( x ) ) + " " + ;
           Ltrim( Str( y ) ) + " " + ;
           "l" + CRLF
RETURN NIL

METHOD Pdfsetlinewidth( w ) CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Str( w ) ) + " " + ;
           "w" + CRLF
RETURN nil

METHOD Pdfsetlinecap( w ) CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Str( w ) ) + " " + ;
           "J" + CRLF
RETURN nil

METHOD Pdfsetdash( b, w ) CLASS TPdf

   IF b == 0 .AND. w == 0
      ::aReport[ PAGEBUFFER ] += CRLF + ;
              "[] " + ;
              "0 d"
   ELSE
      ::aReport[ PAGEBUFFER ] += CRLF + ;
              "[" + Ltrim( Str( b ) ) + " " + ;
              Ltrim( Str( w ) ) + "]" + ;
              "0 d"
   ENDIF

RETURN nil

METHOD Pdfsave( lBuf ) CLASS TPdf

LOCAL cBuf := CRLF + "q"
   DEFAULT lBuf TO .t.

   IF !lBuf
      RETURN cBuf
   ELSE
      ::aReport[ PAGEBUFFER ] += cBuf
   ENDIF

RETURN nil

METHOD Pdfrestore( lBuf ) CLASS TPdf

LOCAL cBuf := CRLF + "Q "
   DEFAULT lBuf TO .t.

   IF !lBuf
      RETURN cBuf
   ELSE
      ::aReport[ PAGEBUFFER ] += cBuf
   ENDIF

RETURN nil

METHOD Pdfunder( Text, Len, X, Y ) CLASS TPdf

LOCAL xScale     := 1
LOCAL yScale     := 1
LOCAL length
LOCAL nWidth     := 0.00
LOCAL nLineWidth
LOCAL Delta_y

   IF Right( Text, 1 ) == Chr( 255 ) .OR. Right( Text, 1 ) == Chr( 254 )        // reverse or underline
      -- len
   ENDIF

   nLineWidth := ::aReport[ FONTSIZE ] * 50 / 1000 * ;
           ( 1 / 100 ) * Iif( xScale > yScale, xScale, yScale )

   /* the font size may be negative, resulting in bad line width */
   nLineWidth := Abs( nLineWidth )
   length     :=::Pdf_Str_Width( text, len, 0, ::aReport[ FONTSIZE ] )

   IF ( length == 0 )
      RETURN nil
   ENDIF

   Delta_y := ( ::aReport[ FONTSIZE ] * - 100 / 1000 + 0 ) * ;
                Abs( 1 ) / 100 * Iif( xScale > yScale, xScale, yScale )

   ::Pdfsave()
   ::PDFSetLineWidth( 1 )
   ::PdfSetLineCap( 0 )
   ::PdfSetDash( 0, 0 )
   ::PDFMoveTo( x, y )
   ::PdfLineTo( x + length, y )
   ::Pdf_Stroke()
   ::PdfRestore()

RETURN nil

METHOD Pdfover( Text, Len, x, y ) CLASS TPdf

LOCAL xScale     := 1
LOCAL yScale     := 1
LOCAL length
LOCAL Delta_y
LOCAL nWidth     := 0.00
LOCAL nLineWidth

   IF Right( Text, 1 ) == Chr( 255 ) .OR. Right( Text, 1 ) == Chr( 254 )        // reverse or underline
      -- Len
   ENDIF
   nLineWidth := ::aReport[ FONTSIZE ] * 50 / 1000 * ;
           ( 1 / 100 ) * Iif( xScale > yScale, xScale, yScale )

   /* the font size may be negative, resulting in bad line width */
   nLineWidth := Abs( nLineWidth )

   length :=::pdf_str_width( text, len, 0, ::aReport[ FONTSIZE ] )

   IF ( length == 0 )
      RETURN nil
   ENDIF

   Delta_y := ( ::aReport[ FONTSIZE ] * - 100 / 1000 + 0 ) * ;
                Abs( 1 ) / 100 * Iif( xScale > yScale, xScale, yScale )
   ::PdfSave()
   ::PDFSetLineWidth( 1 )
   ::PdfSetLineCap( 0 )
   ::PdfSetDash( 0, 0 )
   ::PDFMoveTo( x, y + delta_y )
   ::PdfLineTo( x + length, y + delta_y )
   ::Pdf_Stroke()
   ::PdfRestore()

RETURN nil

METHOD Pdf_Str_Width( Text, Len, Font, Size ) CLASS TPdf

LOCAL cp
LOCAL i
LOCAL width  := 0.0
LOCAL nWidth := 0.00
LOCAL nArr
LOCAL nAdd   := ( ::aReport[ FONTNAME ] - 1 ) % 4

   HB_SYMBOL_UNUSED( Font )

   IF ::pdfGetFontInfo( "NAME" ) = "Times"
      nArr := 1
   ELSEIF ::pdfGetFontInfo( "NAME" ) = "Helvetica"
      nArr := 2
   ELSE
      nArr := 3     // 0.04
   ENDIF

   IF ( Len == 0 )
      Len := Len( text )
   ENDIF

   /* We cannot handle CID fonts */

   FOR EACH cp IN TEXT
      i := hb_enumindex()
      /* take word spacing parameter into account at each blank */
      IF ( cp == " " )
         width += 1
      ENDIF
      /* individual character width plus character spacing parameter */
      width += size * ::aReport[ FONTWIDTH, nArr, ( Asc( Substr( Text, i, 1 ) ) - 32 ) * 4 + 1 + nAdd ] / 1000

   NEXT

   /* take current text matrix and horizontal scaling factor into account */
   width *= 1

RETURN width

METHOD Pdfsetrgbcolor( R, G, B ) CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Transform( R / 255, "9.9999" ) ) + " " + ;
           Ltrim( Transform( G / 255, "9.9999" ) ) + " " + ;
           Ltrim( Transform( B / 255, "9.9999" ) ) + " rg " + CRLF + ;
           Ltrim( Transform( R / 255, "9.9999" ) ) + " " + ;
           Ltrim( Transform( G / 255, "9.9999" ) ) + " " + ;
           Ltrim( Transform( B / 255, "9.9999" ) ) + " RG "

RETURN NIL

METHOD Pdfscale( X, Y, lBuf ) CLASS TPdf

   DEFAULT lBuf TO .t.
   IF !lBuf
      RETURN CRLF + ;
              Ltrim( Str( x ) ) + " 0 0 " + ;
              Ltrim( Str( y ) ) + " 0 0 cm"
   ELSE
      ::aReport[ PAGEBUFFER ] += CRLF + ;
              Ltrim( Str( x ) ) + " 0 0 " + ;
              Ltrim( Str( y ) ) + " 0 0 cm"
   ENDIF

RETURN NIL

METHOD Pdftranslate( tX, tY ) CLASS TPdf

   IF ( tX == 0 .AND. tY == 0 )
      RETURN NIL
   ENDIF

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           " 1 0 0 1 " + Ltrim( Str( Tx ) ) + ;
           Ltrim( Str( tY ) ) + " cm"
RETURN NIL

METHOD Pdfpnginfo( cFile ) CLASS TPdf

LOCAL c255
LOCAL nHandle
LOCAL nWidth       := 0
LOCAL nHeight      := 0
LOCAL nBits        := 8
LOCAL nFrom        := 135
LOCAL nPLTEPos
LOCAL nDistTonPhys := 0
LOCAL nPhys
LOCAL nLength      := filesize( cFile )
LOCAL xRes         := 0
LOCAL cPalletData  := ""
LOCAL yRes         := 0
LOCAL aTemp        := {}
LOCAL nIendPos     := nLength - 16
LOCAL nb
LOCAL cData        := ""
LOCAL nColor

   nHandle := Fopen( cFile )

   c255 := Space( 1024 )
   Fread( nHandle, @c255, 1024 )

   //   xRes := asc(SubStr( c255, 15, 1 )) * 256 + asc(SubStr( c255, 16, 1 ))
   //   yRes := asc( SubStr( c255, 17, 1 )) * 256 + asc(SubStr( c255, 18, 1 ))

   //   nAt := At( Chr(255) + Chr(192), c255 ) + 5
   //   nHeight := asc(SubStr( c255, nAt, 1 )) * 256 + asc(SubStr( c255, nAt + 1, 1 ))
   //   nWidth := asc( SubStr( c255, nAt + 2, 1 )) * 256 + asc(SubStr( c255, nAt + 3, 1 ))
   nPLTEPos := At( "PLTE", c255 )

   IF nPLTEPos > 0
      nPhys := At( 'pHYs', c255 )
      nFrom := At( "IDAT", c255 ) + 3

      nPLTEPos     += 4
      nDistTonPhys := 768
      cPalletData  := Substr( c255, nPLTEPos, nDistTonPhys )

   ENDIF

   xRes   := ( Asc( Substr( c255, 17, 1 ) ) ) + ( Asc( Substr( c255, 18, 1 ) ) ) + ( Asc( Substr( c255, 19, 1 ) ) ) + ( Asc( Substr( c255, 20, 1 ) ) )
   yRes   := ( Asc( Substr( c255, 21, 1 ) ) ) + ( Asc( Substr( c255, 22, 1 ) ) ) + ( Asc( Substr( c255, 23, 1 ) ) ) + ( Asc( Substr( c255, 24, 1 ) ) )
   nBits  := ( Asc( Substr( c255, 25, 1 ) ) )
   nColor := ( Asc( Substr( c255, 26, 1 ) ) )
   cData  := Substr( c255, At( "IDAT", c255 ) + 4 )
   nb     := At( "IDAT", c255 )

   Fclose( nHandle )

   nLength := filesize( cFile )

   Aadd( aTemp, xRes )
   Aadd( aTemp, yRes )
   Aadd( aTemp, xRes )
   Aadd( aTemp, yRes )
   Aadd( aTemp, nBits )
   Aadd( aTemp, nFrom )
   Aadd( aTemp, nLength - nFrom - 16 )
   Aadd( aTemp, nColor )
   Aadd( aTemp, cPalletData )
   Aadd( aTemp, nDistTonPhys )

   RETURN aTemp

#define PDF_M_PI        3.14159265358979323846      /* pi */
METHOD Pdfrotate( pHi ) CLASS TPdf

LOCAL c
LOCAL s

   IF pHi == 0
      RETURN NIL
   ENDIF
   phi := ( phi * PDF_M_PI / 180 ) /* convert to radians */
   c   := cos( phi )
   s   := sin( phi )

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Str( c ) ) + " " + ;
           Ltrim( Str( s ) ) + " " + ;
           Ltrim( Str( - s ) ) + " " + ;
           Ltrim( Str( c ) ) + " 0 0 cm"

RETURN NIL

METHOD Pdfclosepath() CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + "h"
RETURN NIL

METHOD Pdfendpath() CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + "n"
RETURN NIL

METHOD Pdfcurveto( x1, y1, x2, y2, x3, y3 ) CLASS TPdf

   IF ( x2 == x3 .AND. y2 == y3 ) /* second c.p. coincides with final point */
      ::aReport[ PAGEBUFFER ] += CRLF + Ltrim( Str( x1 ) ) + " " + ;
              Ltrim( Str( y1 ) ) + " " + ;
              Ltrim( Str( x3 ) ) + " " + ;
              Ltrim( Str( y3 ) ) + " y"

   ELSE /* general case with four distinct points */
      ::aReport[ PAGEBUFFER ] += CRLF + Ltrim( Str( x1 ) ) + " " + ;
              Ltrim( Str( y1 ) ) + " " + ;
              Ltrim( Str( x2 ) ) + " " + ;
              Ltrim( Str( y2 ) ) + " " + ;
              Ltrim( Str( x3 ) ) + " " + ;
              Ltrim( Str( y3 ) ) + " c"

   ENDIF
RETURN NIL

METHOD Pdfcomplevel( x ) CLASS TPdf

   ::aReport[ PDFCOMPLVL ] := x
RETURN NIL
METHOD Pdfinfo( cTitle, cAuthor, cKey, cCreator ) CLASS TPdf

   DEFAULT cAuthor TO ""
   DEFAULT cTitle TO ""
   DEFAULT cKey TO "", cCreator TO ""
   ::aReport[ CREATOR ] := cCreator
   ::aReport[ KEYWORD ] := cKey
   ::aReport[ AUTHOR ]  := cAuthor
   ::aReport[ SUBJECT ] := cTitle
RETURN NIL

STATIC FUNCTION FilePos( nHandle )

RETURN ( Fseek( nHandle, 0, FS_RELATIVE ) )
STATIC FUNCTION pdfM2X( n ) /*

//‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹                                                   */
RETURN n
METHOD PDF_setrgbcolor_fill( R, G, B ) CLASS TPdf

   ::aReport[ PAGEBUFFER ] += CRLF + ;
           Ltrim( Transform( R / 255, "9.9999" ) ) + " " + ;
           Ltrim( Transform( G / 255, "9.9999" ) ) + " " + ;
           Ltrim( Transform( B / 255, "9.9999" ) ) + " rg "

RETURN nil
