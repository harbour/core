/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * BAR engine library class
 *
 * Copyright 2005 Laverson Espindola <laverson.espindola@gmail.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "gd.ch"
#include "hbclass.ch"

#define IMG_FORMAT_PNG   1
#define IMG_FORMAT_JPEG  2
#define IMG_FORMAT_WBMP  4
#define IMG_FORMAT_GIF   8
#define IMG_QUALITY      95

#define THICKNESS_I250   0
#define THICKNESS_I251   0

CREATE CLASS GDBar FROM GDImage

   // class attributes
   VAR positionX      AS NUMERIC INIT  4
   VAR positionY      AS NUMERIC
   VAR maxHeight      AS NUMERIC INIT 25
   VAR maxHDefa       AS NUMERIC INIT 25
   VAR lastX          AS NUMERIC
   VAR lastY          AS NUMERIC
   VAR error          AS NUMERIC
   VAR imWidth        AS NUMERIC

   // Barcode attributes
   VAR Parity
   VAR LeftHand_Even  AS ARRAY
   VAR Right_Hand     AS ARRAY
   VAR LeftHand_Odd   AS ARRAY
   VAR keys           AS ARRAY

   VAR book           AS LOGICAL INIT .F.
   VAR acode          AS ARRAY
   VAR KeysModeA      AS CHARACTER
   VAR KeysModeB      AS CHARACTER
   VAR KeysModeC      AS ARRAY

   // image attributes
   VAR res            AS NUMERIC
   VAR textfont       AS NUMERIC
   VAR TEXT           AS CHARACTER
   VAR filename       AS CHARACTER
   VAR color_b        AS ARRAY
   VAR color_f        AS ARRAY
   VAR FillColor      AS NUMERIC
   VAR BackColor      AS NUMERIC
   VAR lDrawValue     AS LOGICAL INIT .T.

   // Methods

   METHOD CreateBar( sx, sy, filename, cColor )
   METHOD Configure( nMaxHeight, aFillColor, aBackColor, nRes, nTextFont, lBook, lDrawValue )
   METHOD Allocate()
   METHOD DrawError( ptext )
   METHOD DrawSingleBar( pcode )
   METHOD DrawSingleI25( pcode )
   METHOD DrawText( lIsI25 )
   METHOD nextX( lI25 )
   METHOD Finish( image_style, quality, nFG )
   METHOD SetText( ptext )
   METHOD ResetColor()
   METHOD CheckCode()
   METHOD CheckValInArray( cChar )

ENDCLASS

METHOD CreateBar( sx, sy, filename, cColor ) CLASS GDBar

   ::Create( sx, sy )

   hb_default( @cColor, { 255, 255, 255 } )

   ::SetColor( cColor[ 1 ], cColor[ 2 ], cColor[ 3 ] )

   ::error     := 0
   ::positionY := 0
   ::imWidth   := sx

   IF ! Empty( filename )
      ::filename := filename
   ENDIF

   ::FillColor := ::SetColor( ::color_f[ 1 ], ::color_f[ 2 ], ::color_f[ 3 ] )
   ::BackColor := ::SetColor( ::color_b[ 1 ], ::color_b[ 2 ], ::color_b[ 3 ] )

   ::Setfont( "Arial" )

   // configures Fontes
   IF     ::textfont == 1 ; ::SetFontSmall()
   ELSEIF ::textfont == 2 ; ::SetFontLarge()
   ELSEIF ::textfont == 3 ; ::SetFontMediumBold()
   ELSEIF ::textfont == 4 ; ::SetFontGiant()
   ELSEIF ::textfont == 5 ; ::SetFontTiny()
   ENDIF

   ::SetFontPitch( ::textfont )

   // always restores
   ::maxHeight := ::maxHDefa

   RETURN Self

METHOD Configure( nMaxHeight, aFillColor, aBackColor, nRes, nTextFont, lBook, lDrawValue ) CLASS GDBar

   hb_default( @lBook      , .F. )
   hb_default( @lDrawValue , .T. )
   hb_default( @nMaxHeight , 25 )
   hb_default( @nTextFont  , 2 )
   hb_default( @nRes       , 2 )
   hb_default( @aBackColor , { 255, 255, 255 } )
   hb_default( @aFillColor , { 0, 0, 0 } )

   ::book       := lBook
   ::maxHeight  := nMaxHeight
   ::res        := nRes
   ::textfont   := nTextFont
   ::lDrawValue := lDrawValue

   ::color_b    := AClone( aBackColor )
   ::color_f    := AClone( aFillColor )

   RETURN NIL

METHOD SetText( ptext ) CLASS GDBar

   ::text := ptext

   RETURN NIL

METHOD ResetColor() CLASS GDBar

   ::FillColor := ::SetColor( ::color_f[ 1 ], ::color_f[ 2 ], ::color_f[ 3 ] )
   ::BackColor := ::SetColor( ::color_b[ 1 ], ::color_b[ 2 ], ::color_b[ 3 ] )

   RETURN NIL

METHOD Allocate() CLASS GDBar

   LOCAL R := ::color_b[ 1 ]
   LOCAL G := ::color_b[ 2 ]
   LOCAL B := ::color_b[ 3 ]

   RETURN ::SetColor( R, G, B )

METHOD DrawSingleBar( pcode ) CLASS GDBar

   LOCAL i
   LOCAL j

   FOR j := 1 TO Len( pcode )

      FOR i := 1 TO ::res
         ::Line( ::positionX + i, ::positionY, ::positionX + i, ::positionY + ::maxHeight, ;
            iif( SubStr( pcode, j, 1 ) $ "0", ::BackColor, ::FillColor  ) )
      NEXT

      ::NextX()
   NEXT

   RETURN NIL

METHOD DrawSingleI25( pcode ) CLASS GDBar

   LOCAL j

   LOCAL widthSlimBar  := 1
   LOCAL widthFatBar   := 3

   LOCAL imgBar
   LOCAL imgWid
   LOCAL end_y
   LOCAL qw

   ::positionX := 10

   FOR j := 1 TO Len( pcode )

      imgBar := iif( j % 2 == 0, ::FillColor, ::BackColor )
      imgWid := iif( SubStr( pcode, j, 1 ) == "0", widthSlimBar, widthFatBar )

      end_y := ::maxHeight

      FOR qw := 1 TO imgWid
         ::Line( ::positionX, 1, ::positionX, end_y, imgBar )
         ::nextX( .T. )
      NEXT
   NEXT

   RETURN NIL

METHOD DrawError( ptext ) CLASS GDBar

   ::Say( 5, ::error * 15, ptext, ::FillColor )

   ::error++

   ::lastX := Max( ::GetFontWidth() * Len( ptext ), ::lastX )
   ::lastY := ::error * 15

   RETURN NIL

METHOD nextX( lI25 ) CLASS GDBar

   hb_default( @li25, .F. )

   IF li25
      ::positionX++
   ELSE
      ::positionX += ::res
   ENDIF

   RETURN NIL

METHOD DrawText( lIsI25 ) CLASS GDBar

   LOCAL xPosition

   hb_default( @lIsI25, .F. )

   IF lIsI25
      If ::textfont != 0
         xPosition := 10 * ::GetFontWidth()
         ::say( xPosition, ::maxHeight, "*" + ::text + "*", ::FillColor )
         ::lastY := ::maxHeight + ::GetFontHeight()
      ENDIF
   ELSE
      If ::textfont != 0
         xPosition := ( ::positionX / 2 ) - ( Len( ::text ) / 2 ) * ::GetFontWidth()
         ::say( xPosition, ::maxHeight, ::text, ::FillColor )
         ::lastY := ::maxHeight + ::GetFontHeight()
      ENDIF
   ENDIF

   RETURN .T.

METHOD CheckCode() CLASS GDBar

   LOCAL lRet := .T.
   LOCAL i

   FOR i := 1 TO Len( ::text )
      IF HB_ISSTRING( ::CheckValInArray( SubStr( ::text, i, 1 ) ) )
         ::DrawError( "Character " + SubStr( ::text, i, 1 ) + " not allowed." )
         lRet := .F.
      ENDIF
   NEXT

   RETURN lRet

METHOD CheckValInArray( cChar ) CLASS GDBar

   LOCAL nPos := AScan( ::keys, {| x | SubStr( x, 1, 1 ) == cChar } )

   RETURN iif( nPos > 0, nPos, NIL )

METHOD Finish( image_style, quality, nFG ) CLASS GDBar

   hb_default( @image_style, IMG_FORMAT_PNG )
   hb_default( @quality    , 95 )
   hb_default( @nFG        , { 255, 255, 255 } )

   IF Empty( ::filename ) .OR. ::filename == NIL

      // Output std handle == 1

      // ::filename := ::text
      IF image_style == IMG_FORMAT_PNG
         ::OutputPng()
      ELSEIF image_style == IMG_FORMAT_JPEG
         ::OutputJpeg( , quality )
      ELSEIF image_style == IMG_FORMAT_WBMP
         ::OutputWBmp( , nFG )
      ELSEIF image_style == IMG_FORMAT_GIF
         ::OutputGif()
      ENDIF
   ELSE
      IF image_style == IMG_FORMAT_PNG
         ::SavePng(  ::filename )
      ELSEIF image_style == IMG_FORMAT_JPEG
         ::Savejpeg( ::filename, quality )
      ELSEIF image_style == IMG_FORMAT_WBMP
         ::SaveWBmp( ::filename, nFG )
      ELSEIF image_style == IMG_FORMAT_GIF
         ::SaveGif( ::filename )
      ENDIF
   ENDIF

   RETURN .T.
