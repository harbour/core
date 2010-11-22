/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * BAR engine library class
 *
 * Copyright 2005-2005 Laverson Espindola <laverson.espindola@gmail.com>
 * www - http://www.xharbour.org http://harbour-project.org
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

#include "gd.ch"
#include "hbclass.ch"
#include "common.ch"

#define IMG_FORMAT_PNG   1
#define IMG_FORMAT_JPEG  2
#define IMG_FORMAT_WBMP  4
#define IMG_FORMAT_GIF   8
#define IMG_QUALITY      95

#define THICKNESS_I250   0
#define THICKNESS_I251   0

CREATE CLASS TBarCode FROM GDImage

   // class attributes
   DATA positionX AS NUMERIC INIT  4
   DATA positionY AS NUMERIC
   DATA maxHeight AS NUMERIC INIT 25
   DATA maxHDefa  AS NUMERIC INIT 25
   DATA lastX     AS NUMERIC
   DATA lastY     AS NUMERIC
   DATA error     AS NUMERIC
   DATA imWidth   AS NUMERIC

   // Barcode attributes
   DATA Parity
   DATA LeftHand_Even  AS ARRAY
   DATA Right_Hand     AS ARRAY
   DATA LeftHand_Odd   AS ARRAY
   DATA keys           AS ARRAY

   DATA book      AS LOGICAL INIT .F.
   DATA acode     AS ARRAY
   DATA KeysModeA AS CHARACTER
   DATA KeysModeB AS CHARACTER
   DATA KeysModeC AS ARRAY

   // image attributes
   DATA res          AS NUMERIC
   DATA textfont     AS NUMERIC
   DATA text         AS CHARACTER
   DATA filename     AS CHARACTER
   DATA color_b      AS ARRAY
   DATA color_f      AS ARRAY
   DATA FillColor    AS NUMERIC
   DATA BackColor    AS NUMERIC
   DATA lDrawValue   AS LOGICAL INIT .T.

   // Methods
   METHOD CreateBar( sx, sy, filename, ccolor)
   METHOD Configure( nmaxHeight, aFillColor, aBackColor, nres, ntextfont, lbook, lDrawValue )
   METHOD Allocate()
   METHOD DrawError(ptext)
   METHOD DrawSingleBar(pcode )
   METHOD DrawSingleI25( pcode )
   METHOD DrawText( lIsI25 )
   METHOD nextX( lI25 )
   METHOD Finish( image_style, quality, nFG )
   METHOD SetText( ptext )
   METHOD ResetColor()
   METHOD CheckCode()
   METHOD CheckValInArray(cchar)

ENDCLASS

METHOD CreateBar( sx, sy, filename, ccolor ) CLASS TBarCode

   ::Create( sx, sy )

   DEFAULT ccolor TO {255,255,255}

   ::setcolor( ccolor[1], ccolor[2], ccolor[3] )

   ::error     := 0
   ::positionY := 0
   ::imWidth   := sx

   if !empty( filename )
      ::filename  := filename
   endif

   ::FillColor  := ::setcolor( ::color_f[1] ,::color_f[2] ,::color_f[3] )
   ::BackColor  := ::setcolor( ::color_b[1] ,::color_b[2] ,::color_b[3] )

   ::Setfont("Arial")

   // configures Fontes
   If        ::textfont == 1 ; ::SetFontSmall()
      ElseIf ::textfont == 2 ; ::SetFontLarge()
      ElseIf ::textfont == 3 ; ::SetFontMediumBold()
      ElseIf ::textfont == 4 ; ::SetFontGiant()
      ElseIf ::textfont == 5 ; ::SetFontTiny()
   EndIf

   ::SetFontPitch(::textfont)

   // always restores
   ::maxHeight := ::maxHDefa

   Return Self

METHOD Configure( nmaxHeight, aFillColor, aBackColor, nres, ntextfont, lbook, lDrawValue ) CLASS TBarCode

   DEFAULT lbook       TO .F.
   DEFAULT lDrawValue  TO .T.
   DEFAULT nmaxHeight  TO 25
   DEFAULT ntextfont   TO 2
   DEFAULT nres        TO 2
   DEFAULT aBackColor  TO {255,255,255}
   DEFAULT aFillColor  TO {0,0,0}

   ::book       := lbook
   ::maxHeight  := nmaxHeight
   ::res        := nres
   ::textfont   := ntextfont
   ::lDrawValue := lDrawValue

   ::color_b    := aClone(aBackColor)
   ::color_f    := aClone(aFillColor)

   RETURN NIL

METHOD SetText( ptext )  CLASS TBarCode

   ::text := ptext

   Return NIL

METHOD ResetColor() CLASS TBarCode

   ::FillColor  := ::setcolor( ::color_f[1] ,::color_f[2] ,::color_f[3] )
   ::BackColor  := ::setcolor( ::color_b[1] ,::color_b[2] ,::color_b[3] )

   Return NIL

METHOD Allocate() CLASS TBarCode

   LOCAL R := ::color_b[1]
   LOCAL G := ::color_b[2]
   LOCAL B := ::color_b[3]

   Return ::SetColor(R,G,B)

METHOD DrawSingleBar( pcode ) CLASS TBarCode

   LOCAL i
   LOCAL j

   For j := 1 To Len( pcode )

      For i := 1  TO ::res
         ::Line( ::positionX + i  , ::positionY , ::positionX + i , (::positionY+::maxHeight) ,;
                 iif( SubStr(pcode,j,1) $ "0", ::BackColor, ::FillColor  ) )
      Next

     ::NextX()

   Next

   Return NIL

METHOD DrawSingleI25( pcode ) CLASS TBarCode

   LOCAL j

   LOCAL widthSlimBar  := 1
   LOCAL widthFatBar   := 3

   LOCAL imgBar
   LOCAL imgWid
   LOCAL end_y
   LOCAL qw

   ::positionX := 10

   For j := 1 To Len( pcode )

      imgBar := iif( j % 2 == 0, ::FillColor, ::BackColor )
      imgWid := iif( SubStr(pcode,j,1) =="0" , widthSlimBar, widthFatBar )

       end_y := ::maxHeight

      For qw := 1 TO imgWid
        ::Line( ::positionX, 1, ::positionX, end_y, imgBar)
        ::nextX(.T.)
      Next

   Next

   Return NIL


METHOD DrawError(ptext) CLASS TBarCode

   ::Say( 5, ::error*15, ptext, ::FillColor )

   ::error++

   ::lastX := iif( (::GetFontWidth()*Len(ptext) ) > ::lastX , ( ::GetFontWidth()*Len(ptext)) , ::lastX )
   ::lastY := ::error*15

   Return NIL

METHOD nextX( lI25 ) CLASS TBarCode

   DEFAULT li25 TO .F.

   If li25
      ::positionX ++
   Else
      ::positionX += ::res
   EndIf

   Return NIL

METHOD DrawText(lIsI25) CLASS TBarCode

   LOCAL xPosition

   DEFAULT lIsI25 TO .F.

   If lIsI25
      If ::textfont != 0
          xPosition  := 10 * ::GetFontWidth()
          ::say(  xPosition, ::maxHeight, "*" + ::text + "*" , ::FillColor )
          ::lastY    := ::maxHeight + ::GetFontHeight()
      EndIf
   Else
      If ::textfont != 0
          xPosition  := ( ::positionX / 2) - ( Len( ::text ) /2 ) * ::GetFontWidth()
          ::say(  xPosition, ::maxHeight, ::text, ::FillColor )
            ::lastY    := ::maxHeight + ::GetFontHeight()
      EndIf
   EndIf

   Return .T.

METHOD CheckCode() CLASS TBarCode

   LOCAL lRet := .T.
   LOCAL i

   For i := 1 To Len( ::text )
       If ISCHARACTER( ::CheckValInArray( SubStr( ::text, i, 1 ) ) )
           ::DrawError("Character  "+SubStr( ::text, i, 1 )+" not allowed .")
           lRet := .F.
       EndIf
   Next

   Return lRet

METHOD CheckValInArray(cchar) CLASS TBarCode

   LOCAL npos
   LOCAL uret

   npos := ASCAN( ::keys, { |x| SubStr( x, 1, 1 ) == cchar } )

   If npos > 0
      uret := npos
   Else
      uret := NIL
   EndIf

   Return uret

METHOD Finish( image_style, quality, nFG  ) CLASS TBarCode

   DEFAULT image_style TO IMG_FORMAT_PNG
   DEFAULT quality     TO 95
   DEFAULT nFG         TO {255,255,255}

   If Empty( ::filename ) .OR. ::filename == NIL

      // Output std handle == 1

      //::filename := ::text
      If image_style == IMG_FORMAT_PNG
         ::OutputPng()
      Elseif image_style == IMG_FORMAT_JPEG
         ::OutputJpeg( , quality )
      ElseIf image_style == IMG_FORMAT_WBMP
         ::OutputWBmp( , nFG )
      ElseIf image_style == IMG_FORMAT_GIF
         ::OutputGif()
      EndIf

   else

      If image_style == IMG_FORMAT_PNG
         ::SavePng(  ::filename )
      Elseif image_style == IMG_FORMAT_JPEG
         ::Savejpeg( ::filename, quality )
      ElseIf image_style == IMG_FORMAT_WBMP
         ::SaveWBmp( ::filename, nFG )
      ElseIf image_style == IMG_FORMAT_GIF
         ::SaveGif( ::filename )
      EndIf

   EndIf

   Return .T.
