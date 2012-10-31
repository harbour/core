/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GD graphic library chart class
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
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

#include "hbclass.ch"
#include "gd.ch"

CREATE CLASS GDChart FROM GDImage

   VAR cTitle
   VAR cAxisX
   VAR cAxisY
   VAR nWidth
   VAR nHeight
   VAR nScaleX
   VAR nScaleY

   VAR aSeries
   VAR aDataOfHashes         // Hash contains graph datas
   VAR hDefs


   METHOD New( sx, sy )  CONSTRUCTOR
   METHOD AddData( hData )
   METHOD AddDef( cDefKey, xDefVal )
   METHOD SetData( aData )
   METHOD SetDefs( hDefs )

   METHOD PieChart()
   METHOD VerticalBarChart()
   METHOD HorizontalBarChart()
   METHOD LineChart()

   // clone method for gdchart
   METHOD Clone()

   PROTECTED:
   METHOD CloneDataFrom( oSrc )

ENDCLASS

METHOD New( sx, sy ) CLASS GDChart

   __defaultNIL( @sx, 320 )
   __defaultNIL( @sy, 200 )

   ::cTitle := "Chart"
   ::aSeries        := {}
   ::hDefs          := { => }
   ::aDataOfHashes  := {}

   ::Create( sx, sy )

   RETURN Self

METHOD AddData( hData ) CLASS GDChart

   IF HB_ISHASH( hData )
      AAdd( ::aDataOfHashes, hData )
   ENDIF

   RETURN Self

METHOD SetData( aData ) CLASS GDChart

   IF HB_ISARRAY( aData )
      ::aDataOfHashes := aData
   ENDIF

   RETURN Self

METHOD AddDef( cDefKey, xDefVal ) CLASS GDChart

   IF HB_ISSTRING( cDefKey )
      hb_HSet( ::hDefs, Upper( cDefKey ), xDefVal )
   ENDIF

   RETURN Self

METHOD SetDefs( hDefs ) CLASS GDChart

   IF HB_ISHASH( hDefs )
      ::hDefs := hDefs
   ENDIF

   RETURN Self

METHOD PieChart() CLASS GDChart

   LOCAL hElement, nTot := 0
   LOCAL nDegree := 0
   LOCAL lFilled, lExtruded, nExtrude, nTotExtr := 0, pTile
   LOCAL colorp
   LOCAL nVal, nDim
   LOCAL nPosX, nPosY
   LOCAL cLabel, hFont, cFontName, nPitch, nAngle, textcolor
   LOCAL x, y, nWidth
   LOCAL aPieDataOfHash, hDefs
   LOCAL cFontPitch

   aPieDataOfHash := ::aDataOfHashes
   hDefs          := ::hDefs

   x          := __HGetValue( hDefs, "POSX" )
   y          := __HGetValue( hDefs, "POSY" )
   nWidth     := __HGetValue( hDefs, "WIDTH" )
   cFontPitch := __HGetValue( hDefs, "FONTPITCH" )

   __defaultNIL( @x         , ::CenterWidth() )
   __defaultNIL( @y         , ::CenterHeight() )
   __defaultNIL( @nWidth    , Min( ::Width(), ::Height() ) )
   __defaultNIL( @cFontPitch, "TINY" )

   DO CASE
   CASE cFontPitch == "TINY"
      ::SetFontTiny()
   CASE cFontPitch == "SMALL"
      ::SetFontSmall()
   CASE cFontPitch == "MEDIUM"
      ::SetFontMediumBold()
   CASE cFontPitch == "LARGE"
      ::SetFontLarge()
   CASE cFontPitch == "GIANT"
      ::SetFontGiant()
   ENDCASE

   /*
     hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   // Before sum of values to determine perentual
   FOR EACH hElement IN aPieDataOfHash
      nTot += hElement[ "VALUE" ]
      // Check extrution
      IF ( nExtrude := __HGetValue( hElement, "EXTRUDE" ) ) != NIL
         nTotExtr := Max( nTotExtr, nExtrude )
      ENDIF
   NEXT

   nWidth -= ( nTotExtr + 2 ) * 2

   // Second,
   FOR EACH hElement IN aPieDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
      lFilled   := __HGetValue( hElement, "FILLED" )
      nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      pTile     := __HGetValue( hElement, "TILE" )
      IF nExtrude != NIL
         lExtruded := .T.
      ELSE
         lExtruded := .F.
      ENDIF
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := 360 * ( ( nVal / nTot ) * 100 ) / 100
      __defaultNIL( @lFilled , .F. )
      __defaultNIL( @nExtrude, 0 )
      __defaultNIL( @colorp  , ::SetColor( 0, 0, 0 ) )
      IF lExtruded
         nPosX   := x + nExtrude * Cos( ::Radians( nDegree + nDim / 2 ) )
         nPosY   := y + nExtrude * Sin( ::Radians( nDegree + nDim / 2 ) )
      ELSE
         nPosX   := x
         nPosY   := y
      ENDIF
      IF pTile != NIL
         ::SetTile( pTile )
         colorp := gdTiled
      ELSE
         IF HB_ISARRAY( colorp )
            colorp := ::SetColor( colorp[ 1 ], colorp[ 2 ], colorp[ 3 ] )
         ENDIF
      ENDIF
      IF lFilled
         ::Arc( nPosX, nPosY, nWidth, nWidth, nDegree, nDegree + nDim, .T., colorp, gdPie )
      ELSE
         ::Arc( nPosX, nPosY, nWidth, nWidth, nDegree, nDegree + nDim, .T., colorp, gdNoFill + gdEdged )
      ENDIF
      IF cLabel != NIL
         hFont := __HGetValue( hElement, "FONT" )
         IF hFont == NIL
            ::SetFontMediumBold()
            cFontName := NIL
            nPitch    := NIL
            nAngle    := NIL
            textcolor := NIL
         ELSE
            cFontName := __HGetValue( hFont, "NAME" )
            nPitch    := __HGetValue( hFont, "PITCH" )
            nAngle    := __HGetValue( hFont, "ANGLE" )
            textcolor := __HGetValue( hFont, "COLOR" )
            __defaultNIL( @cFontName, "Arial" )
            __defaultNIL( @nPitch   , 8 )
            __defaultNIL( @nAngle   , 0 )
         ENDIF
         nPosX   := nPosX + ( ( nExtrude + nWidth ) / 4 ) * Cos( ::Radians( nDegree + nDim / 2 ) )
         nPosY   := nPosY + ( ( nExtrude + nWidth ) / 4 ) * Sin( ::Radians( nDegree + nDim / 2 ) )
         IF textcolor == NIL
            colorp    := ::GetPixel( nPosX, nPosY )
            textcolor := ::SetColor( 255 - ::Red( colorp ), 255 - ::Green( colorp ), 255 - ::Blue( colorp ) )
         ENDIF
         // cTitle := hb_ntos( nVal )
         IF hFont == NIL
            ::Say( nPosX, nPosY, cLabel, textcolor, gdAlignCenter )
         ELSE
            ::SayFreeType( nPosX, nPosY, cLabel, cFontName, nPitch, nAngle, textcolor, gdAlignCenter )
         ENDIF
      ENDIF

      nDegree += nDim + 0.1
   NEXT

   RETURN Self

METHOD VerticalBarChart() CLASS GDChart

   LOCAL hElement, nTot := 0
// LOCAL nDegree := 0
   LOCAL lFilled, /*lExtruded, nExtrude,*/ pTile
   LOCAL colorp
   LOCAL nVal, nDim
   LOCAL nPosX, nPosY
   LOCAL nSize, nMax
   LOCAL nBorder, nThick, n
   LOCAL x, y, nWidth, nHeight, nMaxValue, color, nMaxLabel, cLabel
   LOCAL lShowAxis, lShowGrid

   LOCAL nLeftLabelSpace   // := 40
   LOCAL nRightLabelSpace  // := 40
   LOCAL nBottomLabelSpace // := 40
   LOCAL nTopLabelSpace    := 40
   LOCAL lShowLabelLeft    := .T.
   LOCAL lShowLabelRight   := .T. // .F.
   LOCAL lShowLabelBottom  := .T.
   LOCAL lShowLabelTop     := .F.
   LOCAL cAxisPict
   LOCAL cFontPitch

   LOCAL aDataOfHash, hDefs

   aDataOfHash := ::aDataOfHashes
   hDefs       := ::hDefs

   x          := __HGetValue( hDefs, "POSX" )
   y          := __HGetValue( hDefs, "POSY" )
   nWidth     := __HGetValue( hDefs, "WIDTH" )
   nHeight    := __HGetValue( hDefs, "HEIGHT" )
   nMaxValue  := __HGetValue( hDefs, "MAXVALUE" )
   color      := __HGetValue( hDefs, "COLOR" )
   lShowAxis  := __HGetValue( hDefs, "SHOWAXIS" )
   lShowGrid  := __HGetValue( hDefs, "SHOWGRID" )
   cAxisPict  := __HGetValue( hDefs, "AXISPICT" )
   cFontPitch := __HGetValue( hDefs, "FONTPITCH" )

   __defaultNIL( @x         , 0 )
   __defaultNIL( @y         , 0 )
   __defaultNIL( @nWidth    , ::Width() )
   __defaultNIL( @nHeight   , ::Height() )
   __defaultNIL( @color     , ::GetColor() )
   __defaultNIL( @lShowAxis , .T. )
   __defaultNIL( @lShowGrid , .T. )
   __defaultNIL( @cAxisPict , "@E 9,999.99" )
   __defaultNIL( @cFontPitch, "TINY" )

   __defaultNIL( @nBorder, 4 )

   /*
     hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   DO CASE
   CASE cFontPitch == "TINY"
      ::SetFontTiny()
   CASE cFontPitch == "SMALL"
      ::SetFontSmall()
   CASE cFontPitch == "MEDIUM"
      ::SetFontMediumBold()
   CASE cFontPitch == "LARGE"
      ::SetFontLarge()
   CASE cFontPitch == "GIANT"
      ::SetFontGiant()
   ENDCASE



   // Before sum of values to determine perentual
   nMaxLabel := 0
   nMax      := 0
   FOR EACH hElement IN aDataOfHash
      IF hElement:__enumIndex() == 1
         nMax := hElement[ "VALUE" ]
      ELSE
         nMax := Max( nMax, hElement[ "VALUE" ] )
      ENDIF
      cLabel    := __HGetValue( hElement, "LABEL" )
      nMaxLabel := Max( nMaxLabel, Len( iif( cLabel != NIL, cLabel, "" ) ) )
      nTot      += hElement[ "VALUE" ]
   NEXT

   IF ! HB_ISNUMERIC( nLeftLabelSpace )
      nLeftLabelSpace := nBorder + Len( LTrim( Transform( nMax, cAxisPict ) ) ) * ::GetFontWidth() + nBorder
   ENDIF
   IF ! HB_ISNUMERIC( nRightLabelSpace )
      nRightLabelSpace := nLeftLabelSpace // nBorder + Len( hb_ntos( nMax ) ) * ::GetFontWidth() + nBorder
   ENDIF
   IF ! HB_ISNUMERIC( nBottomLabelSpace )
      nBottomLabelSpace := nBorder + nMaxLabel * ::GetFontWidth() + nBorder
   ENDIF

   __defaultNIL( @nMaxValue, nMax )

   IF lShowAxis
      IF lShowLabelLeft
         x       += nLeftLabelSpace
         nWidth  -= nLeftLabelSpace
      ENDIF
      IF lShowLabelRight
         nWidth  -= nRightLabelSpace
      ENDIF
      IF lShowLabelBottom
         y       += nBottomLabelSpace
         nHeight -= nBottomLabelSpace
      ENDIF
      IF lShowLabelTop
         nHeight -= nTopLabelSpace
      ENDIF
   ENDIF

   nSize := nWidth / Len( aDataOfHash )

   IF lShowGrid
      ::Rectangle( x, ::Height() - ( y + nHeight ), x + nWidth, ::Height() - y, .F., color )

      nThick := ::SetThickness( 1 )

      ::ResetStyles()
      ::AddStyle( color )
      ::AddStyle( color )
      ::AddStyle( color )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::SetStyle()
      FOR n := 10 TO 100 STEP 10
         nDim  := ( ( nMaxValue / 100 ) * n )
         nPosY := ( nDim / nMaxValue ) * nHeight
         ::Line( x, ::Height() - ( y + nPosY ), x + nWidth, ::Height() - ( y + nPosY ), gdStyled )
      NEXT
      ::SetThickness( nThick )
   ENDIF
   IF lShowAxis
      // Y Axis
      FOR n := 10 TO 100 STEP 10
         nDim  := ( ( nMaxValue / 100 ) * n )
         cLabel := LTrim( Transform( nDim, cAxisPict ) )
         nPosY := ( nDim / nMaxValue ) * nHeight
         IF lShowLabelLeft
            ::Say( x - nLeftLabelSpace + nBorder, ::Height() - ( y + nPosY ), PadL( cLabel, Len( LTrim( Transform( nMaxValue, cAxisPict ) ) ) ), color )
         ENDIF
         IF lShowLabelRight
            ::Say( x + nWidth + nBorder, ::Height() - ( y + nPosY ), cLabel, color )
         ENDIF
      NEXT
   ENDIF

   // Second,
   FOR EACH hElement IN aDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
      lFilled   := __HGetValue( hElement, "FILLED" )
      // nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      pTile     := __HGetValue( hElement, "TILE" )
      // IF nExtrude != NIL
      //   lExtruded := .T.
      // ELSE
      //   lExtruded := .F.
      // ENDIF
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := ( nVal / nMaxValue ) * nHeight

      __defaultNIL( @lFilled , .F. )
      // __defaultNIL( @nExtrude, 0 )
      __defaultNIL( @colorp  , ::SetColor( 0, 0, 0 ) )

      nPosX   := x + ( nSize * ( hElement:__enumIndex() - 1 ) )
      nPosY   := y
      IF pTile != NIL
         ::SetTile( pTile )
         colorp := gdTiled
      ELSE
         IF HB_ISARRAY( colorp )
            colorp := ::SetColor( colorp[ 1 ], colorp[ 2 ], colorp[ 3 ] )
         ENDIF
      ENDIF
      ::Rectangle( nPosX + nBorder, ::Height() - ( nPosY + nDim ), nPosX + nSize - nBorder, ::Height() - nPosY, lFilled, colorp )

      IF lShowAxis
         // Y Axis
         IF lShowLabelBottom
            ::SayVertical( nPosX + nSize / 2 - ::GetFontHeight() / 2, ::Height() - nBorder, PadL( cLabel, nMaxLabel ), color )
         ENDIF
      ENDIF
   NEXT

   RETURN Self

METHOD HorizontalBarChart() CLASS GDChart

   LOCAL hElement, nTot := 0
// LOCAL nDegree := 0
   LOCAL lFilled, /*lExtruded, nExtrude,*/ pTile
   LOCAL colorp
   LOCAL nVal, nDim
   LOCAL nPosX, nPosY
   LOCAL nSize, nMax
   LOCAL nBorder, nThick, n
   LOCAL x, y, nWidth, nHeight, nMaxValue, color, nMaxLabel, cLabel
   LOCAL lShowAxis, lShowGrid

   LOCAL nLeftLabelSpace   // := 40
   LOCAL nRightLabelSpace  // := 40
   LOCAL nBottomLabelSpace // := 40
   LOCAL nTopLabelSpace    // := 40
   LOCAL lShowLabelLeft    := .T.
   LOCAL lShowLabelRight   := .T.
   LOCAL lShowLabelBottom  := .T.
   LOCAL lShowLabelTop     := .T.
   LOCAL cAxisPict
   LOCAL cFontPitch

   LOCAL aDataOfHash, hDefs

   aDataOfHash := ::aDataOfHashes
   hDefs       := ::hDefs

   x          := __HGetValue( hDefs, "POSX" )
   y          := __HGetValue( hDefs, "POSY" )
   nWidth     := __HGetValue( hDefs, "WIDTH" )
   nHeight    := __HGetValue( hDefs, "HEIGHT" )
   nMaxValue  := __HGetValue( hDefs, "MAXVALUE" )
   color      := __HGetValue( hDefs, "COLOR" )
   lShowAxis  := __HGetValue( hDefs, "SHOWAXIS" )
   lShowGrid  := __HGetValue( hDefs, "SHOWGRID" )
   cAxisPict  := __HGetValue( hDefs, "AXISPICT" )
   cFontPitch := __HGetValue( hDefs, "FONTPITCH" )

   __defaultNIL( @x         , 0 )
   __defaultNIL( @y         , 0 )
   __defaultNIL( @nWidth    , ::Width() )
   __defaultNIL( @nHeight   , ::Height() )
   __defaultNIL( @color     , ::GetColor() )
   __defaultNIL( @lShowAxis , .T. )
   __defaultNIL( @lShowGrid , .T. )
   __defaultNIL( @cAxisPict , "@E 9,999.99" )
   __defaultNIL( @cFontPitch, "TINY" )

   __defaultNIL( @nBorder, 4 )

   /*
     hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   DO CASE
   CASE cFontPitch == "TINY"
      ::SetFontTiny()
   CASE cFontPitch == "SMALL"
      ::SetFontSmall()
   CASE cFontPitch == "MEDIUM"
      ::SetFontMediumBold()
   CASE cFontPitch == "LARGE"
      ::SetFontLarge()
   CASE cFontPitch == "GIANT"
      ::SetFontGiant()
   ENDCASE

   // Before sum of values to determine perentual
   nMaxLabel := 0
   nMax      := 0
   FOR EACH hElement IN aDataOfHash
      IF hElement:__enumIndex() == 1
         nMax := hElement[ "VALUE" ]
      ELSE
         nMax := Max( nMax, hElement[ "VALUE" ] )
      ENDIF
      cLabel    := __HGetValue( hElement, "LABEL" )
      nMaxLabel := Max( nMaxLabel, Len( iif( cLabel != NIL, cLabel, "" ) ) )
      nTot      += hElement[ "VALUE" ]
   NEXT

   IF ! HB_ISNUMERIC( nLeftLabelSpace )
      nLeftLabelSpace := nBorder + nMaxLabel * ::GetFontWidth() + nBorder
   ENDIF
   IF ! HB_ISNUMERIC( nRightLabelSpace )
      nRightLabelSpace := nBorder + ( Len( LTrim( Transform( nMax, cAxisPict ) ) ) * ::GetFontWidth() / 2 )
   ENDIF
   IF ! HB_ISNUMERIC( nTopLabelSpace )
      nTopLabelSpace := nBorder + ::GetFontHeight() + nBorder
   ENDIF
   IF ! HB_ISNUMERIC( nBottomLabelSpace )
      nBottomLabelSpace := nTopLabelSpace // nBorder + ::GetFontHeight() + nBorder
   ENDIF

   __defaultNIL( @nMaxValue, nMax )

   IF lShowAxis
      IF lShowLabelLeft
         x       += nLeftLabelSpace
         nWidth  -= nLeftLabelSpace
      ENDIF
      IF lShowLabelRight
         nWidth  -= nRightLabelSpace
      ENDIF
      IF lShowLabelBottom
         y       += nBottomLabelSpace
         nHeight -= nBottomLabelSpace
      ENDIF
      IF lShowLabelTop
         nHeight -= nTopLabelSpace
      ENDIF
   ENDIF

   nSize := nHeight / Len( aDataOfHash )

   IF lShowGrid
      ::Rectangle( x, ::Height() - ( y + nHeight ), x + nWidth, ::Height() - y, .F., color )

      nThick := ::SetThickness( 1 )

      ::ResetStyles()
      ::AddStyle( color )
      ::AddStyle( color )
      ::AddStyle( color )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::SetStyle()
      FOR n := 10 TO 100 STEP 10
         nDim  := ( ( nMaxValue / 100 ) * n )
         nPosX := ( nDim / nMaxValue ) * nWidth
         ::Line( x + nPosX, y, x + nPosX, y + nHeight, gdStyled )
      NEXT
      ::SetThickness( nThick )
   ENDIF
   IF lShowAxis
      // X Axis
      FOR n := 0 TO 100 STEP 10
         nDim   := ( ( nMaxValue / 100 ) * n )
         cLabel := LTrim( Transform( nDim, cAxisPict ) )
         nPosX  := ( nDim / nMaxValue ) * nWidth - ( ( Len( cLabel ) / 2 ) * ::GetFontWidth() )
         IF lShowLabelTop
            ::Say( x + nPosX, y - nTopLabelSpace + nBorder, cLabel, color )
         ENDIF
         IF lShowLabelBottom
            ::Say( x + nPosX, y + nHeight + nBorder, cLabel, color )
         ENDIF
      NEXT
   ENDIF

   // Second,
   FOR EACH hElement IN aDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
      lFilled   := __HGetValue( hElement, "FILLED" )
      // nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      pTile     := __HGetValue( hElement, "TILE" )
      // IF nExtrude != NIL
      //   lExtruded := .T.
      // ELSE
      //   lExtruded := .F.
      // ENDIF
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := ( nVal / nMaxValue ) * nWidth
      __defaultNIL( @lFilled , .F. )
      // __defaultNIL( @nExtrude, 0 )
      __defaultNIL( @colorp  , ::SetColor( 0, 0, 0 ) )

      nPosX   := x
      nPosY   := y + ( nSize * ( hElement:__enumIndex() - 1 ) )

      IF pTile != NIL
         ::SetTile( pTile )
         colorp := gdTiled
      ELSE
         IF HB_ISARRAY( colorp )
            colorp := ::SetColor( colorp[ 1 ], colorp[ 2 ], colorp[ 3 ] )
         ENDIF
      ENDIF
      ::Rectangle( nPosX, nPosY + nBorder, nPosX + nDim,  nPosY + nSize - nBorder, lFilled, colorp )

      IF lShowAxis
         // Y Axis
         IF lShowLabelBottom
            ::Say( nBorder, nPosY + nSize / 2 - ::GetFontHeight() / 2, PadL( cLabel, nMaxLabel ), color )
         ENDIF
      ENDIF
   NEXT

   RETURN Self

METHOD LineChart() CLASS GDChart

   LOCAL hElement
// LOCAL nDegree := 0
   LOCAL /*lFilled, lExtruded, nExtrude,*/ pTile
   LOCAL colorp
   LOCAL nVal, nDim
   LOCAL nPosX, nPosY
   LOCAL cLabel
   LOCAL nSize, nMax, nMin, nTotRange, nCeiling
   LOCAL nBorder, nThick, n
   LOCAL x, y, nWidth, nHeight, nMaxValue, nMinValue, nMaxLabel, nMinLabel
   LOCAL lShowAxis, lShowGrid

   LOCAL nLeftLabelSpace   // := 40
   LOCAL nRightLabelSpace  // := 40
   LOCAL nBottomLabelSpace // := 40
   LOCAL nTopLabelSpace    := 40
   LOCAL lShowLabelLeft    := .T.
   LOCAL lShowLabelRight   := .T. // .F.
   LOCAL lShowLabelBottom  := .T.
   LOCAL lShowLabelTop     := .F.
   LOCAL cAxisPict
   LOCAL cFontPitch

   LOCAL aDataOfHash, hDefs, aPoints

   aDataOfHash := ::aDataOfHashes
   hDefs       := ::hDefs

   x          := __HGetValue( hDefs, "POSX" )
   y          := __HGetValue( hDefs, "POSY" )
   nWidth     := __HGetValue( hDefs, "WIDTH" )
   nHeight    := __HGetValue( hDefs, "HEIGHT" )
   nMaxValue  := __HGetValue( hDefs, "MAXVALUE" )
   nMinValue  := __HGetValue( hDefs, "MINVALUE" )
   colorp     := __HGetValue( hDefs, "COLOR" )
   lShowAxis  := __HGetValue( hDefs, "SHOWAXIS" )
   lShowGrid  := __HGetValue( hDefs, "SHOWGRID" )
   cAxisPict  := __HGetValue( hDefs, "AXISPICT" )
   cFontPitch := __HGetValue( hDefs, "FONTPITCH" )

   __defaultNIL( @x         , 0 )
   __defaultNIL( @y         , 0 )
   __defaultNIL( @nWidth    , ::Width() )
   __defaultNIL( @nHeight   , ::Height() )
   __defaultNIL( @colorp    , ::GetColor() )
   __defaultNIL( @lShowAxis , .T. )
   __defaultNIL( @lShowGrid , .T. )
   __defaultNIL( @cAxisPict , "@E 9,999.99" )
   __defaultNIL( @cFontPitch, "TINY" )

   __defaultNIL( @nBorder, 4 )

   /*
     hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   DO CASE
   CASE cFontPitch == "TINY"
      ::SetFontTiny()
   CASE cFontPitch == "SMALL"
      ::SetFontSmall()
   CASE cFontPitch == "MEDIUM"
      ::SetFontMediumBold()
   CASE cFontPitch == "LARGE"
      ::SetFontLarge()
   CASE cFontPitch == "GIANT"
      ::SetFontGiant()
   ENDCASE

   // Before sum of values to determine percentual
   nMaxLabel := 0
   nMax      := 0
   FOR EACH hElement IN aDataOfHash
      IF hElement:__enumIndex() == 1
         nMax := hElement[ "VALUE" ]
      ELSE
         nMax := Max( nMax, hElement[ "VALUE" ] )
      ENDIF
      cLabel    := __HGetValue( hElement, "LABEL" )
      nMaxLabel := Max( nMaxLabel, Len( iif( cLabel != NIL, cLabel, "" ) ) )
   NEXT

   // Before sum of values to determine percentual
   nMinLabel := 0
   nMin      := 0
   FOR EACH hElement IN aDataOfHash
      IF hElement:__enumIndex() == 1
         nMin := hElement[ "VALUE" ]
      ELSE
         nMin := Min( nMin, hElement[ "VALUE" ] )
      ENDIF
      cLabel    := __HGetValue( hElement, "LABEL" )
      nMinLabel := Max( nMinLabel, Len( iif( cLabel != NIL, cLabel, "" ) ) )
   NEXT

   IF ! HB_ISNUMERIC( nLeftLabelSpace )
      nLeftLabelSpace := nBorder + Max( Len( LTrim( Transform( nMax, cAxisPict ) ) ), Len( LTrim( Transform( nMin, cAxisPict ) ) ) ) * ::GetFontWidth() + nBorder
   ENDIF
   IF ! HB_ISNUMERIC( nRightLabelSpace )
      nRightLabelSpace := nLeftLabelSpace
   ENDIF
   IF ! HB_ISNUMERIC( nBottomLabelSpace )
      nBottomLabelSpace := nBorder + nMaxLabel * ::GetFontWidth() + nBorder
   ENDIF

   __defaultNIL( @nMaxValue, nMax )
   __defaultNIL( @nMinValue, nMin )

   IF lShowAxis
      IF lShowLabelLeft
         x       += nLeftLabelSpace
         nWidth  -= nLeftLabelSpace
      ENDIF
      IF lShowLabelRight
         nWidth  -= nRightLabelSpace
      ENDIF
      IF lShowLabelBottom
         y       += nBottomLabelSpace
         nHeight -= nBottomLabelSpace
      ENDIF
      IF lShowLabelTop
         nHeight -= nTopLabelSpace
      ENDIF
   ENDIF

   nSize := Len( aDataOfHash ) - 1

   IF nSize > 1
      nSize := nWidth / nSize
   ELSE
      nSize := nWidth
   ENDIF

   nTotRange := nMaxValue + iif( nMinValue < 0, Abs( nMinValue ), 0 )

   nCeiling := 0

   DO WHILE ( nTotRange / ( 10 ^ nCeiling ) ) > 100
      nCeiling++
   ENDDO

   nCeiling := 10 ^ nCeiling

   nMaxValue := ceiling( nMaxValue / nCeiling ) * nCeiling
   nMinValue := iif( nMinValue < 0, -ceiling( Abs( nMinValue ) / nCeiling ) * nCeiling, ceiling( nMinValue / nCeiling ) * nCeiling )

   nTotRange := nMaxValue + iif( nMinValue < 0, Abs( nMinValue ), 0 )

   IF lShowGrid
      ::Rectangle( x, ::Height() - ( y + nHeight ), x + nWidth, ::Height() - y, .F., colorp )

      nThick := ::SetThickness( 1 )

      ::ResetStyles()
      ::AddStyle( colorp )
      ::AddStyle( colorp )
      ::AddStyle( colorp )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::AddStyle( gdTransparent )
      ::SetStyle()
      FOR n := 10 TO 100 STEP 10
         nDim  := ( ( nTotRange / 100 ) * n )
         nPosY := ( nDim / nTotRange ) * nHeight
         ::Line( x, ::Height() - ( y + nPosY ), x + nWidth, ::Height() - ( y + nPosY ), gdStyled )
      NEXT
      FOR EACH hElement IN aDataOfHash
         nPosX   := x + ( nSize * ( hElement:__enumIndex() - 1 ) )
         ::Line( nPosX, ::Height() - y, nPosX, ::Height() - ( y + nHeight ), gdStyled )
      NEXT
      ::SetThickness( nThick )
   ENDIF

   IF lShowAxis
      // Y Axis
      FOR n := 0 TO 100 STEP 10
         nDim  := ( ( nTotRange / 100 ) * n )
         cLabel := LTrim( Transform( nMinValue + ( nTotRange / 10 ) * ( n / 10 ), cAxisPict ) )
         nPosY := ( nDim / nTotRange ) * nHeight
         IF lShowLabelLeft
            ::Say( x - nLeftLabelSpace + nBorder, ::Height() - ( y + nPosY ), cLabel, colorp )
         ENDIF
         IF lShowLabelRight
            ::Say( x + nWidth + nBorder, ::Height() - ( y + nPosY ), cLabel, colorp )
         ENDIF
      NEXT
   ENDIF

   // Second,
   aPoints := {}
   FOR EACH hElement IN aDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
      // lFilled   := __HGetValue( hElement, "FILLED" )
      // nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      pTile     := __HGetValue( hElement, "TILE" )
      // IF nExtrude != NIL
      //   lExtruded := .T.
      // ELSE
      //   lExtruded := .F.
      // ENDIF
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := ( ( nVal + Abs( nMinValue ) ) / nTotRange ) * nHeight

      // __defaultNIL( @lFilled, .F. )
      // __defaultNIL( @nExtrude, 0 )
      __defaultNIL( @colorp, ::SetColor( 0, 0, 0 ) )

      nPosX   := x + ( nSize * ( hElement:__enumIndex() - 1 ) )
      nPosY   := y
      IF pTile != NIL
         ::SetTile( pTile )
         colorp := gdTiled
      ELSE
         IF HB_ISARRAY( colorp )
            colorp := ::SetColor( colorp[ 1 ], colorp[ 2 ], colorp[ 3 ] )
         ENDIF
      ENDIF
      // ::Rectangle( nPosX + nBorder, ::Height() - ( nPosY + nDim ), nPosX + nSize - nBorder, ::Height() - nPosY, lFilled, colorp )
      AAdd( aPoints, { nPosX, ::Height() - ( nPosY + nDim ) } )

      IF lShowAxis
         // Y Axis
         IF lShowLabelBottom
            ::SayVertical( nPosX - ::GetFontHeight() / 2, ::Height() - nBorder, PadL( cLabel, nMaxLabel ), colorp )
         ENDIF
      ENDIF

   NEXT

   // Draw lines
   nThick := ::SetThickness( 3 )

   // ::ResetStyles()
   // ::AddStyle( color )
   // ::AddStyle( color )
   // ::AddStyle( color )
   // ::AddStyle( gdTransparent )
   // ::AddStyle( gdTransparent )
   // ::AddStyle( gdTransparent )
   // ::AddStyle( gdTransparent )
   // ::AddStyle( gdTransparent )
   // ::SetStyle()
   FOR n := 1 TO Len( aPoints ) - 1
      ::Line( aPoints[ n ][ 1 ], aPoints[ n ][ 2 ], aPoints[ n + 1 ][ 1 ], aPoints[ n + 1 ][ 2 ], colorp )
   NEXT
   ::SetThickness( nThick )

   RETURN Self

METHOD Clone() CLASS GDChart

   LOCAL oDestImage
   LOCAL pImage

   IF ::IsTrueColor()
      oDestImage := GDChart():CreateTrueColor( ::Width, ::Height )
   ELSE
      oDestImage := GDChart():Create( ::Width, ::Height )
   ENDIF

   pImage := oDestImage:pImage
   oDestImage := oDestImage:CloneDataFrom( Self )
   // oDestImage := __objClone( Self )
   oDestImage:pImage := pImage
   ::Copy( 0, 0, ::Width, ::Height, 0, 0, oDestImage )


   // pImage := oDestImage:pImage
   // // Signal that this image must not be destroyed
   // oDestImage:lDestroy := .F.
   // oDestImage := NIL
   // oDestImage:pImage := pImage

   RETURN oDestImage


METHOD CloneDataFrom( oSrc )

   // copy values from Source to Dest
   // please update in case of new datas

   ::Super:CloneDataFrom( oSrc )

   ::cTitle        := oSrc:cTitle
   ::cAxisX        := oSrc:cAxisX
   ::cAxisY        := oSrc:cAxisY
   ::nWidth        := oSrc:nWidth
   ::nHeight       := oSrc:nHeight
   ::nScaleX       := oSrc:nScaleX
   ::nScaleY       := oSrc:nScaleY

   ::aSeries       := AClone( oSrc:aSeries )
   ::aDataOfHashes := AClone( oSrc:aDataOfHashes )
   ::hDefs         := hb_HClone( oSrc:hDefs )

   RETURN Self


STATIC FUNCTION __HGetValue( hHash, cKey )
   RETURN iif( HB_ISHASH( hHash ), hb_HGetDef( hHash, cKey ), NIL )
