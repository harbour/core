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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

   ::cTitle := "Chart"
   ::aSeries        := {}
   ::hDefs          := { => }
   ::aDataOfHashes  := {}

   ::Create( hb_defaultValue( sx, 320 ), hb_defaultValue( sy, 200 ) )

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
      ::hDefs[ Upper( cDefKey ) ] := xDefVal
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

   hb_default( @x         , ::CenterWidth() )
   hb_default( @y         , ::CenterHeight() )
   hb_default( @nWidth    , Min( ::Width(), ::Height() ) )
   hb_default( @cFontPitch, "TINY" )

   SWITCH cFontPitch
   CASE "TINY"   ; ::SetFontTiny()       ; EXIT
   CASE "SMALL"  ; ::SetFontSmall()      ; EXIT
   CASE "MEDIUM" ; ::SetFontMediumBold() ; EXIT
   CASE "LARGE"  ; ::SetFontLarge()      ; EXIT
   CASE "GIANT"  ; ::SetFontGiant()      ; EXIT
   ENDSWITCH

   /*
      hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   // Before sum of values to determine percentual
   FOR EACH hElement IN aPieDataOfHash
      nTot += hElement[ "VALUE" ]
      // Check extrution
      IF ( nExtrude := __HGetValue( hElement, "EXTRUDE" ) ) != NIL
         nTotExtr := Max( nTotExtr, nExtrude )
      ENDIF
   NEXT

   nWidth -= ( nTotExtr + 2 ) * 2

   // Second
   FOR EACH hElement IN aPieDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
      lFilled   := __HGetValue( hElement, "FILLED" )
      pTile     := __HGetValue( hElement, "TILE" )
      nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      lExtruded := ( nExtrude != NIL )
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := 360 * ( ( nVal / nTot ) * 100 ) / 100
      hb_default( @lFilled, .F. )
      hb_default( @nExtrude, 0 )
      hb_default( @colorp, ::SetColor( 0, 0, 0 ) )
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
            hb_default( @cFontName, "Arial" )
            hb_default( @nPitch   , 8 )
            hb_default( @nAngle   , 0 )
         ENDIF
         nPosX   := nPosX + ( ( nExtrude + nWidth ) / 4 ) * Cos( ::Radians( nDegree + nDim / 2 ) )
         nPosY   := nPosY + ( ( nExtrude + nWidth ) / 4 ) * Sin( ::Radians( nDegree + nDim / 2 ) )
         IF textcolor == NIL
            colorp    := ::GetPixel( nPosX, nPosY )
            textcolor := ::SetColor( 255 - ::Red( colorp ), 255 - ::Green( colorp ), 255 - ::Blue( colorp ) )
         ENDIF
#if 0
         cTitle := hb_ntos( nVal )
#endif
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
   LOCAL pTile, lFilled
#if 0
   LOCAL nExtrude
   LOCAL lExtruded
#endif
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

   hb_default( @x         , 0 )
   hb_default( @y         , 0 )
   hb_default( @nWidth    , ::Width() )
   hb_default( @nHeight   , ::Height() )
   hb_default( @color     , ::GetColor() )
   hb_default( @lShowAxis , .T. )
   hb_default( @lShowGrid , .T. )
   hb_default( @cAxisPict , "@E 9,999.99" )
   hb_default( @cFontPitch, "TINY" )

   hb_default( @nBorder, 4 )

   /*
      hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   SWITCH cFontPitch
   CASE "TINY"   ; ::SetFontTiny()       ; EXIT
   CASE "SMALL"  ; ::SetFontSmall()      ; EXIT
   CASE "MEDIUM" ; ::SetFontMediumBold() ; EXIT
   CASE "LARGE"  ; ::SetFontLarge()      ; EXIT
   CASE "GIANT"  ; ::SetFontGiant()      ; EXIT
   ENDSWITCH

   // Before sum of values to determine percentual
   nMaxLabel := 0
   nMax      := 0
   FOR EACH hElement IN aDataOfHash
      IF hElement:__enumIsFirst()
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

   hb_default( @nMaxValue, nMax )

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

   // Second
   FOR EACH hElement IN aDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
      lFilled   := __HGetValue( hElement, "FILLED" )
      pTile     := __HGetValue( hElement, "TILE" )
#if 0
      nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      lExtruded := ( nExtrude != NIL )
#endif
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := ( nVal / nMaxValue ) * nHeight

      hb_default( @lFilled, .F. )
#if 0
      hb_default( @nExtrude, 0 )
#endif
      hb_default( @colorp, ::SetColor( 0, 0, 0 ) )

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
   LOCAL pTile, lFilled
#if 0
   LOCAL nExtrude
   LOCAL lExtruded
#endif
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

   hb_default( @x         , 0 )
   hb_default( @y         , 0 )
   hb_default( @nWidth    , ::Width() )
   hb_default( @nHeight   , ::Height() )
   hb_default( @color     , ::GetColor() )
   hb_default( @lShowAxis , .T. )
   hb_default( @lShowGrid , .T. )
   hb_default( @cAxisPict , "@E 9,999.99" )
   hb_default( @cFontPitch, "TINY" )

   hb_default( @nBorder, 4 )

   /*
      hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   SWITCH cFontPitch
   CASE "TINY"   ; ::SetFontTiny()       ; EXIT
   CASE "SMALL"  ; ::SetFontSmall()      ; EXIT
   CASE "MEDIUM" ; ::SetFontMediumBold() ; EXIT
   CASE "LARGE"  ; ::SetFontLarge()      ; EXIT
   CASE "GIANT"  ; ::SetFontGiant()      ; EXIT
   ENDSWITCH

   // Before sum of values to determine perentual
   nMaxLabel := 0
   nMax      := 0
   FOR EACH hElement IN aDataOfHash
      IF hElement:__enumIsFirst()
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

   hb_default( @nMaxValue, nMax )

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

   // Second
   FOR EACH hElement IN aDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
      lFilled   := __HGetValue( hElement, "FILLED" )
      pTile     := __HGetValue( hElement, "TILE" )
#if 0
      nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      lExtruded := ( nExtrude != NIL )
#endif
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := ( nVal / nMaxValue ) * nWidth
      hb_default( @lFilled, .F. )
#if 0
      hb_default( @nExtrude, 0 )
#endif
      hb_default( @colorp, ::SetColor( 0, 0, 0 ) )

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
   LOCAL pTile
#if 0
   LOCAL lFilled
   LOCAL lExtruded
   LOCAL nExtrude
#endif
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

   hb_default( @x         , 0 )
   hb_default( @y         , 0 )
   hb_default( @nWidth    , ::Width() )
   hb_default( @nHeight   , ::Height() )
   hb_default( @colorp    , ::GetColor() )
   hb_default( @lShowAxis , .T. )
   hb_default( @lShowGrid , .T. )
   hb_default( @cAxisPict , "@E 9,999.99" )
   hb_default( @cFontPitch, "TINY" )

   hb_default( @nBorder, 4 )

   /*
      hData := ["TITLE"], ["VALUE"], ["FILLED"], ["COLOR"], ["TILE"], ["EXTRUDE"]
   */

   SWITCH cFontPitch
   CASE "TINY"   ; ::SetFontTiny()       ; EXIT
   CASE "SMALL"  ; ::SetFontSmall()      ; EXIT
   CASE "MEDIUM" ; ::SetFontMediumBold() ; EXIT
   CASE "LARGE"  ; ::SetFontLarge()      ; EXIT
   CASE "GIANT"  ; ::SetFontGiant()      ; EXIT
   ENDSWITCH

   // Before sum of values to determine percentual
   nMaxLabel := 0
   nMax      := 0
   FOR EACH hElement IN aDataOfHash
      IF hElement:__enumIsFirst()
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
      IF hElement:__enumIsFirst()
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

   hb_default( @nMaxValue, nMax )
   hb_default( @nMinValue, nMin )

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

   nMaxValue := Ceiling( nMaxValue / nCeiling ) * nCeiling
   nMinValue := iif( nMinValue < 0, -Ceiling( Abs( nMinValue ) / nCeiling ) * nCeiling, Ceiling( nMinValue / nCeiling ) * nCeiling )

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

   // Second
   aPoints := {}
   FOR EACH hElement IN aDataOfHash
      cLabel    := __HGetValue( hElement, "LABEL" )
#if 0
      lFilled   := __HGetValue( hElement, "FILLED" )
#endif
      pTile     := __HGetValue( hElement, "TILE" )
#if 0
      nExtrude  := __HGetValue( hElement, "EXTRUDE" )
      lExtruded := ( nExtrude != NIL )
#endif
      colorp    := __HGetValue( hElement, "COLOR" )
      nVal      := hElement[ "VALUE" ]
      nDim      := ( ( nVal + Abs( nMinValue ) ) / nTotRange ) * nHeight

#if 0
      hb_default( @lFilled, .F. )
      hb_default( @nExtrude, 0 )
#endif
      hb_default( @colorp, ::SetColor( 0, 0, 0 ) )

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
#if 0
      ::Rectangle( nPosX + nBorder, ::Height() - ( nPosY + nDim ), nPosX + nSize - nBorder, ::Height() - nPosY, lFilled, colorp )
#endif
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

#if 0
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
#endif
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
#if 0
   oDestImage := __objClone( Self )
#endif
   oDestImage:pImage := pImage
   ::Copy( 0, 0, ::Width, ::Height, 0, 0, oDestImage )

#if 0
   pImage := oDestImage:pImage
   // Signal that this image must not be destroyed
   oDestImage:lDestroy := .F.
   oDestImage := NIL
   oDestImage:pImage := pImage
#endif

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
