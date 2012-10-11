/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GD graphic library class
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

CREATE CLASS GDImage

   PROTECTED:
   DATA pImage
   DATA pBrush
   DATA pTile
   DATA pFont
   DATA pColor

   DATA cFontName    INIT "Arial"
   DATA nFontPitch   INIT 20
   DATA nFontAngle   INIT 0

   DATA aPoints      INIT {}
   DATA aStyles      INIT {}
   DATA lDestroy     INIT .T.

   EXPORTED:
   DATA hFile
   DATA cType
   DATA cMime

   METHOD New( sx, sy )  CONSTRUCTOR

   /* IMAGE CREATION, DESTRUCTION, LOADING AND SAVING  */

   // Create in memory
   METHOD Create( sx, sy )                 INLINE ::pImage := gdImageCreate( sx, sy ), Self
   METHOD CreateTrueColor( sx, sy )        INLINE ::pImage := gdImageCreateTrueColor( sx, sy ), Self

   // Load From File
   METHOD LoadFromPng( cFile )             INLINE ::pImage := gdImageCreateFromPng( cFile )          , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromJpeg( cFile )            INLINE ::pImage := gdImageCreateFromJpeg( cFile )         , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromWBmp( cFile )            INLINE ::pImage := gdImageCreateFromWBMP( cFile )         , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromGd( cFile )              INLINE ::pImage := gdImageCreateFromGD( cFile )           , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromGif( cFile )             INLINE ::pImage := gdImageCreateFromGif( cFile )          , iif( ::pImage != NIL, Self, NIL )

   // Load From a specific File handle
   METHOD InputPng( nHandle, nSize )       INLINE ::pImage := gdImageCreateFromPng( nHandle, nSize ) , iif( ::pImage != NIL, Self, NIL )
   METHOD InputJpeg( nHandle, nSize )      INLINE ::pImage := gdImageCreateFromJpeg( nHandle, nSize ), iif( ::pImage != NIL, Self, NIL )
   METHOD InputWBmp( nHandle, nSize )      INLINE ::pImage := gdImageCreateFromWBMP( nHandle, nSize ), iif( ::pImage != NIL, Self, NIL )
   METHOD InputGd( nHandle, nSize )        INLINE ::pImage := gdImageCreateFromGD( nHandle, nSize )  , iif( ::pImage != NIL, Self, NIL )
   METHOD InputGif( nHandle, nSize )       INLINE ::pImage := gdImageCreateFromGif( nHandle, nSize ) , iif( ::pImage != NIL, Self, NIL )

   // Create from an image pointer in memory
   METHOD CreateFromPng( pImage, nSize )   INLINE ::pImage := gdImageCreateFromPng( pImage, nSize )  , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromJpeg( pImage, nSize )  INLINE ::pImage := gdImageCreateFromJpeg( pImage, nSize ) , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromWBmp( pImage, nSize )  INLINE ::pImage := gdImageCreateFromWBMP( pImage, nSize ) , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromGd( pImage, nSize )    INLINE ::pImage := gdImageCreateFromGD( pImage, nSize )   , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromGif( pImage, nSize )   INLINE ::pImage := gdImageCreateFromGif( pImage, nSize )  , iif( ::pImage != NIL, Self, NIL )

   METHOD LoadFromFile( cFile )

   // Save To File Name
   METHOD SavePng( cFile, nLevel )         INLINE gdImagePng( ::pImage, cFile, nLevel )
   METHOD SaveJpeg( cFile, nLevel )        INLINE gdImageJpeg( ::pImage, cFile, nLevel )
   METHOD SaveWBmp( cFile, nFG )           INLINE gdImageWBmp( ::pImage, cFile, nFG )
   METHOD SaveGd( cFile )                  INLINE gdImageGd( ::pImage, cFile )
   METHOD SaveGif( cFile )                 INLINE gdImageGif( ::pImage, cFile )

   METHOD SaveToFile( cFile )              INLINE gdImageToFile( Self, cFile )

   // Output To a specified File handle
   METHOD OutputPng( nHandle, nLevel )     INLINE iif( nHandle == NIL, nHandle := 1, ), gdImagePng( ::pImage, nHandle, nLevel )
   METHOD OutputJpeg( nHandle, nLevel )    INLINE iif( nHandle == NIL, nHandle := 1, ), gdImageJpeg( ::pImage, nHandle, nLevel )
   METHOD OutputWBmp( nHandle, nFG )       INLINE iif( nHandle == NIL, nHandle := 1, ), gdImageWBmp( ::pImage, nHandle, nFG )
   METHOD OutputGd( nHandle )              INLINE iif( nHandle == NIL, nHandle := 1, ), gdImageGd( ::pImage, nHandle )
   METHOD OutputGif( nHandle )             INLINE iif( nHandle == NIL, nHandle := 1, ), gdImageGif( ::pImage, nHandle )

   METHOD Output( nHandle )                INLINE gdImageToHandle( ::pImage, nHandle )

   // Output To a string
   METHOD ToStringPng( nLevel )            INLINE gdImagePng( ::pImage, NIL, nLevel )
   METHOD ToStringJpeg( nLevel )           INLINE gdImageJpeg( ::pImage, NIL, nLevel )
   METHOD ToStringWBmp( nFG )              INLINE gdImageWBmp( ::pImage, NIL, nFG )
   METHOD ToStringGd()                     INLINE gdImageGd( ::pImage, NIL )
   METHOD ToStringGif()                    INLINE gdImageGif( ::pImage, NIL )

   METHOD ToString()                       INLINE gdImageToString( Self )

   // Destructor
   METHOD Destroy()
   DESTRUCTOR Destruct()



   /* DRAWING FUNCTIONS */
   METHOD SetPixel( x, y, color )          INLINE hb_default( @color, ::pColor ), gdImageSetPixel( ::pImage, x, y, color )
   METHOD Line( x1, y1, x2, y2, color )    INLINE hb_default( @color, ::pColor ), gdImageLine( ::pImage, x1, y1, x2, y2, color )
   METHOD DashedLine( x1, y1, x2, y2, color )    INLINE hb_default( @color, ::pColor ), gdImageDashedLine( ::pImage, x1, y1, x2, y2, color )

   // Functions usefull for polygons
   METHOD Polygon( aPoints, lFilled, color )
   METHOD OpenPolygon( aPoints, color )
   METHOD AddPoint( x, y )                 INLINE aAdd( ::aPoints, { x, y } )
   METHOD ResetPoints()                    INLINE ::aPoints := {}
   METHOD Points()                         INLINE Len( ::aPoints )

   METHOD Rectangle( x1, y1, x2, y2, lFilled, color )
   METHOD Arc( x, y, nWidth, nHeight, nStartDegree, nEndDegree, lFilled, color, nStyle )
   METHOD Ellipse( x, y, nWidth, nHeight, lFilled, color )

   METHOD Circle( x, y, nRadius, lFilled, nColor ) ;
                                           INLINE ::Ellipse( x, y, nRadius, nRadius, lFilled, nColor )

   METHOD Fill( x, y, color )              INLINE hb_default( @color, ::pColor ), gdImageFill( ::pImage, x, y, color )
   METHOD FillToBorder( x, y, border, color ) ;
                                           INLINE hb_default( @color, ::pColor ), gdImageFillToBorder( ::pImage, x, y, border, color )
   METHOD SetAntiAliased( color )          INLINE hb_default( @color, ::pColor ), gdImageSetAntiAliased( ::pImage, color )
   METHOD SetAntiAliasedDontBlend( lDontBlend, color ) ;
                                           INLINE hb_default( @color, ::pColor ), gdImageSetAntiAliasedDontBlend( ::pImage, color, lDontBlend )

   METHOD SetBrush( pBrush )               INLINE gdImageSetBrush( ::pImage, pBrush:pImage ), ::pBrush := pBrush
   METHOD SetTile( pTile )                 INLINE gdImageSetTile( ::pImage, pTile:pImage ), ::pTile := pTile

   // Functions usefull for style
   METHOD SetStyle( aStyle )               INLINE hb_default( @aStyle, ::aStyles ), gdImageSetStyle( ::pImage, aStyle )
   METHOD AddStyle( pColor )               INLINE aAdd( ::aStyles, pColor )
   METHOD ResetStyles()                    INLINE ::aStyles := {}
   METHOD StyleLenght()                    INLINE Len( ::aStyles )

   METHOD SetThickness( nThickness )       INLINE gdImageSetThickness( ::pImage, nThickness )
   METHOD SetAlphaBlending( lAlphaBlending )  INLINE gdImageAlphaBlending( ::pImage, lAlphaBlending )
   METHOD SetSaveAlpha( lSaveAlpha )       INLINE gdImageSaveAlpha( ::pImage, lSaveAlpha )
   METHOD SetClippingArea( x1, y1, x2, y2 )   INLINE gdImageSetClip( ::pImage, x1, y1, x2, y2 )

   /* QUERY FUNCTIONS */
   METHOD ColorsTotal()                    INLINE gdImageColorsTotal( ::pImage )
   METHOD Alpha( color )                   INLINE hb_default( @color, ::pColor ), gdImageAlpha( ::pImage, color )
   METHOD Red( color )                     INLINE hb_default( @color, ::pColor ), gdImageRed( ::pImage, color )
   METHOD Green( color )                   INLINE hb_default( @color, ::pColor ), gdImageGreen( ::pImage, color )
   METHOD Blue( color )                    INLINE hb_default( @color, ::pColor ), gdImageBlue( ::pImage, color )
   METHOD Width()                          INLINE gdImageSx( ::pImage )
   METHOD Height()                         INLINE gdImageSy( ::pImage )
   METHOD CenterWidth()                    INLINE ::Width() / 2
   METHOD CenterHeight()                   INLINE ::Height() / 2
   METHOD GetPixel( x, y )                 INLINE gdImageGetPixel( ::pImage, x, y )
   METHOD GetColor()                       INLINE ::pColor
   METHOD GetImagePtr()                    INLINE ::pImage
   METHOD GetClippingArea()                INLINE gdImageGetClip( ::pImage )
   METHOD IsBoundsSafe( x, y )             INLINE gdImageBoundsSafe( ::pImage, x, y )
   METHOD IsInterlaced()                   INLINE gdImageGetInterlaced( ::pImage )
   METHOD GetTransparent()                 INLINE gdImageGetTransparent( ::pImage )
   METHOD IsTransparent()                  INLINE ::GetTransparent() > 0
   METHOD IsTrueColor()                    INLINE gdImageTrueColor( ::pImage )

   METHOD ConvertFromTrueColorToPalette( lDither, nColorsWanted ) ;
                                           INLINE gdImageTrueColorToPalette ( ::pImage, lDither, nColorsWanted )
   METHOD CreatePaletteFromTrueColor( lDither, nColorsWanted ) ;
                                           INLINE gdImageCreatePaletteFromTrueColor( ::pImage, lDither, nColorsWanted )
   METHOD GetPalette( x, y )               INLINE gdImagePalettePixel( ::pImage, x, y )
   METHOD GetTrueColor( x, y )             INLINE gdImageTrueColorPixel( ::pImage, x, y )
   METHOD GetThickness()                   INLINE gdImageGetThickness( ::pImage )

   /* FONTS AND TEXT-HANDLING FUNCTIONS */
   METHOD SetFontSmall()                   INLINE ::pFont := gdFontGetSmall()
   METHOD SetFontLarge()                   INLINE ::pFont := gdFontGetLarge()
   METHOD SetFontMediumBold()              INLINE ::pFont := gdFontGetMediumBold()
   METHOD SetFontGiant()                   INLINE ::pFont := gdFontGetGiant()
   METHOD SetFontTiny()                    INLINE ::pFont := gdFontGetTiny()
   METHOD Say( x, y, cString, color, nAlign )
   METHOD SayVertical( x, y, cString, color )  INLINE hb_default( @color, ::pColor ), gdImageStringUp( ::pImage, ::pFont, x, y, cString, color )

   METHOD SetFontName( cFontName )         INLINE ::cFontName  := cFontName
   METHOD SetFontPitch( nPitch )           INLINE ::nFontPitch := nPitch
   METHOD SetFontAngle( nAngle )           INLINE ::nFontAngle := nAngle
   METHOD SayFreeType( x, y, cString, cFontName, nPitch, nAngle, color, nAlign, ;
                       nLineSpacing, nCharMap, nResolution )

   METHOD SayFreeTypeCircle( x, y, cStringTop, cStringBottom, color, nRadius, nTextRadius, nFillPortion, cFontName, nPitch ) ;
                                           INLINE hb_default( @color, ::pColor ), gdImageStringFTCircle( ::pImage, x, y, nRadius, ;
                                                           nTextRadius, nFillPortion, cFontName, nPitch, cStringTop, cStringBottom, color )

   METHOD GetFont()                        INLINE ::pFont
   METHOD GetFontWidth( pFont )            INLINE hb_default( @pFont, ::pFont ), gdFontGetWidth( pFont )
   METHOD GetFontHeight( pFont )           INLINE hb_default( @pFont, ::pFont ), gdFontGetHeight( pFont )

   METHOD GetFTFontWidth( cFontName, nPitch )  INLINE hb_default( @cFontName, ::cFontName ), ;
                                                      hb_default( @nPitch, ::nFontPitch )  , ;
                                                      gdImageFTWidth( cFontName, nPitch )

   METHOD GetFTFontHeight( cFontName, nPitch ) INLINE hb_default( @cFontName, ::cFontName ), ;
                                                      hb_default( @nPitch, ::nFontPitch )  , ;
                                                      gdImageFTHeight( cFontName, nPitch )

   METHOD GetFTStringSize( cString, cFontName, nPitch ) INLINE hb_default( @cFontName, ::cFontName ), ;
                                                      hb_default( @nPitch, ::nFontPitch )  , ;
                                                      gdImageFTSize( cString, cFontName, nPitch )

   /* COLOR HANDLING FUNCTIONS */
   METHOD SetColor( r, g, b )              INLINE iif( PCount() == 2, ::pColor := r, ::pColor := gdImageColorAllocate( ::pImage, r, g, b ) )
   METHOD DelColor( pColor )               INLINE ::pColor := NIL, gdImageColorDeAllocate( ::pImage, pColor )
   METHOD SetColorAlpha( r, g, b, a )      INLINE ::pColor := gdImageColorAllocateAlpha( ::pImage, r, g, b, a)
   METHOD SetColorClosest( r, g, b )       INLINE ::pColor := gdImageColorClosest( ::pImage, r, g, b )
   METHOD SetColorClosestAlpha( r, g, b, a ) INLINE ::pColor := gdImageColorClosestAlpha( ::pImage, r, g, b, a)
   METHOD SetColorClosestHWB( r, g, b )    INLINE ::pColor := gdImageColorClosestHWB( ::pImage, r, g, b )
   METHOD SetColorExact( r, g, b )         INLINE ::pColor := gdImageColorExact( ::pImage, r, g, b )
   METHOD SetColorResolve( r, g, b )       INLINE ::pColor := gdImageColorResolve( ::pImage, r, g, b )
   METHOD SetColorResolveAlpha( r, g, b, a ) INLINE ::pColor := gdImageColorResolveAlpha( ::pImage, r, g, b, a)
   METHOD SetTransparent( pColor )         INLINE gdImageColorTransparent( ::pImage, pColor )
   METHOD SetSharpen( nPerc )              INLINE gdImageSharpen( ::pImage, nPerc )
   METHOD SetInterlace( lOnOff )           INLINE gdImageInterlace( ::pImage, lOnOff )
   METHOD SetInterlaceOn()                 INLINE gdImageInterlace( ::pImage, .T. )
   METHOD SetInterlaceOff()                INLINE gdImageInterlace( ::pImage, .F. )

   /* COPY AND RESIZING FUNCTIONS */
   METHOD Copy( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, oDestImage )
   METHOD CopyResized( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage )
   METHOD CopyResampled( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage )
   METHOD CopyRotated( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nAngle, oDestImage )
   METHOD CopyMerge( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage )
   METHOD CopyMergeGray( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage )

   /* New implemented */
   METHOD Clone()
   METHOD CopyZoomed( nPerc, nSrcX, nSrcY, nSrcWidth, nSrcHeight )
   METHOD Crop( nX, nY, nWidth, nHeight )
   METHOD Zoom( nPerc )
   METHOD Resize( nWidth, nHeight )
   METHOD Rotate( nAngle, lInside )
   METHOD RotateInside( nAngle )           INLINE ::Rotate( nAngle, .T. )

   METHOD PaletteCopy( oDestImage )        INLINE gdImagePaletteCopy( oDestImage:pImage, ::pImage )
   METHOD SquareToCircle( nRadius )        INLINE gdImageSquareToCircle( ::pImage, nRadius )
   METHOD Compare( oDestImage )            INLINE gdImageCompare( oDestImage:pImage, ::pImage )


   METHOD Radians( nAngle )                INLINE PI() * nAngle / 180
   METHOD Degres( nRadians )               INLINE nRadians * 180 / PI()

   METHOD Version()                        INLINE gdVersion()

   PROTECTED:

   METHOD CloneDataFrom( oSrc )

ENDCLASS

METHOD New( sx, sy ) CLASS GDImage

   ::Create( sx, sy )

   RETURN Self

METHOD PROCEDURE Destruct() CLASS GDImage
   RETURN

METHOD Polygon( aPoints, lFilled, color ) CLASS GDImage

   hb_default( @aPoints, ::aPoints )
   hb_default( @lFilled, .F. )
   hb_default( @color, ::pColor )

   IF lFilled
      gdImageFilledPolygon( ::pImage, aPoints, color )
   ELSE
      gdImagePolygon( ::pImage, aPoints, color )
   ENDIF

   RETURN Self

METHOD OpenPolygon( aPoints, color ) CLASS GDImage

   hb_default( @aPoints, ::aPoints )
   hb_default( @color, ::pColor )

   gdImageOpenPolygon( ::pImage, aPoints, color )

   RETURN Self

METHOD Rectangle( x1, y1, x2, y2, lFilled, color ) CLASS GDImage

   hb_default( @lFilled, .F. )
   hb_default( @color, ::pColor )

   IF lFilled
      gdImageFilledRectangle( ::pImage, x1, y1, x2, y2, color )
   ELSE
      gdImageRectangle( ::pImage, x1, y1, x2, y2, color )
   ENDIF

   RETURN Self

METHOD Arc( x, y, nWidth, nHeight, nStartDegree, nEndDegree, lFilled, color, nStyle ) CLASS GDImage

   hb_default( @lFilled, .F. )
   hb_default( @color, ::pColor )
   hb_default( @nStyle, gdArc )

   IF lFilled
      gdImageFilledArc( ::pImage, x, y, nWidth, nHeight, nStartDegree, nEndDegree, color, nStyle )
   ELSE
      gdImageArc( ::pImage, x, y, nWidth, nHeight, nStartDegree, nEndDegree, color )
   ENDIF

   RETURN Self

METHOD Ellipse( x, y, nWidth, nHeight, lFilled, color ) CLASS GDImage

   hb_default( @lFilled, .F. )
   hb_default( @color, ::pColor )

   IF lFilled
      gdImageFilledEllipse( ::pImage, x, y, nWidth, nHeight, color )
   ELSE
      gdImageEllipse( ::pImage, x, y, nWidth, nHeight, color )
   ENDIF

   RETURN Self

METHOD LoadFromFile( cFile ) CLASS GDImage
   LOCAL aLoad

   aLoad := gdImageFromFile( cFile )
   //Self  := aLoad[1]:Clone()
   ::Destroy()
   Self := ::CloneDataFrom( aLoad[1] )
   //Self := __objClone( aLoad[1] )
   aLoad[1]:lDestroy := .F.
   aLoad[1] := NIL

   ::hFile := aLoad[2]
   ::cType := aLoad[3]
   ::cMime := aLoad[4]

   RETURN Self

/* dummy. no longer needed */
METHOD Destroy() CLASS GDImage
   RETURN Self

METHOD Copy( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, oDestImage ) CLASS GDImage

   hb_default( @nSrcX     , 0 )
   hb_default( @nSrcY     , 0 )
   hb_default( @nWidth    , ::Width() )
   hb_default( @nHeight   , ::Height() )
   hb_default( @nDstX     , 0 )
   hb_default( @nDstY     , 0 )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopy( oDestImage:pImage, ::pImage, nDstX, nDstY, nSrcX, nSrcY, nWidth, nHeight )

   RETURN oDestImage

METHOD CopyResized( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage ) CLASS GDImage

   hb_default( @nSrcX     , 0 )
   hb_default( @nSrcY     , 0 )
   hb_default( @nSrcWidth , ::Width() )
   hb_default( @nSrcHeight, ::Height() )
   hb_default( @nDstX     , 0 )
   hb_default( @nDstY     , 0 )
   hb_default( @nDstWidth , ::Width() )
   hb_default( @nDstHeight, ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nDstWidth, nDstHeight )
      ELSE
         oDestImage := GDImage():Create( nDstWidth, nDstHeight )
      ENDIF
   ENDIF

   gdImageCopyResized( oDestImage:pImage, ::pImage, nDstX, nDstY, nSrcX, nSrcY, nDstWidth, nDstHeight, nSrcWidth, nSrcHeight )

   RETURN oDestImage

METHOD CopyResampled( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage ) CLASS GDImage

   hb_default( @nSrcX      , 0 )
   hb_default( @nSrcY      , 0 )
   hb_default( @nSrcWidth  , ::Width() )
   hb_default( @nSrcHeight , ::Height() )
   hb_default( @nDstX      , 0 )
   hb_default( @nDstY      , 0 )
   hb_default( @nDstWidth  , ::Width() )
   hb_default( @nDstHeight , ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nDstWidth, nDstHeight )
      ELSE
         oDestImage := GDImage():Create( nDstWidth, nDstHeight )
      ENDIF
   ENDIF

   gdImageCopyResampled( oDestImage:pImage, ::pImage, nDstX, nDstY, nSrcX, nSrcY, nDstWidth, nDstHeight, nSrcWidth, nSrcHeight )

   RETURN oDestImage

METHOD CopyRotated( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nAngle, oDestImage ) CLASS GDImage

   hb_default( @nSrcX      , 0 )
   hb_default( @nSrcY      , 0 )
   hb_default( @nWidth     , ::Width )
   hb_default( @nHeight    , ::Height )
   hb_default( @nDstX      , nWidth / 2 )
   hb_default( @nDstY      , nHeight / 2 )
   hb_default( @nAngle     , 90 )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopyRotated( oDestImage:pImage, ::pImage, nDstX, nDstY, nSrcX, nSrcY, nWidth, nHeight, nAngle )

   RETURN oDestImage

METHOD CopyMerge( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage ) CLASS GDImage

   hb_default( @nSrcX      , 0 )
   hb_default( @nSrcY      , 0 )
   hb_default( @nWidth     , ::Width )
   hb_default( @nHeight    , ::Height )
   hb_default( @nDstX      , 0 )
   hb_default( @nDstY      , 0 )
   hb_default( @nPerc      , 100 )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopyMerge( oDestImage:pImage, ::pImage, nDstX, nDstY, nSrcX, nSrcY, nWidth, nHeight, nPerc )

   RETURN oDestImage

METHOD CopyMergeGray( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage ) CLASS GDImage

   hb_default( @nSrcX      , 0 )
   hb_default( @nSrcY      , 0 )
   hb_default( @nWidth     , ::Width )
   hb_default( @nHeight    , ::Height )
   hb_default( @nDstX      , 0 )
   hb_default( @nDstY      , 0 )
   hb_default( @nPerc      , 100 )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopyMergeGray( oDestImage:pImage, ::pImage, nDstX, nDstY, nSrcX, nSrcY, nWidth, nHeight, nPerc )

   RETURN oDestImage

METHOD CopyZoomed( nPerc, nSrcX, nSrcY, nSrcWidth, nSrcHeight ) CLASS GDImage
   LOCAL oDestImage
   LOCAL nDstX, nDstY, nDstWidth, nDstHeight

   hb_default( @nPerc      , 100 )
   hb_default( @nSrcX      , 0 )
   hb_default( @nSrcY      , 0 )
   hb_default( @nSrcWidth  , ::Width() )
   hb_default( @nSrcHeight , ::Height() )

   IF nPerc < 0
      nPerc := 100
   ENDIF

   nDstX      := 0
   nDstY      := 0
   nDstWidth  := nSrcWidth * nPerc / 100
   nDstHeight := nSrcHeight * nPerc / 100

   IF ::IsTrueColor()
      oDestImage := GDImage():CreateTrueColor( nDstWidth, nDstHeight )
   ELSE
      oDestImage := GDImage():Create( nDstWidth, nDstHeight )
   ENDIF

   gdImageCopyResampled( oDestImage:pImage, ::pImage, nDstX, nDstY, nSrcX, nSrcY, nDstWidth, nDstHeight, nSrcWidth, nSrcHeight )

   RETURN oDestImage

METHOD Rotate( nAngle, lInside ) CLASS GDImage
   LOCAL oDestImage
   LOCAL nWidth, nHeight
   LOCAL nAngRad := nAngle * PI() / 180

   hb_default( @lInside, .F. )

   IF !lInside
      nWidth  := ::Width * cos( nAngRad ) + ::Height * sin( nAngRad )
      nHeight := ::Width * sin( nAngRad ) + ::Height * cos( nAngRad )
   ELSE
      nWidth  := ::Width
      nHeight := ::Height
   ENDIF

   IF ::IsTrueColor()
      oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
   ELSE
      oDestImage := GDImage():Create( nWidth, nHeight )
   ENDIF
   IF !lInside
       ::CopyRotated( ,,,, nWidth - nWidth/2, nHeight - nHeight/2, nAngle, oDestImage )
   ELSE
       ::CopyRotated( ,,,,,, nAngle, oDestImage )
   ENDIF
   ::Destroy()
   Self := ::CloneDataFrom( oDestImage )
   //Self := __ObjClone( oDestImage ) // non funziona

   // Move new image to existing one
   // Signal that this image must not be destroyed
   oDestImage:lDestroy := .F.
   oDestImage := NIL

   RETURN Self

METHOD Crop( nX, nY, nWidth, nHeight ) CLASS GDImage
   LOCAL oDestImage

   oDestImage := ::CopyResized( nX, nY, nWidth, nHeight, 0, 0, nWidth, nHeight )
   ::Destroy()
   Self := ::CloneDataFrom( oDestImage )
   //Self := __ObjClone( oDestImage ) // non funziona

   // Move new image to existing one
   // Signal that this image must not be destroyed
   oDestImage:lDestroy := .F.
   oDestImage := NIL

   RETURN Self

METHOD Resize( nWidth, nHeight ) CLASS GDImage
   LOCAL oDestImage

   oDestImage := ::CopyResampled( 0, 0, NIL, NIL, 0, 0, nWidth, nHeight )
   ::Destroy()
   Self := ::CloneDataFrom( oDestImage )
   //Self := __ObjClone( oDestImage ) // non funziona

   // Move new image to existing one
   // Signal that this image must not be destroyed
   oDestImage:lDestroy := .F.
   oDestImage := NIL

   RETURN Self

METHOD Zoom( nPerc ) CLASS GDImage
   LOCAL oDestImage

   oDestImage := ::CopyZoomed( nPerc )
   ::Destroy()
   Self := ::CloneDataFrom( oDestImage )
   //Self := __ObjClone( oDestImage ) // non funziona

   // Move new image to existing one
   // Signal that this image must not be destroyed
   oDestImage:lDestroy := .F.
   oDestImage := NIL

   RETURN Self

METHOD Clone() CLASS GDImage
   LOCAL oDestImage
   LOCAL pImage

   IF ::IsTrueColor()
      oDestImage := GDImage():CreateTrueColor( ::Width, ::Height )
   ELSE
      oDestImage := GDImage():Create( ::Width, ::Height )
   ENDIF

   pImage := oDestImage:pImage
   oDestImage := oDestImage:CloneDataFrom( Self )
   //oDestImage := __objClone( Self )
   oDestImage:pImage := pImage
   ::Copy( 0, 0, ::Width, ::Height, 0, 0, oDestImage )


   //pImage := oDestImage:pImage
   //// Signal that this image must not be destroyed
   //oDestImage:lDestroy := .F.
   //oDestImage := NIL
   //oDestImage:pImage := pImage

   RETURN oDestImage

METHOD Say( x, y, cString, color, nAlign ) CLASS GDImage
   LOCAL nWidth, nLen
   LOCAL nPosX

   hb_default( @color , ::pColor )
   hb_default( @nAlign, gdAlignLeft )

   IF     nAlign == gdAlignCenter
      nWidth := ::GetFontWidth()
      nLen   := Len( cString )
      nPosX  := x - ( nLen / 2 * nWidth )
   ELSEIF nAlign == gdAlignRight
      nWidth := ::GetFontWidth()
      nLen   := Len( cString )
      nPosX  := x - ( nLen * nWidth )
   ELSE
      nPosX  := x
   ENDIF

   gdImageString( ::pImage, ::pFont, nPosX, y, cString, color )

   RETURN Self

METHOD SayFreeType( x, y, cString, cFontName, nPitch, nAngle, color, nAlign, ;
                    nLineSpacing, nCharMap, nResolution )  CLASS GDImage
   LOCAL nWidth, nLen
   LOCAL nPosX

   hb_default( @nAlign    , gdAlignLeft )
   hb_default( @color     , ::pColor )
   hb_default( @cFontName , ::cFontName )
   hb_default( @nPitch    , ::nFontPitch )
   hb_default( @nAngle    , ::nFontAngle )

   IF     nAlign == gdAlignCenter
      nWidth := nPitch //gdImageFTWidth( cFontName, nPitch )//, ::Radians( nAngle ) ) //::GetFontWidth()
      nLen   := Len( cString )
      nPosX  := x - ( (nLen / 2) * nWidth )
   ELSEIF nAlign == gdAlignRight
      nWidth := gdImageFTWidth( cFontName, nPitch ) //, ::Radians( nAngle ) ) //::GetFontWidth()
      nLen   := Len( cString )
      nPosX  := x - ( nLen * nWidth )
   ELSE
      nPosX  := x
   ENDIF

   gdImageStringFT( ::pImage, color, cFontName, nPitch, ::Radians( nAngle ), nPosX, y, ;
                    cString, nLineSpacing, nCharMap, nResolution )

   RETURN Self

METHOD CloneDataFrom( oSrc )
   // copy values from Source to Dest
   // please update in case of new datas

   ::pImage      := oSrc:pImage
   ::pBrush      := oSrc:pBrush
   ::pTile       := oSrc:pTile
   ::pFont       := oSrc:pFont
   ::pColor      := oSrc:pColor

   ::cFontName   := oSrc:cFontName
   ::nFontPitch  := oSrc:nFontPitch
   ::nFontAngle  := oSrc:nFontAngle

   ::aPoints     := AClone( oSrc:aPoints )
   ::aStyles     := AClone( oSrc:aStyles )
   ::lDestroy    := oSrc:lDestroy

   ::hFile       := oSrc:hFile
   ::cType       := oSrc:cType
   ::cMime       := oSrc:cMime

   RETURN Self
