/*
 * GD graphic library class
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
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

CREATE CLASS GDImage

   PROTECTED:
   VAR pImage
   VAR pBrush
   VAR pTile
   VAR pFont
   VAR pColor

   VAR cFontName    INIT "Arial"
   VAR nFontPitch   INIT 20
   VAR nFontAngle   INIT 0

   VAR aPoints      INIT {}
   VAR aStyles      INIT {}

   EXPORTED:
   VAR hFile
   VAR cType
   VAR cMime

   METHOD New( sx, sy )  CONSTRUCTOR

   /* IMAGE CREATION, DESTRUCTION, LOADING AND SAVING */

   // Create in memory
   METHOD Create( sx, sy )                 INLINE ::pImage := gdImageCreate( sx, sy ), Self
   METHOD CreateTrueColor( sx, sy )        INLINE ::pImage := gdImageCreateTrueColor( sx, sy ), Self

   // Load From File
   METHOD LoadFromPng( cFile )             INLINE ::pImage := gdImageCreateFromPng( cFile )          , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromJpeg( cFile )            INLINE ::pImage := gdImageCreateFromJpeg( cFile )         , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromWBmp( cFile )            INLINE ::pImage := gdImageCreateFromWBmp( cFile )         , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromGd( cFile )              INLINE ::pImage := gdImageCreateFromGD( cFile )           , iif( ::pImage != NIL, Self, NIL )
   METHOD LoadFromGif( cFile )             INLINE ::pImage := gdImageCreateFromGif( cFile )          , iif( ::pImage != NIL, Self, NIL )

   // Load From a specific File handle
   METHOD InputPng( nHandle, nSize )       INLINE ::pImage := gdImageCreateFromPng( nHandle, nSize ) , iif( ::pImage != NIL, Self, NIL )
   METHOD InputJpeg( nHandle, nSize )      INLINE ::pImage := gdImageCreateFromJpeg( nHandle, nSize ), iif( ::pImage != NIL, Self, NIL )
   METHOD InputWBmp( nHandle, nSize )      INLINE ::pImage := gdImageCreateFromWBmp( nHandle, nSize ), iif( ::pImage != NIL, Self, NIL )
   METHOD InputGd( nHandle, nSize )        INLINE ::pImage := gdImageCreateFromGD( nHandle, nSize )  , iif( ::pImage != NIL, Self, NIL )
   METHOD InputGif( nHandle, nSize )       INLINE ::pImage := gdImageCreateFromGif( nHandle, nSize ) , iif( ::pImage != NIL, Self, NIL )

   // Create from an image pointer in memory
   METHOD CreateFromPng( pImage, nSize )   INLINE ::pImage := gdImageCreateFromPng( pImage, nSize )  , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromJpeg( pImage, nSize )  INLINE ::pImage := gdImageCreateFromJpeg( pImage, nSize ) , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromWBmp( pImage, nSize )  INLINE ::pImage := gdImageCreateFromWBmp( pImage, nSize ) , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromGd( pImage, nSize )    INLINE ::pImage := gdImageCreateFromGD( pImage, nSize )   , iif( ::pImage != NIL, Self, NIL )
   METHOD CreateFromGif( pImage, nSize )   INLINE ::pImage := gdImageCreateFromGif( pImage, nSize )  , iif( ::pImage != NIL, Self, NIL )

   METHOD LoadFromFile( cFile )

   // Save To File Name
   METHOD SavePng( cFile, nLevel )         INLINE gdImagePng( ::pImage, cFile, nLevel )
   METHOD SaveJpeg( cFile, nLevel )        INLINE gdImageJpeg( ::pImage, cFile, nLevel )
   METHOD SaveWBmp( cFile, nFG )           INLINE gdImageWBmp( ::pImage, cFile, nFG )
   METHOD SaveGd( cFile )                  INLINE gdImageGD( ::pImage, cFile )
   METHOD SaveGif( cFile )                 INLINE gdImageGif( ::pImage, cFile )

   METHOD SaveToFile( cFile )              INLINE gdImageToFile( Self, cFile )

   // Output To a specified File handle
   METHOD OutputPng( nHandle, nLevel )     INLINE gdImagePng( ::pImage, hb_defaultValue( nHandle, hb_GetStdOut() ), nLevel )
   METHOD OutputJpeg( nHandle, nLevel )    INLINE gdImageJpeg( ::pImage, hb_defaultValue( nHandle, hb_GetStdOut() ), nLevel )
   METHOD OutputWBmp( nHandle, nFG )       INLINE gdImageWBmp( ::pImage, hb_defaultValue( nHandle, hb_GetStdOut() ), nFG )
   METHOD OutputGd( nHandle )              INLINE gdImageGD( ::pImage, hb_defaultValue( nHandle, hb_GetStdOut() ) )
   METHOD OutputGif( nHandle )             INLINE gdImageGif( ::pImage, hb_defaultValue( nHandle, hb_GetStdOut() ) )

   METHOD Output( nHandle )                INLINE gdImageToHandle( ::pImage, nHandle )

   // Output To a string
   METHOD ToStringPng( nLevel )            INLINE gdImagePng( ::pImage, NIL, nLevel )
   METHOD ToStringJpeg( nLevel )           INLINE gdImageJpeg( ::pImage, NIL, nLevel )
   METHOD ToStringWBmp( nFG )              INLINE gdImageWBmp( ::pImage, NIL, nFG )
   METHOD ToStringGd()                     INLINE gdImageGD( ::pImage, NIL )
   METHOD ToStringGif()                    INLINE gdImageGif( ::pImage, NIL )

   METHOD ToString()                       INLINE gdImageToString( Self )

#if defined( HB_LEGACY_LEVEL4 )
   // Destructor
   METHOD Destroy()
   DESTRUCTOR Destruct()
#endif

   /* Drawing functions */

   METHOD SetPixel( x, y, color )          INLINE gdImageSetPixel( ::pImage, x, y, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD Line( x1, y1, x2, y2, color )    INLINE gdImageLine( ::pImage, x1, y1, x2, y2, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD DashedLine( x1, y1, x2, y2, color )    INLINE gdImageDashedLine( ::pImage, x1, y1, x2, y2, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )

   // Functions useful for polygons
   METHOD Polygon( aPoints, lFilled, color )
   METHOD OpenPolygon( aPoints, color )
   METHOD AddPoint( x, y )                 INLINE AAdd( ::aPoints, { x, y } )
   METHOD ResetPoints()                    INLINE ::aPoints := {}
   METHOD Points()                         INLINE Len( ::aPoints )

   METHOD Rectangle( x1, y1, x2, y2, lFilled, color )
   METHOD Arc( x, y, nWidth, nHeight, nStartDegree, nEndDegree, lFilled, color, nStyle )
   METHOD Ellipse( x, y, nWidth, nHeight, lFilled, color )

   METHOD Circle( x, y, nRadius, lFilled, nColor ) ;
                                           INLINE ::Ellipse( x, y, nRadius, nRadius, lFilled, nColor )

   METHOD Fill( x, y, color )              INLINE gdImageFill( ::pImage, x, y, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD FillToBorder( x, y, border, color ) ;
                                           INLINE gdImageFillToBorder( ::pImage, x, y, border, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD SetAntiAliased( color )          INLINE gdImageSetAntiAliased( ::pImage, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD SetAntiAliasedDontBlend( lDontBlend, color ) ;
                                           INLINE gdImageSetAntiAliasedDontBlend( ::pImage, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ), lDontBlend )

   METHOD SetBrush( pBrush )               INLINE gdImageSetBrush( ::pImage, pBrush:pImage ), ::pBrush := pBrush
   METHOD SetTile( pTile )                 INLINE gdImageSetTile( ::pImage, pTile:pImage ), ::pTile := pTile

   // Functions useful for style
   METHOD SetStyle( aStyle )               INLINE gdImageSetStyle( ::pImage, hb_defaultValue( aStyle, ::aStyles ) )
   METHOD AddStyle( pColor )               INLINE AAdd( ::aStyles, pColor )
   METHOD ResetStyles()                    INLINE ::aStyles := {}
   METHOD StyleLength()                    INLINE Len( ::aStyles )

   METHOD SetThickness( nThickness )          INLINE gdImageSetThickness( ::pImage, nThickness )
   METHOD SetAlphaBlending( lAlphaBlending )  INLINE gdImageAlphaBlending( ::pImage, lAlphaBlending )
   METHOD SetSaveAlpha( lSaveAlpha )          INLINE gdImageSaveAlpha( ::pImage, lSaveAlpha )
   METHOD SetClippingArea( x1, y1, x2, y2 )   INLINE gdImageSetClip( ::pImage, x1, y1, x2, y2 )

   /* Query functions */

   METHOD ColorsTotal()                    INLINE gdImageColorsTotal( ::pImage )
   METHOD Alpha( color )                   INLINE gdImageAlpha( ::pImage, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD Red( color )                     INLINE gdImageRed( ::pImage, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD Green( color )                   INLINE gdImageGreen( ::pImage, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD Blue( color )                    INLINE gdImageBlue( ::pImage, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )
   METHOD Width()                          INLINE gdImageSX( ::pImage )
   METHOD Height()                         INLINE gdImageSY( ::pImage )
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

   /* Fonts and text-handling functions */

   METHOD SetFontSmall()                   INLINE ::pFont := gdFontGetSmall()
   METHOD SetFontLarge()                   INLINE ::pFont := gdFontGetLarge()
   METHOD SetFontMediumBold()              INLINE ::pFont := gdFontGetMediumBold()
   METHOD SetFontGiant()                   INLINE ::pFont := gdFontGetGiant()
   METHOD SetFontTiny()                    INLINE ::pFont := gdFontGetTiny()
   METHOD Say( x, y, cString, color, nAlign )
   METHOD SayVertical( x, y, cString, color )  INLINE gdImageStringUp( ::pImage, ::pFont, x, y, cString, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )

   METHOD SetFontName( cFontName )         INLINE ::cFontName  := cFontName
   METHOD SetFontPitch( nPitch )           INLINE ::nFontPitch := nPitch
   METHOD SetFontAngle( nAngle )           INLINE ::nFontAngle := nAngle
   METHOD SayFreeType( x, y, cString, cFontName, nPitch, nAngle, color, nAlign, nLineSpacing, nCharMap, nResolution )

   METHOD SayFreeTypeCircle( x, y, cStringTop, cStringBottom, color, nRadius, nTextRadius, nFillPortion, cFontName, nPitch ) ;
                                           INLINE gdImageStringFTCircle( ::pImage, x, y, nRadius, ;
                                                           nTextRadius, nFillPortion, cFontName, nPitch, cStringTop, cStringBottom, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )

   METHOD GetFont()                        INLINE ::pFont
   METHOD GetFontWidth( pFont )            INLINE gdFontGetWidth( hb_defaultValue( pFont, iif( ::pFont == NIL, pFont, ::pFont ) ) )
   METHOD GetFontHeight( pFont )           INLINE gdFontGetHeight( hb_defaultValue( pFont, iif( ::pFont == NIL, pFont, ::pFont ) ) )

   METHOD GetFTFontWidth( cFontName, nPitch )           INLINE gdImageFTWidth( hb_defaultValue( cFontName, ::cFontName ), hb_defaultValue( nPitch, ::nFontPitch ) )
   METHOD GetFTFontHeight( cFontName, nPitch )          INLINE gdImageFTHeight( hb_defaultValue( cFontName, ::cFontName ), hb_defaultValue( nPitch, ::nFontPitch ) )
   METHOD GetFTStringSize( cString, cFontName, nPitch ) INLINE gdImageFTSize( cString, hb_defaultValue( cFontName, ::cFontName ), hb_defaultValue( nPitch, ::nFontPitch ) )

   /* Color handling functions */

   METHOD SetColor( r, g, b )                INLINE iif( PCount() == 2 .AND. ( r == NIL .OR. HB_ISNUMERIC( r ) ), ::pColor := r, ::pColor := gdImageColorAllocate( ::pImage, r, g, b ) )
   METHOD DelColor( pColor )                 INLINE ::pColor := NIL, gdImageColorDeallocate( ::pImage, pColor )
   METHOD SetColorAlpha( r, g, b, a )        INLINE ::pColor := gdImageColorAllocateAlpha( ::pImage, r, g, b, a)
   METHOD SetColorClosest( r, g, b )         INLINE ::pColor := gdImageColorClosest( ::pImage, r, g, b )
   METHOD SetColorClosestAlpha( r, g, b, a ) INLINE ::pColor := gdImageColorClosestAlpha( ::pImage, r, g, b, a)
   METHOD SetColorClosestHWB( r, g, b )      INLINE ::pColor := gdImageColorClosestHWB( ::pImage, r, g, b )
   METHOD SetColorExact( r, g, b )           INLINE ::pColor := gdImageColorExact( ::pImage, r, g, b )
   METHOD SetColorResolve( r, g, b )         INLINE ::pColor := gdImageColorResolve( ::pImage, r, g, b )
   METHOD SetColorResolveAlpha( r, g, b, a ) INLINE ::pColor := gdImageColorResolveAlpha( ::pImage, r, g, b, a)
   METHOD SetTransparent( pColor )           INLINE gdImageColorTransparent( ::pImage, pColor )
   METHOD SetSharpen( nPerc )                INLINE gdImageSharpen( ::pImage, nPerc )
   METHOD SetInterlace( lOnOff )             INLINE gdImageInterlace( ::pImage, lOnOff )
   METHOD SetInterlaceOn()                   INLINE gdImageInterlace( ::pImage, .T. )
   METHOD SetInterlaceOff()                  INLINE gdImageInterlace( ::pImage, .F. )

   /* Copy and resizing functions */

   METHOD Copy( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, oDestImage )
   METHOD CopyResized( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage )
   METHOD CopyResampled( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage )
   METHOD CopyRotated( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nAngle, oDestImage )
   METHOD CopyMerge( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage )
   METHOD CopyMergeGray( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage )

   /* Newly implemented */
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


   METHOD Radians( nAngle )                INLINE Pi() * nAngle / 180
   METHOD Degres( nRadians )               INLINE nRadians * 180 / Pi()

   METHOD Version()                        INLINE gdVersion()

   PROTECTED:

   METHOD CloneDataFrom( oSrc )

ENDCLASS

METHOD New( sx, sy ) CLASS GDImage

   ::Create( sx, sy )

   RETURN Self

#if defined( HB_LEGACY_LEVEL4 )

METHOD PROCEDURE Destruct() CLASS GDImage
   RETURN

#endif

METHOD Polygon( aPoints, lFilled, color ) CLASS GDImage

   hb_default( @aPoints, ::aPoints )
   hb_default( @color, iif( ::pColor == NIL, color, ::pColor ) )

   IF hb_defaultValue( lFilled, .F. )
      gdImageFilledPolygon( ::pImage, aPoints, color )
   ELSE
      gdImagePolygon( ::pImage, aPoints, color )
   ENDIF

   RETURN Self

METHOD OpenPolygon( aPoints, color ) CLASS GDImage

   gdImageOpenPolygon( ::pImage, hb_defaultValue( aPoints, ::aPoints ), hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )

   RETURN Self

METHOD Rectangle( x1, y1, x2, y2, lFilled, color ) CLASS GDImage

   hb_default( @color, iif( ::pColor == NIL, color, ::pColor ) )

   IF hb_defaultValue( lFilled, .F. )
      gdImageFilledRectangle( ::pImage, x1, y1, x2, y2, color )
   ELSE
      gdImageRectangle( ::pImage, x1, y1, x2, y2, color )
   ENDIF

   RETURN Self

METHOD Arc( x, y, nWidth, nHeight, nStartDegree, nEndDegree, lFilled, color, nStyle ) CLASS GDImage

   hb_default( @color, iif( ::pColor == NIL, color, ::pColor ) )

   IF hb_defaultValue( lFilled, .F. )
      gdImageFilledArc( ::pImage, x, y, nWidth, nHeight, nStartDegree, nEndDegree, color, hb_defaultValue( nStyle, gdArc ) )
   ELSE
      gdImageArc( ::pImage, x, y, nWidth, nHeight, nStartDegree, nEndDegree, color )
   ENDIF

   RETURN Self

METHOD Ellipse( x, y, nWidth, nHeight, lFilled, color ) CLASS GDImage

   hb_default( @color, iif( ::pColor == NIL, color, ::pColor ) )

   IF hb_defaultValue( lFilled, .F. )
      gdImageFilledEllipse( ::pImage, x, y, nWidth, nHeight, color )
   ELSE
      gdImageEllipse( ::pImage, x, y, nWidth, nHeight, color )
   ENDIF

   RETURN Self

METHOD LoadFromFile( cFile ) CLASS GDImage

   LOCAL aLoad := gdImageFromFile( cFile )

   Self := ::CloneDataFrom( aLoad[ 1 ] )
   aLoad[ 1 ] := NIL

   ::hFile := aLoad[ 2 ]
   ::cType := aLoad[ 3 ]
   ::cMime := aLoad[ 4 ]

   RETURN Self

#if defined( HB_LEGACY_LEVEL4 )

/* dummy. no longer needed */
METHOD Destroy() CLASS GDImage
   RETURN Self

#endif

METHOD Copy( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, oDestImage ) CLASS GDImage

   hb_default( @nWidth , ::Width() )
   hb_default( @nHeight, ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopy( oDestImage:pImage, ::pImage, hb_defaultValue( nDstX, 0 ), hb_defaultValue( nDstY, 0 ), hb_defaultValue( nSrcX, 0 ), hb_defaultValue( nSrcY, 0 ), nWidth, nHeight )

   RETURN oDestImage

METHOD CopyResized( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage ) CLASS GDImage

   hb_default( @nDstWidth , ::Width() )
   hb_default( @nDstHeight, ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nDstWidth, nDstHeight )
      ELSE
         oDestImage := GDImage():Create( nDstWidth, nDstHeight )
      ENDIF
   ENDIF

   gdImageCopyResized( oDestImage:pImage, ::pImage, ;
      hb_defaultValue( nDstX, 0 ), ;
      hb_defaultValue( nDstY, 0 ), ;
      hb_defaultValue( nSrcX, 0 ), ;
      hb_defaultValue( nSrcY, 0 ), ;
      nDstWidth, nDstHeight, ;
      hb_defaultValue( nSrcWidth, ::Width() ), ;
      hb_defaultValue( nSrcHeight, ::Height() ) )

   RETURN oDestImage

METHOD CopyResampled( nSrcX, nSrcY, nSrcWidth, nSrcHeight, nDstX, nDstY, nDstWidth, nDstHeight, oDestImage ) CLASS GDImage

   hb_default( @nDstWidth , ::Width() )
   hb_default( @nDstHeight, ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nDstWidth, nDstHeight )
      ELSE
         oDestImage := GDImage():Create( nDstWidth, nDstHeight )
      ENDIF
   ENDIF

   gdImageCopyResampled( oDestImage:pImage, ::pImage, ;
      hb_defaultValue( nDstX, 0 ), ;
      hb_defaultValue( nDstY, 0 ), ;
      hb_defaultValue( nSrcX, 0 ), ;
      hb_defaultValue( nSrcY, 0 ), ;
      nDstWidth, nDstHeight, ;
      hb_defaultValue( nSrcWidth, ::Width() ), ;
      hb_defaultValue( nSrcHeight, ::Height() ) )

   RETURN oDestImage

METHOD CopyRotated( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nAngle, oDestImage ) CLASS GDImage

   hb_default( @nWidth , ::Width() )
   hb_default( @nHeight, ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopyRotated( oDestImage:pImage, ::pImage, ;
      hb_defaultValue( nDstX, nWidth / 2 ), ;
      hb_defaultValue( nDstY, nHeight / 2 ), ;
      hb_defaultValue( nSrcX, 0 ), ;
      hb_defaultValue( nSrcY, 0 ), ;
      nWidth, nHeight, ;
      hb_defaultValue( nAngle, 90 ) )

   RETURN oDestImage

METHOD CopyMerge( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage ) CLASS GDImage

   hb_default( @nWidth , ::Width() )
   hb_default( @nHeight, ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopyMerge( oDestImage:pImage, ::pImage, ;
      hb_defaultValue( nDstX, 0 ), ;
      hb_defaultValue( nDstY, 0 ), ;
      hb_defaultValue( nSrcX, 0 ), ;
      hb_defaultValue( nSrcY, 0 ), ;
      nWidth, nHeight, ;
      hb_defaultValue( nPerc, 100 ) )

   RETURN oDestImage

METHOD CopyMergeGray( nSrcX, nSrcY, nWidth, nHeight, nDstX, nDstY, nPerc, oDestImage ) CLASS GDImage

   hb_default( @nWidth , ::Width() )
   hb_default( @nHeight, ::Height() )

   IF oDestImage == NIL
      IF ::IsTrueColor()
         oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
      ELSE
         oDestImage := GDImage():Create( nWidth, nHeight )
      ENDIF
   ENDIF

   gdImageCopyMergeGray( oDestImage:pImage, ::pImage, ;
      hb_defaultValue( nDstX, 0 ), ;
      hb_defaultValue( nDstY, 0 ), ;
      hb_defaultValue( nSrcX, 0 ), ;
      hb_defaultValue( nSrcY, 0 ), ;
      nWidth, nHeight, ;
      hb_defaultValue( nPerc, 100 ) )

   RETURN oDestImage

METHOD CopyZoomed( nPerc, nSrcX, nSrcY, nSrcWidth, nSrcHeight ) CLASS GDImage

   LOCAL oDestImage
   LOCAL nDstWidth, nDstHeight

   hb_default( @nPerc     , 100 )
   hb_default( @nSrcWidth , ::Width() )
   hb_default( @nSrcHeight, ::Height() )

   IF nPerc < 0
      nPerc := 100
   ENDIF

   nDstWidth  := nSrcWidth * nPerc / 100
   nDstHeight := nSrcHeight * nPerc / 100

   IF ::IsTrueColor()
      oDestImage := GDImage():CreateTrueColor( nDstWidth, nDstHeight )
   ELSE
      oDestImage := GDImage():Create( nDstWidth, nDstHeight )
   ENDIF

   gdImageCopyResampled( oDestImage:pImage, ::pImage, ;
      0, ;
      0, ;
      hb_defaultValue( nSrcX, 0 ), ;
      hb_defaultValue( nSrcY, 0 ), ;
      nDstWidth, nDstHeight, nSrcWidth, nSrcHeight )

   RETURN oDestImage

METHOD Rotate( nAngle, lInside ) CLASS GDImage

   LOCAL oDestImage
   LOCAL nWidth, nHeight
   LOCAL nAngRad := nAngle * Pi() / 180

   hb_default( @lInside, .F. )

   IF lInside
      nWidth  := ::Width()
      nHeight := ::Height()
   ELSE
      nWidth  := ::Width() * Cos( nAngRad ) + ::Height() * Sin( nAngRad )
      nHeight := ::Width() * Sin( nAngRad ) + ::Height() * Cos( nAngRad )
   ENDIF

   IF ::IsTrueColor()
      oDestImage := GDImage():CreateTrueColor( nWidth, nHeight )
   ELSE
      oDestImage := GDImage():Create( nWidth, nHeight )
   ENDIF
   IF lInside
      ::CopyRotated( ,,,,,, nAngle, oDestImage )
   ELSE
      ::CopyRotated( ,,,, nWidth - nWidth / 2, nHeight - nHeight / 2, nAngle, oDestImage )
   ENDIF

   Self := ::CloneDataFrom( oDestImage )

   RETURN Self

METHOD Crop( nX, nY, nWidth, nHeight ) CLASS GDImage

   Self := ::CloneDataFrom( ::CopyResized( nX, nY, nWidth, nHeight, 0, 0, nWidth, nHeight ) )

   RETURN Self

METHOD Resize( nWidth, nHeight ) CLASS GDImage

   Self := ::CloneDataFrom( ::CopyResampled( 0, 0,,, 0, 0, nWidth, nHeight ) )

   RETURN Self

METHOD Zoom( nPerc ) CLASS GDImage

   Self := ::CloneDataFrom( ::CopyZoomed( nPerc ) )

   RETURN Self

METHOD Clone() CLASS GDImage

   LOCAL oDestImage
   LOCAL pImage

   IF ::IsTrueColor()
      oDestImage := GDImage():CreateTrueColor( ::Width(), ::Height() )
   ELSE
      oDestImage := GDImage():Create( ::Width(), ::Height() )
   ENDIF

   pImage := oDestImage:pImage
   oDestImage := oDestImage:CloneDataFrom( Self )
   oDestImage:pImage := pImage
   ::Copy( 0, 0, ::Width(), ::Height(), 0, 0, oDestImage )

#if 0
   pImage := oDestImage:pImage
   oDestImage := NIL
   oDestImage:pImage := pImage
#endif

   RETURN oDestImage

METHOD Say( x, y, cString, color, nAlign ) CLASS GDImage

   LOCAL nWidth, nLen
   LOCAL nPosX

   SWITCH hb_defaultValue( nAlign, gdAlignLeft )
   CASE gdAlignCenter
      nWidth := ::GetFontWidth()
      nLen   := hb_BLen( cString )
      nPosX  := x - ( nLen / 2 * nWidth )
      EXIT
   CASE gdAlignRight
      nWidth := ::GetFontWidth()
      nLen   := hb_BLen( cString )
      nPosX  := x - ( nLen * nWidth )
      EXIT
   OTHERWISE
      nPosX  := x
   ENDSWITCH

   gdImageString( ::pImage, ::pFont, nPosX, y, cString, hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ) )

   RETURN Self

METHOD SayFreeType( x, y, cString, cFontName, nPitch, nAngle, color, nAlign, ;
      nLineSpacing, nCharMap, nResolution ) CLASS GDImage

   LOCAL nWidth, nLen
   LOCAL nPosX

   hb_default( @cFontName, ::cFontName )
   hb_default( @nPitch, ::nFontPitch )

   SWITCH hb_defaultValue( nAlign, gdAlignLeft )
   CASE gdAlignCenter
      nWidth := nPitch
      nLen   := hb_ULen( cString )
      nPosX  := x - ( ( nLen / 2 ) * nWidth )
      EXIT
   CASE gdAlignRight
      nWidth := gdImageFTWidth( cFontName, nPitch )
      nLen   := hb_ULen( cString )
      nPosX  := x - ( nLen * nWidth )
      EXIT
   OTHERWISE
      nPosX  := x
   ENDSWITCH

   gdImageStringFT( ::pImage, ;
      hb_defaultValue( color, iif( ::pColor == NIL, color, ::pColor ) ), ;
      cFontName, nPitch, ;
      ::Radians( hb_defaultValue( nAngle, ::nFontAngle ) ), ;
      nPosX, y, cString, nLineSpacing, nCharMap, nResolution )

   RETURN Self

METHOD CloneDataFrom( oSrc ) CLASS GDImage

   // copy values from Source to Dest
   // please update in case of new data

   ::pImage     := oSrc:pImage
   ::pBrush     := oSrc:pBrush
   ::pTile      := oSrc:pTile
   ::pFont      := oSrc:pFont
   ::pColor     := oSrc:pColor

   ::cFontName  := oSrc:cFontName
   ::nFontPitch := oSrc:nFontPitch
   ::nFontAngle := oSrc:nFontAngle

   ::aPoints    := AClone( oSrc:aPoints )
   ::aStyles    := AClone( oSrc:aStyles )

   ::hFile      := oSrc:hFile
   ::cType      := oSrc:cType
   ::cMime      := oSrc:cMime

   RETURN Self
