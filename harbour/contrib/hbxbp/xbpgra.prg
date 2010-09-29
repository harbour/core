/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                 Xbase++ Compatible Gra*() Functions
 *
 *                            Pritpal Bedi
 *                              13Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"
#include "gra.ch"

/*----------------------------------------------------------------------*/

FUNCTION GraArc( oPS, aCenter, nRadius, aEllipse, nStartAngle, nSweepAngle, nFill )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aCenter )
   HB_SYMBOL_UNUSED( nRadius )
   HB_SYMBOL_UNUSED( aEllipse )
   HB_SYMBOL_UNUSED( nStartAngle )
   HB_SYMBOL_UNUSED( nSweepAngle )
   HB_SYMBOL_UNUSED( nFill )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraBitBlt( oTargetPS, oSourcePS, aTargetRect, aSourceRect, nRasterOP, nCompress  )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oTargetPS )
   HB_SYMBOL_UNUSED( oSourcePS )
   HB_SYMBOL_UNUSED( aTargetRect )
   HB_SYMBOL_UNUSED( aSourceRect )
   HB_SYMBOL_UNUSED( nRasterOP )
   HB_SYMBOL_UNUSED( nCompress )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraBox( oPS, aLeftBottom, aRightTop, nFill, nHRadius, nVRadius )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aLeftBottom )
   HB_SYMBOL_UNUSED( aRightTop )
   HB_SYMBOL_UNUSED( nFill )
   HB_SYMBOL_UNUSED( nHRadius )
   HB_SYMBOL_UNUSED( nVRadius )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraCaptionStr( oPS, aStartPoint, aEndPoint, cCaption, nAlign, nTabChars  )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aStartPoint )
   HB_SYMBOL_UNUSED( aEndPoint )
   HB_SYMBOL_UNUSED( cCaption )
   HB_SYMBOL_UNUSED( nAlign )
   HB_SYMBOL_UNUSED( nTabChars )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraError( oPS )
   LOCAL nErrorCode := 0

   HB_SYMBOL_UNUSED( oPS )

   RETURN nErrorCode

/*----------------------------------------------------------------------*/

FUNCTION GraFocusRect( oPS, aStartPoint, aEndPoint )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aStartPoint )
   HB_SYMBOL_UNUSED( aEndPoint )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraGetRGBIntensity( nRGBColor )
   LOCAL cRGBHex

   DEFAULT nRGBColor TO 0

   cRGBHex := HB_NumToHex( nRGBColor, 6 )

   RETURN { HB_HexToNum( SubStr( cRGBHex, 1, 2 ) ),;
            HB_HexToNum( SubStr( cRGBHex, 3, 2 ) ),;
            HB_HexToNum( SubStr( cRGBHex, 5, 2 ) ) }

/*----------------------------------------------------------------------*/

FUNCTION GraIsRGBColor( nColor )
   LOCAL lIsRGBColor := .F.

   HB_SYMBOL_UNUSED( nColor )

   RETURN lIsRGBColor

/*----------------------------------------------------------------------*/

FUNCTION GraLine( oPS, aStartPoint, aEndPoint )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aStartPoint )
   HB_SYMBOL_UNUSED( aEndPoint )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraMakeRGBColor( aRGB )
   LOCAL nRGB

   IF hb_isArray( aRGB ) .AND. Len( aRGB ) == 3
      IF hb_isNumeric( aRGB[ 1 ] ) .AND. ( aRGB[ 1 ] >= 0 ) .AND. ( aRGB[ 1 ] <= 255 )
         IF hb_isNumeric( aRGB[ 2 ] ) .AND. ( aRGB[ 2 ] >= 0 ) .AND. ( aRGB[ 2 ] <= 255 )
            IF hb_isNumeric( aRGB[ 3 ] ) .AND. ( aRGB[ 3 ] >= 0 ) .AND. ( aRGB[ 3 ] <= 255 )
               nRGB := ( aRGB[ 1 ] + ( aRGB[ 2 ] * 256 ) + ( aRGB[ 3 ] * 256 * 256 ) ) + ( 256 * 256 * 256 )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN nRGB

/*----------------------------------------------------------------------*/

FUNCTION GraMarker( oPS, aPoint )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aPoint )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathBegin( oPS )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathClip( oPS, lClip, nClipMode )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( lClip )
   HB_SYMBOL_UNUSED( nClipMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathEnd( oPS, lCloseFigure )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( lCloseFigure )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathFill( oPS, nFillMode )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( nFillMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathOutline( oPS )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPos( oPS, aPoint )
   LOCAL aPenPosition := { 0, 0 }

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aPoint )

   RETURN aPenPosition

/*----------------------------------------------------------------------*/

FUNCTION GraQueryTextBox( oPS, cString )
   LOCAL aPoints := { 0, 0 }

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( cString )

   RETURN aPoints

/*----------------------------------------------------------------------*/

FUNCTION GraRotate( oPS, aMatrix, nAngle, aPoint , nMode )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aMatrix )
   HB_SYMBOL_UNUSED( nAngle )
   HB_SYMBOL_UNUSED( aPoint )
   HB_SYMBOL_UNUSED( nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraScale( oPS, aMatrix, aScale, aPoint , nMode )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aMatrix )
   HB_SYMBOL_UNUSED( aScale )
   HB_SYMBOL_UNUSED( aPoint )
   HB_SYMBOL_UNUSED( nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegClose( oPS )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegDestroy( oPS, nSegmentID )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( nSegmentID )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegDraw( oPS, nSegmentID, aMatrix, nMode )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( nSegmentID )
   HB_SYMBOL_UNUSED( aMatrix )
   HB_SYMBOL_UNUSED( nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegDrawMode( oPS, nDrawMode )
   LOCAL nOldMode := 0

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( nDrawMode )

   RETURN nOldMode

/*----------------------------------------------------------------------*/

FUNCTION GraSegFind( oPS, aPoint )
   LOCAL aSegmentIDs := {}

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aPoint )

   RETURN aSegmentIDs

/*----------------------------------------------------------------------*/

FUNCTION GraSegOpen( oPS, nMode, nSegmentID )

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( nMode )
   HB_SYMBOL_UNUSED( nSegmentID )

   RETURN nSegmentID

/*----------------------------------------------------------------------*/

FUNCTION GraSegPickResolution( oPS, aCenter, aSize )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aCenter )
   HB_SYMBOL_UNUSED( aSize )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegPriority( oPS, nSegmentA, nSegmentB, nPriority )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( nSegmentA )
   HB_SYMBOL_UNUSED( nSegmentB )
   HB_SYMBOL_UNUSED( nPriority )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrArea( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrLine( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrMarker( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrString( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetColor( oPS, nForeground, nBackground )
   LOCAL aOldColor := {}

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( nForeground )
   HB_SYMBOL_UNUSED( nBackground )

   RETURN aOldColor

/*----------------------------------------------------------------------*/

FUNCTION GraSetFont( oPS, oXbpFont )
   LOCAL oXbpCurrentFont := NIL

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( oXbpFont )

   RETURN oXbpCurrentFont

/*----------------------------------------------------------------------*/

FUNCTION GraSpline( oPS, aPoints, lPenPos )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aPoints )
   HB_SYMBOL_UNUSED( lPenPos )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraStringAt( oPS, aPoint, cString )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aPoint )
   HB_SYMBOL_UNUSED( cString )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraTranslate( oPS, aMatrix, nXDiff, nYDiff, nMode )
   LOCAL lSuccess := .T.

   HB_SYMBOL_UNUSED( oPS )
   HB_SYMBOL_UNUSED( aMatrix )
   HB_SYMBOL_UNUSED( nXDiff )
   HB_SYMBOL_UNUSED( nYDiff )
   HB_SYMBOL_UNUSED( nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSaveScreen( ... )
   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION GraInitMatrix( ... )
   RETURN {}

/*----------------------------------------------------------------------*/
