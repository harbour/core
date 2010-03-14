/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
 *                  Pritpal Bedi <pritpal@vouchcac.com>
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
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aCenter, nRadius, aEllipse, nStartAngle, nSweepAngle, nFill )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraBitBlt( oTargetPS, oSourcePS, aTargetRect, aSourceRect, nRasterOP, nCompress  )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oTargetPS, oSourcePS, aTargetRect, aSourceRect, nRasterOP, nCompress  )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraBox( oPS, aLeftBottom, aRightTop, nFill, nHRadius, nVRadius )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aLeftBottom, aRightTop, nFill, nHRadius, nVRadius )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraCaptionStr( oPS, aStartPoint, aEndPoint, cCaption, nAlign, nTabChars  )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aStartPoint, aEndPoint, cCaption, nAlign, nTabChars  )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraError( oPS )
   LOCAL nErrorCode := 0

   HB_SYMBOL_UNUSED( oPS )

   RETURN nErrorCode

/*----------------------------------------------------------------------*/

FUNCTION GraFocusRect( oPS, aStartPoint, aEndPoint )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aStartPoint, aEndPoint )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraGetRGBIntensity( nRGBColor )
   LOCAL aRGB := {}

   HB_SYMBOL_UNUSED( nRGBColor )

   RETURN aRGB

/*----------------------------------------------------------------------*/

FUNCTION GraIsRGBColor( nColor )
   LOCAL lIsRGBColor := .f.

   HB_SYMBOL_UNUSED( nColor )

   RETURN lIsRGBColor

/*----------------------------------------------------------------------*/

FUNCTION GraLine( oPS, aStartPoint, aEndPoint )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aStartPoint, aEndPoint )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraMakeRGBColor( aRGB )
   LOCAL nRGB

   IF hb_isArray( aRGB ) .and. len( aRGB ) == 3
      IF hb_isNumeric( aRGB[ 1 ] ) .and. ( aRGB[ 1 ] >= 0 ) .and. ( aRGB[ 1 ] <= 255 )
         IF hb_isNumeric( aRGB[ 2 ] ) .and. ( aRGB[ 2 ] >= 0 ) .and. ( aRGB[ 2 ] <= 255 )
            IF hb_isNumeric( aRGB[ 3 ] ) .and. ( aRGB[ 3 ] >= 0 ) .and. ( aRGB[ 3 ] <= 255 )
               nRGB := ( aRGB[ 1 ] + ( aRGB[ 2 ] * 256 ) + ( aRGB[ 3 ] * 256 * 256 ) ) + ( 256 * 256 * 256 )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN nRGB

/*----------------------------------------------------------------------*/

FUNCTION GraMarker( oPS, aPoint )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aPoint )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathBegin( oPS )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathClip( oPS, lClip, nClipMode )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, lClip, nClipMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathEnd( oPS, lCloseFigure )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, lCloseFigure )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathFill( oPS, nFillMode )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, nFillMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPathOutline( oPS )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraPos( oPS, aPoint )
   LOCAL aPenPosition := { 0,0 }

   HB_SYMBOL_UNUSED( oPS, aPoint )

   RETURN aPenPosition

/*----------------------------------------------------------------------*/

FUNCTION GraQueryTextBox( oPS, cString )
   LOCAL aPoints := { 0,0 }

   HB_SYMBOL_UNUSED( oPS, cString )

   RETURN aPoints

/*----------------------------------------------------------------------*/

FUNCTION GraRotate( oPS, aMatrix, nAngle, aPoint , nMode )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aMatrix, nAngle, aPoint , nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraScale( oPS, aMatrix, aScale, aPoint , nMode )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aMatrix, aScale, aPoint , nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegClose( oPS )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegDestroy( oPS, nSegmentID )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, nSegmentID )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegDraw( oPS, nSegmentID, aMatrix, nMode )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, nSegmentID, aMatrix, nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegDrawMode( oPS, nDrawMode )
   LOCAL nOldMode := 0

   HB_SYMBOL_UNUSED( oPS, nDrawMode )

   RETURN nOldMode

/*----------------------------------------------------------------------*/

FUNCTION GraSegFind( oPS, aPoint )
   LOCAL aSegmentIDs := {}

   HB_SYMBOL_UNUSED( oPS, aPoint )

   RETURN aSegmentIDs

/*----------------------------------------------------------------------*/

FUNCTION GraSegOpen( oPS, nMode, nSegmentID )

   HB_SYMBOL_UNUSED( oPS, nMode, nSegmentID )

   RETURN nSegmentID

/*----------------------------------------------------------------------*/

FUNCTION GraSegPickResolution( oPS, aCenter, aSize )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aCenter, aSize )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSegPriority( oPS, nSegmentA, nSegmentB, nPriority )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, nSegmentA, nSegmentB, nPriority )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrArea( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS, aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrLine( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS, aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrMarker( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS, aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetAttrString( oPS, aAttributes )
   LOCAL aOldAttributes := {}

   HB_SYMBOL_UNUSED( oPS, aAttributes )

   RETURN aOldAttributes

/*----------------------------------------------------------------------*/

FUNCTION GraSetColor( oPS, nForeground, nBackground )
   LOCAL aOldColor := {}

   HB_SYMBOL_UNUSED( oPS, nForeground, nBackground )

   RETURN aOldColor

/*----------------------------------------------------------------------*/

FUNCTION GraSetFont( oPS, oXbpFont )
   LOCAL oXbpCurrentFont := NIL

   HB_SYMBOL_UNUSED( oPS, oXbpFont )

   RETURN oXbpCurrentFont

/*----------------------------------------------------------------------*/

FUNCTION GraSpline( oPS, aPoints, lPenPos )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED( oPS, aPoints, lPenPos )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraStringAt( oPS, aPoint, cString )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED(  oPS, aPoint, cString )

   RETURN lSuccess

/*----------------------------------------------------------------------*/

FUNCTION GraTranslate( oPS, aMatrix, nXDiff, nYDiff, nMode )
   LOCAL lSuccess := .t.

   HB_SYMBOL_UNUSED(  oPS, aMatrix, nXDiff, nYDiff, nMode )

   RETURN lSuccess

/*----------------------------------------------------------------------*/
