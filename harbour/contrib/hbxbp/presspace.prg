/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                Xbase++ Compatible xbpPresSpace Class
 *
 *                            Pritpal Bedi
 *                              08Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpPresSpace

   DATA     mode                                  INIT      XBPPS_MODE_NORMAL
   DATA     d

   METHOD   init()
   METHOD   create( oDevice, aPageSize, nUnits )
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()                             VIRTUAL

   METHOD   drawMode( nDrawMode )
   METHOD   device()
   METHOD   lastError()
   METHOD   mapPoint( aPoint, lMapToDevice )
   METHOD   mapColor( aRGB, lExactMatch )
   METHOD   maxColorIndex( lCompatible )
   METHOD   setAttrArea( aAttributes )
   METHOD   setAttrLine( aAttributes )
   METHOD   setAttrMarker( aAttributes )
   METHOD   setAttrString( aAttributes )
   METHOD   setColor( nForeground, nBackground )
   METHOD   setColorIndex( nColorIndex, aRGB )
   METHOD   setFont( oXbpFont )
   METHOD   setGraTransform( aMatrix, nMode )
   METHOD   setPageSize( aPageSize, nUnits )
   METHOD   setViewPort( aViewPort )

   PROTECTED:

   DATA     aPalette                              INIT      {}
   DATA     oDevice
   DATA     nDrawMode                             INIT      GRA_DM_DRAWANDRETAIN
   DATA     lMapColorIndex
   DATA     nColorFG                              INIT      GraMakeRGBColor( { 0,0,0 } )
   DATA     nColorBG                              INIT      GraMakeRGBColor( { 255,255,255 } )
   DATA     oXbpFont
   DATA     aMatrix                               INIT      { {1.0,0.0,0.0}, {0.0,1.0,0.0}, {0.0,0.0,1.0} }
   DATA     nGraTransMode
   DATA     aPageSize
   DATA     nUnits
   DATA     aViewPort                             INIT      {}
   DATA     x                                     INIT      0
   DATA     y                                     INIT      0

   /* To be used in GraArc(), GraBox(), GraPathFill */
   DATA     aGRA_AA_ATTR                          INIT      { GRA_CLR_NEUTRAL     , ;
                                                              GRA_CLR_NEUTRAL     , ;
                                                              GRA_CLR_BACKGROUND  , ;
                                                              GRA_FGMIX_OVERPAINT , ;
                                                              GRA_BGMIX_LEAVEALONE, ;
                                                              GRA_SYM_SOLID       , ;
                                                              { 0, 0 }              }

   /* To be used in GraLine(), GraSpline(),  GraPathOutLine() and GraArc() and GraBox() in certain cases */
   DATA     aGRA_AL_ATTR                          INIT      { GRA_CLR_NEUTRAL     , ;
                                                              GRA_FGMIX_OVERPAINT , ;
                                                              GRA_LINEWIDTH_NORMAL, ;
                                                              GRA_LINETYPE_SOLID    }

   /* To be used with GraMarker() */
   DATA     aGRA_AM_ATTR                          INIT      { GRA_CLR_NEUTRAL     , ;
                                                              GRA_CLR_BACKGROUND  , ;
                                                              GRA_FGMIX_OVERPAINT , ;
                                                              GRA_BGMIX_LEAVEALONE, ;
                                                              GRA_MARKSYM_CROSS   , ;
                                                              { 1, 1 }              }

   /* To be used with GraString() */
   DATA     aGRA_AS_ATTR                          INIT      { GRA_CLR_NEUTRAL     , ;
                                                              GRA_CLR_BACKGROUND  , ;
                                                              GRA_FGMIX_OVERPAINT , ;
                                                              GRA_BGMIX_LEAVEALONE, ;
                                                              { 1, 1 }            , ;
                                                              { 1, 0 }            , ;
                                                              { 0, 1 }            , ;
                                                              GRA_CHDIRN_LEFTRIGHT, ;
                                                              GRA_HALIGN_LEFT     , ;
                                                              GRA_VALIGN_BASE     , ;
                                                              0                   , ;
                                                              0                     }
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:init()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:create( oDevice, aPageSize, nUnits )

   DEFAULT nUnits TO GRA_PU_PIXEL

   IF HB_ISOBJECT( oDevice )
      ::oDevice := oDevice
   ENDIF
   IF HB_ISARRAY( aPageSize ) .AND. len( aPageSize ) == 2
      ::aPageSize := aPageSize
   ENDIF

   ::nUnits := nUnits

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:drawMode( nDrawMode )
   LOCAL nOldMode := ::nDrawMode

   IF HB_ISNUMERIC( nDrawMode ) .AND. nDrawMode >= GRA_DM_DRAW .AND. nDrawMode <= GRA_DM_DRAWANDRETAIN
      ::nDrawMode := nDrawMode
   ENDIF

   RETURN nOldMode

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:device()
   RETURN ::oDevice

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:lastError()
   LOCAL nError := 0

   RETURN nError

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:mapPoint( aPoint, lMapToDevice )

   DEFAULT  lMapToDevice TO .t.

   IF HB_ISARRAY( aPoint )
      IF lMapToDevice
         aPoint := { 0,0 }
      ENDIF
   ENDIF

   RETURN aPoint

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:mapColor( aRGB, lExactMatch )
   LOCAL xValue := 0

   HB_SYMBOL_UNUSED( aRGB )

   DEFAULT lExactMatch TO .f.


   RETURN xValue //<nColorIndex>|<nRGBColor>|NIL )

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:maxColorIndex( lCompatible )

   DEFAULT lCompatible TO .t.

   RETURN 255

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setAttrArea( aAttributes )
   LOCAL v
   LOCAL aOldAttr := ::aGRA_AA_ATTR

   IF HB_ISARRAY( aAttributes ) .AND. len( aAttributes ) == GRA_AA_COUNT
      FOR EACH v IN aAttributes
         IF !empty( v )
             ::aGRA_AA_ATTR[ v:__enumIndex() ] := v
         ENDIF
      NEXT
   ENDIF

   RETURN aOldAttr

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setAttrLine( aAttributes )
   LOCAL v
   LOCAL aOldAttr := ::aGRA_AL_ATTR

   IF HB_ISARRAY( aAttributes ) .AND. len( aAttributes ) == GRA_AL_COUNT
      FOR EACH v IN aAttributes
         IF !empty( v )
             ::aGRA_AL_ATTR[ v:__enumIndex() ] := v
         ENDIF
      NEXT
   ENDIF

   RETURN aOldAttr

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setAttrMarker( aAttributes )
   LOCAL v
   LOCAL aOldAttr := ::aGRA_AM_ATTR

   IF HB_ISARRAY( aAttributes ) .AND. len( aAttributes ) == GRA_AM_COUNT
      FOR EACH v IN aAttributes
         IF !empty( v )
             ::aGRA_AM_ATTR[ v:__enumIndex() ] := v
         ENDIF
      NEXT
   ENDIF

   RETURN aOldAttr

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setAttrString( aAttributes )
   LOCAL v
   LOCAL aOldAttr := ::aGRA_AS_ATTR

   IF HB_ISARRAY( aAttributes ) .AND. len( aAttributes ) == GRA_AS_COUNT
      FOR EACH v IN aAttributes
         IF !empty( v )
             ::aGRA_AS_ATTR[ v:__enumIndex() ] := v
         ENDIF
      NEXT
   ENDIF

   RETURN aOldAttr

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setColor( nForeground, nBackground )
   LOCAL aOldAttr := { ::nColorFG, ::nColorBG }

   IF !empty( nForeground )
      ::nColorFG := nForeground
   ENDIF
   IF !empty( nBackground )
      ::nColorBG := nBackground
   ENDIF

   RETURN aOldAttr

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setColorIndex( nColorIndex, aRGB )
   LOCAL a_
   LOCAL aOldAttr := {}

   IF HB_ISNUMERIC( nColorIndex ) .AND. nColorIndex > 15 .AND. nColorIndex <= 255 .AND. nColorIndex < len( ::aPalette )
      aOldAttr := ::aPalette[ nColorIndex ]
      IF HB_ISARRAY( aRGB )
         IF HB_ISARRAY( aRGB[ 1 ] )
            FOR EACH a_ IN aRGB
               ::aPalette[ nColorIndex + a_:__enumIndex() - 1 ] := a_
            NEXT
         ELSE
            ::aPalette[ nColorIndex ] := aRGB
         ENDIF
      ENDIF
   ENDIF

   RETURN aOldAttr

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setFont( oXbpFont )
   LOCAL oOldFont := ::oXbpFont

   IF HB_ISOBJECT( oXbpFont )
      ::oXbpFont := oXbpFont
   ENDIF

   RETURN oOldFont

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setGraTransform( aMatrix, nMode )
   LOCAL oOldMatrix := ::aMatrix

   DEFAULT nMode TO GRA_TRANSFORM_REPLACE

   IF HB_ISARRAY( aMatrix )
      IF nMode == GRA_TRANSFORM_REPLACE
         ::aMatrix := aMatrix
      ELSE
         // Recalculate ??
      ENDIF
   ENDIF

   RETURN oOldMatrix

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setPageSize( aPageSize, nUnits )
   LOCAL aOldSize := ::aPageSize

   IF HB_ISARRAY( aPageSize ) .AND. len( aPageSize ) == 2
      ::aPageSize := aPageSize
   ENDIF
   IF HB_ISNUMERIC( nUnits )
      ::nUnits := nUnits
   ENDIF

   RETURN aOldSize

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:setViewPort( aViewPort )
   LOCAL aOldViewPort := ::aViewPort

   IF HB_ISARRAY( aViewPort )
      ::aViewPort := aViewPort
   ENDIF

   RETURN aOldViewPort

/*----------------------------------------------------------------------*/

