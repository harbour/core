/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               07Aug2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS IdeToolbar INHERIT IdeObject

   DATA   oWidget
   DATA   cName
   DATA   oParent
   DATA   hItems                                  INIT   {=>}

   DATA   allowedAreas                            INIT   Qt_TopToolBarArea
   DATA   initialArea                             INIT   Qt_TopToolBarArea
   DATA   orientation                             INIT   Qt_Horizontal
   DATA   size                                    INIT   QSize():new( 16,16 )
   DATA   moveable                                INIT   .f.
   DATA   floatable                               INIT   .f.

   METHOD new( cName, oParent )
   METHOD create( cName, oParent )
   METHOD destroy()
   METHOD execEvent( cEvent, p )
   METHOD addToolButton( cName, cDesc, cImage, bAction, lCheckable )
   METHOD setItemChecked( cName, lState )
   METHOD setItemEnabled( cName, lEnabled )
   METHOD addWidget( cName, qWidget )
   METHOD contains( cName )                       INLINE hb_hHasKey( ::hItems, cName )

   ERROR HANDLER onError( ... )
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:new( cName, oParent )

   ::cName   := cName
   ::oParent := oParent

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:create( cName, oParent )

   STATIC nID := 0

   DEFAULT cName   TO ::cName
   DEFAULT oParent TO ::oParent
   ::cName   := cName
   ::oParent := oParent

   DEFAULT ::cName TO "IdeToolbar_" + hb_ntos( ++nID )

   ::oWidget := QToolbar():new()
   ::oWidget:setObjectName( ::cName )
   ::oWidget:setAllowedAreas( ::allowedAreas )
   ::oWidget:setOrientation( ::orientation )
   ::oWidget:setIconSize( ::size )
   ::oWidget:setMovable( ::moveable )
   ::oWidget:setFloatable( ::floatable )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:onError( ... )
   LOCAL cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF
   RETURN ::oWidget:&cMsg( ... )

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:execEvent( cEvent, p )

   HB_SYMBOL_UNUSED( p )

   SWITCH cEvent
   CASE "buttonNew_clicked"
      EXIT

   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:addWidget( cName, qWidget )
   LOCAL qAction

   STATIC nID := 0

   DEFAULT cName TO "IdeToolButtonWidget_" + hb_ntos( ++nID )

   qAction := QWidgetAction():new( ::oWidget )
   qAction:setDefaultWidget( qWidget )
   ::oWidget:addAction( qAction )

   ::hItems[ cName ] := qWidget

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:addToolButton( cName, cDesc, cImage, bAction, lCheckable )
   LOCAL oButton, qAction

   STATIC nID := 0

   DEFAULT cName      TO "IdeToolButton_" + hb_ntos( ++nID )
   DEFAULT cDesc      TO ""
   DEFAULT lCheckable TO .f.

   oButton := QToolButton():new( ::oWidget )
   oButton:setObjectName( cName )
   oButton:setTooltip( cDesc )
   oButton:setIcon( cImage )
   oButton:setCheckable( lCheckable )

   IF hb_isBlock( bAction )
      ::connect( oButton, "clicked()", bAction )
   ENDIF
   #if 1
   qAction := QWidgetAction():new( ::oWidget )
   qAction:setDefaultWidget( oButton )
   ::oWidget:addAction( qAction )
   #else
   ::oWidget:addWidget( oButton )
   #endif

   ::hItems[ cName ] := oButton
   RETURN oButton

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:setItemChecked( cName, lState )
   LOCAL lOldState

   IF hb_hHasKey( ::hItems, cName )
      IF ::hItems[ cName ]:isCheckable()
         lOldState := ::hItems[ cName ]:isChecked()
         IF hb_isLogical( lState )
            ::hItems[ cName ]:setChecked( lState )
         ENDIF
      ENDIF
   ENDIF

   RETURN lOldState

/*----------------------------------------------------------------------*/

METHOD IdeToolbar:setItemEnabled( cName, lEnabled )
   LOCAL lOldEnabled

   IF hb_hHasKey( ::hItems, cName )
      lOldEnabled := ::hItems[ cName ]:isEnabled()
      IF hb_isLogical( lEnabled )
         ::hItems[ cName ]:setEnabled( lEnabled )
      ENDIF
   ENDIF

   RETURN lOldEnabled

/*----------------------------------------------------------------------*/

