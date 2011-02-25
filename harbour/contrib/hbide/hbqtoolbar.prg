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
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

CLASS HbqToolbar

   DATA   oWidget
   DATA   cName
   DATA   oParent
   DATA   hItems                                  INIT   {=>}

   DATA   allowedAreas                            INIT   Qt_TopToolBarArea
   DATA   initialArea                             INIT   Qt_TopToolBarArea
   DATA   orientation                             INIT   Qt_Horizontal
   DATA   size                                    INIT   QSize( 16,16 )
   DATA   moveable                                INIT   .f.
   DATA   floatable                               INIT   .f.

   DATA   lPressed                                INIT   .f.
   DATA   qPos
   DATA   qDrag
   DATA   qMime
   DATA   qDropAction
   DATA   qPix
   DATA   qByte

   METHOD new( cName, oParent )
   METHOD create( cName, oParent )
   METHOD destroy()
   METHOD execEvent( cEvent, p, p1 )
   METHOD addToolButton( cName, cDesc, cImage, bAction, lCheckable, lDragEnabled )
   METHOD setItemChecked( cName, lState )
   METHOD setItemEnabled( cName, lEnabled )
   METHOD addWidget( cName, qWidget )
   METHOD contains( cName )                       INLINE hb_hHasKey( ::hItems, cName )
   METHOD getItem( cName )                        INLINE iif( hb_hHasKey( ::hItems, cName ), ::hItems[ cName ], NIL )

   ERROR HANDLER onError( ... )
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:new( cName, oParent )

   ::cName   := cName
   ::oParent := oParent

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:create( cName, oParent )

   STATIC nID := 0

   DEFAULT cName   TO ::cName
   DEFAULT oParent TO ::oParent
   ::cName   := cName
   ::oParent := oParent

   DEFAULT ::cName TO "HbqToolbar_" + hb_ntos( ++nID )

   ::oWidget := QToolbar()
   ::oWidget:setObjectName( ::cName )
   ::oWidget:setAllowedAreas( ::allowedAreas )
   ::oWidget:setOrientation( ::orientation )
   ::oWidget:setIconSize( ::size )
   ::oWidget:setMovable( ::moveable )
   ::oWidget:setFloatable( ::floatable )
   ::oWidget:setFocusPolicy( Qt_NoFocus )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:onError( ... )
   LOCAL cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF
   RETURN ::oWidget:&cMsg( ... )

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:execEvent( cEvent, p, p1 )
   LOCAL qEvent, qRC

   qEvent := p

   SWITCH cEvent
   CASE "QEvent_MouseLeave"
      EXIT

   CASE "QEvent_MouseMove"
      qRC := QRect( ::qPos:x() - 5, ::qPos:y() - 5, 10, 10 ):normalized()
      IF qRC:contains( qEvent:pos() )
         ::qByte := QByteArray( ::hItems[ p1 ]:objectName() )

         ::qMime := QMimeData()
         ::qMime:setData( "application/x-toolbaricon", ::qByte )
         ::qMime:setHtml( ::hItems[ p1 ]:objectName() )

         ::qPix  := QPixmap( QIcon( ::hItems[ p1 ]:icon ):pixmap( 16,16 ) )

         ::qDrag := QDrag( hbide_setIde():oDlg:oWidget )
         ::qDrag:setMimeData( ::qMime )
         ::qDrag:setPixmap( ::qPix )
         ::qDrag:setHotSpot( QPoint( 15,15 ) )
         ::qDrag:setDragCursor( ::qPix, Qt_MoveAction )
         ::qDropAction := ::qDrag:exec( Qt_MoveAction )
         ::qDrag := NIL
         ::qPos  := NIL
         ::hItems[ p1 ]:setChecked( .f. )
         ::hItems[ p1 ]:setWindowState( 0 )
      ENDIF
      EXIT

   CASE "QEvent_MouseRelease"
      ::qDrag := NIL
      EXIT

   CASE "QEvent_MousePress"
      ::qPos := qEvent:pos()
      EXIT

   CASE "buttonNew_clicked"
      EXIT

   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:addWidget( cName, qWidget )
   LOCAL qAction

   DEFAULT cName TO hbide_getNextIDasString( "IdeToolButtonWidget" )

   qAction := QWidgetAction( ::oWidget )
   qAction:setDefaultWidget( qWidget )
   ::oWidget:addAction( qAction )

   ::hItems[ cName ] := qWidget

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:addToolButton( cName, cDesc, cImage, bAction, lCheckable, lDragEnabled )
   LOCAL oButton

   DEFAULT cName        TO hbide_getNextIDasString( "IdeToolButton" )
   DEFAULT cDesc        TO ""
   DEFAULT lCheckable   TO .f.
   DEFAULT lDragEnabled TO .f.

   oButton := QToolButton() // ::oWidget )
   oButton:setObjectName( cName )
   oButton:setTooltip( cDesc )
   oButton:setIcon( cImage )
   oButton:setCheckable( lCheckable )
   oButton:setFocusPolicy( Qt_NoFocus )

   IF lDragEnabled
      oButton:connect( QEvent_MouseButtonPress  , {|p| ::execEvent( "QEvent_MousePress"  , p, cName ) } )
      oButton:connect( QEvent_MouseButtonRelease, {|p| ::execEvent( "QEvent_MouseRelease", p, cName ) } )
      oButton:connect( QEvent_MouseMove         , {|p| ::execEvent( "QEvent_MouseMove"   , p, cName ) } )
      oButton:connect( QEvent_Enter             , {|p| ::execEvent( "QEvent_MouseEnter"  , p, cName ) } )
   ENDIF

   IF hb_isBlock( bAction )
      oButton:connect( "clicked()", bAction )
   ENDIF
   ::oWidget:addWidget( oButton )

   ::hItems[ cName ] := oButton
   RETURN oButton

/*----------------------------------------------------------------------*/

METHOD HbqToolbar:setItemChecked( cName, lState )
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

METHOD HbqToolbar:setItemEnabled( cName, lEnabled )
   LOCAL lOldEnabled

   IF hb_hHasKey( ::hItems, cName )
      lOldEnabled := ::hItems[ cName ]:isEnabled()
      IF hb_isLogical( lEnabled )
         ::hItems[ cName ]:setEnabled( lEnabled )
      ENDIF
   ENDIF

   RETURN lOldEnabled

/*----------------------------------------------------------------------*/
