/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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


#include "hbclass.ch"


CREATE CLASS QApplication INHERIT QCoreApplication

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  commitData( pManager )              INLINE  Qt_QApplication_commitData( ::pPtr, pManager )
   METHOD  inputContext()                      INLINE  Qt_QApplication_inputContext( ::pPtr )
   METHOD  isSessionRestored()                 INLINE  Qt_QApplication_isSessionRestored( ::pPtr )
   METHOD  saveState( pManager )               INLINE  Qt_QApplication_saveState( ::pPtr, pManager )
   METHOD  sessionId()                         INLINE  Qt_QApplication_sessionId( ::pPtr )
   METHOD  sessionKey()                        INLINE  Qt_QApplication_sessionKey( ::pPtr )
   METHOD  setInputContext( pInputContext )    INLINE  Qt_QApplication_setInputContext( ::pPtr, pInputContext )
   METHOD  styleSheet()                        INLINE  Qt_QApplication_styleSheet( ::pPtr )
   METHOD  activeModalWidget()                 INLINE  Qt_QApplication_activeModalWidget( ::pPtr )
   METHOD  activePopupWidget()                 INLINE  Qt_QApplication_activePopupWidget( ::pPtr )
   METHOD  activeWindow()                      INLINE  Qt_QApplication_activeWindow( ::pPtr )
   METHOD  alert( pWidget, nMsec )             INLINE  Qt_QApplication_alert( ::pPtr, pWidget, nMsec )
   METHOD  allWidgets()                        INLINE  Qt_QApplication_allWidgets( ::pPtr )
   METHOD  beep()                              INLINE  Qt_QApplication_beep( ::pPtr )
   METHOD  changeOverrideCursor( pCursor )     INLINE  Qt_QApplication_changeOverrideCursor( ::pPtr, pCursor )
   METHOD  clipboard()                         INLINE  Qt_QApplication_clipboard( ::pPtr )
   METHOD  colorSpec()                         INLINE  Qt_QApplication_colorSpec( ::pPtr )
   METHOD  cursorFlashTime()                   INLINE  Qt_QApplication_cursorFlashTime( ::pPtr )
   METHOD  desktop()                           INLINE  Qt_QApplication_desktop( ::pPtr )
   METHOD  desktopSettingsAware()              INLINE  Qt_QApplication_desktopSettingsAware( ::pPtr )
   METHOD  doubleClickInterval()               INLINE  Qt_QApplication_doubleClickInterval( ::pPtr )
   METHOD  exec()                              INLINE  Qt_QApplication_exec( ::pPtr )
   METHOD  focusWidget()                       INLINE  Qt_QApplication_focusWidget( ::pPtr )
   METHOD  font()                              INLINE  Qt_QApplication_font( ::pPtr )
   METHOD  font_1( pWidget )                   INLINE  Qt_QApplication_font_1( ::pPtr, pWidget )
   METHOD  font_2( pClassName )                INLINE  Qt_QApplication_font_2( ::pPtr, pClassName )
   METHOD  fontMetrics()                       INLINE  Qt_QApplication_fontMetrics( ::pPtr )
   METHOD  globalStrut()                       INLINE  Qt_QApplication_globalStrut( ::pPtr )
   METHOD  isEffectEnabled( nEffect )          INLINE  Qt_QApplication_isEffectEnabled( ::pPtr, nEffect )
   METHOD  isLeftToRight()                     INLINE  Qt_QApplication_isLeftToRight( ::pPtr )
   METHOD  isRightToLeft()                     INLINE  Qt_QApplication_isRightToLeft( ::pPtr )
   METHOD  keyboardInputDirection()            INLINE  Qt_QApplication_keyboardInputDirection( ::pPtr )
   METHOD  keyboardInputInterval()             INLINE  Qt_QApplication_keyboardInputInterval( ::pPtr )
   METHOD  keyboardInputLocale()               INLINE  Qt_QApplication_keyboardInputLocale( ::pPtr )
   METHOD  keyboardModifiers()                 INLINE  Qt_QApplication_keyboardModifiers( ::pPtr )
   METHOD  layoutDirection()                   INLINE  Qt_QApplication_layoutDirection( ::pPtr )
   METHOD  mouseButtons()                      INLINE  Qt_QApplication_mouseButtons( ::pPtr )
   METHOD  overrideCursor()                    INLINE  Qt_QApplication_overrideCursor( ::pPtr )
   METHOD  palette()                           INLINE  Qt_QApplication_palette( ::pPtr )
   METHOD  palette_1( pWidget )                INLINE  Qt_QApplication_palette_1( ::pPtr, pWidget )
   METHOD  palette_2( pClassName )             INLINE  Qt_QApplication_palette_2( ::pPtr, pClassName )
   METHOD  quitOnLastWindowClosed()            INLINE  Qt_QApplication_quitOnLastWindowClosed( ::pPtr )
   METHOD  restoreOverrideCursor()             INLINE  Qt_QApplication_restoreOverrideCursor( ::pPtr )
   METHOD  setActiveWindow( pActive )          INLINE  Qt_QApplication_setActiveWindow( ::pPtr, pActive )
   METHOD  setColorSpec( nSpec )               INLINE  Qt_QApplication_setColorSpec( ::pPtr, nSpec )
   METHOD  setCursorFlashTime( nInt )          INLINE  Qt_QApplication_setCursorFlashTime( ::pPtr, nInt )
   METHOD  setDesktopSettingsAware( lOn )      INLINE  Qt_QApplication_setDesktopSettingsAware( ::pPtr, lOn )
   METHOD  setDoubleClickInterval( nInt )      INLINE  Qt_QApplication_setDoubleClickInterval( ::pPtr, nInt )
   METHOD  setEffectEnabled( nEffect, lEnable )  INLINE  Qt_QApplication_setEffectEnabled( ::pPtr, nEffect, lEnable )
   METHOD  setFont( pFont, pClassName )        INLINE  Qt_QApplication_setFont( ::pPtr, pFont, pClassName )
   METHOD  setGlobalStrut( pQSize )            INLINE  Qt_QApplication_setGlobalStrut( ::pPtr, pQSize )
   METHOD  setGraphicsSystem( cSystem )        INLINE  Qt_QApplication_setGraphicsSystem( ::pPtr, cSystem )
   METHOD  setKeyboardInputInterval( nInt )    INLINE  Qt_QApplication_setKeyboardInputInterval( ::pPtr, nInt )
   METHOD  setLayoutDirection( nDirection )    INLINE  Qt_QApplication_setLayoutDirection( ::pPtr, nDirection )
   METHOD  setOverrideCursor( pCursor )        INLINE  Qt_QApplication_setOverrideCursor( ::pPtr, pCursor )
   METHOD  setPalette( pPalette, pClassName )  INLINE  Qt_QApplication_setPalette( ::pPtr, pPalette, pClassName )
   METHOD  setQuitOnLastWindowClosed( lQuit )  INLINE  Qt_QApplication_setQuitOnLastWindowClosed( ::pPtr, lQuit )
   METHOD  setStartDragDistance( nL )          INLINE  Qt_QApplication_setStartDragDistance( ::pPtr, nL )
   METHOD  setStartDragTime( nMs )             INLINE  Qt_QApplication_setStartDragTime( ::pPtr, nMs )
   METHOD  setStyle( pStyle )                  INLINE  Qt_QApplication_setStyle( ::pPtr, pStyle )
   METHOD  setStyle_1( cStyle )                INLINE  Qt_QApplication_setStyle_1( ::pPtr, cStyle )
   METHOD  setWheelScrollLines( nInt )         INLINE  Qt_QApplication_setWheelScrollLines( ::pPtr, nInt )
   METHOD  setWindowIcon( cIcon )              INLINE  Qt_QApplication_setWindowIcon( ::pPtr, cIcon )
   METHOD  startDragDistance()                 INLINE  Qt_QApplication_startDragDistance( ::pPtr )
   METHOD  startDragTime()                     INLINE  Qt_QApplication_startDragTime( ::pPtr )
   METHOD  style()                             INLINE  Qt_QApplication_style( ::pPtr )
   METHOD  syncX()                             INLINE  Qt_QApplication_syncX( ::pPtr )
   METHOD  topLevelAt( pPoint )                INLINE  Qt_QApplication_topLevelAt( ::pPtr, pPoint )
   METHOD  topLevelAt_1( nX, nY )              INLINE  Qt_QApplication_topLevelAt_1( ::pPtr, nX, nY )
   METHOD  topLevelWidgets()                   INLINE  Qt_QApplication_topLevelWidgets( ::pPtr )
   METHOD  type()                              INLINE  Qt_QApplication_type( ::pPtr )
   METHOD  wheelScrollLines()                  INLINE  Qt_QApplication_wheelScrollLines( ::pPtr )
   METHOD  widgetAt( pPoint )                  INLINE  Qt_QApplication_widgetAt( ::pPtr, pPoint )
   METHOD  widgetAt_1( nX, nY )                INLINE  Qt_QApplication_widgetAt_1( ::pPtr, nX, nY )
   METHOD  windowIcon()                        INLINE  Qt_QApplication_windowIcon( ::pPtr )
   METHOD  aboutQt()                           INLINE  Qt_QApplication_aboutQt( ::pPtr )
   METHOD  closeAllWindows()                   INLINE  Qt_QApplication_closeAllWindows( ::pPtr )
   METHOD  setStyleSheet( cSheet )             INLINE  Qt_QApplication_setStyleSheet( ::pPtr, cSheet )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QApplication

   ::pParent := pParent

   ::pPtr := Qt_QApplication( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

