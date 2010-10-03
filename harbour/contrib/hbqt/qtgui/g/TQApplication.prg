/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QApplication( ... )
   RETURN HB_QApplication():new( ... )


CREATE CLASS QApplication INHERIT HbQtObjectHandler, HB_QCoreApplication FUNCTION HB_QApplication

   METHOD  new( ... )

   METHOD  commitData                    // ( oQSessionManager )                               -> NIL
   METHOD  isSessionRestored             // (  )                                               -> lBool
   METHOD  saveState                     // ( oQSessionManager )                               -> NIL
   METHOD  sessionId                     // (  )                                               -> cQString
   METHOD  sessionKey                    // (  )                                               -> cQString
   METHOD  styleSheet                    // (  )                                               -> cQString
   METHOD  activeModalWidget             // (  )                                               -> oQWidget
   METHOD  activePopupWidget             // (  )                                               -> oQWidget
   METHOD  activeWindow                  // (  )                                               -> oQWidget
   METHOD  alert                         // ( oQWidget, nMsec )                                -> NIL
   METHOD  beep                          // (  )                                               -> NIL
   METHOD  changeOverrideCursor          // ( oQCursor )                                       -> NIL
   METHOD  clipboard                     // (  )                                               -> oQClipboard
   METHOD  colorSpec                     // (  )                                               -> nInt
   METHOD  cursorFlashTime               // (  )                                               -> nInt
   METHOD  desktop                       // (  )                                               -> oQDesktopWidget
   METHOD  desktopSettingsAware          // (  )                                               -> lBool
   METHOD  doubleClickInterval           // (  )                                               -> nInt
   METHOD  exec                          // (  )                                               -> nInt
   METHOD  focusWidget                   // (  )                                               -> oQWidget
   METHOD  font                          // (  )                                               -> oQFont
                                         // ( oQWidget )                                       -> oQFont
                                         // ( cClassName )                                     -> oQFont
   METHOD  fontMetrics                   // (  )                                               -> oQFontMetrics
   METHOD  globalStrut                   // (  )                                               -> oQSize
   METHOD  isEffectEnabled               // ( nEffect )                                        -> lBool
   METHOD  isLeftToRight                 // (  )                                               -> lBool
   METHOD  isRightToLeft                 // (  )                                               -> lBool
   METHOD  keyboardInputDirection        // (  )                                               -> nQt_LayoutDirection
   METHOD  keyboardInputInterval         // (  )                                               -> nInt
   METHOD  keyboardInputLocale           // (  )                                               -> oQLocale
   METHOD  keyboardModifiers             // (  )                                               -> nQt_KeyboardModifiers
   METHOD  layoutDirection               // (  )                                               -> nQt_LayoutDirection
   METHOD  mouseButtons                  // (  )                                               -> nQt_MouseButtons
   METHOD  overrideCursor                // (  )                                               -> oQCursor
   METHOD  palette                       // (  )                                               -> oQPalette
                                         // ( oQWidget )                                       -> oQPalette
                                         // ( cClassName )                                     -> oQPalette
   METHOD  quitOnLastWindowClosed        // (  )                                               -> lBool
   METHOD  restoreOverrideCursor         // (  )                                               -> NIL
   METHOD  setActiveWindow               // ( oQWidget )                                       -> NIL
   METHOD  setColorSpec                  // ( nSpec )                                          -> NIL
   METHOD  setCursorFlashTime            // ( nInt )                                           -> NIL
   METHOD  setDesktopSettingsAware       // ( lOn )                                            -> NIL
   METHOD  setDoubleClickInterval        // ( nInt )                                           -> NIL
   METHOD  setEffectEnabled              // ( nEffect, lEnable )                               -> NIL
   METHOD  setFont                       // ( oQFont, cClassName )                             -> NIL
   METHOD  setGlobalStrut                // ( oQSize )                                         -> NIL
   METHOD  setGraphicsSystem             // ( cSystem )                                        -> NIL
   METHOD  setKeyboardInputInterval      // ( nInt )                                           -> NIL
   METHOD  setLayoutDirection            // ( nDirection )                                     -> NIL
   METHOD  setOverrideCursor             // ( oQCursor )                                       -> NIL
   METHOD  setPalette                    // ( oQPalette, cClassName )                          -> NIL
   METHOD  setQuitOnLastWindowClosed     // ( lQuit )                                          -> NIL
   METHOD  setStartDragDistance          // ( nL )                                             -> NIL
   METHOD  setStartDragTime              // ( nMs )                                            -> NIL
   METHOD  setStyle                      // ( oQStyle )                                        -> NIL
                                         // ( cStyle )                                         -> oQStyle
   METHOD  setWheelScrollLines           // ( nInt )                                           -> NIL
   METHOD  setWindowIcon                 // ( coQIcon )                                        -> NIL
   METHOD  startDragDistance             // (  )                                               -> nInt
   METHOD  startDragTime                 // (  )                                               -> nInt
   METHOD  style                         // (  )                                               -> oQStyle
   METHOD  syncX                         // (  )                                               -> NIL
   METHOD  topLevelAt                    // ( oQPoint )                                        -> oQWidget
                                         // ( nX, nY )                                         -> oQWidget
   METHOD  type                          // (  )                                               -> nType
   METHOD  wheelScrollLines              // (  )                                               -> nInt
   METHOD  widgetAt                      // ( oQPoint )                                        -> oQWidget
                                         // ( nX, nY )                                         -> oQWidget
   METHOD  windowIcon                    // (  )                                               -> oQIcon
   METHOD  aboutQt                       // (  )                                               -> NIL
   METHOD  closeAllWindows               // (  )                                               -> NIL
   METHOD  setStyleSheet                 // ( cSheet )                                         -> NIL

   ENDCLASS


METHOD QApplication:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QApplication( ... )
   RETURN Self


METHOD QApplication:commitData( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_commitData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:isSessionRestored( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_isSessionRestored( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:saveState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_saveState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:sessionId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_sessionId( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:sessionKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_sessionKey( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:styleSheet( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_styleSheet( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:activeModalWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QWidget():from( Qt_QApplication_activeModalWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:activePopupWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QWidget():from( Qt_QApplication_activePopupWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:activeWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QWidget():from( Qt_QApplication_activeWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:alert( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QApplication_alert( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_alert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:beep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_beep( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:changeOverrideCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_changeOverrideCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:clipboard( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QClipboard():from( Qt_QApplication_clipboard( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:colorSpec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_colorSpec( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:cursorFlashTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_cursorFlashTime( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:desktop( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QDesktopWidget():from( Qt_QApplication_desktop( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:desktopSettingsAware( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_desktopSettingsAware( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:doubleClickInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_doubleClickInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:focusWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QWidget():from( Qt_QApplication_focusWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:font( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QFont():from( Qt_QApplication_font_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QFont():from( Qt_QApplication_font_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QFont():from( Qt_QApplication_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:fontMetrics( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QFontMetrics():from( Qt_QApplication_fontMetrics( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:globalStrut( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QSize():from( Qt_QApplication_globalStrut( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:isEffectEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_isEffectEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:isLeftToRight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_isLeftToRight( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:isRightToLeft( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_isRightToLeft( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:keyboardInputDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_keyboardInputDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:keyboardInputInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_keyboardInputInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:keyboardInputLocale( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QLocale():from( Qt_QApplication_keyboardInputLocale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:keyboardModifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_keyboardModifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:layoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_layoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:mouseButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_mouseButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:overrideCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QCursor():from( Qt_QApplication_overrideCursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:palette( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QPalette():from( Qt_QApplication_palette_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QPalette():from( Qt_QApplication_palette_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN HB_QPalette():from( Qt_QApplication_palette( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:quitOnLastWindowClosed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_quitOnLastWindowClosed( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:restoreOverrideCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_restoreOverrideCursor( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setActiveWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setActiveWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setColorSpec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setColorSpec( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setCursorFlashTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setCursorFlashTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setDesktopSettingsAware( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setDesktopSettingsAware( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setDoubleClickInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setDoubleClickInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setEffectEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QApplication_setEffectEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setEffectEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setFont( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QApplication_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setGlobalStrut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setGlobalStrut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setGraphicsSystem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setGraphicsSystem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setKeyboardInputInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setKeyboardInputInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setLayoutDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setLayoutDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setOverrideCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setOverrideCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setPalette( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QApplication_setPalette( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setPalette( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setQuitOnLastWindowClosed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setQuitOnLastWindowClosed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setStartDragDistance( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStartDragDistance( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setStartDragTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStartDragTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QStyle():from( Qt_QApplication_setStyle_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setWheelScrollLines( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setWheelScrollLines( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setWindowIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QApplication_setWindowIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:startDragDistance( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_startDragDistance( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:startDragTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_startDragTime( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QStyle():from( Qt_QApplication_style( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:syncX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_syncX( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:topLevelAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QWidget():from( Qt_QApplication_topLevelAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QWidget():from( Qt_QApplication_topLevelAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_type( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:wheelScrollLines( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_wheelScrollLines( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:widgetAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QWidget():from( Qt_QApplication_widgetAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QWidget():from( Qt_QApplication_widgetAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:windowIcon( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QIcon():from( Qt_QApplication_windowIcon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:aboutQt( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_aboutQt( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:closeAllWindows( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_closeAllWindows( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QApplication:setStyleSheet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStyleSheet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()

