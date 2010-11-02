/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QApplication( ... )
   RETURN HB_QApplication():new( ... )

FUNCTION QApplicationFromPointer( ... )
   RETURN HB_QApplication():fromPointer( ... )


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
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
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
   RETURN __hbqt_error()


METHOD QApplication:isSessionRestored( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_isSessionRestored( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:saveState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_saveState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:sessionId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_sessionId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:sessionKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_sessionKey( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:styleSheet( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_styleSheet( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:activeModalWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QApplication_activeModalWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:activePopupWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QApplication_activePopupWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:activeWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QApplication_activeWindow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QApplication:beep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_beep( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:changeOverrideCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_changeOverrideCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:clipboard( ... )
   SWITCH PCount()
   CASE 0
      RETURN QClipboardFromPointer( Qt_QApplication_clipboard( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:colorSpec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_colorSpec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:cursorFlashTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_cursorFlashTime( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:desktop( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDesktopWidgetFromPointer( Qt_QApplication_desktop( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:desktopSettingsAware( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_desktopSettingsAware( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:doubleClickInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_doubleClickInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:exec( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_exec( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:focusWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QApplication_focusWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:font( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QApplication_font_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QApplication_font_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QFontFromPointer( Qt_QApplication_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:fontMetrics( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontMetricsFromPointer( Qt_QApplication_fontMetrics( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:globalStrut( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QApplication_globalStrut( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:isEffectEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_isEffectEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:isLeftToRight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_isLeftToRight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:isRightToLeft( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_isRightToLeft( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:keyboardInputDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_keyboardInputDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:keyboardInputInterval( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_keyboardInputInterval( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:keyboardInputLocale( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLocaleFromPointer( Qt_QApplication_keyboardInputLocale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:keyboardModifiers( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_keyboardModifiers( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:layoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_layoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:mouseButtons( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_mouseButtons( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:overrideCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCursorFromPointer( Qt_QApplication_overrideCursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:palette( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QPaletteFromPointer( Qt_QApplication_palette_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPaletteFromPointer( Qt_QApplication_palette_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QPaletteFromPointer( Qt_QApplication_palette( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:quitOnLastWindowClosed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_quitOnLastWindowClosed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:restoreOverrideCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_restoreOverrideCursor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setActiveWindow( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setActiveWindow( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setColorSpec( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setColorSpec( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setCursorFlashTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setCursorFlashTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setDesktopSettingsAware( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setDesktopSettingsAware( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setDoubleClickInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setDoubleClickInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QApplication:setGlobalStrut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setGlobalStrut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setGraphicsSystem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setGraphicsSystem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setKeyboardInputInterval( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setKeyboardInputInterval( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setLayoutDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setLayoutDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setOverrideCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setOverrideCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QApplication:setQuitOnLastWindowClosed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setQuitOnLastWindowClosed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setStartDragDistance( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStartDragDistance( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setStartDragTime( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStartDragTime( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStyleFromPointer( Qt_QApplication_setStyle_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setWheelScrollLines( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setWheelScrollLines( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setWindowIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QApplication_setWindowIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:startDragDistance( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_startDragDistance( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:startDragTime( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_startDragTime( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStyleFromPointer( Qt_QApplication_style( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:syncX( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_syncX( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:topLevelAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QApplication_topLevelAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QApplication_topLevelAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:wheelScrollLines( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_wheelScrollLines( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:widgetAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QApplication_widgetAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QApplication_widgetAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:windowIcon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QApplication_windowIcon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:aboutQt( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_aboutQt( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:closeAllWindows( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QApplication_closeAllWindows( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QApplication:setStyleSheet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QApplication_setStyleSheet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

