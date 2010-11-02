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


FUNCTION QGraphicsWidget( ... )
   RETURN HB_QGraphicsWidget():new( ... )

FUNCTION QGraphicsWidgetFromPointer( ... )
   RETURN HB_QGraphicsWidget():fromPointer( ... )


CREATE CLASS QGraphicsWidget INHERIT HbQtObjectHandler, HB_QObject, HB_QGraphicsItem, HB_QGraphicsLayoutItem FUNCTION HB_QGraphicsWidget

   METHOD  new( ... )

   METHOD  actions                       // (  )                                               -> oQList_QAction
   METHOD  addAction                     // ( oQAction )                                       -> NIL
   METHOD  adjustSize                    // (  )                                               -> NIL
   METHOD  focusPolicy                   // (  )                                               -> nQt_FocusPolicy
   METHOD  focusWidget                   // (  )                                               -> oQGraphicsWidget
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  getContentsMargins            // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  getWindowFrameMargins         // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  grabShortcut                  // ( oQKeySequence, nContext )                        -> nInt
   METHOD  insertAction                  // ( oQAction, oQAction )                             -> NIL
   METHOD  isActiveWindow                // (  )                                               -> lBool
   METHOD  layout                        // (  )                                               -> oQGraphicsLayout
   METHOD  layoutDirection               // (  )                                               -> nQt_LayoutDirection
   METHOD  palette                       // (  )                                               -> oQPalette
   METHOD  rect                          // (  )                                               -> oQRectF
   METHOD  releaseShortcut               // ( nId )                                            -> NIL
   METHOD  removeAction                  // ( oQAction )                                       -> NIL
   METHOD  resize                        // ( oQSizeF )                                        -> NIL
                                         // ( nW, nH )                                         -> NIL
   METHOD  setAttribute                  // ( nAttribute, lOn )                                -> NIL
   METHOD  setContentsMargins            // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  setFocusPolicy                // ( nPolicy )                                        -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setGeometry                   // ( oQRectF )                                        -> NIL
                                         // ( nX, nY, nW, nH )                                 -> NIL
   METHOD  setLayout                     // ( oQGraphicsLayout )                               -> NIL
   METHOD  setLayoutDirection            // ( nDirection )                                     -> NIL
   METHOD  setPalette                    // ( oQPalette )                                      -> NIL
   METHOD  setShortcutAutoRepeat         // ( nId, lEnabled )                                  -> NIL
   METHOD  setShortcutEnabled            // ( nId, lEnabled )                                  -> NIL
   METHOD  setStyle                      // ( oQStyle )                                        -> NIL
   METHOD  setWindowFlags                // ( nWFlags )                                        -> NIL
   METHOD  setWindowFrameMargins         // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  setWindowTitle                // ( cTitle )                                         -> NIL
   METHOD  size                          // (  )                                               -> oQSizeF
   METHOD  style                         // (  )                                               -> oQStyle
   METHOD  testAttribute                 // ( nAttribute )                                     -> lBool
   METHOD  unsetLayoutDirection          // (  )                                               -> NIL
   METHOD  unsetWindowFrameMargins       // (  )                                               -> NIL
   METHOD  windowFlags                   // (  )                                               -> nQt_WindowFlags
   METHOD  windowFrameGeometry           // (  )                                               -> oQRectF
   METHOD  windowFrameRect               // (  )                                               -> oQRectF
   METHOD  windowTitle                   // (  )                                               -> cQString
   METHOD  windowType                    // (  )                                               -> nQt_WindowType
   METHOD  setTabOrder                   // ( oQGraphicsWidget, oQGraphicsWidget )             -> NIL
   METHOD  close                         // (  )                                               -> lBool

   ENDCLASS


METHOD QGraphicsWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsWidget( ... )
   RETURN Self


METHOD QGraphicsWidget:actions( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QGraphicsWidget_actions( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:addAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_addAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:adjustSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_adjustSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:focusPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_focusPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:focusWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsWidgetFromPointer( Qt_QGraphicsWidget_focusWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QGraphicsWidget_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:getContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsWidget_getContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:getWindowFrameMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsWidget_getWindowFrameMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:grabShortcut( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_grabShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_grabShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:insertAction( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_insertAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:isActiveWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_isActiveWindow( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:layout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsLayoutFromPointer( Qt_QGraphicsWidget_layout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:layoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_layoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:palette( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaletteFromPointer( Qt_QGraphicsWidget_palette( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsWidget_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:releaseShortcut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_releaseShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:removeAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_removeAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:resize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_resize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_resize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setAttribute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsWidget_setContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setFocusPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setFocusPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setGeometry( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsWidget_setGeometry_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setLayout( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setLayoutDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setLayoutDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setPalette( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setPalette( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setShortcutAutoRepeat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_setShortcutAutoRepeat( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setShortcutAutoRepeat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setShortcutEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_setShortcutEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setShortcutEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setWindowFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setWindowFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setWindowFrameMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QGraphicsWidget_setWindowFrameMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setWindowTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_setWindowTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QGraphicsWidget_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStyleFromPointer( Qt_QGraphicsWidget_style( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:testAttribute( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsWidget_testAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:unsetLayoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_unsetLayoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:unsetWindowFrameMargins( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_unsetWindowFrameMargins( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:windowFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_windowFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:windowFrameGeometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsWidget_windowFrameGeometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:windowFrameRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QGraphicsWidget_windowFrameRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:windowTitle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_windowTitle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:windowType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_windowType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:setTabOrder( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsWidget_setTabOrder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QGraphicsWidget:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsWidget_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

