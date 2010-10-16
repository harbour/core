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


FUNCTION QWidget( ... )
   RETURN HB_QWidget():new( ... )

FUNCTION QWidgetFrom( ... )
   RETURN HB_QWidget():from( ... )

FUNCTION QWidgetFromPointer( ... )
   RETURN HB_QWidget():fromPointer( ... )


CREATE CLASS QWidget INHERIT HbQtObjectHandler, HB_QObject, HB_QPaintDevice FUNCTION HB_QWidget

   METHOD  new( ... )

   METHOD  acceptDrops                   // (  )                                               -> lBool
   METHOD  actions                       // (  )                                               -> oQList_QAction
   METHOD  activateWindow                // (  )                                               -> NIL
   METHOD  addAction                     // ( oQAction )                                       -> NIL
   METHOD  adjustSize                    // (  )                                               -> NIL
   METHOD  autoFillBackground            // (  )                                               -> lBool
   METHOD  backgroundRole                // (  )                                               -> nQPalette_ColorRole
   METHOD  baseSize                      // (  )                                               -> oQSize
   METHOD  childAt                       // ( nX, nY )                                         -> oQWidget
                                         // ( oQPoint )                                        -> oQWidget
   METHOD  childrenRect                  // (  )                                               -> oQRect
   METHOD  childrenRegion                // (  )                                               -> oQRegion
   METHOD  clearFocus                    // (  )                                               -> NIL
   METHOD  clearMask                     // (  )                                               -> NIL
   METHOD  contentsRect                  // (  )                                               -> oQRect
   METHOD  contextMenuPolicy             // (  )                                               -> nQt_ContextMenuPolicy
   METHOD  cursor                        // (  )                                               -> oQCursor
   METHOD  ensurePolished                // (  )                                               -> NIL
   METHOD  focusPolicy                   // (  )                                               -> nQt_FocusPolicy
   METHOD  focusProxy                    // (  )                                               -> oQWidget
   METHOD  focusWidget                   // (  )                                               -> oQWidget
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  fontInfo                      // (  )                                               -> oQFontInfo
   METHOD  fontMetrics                   // (  )                                               -> oQFontMetrics
   METHOD  foregroundRole                // (  )                                               -> nQPalette_ColorRole
   METHOD  frameGeometry                 // (  )                                               -> oQRect
   METHOD  frameSize                     // (  )                                               -> oQSize
   METHOD  geometry                      // (  )                                               -> oQRect
   METHOD  getContentsMargins            // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  grabKeyboard                  // (  )                                               -> NIL
   METHOD  grabMouse                     // (  )                                               -> NIL
                                         // ( oQCursor )                                       -> NIL
   METHOD  grabShortcut                  // ( oQKeySequence, nContext )                        -> nInt
   METHOD  hasFocus                      // (  )                                               -> lBool
   METHOD  hasMouseTracking              // (  )                                               -> lBool
   METHOD  height                        // (  )                                               -> nInt
   METHOD  heightForWidth                // ( nW )                                             -> nInt
   METHOD  inputMethodQuery              // ( nQuery )                                         -> oQVariant
   METHOD  insertAction                  // ( oQAction, oQAction )                             -> NIL
   METHOD  isActiveWindow                // (  )                                               -> lBool
   METHOD  isAncestorOf                  // ( oQWidget )                                       -> lBool
   METHOD  isEnabled                     // (  )                                               -> lBool
   METHOD  isEnabledTo                   // ( oQWidget )                                       -> lBool
   METHOD  isFullScreen                  // (  )                                               -> lBool
   METHOD  isHidden                      // (  )                                               -> lBool
   METHOD  isMaximized                   // (  )                                               -> lBool
   METHOD  isMinimized                   // (  )                                               -> lBool
   METHOD  isModal                       // (  )                                               -> lBool
   METHOD  isVisible                     // (  )                                               -> lBool
   METHOD  isVisibleTo                   // ( oQWidget )                                       -> lBool
   METHOD  isWindow                      // (  )                                               -> lBool
   METHOD  isWindowModified              // (  )                                               -> lBool
   METHOD  layout                        // (  )                                               -> oQLayout
   METHOD  layoutDirection               // (  )                                               -> nQt_LayoutDirection
   METHOD  locale                        // (  )                                               -> oQLocale
   METHOD  mapFrom                       // ( oQWidget, oQPoint )                              -> oQPoint
   METHOD  mapFromGlobal                 // ( oQPoint )                                        -> oQPoint
   METHOD  mapFromParent                 // ( oQPoint )                                        -> oQPoint
   METHOD  mapTo                         // ( oQWidget, oQPoint )                              -> oQPoint
   METHOD  mapToGlobal                   // ( oQPoint )                                        -> oQPoint
   METHOD  mapToParent                   // ( oQPoint )                                        -> oQPoint
   METHOD  mask                          // (  )                                               -> oQRegion
   METHOD  maximumHeight                 // (  )                                               -> nInt
   METHOD  maximumSize                   // (  )                                               -> oQSize
   METHOD  maximumWidth                  // (  )                                               -> nInt
   METHOD  minimumHeight                 // (  )                                               -> nInt
   METHOD  minimumSize                   // (  )                                               -> oQSize
   METHOD  minimumSizeHint               // (  )                                               -> oQSize
   METHOD  minimumWidth                  // (  )                                               -> nInt
   METHOD  move                          // ( nX, nY )                                         -> NIL
                                         // ( oQPoint )                                        -> NIL
   METHOD  nativeParentWidget            // (  )                                               -> oQWidget
   METHOD  nextInFocusChain              // (  )                                               -> oQWidget
   METHOD  normalGeometry                // (  )                                               -> oQRect
   METHOD  overrideWindowFlags           // ( nFlags )                                         -> NIL
   METHOD  paintEngine                   // (  )                                               -> oQPaintEngine
   METHOD  palette                       // (  )                                               -> oQPalette
   METHOD  parentWidget                  // (  )                                               -> oQWidget
   METHOD  pos                           // (  )                                               -> oQPoint
   METHOD  rect                          // (  )                                               -> oQRect
   METHOD  releaseKeyboard               // (  )                                               -> NIL
   METHOD  releaseMouse                  // (  )                                               -> NIL
   METHOD  releaseShortcut               // ( nId )                                            -> NIL
   METHOD  removeAction                  // ( oQAction )                                       -> NIL
   METHOD  repaint                       // ( nX, nY, nW, nH )                                 -> NIL
                                         // ( oQRect )                                         -> NIL
                                         // ( oQRegion )                                       -> NIL
   METHOD  resize                        // ( nW, nH )                                         -> NIL
                                         // ( oQSize )                                         -> NIL
   METHOD  restoreGeometry               // ( oQByteArray )                                    -> lBool
   METHOD  saveGeometry                  // (  )                                               -> oQByteArray
   METHOD  scroll                        // ( nDx, nDy )                                       -> NIL
                                         // ( nDx, nDy, oQRect )                               -> NIL
   METHOD  setAcceptDrops                // ( lOn )                                            -> NIL
   METHOD  setAttribute                  // ( nAttribute, lOn )                                -> NIL
   METHOD  setAutoFillBackground         // ( lEnabled )                                       -> NIL
   METHOD  setBackgroundRole             // ( nRole )                                          -> NIL
   METHOD  setBaseSize                   // ( oQSize )                                         -> NIL
                                         // ( nBasew, nBaseh )                                 -> NIL
   METHOD  setContentsMargins            // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  setContextMenuPolicy          // ( nPolicy )                                        -> NIL
   METHOD  setCursor                     // ( oQCursor )                                       -> NIL
   METHOD  setFixedHeight                // ( nH )                                             -> NIL
   METHOD  setFixedSize                  // ( oQSize )                                         -> NIL
                                         // ( nW, nH )                                         -> NIL
   METHOD  setFixedWidth                 // ( nW )                                             -> NIL
   METHOD  setFocus                      // ( nReason )                                        -> NIL
   METHOD  setFocusPolicy                // ( nPolicy )                                        -> NIL
   METHOD  setFocusProxy                 // ( oQWidget )                                       -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setForegroundRole             // ( nRole )                                          -> NIL
   METHOD  setGeometry                   // ( oQRect )                                         -> NIL
                                         // ( nX, nY, nW, nH )                                 -> NIL
   METHOD  setLayout                     // ( oQLayout )                                       -> NIL
   METHOD  setLayoutDirection            // ( nDirection )                                     -> NIL
   METHOD  setLocale                     // ( oQLocale )                                       -> NIL
   METHOD  setMask                       // ( oQBitmap )                                       -> NIL
                                         // ( oQRegion )                                       -> NIL
   METHOD  setMaximumHeight              // ( nMaxh )                                          -> NIL
   METHOD  setMaximumSize                // ( oQSize )                                         -> NIL
                                         // ( nMaxw, nMaxh )                                   -> NIL
   METHOD  setMaximumWidth               // ( nMaxw )                                          -> NIL
   METHOD  setMinimumHeight              // ( nMinh )                                          -> NIL
   METHOD  setMinimumSize                // ( oQSize )                                         -> NIL
                                         // ( nMinw, nMinh )                                   -> NIL
   METHOD  setMinimumWidth               // ( nMinw )                                          -> NIL
   METHOD  setMouseTracking              // ( lEnable )                                        -> NIL
   METHOD  setPalette                    // ( oQPalette )                                      -> NIL
   METHOD  setParent                     // ( oQWidget )                                       -> NIL
                                         // ( oQWidget, nF )                                   -> NIL
   METHOD  setShortcutAutoRepeat         // ( nId, lEnable )                                   -> NIL
   METHOD  setShortcutEnabled            // ( nId, lEnable )                                   -> NIL
   METHOD  setSizeIncrement              // ( oQSize )                                         -> NIL
                                         // ( nW, nH )                                         -> NIL
   METHOD  setSizePolicy                 // ( oQSizePolicy )                                   -> NIL
                                         // ( nHorizontal, nVertical )                         -> NIL
   METHOD  setStatusTip                  // ( cQString )                                       -> NIL
   METHOD  setStyle                      // ( oQStyle )                                        -> NIL
   METHOD  setToolTip                    // ( cQString )                                       -> NIL
   METHOD  setUpdatesEnabled             // ( lEnable )                                        -> NIL
   METHOD  setWhatsThis                  // ( cQString )                                       -> NIL
   METHOD  setWindowFilePath             // ( cFilePath )                                      -> NIL
   METHOD  setWindowFlags                // ( nType )                                          -> NIL
   METHOD  setWindowIcon                 // ( coQIcon )                                        -> NIL
   METHOD  setWindowIconText             // ( cQString )                                       -> NIL
   METHOD  setWindowModality             // ( nWindowModality )                                -> NIL
   METHOD  setWindowOpacity              // ( nLevel )                                         -> NIL
   METHOD  setWindowRole                 // ( cRole )                                          -> NIL
   METHOD  setWindowState                // ( nWindowState )                                   -> NIL
   METHOD  size                          // (  )                                               -> oQSize
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  sizeIncrement                 // (  )                                               -> oQSize
   METHOD  sizePolicy                    // (  )                                               -> oQSizePolicy
   METHOD  stackUnder                    // ( oQWidget )                                       -> NIL
   METHOD  statusTip                     // (  )                                               -> cQString
   METHOD  style                         // (  )                                               -> oQStyle
   METHOD  styleSheet                    // (  )                                               -> cQString
   METHOD  testAttribute                 // ( nAttribute )                                     -> lBool
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  underMouse                    // (  )                                               -> lBool
   METHOD  unsetCursor                   // (  )                                               -> NIL
   METHOD  unsetLayoutDirection          // (  )                                               -> NIL
   METHOD  unsetLocale                   // (  )                                               -> NIL
   METHOD  update                        // ( nX, nY, nW, nH )                                 -> NIL
                                         // ( oQRect )                                         -> NIL
                                         // ( oQRegion )                                       -> NIL
   METHOD  updateGeometry                // (  )                                               -> NIL
   METHOD  updatesEnabled                // (  )                                               -> lBool
   METHOD  visibleRegion                 // (  )                                               -> oQRegion
   METHOD  whatsThis                     // (  )                                               -> cQString
   METHOD  width                         // (  )                                               -> nInt
   METHOD  window                        // (  )                                               -> oQWidget
   METHOD  windowFilePath                // (  )                                               -> cQString
   METHOD  windowFlags                   // (  )                                               -> nQt_WindowFlags
   METHOD  windowIcon                    // (  )                                               -> oQIcon
   METHOD  windowIconText                // (  )                                               -> cQString
   METHOD  windowModality                // (  )                                               -> nQt_WindowModality
   METHOD  windowOpacity                 // (  )                                               -> nQreal
   METHOD  windowRole                    // (  )                                               -> cQString
   METHOD  windowState                   // (  )                                               -> nQt_WindowStates
   METHOD  windowTitle                   // (  )                                               -> cQString
   METHOD  windowType                    // (  )                                               -> nQt_WindowType
   METHOD  x                             // (  )                                               -> nInt
   METHOD  y                             // (  )                                               -> nInt
   METHOD  keyboardGrabber               // (  )                                               -> oQWidget
   METHOD  mouseGrabber                  // (  )                                               -> oQWidget
   METHOD  setTabOrder                   // ( oQWidget, oQWidget )                             -> NIL
   METHOD  close                         // (  )                                               -> lBool
   METHOD  hide                          // (  )                                               -> NIL
   METHOD  lower                         // (  )                                               -> NIL
   METHOD  raise                         // (  )                                               -> NIL
                                         // (  )                                               -> NIL
   METHOD  setDisabled                   // ( lDisable )                                       -> NIL
   METHOD  setEnabled                    // ( lEnable )                                        -> NIL
                                         // (  )                                               -> NIL
   METHOD  setHidden                     // ( lHidden )                                        -> NIL
   METHOD  setStyleSheet                 // ( cStyleSheet )                                    -> NIL
   METHOD  setVisible                    // ( lVisible )                                       -> NIL
   METHOD  setWindowModified             // ( lModified )                                      -> NIL
   METHOD  setWindowTitle                // ( cTitle )                                         -> NIL
   METHOD  show                          // (  )                                               -> NIL
   METHOD  showFullScreen                // (  )                                               -> NIL
   METHOD  showMaximized                 // (  )                                               -> NIL
   METHOD  showMinimized                 // (  )                                               -> NIL
   METHOD  showNormal                    // (  )                                               -> NIL
                                         // (  )                                               -> NIL

   ENDCLASS


METHOD QWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWidget( ... )
   RETURN Self


METHOD QWidget:acceptDrops( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_acceptDrops( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:actions( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QWidget_actions( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:activateWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_activateWindow( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:addAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_addAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:adjustSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_adjustSize( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:autoFillBackground( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_autoFillBackground( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:backgroundRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_backgroundRole( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:baseSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_baseSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:childAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QWidgetFromPointer( Qt_QWidget_childAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QWidgetFromPointer( Qt_QWidget_childAt_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:childrenRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWidget_childrenRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:childrenRegion( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegionFromPointer( Qt_QWidget_childrenRegion( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:clearFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_clearFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:clearMask( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_clearMask( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:contentsRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWidget_contentsRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:contextMenuPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_contextMenuPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:cursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCursorFromPointer( Qt_QWidget_cursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:ensurePolished( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_ensurePolished( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:focusPolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_focusPolicy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:focusProxy( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_focusProxy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:focusWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_focusWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QWidget_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:fontInfo( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontInfoFromPointer( Qt_QWidget_fontInfo( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:fontMetrics( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontMetricsFromPointer( Qt_QWidget_fontMetrics( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:foregroundRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_foregroundRole( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:frameGeometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWidget_frameGeometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:frameSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_frameSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:geometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWidget_geometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:getContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QWidget_getContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:grabKeyboard( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_grabKeyboard( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:grabMouse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_grabMouse_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QWidget_grabMouse( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:grabShortcut( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_grabShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_grabShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:hasFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_hasFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:hasMouseTracking( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_hasMouseTracking( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_height( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:heightForWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_heightForWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:inputMethodQuery( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QWidget_inputMethodQuery( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:insertAction( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_insertAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isActiveWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isActiveWindow( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isAncestorOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_isAncestorOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isEnabledTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_isEnabledTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isFullScreen( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isFullScreen( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isMaximized( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isMaximized( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isMinimized( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isMinimized( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isModal( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isModal( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isVisibleTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_isVisibleTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isWindow( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isWindow( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:isWindowModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_isWindowModified( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:layout( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLayoutFromPointer( Qt_QWidget_layout( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:layoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_layoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:locale( ... )
   SWITCH PCount()
   CASE 0
      RETURN QLocaleFromPointer( Qt_QWidget_locale( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mapFrom( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QPointFromPointer( Qt_QWidget_mapFrom( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mapFromGlobal( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPointFromPointer( Qt_QWidget_mapFromGlobal( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mapFromParent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPointFromPointer( Qt_QWidget_mapFromParent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mapTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QPointFromPointer( Qt_QWidget_mapTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mapToGlobal( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPointFromPointer( Qt_QWidget_mapToGlobal( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mapToParent( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPointFromPointer( Qt_QWidget_mapToParent( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mask( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegionFromPointer( Qt_QWidget_mask( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:maximumHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_maximumHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:maximumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_maximumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:maximumWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_maximumWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:minimumHeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_minimumHeight( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:minimumSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_minimumSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:minimumSizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_minimumSizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:minimumWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_minimumWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:move( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_move( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_move_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:nativeParentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_nativeParentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:nextInFocusChain( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_nextInFocusChain( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:normalGeometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWidget_normalGeometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:overrideWindowFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_overrideWindowFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:paintEngine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaintEngineFromPointer( Qt_QWidget_paintEngine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:palette( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPaletteFromPointer( Qt_QWidget_palette( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:parentWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_parentWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:pos( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QWidget_pos( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:rect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QWidget_rect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:releaseKeyboard( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_releaseKeyboard( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:releaseMouse( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_releaseMouse( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:releaseShortcut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_releaseShortcut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:removeAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_removeAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:repaint( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QWidget_repaint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECT"
            RETURN Qt_QWidget_repaint_1( ::pPtr, ... )
         CASE "QREGION"
            RETURN Qt_QWidget_repaint_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QWidget_repaint_3( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:resize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_resize( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_resize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:restoreGeometry( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_restoreGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:saveGeometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN QByteArrayFromPointer( Qt_QWidget_saveGeometry( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:scroll( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QWidget_scroll_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_scroll( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setAcceptDrops( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setAcceptDrops( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setAttribute( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setAutoFillBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setAutoFillBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setBackgroundRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setBackgroundRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setBaseSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setBaseSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setBaseSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setContentsMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QWidget_setContentsMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setContextMenuPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setContextMenuPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setFixedHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setFixedHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setFixedSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setFixedSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setFixedSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setFixedWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setFixedWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setFocus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setFocus( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QWidget_setFocus_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setFocusPolicy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setFocusPolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setFocusProxy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setFocusProxy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setForegroundRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setForegroundRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setGeometry( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QWidget_setGeometry_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setGeometry( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setLayout( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setLayout( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setLayoutDirection( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setLayoutDirection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setLocale( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setLocale( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QBITMAP"
            RETURN Qt_QWidget_setMask( ::pPtr, ... )
         CASE "QREGION"
            RETURN Qt_QWidget_setMask_1( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMaximumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setMaximumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMaximumSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setMaximumSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setMaximumSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMaximumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setMaximumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMinimumHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setMinimumHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMinimumSize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setMinimumSize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setMinimumSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMinimumWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setMinimumWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setMouseTracking( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setMouseTracking( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setPalette( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setPalette( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setParent( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setParent_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setParent( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setShortcutAutoRepeat( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setShortcutAutoRepeat( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setShortcutAutoRepeat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setShortcutEnabled( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setShortcutEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setShortcutEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setSizeIncrement( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setSizeIncrement_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setSizeIncrement( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setSizePolicy( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setSizePolicy_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setSizePolicy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setStatusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setUpdatesEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setUpdatesEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowFilePath( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowFilePath( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QWidget_setWindowIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowIconText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowIconText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowModality( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowModality( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowOpacity( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowOpacity( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowRole( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowRole( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:sizeIncrement( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QWidget_sizeIncrement( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:sizePolicy( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizePolicyFromPointer( Qt_QWidget_sizePolicy( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:stackUnder( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_stackUnder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:statusTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_statusTip( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStyleFromPointer( Qt_QWidget_style( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:styleSheet( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_styleSheet( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:testAttribute( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_testAttribute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:underMouse( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_underMouse( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:unsetCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_unsetCursor( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:unsetLayoutDirection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_unsetLayoutDirection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:unsetLocale( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_unsetLocale( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:update( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QWidget_update( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECT"
            RETURN Qt_QWidget_update_1( ::pPtr, ... )
         CASE "QREGION"
            RETURN Qt_QWidget_update_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QWidget_update_3( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:updateGeometry( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_updateGeometry( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:updatesEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_updatesEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:visibleRegion( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRegionFromPointer( Qt_QWidget_visibleRegion( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:whatsThis( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_whatsThis( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_width( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:window( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_window( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowFilePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowFilePath( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowIcon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QWidget_windowIcon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowIconText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowIconText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowModality( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowModality( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowOpacity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowOpacity( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowRole( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowRole( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowState( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowTitle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowTitle( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:windowType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_windowType( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_x( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_y( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:keyboardGrabber( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_keyboardGrabber( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:mouseGrabber( ... )
   SWITCH PCount()
   CASE 0
      RETURN QWidgetFromPointer( Qt_QWidget_mouseGrabber( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setTabOrder( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QWidget_setTabOrder( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_close( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:hide( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_hide( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:lower( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_lower( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:raise( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_raise( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setDisabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setDisabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setHidden( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setHidden( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setStyleSheet( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setStyleSheet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowModified( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowModified( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:setWindowTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QWidget_setWindowTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:show( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_show( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:showFullScreen( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_showFullScreen( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:showMaximized( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_showMaximized( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:showMinimized( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_showMinimized( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QWidget:showNormal( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QWidget_showNormal( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()

