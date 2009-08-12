/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


CREATE CLASS QWidget INHERIT QObject, QPaintDevice

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QWidget_destroy( ::pPtr )

   METHOD  acceptDrops()                       INLINE  Qt_QWidget_acceptDrops( ::pPtr )
   METHOD  accessibleDescription()             INLINE  Qt_QWidget_accessibleDescription( ::pPtr )
   METHOD  accessibleName()                    INLINE  Qt_QWidget_accessibleName( ::pPtr )
   METHOD  activateWindow()                    INLINE  Qt_QWidget_activateWindow( ::pPtr )
   METHOD  addAction( pAction )                INLINE  Qt_QWidget_addAction( ::pPtr, pAction )
   METHOD  adjustSize()                        INLINE  Qt_QWidget_adjustSize( ::pPtr )
   METHOD  autoFillBackground()                INLINE  Qt_QWidget_autoFillBackground( ::pPtr )
   METHOD  backgroundRole()                    INLINE  Qt_QWidget_backgroundRole( ::pPtr )
   METHOD  baseSize()                          INLINE  Qt_QWidget_baseSize( ::pPtr )
   METHOD  childAt( nX, nY )                   INLINE  Qt_QWidget_childAt( ::pPtr, nX, nY )
   METHOD  childAt_1( pP )                     INLINE  Qt_QWidget_childAt_1( ::pPtr, pP )
   METHOD  childrenRect()                      INLINE  Qt_QWidget_childrenRect( ::pPtr )
   METHOD  childrenRegion()                    INLINE  Qt_QWidget_childrenRegion( ::pPtr )
   METHOD  clearFocus()                        INLINE  Qt_QWidget_clearFocus( ::pPtr )
   METHOD  clearMask()                         INLINE  Qt_QWidget_clearMask( ::pPtr )
   METHOD  contentsRect()                      INLINE  Qt_QWidget_contentsRect( ::pPtr )
   METHOD  contextMenuPolicy()                 INLINE  Qt_QWidget_contextMenuPolicy( ::pPtr )
   METHOD  cursor()                            INLINE  Qt_QWidget_cursor( ::pPtr )
   METHOD  ensurePolished()                    INLINE  Qt_QWidget_ensurePolished( ::pPtr )
   METHOD  focusPolicy()                       INLINE  Qt_QWidget_focusPolicy( ::pPtr )
   METHOD  focusProxy()                        INLINE  Qt_QWidget_focusProxy( ::pPtr )
   METHOD  focusWidget()                       INLINE  Qt_QWidget_focusWidget( ::pPtr )
   METHOD  font()                              INLINE  Qt_QWidget_font( ::pPtr )
   METHOD  fontInfo()                          INLINE  Qt_QWidget_fontInfo( ::pPtr )
   METHOD  fontMetrics()                       INLINE  Qt_QWidget_fontMetrics( ::pPtr )
   METHOD  foregroundRole()                    INLINE  Qt_QWidget_foregroundRole( ::pPtr )
   METHOD  frameGeometry()                     INLINE  Qt_QWidget_frameGeometry( ::pPtr )
   METHOD  frameSize()                         INLINE  Qt_QWidget_frameSize( ::pPtr )
   METHOD  geometry()                          INLINE  Qt_QWidget_geometry( ::pPtr )
   METHOD  getContentsMargins( nLeft, nTop, nRight, nBottom )  INLINE  Qt_QWidget_getContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )
   METHOD  grabKeyboard()                      INLINE  Qt_QWidget_grabKeyboard( ::pPtr )
   METHOD  grabMouse()                         INLINE  Qt_QWidget_grabMouse( ::pPtr )
   METHOD  grabMouse_1( pCursor )              INLINE  Qt_QWidget_grabMouse_1( ::pPtr, pCursor )
   METHOD  grabShortcut( pKey, nContext )      INLINE  Qt_QWidget_grabShortcut( ::pPtr, pKey, nContext )
   METHOD  graphicsProxyWidget()               INLINE  Qt_QWidget_graphicsProxyWidget( ::pPtr )
   METHOD  hasFocus()                          INLINE  Qt_QWidget_hasFocus( ::pPtr )
   METHOD  hasMouseTracking()                  INLINE  Qt_QWidget_hasMouseTracking( ::pPtr )
   METHOD  height()                            INLINE  Qt_QWidget_height( ::pPtr )
   METHOD  heightForWidth( nW )                INLINE  Qt_QWidget_heightForWidth( ::pPtr, nW )
   METHOD  inputContext()                      INLINE  Qt_QWidget_inputContext( ::pPtr )
   METHOD  inputMethodQuery( nQuery )          INLINE  Qt_QWidget_inputMethodQuery( ::pPtr, nQuery )
   METHOD  insertAction( pBefore, pAction )    INLINE  Qt_QWidget_insertAction( ::pPtr, pBefore, pAction )
   METHOD  isActiveWindow()                    INLINE  Qt_QWidget_isActiveWindow( ::pPtr )
   METHOD  isAncestorOf( pChild )              INLINE  Qt_QWidget_isAncestorOf( ::pPtr, pChild )
   METHOD  isEnabled()                         INLINE  Qt_QWidget_isEnabled( ::pPtr )
   METHOD  isEnabledTo( pAncestor )            INLINE  Qt_QWidget_isEnabledTo( ::pPtr, pAncestor )
   METHOD  isFullScreen()                      INLINE  Qt_QWidget_isFullScreen( ::pPtr )
   METHOD  isHidden()                          INLINE  Qt_QWidget_isHidden( ::pPtr )
   METHOD  isMaximized()                       INLINE  Qt_QWidget_isMaximized( ::pPtr )
   METHOD  isMinimized()                       INLINE  Qt_QWidget_isMinimized( ::pPtr )
   METHOD  isModal()                           INLINE  Qt_QWidget_isModal( ::pPtr )
   METHOD  isVisible()                         INLINE  Qt_QWidget_isVisible( ::pPtr )
   METHOD  isVisibleTo( pAncestor )            INLINE  Qt_QWidget_isVisibleTo( ::pPtr, pAncestor )
   METHOD  isWindow()                          INLINE  Qt_QWidget_isWindow( ::pPtr )
   METHOD  isWindowModified()                  INLINE  Qt_QWidget_isWindowModified( ::pPtr )
   METHOD  layout()                            INLINE  Qt_QWidget_layout( ::pPtr )
   METHOD  layoutDirection()                   INLINE  Qt_QWidget_layoutDirection( ::pPtr )
   METHOD  locale()                            INLINE  Qt_QWidget_locale( ::pPtr )
   METHOD  mapFrom( pParent, pPos )            INLINE  Qt_QWidget_mapFrom( ::pPtr, pParent, pPos )
   METHOD  mapFromGlobal( pPos )               INLINE  Qt_QWidget_mapFromGlobal( ::pPtr, pPos )
   METHOD  mapFromParent( pPos )               INLINE  Qt_QWidget_mapFromParent( ::pPtr, pPos )
   METHOD  mapTo( pParent, pPos )              INLINE  Qt_QWidget_mapTo( ::pPtr, pParent, pPos )
   METHOD  mapToGlobal( pPos )                 INLINE  Qt_QWidget_mapToGlobal( ::pPtr, pPos )
   METHOD  mapToParent( pPos )                 INLINE  Qt_QWidget_mapToParent( ::pPtr, pPos )
   METHOD  mask()                              INLINE  Qt_QWidget_mask( ::pPtr )
   METHOD  maximumHeight()                     INLINE  Qt_QWidget_maximumHeight( ::pPtr )
   METHOD  maximumSize()                       INLINE  Qt_QWidget_maximumSize( ::pPtr )
   METHOD  maximumWidth()                      INLINE  Qt_QWidget_maximumWidth( ::pPtr )
   METHOD  minimumHeight()                     INLINE  Qt_QWidget_minimumHeight( ::pPtr )
   METHOD  minimumSize()                       INLINE  Qt_QWidget_minimumSize( ::pPtr )
   METHOD  minimumSizeHint()                   INLINE  Qt_QWidget_minimumSizeHint( ::pPtr )
   METHOD  minimumWidth()                      INLINE  Qt_QWidget_minimumWidth( ::pPtr )
   METHOD  move( nX, nY )                      INLINE  Qt_QWidget_move( ::pPtr, nX, nY )
   METHOD  move_1( pQPoint )                   INLINE  Qt_QWidget_move_1( ::pPtr, pQPoint )
   METHOD  nativeParentWidget()                INLINE  Qt_QWidget_nativeParentWidget( ::pPtr )
   METHOD  nextInFocusChain()                  INLINE  Qt_QWidget_nextInFocusChain( ::pPtr )
   METHOD  normalGeometry()                    INLINE  Qt_QWidget_normalGeometry( ::pPtr )
   METHOD  overrideWindowFlags( nFlags )       INLINE  Qt_QWidget_overrideWindowFlags( ::pPtr, nFlags )
   METHOD  paintEngine()                       INLINE  Qt_QWidget_paintEngine( ::pPtr )
   METHOD  palette()                           INLINE  Qt_QWidget_palette( ::pPtr )
   METHOD  parentWidget()                      INLINE  Qt_QWidget_parentWidget( ::pPtr )
   METHOD  pos()                               INLINE  Qt_QWidget_pos( ::pPtr )
   METHOD  rect()                              INLINE  Qt_QWidget_rect( ::pPtr )
   METHOD  releaseKeyboard()                   INLINE  Qt_QWidget_releaseKeyboard( ::pPtr )
   METHOD  releaseMouse()                      INLINE  Qt_QWidget_releaseMouse( ::pPtr )
   METHOD  releaseShortcut( nId )              INLINE  Qt_QWidget_releaseShortcut( ::pPtr, nId )
   METHOD  removeAction( pAction )             INLINE  Qt_QWidget_removeAction( ::pPtr, pAction )
   METHOD  repaint( nX, nY, nW, nH )           INLINE  Qt_QWidget_repaint( ::pPtr, nX, nY, nW, nH )
   METHOD  repaint_1( pRect )                  INLINE  Qt_QWidget_repaint_1( ::pPtr, pRect )
   METHOD  repaint_2( pRgn )                   INLINE  Qt_QWidget_repaint_2( ::pPtr, pRgn )
   METHOD  resize( nW, nH )                    INLINE  Qt_QWidget_resize( ::pPtr, nW, nH )
   METHOD  resize_1( pQSize )                  INLINE  Qt_QWidget_resize_1( ::pPtr, pQSize )
   METHOD  restoreGeometry( pGeometry )        INLINE  Qt_QWidget_restoreGeometry( ::pPtr, pGeometry )
   METHOD  saveGeometry()                      INLINE  Qt_QWidget_saveGeometry( ::pPtr )
   METHOD  scroll( nDx, nDy )                  INLINE  Qt_QWidget_scroll( ::pPtr, nDx, nDy )
   METHOD  scroll_1( nDx, nDy, pR )            INLINE  Qt_QWidget_scroll_1( ::pPtr, nDx, nDy, pR )
   METHOD  setAcceptDrops( lOn )               INLINE  Qt_QWidget_setAcceptDrops( ::pPtr, lOn )
   METHOD  setAccessibleDescription( cDescription )  INLINE  Qt_QWidget_setAccessibleDescription( ::pPtr, cDescription )
   METHOD  setAccessibleName( cName )          INLINE  Qt_QWidget_setAccessibleName( ::pPtr, cName )
   METHOD  setAttribute( nAttribute, lOn )     INLINE  Qt_QWidget_setAttribute( ::pPtr, nAttribute, lOn )
   METHOD  setAutoFillBackground( lEnabled )   INLINE  Qt_QWidget_setAutoFillBackground( ::pPtr, lEnabled )
   METHOD  setBackgroundRole( nRole )          INLINE  Qt_QWidget_setBackgroundRole( ::pPtr, nRole )
   METHOD  setBaseSize( pQSize )               INLINE  Qt_QWidget_setBaseSize( ::pPtr, pQSize )
   METHOD  setBaseSize_1( nBasew, nBaseh )     INLINE  Qt_QWidget_setBaseSize_1( ::pPtr, nBasew, nBaseh )
   METHOD  setContentsMargins( nLeft, nTop, nRight, nBottom )  INLINE  Qt_QWidget_setContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )
   METHOD  setContextMenuPolicy( nPolicy )     INLINE  Qt_QWidget_setContextMenuPolicy( ::pPtr, nPolicy )
   METHOD  setCursor( pQCursor )               INLINE  Qt_QWidget_setCursor( ::pPtr, pQCursor )
   METHOD  setFixedHeight( nH )                INLINE  Qt_QWidget_setFixedHeight( ::pPtr, nH )
   METHOD  setFixedSize( pS )                  INLINE  Qt_QWidget_setFixedSize( ::pPtr, pS )
   METHOD  setFixedSize_1( nW, nH )            INLINE  Qt_QWidget_setFixedSize_1( ::pPtr, nW, nH )
   METHOD  setFixedWidth( nW )                 INLINE  Qt_QWidget_setFixedWidth( ::pPtr, nW )
   METHOD  setFocus( nReason )                 INLINE  Qt_QWidget_setFocus( ::pPtr, nReason )
   METHOD  setFocusPolicy( nPolicy )           INLINE  Qt_QWidget_setFocusPolicy( ::pPtr, nPolicy )
   METHOD  setFocusProxy( pW )                 INLINE  Qt_QWidget_setFocusProxy( ::pPtr, pW )
   METHOD  setFont( pQFont )                   INLINE  Qt_QWidget_setFont( ::pPtr, pQFont )
   METHOD  setForegroundRole( nRole )          INLINE  Qt_QWidget_setForegroundRole( ::pPtr, nRole )
   METHOD  setGeometry( pQRect )               INLINE  Qt_QWidget_setGeometry( ::pPtr, pQRect )
   METHOD  setGeometry_1( nX, nY, nW, nH )     INLINE  Qt_QWidget_setGeometry_1( ::pPtr, nX, nY, nW, nH )
   METHOD  setInputContext( pContext )         INLINE  Qt_QWidget_setInputContext( ::pPtr, pContext )
   METHOD  setLayout( pLayout )                INLINE  Qt_QWidget_setLayout( ::pPtr, pLayout )
   METHOD  setLayoutDirection( nDirection )    INLINE  Qt_QWidget_setLayoutDirection( ::pPtr, nDirection )
   METHOD  setLocale( pLocale )                INLINE  Qt_QWidget_setLocale( ::pPtr, pLocale )
   METHOD  setMask( pBitmap )                  INLINE  Qt_QWidget_setMask( ::pPtr, pBitmap )
   METHOD  setMask_1( pRegion )                INLINE  Qt_QWidget_setMask_1( ::pPtr, pRegion )
   METHOD  setMaximumHeight( nMaxh )           INLINE  Qt_QWidget_setMaximumHeight( ::pPtr, nMaxh )
   METHOD  setMaximumSize( pQSize )            INLINE  Qt_QWidget_setMaximumSize( ::pPtr, pQSize )
   METHOD  setMaximumSize_1( nMaxw, nMaxh )    INLINE  Qt_QWidget_setMaximumSize_1( ::pPtr, nMaxw, nMaxh )
   METHOD  setMaximumWidth( nMaxw )            INLINE  Qt_QWidget_setMaximumWidth( ::pPtr, nMaxw )
   METHOD  setMinimumHeight( nMinh )           INLINE  Qt_QWidget_setMinimumHeight( ::pPtr, nMinh )
   METHOD  setMinimumSize( pQSize )            INLINE  Qt_QWidget_setMinimumSize( ::pPtr, pQSize )
   METHOD  setMinimumSize_1( nMinw, nMinh )    INLINE  Qt_QWidget_setMinimumSize_1( ::pPtr, nMinw, nMinh )
   METHOD  setMinimumWidth( nMinw )            INLINE  Qt_QWidget_setMinimumWidth( ::pPtr, nMinw )
   METHOD  setMouseTracking( lEnable )         INLINE  Qt_QWidget_setMouseTracking( ::pPtr, lEnable )
   METHOD  setPalette( pQPalette )             INLINE  Qt_QWidget_setPalette( ::pPtr, pQPalette )
   METHOD  setParent( pParent )                INLINE  Qt_QWidget_setParent( ::pPtr, pParent )
   METHOD  setParent_1( pParent, nF )          INLINE  Qt_QWidget_setParent_1( ::pPtr, pParent, nF )
   METHOD  setShortcutAutoRepeat( nId, lEnable )  INLINE  Qt_QWidget_setShortcutAutoRepeat( ::pPtr, nId, lEnable )
   METHOD  setShortcutEnabled( nId, lEnable )  INLINE  Qt_QWidget_setShortcutEnabled( ::pPtr, nId, lEnable )
   METHOD  setSizeIncrement( pQSize )          INLINE  Qt_QWidget_setSizeIncrement( ::pPtr, pQSize )
   METHOD  setSizeIncrement_1( nW, nH )        INLINE  Qt_QWidget_setSizeIncrement_1( ::pPtr, nW, nH )
   METHOD  setSizePolicy( nHorizontal, nVertical )  INLINE  Qt_QWidget_setSizePolicy( ::pPtr, nHorizontal, nVertical )
   METHOD  setStatusTip( cQString )            INLINE  Qt_QWidget_setStatusTip( ::pPtr, cQString )
   METHOD  setStyle( pStyle )                  INLINE  Qt_QWidget_setStyle( ::pPtr, pStyle )
   METHOD  setToolTip( cQString )              INLINE  Qt_QWidget_setToolTip( ::pPtr, cQString )
   METHOD  setUpdatesEnabled( lEnable )        INLINE  Qt_QWidget_setUpdatesEnabled( ::pPtr, lEnable )
   METHOD  setWhatsThis( cQString )            INLINE  Qt_QWidget_setWhatsThis( ::pPtr, cQString )
   METHOD  setWindowFilePath( cFilePath )      INLINE  Qt_QWidget_setWindowFilePath( ::pPtr, cFilePath )
   METHOD  setWindowFlags( nType )             INLINE  Qt_QWidget_setWindowFlags( ::pPtr, nType )
   METHOD  setWindowIcon( cIcon )              INLINE  Qt_QWidget_setWindowIcon( ::pPtr, cIcon )
   METHOD  setWindowIconText( cQString )       INLINE  Qt_QWidget_setWindowIconText( ::pPtr, cQString )
   METHOD  setWindowModality( nWindowModality )  INLINE  Qt_QWidget_setWindowModality( ::pPtr, nWindowModality )
   METHOD  setWindowOpacity( nLevel )          INLINE  Qt_QWidget_setWindowOpacity( ::pPtr, nLevel )
   METHOD  setWindowRole( cRole )              INLINE  Qt_QWidget_setWindowRole( ::pPtr, cRole )
   METHOD  setWindowState( nWindowState )      INLINE  Qt_QWidget_setWindowState( ::pPtr, nWindowState )
   METHOD  setWindowSurface( pSurface )        INLINE  Qt_QWidget_setWindowSurface( ::pPtr, pSurface )
   METHOD  size()                              INLINE  Qt_QWidget_size( ::pPtr )
   METHOD  sizeHint()                          INLINE  Qt_QWidget_sizeHint( ::pPtr )
   METHOD  sizeIncrement()                     INLINE  Qt_QWidget_sizeIncrement( ::pPtr )
   METHOD  sizePolicy()                        INLINE  Qt_QWidget_sizePolicy( ::pPtr )
   METHOD  stackUnder( pW )                    INLINE  Qt_QWidget_stackUnder( ::pPtr, pW )
   METHOD  statusTip()                         INLINE  Qt_QWidget_statusTip( ::pPtr )
   METHOD  style()                             INLINE  Qt_QWidget_style( ::pPtr )
   METHOD  styleSheet()                        INLINE  Qt_QWidget_styleSheet( ::pPtr )
   METHOD  testAttribute( nAttribute )         INLINE  Qt_QWidget_testAttribute( ::pPtr, nAttribute )
   METHOD  toolTip()                           INLINE  Qt_QWidget_toolTip( ::pPtr )
   METHOD  underMouse()                        INLINE  Qt_QWidget_underMouse( ::pPtr )
   METHOD  unsetCursor()                       INLINE  Qt_QWidget_unsetCursor( ::pPtr )
   METHOD  unsetLayoutDirection()              INLINE  Qt_QWidget_unsetLayoutDirection( ::pPtr )
   METHOD  unsetLocale()                       INLINE  Qt_QWidget_unsetLocale( ::pPtr )
   METHOD  update( nX, nY, nW, nH )            INLINE  Qt_QWidget_update( ::pPtr, nX, nY, nW, nH )
   METHOD  update_1( pRect )                   INLINE  Qt_QWidget_update_1( ::pPtr, pRect )
   METHOD  update_2( pRgn )                    INLINE  Qt_QWidget_update_2( ::pPtr, pRgn )
   METHOD  updateGeometry()                    INLINE  Qt_QWidget_updateGeometry( ::pPtr )
   METHOD  updatesEnabled()                    INLINE  Qt_QWidget_updatesEnabled( ::pPtr )
   METHOD  visibleRegion()                     INLINE  Qt_QWidget_visibleRegion( ::pPtr )
   METHOD  whatsThis()                         INLINE  Qt_QWidget_whatsThis( ::pPtr )
   METHOD  width()                             INLINE  Qt_QWidget_width( ::pPtr )
   METHOD  winId()                             INLINE  Qt_QWidget_winId( ::pPtr )
   METHOD  window()                            INLINE  Qt_QWidget_window( ::pPtr )
   METHOD  windowFilePath()                    INLINE  Qt_QWidget_windowFilePath( ::pPtr )
   METHOD  windowFlags()                       INLINE  Qt_QWidget_windowFlags( ::pPtr )
   METHOD  windowIcon()                        INLINE  Qt_QWidget_windowIcon( ::pPtr )
   METHOD  windowIconText()                    INLINE  Qt_QWidget_windowIconText( ::pPtr )
   METHOD  windowModality()                    INLINE  Qt_QWidget_windowModality( ::pPtr )
   METHOD  windowOpacity()                     INLINE  Qt_QWidget_windowOpacity( ::pPtr )
   METHOD  windowRole()                        INLINE  Qt_QWidget_windowRole( ::pPtr )
   METHOD  windowState()                       INLINE  Qt_QWidget_windowState( ::pPtr )
   METHOD  windowSurface()                     INLINE  Qt_QWidget_windowSurface( ::pPtr )
   METHOD  windowTitle()                       INLINE  Qt_QWidget_windowTitle( ::pPtr )
   METHOD  windowType()                        INLINE  Qt_QWidget_windowType( ::pPtr )
   METHOD  x()                                 INLINE  Qt_QWidget_x( ::pPtr )
   METHOD  y()                                 INLINE  Qt_QWidget_y( ::pPtr )
   METHOD  keyboardGrabber()                   INLINE  Qt_QWidget_keyboardGrabber( ::pPtr )
   METHOD  mouseGrabber()                      INLINE  Qt_QWidget_mouseGrabber( ::pPtr )
   METHOD  setTabOrder( pFirst, pSecond )      INLINE  Qt_QWidget_setTabOrder( ::pPtr, pFirst, pSecond )
   METHOD  close()                             INLINE  Qt_QWidget_close( ::pPtr )
   METHOD  hide()                              INLINE  Qt_QWidget_hide( ::pPtr )
   METHOD  lower()                             INLINE  Qt_QWidget_lower( ::pPtr )
   METHOD  raise()                             INLINE  Qt_QWidget_raise( ::pPtr )
   METHOD  repaint_3()                         INLINE  Qt_QWidget_repaint_3( ::pPtr )
   METHOD  setDisabled( lDisable )             INLINE  Qt_QWidget_setDisabled( ::pPtr, lDisable )
   METHOD  setEnabled( lEnable )               INLINE  Qt_QWidget_setEnabled( ::pPtr, lEnable )
   METHOD  setFocus_1()                        INLINE  Qt_QWidget_setFocus_1( ::pPtr )
   METHOD  setHidden( lHidden )                INLINE  Qt_QWidget_setHidden( ::pPtr, lHidden )
   METHOD  setStyleSheet( cStyleSheet )        INLINE  Qt_QWidget_setStyleSheet( ::pPtr, cStyleSheet )
   METHOD  setVisible( lVisible )              INLINE  Qt_QWidget_setVisible( ::pPtr, lVisible )
   METHOD  setWindowModified( lModified )      INLINE  Qt_QWidget_setWindowModified( ::pPtr, lModified )
   METHOD  setWindowTitle( cTitle )            INLINE  Qt_QWidget_setWindowTitle( ::pPtr, cTitle )
   METHOD  show()                              INLINE  Qt_QWidget_show( ::pPtr )
   METHOD  showFullScreen()                    INLINE  Qt_QWidget_showFullScreen( ::pPtr )
   METHOD  showMaximized()                     INLINE  Qt_QWidget_showMaximized( ::pPtr )
   METHOD  showMinimized()                     INLINE  Qt_QWidget_showMinimized( ::pPtr )
   METHOD  showNormal()                        INLINE  Qt_QWidget_showNormal( ::pPtr )
   METHOD  update_3()                          INLINE  Qt_QWidget_update_3( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QWidget

   ::pParent := pParent

   ::pPtr := Qt_QWidget( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QWidget

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
