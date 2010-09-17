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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QWidget( ... )
   RETURN HB_QWidget():new( ... )


CREATE CLASS QWidget INHERIT HbQtObjectHandler, HB_QObject, HB_QPaintDevice FUNCTION HB_QWidget

   METHOD  new( ... )

   METHOD  acceptDrops()
   METHOD  actions()
   METHOD  activateWindow()
   METHOD  addAction( pAction )
   METHOD  adjustSize()
   METHOD  autoFillBackground()
   METHOD  backgroundRole()
   METHOD  baseSize()
   METHOD  childAt( nX, nY )
   METHOD  childAt_1( pP )
   METHOD  childrenRect()
   METHOD  childrenRegion()
   METHOD  clearFocus()
   METHOD  clearMask()
   METHOD  contentsRect()
   METHOD  contextMenuPolicy()
   METHOD  cursor()
   METHOD  ensurePolished()
   METHOD  focusPolicy()
   METHOD  focusProxy()
   METHOD  focusWidget()
   METHOD  font()
   METHOD  fontInfo()
   METHOD  fontMetrics()
   METHOD  foregroundRole()
   METHOD  frameGeometry()
   METHOD  frameSize()
   METHOD  geometry()
   METHOD  getContentsMargins( nLeft, nTop, nRight, nBottom )
   METHOD  grabKeyboard()
   METHOD  grabMouse()
   METHOD  grabMouse_1( pCursor )
   METHOD  grabShortcut( pKey, nContext )
   METHOD  hasFocus()
   METHOD  hasMouseTracking()
   METHOD  height()
   METHOD  heightForWidth( nW )
   METHOD  inputMethodQuery( nQuery )
   METHOD  insertAction( pBefore, pAction )
   METHOD  isActiveWindow()
   METHOD  isAncestorOf( pChild )
   METHOD  isEnabled()
   METHOD  isEnabledTo( pAncestor )
   METHOD  isFullScreen()
   METHOD  isHidden()
   METHOD  isMaximized()
   METHOD  isMinimized()
   METHOD  isModal()
   METHOD  isVisible()
   METHOD  isVisibleTo( pAncestor )
   METHOD  isWindow()
   METHOD  isWindowModified()
   METHOD  layout()
   METHOD  layoutDirection()
   METHOD  locale()
   METHOD  mapFrom( pParent, pPos )
   METHOD  mapFromGlobal( pPos )
   METHOD  mapFromParent( pPos )
   METHOD  mapTo( pParent, pPos )
   METHOD  mapToGlobal( pPos )
   METHOD  mapToParent( pPos )
   METHOD  mask()
   METHOD  maximumHeight()
   METHOD  maximumSize()
   METHOD  maximumWidth()
   METHOD  minimumHeight()
   METHOD  minimumSize()
   METHOD  minimumSizeHint()
   METHOD  minimumWidth()
   METHOD  move( nX, nY )
   METHOD  move_1( pQPoint )
   METHOD  nativeParentWidget()
   METHOD  nextInFocusChain()
   METHOD  normalGeometry()
   METHOD  overrideWindowFlags( nFlags )
   METHOD  paintEngine()
   METHOD  palette()
   METHOD  parentWidget()
   METHOD  pos()
   METHOD  rect()
   METHOD  releaseKeyboard()
   METHOD  releaseMouse()
   METHOD  releaseShortcut( nId )
   METHOD  removeAction( pAction )
   METHOD  repaint( nX, nY, nW, nH )
   METHOD  repaint_1( pRect )
   METHOD  repaint_2( pRgn )
   METHOD  resize( nW, nH )
   METHOD  resize_1( pQSize )
   METHOD  restoreGeometry( pGeometry )
   METHOD  saveGeometry()
   METHOD  scroll( nDx, nDy )
   METHOD  scroll_1( nDx, nDy, pR )
   METHOD  setAcceptDrops( lOn )
   METHOD  setAttribute( nAttribute, lOn )
   METHOD  setAutoFillBackground( lEnabled )
   METHOD  setBackgroundRole( nRole )
   METHOD  setBaseSize( pQSize )
   METHOD  setBaseSize_1( nBasew, nBaseh )
   METHOD  setContentsMargins( nLeft, nTop, nRight, nBottom )
   METHOD  setContextMenuPolicy( nPolicy )
   METHOD  setCursor( pQCursor )
   METHOD  setFixedHeight( nH )
   METHOD  setFixedSize( pS )
   METHOD  setFixedSize_1( nW, nH )
   METHOD  setFixedWidth( nW )
   METHOD  setFocus( nReason )
   METHOD  setFocusPolicy( nPolicy )
   METHOD  setFocusProxy( pW )
   METHOD  setFont( pQFont )
   METHOD  setForegroundRole( nRole )
   METHOD  setGeometry( pQRect )
   METHOD  setGeometry_1( nX, nY, nW, nH )
   METHOD  setLayout( pLayout )
   METHOD  setLayoutDirection( nDirection )
   METHOD  setLocale( pLocale )
   METHOD  setMask( pBitmap )
   METHOD  setMask_1( pRegion )
   METHOD  setMaximumHeight( nMaxh )
   METHOD  setMaximumSize( pQSize )
   METHOD  setMaximumSize_1( nMaxw, nMaxh )
   METHOD  setMaximumWidth( nMaxw )
   METHOD  setMinimumHeight( nMinh )
   METHOD  setMinimumSize( pQSize )
   METHOD  setMinimumSize_1( nMinw, nMinh )
   METHOD  setMinimumWidth( nMinw )
   METHOD  setMouseTracking( lEnable )
   METHOD  setPalette( pQPalette )
   METHOD  setParent( pParent )
   METHOD  setParent_1( pParent, nF )
   METHOD  setShortcutAutoRepeat( nId, lEnable )
   METHOD  setShortcutEnabled( nId, lEnable )
   METHOD  setSizeIncrement( pQSize )
   METHOD  setSizeIncrement_1( nW, nH )
   METHOD  setSizePolicy( pPolicy )
   METHOD  setSizePolicy_1( nHorizontal, nVertical )
   METHOD  setStatusTip( cQString )
   METHOD  setStyle( pStyle )
   METHOD  setToolTip( cQString )
   METHOD  setUpdatesEnabled( lEnable )
   METHOD  setWhatsThis( cQString )
   METHOD  setWindowFilePath( cFilePath )
   METHOD  setWindowFlags( nType )
   METHOD  setWindowIcon( pIcon )
   METHOD  setWindowIconText( cQString )
   METHOD  setWindowModality( nWindowModality )
   METHOD  setWindowOpacity( nLevel )
   METHOD  setWindowRole( cRole )
   METHOD  setWindowState( nWindowState )
   METHOD  size()
   METHOD  sizeHint()
   METHOD  sizeIncrement()
   METHOD  sizePolicy()
   METHOD  stackUnder( pW )
   METHOD  statusTip()
   METHOD  style()
   METHOD  styleSheet()
   METHOD  testAttribute( nAttribute )
   METHOD  toolTip()
   METHOD  underMouse()
   METHOD  unsetCursor()
   METHOD  unsetLayoutDirection()
   METHOD  unsetLocale()
   METHOD  update( nX, nY, nW, nH )
   METHOD  update_1( pRect )
   METHOD  update_2( pRgn )
   METHOD  updateGeometry()
   METHOD  updatesEnabled()
   METHOD  visibleRegion()
   METHOD  whatsThis()
   METHOD  width()
   METHOD  window()
   METHOD  windowFilePath()
   METHOD  windowFlags()
   METHOD  windowIcon()
   METHOD  windowIconText()
   METHOD  windowModality()
   METHOD  windowOpacity()
   METHOD  windowRole()
   METHOD  windowState()
   METHOD  windowTitle()
   METHOD  windowType()
   METHOD  x()
   METHOD  y()
   METHOD  keyboardGrabber()
   METHOD  mouseGrabber()
   METHOD  setTabOrder( pFirst, pSecond )
   METHOD  close()
   METHOD  hide()
   METHOD  lower()
   METHOD  raise()
   METHOD  repaint_3()
   METHOD  setDisabled( lDisable )
   METHOD  setEnabled( lEnable )
   METHOD  setFocus_1()
   METHOD  setHidden( lHidden )
   METHOD  setStyleSheet( cStyleSheet )
   METHOD  setVisible( lVisible )
   METHOD  setWindowModified( lModified )
   METHOD  setWindowTitle( cTitle )
   METHOD  show()
   METHOD  showFullScreen()
   METHOD  showMaximized()
   METHOD  showMinimized()
   METHOD  showNormal()
   METHOD  update_3()

   ENDCLASS


METHOD QWidget:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QWidget( ... )
   RETURN Self


METHOD QWidget:acceptDrops()
   RETURN Qt_QWidget_acceptDrops( ::pPtr )


METHOD QWidget:actions()
   RETURN Qt_QWidget_actions( ::pPtr )


METHOD QWidget:activateWindow()
   RETURN Qt_QWidget_activateWindow( ::pPtr )


METHOD QWidget:addAction( pAction )
   RETURN Qt_QWidget_addAction( ::pPtr, hbqt_ptr( pAction ) )


METHOD QWidget:adjustSize()
   RETURN Qt_QWidget_adjustSize( ::pPtr )


METHOD QWidget:autoFillBackground()
   RETURN Qt_QWidget_autoFillBackground( ::pPtr )


METHOD QWidget:backgroundRole()
   RETURN Qt_QWidget_backgroundRole( ::pPtr )


METHOD QWidget:baseSize()
   RETURN Qt_QWidget_baseSize( ::pPtr )


METHOD QWidget:childAt( nX, nY )
   RETURN Qt_QWidget_childAt( ::pPtr, nX, nY )


METHOD QWidget:childAt_1( pP )
   RETURN Qt_QWidget_childAt_1( ::pPtr, hbqt_ptr( pP ) )


METHOD QWidget:childrenRect()
   RETURN Qt_QWidget_childrenRect( ::pPtr )


METHOD QWidget:childrenRegion()
   RETURN Qt_QWidget_childrenRegion( ::pPtr )


METHOD QWidget:clearFocus()
   RETURN Qt_QWidget_clearFocus( ::pPtr )


METHOD QWidget:clearMask()
   RETURN Qt_QWidget_clearMask( ::pPtr )


METHOD QWidget:contentsRect()
   RETURN Qt_QWidget_contentsRect( ::pPtr )


METHOD QWidget:contextMenuPolicy()
   RETURN Qt_QWidget_contextMenuPolicy( ::pPtr )


METHOD QWidget:cursor()
   RETURN Qt_QWidget_cursor( ::pPtr )


METHOD QWidget:ensurePolished()
   RETURN Qt_QWidget_ensurePolished( ::pPtr )


METHOD QWidget:focusPolicy()
   RETURN Qt_QWidget_focusPolicy( ::pPtr )


METHOD QWidget:focusProxy()
   RETURN Qt_QWidget_focusProxy( ::pPtr )


METHOD QWidget:focusWidget()
   RETURN Qt_QWidget_focusWidget( ::pPtr )


METHOD QWidget:font()
   RETURN Qt_QWidget_font( ::pPtr )


METHOD QWidget:fontInfo()
   RETURN Qt_QWidget_fontInfo( ::pPtr )


METHOD QWidget:fontMetrics()
   RETURN Qt_QWidget_fontMetrics( ::pPtr )


METHOD QWidget:foregroundRole()
   RETURN Qt_QWidget_foregroundRole( ::pPtr )


METHOD QWidget:frameGeometry()
   RETURN Qt_QWidget_frameGeometry( ::pPtr )


METHOD QWidget:frameSize()
   RETURN Qt_QWidget_frameSize( ::pPtr )


METHOD QWidget:geometry()
   RETURN Qt_QWidget_geometry( ::pPtr )


METHOD QWidget:getContentsMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QWidget_getContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QWidget:grabKeyboard()
   RETURN Qt_QWidget_grabKeyboard( ::pPtr )


METHOD QWidget:grabMouse()
   RETURN Qt_QWidget_grabMouse( ::pPtr )


METHOD QWidget:grabMouse_1( pCursor )
   RETURN Qt_QWidget_grabMouse_1( ::pPtr, hbqt_ptr( pCursor ) )


METHOD QWidget:grabShortcut( pKey, nContext )
   RETURN Qt_QWidget_grabShortcut( ::pPtr, hbqt_ptr( pKey ), nContext )


METHOD QWidget:hasFocus()
   RETURN Qt_QWidget_hasFocus( ::pPtr )


METHOD QWidget:hasMouseTracking()
   RETURN Qt_QWidget_hasMouseTracking( ::pPtr )


METHOD QWidget:height()
   RETURN Qt_QWidget_height( ::pPtr )


METHOD QWidget:heightForWidth( nW )
   RETURN Qt_QWidget_heightForWidth( ::pPtr, nW )


METHOD QWidget:inputMethodQuery( nQuery )
   RETURN Qt_QWidget_inputMethodQuery( ::pPtr, nQuery )


METHOD QWidget:insertAction( pBefore, pAction )
   RETURN Qt_QWidget_insertAction( ::pPtr, hbqt_ptr( pBefore ), hbqt_ptr( pAction ) )


METHOD QWidget:isActiveWindow()
   RETURN Qt_QWidget_isActiveWindow( ::pPtr )


METHOD QWidget:isAncestorOf( pChild )
   RETURN Qt_QWidget_isAncestorOf( ::pPtr, hbqt_ptr( pChild ) )


METHOD QWidget:isEnabled()
   RETURN Qt_QWidget_isEnabled( ::pPtr )


METHOD QWidget:isEnabledTo( pAncestor )
   RETURN Qt_QWidget_isEnabledTo( ::pPtr, hbqt_ptr( pAncestor ) )


METHOD QWidget:isFullScreen()
   RETURN Qt_QWidget_isFullScreen( ::pPtr )


METHOD QWidget:isHidden()
   RETURN Qt_QWidget_isHidden( ::pPtr )


METHOD QWidget:isMaximized()
   RETURN Qt_QWidget_isMaximized( ::pPtr )


METHOD QWidget:isMinimized()
   RETURN Qt_QWidget_isMinimized( ::pPtr )


METHOD QWidget:isModal()
   RETURN Qt_QWidget_isModal( ::pPtr )


METHOD QWidget:isVisible()
   RETURN Qt_QWidget_isVisible( ::pPtr )


METHOD QWidget:isVisibleTo( pAncestor )
   RETURN Qt_QWidget_isVisibleTo( ::pPtr, hbqt_ptr( pAncestor ) )


METHOD QWidget:isWindow()
   RETURN Qt_QWidget_isWindow( ::pPtr )


METHOD QWidget:isWindowModified()
   RETURN Qt_QWidget_isWindowModified( ::pPtr )


METHOD QWidget:layout()
   RETURN Qt_QWidget_layout( ::pPtr )


METHOD QWidget:layoutDirection()
   RETURN Qt_QWidget_layoutDirection( ::pPtr )


METHOD QWidget:locale()
   RETURN Qt_QWidget_locale( ::pPtr )


METHOD QWidget:mapFrom( pParent, pPos )
   RETURN Qt_QWidget_mapFrom( ::pPtr, hbqt_ptr( pParent ), hbqt_ptr( pPos ) )


METHOD QWidget:mapFromGlobal( pPos )
   RETURN Qt_QWidget_mapFromGlobal( ::pPtr, hbqt_ptr( pPos ) )


METHOD QWidget:mapFromParent( pPos )
   RETURN Qt_QWidget_mapFromParent( ::pPtr, hbqt_ptr( pPos ) )


METHOD QWidget:mapTo( pParent, pPos )
   RETURN Qt_QWidget_mapTo( ::pPtr, hbqt_ptr( pParent ), hbqt_ptr( pPos ) )


METHOD QWidget:mapToGlobal( pPos )
   RETURN Qt_QWidget_mapToGlobal( ::pPtr, hbqt_ptr( pPos ) )


METHOD QWidget:mapToParent( pPos )
   RETURN Qt_QWidget_mapToParent( ::pPtr, hbqt_ptr( pPos ) )


METHOD QWidget:mask()
   RETURN Qt_QWidget_mask( ::pPtr )


METHOD QWidget:maximumHeight()
   RETURN Qt_QWidget_maximumHeight( ::pPtr )


METHOD QWidget:maximumSize()
   RETURN Qt_QWidget_maximumSize( ::pPtr )


METHOD QWidget:maximumWidth()
   RETURN Qt_QWidget_maximumWidth( ::pPtr )


METHOD QWidget:minimumHeight()
   RETURN Qt_QWidget_minimumHeight( ::pPtr )


METHOD QWidget:minimumSize()
   RETURN Qt_QWidget_minimumSize( ::pPtr )


METHOD QWidget:minimumSizeHint()
   RETURN Qt_QWidget_minimumSizeHint( ::pPtr )


METHOD QWidget:minimumWidth()
   RETURN Qt_QWidget_minimumWidth( ::pPtr )


METHOD QWidget:move( nX, nY )
   RETURN Qt_QWidget_move( ::pPtr, nX, nY )


METHOD QWidget:move_1( pQPoint )
   RETURN Qt_QWidget_move_1( ::pPtr, hbqt_ptr( pQPoint ) )


METHOD QWidget:nativeParentWidget()
   RETURN Qt_QWidget_nativeParentWidget( ::pPtr )


METHOD QWidget:nextInFocusChain()
   RETURN Qt_QWidget_nextInFocusChain( ::pPtr )


METHOD QWidget:normalGeometry()
   RETURN Qt_QWidget_normalGeometry( ::pPtr )


METHOD QWidget:overrideWindowFlags( nFlags )
   RETURN Qt_QWidget_overrideWindowFlags( ::pPtr, nFlags )


METHOD QWidget:paintEngine()
   RETURN Qt_QWidget_paintEngine( ::pPtr )


METHOD QWidget:palette()
   RETURN Qt_QWidget_palette( ::pPtr )


METHOD QWidget:parentWidget()
   RETURN Qt_QWidget_parentWidget( ::pPtr )


METHOD QWidget:pos()
   RETURN Qt_QWidget_pos( ::pPtr )


METHOD QWidget:rect()
   RETURN Qt_QWidget_rect( ::pPtr )


METHOD QWidget:releaseKeyboard()
   RETURN Qt_QWidget_releaseKeyboard( ::pPtr )


METHOD QWidget:releaseMouse()
   RETURN Qt_QWidget_releaseMouse( ::pPtr )


METHOD QWidget:releaseShortcut( nId )
   RETURN Qt_QWidget_releaseShortcut( ::pPtr, nId )


METHOD QWidget:removeAction( pAction )
   RETURN Qt_QWidget_removeAction( ::pPtr, hbqt_ptr( pAction ) )


METHOD QWidget:repaint( nX, nY, nW, nH )
   RETURN Qt_QWidget_repaint( ::pPtr, nX, nY, nW, nH )


METHOD QWidget:repaint_1( pRect )
   RETURN Qt_QWidget_repaint_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QWidget:repaint_2( pRgn )
   RETURN Qt_QWidget_repaint_2( ::pPtr, hbqt_ptr( pRgn ) )


METHOD QWidget:resize( nW, nH )
   RETURN Qt_QWidget_resize( ::pPtr, nW, nH )


METHOD QWidget:resize_1( pQSize )
   RETURN Qt_QWidget_resize_1( ::pPtr, hbqt_ptr( pQSize ) )


METHOD QWidget:restoreGeometry( pGeometry )
   RETURN Qt_QWidget_restoreGeometry( ::pPtr, hbqt_ptr( pGeometry ) )


METHOD QWidget:saveGeometry()
   RETURN Qt_QWidget_saveGeometry( ::pPtr )


METHOD QWidget:scroll( nDx, nDy )
   RETURN Qt_QWidget_scroll( ::pPtr, nDx, nDy )


METHOD QWidget:scroll_1( nDx, nDy, pR )
   RETURN Qt_QWidget_scroll_1( ::pPtr, nDx, nDy, hbqt_ptr( pR ) )


METHOD QWidget:setAcceptDrops( lOn )
   RETURN Qt_QWidget_setAcceptDrops( ::pPtr, lOn )


METHOD QWidget:setAttribute( nAttribute, lOn )
   RETURN Qt_QWidget_setAttribute( ::pPtr, nAttribute, lOn )


METHOD QWidget:setAutoFillBackground( lEnabled )
   RETURN Qt_QWidget_setAutoFillBackground( ::pPtr, lEnabled )


METHOD QWidget:setBackgroundRole( nRole )
   RETURN Qt_QWidget_setBackgroundRole( ::pPtr, nRole )


METHOD QWidget:setBaseSize( pQSize )
   RETURN Qt_QWidget_setBaseSize( ::pPtr, hbqt_ptr( pQSize ) )


METHOD QWidget:setBaseSize_1( nBasew, nBaseh )
   RETURN Qt_QWidget_setBaseSize_1( ::pPtr, nBasew, nBaseh )


METHOD QWidget:setContentsMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QWidget_setContentsMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QWidget:setContextMenuPolicy( nPolicy )
   RETURN Qt_QWidget_setContextMenuPolicy( ::pPtr, nPolicy )


METHOD QWidget:setCursor( pQCursor )
   RETURN Qt_QWidget_setCursor( ::pPtr, hbqt_ptr( pQCursor ) )


METHOD QWidget:setFixedHeight( nH )
   RETURN Qt_QWidget_setFixedHeight( ::pPtr, nH )


METHOD QWidget:setFixedSize( pS )
   RETURN Qt_QWidget_setFixedSize( ::pPtr, hbqt_ptr( pS ) )


METHOD QWidget:setFixedSize_1( nW, nH )
   RETURN Qt_QWidget_setFixedSize_1( ::pPtr, nW, nH )


METHOD QWidget:setFixedWidth( nW )
   RETURN Qt_QWidget_setFixedWidth( ::pPtr, nW )


METHOD QWidget:setFocus( nReason )
   RETURN Qt_QWidget_setFocus( ::pPtr, nReason )


METHOD QWidget:setFocusPolicy( nPolicy )
   RETURN Qt_QWidget_setFocusPolicy( ::pPtr, nPolicy )


METHOD QWidget:setFocusProxy( pW )
   RETURN Qt_QWidget_setFocusProxy( ::pPtr, hbqt_ptr( pW ) )


METHOD QWidget:setFont( pQFont )
   RETURN Qt_QWidget_setFont( ::pPtr, hbqt_ptr( pQFont ) )


METHOD QWidget:setForegroundRole( nRole )
   RETURN Qt_QWidget_setForegroundRole( ::pPtr, nRole )


METHOD QWidget:setGeometry( pQRect )
   RETURN Qt_QWidget_setGeometry( ::pPtr, hbqt_ptr( pQRect ) )


METHOD QWidget:setGeometry_1( nX, nY, nW, nH )
   RETURN Qt_QWidget_setGeometry_1( ::pPtr, nX, nY, nW, nH )


METHOD QWidget:setLayout( pLayout )
   RETURN Qt_QWidget_setLayout( ::pPtr, hbqt_ptr( pLayout ) )


METHOD QWidget:setLayoutDirection( nDirection )
   RETURN Qt_QWidget_setLayoutDirection( ::pPtr, nDirection )


METHOD QWidget:setLocale( pLocale )
   RETURN Qt_QWidget_setLocale( ::pPtr, hbqt_ptr( pLocale ) )


METHOD QWidget:setMask( pBitmap )
   RETURN Qt_QWidget_setMask( ::pPtr, hbqt_ptr( pBitmap ) )


METHOD QWidget:setMask_1( pRegion )
   RETURN Qt_QWidget_setMask_1( ::pPtr, hbqt_ptr( pRegion ) )


METHOD QWidget:setMaximumHeight( nMaxh )
   RETURN Qt_QWidget_setMaximumHeight( ::pPtr, nMaxh )


METHOD QWidget:setMaximumSize( pQSize )
   RETURN Qt_QWidget_setMaximumSize( ::pPtr, hbqt_ptr( pQSize ) )


METHOD QWidget:setMaximumSize_1( nMaxw, nMaxh )
   RETURN Qt_QWidget_setMaximumSize_1( ::pPtr, nMaxw, nMaxh )


METHOD QWidget:setMaximumWidth( nMaxw )
   RETURN Qt_QWidget_setMaximumWidth( ::pPtr, nMaxw )


METHOD QWidget:setMinimumHeight( nMinh )
   RETURN Qt_QWidget_setMinimumHeight( ::pPtr, nMinh )


METHOD QWidget:setMinimumSize( pQSize )
   RETURN Qt_QWidget_setMinimumSize( ::pPtr, hbqt_ptr( pQSize ) )


METHOD QWidget:setMinimumSize_1( nMinw, nMinh )
   RETURN Qt_QWidget_setMinimumSize_1( ::pPtr, nMinw, nMinh )


METHOD QWidget:setMinimumWidth( nMinw )
   RETURN Qt_QWidget_setMinimumWidth( ::pPtr, nMinw )


METHOD QWidget:setMouseTracking( lEnable )
   RETURN Qt_QWidget_setMouseTracking( ::pPtr, lEnable )


METHOD QWidget:setPalette( pQPalette )
   RETURN Qt_QWidget_setPalette( ::pPtr, hbqt_ptr( pQPalette ) )


METHOD QWidget:setParent( pParent )
   RETURN Qt_QWidget_setParent( ::pPtr, hbqt_ptr( pParent ) )


METHOD QWidget:setParent_1( pParent, nF )
   RETURN Qt_QWidget_setParent_1( ::pPtr, hbqt_ptr( pParent ), nF )


METHOD QWidget:setShortcutAutoRepeat( nId, lEnable )
   RETURN Qt_QWidget_setShortcutAutoRepeat( ::pPtr, nId, lEnable )


METHOD QWidget:setShortcutEnabled( nId, lEnable )
   RETURN Qt_QWidget_setShortcutEnabled( ::pPtr, nId, lEnable )


METHOD QWidget:setSizeIncrement( pQSize )
   RETURN Qt_QWidget_setSizeIncrement( ::pPtr, hbqt_ptr( pQSize ) )


METHOD QWidget:setSizeIncrement_1( nW, nH )
   RETURN Qt_QWidget_setSizeIncrement_1( ::pPtr, nW, nH )


METHOD QWidget:setSizePolicy( pPolicy )
   RETURN Qt_QWidget_setSizePolicy( ::pPtr, hbqt_ptr( pPolicy ) )


METHOD QWidget:setSizePolicy_1( nHorizontal, nVertical )
   RETURN Qt_QWidget_setSizePolicy_1( ::pPtr, nHorizontal, nVertical )


METHOD QWidget:setStatusTip( cQString )
   RETURN Qt_QWidget_setStatusTip( ::pPtr, cQString )


METHOD QWidget:setStyle( pStyle )
   RETURN Qt_QWidget_setStyle( ::pPtr, hbqt_ptr( pStyle ) )


METHOD QWidget:setToolTip( cQString )
   RETURN Qt_QWidget_setToolTip( ::pPtr, cQString )


METHOD QWidget:setUpdatesEnabled( lEnable )
   RETURN Qt_QWidget_setUpdatesEnabled( ::pPtr, lEnable )


METHOD QWidget:setWhatsThis( cQString )
   RETURN Qt_QWidget_setWhatsThis( ::pPtr, cQString )


METHOD QWidget:setWindowFilePath( cFilePath )
   RETURN Qt_QWidget_setWindowFilePath( ::pPtr, cFilePath )


METHOD QWidget:setWindowFlags( nType )
   RETURN Qt_QWidget_setWindowFlags( ::pPtr, nType )


METHOD QWidget:setWindowIcon( pIcon )
   RETURN Qt_QWidget_setWindowIcon( ::pPtr, hbqt_ptr( pIcon ) )


METHOD QWidget:setWindowIconText( cQString )
   RETURN Qt_QWidget_setWindowIconText( ::pPtr, cQString )


METHOD QWidget:setWindowModality( nWindowModality )
   RETURN Qt_QWidget_setWindowModality( ::pPtr, nWindowModality )


METHOD QWidget:setWindowOpacity( nLevel )
   RETURN Qt_QWidget_setWindowOpacity( ::pPtr, nLevel )


METHOD QWidget:setWindowRole( cRole )
   RETURN Qt_QWidget_setWindowRole( ::pPtr, cRole )


METHOD QWidget:setWindowState( nWindowState )
   RETURN Qt_QWidget_setWindowState( ::pPtr, nWindowState )


METHOD QWidget:size()
   RETURN Qt_QWidget_size( ::pPtr )


METHOD QWidget:sizeHint()
   RETURN Qt_QWidget_sizeHint( ::pPtr )


METHOD QWidget:sizeIncrement()
   RETURN Qt_QWidget_sizeIncrement( ::pPtr )


METHOD QWidget:sizePolicy()
   RETURN Qt_QWidget_sizePolicy( ::pPtr )


METHOD QWidget:stackUnder( pW )
   RETURN Qt_QWidget_stackUnder( ::pPtr, hbqt_ptr( pW ) )


METHOD QWidget:statusTip()
   RETURN Qt_QWidget_statusTip( ::pPtr )


METHOD QWidget:style()
   RETURN Qt_QWidget_style( ::pPtr )


METHOD QWidget:styleSheet()
   RETURN Qt_QWidget_styleSheet( ::pPtr )


METHOD QWidget:testAttribute( nAttribute )
   RETURN Qt_QWidget_testAttribute( ::pPtr, nAttribute )


METHOD QWidget:toolTip()
   RETURN Qt_QWidget_toolTip( ::pPtr )


METHOD QWidget:underMouse()
   RETURN Qt_QWidget_underMouse( ::pPtr )


METHOD QWidget:unsetCursor()
   RETURN Qt_QWidget_unsetCursor( ::pPtr )


METHOD QWidget:unsetLayoutDirection()
   RETURN Qt_QWidget_unsetLayoutDirection( ::pPtr )


METHOD QWidget:unsetLocale()
   RETURN Qt_QWidget_unsetLocale( ::pPtr )


METHOD QWidget:update( nX, nY, nW, nH )
   RETURN Qt_QWidget_update( ::pPtr, nX, nY, nW, nH )


METHOD QWidget:update_1( pRect )
   RETURN Qt_QWidget_update_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QWidget:update_2( pRgn )
   RETURN Qt_QWidget_update_2( ::pPtr, hbqt_ptr( pRgn ) )


METHOD QWidget:updateGeometry()
   RETURN Qt_QWidget_updateGeometry( ::pPtr )


METHOD QWidget:updatesEnabled()
   RETURN Qt_QWidget_updatesEnabled( ::pPtr )


METHOD QWidget:visibleRegion()
   RETURN Qt_QWidget_visibleRegion( ::pPtr )


METHOD QWidget:whatsThis()
   RETURN Qt_QWidget_whatsThis( ::pPtr )


METHOD QWidget:width()
   RETURN Qt_QWidget_width( ::pPtr )


METHOD QWidget:window()
   RETURN Qt_QWidget_window( ::pPtr )


METHOD QWidget:windowFilePath()
   RETURN Qt_QWidget_windowFilePath( ::pPtr )


METHOD QWidget:windowFlags()
   RETURN Qt_QWidget_windowFlags( ::pPtr )


METHOD QWidget:windowIcon()
   RETURN Qt_QWidget_windowIcon( ::pPtr )


METHOD QWidget:windowIconText()
   RETURN Qt_QWidget_windowIconText( ::pPtr )


METHOD QWidget:windowModality()
   RETURN Qt_QWidget_windowModality( ::pPtr )


METHOD QWidget:windowOpacity()
   RETURN Qt_QWidget_windowOpacity( ::pPtr )


METHOD QWidget:windowRole()
   RETURN Qt_QWidget_windowRole( ::pPtr )


METHOD QWidget:windowState()
   RETURN Qt_QWidget_windowState( ::pPtr )


METHOD QWidget:windowTitle()
   RETURN Qt_QWidget_windowTitle( ::pPtr )


METHOD QWidget:windowType()
   RETURN Qt_QWidget_windowType( ::pPtr )


METHOD QWidget:x()
   RETURN Qt_QWidget_x( ::pPtr )


METHOD QWidget:y()
   RETURN Qt_QWidget_y( ::pPtr )


METHOD QWidget:keyboardGrabber()
   RETURN Qt_QWidget_keyboardGrabber( ::pPtr )


METHOD QWidget:mouseGrabber()
   RETURN Qt_QWidget_mouseGrabber( ::pPtr )


METHOD QWidget:setTabOrder( pFirst, pSecond )
   RETURN Qt_QWidget_setTabOrder( ::pPtr, hbqt_ptr( pFirst ), hbqt_ptr( pSecond ) )


METHOD QWidget:close()
   RETURN Qt_QWidget_close( ::pPtr )


METHOD QWidget:hide()
   RETURN Qt_QWidget_hide( ::pPtr )


METHOD QWidget:lower()
   RETURN Qt_QWidget_lower( ::pPtr )


METHOD QWidget:raise()
   RETURN Qt_QWidget_raise( ::pPtr )


METHOD QWidget:repaint_3()
   RETURN Qt_QWidget_repaint_3( ::pPtr )


METHOD QWidget:setDisabled( lDisable )
   RETURN Qt_QWidget_setDisabled( ::pPtr, lDisable )


METHOD QWidget:setEnabled( lEnable )
   RETURN Qt_QWidget_setEnabled( ::pPtr, lEnable )


METHOD QWidget:setFocus_1()
   RETURN Qt_QWidget_setFocus_1( ::pPtr )


METHOD QWidget:setHidden( lHidden )
   RETURN Qt_QWidget_setHidden( ::pPtr, lHidden )


METHOD QWidget:setStyleSheet( cStyleSheet )
   RETURN Qt_QWidget_setStyleSheet( ::pPtr, cStyleSheet )


METHOD QWidget:setVisible( lVisible )
   RETURN Qt_QWidget_setVisible( ::pPtr, lVisible )


METHOD QWidget:setWindowModified( lModified )
   RETURN Qt_QWidget_setWindowModified( ::pPtr, lModified )


METHOD QWidget:setWindowTitle( cTitle )
   RETURN Qt_QWidget_setWindowTitle( ::pPtr, cTitle )


METHOD QWidget:show()
   RETURN Qt_QWidget_show( ::pPtr )


METHOD QWidget:showFullScreen()
   RETURN Qt_QWidget_showFullScreen( ::pPtr )


METHOD QWidget:showMaximized()
   RETURN Qt_QWidget_showMaximized( ::pPtr )


METHOD QWidget:showMinimized()
   RETURN Qt_QWidget_showMinimized( ::pPtr )


METHOD QWidget:showNormal()
   RETURN Qt_QWidget_showNormal( ::pPtr )


METHOD QWidget:update_3()
   RETURN Qt_QWidget_update_3( ::pPtr )

