/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta (marcosgambeta at gmail dot com)
 * Copyright 2009 Pritpal Bedi (pritpal@vouchcac.com)
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2010 Francesco Perillo ()
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

#include "hbqt.h"

#include "hbapierr.h"
#include "hbvm.h"
#include "hbinit.h"

#include "hbqtgui.ch"

#if QT_VERSION >= 0x040500

#include <QtGui/QApplication>

#include <QtGui/QItemSelection>
#include <QtGui/QTextCursor>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTextBlock>
#include <QtGui/QSessionManager>
#include <QtGui/QColor>

HB_EXTERN_BEGIN
extern void * hbqt_gcAllocate_QColor( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QItemSelection( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextCharFormat( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QFont( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextCursor( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextBlock( void * pObj, bool bNew );
HB_EXTERN_END

/*----------------------------------------------------------------------*/

static void hbqt_SlotsExecQColor( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QColor( new QColor( ( *reinterpret_cast< QColor( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QColor", 0 ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecItemSelItemSel( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( *reinterpret_cast< QItemSelection( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QItemSelection", 0 ) );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( *reinterpret_cast< QItemSelection( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QItemSelection", 0 ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQTextCharFormat( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( *reinterpret_cast< QTextCharFormat( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QTextCharFormat", 0 ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQFont( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QFont( new QFont( ( *reinterpret_cast< QFont( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QFont", 0 ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextCursor( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( *reinterpret_cast< QTextCursor( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QTextCursor", 0 ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextBlock( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( *reinterpret_cast< QTextBlock( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QTextBlock", 0 ) );
   hb_vmSend( 1 );
}

/*----------------------------------------------------------------------*/

HB_FUNC_EXTERN( HB_QCONTEXTMENUEVENT );
HB_FUNC_EXTERN( HB_QDRAGENTEREVENT );
HB_FUNC_EXTERN( HB_QDRAGLEAVEEVENT );
HB_FUNC_EXTERN( HB_QDRAGMOVEEVENT );
HB_FUNC_EXTERN( HB_QDROPEVENT );
HB_FUNC_EXTERN( HB_QEVENT );
HB_FUNC_EXTERN( HB_QFOCUSEVENT );
HB_FUNC_EXTERN( HB_QFOCUSEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENECONTEXTMENUEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEDRAGDROPEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEHOVEREVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEMOUSEEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEMOVEEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENERESIZEEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEWHEELEVENT );
HB_FUNC_EXTERN( HB_QHELPEVENT );
HB_FUNC_EXTERN( HB_QHIDEEVENT );
HB_FUNC_EXTERN( HB_QINPUTMETHODEVENT );
HB_FUNC_EXTERN( HB_QKEYEVENT );
HB_FUNC_EXTERN( HB_QMOUSEEVENT );
HB_FUNC_EXTERN( HB_QMOVEEVENT );
HB_FUNC_EXTERN( HB_QPAINTEVENT );
HB_FUNC_EXTERN( HB_QRESIZEEVENT );
HB_FUNC_EXTERN( HB_QSHOWEVENT );
HB_FUNC_EXTERN( HB_QWHEELEVENT );
HB_FUNC_EXTERN( HB_QWINDOWSTATECHANGEEVENT );

void _hbqtgui_force_link_for_event( void )
{
   HB_FUNC_EXEC( HB_QCONTEXTMENUEVENT );
   HB_FUNC_EXEC( HB_QDRAGENTEREVENT );
   HB_FUNC_EXEC( HB_QDRAGLEAVEEVENT );
   HB_FUNC_EXEC( HB_QDRAGMOVEEVENT );
   HB_FUNC_EXEC( HB_QDROPEVENT );
   HB_FUNC_EXEC( HB_QEVENT );
   HB_FUNC_EXEC( HB_QFOCUSEVENT );
   HB_FUNC_EXEC( HB_QFOCUSEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENECONTEXTMENUEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEDRAGDROPEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEHOVEREVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEMOUSEEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEMOVEEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENERESIZEEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEWHEELEVENT );
   HB_FUNC_EXEC( HB_QHELPEVENT );
   HB_FUNC_EXEC( HB_QHIDEEVENT );
   HB_FUNC_EXEC( HB_QINPUTMETHODEVENT );
   HB_FUNC_EXEC( HB_QKEYEVENT );
   HB_FUNC_EXEC( HB_QMOUSEEVENT );
   HB_FUNC_EXEC( HB_QMOVEEVENT );
   HB_FUNC_EXEC( HB_QPAINTEVENT );
   HB_FUNC_EXEC( HB_QRESIZEEVENT );
   HB_FUNC_EXEC( HB_QSHOWEVENT );
   HB_FUNC_EXEC( HB_QWHEELEVENT );
   HB_FUNC_EXEC( HB_QWINDOWSTATECHANGEEVENT );
}

static void hbqt_registerCallbacks( void )
{
   hbqt_slots_register_callback( "QColor"                           , hbqt_SlotsExecQColor          );
   hbqt_slots_register_callback( "QFont"                            , hbqt_SlotsExecQFont           );
   hbqt_slots_register_callback( "QItemSelection$QItemSelection"    , hbqt_SlotsExecItemSelItemSel  );
   hbqt_slots_register_callback( "QTextBlock"                       , hbqt_SlotsExecQTextBlock      );
   hbqt_slots_register_callback( "QTextCharFormat"                  , hbqt_SlotsExecQTextCharFormat );
   hbqt_slots_register_callback( "QTextCursor"                      , hbqt_SlotsExecQTextCursor     );

   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Timer                             , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MouseButtonPress                  , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MouseButtonRelease                , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MouseButtonDblClick               , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MouseMove                         , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_KeyPress                          , "hb_QKeyEvent"                      );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_KeyRelease                        , "hb_QKeyEvent"                      );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_FocusIn                           , "hb_QFocusEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_FocusOut                          , "hb_QFocusEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Enter                             , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Leave                             , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Paint                             , "hb_QPaintEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Move                              , "hb_QMoveEvent"                     );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Resize                            , "hb_QResizeEvent"                   );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Show                              , "hb_QShowEvent"                     );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Hide                              , "hb_QHideEvent"                     );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Close                             , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ParentChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WindowActivate                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WindowDeactivate                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ShowToParent                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_HideToParent                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Wheel                             , "hb_QWheelEvent"                    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WindowTitleChange                 , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WindowIconChange                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ApplicationWindowIconChange       , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ApplicationFontChange             , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ApplicationLayoutDirectionChange  , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ApplicationPaletteChange          , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_PaletteChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Clipboard                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MetaCall                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_SockAct                           , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ShortcutOverride                  , "hb_QKeyEvent"                      );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_DeferredDelete                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_DragEnter                         , "hb_QDragEnterEvent"                );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_DragLeave                         , "hb_QDragLeaveEvent"                );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_DragMove                          , "hb_QDragMoveEvent"                 );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Drop                              , "hb_QDropEvent"                     );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ChildAdded                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ChildPolished                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ChildInserted                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ChildRemoved                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_PolishRequest                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Polish                            , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_LayoutRequest                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_UpdateRequest                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_UpdateLater                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ContextMenu                       , "hb_QContextMenuEvent"              );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_InputMethod                       , "hb_QInputMethodEvent"              );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_AccessibilityPrepare              , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_TabletMove                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_LocaleChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_LanguageChange                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_LayoutDirectionChange             , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_TabletPress                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_TabletRelease                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_OkRequest                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_IconDrag                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_FontChange                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_EnabledChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ActivationChange                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_StyleChange                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_IconTextChange                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ModifiedChange                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WindowBlocked                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WindowUnblocked                   , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WindowStateChange                 , "hb_QWindowStateChangeEvent"        );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MouseTrackingChange               , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ToolTip                           , "hb_QHelpEvent"                     );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WhatsThis                         , "hb_QHelpEvent"                     );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_StatusTip                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ActionChanged                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ActionAdded                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ActionRemoved                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_FileOpen                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_Shortcut                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WhatsThisClicked                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_AccessibilityHelp                 , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ToolBarChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ApplicationActivate               , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ApplicationActivated              , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ApplicationDeactivate             , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_QueryWhatsThis                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_EnterWhatsThisMode                , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_LeaveWhatsThisMode                , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ZOrderChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_HoverEnter                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_HoverLeave                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_HoverMove                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_AccessibilityDescription          , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ParentAboutToChange               , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_WinEventAct                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_EnterEditFocus                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_LeaveEditFocus                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MenubarUpdated                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneMouseMove            , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneMousePress           , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneMouseRelease         , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneMouseDoubleClick     , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneContextMenu          , "hb_QGraphicsSceneContextMenuEvent" );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneHoverEnter           , "hb_QGraphicsSceneHoverEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneHoverMove            , "hb_QGraphicsSceneHoverEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneHoverLeave           , "hb_QGraphicsSceneHoverEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneHelp                 , "hb_QHelpEvent"                     );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneDragEnter            , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneDragMove             , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneDragLeave            , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneDrop                 , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneWheel                , "hb_QGraphicsSceneWheelEvent"       );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_KeyboardLayoutChange              , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_DynamicPropertyChange             , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_TabletEnterProximity              , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_TabletLeaveProximity              , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_NonClientAreaMouseMove            , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_NonClientAreaMouseButtonPress     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_NonClientAreaMouseButtonRelease   , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_NonClientAreaMouseButtonDblClick  , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_MacSizeChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ContentsRectChange                , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneResize               , "hb_QGraphicsSceneResizeEvent"      );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GraphicsSceneMove                 , "hb_QGraphicsSceneMoveEvent"        );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_CursorChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_ToolTipChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GrabMouse                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_UngrabMouse                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_GrabKeyboard                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( ( QEvent::Type ) QEvent_UngrabKeyboard                    , "hb_QEvent"                         );
}

/*----------------------------------------------------------------------*/

static QApplication * s_app = NULL;

static int s_argc;
static char ** s_argv;

HB_FUNC_EXTERN( __HBQTCORE );

HB_FUNC( __HBQTGUI )
{
   HB_FUNC_EXEC( __HBQTCORE );
}

HB_EXTERN_BEGIN
extern HB_EXPORT QApplication * __hbqtgui_app( void );
HB_EXTERN_END

QApplication * __hbqtgui_app( void )
{
   return s_app;
}

static void hbqt_lib_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_argc = hb_cmdargARGC();
   s_argv = hb_cmdargARGV();

   s_app = new QApplication( s_argc, s_argv );

   if( ! s_app )
      hb_errInternal( 11001, "hbqt_lib_init(): HBQTGUI Initilization Error.", NULL, NULL );

   hb_cmdargInit( s_argc, s_argv );

   hbqt_registerCallbacks();
}

static void hbqt_lib_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqtgui_init_ )
   hb_vmAtInit( hbqt_lib_init, NULL );
   hb_vmAtExit( hbqt_lib_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtgui_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtgui_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtgui_init_ )
   #include "hbiniseg.h"
#endif

#endif
