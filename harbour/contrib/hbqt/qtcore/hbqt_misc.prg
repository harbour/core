/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "error.ch"
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

CREATE CLASS HbQtObjectHandler

   VAR    pPtr     /* TODO: Rename to __pPtr */

   VAR    __pSlots   PROTECTED
   VAR    __pEvents  PROTECTED

   METHOD fromPointer( pPtr )
   METHOD hasValidPointer()

   METHOD connect( cnEvent, bBlock )
   METHOD disconnect( cnEvent )

   ERROR HANDLER onError()

ENDCLASS

/*----------------------------------------------------------------------*/

/* TODO: Drop this function when all raw QT pointers are fully eliminated from .prg level. */
METHOD HbQtObjectHandler:fromPointer( pPtr )
   /* NOTE: Non-GC collected pointers are allowed here. */
   IF hb_isPointer( pPtr )
      ::pPtr := pPtr
   ELSE
      __hbqt_error()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

/* TODO: Drop this function, as it's not desired to have invalid QT pointers wrapped
         into valid .prg level QT objects.
         Currently it will return .F. for objects created using :fromPointer() */
METHOD HbQtObjectHandler:hasValidPointer()
   RETURN __hbqt_isPointer( ::pPtr )

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:onError()
   LOCAL cMsg := __GetMessage()
   LOCAL oError

   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF
   cMsg := "Message not found :" + cMsg

   oError := ErrorNew()

   oError:severity    := ES_ERROR
   oError:genCode     := EG_NOMETHOD
   oError:subSystem   := "HBQT"
   oError:subCode     := 1000
   oError:canRetry    := .F.
   oError:canDefault  := .F.
   oError:Args        := hb_AParams()
   oError:operation   := ProcName()
   oError:Description := cMsg

   Eval( ErrorBlock(), oError )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:connect( cnEvent, bBlock )
   LOCAL nResult

   SWITCH ValType( cnEvent )
   CASE "C"

      IF Empty( ::__pSlots )
         ::__pSlots := __hbqt_slots_new()
      ENDIF
      nResult := __hbqt_slots_connect( ::__pSlots, ::pPtr, cnEvent, bBlock )

      SWITCH nResult
      CASE 0
         RETURN .T.
      CASE 8 /* QT connect call failure */
         RETURN .F.
      ENDSWITCH
      EXIT

   CASE "N"

      IF Empty( ::__pEvents )
         ::__pEvents := __hbqt_events_new()
         ::installEventFilter( HBQEventsFromPointer( ::__pEvents ) )
      ENDIF
      nResult := __hbqt_events_connect( ::__pEvents, ::pPtr, cnEvent, bBlock )

      SWITCH nResult
      CASE 0
         RETURN .T.
      CASE -3 /* event already connected */
         RETURN .F.
      ENDSWITCH
      EXIT

   OTHERWISE
      nResult := 99
   ENDSWITCH

   __hbqt_error( 1200 + nResult )

   RETURN .F.

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:disconnect( cnEvent )
   LOCAL nResult

   SWITCH ValType( cnEvent )
   CASE "C"

      nResult := __hbqt_slots_disconnect( ::__pSlots, ::pPtr, cnEvent )

      SWITCH nResult
      CASE 0
      CASE 4 /* signal not found in object */
      CASE 5 /* disconnect failure */
         RETURN .T.
      CASE 1 /* wrong slot container, no connect was called yet */
      CASE 3 /* event not found */
         RETURN .F.
      ENDSWITCH
      EXIT

   CASE "N"

      nResult := __hbqt_events_disconnect( ::__pEvents, ::pPtr, cnEvent )

      SWITCH nResult
      CASE 0
         RETURN .T.
      CASE -3 /* event not found */
         RETURN .F.
      ENDSWITCH
      EXIT

   OTHERWISE
      nResult := 99
   ENDSWITCH

   __hbqt_error( 1300 + nResult )

   RETURN .F.

/*----------------------------------------------------------------------*/

FUNCTION hbqt_createQtObject( cObjectList, nIndex, pPtr )
   LOCAL cObject, qObj, aObjs

   IF nIndex > 0
      aObjs := hb_aTokens( cObjectList, "$" )
      IF nIndex <= len( aObjs )
         cObject := aObjs[ nIndex ]
      ENDIF
   ELSE
      cObject := cObjectList
   ENDIF

   IF !empty( cObject )
      cObject := strtran( cObject, "*" )
      qObj := eval( &( "{|| HB_" + cObject + "() }" ) )
      qObj:pPtr := pPtr
   ENDIF

   RETURN qObj

/*----------------------------------------------------------------------*/

FUNCTION hbqt_createQtObjectByType( nType, pPtr )
   LOCAL qObj

   SWITCH nType
   CASE QEvent_Timer                              // Regular timer events (QTimerEvent).
//      qObj := HB_QTimerEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_MouseButtonPress                   // Mouse press (QMouseEvent).
      qObj := HB_QMouseEvent(); EXIT
   CASE QEvent_MouseButtonRelease                 // Mouse release (QMouseEvent).
      qObj := HB_QMouseEvent(); EXIT
   CASE QEvent_MouseButtonDblClick                // Mouse press again (QMouseEvent).
      qObj := HB_QMouseEvent(); EXIT
   CASE QEvent_MouseMove                          // Mouse move (QMouseEvent).
      qObj := HB_QMouseEvent(); EXIT
   CASE QEvent_KeyPress                           // Key press (QKeyEvent).
      qObj := HB_QKeyEvent(); EXIT
   CASE QEvent_KeyRelease                         // Key release (QKeyEvent).
      qObj := HB_QKeyEvent(); EXIT
   CASE QEvent_FocusIn                            // Widget gains keyboard focus (QFocusEvent).
      qObj := HB_QFocusEvent(); EXIT
   CASE QEvent_FocusOut                           // Widget loses keyboard focus (QFocusEvent).
      qObj := HB_QFocusEvent(); EXIT
   CASE QEvent_Enter                              // Mouse enters widget's boundaries.
      qObj := HB_QMouseEvent(); EXIT
   CASE QEvent_Leave                              // Mouse leaves widget's boundaries.
      qObj := HB_QMouseEvent(); EXIT
   CASE QEvent_Paint                              // Screen update necessary (QPaintEvent).
      qObj := HB_QPaintEvent(); EXIT
   CASE QEvent_Move                               // Widget's position changed (QMoveEvent).
      qObj := HB_QMoveEvent(); EXIT
   CASE QEvent_Resize                             // Widget's size changed (QResizeEvent).
      qObj := HB_QResizeEvent(); EXIT
   CASE QEvent_Show                               // Widget was shown on screen (QShowEvent).
      qObj := HB_QShowEvent(); EXIT
   CASE QEvent_Hide                               // Widget was hidden (QHideEvent).
      qObj := HB_QHideEvent(); EXIT
   CASE QEvent_Close                              // Widget was closed (QCloseEvent).
//      qObj := HB_QCloseEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ParentChange                       // The widget parent has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WindowActivate                     // Window was activated.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WindowDeactivate                   // Window was deactivated.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ShowToParent                       // A child widget has been shown.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_HideToParent                       // A child widget has been hidden.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_Wheel                              // Mouse wheel rolled (QWheelEvent).
      qObj := HB_QWheelEvent(); EXIT
   CASE QEvent_WindowTitleChange                  // The window title has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WindowIconChange                   // The window's icon has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ApplicationWindowIconChange        // The application's icon has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ApplicationFontChange              // The default application font has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ApplicationLayoutDirectionChange   // The default application layout direction has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ApplicationPaletteChange           // The default application palette has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_PaletteChange                      // Palette of the widget changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_Clipboard                          // The clipboard contents have changed (QClipboardEvent).
//      qObj := HB_QClipboardEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_MetaCall                           // An asynchronous method invocation via QMetaObject_invokeMethod().
      qObj := HB_QEvent(); EXIT
   CASE QEvent_SockAct                            // Socket activated, used to implement QSocketNotifier.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ShortcutOverride                   // Key press in child, for overriding shortcut key handling (QKeyEvent).
      qObj := HB_QKeyEvent(); EXIT
   CASE QEvent_DeferredDelete                     // The object will be deleted after it has cleaned up.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_DragEnter                          // The cursor enters a widget during a drag and drop operation (QDragEnterEvent).
      qObj := HB_QDragEnterEvent(); EXIT
   CASE QEvent_DragLeave                          // The cursor leaves a widget during a drag and drop operation (QDragLeaveEvent).
      qObj := HB_QDragLeaveEvent(); EXIT
   CASE QEvent_DragMove                           // A drag and drop operation is in progress (QDragMoveEvent).
      qObj := HB_QDragMoveEvent(); EXIT
   CASE QEvent_Drop                               // A drag and drop operation is completed (QDropEvent).
      qObj := HB_QDropEvent(); EXIT
   CASE QEvent_ChildAdded                         // An object gets a child (QChildEvent).
//      qObj := HB_QChildEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ChildPolished                      // A widget child gets polished (QChildEvent).
//      qObj := HB_QChildEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ChildInserted                      // An object gets a child (QChildEvent). Qt3Support only, use ChildAdded instead.
//      qObj := HB_QChildEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ChildRemoved                       // An object loses a child (QChildEvent).
//      qObj := HB_QChildEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_PolishRequest                      // The widget should be polished.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_Polish                             // The widget is polished.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_LayoutRequest                      // Widget layout needs to be redone.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_UpdateRequest                      // The widget should be repainted.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_UpdateLater                        // The widget should be queued to be repainted at a later time.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ContextMenu                        // Context popup menu (QContextMenuEvent).
      qObj := HB_QContextMenuEvent(); EXIT
   CASE QEvent_InputMethod                        // An input method is being used (QInputMethodEvent).
      qObj := HB_QInputMethodEvent(); EXIT
   CASE QEvent_AccessibilityPrepare               // Accessibility information is requested.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_TabletMove                         // Wacom tablet move (QTabletEvent).
//      qObj := HB_QTabletEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_LocaleChange                       // The system locale has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_LanguageChange                     // The application translation changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_LayoutDirectionChange              // The direction of layouts changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_TabletPress                        // Wacom tablet press (QTabletEvent).
//      qObj := HB_QTabletEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_TabletRelease                      // Wacom tablet release (QTabletEvent).
//      qObj := HB_QTabletEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_OkRequest                          // Ok button in decoration pressed. Supported only for Windows CE.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_IconDrag                           // The main icon of a window has been dragged away (QIconDragEvent).
//      qObj := HB_QIconDragEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_FontChange                         // Widget's font has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_EnabledChange                      // Widget's enabled state has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ActivationChange                   // A widget's top-level window activation state has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_StyleChange                        // Widget's style has been changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_IconTextChange                     // Widget's icon text has been changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ModifiedChange                     // Widgets modification state has been changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WindowBlocked                      // The window is blocked by a modal dialog.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WindowUnblocked                    // The window is unblocked after a modal dialog exited.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WindowStateChange                  // The window's state (minimized, maximized or full-screen) has changed (QWindowStateChangeEvent).
      qObj := HB_QWindowStateChangeEvent(); EXIT
   CASE QEvent_MouseTrackingChange                // The mouse tracking state has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ToolTip                            // A tooltip was requested (QHelpEvent).
      qObj := HB_QHelpEvent(); EXIT
   CASE QEvent_WhatsThis                          // The widget should reveal "What's This?" help (QHelpEvent).
      qObj := HB_QHelpEvent(); EXIT
   CASE QEvent_StatusTip                          // A status tip is requested (QStatusTipEvent).
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ActionChanged                      // An action has been changed (QActionEvent).
//      qObj := HB_QActionEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ActionAdded                        // A new action has been added (QActionEvent).
//      qObj := HB_QActionEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ActionRemoved                      // An action has been removed (QActionEvent).
//      qObj := HB_QActionEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_FileOpen                           // File open request (QFileOpenEvent).
//      qObj := HB_QFileOpenEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_Shortcut                           // Key press in child for shortcut key handling (QShortcutEvent).
//      qObj := HB_QShortcutEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WhatsThisClicked                   // A link in a widget's "What's This?" help was clicked.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_AccessibilityHelp                  // Used to query accessibility help texts (QAccessibleEvent).
//      qObj := HB_QAccessibleEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ToolBarChange                      // The toolbar button is toggled on Mac OS X.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ApplicationActivate                // The application has been made available to the user.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ApplicationActivated               // This enum has been deprecated. Use ApplicationActivate instead.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ApplicationDeactivate              // The application has been suspended, and is unavailable to the user.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_QueryWhatsThis                     // The widget should accept the event if it has "What's This?" help.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_EnterWhatsThisMode                 // Send to toplevel widgets when the application enters "What's This?" mode.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_LeaveWhatsThisMode                 // Send to toplevel widgets when the application leaves "What's This?" mode.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ZOrderChange                       // The widget's z-order has changed. This event is never sent to top level windows.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_HoverEnter                         // The mouse cursor enters a hover widget (QHoverEvent).
//      qObj := HB_QHoverEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_HoverLeave                         // The mouse cursor leaves a hover widget (QHoverEvent).
//      qObj := HB_QHoverEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_HoverMove                          // The mouse cursor moves inside a hover widget (QHoverEvent).
//      qObj := HB_QHoverEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_AccessibilityDescription           // Used to query accessibility description texts (QAccessibleEvent).
//      qObj := HB_QAccessibleEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ParentAboutToChange                // The widget parent is about to change.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_WinEventAct                        // A Windows-specific activation event has occurred.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_EnterEditFocus                     // An editor widget gains focus for editing.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_LeaveEditFocus                     // An editor widget loses focus for editing.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_MenubarUpdated                     // The window's menu bar has been updated.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_GraphicsSceneMouseMove             // Move mouse in a graphics scene (QGraphicsSceneMouseEvent).
      qObj := HB_QGraphicsSceneMouseEvent(); EXIT
   CASE QEvent_GraphicsSceneMousePress            // Mouse press in a graphics scene (QGraphicsSceneMouseEvent).
      qObj := HB_QGraphicsSceneMouseEvent(); EXIT
   CASE QEvent_GraphicsSceneMouseRelease          // Mouse release in a graphics scene (QGraphicsSceneMouseEvent).
      qObj := HB_QGraphicsSceneMouseEvent(); EXIT
   CASE QEvent_GraphicsSceneMouseDoubleClick      // Mouse press again (double click) in a graphics scene (QGraphicsSceneMouseEvent).
      qObj := HB_QGraphicsSceneMouseEvent(); EXIT
   CASE QEvent_GraphicsSceneContextMenu           // Context popup menu over a graphics scene (QGraphicsSceneContextMenuEvent).
      qObj := HB_QGraphicsSceneContextMenuEvent(); EXIT
   CASE QEvent_GraphicsSceneHoverEnter            // The mouse cursor enters a hover item in a graphics scene (QGraphicsSceneHoverEvent).
      qObj := HB_QGraphicsSceneHoverEvent(); EXIT
   CASE QEvent_GraphicsSceneHoverMove             // The mouse cursor moves inside a hover item in a graphics scene (QGraphicsSceneHoverEvent).
      qObj := HB_QGraphicsSceneHoverEvent(); EXIT
   CASE QEvent_GraphicsSceneHoverLeave            // The mouse cursor leaves a hover item in a graphics scene (QGraphicsSceneHoverEvent).
      qObj := HB_QGraphicsSceneHoverEvent(); EXIT
   CASE QEvent_GraphicsSceneHelp                  // The user requests help for a graphics scene (QHelpEvent).
      qObj := HB_QHelpEvent(); EXIT
   CASE QEvent_GraphicsSceneDragEnter             // The cursor enters a graphics scene during a drag and drop operation.
      qObj := HB_QGraphicsSceneDragDropEvent(); EXIT
   CASE QEvent_GraphicsSceneDragMove              // A drag and drop operation is in progress over a scene.
      qObj := HB_QGraphicsSceneDragDropEvent(); EXIT
   CASE QEvent_GraphicsSceneDragLeave             // The cursor leaves a graphics scene during a drag and drop operation.
      qObj := HB_QGraphicsSceneDragDropEvent(); EXIT
   CASE QEvent_GraphicsSceneDrop                  // A drag and drop operation is completed over a scene.
      qObj := HB_QGraphicsSceneDragDropEvent(); EXIT
   CASE QEvent_GraphicsSceneWheel                 // Mouse wheel rolled in a graphics scene (QGraphicsSceneWheelEvent).
      qObj := HB_QGraphicsSceneWheelEvent(); EXIT
   CASE QEvent_KeyboardLayoutChange               // The keyboard layout has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_DynamicPropertyChange              // A dynamic property was added, changed or removed from the object. User events should have values b
      qObj := HB_QEvent(); EXIT
   CASE QEvent_TabletEnterProximity               // Wacom tablet enter proximity event (QTabletEvent), sent to QApplication.
//      qObj := HB_QTabletEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_TabletLeaveProximity               // Wacom tablet leave proximity event (QTabletEvent), sent to QApplication.
//      qObj := HB_QTabletEvent(); EXIT
      qObj := HB_QEvent(); EXIT
   CASE QEvent_NonClientAreaMouseMove             // A mouse move occurred outside the client area.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_NonClientAreaMouseButtonPress      // A mouse button press occurred outside the client area.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_NonClientAreaMouseButtonRelease    // A mouse button release occurred outside the client area.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_NonClientAreaMouseButtonDblClick   // A mouse double click occurred outside the client area.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_MacSizeChange                      // The user changed his widget sizes (Mac OS X only).
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ContentsRectChange                 // The margins of the widget's content rect changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_GraphicsSceneResize                // Widget was resized (QGraphicsSceneResizeEvent).
      qObj := HB_QGraphicsSceneResizeEvent(); EXIT
   CASE QEvent_GraphicsSceneMove                  // Widget was moved (QGraphicsSceneMoveEvent).
      qObj := HB_QGraphicsSceneMoveEvent(); EXIT
   CASE QEvent_CursorChange                       // The widget's cursor has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_ToolTipChange                      // The widget's tooltip has changed.
      qObj := HB_QEvent(); EXIT
   CASE QEvent_GrabMouse                          // Item gains mouse grab (QGraphicsItem only).
      qObj := HB_QEvent(); EXIT
   CASE QEvent_UngrabMouse                        // Item loses mouse grab (QGraphicsItem only).
      qObj := HB_QEvent(); EXIT
   CASE QEvent_GrabKeyboard                       // Item gains keyboard grab (QGraphicsItem only).
      qObj := HB_QEvent(); EXIT
   CASE QEvent_UngrabKeyboard                     // Item loses keyboard grab (QGraphicsItem only).
      qObj := HB_QEvent(); EXIT
   OTHERWISE
      qObj := HB_QEvent(); EXIT
   ENDSWITCH

   IF !empty( qObj )
      qObj:pPtr := pPtr
   ENDIF

   RETURN qObj

/*----------------------------------------------------------------------*/


