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
      aObjs := hb_ATokens( cObjectList, "$" )
      IF nIndex <= Len( aObjs )
         cObject := aObjs[ nIndex ]
      ENDIF
   ELSE
      cObject := cObjectList
   ENDIF

   IF ! Empty( cObject )
      /* TODO: Change macro expansion to more efficient solution */
      qObj := Eval( hb_MacroBlock( "HB_" + StrTran( cObject, "*" ) + "()" ) )
      qObj:pPtr := pPtr
   ENDIF

   RETURN qObj

/*----------------------------------------------------------------------*/

/* TOFIX: QTCORE should _NEVER_ refer to any names or parts of higher level
          QT components (f.e. QTGUI). */
#define QEvent_None                               0
#define QEvent_Timer                              1
#define QEvent_MouseButtonPress                   2
#define QEvent_MouseButtonRelease                 3
#define QEvent_MouseButtonDblClick                4
#define QEvent_MouseMove                          5
#define QEvent_KeyPress                           6
#define QEvent_KeyRelease                         7
#define QEvent_FocusIn                            8
#define QEvent_FocusOut                           9
#define QEvent_Enter                              10
#define QEvent_Leave                              11
#define QEvent_Paint                              12
#define QEvent_Move                               13
#define QEvent_Resize                             14
#define QEvent_Show                               17
#define QEvent_Hide                               18
#define QEvent_Close                              19
#define QEvent_ParentChange                       21
#define QEvent_WindowActivate                     24
#define QEvent_WindowDeactivate                   25
#define QEvent_ShowToParent                       26
#define QEvent_HideToParent                       27
#define QEvent_Wheel                              31
#define QEvent_WindowTitleChange                  33
#define QEvent_WindowIconChange                   34
#define QEvent_ApplicationWindowIconChange        35
#define QEvent_ApplicationFontChange              36
#define QEvent_ApplicationLayoutDirectionChange   37
#define QEvent_ApplicationPaletteChange           38
#define QEvent_PaletteChange                      39
#define QEvent_Clipboard                          40
#define QEvent_MetaCall                           43
#define QEvent_SockAct                            50
#define QEvent_ShortcutOverride                   51
#define QEvent_DeferredDelete                     52
#define QEvent_DragEnter                          60
#define QEvent_DragLeave                          62
#define QEvent_DragMove                           61
#define QEvent_Drop                               63
#define QEvent_ChildAdded                         68
#define QEvent_ChildPolished                      69
#define QEvent_ChildInserted                      70
#define QEvent_ChildRemoved                       71
#define QEvent_PolishRequest                      74
#define QEvent_Polish                             75
#define QEvent_LayoutRequest                      76
#define QEvent_UpdateRequest                      77
#define QEvent_UpdateLater                        78
#define QEvent_ContextMenu                        82
#define QEvent_InputMethod                        83
#define QEvent_AccessibilityPrepare               86
#define QEvent_TabletMove                         87
#define QEvent_LocaleChange                       88
#define QEvent_LanguageChange                     89
#define QEvent_LayoutDirectionChange              90
#define QEvent_TabletPress                        92
#define QEvent_TabletRelease                      93
#define QEvent_OkRequest                          94
#define QEvent_IconDrag                           96
#define QEvent_FontChange                         97
#define QEvent_EnabledChange                      98
#define QEvent_ActivationChange                   99
#define QEvent_StyleChange                        100
#define QEvent_IconTextChange                     101
#define QEvent_ModifiedChange                     102
#define QEvent_WindowBlocked                      103
#define QEvent_WindowUnblocked                    104
#define QEvent_WindowStateChange                  105
#define QEvent_MouseTrackingChange                109
#define QEvent_ToolTip                            110
#define QEvent_WhatsThis                          111
#define QEvent_StatusTip                          112
#define QEvent_ActionChanged                      113
#define QEvent_ActionAdded                        114
#define QEvent_ActionRemoved                      115
#define QEvent_FileOpen                           116
#define QEvent_Shortcut                           117
#define QEvent_WhatsThisClicked                   118
#define QEvent_AccessibilityHelp                  119
#define QEvent_ToolBarChange                      120
#define QEvent_ApplicationActivate                121
#define QEvent_ApplicationActivated               121
#define QEvent_ApplicationDeactivate              122
#define QEvent_QueryWhatsThis                     123
#define QEvent_EnterWhatsThisMode                 124
#define QEvent_LeaveWhatsThisMode                 125
#define QEvent_ZOrderChange                       126
#define QEvent_HoverEnter                         127
#define QEvent_HoverLeave                         128
#define QEvent_HoverMove                          129
#define QEvent_AccessibilityDescription           130
#define QEvent_ParentAboutToChange                131
#define QEvent_WinEventAct                        132
#define QEvent_EnterEditFocus                     150
#define QEvent_LeaveEditFocus                     151
#define QEvent_MenubarUpdated                     153
#define QEvent_GraphicsSceneMouseMove             155
#define QEvent_GraphicsSceneMousePress            156
#define QEvent_GraphicsSceneMouseRelease          157
#define QEvent_GraphicsSceneMouseDoubleClick      158
#define QEvent_GraphicsSceneContextMenu           159
#define QEvent_GraphicsSceneHoverEnter            160
#define QEvent_GraphicsSceneHoverMove             161
#define QEvent_GraphicsSceneHoverLeave            162
#define QEvent_GraphicsSceneHelp                  163
#define QEvent_GraphicsSceneDragEnter             164
#define QEvent_GraphicsSceneDragMove              165
#define QEvent_GraphicsSceneDragLeave             166
#define QEvent_GraphicsSceneDrop                  167
#define QEvent_GraphicsSceneWheel                 168
#define QEvent_KeyboardLayoutChange               169
#define QEvent_DynamicPropertyChange              170
#define QEvent_TabletEnterProximity               171
#define QEvent_TabletLeaveProximity               172
#define QEvent_NonClientAreaMouseMove             173
#define QEvent_NonClientAreaMouseButtonPress      174
#define QEvent_NonClientAreaMouseButtonRelease    175
#define QEvent_NonClientAreaMouseButtonDblClick   176
#define QEvent_MacSizeChange                      177
#define QEvent_ContentsRectChange                 178
#define QEvent_GraphicsSceneResize                181
#define QEvent_GraphicsSceneMove                  182
#define QEvent_CursorChange                       183
#define QEvent_ToolTipChange                      184
#define QEvent_GrabMouse                          186
#define QEvent_UngrabMouse                        187
#define QEvent_GrabKeyboard                       188
#define QEvent_UngrabKeyboard                     189

FUNCTION hbqtgui_createQtObjectByType( nType, pPtr )
   STATIC s_b_ := { ;
      QEvent_Timer                             => "QEvent"      ,;
      QEvent_MouseButtonPress                  => "QMouseEvent" ,;
      QEvent_MouseButtonRelease                => "QMouseEvent" ,;
      QEvent_MouseButtonDblClick               => "QMouseEvent" ,;
      QEvent_MouseMove                         => "QMouseEvent" ,;
      QEvent_KeyPress                          => "QKeyEvent"   ,;
      QEvent_KeyRelease                        => "QKeyEvent"   ,;
      QEvent_FocusIn                           => "QFocusEvent" ,;
      QEvent_FocusOut                          => "QFocusEvent" ,;
      QEvent_Enter                             => "QMouseEvent" ,;
      QEvent_Leave                             => "QMouseEvent" ,;
      QEvent_Paint                             => "QPaintEvent" ,;
      QEvent_Move                              => "QMoveEvent"  ,;
      QEvent_Resize                            => "QResizeEvent",;
      QEvent_Show                              => "QShowEvent"  ,;
      QEvent_Hide                              => "QHideEvent"  ,;
      QEvent_Close                             => "QEvent"      ,;
      QEvent_ParentChange                      => "QEvent"      ,;
      QEvent_WindowActivate                    => "QEvent"      ,;
      QEvent_WindowDeactivate                  => "QEvent"      ,;
      QEvent_ShowToParent                      => "QEvent"      ,;
      QEvent_HideToParent                      => "QEvent"      ,;
      QEvent_Wheel                             => "QWheelEvent" ,;
      QEvent_WindowTitleChange                 => "QEvent"      ,;
      QEvent_WindowIconChange                  => "QEvent"      ,;
      QEvent_ApplicationWindowIconChange       => "QEvent"      ,;
      QEvent_ApplicationFontChange             => "QEvent"      ,;
      QEvent_ApplicationLayoutDirectionChange  => "QEvent"      ,;
      QEvent_ApplicationPaletteChange          => "QEvent"      ,;
      QEvent_PaletteChange                     => "QEvent"      ,;
      QEvent_Clipboard                         => "QEvent"      ,;
      QEvent_MetaCall                          => "QEvent"      ,;
      QEvent_SockAct                           => "QEvent"      ,;
      QEvent_ShortcutOverride                  => "QKeyEvent"   ,;
      QEvent_DeferredDelete                    => "QEvent"      ,;
      QEvent_DragEnter                         => "QDragEnterEvent" ,;
      QEvent_DragLeave                         => "QDragLeaveEvent" ,;
      QEvent_DragMove                          => "QDragMoveEvent"  ,;
      QEvent_Drop                              => "QDropEvent"      ,;
      QEvent_ChildAdded                        => "QEvent"      ,;
      QEvent_ChildPolished                     => "QEvent"      ,;
      QEvent_ChildInserted                     => "QEvent"      ,;
      QEvent_ChildRemoved                      => "QEvent"      ,;
      QEvent_PolishRequest                     => "QEvent"      ,;
      QEvent_Polish                            => "QEvent"      ,;
      QEvent_LayoutRequest                     => "QEvent"      ,;
      QEvent_UpdateRequest                     => "QEvent"      ,;
      QEvent_UpdateLater                       => "QEvent"      ,;
      QEvent_ContextMenu                       => "QContextMenuEvent" ,;
      QEvent_InputMethod                       => "QInputMethodEvent" ,;
      QEvent_AccessibilityPrepare              => "QEvent"      ,;
      QEvent_TabletMove                        => "QEvent"      ,;
      QEvent_LocaleChange                      => "QEvent"      ,;
      QEvent_LanguageChange                    => "QEvent"      ,;
      QEvent_LayoutDirectionChange             => "QEvent"      ,;
      QEvent_TabletPress                       => "QEvent"      ,;
      QEvent_TabletRelease                     => "QEvent"      ,;
      QEvent_OkRequest                         => "QEvent"      ,;
      QEvent_IconDrag                          => "QEvent"      ,;
      QEvent_FontChange                        => "QEvent"      ,;
      QEvent_EnabledChange                     => "QEvent"      ,;
      QEvent_ActivationChange                  => "QEvent"      ,;
      QEvent_StyleChange                       => "QEvent"      ,;
      QEvent_IconTextChange                    => "QEvent"      ,;
      QEvent_ModifiedChange                    => "QEvent"      ,;
      QEvent_WindowBlocked                     => "QEvent"      ,;
      QEvent_WindowUnblocked                   => "QEvent"      ,;
      QEvent_WindowStateChange                 => "QWindowStateChangeEvent" ,;
      QEvent_MouseTrackingChange               => "QEvent"      ,;
      QEvent_ToolTip                           => "QHelpEvent"  ,;
      QEvent_WhatsThis                         => "QHelpEvent"  ,;
      QEvent_StatusTip                         => "QEvent"      ,;
      QEvent_ActionChanged                     => "QEvent"      ,;
      QEvent_ActionAdded                       => "QEvent"      ,;
      QEvent_ActionRemoved                     => "QEvent"      ,;
      QEvent_FileOpen                          => "QEvent"      ,;
      QEvent_Shortcut                          => "QEvent"      ,;
      QEvent_WhatsThisClicked                  => "QEvent"      ,;
      QEvent_AccessibilityHelp                 => "QEvent"      ,;
      QEvent_ToolBarChange                     => "QEvent"      ,;
      QEvent_ApplicationActivate               => "QEvent"      ,;
      QEvent_ApplicationActivated              => "QEvent"      ,;
      QEvent_ApplicationDeactivate             => "QEvent"      ,;
      QEvent_QueryWhatsThis                    => "QEvent"      ,;
      QEvent_EnterWhatsThisMode                => "QEvent"      ,;
      QEvent_LeaveWhatsThisMode                => "QEvent"      ,;
      QEvent_ZOrderChange                      => "QEvent"      ,;
      QEvent_HoverEnter                        => "QEvent"      ,;
      QEvent_HoverLeave                        => "QEvent"      ,;
      QEvent_HoverMove                         => "QEvent"      ,;
      QEvent_AccessibilityDescription          => "QEvent"      ,;
      QEvent_ParentAboutToChange               => "QEvent"      ,;
      QEvent_WinEventAct                       => "QEvent"      ,;
      QEvent_EnterEditFocus                    => "QEvent"      ,;
      QEvent_LeaveEditFocus                    => "QEvent"      ,;
      QEvent_MenubarUpdated                    => "QEvent"      ,;
      QEvent_GraphicsSceneMouseMove            => "QGraphicsSceneMouseEvent"       ,;
      QEvent_GraphicsSceneMousePress           => "QGraphicsSceneMouseEvent"       ,;
      QEvent_GraphicsSceneMouseRelease         => "QGraphicsSceneMouseEvent"       ,;
      QEvent_GraphicsSceneMouseDoubleClick     => "QGraphicsSceneMouseEvent"       ,;
      QEvent_GraphicsSceneContextMenu          => "QGraphicsSceneContextMenuEvent" ,;
      QEvent_GraphicsSceneHoverEnter           => "QGraphicsSceneHoverEvent"       ,;
      QEvent_GraphicsSceneHoverMove            => "QGraphicsSceneHoverEvent"       ,;
      QEvent_GraphicsSceneHoverLeave           => "QGraphicsSceneHoverEvent"       ,;
      QEvent_GraphicsSceneHelp                 => "QHelpEvent"  ,;
      QEvent_GraphicsSceneDragEnter            => "QGraphicsSceneDragDropEvent"    ,;
      QEvent_GraphicsSceneDragMove             => "QGraphicsSceneDragDropEvent"    ,;
      QEvent_GraphicsSceneDragLeave            => "QGraphicsSceneDragDropEvent"    ,;
      QEvent_GraphicsSceneDrop                 => "QGraphicsSceneDragDropEvent"    ,;
      QEvent_GraphicsSceneWheel                => "QGraphicsSceneWheelEvent"       ,;
      QEvent_KeyboardLayoutChange              => "QEvent"      ,;
      QEvent_DynamicPropertyChange             => "QEvent"      ,;
      QEvent_TabletEnterProximity              => "QEvent"      ,;
      QEvent_TabletLeaveProximity              => "QEvent"      ,;
      QEvent_NonClientAreaMouseMove            => "QEvent"      ,;
      QEvent_NonClientAreaMouseButtonPress     => "QEvent"      ,;
      QEvent_NonClientAreaMouseButtonRelease   => "QEvent"      ,;
      QEvent_NonClientAreaMouseButtonDblClick  => "QEvent"      ,;
      QEvent_MacSizeChange                     => "QEvent"      ,;
      QEvent_ContentsRectChange                => "QEvent"      ,;
      QEvent_GraphicsSceneResize               => "QGraphicsSceneResizeEvent" ,;
      QEvent_GraphicsSceneMove                 => "QGraphicsSceneMoveEvent"   ,;
      QEvent_CursorChange                      => "QEvent"      ,;
      QEvent_ToolTipChange                     => "QEvent"      ,;
      QEvent_GrabMouse                         => "QEvent"      ,;
      QEvent_UngrabMouse                       => "QEvent"      ,;
      QEvent_GrabKeyboard                      => "QEvent"      ,;
      QEvent_UngrabKeyboard                    => "QEvent"      }

   /* TODO: Change macro expansion to more efficient solution */
   LOCAL qObj := Eval( hb_MacroBlock( "HB_" + iif( hb_HHasKey( s_b_, nType ), s_b_[ nType ], "QEvent" ) + "()" ) )

   qObj:pPtr := pPtr

   RETURN qObj

/*----------------------------------------------------------------------*/
