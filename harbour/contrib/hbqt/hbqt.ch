/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                             QT Headers
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              10Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#ifndef _HBQT_CH


#define QEvent_None                                0        // Not an event.
#define QEvent_Timer                               1        // Regular timer events (QTimerEvent).
#define QEvent_MouseButtonPress                    2        // Mouse press (QMouseEvent).
#define QEvent_MouseButtonRelease                  3        // Mouse release (QMouseEvent).
#define QEvent_MouseButtonDblClick                 4        // Mouse press again (QMouseEvent).
#define QEvent_MouseMove                           5        // Mouse move (QMouseEvent).
#define QEvent_KeyPress                            6        // Key press (QKeyEvent).
#define QEvent_KeyRelease                          7        // Key release (QKeyEvent).
#define QEvent_FocusIn                             8        // Widget gains keyboard focus (QFocusEvent).
#define QEvent_FocusOut                            9        // Widget loses keyboard focus (QFocusEvent).
#define QEvent_Enter                               10       // Mouse enters widget's boundaries.
#define QEvent_Leave                               11       // Mouse leaves widget's boundaries.
#define QEvent_Paint                               12       // Screen update necessary (QPaintEvent).
#define QEvent_Move                                13       // Widget's position changed (QMoveEvent).
#define QEvent_Resize                              14       // Widget's size changed (QResizeEvent).
#define QEvent_Show                                17       // Widget was shown on screen (QShowEvent).
#define QEvent_Hide                                18       // Widget was hidden (QHideEvent).
#define QEvent_Close                               19       // Widget was closed (QCloseEvent).
#define QEvent_ParentChange                        21       // The widget parent has changed.
#define QEvent_WindowActivate                      24       // Window was activated.
#define QEvent_WindowDeactivate                    25       // Window was deactivated.
#define QEvent_ShowToParent                        26       // A child widget has been shown.
#define QEvent_HideToParent                        27       // A child widget has been hidden.
#define QEvent_Wheel                               31       // Mouse wheel rolled (QWheelEvent).
#define QEvent_WindowTitleChange                   33       // The window title has changed.
#define QEvent_WindowIconChange                    34       // The window's icon has changed.
#define QEvent_ApplicationWindowIconChange         35       // The application's icon has changed.
#define QEvent_ApplicationFontChange               36       // The default application font has changed.
#define QEvent_ApplicationLayoutDirectionChange    37       // The default application layout direction has changed.
#define QEvent_ApplicationPaletteChange            38       // The default application palette has changed.
#define QEvent_PaletteChange                       39       // Palette of the widget changed.
#define QEvent_Clipboard                           40       // The clipboard contents have changed (QClipboardEvent).
#define QEvent_MetaCall                            43       // An asynchronous method invocation via QMetaObject_invokeMethod().
#define QEvent_SockAct                             50       // Socket activated, used to implement QSocketNotifier.
#define QEvent_ShortcutOverride                    51       // Key press in child, for overriding shortcut key handling (QKeyEvent).
#define QEvent_DeferredDelete                      52       // The object will be deleted after it has cleaned up.
#define QEvent_DragEnter                           60       // The cursor enters a widget during a drag and drop operation (QDragEnterEvent).
#define QEvent_DragLeave                           62       // The cursor leaves a widget during a drag and drop operation (QDragLeaveEvent).
#define QEvent_DragMove                            61       // A drag and drop operation is in progress (QDragMoveEvent).
#define QEvent_Drop                                63       // A drag and drop operation is completed (QDropEvent).
#define QEvent_ChildAdded                          68       // An object gets a child (QChildEvent).
#define QEvent_ChildPolished                       69       // A widget child gets polished (QChildEvent).
#define QEvent_ChildInserted                       70       // An object gets a child (QChildEvent). Qt3Support only, use ChildAdded instead.
#define QEvent_ChildRemoved                        71       // An object loses a child (QChildEvent).
#define QEvent_PolishRequest                       74       // The widget should be polished.
#define QEvent_Polish                              75       // The widget is polished.
#define QEvent_LayoutRequest                       76       // Widget layout needs to be redone.
#define QEvent_UpdateRequest                       77       // The widget should be repainted.
#define QEvent_UpdateLater                         78       // The widget should be queued to be repainted at a later time.
#define QEvent_ContextMenu                         82       // Context popup menu (QContextMenuEvent).
#define QEvent_InputMethod                         83       // An input method is being used (QInputMethodEvent).
#define QEvent_AccessibilityPrepare                86       // Accessibility information is requested.
#define QEvent_TabletMove                          87       // Wacom tablet move (QTabletEvent).
#define QEvent_LocaleChange                        88       // The system locale has changed.
#define QEvent_LanguageChange                      89       // The application translation changed.
#define QEvent_LayoutDirectionChange               90       // The direction of layouts changed.
#define QEvent_TabletPress                         92       // Wacom tablet press (QTabletEvent).
#define QEvent_TabletRelease                       93       // Wacom tablet release (QTabletEvent).
#define QEvent_OkRequest                           94       // Ok button in decoration pressed. Supported only for Windows CE.
#define QEvent_IconDrag                            96       // The main icon of a window has been dragged away (QIconDragEvent).
#define QEvent_FontChange                          97       // Widget's font has changed.
#define QEvent_EnabledChange                       98       // Widget's enabled state has changed.
#define QEvent_ActivationChange                    99       // A widget's top-level window activation state has changed.
#define QEvent_StyleChange                         100      // Widget's style has been changed.
#define QEvent_IconTextChange                      101      // Widget's icon text has been changed.
#define QEvent_ModifiedChange                      102      // Widgets modification state has been changed.
#define QEvent_WindowBlocked                       103      // The window is blocked by a modal dialog.
#define QEvent_WindowUnblocked                     104      // The window is unblocked after a modal dialog exited.
#define QEvent_WindowStateChange                   105      // The window's state (minimized, maximized or full-screen) has changed (QWindowStateChangeEvent).
#define QEvent_MouseTrackingChange                 109      // The mouse tracking state has changed.
#define QEvent_ToolTip                             110      // A tooltip was requested (QHelpEvent).
#define QEvent_WhatsThis                           111      // The widget should reveal "What's This?" help (QHelpEvent).
#define QEvent_StatusTip                           112      // A status tip is requested (QStatusTipEvent).
#define QEvent_ActionChanged                       113      // An action has been changed (QActionEvent).
#define QEvent_ActionAdded                         114      // A new action has been added (QActionEvent).
#define QEvent_ActionRemoved                       115      // An action has been removed (QActionEvent).
#define QEvent_FileOpen                            116      // File open request (QFileOpenEvent).
#define QEvent_Shortcut                            117      // Key press in child for shortcut key handling (QShortcutEvent).
#define QEvent_WhatsThisClicked                    118      // A link in a widget's "What's This?" help was clicked.
#define QEvent_AccessibilityHelp                   119      // Used to query accessibility help texts (QAccessibleEvent).
#define QEvent_ToolBarChange                       120      // The toolbar button is toggled on Mac OS X.
#define QEvent_ApplicationActivate                 121      // The application has been made available to the user.
#define QEvent_ApplicationActivated                121      // This enum has been deprecated. Use ApplicationActivate instead.
#define QEvent_ApplicationDeactivate               122      // The application has been suspended, and is unavailable to the user.
#define QEvent_QueryWhatsThis                      123      // The widget should accept the event if it has "What's This?" help.
#define QEvent_EnterWhatsThisMode                  124      // Send to toplevel widgets when the application enters "What's This?" mode.
#define QEvent_LeaveWhatsThisMode                  125      // Send to toplevel widgets when the application leaves "What's This?" mode.
#define QEvent_ZOrderChange                        126      // The widget's z-order has changed. This event is never sent to top level windows.
#define QEvent_HoverEnter                          127      // The mouse cursor enters a hover widget (QHoverEvent).
#define QEvent_HoverLeave                          128      // The mouse cursor leaves a hover widget (QHoverEvent).
#define QEvent_HoverMove                           129      // The mouse cursor moves inside a hover widget (QHoverEvent).
#define QEvent_AccessibilityDescription            130      // Used to query accessibility description texts (QAccessibleEvent).
#define QEvent_ParentAboutToChange                 131      // The widget parent is about to change.
#define QEvent_WinEventAct                         132      // A Windows-specific activation event has occurred.
#define QEvent_EnterEditFocus                      150      // An editor widget gains focus for editing.
#define QEvent_LeaveEditFocus                      151      // An editor widget loses focus for editing.
#define QEvent_MenubarUpdated                      153      // The window's menu bar has been updated.
#define QEvent_GraphicsSceneMouseMove              155      // Move mouse in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneMousePress             156      // Mouse press in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneMouseRelease           157      // Mouse release in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneMouseDoubleClick       158      // Mouse press again (double click) in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneContextMenu            159      // Context popup menu over a graphics scene (QGraphicsSceneContextMenuEvent).
#define QEvent_GraphicsSceneHoverEnter             160      // The mouse cursor enters a hover item in a graphics scene (QGraphicsSceneHoverEvent).
#define QEvent_GraphicsSceneHoverMove              161      // The mouse cursor moves inside a hover item in a graphics scene (QGraphicsSceneHoverEvent).
#define QEvent_GraphicsSceneHoverLeave             162      // The mouse cursor leaves a hover item in a graphics scene (QGraphicsSceneHoverEvent).
#define QEvent_GraphicsSceneHelp                   163      // The user requests help for a graphics scene (QHelpEvent).
#define QEvent_GraphicsSceneDragEnter              164      // The cursor enters a graphics scene during a drag and drop operation.
#define QEvent_GraphicsSceneDragMove               165      // A drag and drop operation is in progress over a scene.
#define QEvent_GraphicsSceneDragLeave              166      // The cursor leaves a graphics scene during a drag and drop operation.
#define QEvent_GraphicsSceneDrop                   167      // A drag and drop operation is completed over a scene.
#define QEvent_GraphicsSceneWheel                  168      // Mouse wheel rolled in a graphics scene (QGraphicsSceneWheelEvent).
#define QEvent_KeyboardLayoutChange                169      // The keyboard layout has changed.
#define QEvent_DynamicPropertyChange               170      // A dynamic property was added, changed or removed from the object. User events should have values b
#define QEvent_TabletEnterProximity                171      // Wacom tablet enter proximity event (QTabletEvent), sent to QApplication.
#define QEvent_TabletLeaveProximity                172      // Wacom tablet leave proximity event (QTabletEvent), sent to QApplication.
#define QEvent_NonClientAreaMouseMove              173      // A mouse move occurred outside the client area.
#define QEvent_NonClientAreaMouseButtonPress       174      // A mouse button press occurred outside the client area.
#define QEvent_NonClientAreaMouseButtonRelease     175      // A mouse button release occurred outside the client area.
#define QEvent_NonClientAreaMouseButtonDblClick    176      // A mouse double click occurred outside the client area.
#define QEvent_MacSizeChange                       177      // The user changed his widget sizes (Mac OS X only).
#define QEvent_ContentsRectChange                  178      // The margins of the widget's content rect changed.
#define QEvent_GraphicsSceneResize                 181      // Widget was resized (QGraphicsSceneResizeEvent).
#define QEvent_GraphicsSceneMove                   182      // Widget was moved (QGraphicsSceneMoveEvent).
#define QEvent_CursorChange                        183      // The widget's cursor has changed.
#define QEvent_ToolTipChange                       184      // The widget's tooltip has changed.
#define QEvent_GrabMouse                           186      // Item gains mouse grab (QGraphicsItem only).
#define QEvent_UngrabMouse                         187      // Item loses mouse grab (QGraphicsItem only).
#define QEvent_GrabKeyboard                        188      // Item gains keyboard grab (QGraphicsItem only).
#define QEvent_UngrabKeyboard                      189      // Item loses keyboard grab (QGraphicsItem only).

#define QPalette_Window                            10       // A general background color.
#define QPalette_Background                        10       // This value is obsolete. Use Window instead.
#define QPalette_WindowText                        0        // A general foreground color.
#define QPalette_Foreground                        0        // This value is obsolete. Use WindowText instead.
#define QPalette_Base                              9        // Used mostly as the background color for text entry widgets, but can also be used for other painting - such as the background of combobox drop down lists and toolbar handles. It is usually white or another light color.
#define QPalette_AlternateBase                     16       // Used as the alternate background color in views with alternating row colors (see QAbstractItemView_setAlternatingRowColors()).
#define QPalette_ToolTipBase                       18       // Used as the background color for QToolTip and QWhatsThis. Tool tips use the Inactive color group of QPalette, because tool tips are not active windows.
#define QPalette_ToolTipText                       19       // Used as the foreground color for QToolTip and QWhatsThis. Tool tips use the Inactive color group of QPalette, because tool tips are not active windows.
#define QPalette_Text                              6        // The foreground color used with Base. This is usually the same as the WindowText, in which case it must provide good contrast with Window and Base.
#define QPalette_Button                            1        // The general button background color. This background can be different from Window as some styles require a different background color for buttons.
#define QPalette_ButtonText                        8        // A foreground color used with the Button color.
#define QPalette_BrightText                        7        // A text color that is very different from WindowText, and contrasts well with e.g. Dark. Typically used for text that needs to be drawn where Text or WindowText would give poor contrast, such as on pressed push buttons. Note that text colors can be used for things other than just words; text colors are usually used for text, but it's quite common to use the text color roles for lines, icons, etc.
/*
 * There are some color roles used mostly for 3D bevel and shadow effects.
 * All of these are normally derived from Window, and used in ways that depend on that
 * relationship. For example, buttons depend on it to make the bevels look attractive,
 * and Motif scroll bars depend on Mid to be slightly different from Window.
 */
#define QPalette_Light                             2        // Lighter than Button color.
#define QPalette_Midlight                          3        // Between Button and Light.
#define QPalette_Dark                              4        // Darker than Button.
#define QPalette_Mid                               5        // Between Button and Dark.
#define QPalette_Shadow                            11       // A very dark color. By default, the shadow color is Qt_black.
#define QPalette_Highlight                         12       // A color to indicate a selected item or the current item. By default, the highlight color is Qt_darkBlue.
#define QPalette_HighlightedText                   13       // A text color that contrasts with Highlight. By default, the highlighted text color is Qt_white.
#define QPalette_Link                              14       // A text color used for unvisited hyperlinks. By default, the link color is Qt_blue.
#define QPalette_LinkVisited                       15       // A text color used for already visited hyperlinks. By default, the linkvisited color is Qt_magenta.
#define QPalette_NoRole                            17       // No role; this special role is often used to indicate that a role has not been assigned.


#define QAbstractSlider_SliderNoAction             0
#define QAbstractSlider_SliderSingleStepAdd        1
#define QAbstractSlider_SliderSingleStepSub        2
#define QAbstractSlider_SliderPageStepAdd          3
#define QAbstractSlider_SliderPageStepSub          4
#define QAbstractSlider_SliderToMinimum            5
#define QAbstractSlider_SliderToMaximum            6
#define QAbstractSlider_SliderMove                 7

#define QAbstractSlider_SliderRangeChange          0
#define QAbstractSlider_SliderOrientationChange    1
#define QAbstractSlider_SliderStepsChange          2
#define QAbstractSlider_SliderValueChange          3


#define Qt_AlignLeft                              1         // 0x0001  // Aligns with the left edge.
#define Qt_AlignRight                             2         // 0x0002  // Aligns with the right edge.
#define Qt_AlignHCenter                           4         // 0x0004  // Centers horizontally in the available space.
#define Qt_AlignJustify                           8         // 0x0008  // Justifies the text in the available space.

#define Qt_AlignTop                               32        // 0x0020  // Aligns with the top.
#define Qt_AlignBottom                            64        // 0x0040  // Aligns with the bottom.
#define Qt_AlignVCenter                           128       // 0x0080  // Centers vertically in the available space.

#define Qt_AlignCenter                            AlignVCenter+AlignHCenter   // Centers in both dimensions.

#define Qt_AlignAbsolute                          16        // 0x0010
           // If the widget's layout
           // direction is Qt_RightToLeft (instead of Qt_LeftToRight, the default),
           // Qt_AlignLeft refers to the right edge and Qt_AlignRight to the left edge.
           // This is normally the desired behavior. If you want Qt_AlignLeft to always mean
           // "left" and Qt_AlignRight to always mean "right", combine the flag with Qt_AlignAbsolute.
#define Qt_AlignLeading                           AlignLeft // Synonym for Qt_AlignLeft.
#define Qt_AlignTrailing                          AlignRight// Synonym for Qt_AlignRight.

#define Qt_AlignHorizontal_Mask                   AlignLeft + AlignRight + AlignHCenter + AlignJustify + AlignAbsolute
#define Qt_AlignVertical_Mask                     AlignTop + AlignBottom + AlignVCenter



#define QLineEdit_Normal                          0        // Display characters as they are entered. This is the default.
#define QLineEdit_NoEcho                          1        // Do not display anything. This may be appropriate for passwords where even the length of the password should be kept secret.
#define QLineEdit_Password                        2        // Display asterisks instead of the characters actually entered.
#define QLineEdit_PasswordEchoOnEdit              3

/*----------------------------------------------------------------------*/

#define _HBQT_CH
#endif