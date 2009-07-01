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

#define QPalette_WindowText                        0        // A general foreground color.
#define QPalette_Foreground                        0        // This value is obsolete. Use WindowText instead.
#define QPalette_Button                            1        // The general button background color. This background can be different from Window as some styles require a different background color for buttons.
#define QPalette_Text                              6        // The foreground color used with Base. This is usually the same as the WindowText, in which case it must provide good contrast with Window and Base.
#define QPalette_BrightText                        7        // A text color that is very different from WindowText, and contrasts well with e.g. Dark. Typically used for text that needs to be drawn where Text or WindowText would give poor contrast, such as on pressed push buttons. Note that text colors can be used for things other than just words; text colors are usually used for text, but it's quite common to use the text color roles for lines, icons, etc.
#define QPalette_ButtonText                        8        // A foreground color used with the Button color.
#define QPalette_Base                              9        // Used mostly as the background color for text entry widgets, but can also be used for other painting - such as the background of combobox drop down lists and toolbar handles. It is usually white or another light color.
#define QPalette_Window                            10       // A general background color.
#define QPalette_Background                        10       // This value is obsolete. Use Window instead.
#define QPalette_AlternateBase                     16       // Used as the alternate background color in views with alternating row colors (see QAbstractItemView_setAlternatingRowColors()).
#define QPalette_ToolTipBase                       18       // Used as the background color for QToolTip and QWhatsThis. Tool tips use the Inactive color group of QPalette, because tool tips are not active windows.
#define QPalette_ToolTipText                       19       // Used as the foreground color for QToolTip and QWhatsThis. Tool tips use the Inactive color group of QPalette, because tool tips are not active windows.

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

#define QLineEdit_Normal                          0        // Display characters as they are entered. This is the default.
#define QLineEdit_NoEcho                          1        // Do not display anything. This may be appropriate for passwords where even the length of the password should be kept secret.
#define QLineEdit_Password                        2        // Display asterisks instead of the characters actually entered.
#define QLineEdit_PasswordEchoOnEdit              3

#define QMessageBox_InvalidRole                   -1        // The button is invalid.
#define QMessageBox_AcceptRole                    0         // Clicking the button causes the dialog to be accepted (e.g. OK).
#define QMessageBox_RejectRole                    1         // Clicking the button causes the dialog to be rejected (e.g. Cancel).
#define QMessageBox_DestructiveRole               2         // Clicking the button causes a destructive change (e.g. for Discarding Changes) and closes the dialog.
#define QMessageBox_ActionRole                    3         // Clicking the button causes changes to the elements within the dialog.
#define QMessageBox_HelpRole                      4         // The button can be clicked to request help.
#define QMessageBox_YesRole                       5         // The button is a "Yes"-like button.
#define QMessageBox_NoRole                        6         // The button is a "No"-like button.
#define QMessageBox_ApplyRole                     8         // The button applies current changes.
#define QMessageBox_ResetRole                     7         // The button resets the dialog's fields to default values.
                                                            //
#define QMessageBox_NoIcon                        0         // the message box does not have any icon.
#define QMessageBox_Question                      4         // an icon indicating that the message is asking a question.
#define QMessageBox_Information                   1         // an icon indicating that the message is nothing out of the ordinary.
#define QMessageBox_Warning                       2         // an icon indicating that the message is a warning, but can be dealt with.
#define QMessageBox_Critical                      3         // an icon indicating that the message represents a critical problem.

#define QMessageBox_Ok                            0x00000400   // An "OK" button defined with the AcceptRole.
#define QMessageBox_Open                          0x00002000   // A "Open" button defined with the AcceptRole.
#define QMessageBox_Save                          0x00000800   // A "Save" button defined with the AcceptRole.
#define QMessageBox_Cancel                        0x00400000   // A "Cancel" button defined with the RejectRole.
#define QMessageBox_Close                         0x00200000   // A "Close" button defined with the RejectRole.
#define QMessageBox_Discard                       0x00800000   // A "Discard" or "Don't Save" button, depending on the platform, defined with the DestructiveRole.
#define QMessageBox_Apply                         0x02000000   // An "Apply" button defined with the ApplyRole.
#define QMessageBox_Reset                         0x04000000   // A "Reset" button defined with the ResetRole.
#define QMessageBox_RestoreDefaults               0x08000000   // A "Restore Defaults" button defined with the ResetRole.
#define QMessageBox_Help                          0x01000000   // A "Help" button defined with the HelpRole.
#define QMessageBox_SaveAll                       0x00001000   // A "Save All" button defined with the AcceptRole.
#define QMessageBox_Yes                           0x00004000   // A "Yes" button defined with the YesRole.
#define QMessageBox_YesToAll                      0x00008000   // A "Yes to All" button defined with the YesRole.
#define QMessageBox_No                            0x00010000   // A "No" button defined with the NoRole.
#define QMessageBox_NoToAll                       0x00020000   // A "No to All" button defined with the NoRole.
#define QMessageBox_Abort                         0x00040000   // An "Abort" button defined with the RejectRole.
#define QMessageBox_Retry                         0x00080000   // A "Retry" button defined with the AcceptRole.
#define QMessageBox_Ignore                        0x00100000   // An "Ignore" button defined with the AcceptRole.
#define QMessageBox_NoButton                      0x00000000   // An invalid button.

#define Qt_AlignLeft                              0x0001      // Aligns with the left edge.
#define Qt_AlignRight                             0x0002      // Aligns with the right edge.
#define Qt_AlignHCenter                           0x0004      // Centers horizontally in the available space.
#define Qt_AlignJustify                           0x0008      // Justifies the text in the available space.
                                                              //
#define Qt_AlignTop                               0x0020      // Aligns with the top.
#define Qt_AlignBottom                            0x0040      // Aligns with the bottom.
#define Qt_AlignVCenter                           0x0080      // Centers vertically in the available space.

#define Qt_AlignCenter                            Qt_AlignVCenter + Qt_AlignHCenter   // Centers in both dimensions.

#define Qt_AlignAbsolute                          0x0010      // If the widget's layout direction is #define Qt_RightToLeft (instead of #define Qt_LeftToRight, the default), #define Qt_AlignLeft refers to the right edge and #define Qt_AlignRight to the left edge. This is normally the desired behavior. If you want #define Qt_AlignLeft to always mean "left" and #define Qt_AlignRight to always mean "right", combine the flag with #define Qt_AlignAbsolute.
#define Qt_AlignLeading                           Qt_AlignLeft   // Synonym for #define Qt_AlignLeft.
#define Qt_AlignTrailing                          Qt_AlignRight  // Synonym for #define Qt_AlignRight.

#define Qt_AlignHorizontal_Mask                   Qt_AlignLeft + Qt_AlignRight + Qt_AlignHCenter + Qt_AlignJustify + Qt_AlignAbsolute
#define Qt_AlignVertical_Mask                     Qt_AlignTop + Qt_AlignBottom + Qt_AlignVCenter

#define Qt_AnchorName                             0     // the name attribute of the anchor. This attribute is used when scrolling to an anchor in the document.
#define Qt_AnchorHref                             1     // the href attribute of the anchor. This attribute is used when a link is clicked to determine what content to load.

#define Qt_AA_ImmediateWidgetCreation             0     // Ensures that widgets are created as soon as they are constructed. By default, resources for widgets are allocated on demand to improve efficiency and minimize resource usage. Setting or clearing this attribute affects widgets constructed after the change. Setting it tells Qt to create toplevel windows immediately. Therefore, if it is important to minimize resource consumption, do not set this attribute.
#define Qt_AA_MSWindowsUseDirect3DByDefault       1     // Is a Windows specific attribute, that will make the Direct3D paint engine the default Qt widget paint engine. Note that you can toggle usage of the Direct3D engine on individual QWidgets by setting/clearing the WA_MSWindowsUseDirect3D attribute on a specific widget. This functionality is experimental.
#define Qt_AA_DontShowIconsInMenus                2     // Actions with the Icon property won't be shown in any menus unless specifically set by the QAction_iconVisibleInMenu property.
// Menus that are currently open or menus already created in the native Mac OS X menubar MAY NOT pick up a change in this attribute. Changes in the QAction_iconVisibleInMenu property will always be picked up.

#define Qt_AA_NativeWindows                       3     // Ensures that widgets have native windows.
#define Qt_AA_DontCreateNativeWidgetSiblings      4     // Ensures that siblings of native widgets stay non-native unless specifically set by the #define Qt_WA_NativeWindow attribute.
#define Qt_AA_MacPluginApplication                5     // Stops the a Qt mac application from doing specific initializations that do not necessarily make sense when using Qt to author a plugin. This includes avoiding loading our nib for the main menu and not taking possession of the native menu bar.

#define Qt_NoArrow                                0
#define Qt_UpArrow                                1
#define Qt_DownArrow                              2
#define Qt_LeftArrow                              3
#define Qt_RightArrow                             4

// enum #define Qt_AspectRatioMode
// This enum type defines what happens to the aspect ratio when scaling an rectangle.
//
#define Qt_IgnoreAspectRatio                      0     // The size is scaled freely. The aspect ratio is not preserved.
#define Qt_KeepAspectRatio                        1     // The size is scaled to a rectangle as large as possible inside a given rectangle, preserving the aspect ratio.
#define Qt_KeepAspectRatioByExpanding             2     // The size is scaled to a rectangle as small as possible outside a given rectangle, preserving the aspect ratio.
// See also QSize_scale() and QImage_scaled().

// enum #define Qt_Axis
// This enum type defines three values to represent the three axes in the cartesian coordinate system.
#define Qt_XAxis                                  0     // The X axis.
#define Qt_YAxis                                  1     // The Y axis.
#define Qt_ZAxis                                  2     // The Z axis.
// See also QTransform_rotate() and QTransform_rotateRadians().

// enum #define Qt_BGMode - Background mode:
#define Qt_TransparentMode                        0
#define Qt_OpaqueMode                             1

//enum #define Qt_BrushStyle
//This enum type defines the brush styles supported by Qt, i.e. the fill pattern of shapes drawn using QPainter.
//
#define Qt_NoBrush                                0     // No brush pattern.
#define Qt_SolidPattern                           1     // Uniform color.
#define Qt_Dense1Pattern                          2     // Extremely dense brush pattern.
#define Qt_Dense2Pattern                          3     // Very dense brush pattern.
#define Qt_Dense3Pattern                          4     // Somewhat dense brush pattern.
#define Qt_Dense4Pattern                          5     // Half dense brush pattern.
#define Qt_Dense5Pattern                          6     // Somewhat sparse brush pattern.
#define Qt_Dense6Pattern                          7     // Very sparse brush pattern.
#define Qt_Dense7Pattern                          8     // Extremely sparse brush pattern.
#define Qt_HorPattern                             9     // Horizontal lines.
#define Qt_VerPattern                             10    // Vertical lines.
#define Qt_CrossPattern                           11    // Crossing horizontal and vertical lines.
#define Qt_BDiagPattern                           12    // Backward diagonal lines.
#define Qt_FDiagPattern                           13    // Forward diagonal lines.
#define Qt_DiagCrossPattern                       14    // Crossing diagonal lines.
#define Qt_LinearGradientPattern                  15    // Linear gradient (set using a dedicated QBrush constructor).
#define Qt_ConicalGradientPattern                 17    // Conical gradient (set using a dedicated QBrush constructor).
#define Qt_RadialGradientPattern                  16    // Radial gradient (set using a dedicated QBrush constructor).
#define Qt_TexturePattern                         24    // Custom pattern (see QBrush_setTexture()).

// enum #define Qt_CaseSensitivity
//
#define Qt_CaseInsensitive                        0
#define Qt_CaseSensitive                          1

// enum #define Qt_CheckState
// This enum describes the state of checkable items, controls, and widgets.
//
#define Qt_Unchecked                              0     // The item is unchecked.
#define Qt_PartiallyChecked                       1     // The item is partially checked. Items in hierarchical models may be partially checked if some, but not all, of their children are checked.
#define Qt_Checked                                2     // The item is checked.
// See also QCheckBox, #define Qt_ItemFlags, and #define Qt_ItemDataRole.

// enum #define Qt_ClipOperation
//
#define Qt_NoClip                                 0     // This operation turns clipping off.
#define Qt_ReplaceClip                            1     // Replaces the current clip path/rect/region with the one supplied in the function call.
#define Qt_IntersectClip                          2     // Intersects the current clip path/rect/region with the one supplied in the function call.
#define Qt_UniteClip                              3     // Unites the current clip path/rect/region with the one supplied in the function call.

// enum #define Qt_ConnectionType
// This enum describes the types of connection that can be used between signals and slots. In particular, it determines whether a particular signal is delivered to a slot immediately or queued for delivery at a later time.
//
#define Qt_DirectConnection                       1     // When emitted, the signal is immediately delivered to the slot.
#define Qt_QueuedConnection                       2     // When emitted, the signal is queued until the event loop is able to deliver it to the slot.
#define Qt_BlockingQueuedConnection               4     // Same as QueuedConnection, except that the current thread blocks until the slot has been delivered. This connection type should only be used for receivers in a different thread. Note that misuse of this type can lead to dead locks in your application.
#define Qt_AutoConnection                         0     // If the signal is emitted from the thread in which the receiving object lives, the slot is invoked directly, as with #define Qt_DirectConnection; otherwise the signal is queued, as with #define Qt_QueuedConnection.
// With queued connections, the parameters must be of types that are known to Qt's meta-object system, because Qt needs to copy the arguments to store them in an event behind the scenes. If you try to use a queued connection and get the error message
// QObject_connect: Cannot queue arguments of type 'MyType'
// call qRegisterMetaType() to register the data type before you establish the connection.
// See also Thread Support in Qt, QObject_connect(), and qRegisterMetaType().

// enum #define Qt_ContextMenuPolicy
// This enum type defines the various policies a widget can have with respect to showing a context menu.
//
#define Qt_NoContextMenu                          0     // the widget does not feature a context menu, context menu handling is deferred to the widget's parent.
#define Qt_PreventContextMenu                     4     // the widget does not feature a context menu, and in contrast to NoContextMenu, the handling is not deferred to the widget's parent. This means that all right mouse button events are guaranteed to be delivered to the widget itself through mousePressEvent(), and mouseReleaseEvent().
#define Qt_DefaultContextMenu                     1     // the widget's QWidget_contextMenuEvent() handler is called.
#define Qt_ActionsContextMenu                     2     // the widget displays its QWidget_actions() as context menu.
#define Qt_CustomContextMenu                      3     // the widget emits the QWidget_customContextMenuRequested() signal.

// enum #define Qt_Corner
// This enum type specifies a corner in a rectangle:
//
#define Qt_TopLeftCorner                          0x00000   // The top-left corner of the rectangle.
#define Qt_TopRightCorner                         0x00001   // The top-right corner of the rectangle.
#define Qt_BottomLeftCorner                       0x00002   // The bottom-left corner of the rectangle.
#define Qt_BottomRightCorner                      0x00003   // The bottom-right corner of the rectangle.

// enum #define Qt_CursorShape
// This enum type defines the various cursors that can be used.
// The standard arrow cursor is the default for widgets in a normal state.
//
#define Qt_ArrowCursor                            0     // The standard arrow cursor.
#define Qt_UpArrowCursor                          1     // An arrow pointing upwards toward the top of the screen.
#define Qt_CrossCursor                            2     // A crosshair cursor, typically used to help the user accurately select a point on the screen.
#define Qt_WaitCursor                             3     // An hourglass or watch cursor, usually shown during operations that prevent the user from interacting with the application.
#define Qt_IBeamCursor                            4     // A caret or ibeam cursor, indicating that a widget can accept and display text input.
#define Qt_SizeVerCursor                          5     // A cursor used for elements that are used to vertically resize top-level windows.
#define Qt_SizeHorCursor                          6     // A cursor used for elements that are used to horizontally resize top-level windows.
#define Qt_SizeBDiagCursor                        7     // A cursor used for elements that are used to diagonally resize top-level windows at their top-right and bottom-left corners.
#define Qt_SizeFDiagCursor                        8     // A cursor used for elements that are used to diagonally resize top-level windows at their top-left and bottom-right corners.
#define Qt_SizeAllCursor                          9     // A cursor used for elements that are used to resize top-level windows in any direction.
#define Qt_BlankCursor                            10    // A blank/invisible cursor, typically used when the cursor shape needs to be hidden.
#define Qt_SplitVCursor                           11    // A cursor used for vertical splitters, indicating that a handle can be dragged horizontally to adjust the use of available space.
#define Qt_SplitHCursor                           12    // A cursor used for horizontal splitters, indicating that a handle can be dragged vertically to adjust the use of available space.
#define Qt_PointingHandCursor                     13    // A pointing hand cursor that is typically used for clickable elements such as hyperlinks.
#define Qt_ForbiddenCursor                        14    // A slashed circle cursor, typically used during drag and drop operations to indicate that dragged content cannot be dropped on particular widgets or inside certain regions.
#define Qt_OpenHandCursor                         17    // A cursor representing an open hand, typically used to indicate that the area under the cursor is the visible part of a canvas that the user can click and drag in order to scroll around.
#define Qt_ClosedHandCursor                       18    // A cursor representing a closed hand, typically used to indicate that a dragging operation is in progress that involves scrolling.
#define Qt_WhatsThisCursor                        15    // An arrow with a question mark, typically used to indicate the presence of What's This? help for a widget.
#define Qt_BusyCursor                             16    // An hourglass or watch cursor, usually shown during operations that allow the user to interact with the application while they are performed in the background.
#define Qt_BitmapCursor                           24

// enum #define Qt_DateFormat
#define Qt_TextDate                               0     // The default Qt format, which includes the day and month name, the day number in the month, and the year in full. The day and month names will be short, localized names. This is basically equivalent to using the date format string, "ddd MMM d yyyy". See QDate_toString() for more information.
#define Qt_ISODate                                1     // ISO 8601 extended format: either YYYY-MM-DD for dates or YYYY-MM-DDTHH:MM:SS for combined dates and times.
#define Qt_SystemLocaleShortDate                  ?     // The short format used by the operating system.
#define Qt_SystemLocaleLongDate                   ?     // The long format used by the operating system.
#define Qt_DefaultLocaleShortDate                 ?     // The short format specified by the application's locale.
#define Qt_DefaultLocaleLongDate                  ?     // The long format used by the application's locale.
#define Qt_SystemLocaleDate                       2     // This enum value is deprecated. Use #define Qt_SystemLocaleShortDate instead (or #define Qt_SystemLocaleLongDate if you want long dates).
#define Qt_LocaleDate                             ?     // This enum value is deprecated. Use #define Qt_DefaultLocaleShortDate instead (or #define Qt_DefaultLocaleLongDate if you want long dates).
#define Qt_LocalDate                              SystemLocaleDate   // This enum value is deprecated. Use #define Qt_SystemLocaleShortDate instead (or #define Qt_SystemLocaleLongDate if you want long dates).
// Note: For ISODate formats, each Y, M and D represents a single digit of the year, month and day used to specify the date. Each H, M and S represents a single digit of the hour, minute and second used to specify the time. The presence of a literal T character is used to separate the date and time when both are specified.

// enum #define Qt_DayOfWeek
//
#define Qt_Monday                                 1
#define Qt_Tuesday                                2
#define Qt_Wednesday                              3
#define Qt_Thursday                               4
#define Qt_Friday                                 5
#define Qt_Saturday                               6
#define Qt_Sunday                                 7

// enum #define Qt_DockWidgetArea
// flags #define Qt_DockWidgetAreas
//
#define Qt_LeftDockWidgetArea                     0x1
#define Qt_RightDockWidgetArea                    0x2
#define Qt_TopDockWidgetArea                      0x4
#define Qt_BottomDockWidgetArea                   0x8
#define Qt_AllDockWidgetAreas                     Qt_DockWidgetArea_Mask
#define Qt_NoDockWidgetArea                       0
// The DockWidgetAreas type is a typedef for QFlags<DockWidgetArea>. It stores an OR combination of DockWidgetArea values.

// enum #define Qt_DropAction
// flags #define Qt_DropActions
//
#define Qt_CopyAction                             0x1   // Copy the data to the target.
#define Qt_MoveAction                             0x2   // Move the data from the source to the target.
#define Qt_LinkAction                             0x4   // Create a link from the source to the target.
#define Qt_ActionMask                             0xff  //
#define Qt_IgnoreAction                           0x0   // Ignore the action (do nothing with the data).
#define Qt_TargetMoveAction                       0x8002// On Windows, this value is used when the ownership of the D&D data should be taken over by the target application, i.e., the source application should not delete the data.
// On X11 this value is used to do a move.
// TargetMoveAction is not used on the Mac.
// The DropActions type is a typedef for QFlags<DropAction>. It stores an OR combination of DropAction values.

// enum #define Qt_EventPriority
// This enum can be used to specify event priorities.
//
#define Qt_HighEventPriority                      1     // Events with this priority are sent before events with NormalEventPriority or LowEventPriority.
#define Qt_NormalEventPriority                    0     // Events with this priority are sent after events with HighEventPriority, but before events with LowEventPriority.
#define Qt_LowEventPriority                       -1    // Events with this priority are sent after events with HighEventPriority or NormalEventPriority.
// Note that these values are provided purely for convenience, since event priorities can be any value between INT_MAX and INT_MIN, inclusive. For example, you can define custom priorities as being relative to each other:
// See also QCoreApplication_postEvent().

// enum #define Qt_FillRule
// Specifies which method should be used to fill the paths and polygons.
//
#define Qt_OddEvenFill                            0     // Specifies that the region is filled using the odd even fill rule. With this rule, we determine whether a point is inside the shape by using the following method. Draw a horizontal line from the point to a location outside the shape, and count the number of intersections. If the number of intersections is an odd number, the point is inside the shape. This mode is the default.
#define Qt_WindingFill                            1     // Specifies that the region is filled using the non zero winding rule. With this rule, we determine whether a point is inside the shape by using the following method. Draw a horizontal line from the point to a location outside the shape. Determine whether the direction of the line at each intersection point is up or down. The winding number is determined by summing the direction of each intersection. If the number is non zero, the point is inside the shape. This fill mode can also in most cases be considered as the intersection of closed shapes.

// enum #define Qt_FocusPolicy
// This enum type defines the various policies a widget can have with respect to acquiring keyboard focus.
//
#define Qt_TabFocus                               0x1   // the widget accepts focus by tabbing.
#define Qt_ClickFocus                             0x2   // the widget accepts focus by clicking.
#define Qt_StrongFocus                            Qt_TabFocus + Qt_ClickFocus + 0x8   // the widget accepts focus by both tabbing and clicking. On Mac OS X this will also be indicate that the widget accepts tab focus when in 'Text/List focus mode'.
#define Qt_WheelFocus                             Qt_StrongFocus + 0x4                // like #define Qt_StrongFocus plus the widget accepts focus by using the mouse wheel.
#define Qt_NoFocus                                0     // the widget does not accept focus.

// enum #define Qt_FocusReason
// This enum specifies why the focus changed. It will be passed through QWidget_setFocus and can be retrieved in the QFocusEvent sent to the widget upon focus change.
//
#define Qt_MouseFocusReason                       0     // A mouse action occurred.
#define Qt_TabFocusReason                         1     // The Tab key was pressed.
#define Qt_BacktabFocusReason                     2     // A Backtab occurred. The input for this may include the Shift or Control keys; e.g. Shift+Tab.
#define Qt_ActiveWindowFocusReason                3     // The window system made this window either active or inactive.
#define Qt_PopupFocusReason                       4     // The application opened/closed a pop-up that grabbed/released the keyboard focus.
#define Qt_ShortcutFocusReason                    5     // The user typed a label's buddy shortcut
#define Qt_MenuBarFocusReason                     6     // The menu bar took focus.
#define Qt_OtherFocusReason                       7     // Another reason, usually application-specific.
// See also Keyboard Focus.

// enum #define Qt_GlobalColor
// Qt's predefined QColor objects:
//
#define Qt_white                                  3     // White (#ffffff)
#define Qt_black                                  2     // Black (#000000)
#define Qt_red                                    7     // Red (#ff0000)
#define Qt_darkRed                                13    // Dark red (#800000)
#define Qt_green                                  8     // Green (#00ff00)
#define Qt_darkGreen                              14    // Dark green (#008000)
#define Qt_blue                                   9     // Blue (#0000ff)
#define Qt_darkBlue                               15    // Dark blue (#000080)
#define Qt_cyan                                   10    // Cyan (#00ffff)
#define Qt_darkCyan                               16    // Dark cyan (#008080)
#define Qt_magenta                                11    // Magenta (#ff00ff)
#define Qt_darkMagenta                            17    // Dark magenta (#800080)
#define Qt_yellow                                 12    // Yellow (#ffff00)
#define Qt_darkYellow                             18    // Dark yellow (#808000)
#define Qt_gray                                   5     // Gray (#a0a0a4)
#define Qt_darkGray                               4     // Dark gray (#808080)
#define Qt_lightGray                              6     // Light gray (#c0c0c0)
#define Qt_transparent                            19    // a transparent black value (i.e., QColor(0, 0, 0, 0))
#define Qt_color0                                 0     // 0 pixel value (for bitmaps)
#define Qt_color1                                 1     // 1 pixel value (for bitmaps)
// See also QColor.

// enum #define Qt_HitTestAccuracy
// This enum contains the types of accuracy that can be used by the QTextDocument class when testing for mouse clicks on text documents.
//
#define Qt_ExactHit                               0     // The point at which input occurred must coincide exactly with input-sensitive parts of the document.
#define Qt_FuzzyHit                               1     // The point at which input occurred can lie close to input-sensitive parts of the document.
// This enum is defined in the <QTextDocument> header file.

// enum #define Qt_ImageConversionFlag
// flags #define Qt_ImageConversionFlags
// The options marked "(default)" are set if no other values from the list are included (since the defaults are zero):
// Color/Mono preference (ignored for QBitmap):
//
#define Qt_AutoColor                              0x00000000   // (default) - If the image has depth 1 and contains only black and white pixels, the pixmap becomes monochrome.
#define Qt_ColorOnly                              0x00000003   // The pixmap is dithered/converted to the native display depth.
#define Qt_MonoOnly                               0x00000002   // The pixmap becomes monochrome. If necessary, it is dithered using the chosen dithering algorithm.

// Dithering mode preference for RGB channels:
//
#define Qt_DiffuseDither                          0x00000000   // (default) - A high-quality dither.
#define Qt_OrderedDither                          0x00000010   // A faster, more ordered dither.
#define Qt_ThresholdDither                        0x00000020   // No dithering; closest color is used.

// Dithering mode preference for alpha channel:
//
#define Qt_ThresholdAlphaDither                   0x00000000   // (default) - No dithering.
#define Qt_OrderedAlphaDither                     0x00000004   // A faster, more ordered dither.
#define Qt_DiffuseAlphaDither                     0x00000008   // A high-quality dither.

// Color matching versus dithering preference:
//
#define Qt_PreferDither                           0x00000040   // (default when converting to a pixmap) - Always dither 32-bit images when the image is converted to 8 bits.
#define Qt_AvoidDither                            0x00000080   // (default when converting for the purpose of saving to file) - Dither 32-bit images only if the image has more than 256 colors and it is being converted to 8 bits.
// The ImageConversionFlags type is a typedef for QFlags<ImageConversionFlag>. It stores an OR combination of ImageConversionFlag values.

// enum #define Qt_InputMethodQuery
//
#define Qt_ImMicroFocus                           0     // The rectangle covering the area of the input cursor in widget coordinates.
#define Qt_ImFont                                 1     // The currently used font for text input.
#define Qt_ImCursorPosition                       2     // The logical position of the cursor within the text surrounding the input area (see ImSurroundingText). If any text is selected, the position returned will be at the logical end of the selection, even if the real cursor is located at the logical start.
#define Qt_ImSurroundingText                      3     // The plain text around the input area, for example the current paragraph.
#define Qt_ImCurrentSelection                     4     // The currently selected text.

// enum #define Qt_ItemDataRole
// Each item in the model has a set of data elements associated with it, each with its own role. The roles are used by the view to indicate to the model which type of data it needs.
// The general purpose roles are:
//
#define Qt_DisplayRole                            0     // The key data to be rendered in the form of text.
#define Qt_DecorationRole                         1     // The data to be rendered as a decoration in the form of an icon.
#define Qt_EditRole                               2     // The data in a form suitable for editing in an editor.
#define Qt_ToolTipRole                            3     // The data displayed in the item's tooltip.
#define Qt_StatusTipRole                          4     // The data displayed in the status bar.
#define Qt_WhatsThisRole                          5     // The data displayed for the item in "What's This?" mode.
#define Qt_SizeHintRole                           13    // The size hint for the item that will be supplied to views.

// Roles describing appearance and meta data:
//
#define Qt_FontRole                               6     // The font used for items rendered with the default delegate.
#define Qt_TextAlignmentRole                      7     // The alignment of the text for items rendered with the default delegate.
#define Qt_BackgroundRole                         8     // The background brush used for items rendered with the default delegate.
#define Qt_BackgroundColorRole                    8     // This role is obsolete. Use BackgroundRole instead.
#define Qt_ForegroundRole                         9     // The foreground brush (text color, typically) used for items rendered with the default delegate.
#define Qt_TextColorRole                          9     // This role is obsolete. Use ForegroundRole instead.
#define Qt_CheckStateRole                         10    // This role is used to obtain the checked state of an item (see #define Qt_CheckState).

// Accessibility roles:
//
#define Qt_AccessibleTextRole                     11    // The text to be used by accessibility extensions and plugins, such as screen readers.
#define Qt_AccessibleDescriptionRole              12    // A description of the item for accessibility purposes.

// User roles:
//
#define Qt_UserRole                               32    // The first role that can be used for application-specific purposes.

// enum #define Qt_ItemFlag
// flags #define Qt_ItemFlags
// This enum describes the properties of an item:
//
#define Qt_NoItemFlags                            0     // It does not have any properties set.
#define Qt_ItemIsSelectable                       1     // It can be selected.
#define Qt_ItemIsEditable                         2     // It can be edited.
#define Qt_ItemIsDragEnabled                      4     // It can be dragged.
#define Qt_ItemIsDropEnabled                      8     // It can be used as a drop target.
#define Qt_ItemIsUserCheckable                    16    // It can be checked or unchecked by the user.
#define Qt_ItemIsEnabled                          32    // The user can interact with the item.
#define Qt_ItemIsTristate                         64    // The item is checkable with three separate states.
// Note that checkable items need to be given both a suitable set of flags and an initial state, indicating whether the item is checked or not. This is handled automatically for model/view components, but needs to be explicitly set for instances of QListWidgetItem, QTableWidgetItem, and QTreeWidgetItem.
// The ItemFlags type is a typedef for QFlags<ItemFlag>. It stores an OR combination of ItemFlag values.
// See also QAbstractItemModel.

// enum #define Qt_ItemSelectionMode
// This enum is used in QGraphicsItem, QGraphicsScene and QGraphicsView to specify how items are selected, or how to determine if a shapes and items collide.
//
#define Qt_ContainsItemShape                      0x0   // The output list contains only items whose shape is fully contained inside the selection area. Items that intersect with the area's outline are not included.
#define Qt_IntersectsItemShape                    0x1   // The output list contains both items whose shape is fully contained inside the selection area, and items that intersect with the area's outline. This is a common mode for rubber band selection.
#define Qt_ContainsItemBoundingRect               0x2   // The output list contains only items whose bounding rectangle is fully contained inside the selection area. Items that intersect with the area's outline are not included.
#define Qt_IntersectsItemBoundingRect             0x3   // The output list contains both items whose bounding rectangle is fully contained inside the selection area, and items that intersect with the area's outline. This method is commonly used for determining areas that need redrawing.
// See also QGraphicsScene_items(), QGraphicsScene_collidingItems(), QGraphicsView_items(), QGraphicsItem_collidesWithItem(), and QGraphicsItem_collidesWithPath().

// enum #define Qt_Key
// The key names used by Qt.
//
#define Qt_Key_Escape                             0x01000000
#define Qt_Key_Tab                                0x01000001
#define Qt_Key_Backtab                            0x01000002
#define Qt_Key_Backspace                          0x01000003
#define Qt_Key_Return                             0x01000004
#define Qt_Key_Enter                              0x01000005   // Typically located on the keypad.
#define Qt_Key_Insert                             0x01000006
#define Qt_Key_Delete                             0x01000007
#define Qt_Key_Pause                              0x01000008
#define Qt_Key_Print                              0x01000009
#define Qt_Key_SysReq                             0x0100000a
#define Qt_Key_Clear                              0x0100000b
#define Qt_Key_Home                               0x01000010
#define Qt_Key_End                                0x01000011
#define Qt_Key_Left                               0x01000012
#define Qt_Key_Up                                 0x01000013
#define Qt_Key_Right                              0x01000014
#define Qt_Key_Down                               0x01000015
#define Qt_Key_PageUp                             0x01000016
#define Qt_Key_PageDown                           0x01000017
#define Qt_Key_Shift                              0x01000020
#define Qt_Key_Control                            0x01000021   // On Mac OS X, this corresponds to the Command keys.
#define Qt_Key_Meta                               0x01000022   // On Mac OS X, this corresponds to the Control keys. On Windows keyboards, this key is mapped to the Windows key.
#define Qt_Key_Alt                                0x01000023
#define Qt_Key_AltGr                              0x01001103   // On Windows, when the KeyDown event for this key is sent, the Ctrl+Alt modifiers are also set.
#define Qt_Key_CapsLock                           0x01000024
#define Qt_Key_NumLock                            0x01000025
#define Qt_Key_ScrollLock                         0x01000026
#define Qt_Key_F1                                 0x01000030
#define Qt_Key_F2                                 0x01000031
#define Qt_Key_F3                                 0x01000032
#define Qt_Key_F4                                 0x01000033
#define Qt_Key_F5                                 0x01000034
#define Qt_Key_F6                                 0x01000035
#define Qt_Key_F7                                 0x01000036
#define Qt_Key_F8                                 0x01000037
#define Qt_Key_F9                                 0x01000038
#define Qt_Key_F10                                0x01000039
#define Qt_Key_F11                                0x0100003a
#define Qt_Key_F12                                0x0100003b
#define Qt_Key_F13                                0x0100003c
#define Qt_Key_F14                                0x0100003d
#define Qt_Key_F15                                0x0100003e
#define Qt_Key_F16                                0x0100003f
#define Qt_Key_F17                                0x01000040
#define Qt_Key_F18                                0x01000041
#define Qt_Key_F19                                0x01000042
#define Qt_Key_F20                                0x01000043
#define Qt_Key_F21                                0x01000044
#define Qt_Key_F22                                0x01000045
#define Qt_Key_F23                                0x01000046
#define Qt_Key_F24                                0x01000047
#define Qt_Key_F25                                0x01000048
#define Qt_Key_F26                                0x01000049
#define Qt_Key_F27                                0x0100004a
#define Qt_Key_F28                                0x0100004b
#define Qt_Key_F29                                0x0100004c
#define Qt_Key_F30                                0x0100004d
#define Qt_Key_F31                                0x0100004e
#define Qt_Key_F32                                0x0100004f
#define Qt_Key_F33                                0x01000050
#define Qt_Key_F34                                0x01000051
#define Qt_Key_F35                                0x01000052
#define Qt_Key_Super_L                            0x01000053
#define Qt_Key_Super_R                            0x01000054
#define Qt_Key_Menu                               0x01000055
#define Qt_Key_Hyper_L                            0x01000056
#define Qt_Key_Hyper_R                            0x01000057
#define Qt_Key_Help                               0x01000058
#define Qt_Key_Direction_L                        0x01000059
#define Qt_Key_Direction_R                        0x01000060
#define Qt_Key_Space                              0x20
#define Qt_Key_Any                                Key_Space
#define Qt_Key_Exclam                             0x21
#define Qt_Key_QuoteDbl                           0x22
#define Qt_Key_NumberSign                         0x23
#define Qt_Key_Dollar                             0x24
#define Qt_Key_Percent                            0x25
#define Qt_Key_Ampersand                          0x26
#define Qt_Key_Apostrophe                         0x27
#define Qt_Key_ParenLeft                          0x28
#define Qt_Key_ParenRight                         0x29
#define Qt_Key_Asterisk                           0x2a
#define Qt_Key_Plus                               0x2b
#define Qt_Key_Comma                              0x2c
#define Qt_Key_Minus                              0x2d
#define Qt_Key_Period                             0x2e
#define Qt_Key_Slash                              0x2f
#define Qt_Key_0                                  0x30
#define Qt_Key_1                                  0x31
#define Qt_Key_2                                  0x32
#define Qt_Key_3                                  0x33
#define Qt_Key_4                                  0x34
#define Qt_Key_5                                  0x35
#define Qt_Key_6                                  0x36
#define Qt_Key_7                                  0x37
#define Qt_Key_8                                  0x38
#define Qt_Key_9                                  0x39
#define Qt_Key_Colon                              0x3a
#define Qt_Key_Semicolon                          0x3b
#define Qt_Key_Less                               0x3c
#define Qt_Key_Equal                              0x3d
#define Qt_Key_Greater                            0x3e
#define Qt_Key_Question                           0x3f
#define Qt_Key_At                                 0x40      // 64
#define Qt_Key_A                                  0x41      // 65
#define Qt_Key_B                                  0x42
#define Qt_Key_C                                  0x43
#define Qt_Key_D                                  0x44
#define Qt_Key_E                                  0x45
#define Qt_Key_F                                  0x46
#define Qt_Key_G                                  0x47
#define Qt_Key_H                                  0x48
#define Qt_Key_I                                  0x49
#define Qt_Key_J                                  0x4a
#define Qt_Key_K                                  0x4b
#define Qt_Key_L                                  0x4c
#define Qt_Key_M                                  0x4d
#define Qt_Key_N                                  0x4e      // 78
#define Qt_Key_O                                  0x4f      // 79
#define Qt_Key_P                                  0x50      // 80
#define Qt_Key_Q                                  0x51      // 81
#define Qt_Key_R                                  0x52      // 82
#define Qt_Key_S                                  0x53      // 83
#define Qt_Key_T                                  0x54
#define Qt_Key_U                                  0x55
#define Qt_Key_V                                  0x56
#define Qt_Key_W                                  0x57
#define Qt_Key_X                                  0x58
#define Qt_Key_Y                                  0x59
#define Qt_Key_Z                                  0x5a      // 90
#define Qt_Key_BracketLeft                        0x5b
#define Qt_Key_Backslash                          0x5c
#define Qt_Key_BracketRight                       0x5d
#define Qt_Key_AsciiCircum                        0x5e
#define Qt_Key_Underscore                         0x5f
#define Qt_Key_QuoteLeft                          0x60
#define Qt_Key_BraceLeft                          0x7b
#define Qt_Key_Bar                                0x7c
#define Qt_Key_BraceRight                         0x7d
#define Qt_Key_AsciiTilde                         0x7e
#define Qt_Key_nobreakspace                       0x0a0
#define Qt_Key_exclamdown                         0x0a1
#define Qt_Key_cent                               0x0a2
#define Qt_Key_sterling                           0x0a3
#define Qt_Key_currency                           0x0a4
#define Qt_Key_yen                                0x0a5
#define Qt_Key_brokenbar                          0x0a6
#define Qt_Key_section                            0x0a7
#define Qt_Key_diaeresis                          0x0a8
#define Qt_Key_copyright                          0x0a9
#define Qt_Key_ordfeminine                        0x0aa
#define Qt_Key_guillemotleft                      0x0ab
#define Qt_Key_notsign                            0x0ac
#define Qt_Key_hyphen                             0x0ad
#define Qt_Key_registered                         0x0ae
#define Qt_Key_macron                             0x0af
#define Qt_Key_degree                             0x0b0
#define Qt_Key_plusminus                          0x0b1
#define Qt_Key_twosuperior                        0x0b2
#define Qt_Key_threesuperior                      0x0b3
#define Qt_Key_acute                              0x0b4
#define Qt_Key_mu                                 0x0b5
#define Qt_Key_paragraph                          0x0b6
#define Qt_Key_periodcentered                     0x0b7
#define Qt_Key_cedilla                            0x0b8
#define Qt_Key_onesuperior                        0x0b9
#define Qt_Key_masculine                          0x0ba
#define Qt_Key_guillemotright                     0x0bb
#define Qt_Key_onequarter                         0x0bc
#define Qt_Key_onehalf                            0x0bd
#define Qt_Key_threequarters                      0x0be
#define Qt_Key_questiondown                       0x0bf
#define Qt_Key_Agrave                             0x0c0
#define Qt_Key_Aacute                             0x0c1
#define Qt_Key_Acircumflex                        0x0c2
#define Qt_Key_Atilde                             0x0c3
#define Qt_Key_Adiaeresis                         0x0c4
#define Qt_Key_Aring                              0x0c5
#define Qt_Key_AE                                 0x0c6
#define Qt_Key_Ccedilla                           0x0c7
#define Qt_Key_Egrave                             0x0c8
#define Qt_Key_Eacute                             0x0c9
#define Qt_Key_Ecircumflex                        0x0ca
#define Qt_Key_Ediaeresis                         0x0cb
#define Qt_Key_Igrave                             0x0cc
#define Qt_Key_Iacute                             0x0cd
#define Qt_Key_Icircumflex                        0x0ce
#define Qt_Key_Idiaeresis                         0x0cf
#define Qt_Key_ETH                                0x0d0
#define Qt_Key_Ntilde                             0x0d1
#define Qt_Key_Ograve                             0x0d2
#define Qt_Key_Oacute                             0x0d3
#define Qt_Key_Ocircumflex                        0x0d4
#define Qt_Key_Otilde                             0x0d5
#define Qt_Key_Odiaeresis                         0x0d6
#define Qt_Key_multiply                           0x0d7
#define Qt_Key_Ooblique                           0x0d8
#define Qt_Key_Ugrave                             0x0d9
#define Qt_Key_Uacute                             0x0da
#define Qt_Key_Ucircumflex                        0x0db
#define Qt_Key_Udiaeresis                         0x0dc
#define Qt_Key_Yacute                             0x0dd
#define Qt_Key_THORN                              0x0de
#define Qt_Key_ssharp                             0x0df
#define Qt_Key_division                           0x0f7
#define Qt_Key_ydiaeresis                         0x0ff
#define Qt_Key_Multi_key                          0x01001120
#define Qt_Key_Codeinput                          0x01001137
#define Qt_Key_SingleCandidate                    0x0100113c
#define Qt_Key_MultipleCandidate                  0x0100113d
#define Qt_Key_PreviousCandidate                  0x0100113e
#define Qt_Key_Mode_switch                        0x0100117e
#define Qt_Key_Kanji                              0x01001121
#define Qt_Key_Muhenkan                           0x01001122
#define Qt_Key_Henkan                             0x01001123
#define Qt_Key_Romaji                             0x01001124
#define Qt_Key_Hiragana                           0x01001125
#define Qt_Key_Katakana                           0x01001126
#define Qt_Key_Hiragana_Katakana                  0x01001127
#define Qt_Key_Zenkaku                            0x01001128
#define Qt_Key_Hankaku                            0x01001129
#define Qt_Key_Zenkaku_Hankaku                    0x0100112a
#define Qt_Key_Touroku                            0x0100112b
#define Qt_Key_Massyo                             0x0100112c
#define Qt_Key_Kana_Lock                          0x0100112d
#define Qt_Key_Kana_Shift                         0x0100112e
#define Qt_Key_Eisu_Shift                         0x0100112f
#define Qt_Key_Eisu_toggle                        0x01001130
#define Qt_Key_Hangul                             0x01001131
#define Qt_Key_Hangul_Start                       0x01001132
#define Qt_Key_Hangul_End                         0x01001133
#define Qt_Key_Hangul_Hanja                       0x01001134
#define Qt_Key_Hangul_Jamo                        0x01001135
#define Qt_Key_Hangul_Romaja                      0x01001136
#define Qt_Key_Hangul_Jeonja                      0x01001138
#define Qt_Key_Hangul_Banja                       0x01001139
#define Qt_Key_Hangul_PreHanja                    0x0100113a
#define Qt_Key_Hangul_PostHanja                   0x0100113b
#define Qt_Key_Hangul_Special                     0x0100113f
#define Qt_Key_Dead_Grave                         0x01001250
#define Qt_Key_Dead_Acute                         0x01001251
#define Qt_Key_Dead_Circumflex                    0x01001252
#define Qt_Key_Dead_Tilde                         0x01001253
#define Qt_Key_Dead_Macron                        0x01001254
#define Qt_Key_Dead_Breve                         0x01001255
#define Qt_Key_Dead_Abovedot                      0x01001256
#define Qt_Key_Dead_Diaeresis                     0x01001257
#define Qt_Key_Dead_Abovering                     0x01001258
#define Qt_Key_Dead_Doubleacute                   0x01001259
#define Qt_Key_Dead_Caron                         0x0100125a
#define Qt_Key_Dead_Cedilla                       0x0100125b
#define Qt_Key_Dead_Ogonek                        0x0100125c
#define Qt_Key_Dead_Iota                          0x0100125d
#define Qt_Key_Dead_Voiced_Sound                  0x0100125e
#define Qt_Key_Dead_Semivoiced_Sound              0x0100125f
#define Qt_Key_Dead_Belowdot                      0x01001260
#define Qt_Key_Dead_Hook                          0x01001261
#define Qt_Key_Dead_Horn                          0x01001262
#define Qt_Key_Back                               0x01000061
#define Qt_Key_Forward                            0x01000062
#define Qt_Key_Stop                               0x01000063
#define Qt_Key_Refresh                            0x01000064
#define Qt_Key_VolumeDown                         0x01000070
#define Qt_Key_VolumeMute                         0x01000071
#define Qt_Key_VolumeUp                           0x01000072
#define Qt_Key_BassBoost                          0x01000073
#define Qt_Key_BassUp                             0x01000074
#define Qt_Key_BassDown                           0x01000075
#define Qt_Key_TrebleUp                           0x01000076
#define Qt_Key_TrebleDown                         0x01000077
#define Qt_Key_MediaPlay                          0x01000080
#define Qt_Key_MediaStop                          0x01000081
#define Qt_Key_MediaPrevious                      0x01000082
#define Qt_Key_MediaNext                          0x01000083
#define Qt_Key_MediaRecord                        0x01000084
#define Qt_Key_HomePage                           0x01000090
#define Qt_Key_Favorites                          0x01000091
#define Qt_Key_Search                             0x01000092
#define Qt_Key_Standby                            0x01000093
#define Qt_Key_OpenUrl                            0x01000094
#define Qt_Key_LaunchMail                         0x010000a0
#define Qt_Key_LaunchMedia                        0x010000a1
#define Qt_Key_Launch0                            0x010000a2
#define Qt_Key_Launch1                            0x010000a3
#define Qt_Key_Launch2                            0x010000a4
#define Qt_Key_Launch3                            0x010000a5
#define Qt_Key_Launch4                            0x010000a6
#define Qt_Key_Launch5                            0x010000a7
#define Qt_Key_Launch6                            0x010000a8
#define Qt_Key_Launch7                            0x010000a9
#define Qt_Key_Launch8                            0x010000aa
#define Qt_Key_Launch9                            0x010000ab
#define Qt_Key_LaunchA                            0x010000ac
#define Qt_Key_LaunchB                            0x010000ad
#define Qt_Key_LaunchC                            0x010000ae
#define Qt_Key_LaunchD                            0x010000af
#define Qt_Key_LaunchE                            0x010000b0
#define Qt_Key_LaunchF                            0x010000b1
#define Qt_Key_MediaLast                          0x0100ffff
#define Qt_Key_unknown                            0x01ffffff
#define Qt_Key_Call                               0x01100004
#define Qt_Key_Context1                           0x01100000
#define Qt_Key_Context2                           0x01100001
#define Qt_Key_Context3                           0x01100002
#define Qt_Key_Context4                           0x01100003
#define Qt_Key_Flip                               0x01100006
#define Qt_Key_Hangup                             0x01100005
#define Qt_Key_No                                 0x01010002
#define Qt_Key_Select                             0x01010000
#define Qt_Key_Yes                                0x01010001
#define Qt_Key_Execute                            0x01020003
#define Qt_Key_Printer                            0x01020002
#define Qt_Key_Play                               0x01020005
#define Qt_Key_Sleep                              0x01020004
#define Qt_Key_Zoom                               0x01020006
#define Qt_Key_Cancel                             0x01020001
// See also QKeyEvent_key().

// enum #define Qt_KeyboardModifier
// flags #define Qt_KeyboardModifiers
// This enum describes the modifier keys.
//
#define Qt_NoModifier                             0x00000000   // No modifier key is pressed.
#define Qt_ShiftModifier                          0x02000000   // A Shift key on the keyboard is pressed.
#define Qt_ControlModifier                        0x04000000   // A Ctrl key on the keyboard is pressed.
#define Qt_AltModifier                            0x08000000   // An Alt key on the keyboard is pressed.
#define Qt_MetaModifier                           0x10000000   // A Meta key on the keyboard is pressed.
#define Qt_KeypadModifier                         0x20000000   // A keypad button is pressed.
#define Qt_GroupSwitchModifier                    0x40000000   // X11 only. A Mode_switch key on the keyboard is pressed.
// Note: On Mac OS X, the ControlModifier value corresponds to the Command keys on the Macintosh keyboard, and the MetaModifier value corresponds to the Control keys. The KeypadModifier value will also be set when an arrow key is pressed as the arrow keys are considered part of the keypad.
// Note: On Windows Keyboards, #define Qt_MetaModifier and #define Qt_Key_Meta are mapped to the Windows key.
// The KeyboardModifiers type is a typedef for QFlags<KeyboardModifier>. It stores an OR combination of KeyboardModifier values.
// See also MouseButton and Modifier.

// enum #define Qt_LayoutDirection
// Specifies the direction of Qt's layouts:
//
#define Qt_LeftToRight                            0    // Left-to-right layout.
#define Qt_RightToLeft                            1    // Right-to-left layout.
// Right-to-left layouts are necessary for certain languages, notably Arabic and Hebrew.
// See also QApplication_setLayoutDirection() and QWidget_setLayoutDirection().

// enum #define Qt_MaskMode
// This enum specifies the behavior of the QPixmap_createMaskFromColor() and QImage_createMaskFromColor() functions.
//
#define Qt_MaskInColor                            0    // Creates a mask where all pixels matching the given color are opaque.
#define Qt_MaskOutColor                           1    // Creates a mask where all pixels matching the given color are transparent.

// enum #define Qt_MatchFlag
// flags #define Qt_MatchFlags
// This enum describes the type of matches that can be used when searching for items in a model.
//
#define Qt_MatchExactly                           0    // Performs QVariant-based matching.
#define Qt_MatchFixedString                       8    // Performs string-based matching. String-based comparisons are case-insensitive unless the MatchCaseSensitive flag is also specified.
#define Qt_MatchContains                          1    // The search term is contained in the item.
#define Qt_MatchStartsWith                        2    // The search term matches the start of the item.
#define Qt_MatchEndsWith                          3    // The search term matches the end of the item.
#define Qt_MatchCaseSensitive                     16   // The search is case sensitive.
#define Qt_MatchRegExp                            4    // Performs string-based matching using a regular expression as the search term.
#define Qt_MatchWildcard                          5    // Performs string-based matching using a string with wildcards as the search term.
#define Qt_MatchWrap                              32   // Perform a search that wraps around, so that when the search reaches the last item in the model, it begins again at the first item and continues until all items have been examined.
#define Qt_MatchRecursive                         64   // Searches the entire hierarchy.
// The MatchFlags type is a typedef for QFlags<MatchFlag>. It stores an OR combination of MatchFlag values.
// See also QString_compare() and QRegExp.

// enum #define Qt_Modifier
// This enum provides shorter names for the keyboard modifier keys supported by Qt.
// Note: On Mac OS X, the CTRL value corresponds to the Command keys on the Macintosh keyboard, and the META value corresponds to the Control keys.
//
#define Qt_SHIFT                                  Qt_ShiftModifier     // The Shift keys provided on all standard keyboards.
#define Qt_META                                   Qt_MetaModifier      // The Meta keys.
#define Qt_CTRL                                   Qt_ControlModifier   // The Ctrl keys.
#define Qt_ALT                                    Qt_AltModifier       // The normal Alt keys, but not keys like AltGr.
#define Qt_UNICODE_ACCEL                          0x00000000           // The shortcut is specified as a Unicode code point, not as a Qt Key.
// See also KeyboardModifier and MouseButton.

// enum #define Qt_MouseButton
// flags #define Qt_MouseButtons
// This enum type describes the different mouse buttons.
//
#define Qt_NoButton                               0x00000000   // The button state does not refer to any button (see QMouseEvent_button()).
#define Qt_LeftButton                             0x00000001   // The left button is pressed, or an event refers to the left button. (The left button may be the right button on left-handed mice.)
#define Qt_RightButton                            0x00000002   // The right button.
#define Qt_MidButton                              0x00000004   // The middle button.
#define Qt_XButton1                               0x00000008   // The first X button.
#define Qt_XButton2                               0x00000010   // The second X button.
// The MouseButtons type is a typedef for QFlags<MouseButton>. It stores an OR combination of MouseButton values.
// See also KeyboardModifier and Modifier.

// enum #define Qt_Orientation
// flags #define Qt_Orientations
// This type is used to signify an object's orientation.
//
#define Qt_Horizontal                             0x1
#define Qt_Vertical                               0x2
// Orientation is used with QScrollBar for example.
// The Orientations type is a typedef for QFlags<Orientation>. It stores an OR combination of Orientation values.

// enum #define Qt_PenCapStyle
// This enum type defines the pen cap styles supported by Qt, i.e. the line end caps that can be drawn using QPainter.
//
#define Qt_FlatCap                                0x00  // a square line end that does not cover the end point of the line.
#define Qt_SquareCap                              0x10  // a square line end that covers the end point and extends beyond it by half the line width.
#define Qt_RoundCap                               0x20  // a rounded line end.
// See also QPen.

// enum #define Qt_PenJoinStyle
// This enum type defines the pen join styles supported by Qt, i.e. which joins between two connected lines can be drawn using QPainter.
//
#define Qt_MiterJoin                              0x00  // The outer edges of the lines are extended to meet at an angle, and this area is filled.
#define Qt_BevelJoin                              0x40  // The triangular notch between the two lines is filled.
#define Qt_RoundJoin                              0x80  // A circular arc between the two lines is filled.
#define Qt_SvgMiterJoin                           0x100 // A miter join corresponding to the definition of a miter join in the SVG 1.2 Tiny specification.
// See also QPen.

// enum #define Qt_PenStyle
// This enum type defines the pen styles that can be drawn using QPainter. The styles are:
//
#define Qt_NoPen                                  0     // no line at all. For example, QPainter_drawRect() fills but does not draw any boundary line.
#define Qt_SolidLine                              1     // A plain line.
#define Qt_DashLine                               2     // Dashes separated by a few pixels.
#define Qt_DotLine                                3     // Dots separated by a few pixels.
#define Qt_DashDotLine                            4     // Alternate dots and dashes.
#define Qt_DashDotDotLine                         5     // One dash, two dots, one dash, two dots.
#define Qt_CustomDashLine                         6     // A custom pattern defined using QPainterPathStroker_setDashPattern().
// See also QPen.

// enum #define Qt_ScrollBarPolicy
// This enum type describes the various modes of QAbstractScrollArea's scroll bars.
//
#define Qt_ScrollBarAsNeeded                      0     // QAbstractScrollArea shows a scroll bar when the content is too large to fit and not otherwise. This is the default.
#define Qt_ScrollBarAlwaysOff                     1     // QAbstractScrollArea never shows a scroll bar.
#define Qt_ScrollBarAlwaysOn                      2     // QAbstractScrollArea always shows a scroll bar.
// The modes for the horizontal and vertical scroll bars are independent.)

// enum #define Qt_ShortcutContext
// For a QEvent_Shortcut event to occur, the shortcut's key sequence must be entered by the user in a context where the shortcut is active. The possible contexts are these:
//
#define Qt_WidgetShortcut                         0     // The shortcut is active when its parent widget has focus.
#define Qt_WidgetWithChildrenShortcut             3     // The shortcut is active when its parent widget, or any of its children has focus. Children which are top-level widgets, except pop-ups, are not affected by this shortcut context.
#define Qt_WindowShortcut                         1     // The shortcut is active when its parent widget is a logical subwidget of the active top-level window.
#define Qt_ApplicationShortcut                    2     // The shortcut is active when one of the applications windows are active.

// enum #define Qt_SizeHint
// This enum is used by QGraphicsLayoutItem_sizeHint()
//
#define Qt_MinimumSize                            0     // is used to specify the minimum size of a graphics layout item.
#define Qt_PreferredSize                          1     // is used to specify the preferred size of a graphics layout item.
#define Qt_MaximumSize                            2     // is used to specify the maximum size of a graphics layout item.
#define Qt_MinimumDescent                         3     // is used to specify the minimum descent of a text string in a graphics layout item.
// See also QGraphicsLayoutItem_sizeHint().

// enum #define Qt_SizeMode
// This enum is used by QPainter_drawRoundedRect() and QPainterPath_addRoundedRect() functions to specify the radii of rectangle corners with respect to the dimensions of the bounding rectangles specified.
//
#define Qt_AbsoluteSize                           0     // Specifies the size using absolute measurements.
#define Qt_RelativeSize                           1     // Specifies the size relative to the bounding rectangle, typically using percentage measurements.

// enum #define Qt_SortOrder
// This enum describes how the items in a widget are sorted.
//
#define Qt_AscendingOrder                         0     // The items are sorted ascending e.g. starts with 'AAA' ends with 'ZZZ' in Latin-1 locales
#define Qt_DescendingOrder                        1     // The items are sorted descending e.g. starts with 'ZZZ' ends with 'AAA' in Latin-1 locales

// enum #define Qt_TextElideMode
// This enum specifies where the ellipsis should appear when displaying texts that don't fit:
//
#define Qt_ElideLeft                              0     // The ellipsis should appear at the beginning of the text.
#define Qt_ElideRight                             1     // The ellipsis should appear at the end of the text.
#define Qt_ElideMiddle                            2     // The ellipsis should appear in the middle of the text.
#define Qt_ElideNone                              3     // Ellipsis should NOT appear in the text.
// #define Qt_ElideMiddle is normally the most appropriate choice for URLs (e.g., "http://www.qtsof...ovingto/beijing/"), whereas #define Qt_ElideRight is appropriate for other strings (e.g., "Deploying Applications on Ma...").
// See also QAbstractItemView_textElideMode, QFontMetrics_elidedText(), AlignmentFlag, and QTabBar_elideMode.

// enum #define Qt_TextFlag
// This enum type is used to define some modifier flags. Some of these flags only make sense in the context of printing:
//
#define Qt_TextSingleLine                         0x0100   // Treats all whitespace as spaces and prints just one line.
#define Qt_TextDontClip                           0x0200   // If it's impossible to stay within the given bounds, it prints outside.
#define Qt_TextExpandTabs                         0x0400   // Makes the U+0009 (ASCII tab) character move to the next tab stop.
#define Qt_TextShowMnemonic                       0x0800   // Displays the string "&P" as P (see QButton for an example). For an ampersand, use "&&".
#define Qt_TextWordWrap                           0x1000   // Breaks lines at appropriate points, e.g. at word boundaries.
#define Qt_TextWrapAnywhere                       0x2000   // Breaks lines anywhere, even within words.
#define Qt_TextHideMnemonic                       0x8000   // Same as #define Qt_TextShowMnemonic but doesn't draw the underlines.
#define Qt_TextDontPrint                          0x4000   // Treat this text as "hidden" and don't print it.
#define Qt_IncludeTrailingSpaces                  Qt_TextIncludeTrailingSpaces   // When this option is set, QTextLine_naturalTextWidth() and naturalTextRect() will return a value that includes the width of trailing spaces in the text; otherwise this width is excluded.
#define Qt_TextIncludeTrailingSpaces              0x08000000 // Same as IncludeTrailingSpaces
#define Qt_TextJustificationForced                0x10000    // Ensures that text lines are justified.
// You can use as many modifier flags as you want, except that #define Qt_TextSingleLine and #define Qt_TextWordWrap cannot be combined.
// Flags that are inappropriate for a given use are generally ignored.

// enum #define Qt_TextFormat
// This enum is used in widgets that can display both plain text and rich text, e.g. QLabel. It is used for deciding whether a text string should be interpreted as one or the other. This is normally done by passing one of the enum values to a setTextFormat() function.
//
#define Qt_PlainText                              0     // The text string is interpreted as a plain text string.
#define Qt_RichText                               1     // The text string is interpreted as a rich text string.
#define Qt_AutoText                               2     // The text string is interpreted as for #define Qt_RichText if #define Qt_mightBeRichText() returns true, otherwise as #define Qt_PlainText.
#define Qt_LogText                                3     // A special, limited text format which is only used by Q3TextEdit in an optimized mode.

// enum #define Qt_TextInteractionFlag
// flags #define Qt_TextInteractionFlags
// This enum specifies how a text displaying widget reacts to user input.
//
#define Qt_NoTextInteraction                      0     // No interaction with the text is possible.
#define Qt_TextSelectableByMouse                  1     // Text can be selected with the mouse and copied to the clipboard using a context menu or standard keyboard shortcuts.
#define Qt_TextSelectableByKeyboard               2     // Text can be selected with the cursor keys on the keyboard. A text cursor is shown.
#define Qt_LinksAccessibleByMouse                 4     // Links can be highlighted and activated with the mouse.
#define Qt_LinksAccessibleByKeyboard              8     // Links can be focused using tab and activated with enter.
#define Qt_TextEditable                           16    // The text is fully editable.
#define Qt_TextEditorInteraction                  Qt_TextSelectableByMouse + Qt_TextSelectableByKeyboard + Qt_TextEditable              // The default for a text editor.
#define Qt_TextBrowserInteraction                 Qt_TextSelectableByMouse + Qt_LinksAccessibleByMouse + Qt_LinksAccessibleByKeyboard   // The default for QTextBrowser.
// The TextInteractionFlags type is a typedef for QFlags<TextInteractionFlag>. It stores an OR combination of TextInteractionFlag values.

// enum #define Qt_TimeSpec
//
#define Qt_LocalTime                              0     // Locale dependent time (Timezones and Daylight Savings Time).
#define Qt_UTC                                    1     // Coordinated Universal Time, replaces Greenwich Mean Time.
#define Qt_OffsetFromUTC                          2     // An offset in seconds from Coordinated Universal Time.

// enum #define Qt_ToolBarArea
// flags #define Qt_ToolBarAreas
//
#define Qt_LeftToolBarArea                        0x1
#define Qt_RightToolBarArea                       0x2
#define Qt_TopToolBarArea                         0x4
#define Qt_BottomToolBarArea                      0x8
#define Qt_AllToolBarAreas                        Qt_ToolBarArea_Mask
#define Qt_NoToolBarArea                          0
// The ToolBarAreas type is a typedef for QFlags<ToolBarArea>. It stores an OR combination of ToolBarArea values.

// enum #define Qt_ToolButtonStyle
// The style of the tool button, describing how the button's text and icon should be displayed.
//
#define Qt_ToolButtonIconOnly                     0     // Only display the icon.
#define Qt_ToolButtonTextOnly                     1     // Only display the text.
#define Qt_ToolButtonTextBesideIcon               2     // The text appears beside the icon.
#define Qt_ToolButtonTextUnderIcon                3     // The text appears under the icon.

// enum #define Qt_TransformationMode
// This enum type defines whether image transformations (e.g., scaling) should be smooth or not.
//
#define Qt_FastTransformation                     0     // The transformation is performed quickly, with no smoothing.
#define Qt_SmoothTransformation                   1     // The resulting image is transformed using bilinear filtering.
// See also QImage_scaled().

// enum #define Qt_UIEffect
// This enum describes the available UI effects.
// By default, Qt will try to use the platform specific desktop settings for each effect. Use the QApplication_setDesktopSettingsAware() function (passing false as argument) to prevent this, and the QApplication_setEffectEnabled() to enable or disable a particular effect.
// Note that all effects are disabled on screens running at less than 16-bit color depth.
//
#define Qt_UI_AnimateMenu                         1     // Show animated menus.
#define Qt_UI_FadeMenu                            2     // Show faded menus.
#define Qt_UI_AnimateCombo                        3     // Show animated comboboxes.
#define Qt_UI_AnimateTooltip                      4     // Show tooltip animations.
#define Qt_UI_FadeTooltip                         5     // Show tooltip fading effects.
#define Qt_UI_AnimateToolBox                      6     // Reserved
// See also QApplication_setEffectEnabled() and QApplication_setDesktopSettingsAware().

// enum #define Qt_WhiteSpaceMode
// This enum describes the types of whitespace mode that are used by the QTextDocument class to meet the requirements of different kinds of textual information.
//
#define Qt_WhiteSpaceNormal                       0     // The whitespace mode used to display normal word wrapped text in paragraphs.
#define Qt_WhiteSpacePre                          1     // A preformatted text mode in which whitespace is reproduced exactly.
#define Qt_WhiteSpaceNoWrap                       2
// This enum is defined in the <QTextDocument> header file.

// enum #define Qt_WidgetAttribute
// This enum type is used to specify various widget attributes. Attributes are set and cleared with QWidget_setAttribute(), and queried with QWidget_testAttribute(), although some have special convenience functions which are mentioned below.
//
#define Qt_WA_AcceptDrops                         78    // Allows data from drag and drop operations to be dropped onto the widget (see QWidget_setAcceptDrops()).
#define Qt_WA_AlwaysShowToolTips                  84    // Enables tooltips for inactive windows.
#define Qt_WA_ContentsPropagated                  3     // This flag is superfluous and obsolete; it no longer has any effect. Since Qt 4.1, all widgets that do not set WA_PaintOnScreen propagate their contents.
#define Qt_WA_CustomWhatsThis                     47    // Indicates that the widget wants to continue operating normally in "What's This?" mode. This is set by the widget's author.
#define Qt_WA_DeleteOnClose                       55    // Makes Qt delete this widget when the widget has accepted the close event (see QWidget_closeEvent()).
#define Qt_WA_Disabled                            0     // Indicates that the widget is disabled, i.e. it does not receive any mouse or keyboard events. There is also a getter functions QWidget_isEnabled(). This is set/cleared by the Qt kernel.
#define Qt_WA_ForceDisabled                       32    // Indicates that the widget is explicitly disabled, i.e. it will remain disabled even when all its ancestors are set to the enabled state. This implies WA_Disabled. This is set/cleared by QWidget_setEnabled() and QWidget_setDisabled().
#define Qt_WA_ForceUpdatesDisabled                59    // Indicates that updates are explicitly disabled for the widget; i.e. it will remain disabled even when all its ancestors are set to the updates-enabled state. This implies WA_UpdatesDisabled. This is set/cleared by QWidget_setUpdatesEnabled().
#define Qt_WA_GroupLeader                         72    // This attribute has been deprecated. Use QWidget_windowModality instead.
#define Qt_WA_Hover                               74    // Forces Qt to generate paint events when the mouse enters or leaves the widget. This feature is typically used when implementing custom styles; see the Styles example for details.
#define Qt_WA_InputMethodEnabled                  14    // Enables input methods for Asian languages. Must be set when creating custom text editing widgets. On Windows CE this flag can be used in addition to QApplication_autoSipEnabled to automatically display the SIP when entering a widget.
#define Qt_WA_KeyboardFocusChange                 77    // Set on a toplevel window when the users changes focus with the keyboard (tab, backtab, or shortcut).
#define Qt_WA_KeyCompression                      33    // Enables key event compression if set, and disables it if not set. By default key compression is off, so widgets receive one key press event for each key press (or more, since autorepeat is usually on). If you turn it on and your program doesn't keep up with key input, Qt may try to compress key events so that more than one character can be processed in each event. For example, a word processor widget might receive 2, 3 or more characters in each QKeyEvent_text(), if the layout recalculation takes too long for the CPU. If a widget supports multiple character unicode input, it is always safe to turn the compression on. Qt performs key event compression only for printable characters. #define Qt_Modifier keys, cursor movement keys, function keys and miscellaneous action keys (e.g. Escape, Enter, Backspace, PrintScreen) will stop key event compression, even if there are more compressible key events available. Platforms other than Mac and X11 do not support this compression, in which case turning it on will have no effect. This is set/cleared by the widget's author.
#define Qt_WA_LayoutOnEntireRect                  48    // Indicates that the widget wants QLayout to operate on the entire QWidget_rect(), not only on QWidget_contentsRect(). This is set by the widget's author.
#define Qt_WA_LayoutUsesWidgetRect                92    // Ignore the layout item rect from the style when laying out this widget with QLayout. This makes a difference in QMacStyle and QPlastiqueStyle for some widgets.
#define Qt_WA_MacNoClickThrough                   12    // When a widget that has this attribute set is clicked, and its window is inactive, the click will make the window active but won't be seen by the widget. Typical use of this attribute is on widgets with "destructive" actions, such as a "Delete" button. WA_MacNoClickThrough also applies to all child widgets of the widget that has it set.
#define Qt_WA_MacOpaqueSizeGrip                   85    // Indicates that the native Carbon size grip should be opaque instead of transparent (the default). This attribute is only applicable to Mac OS X and is set by the widget's author.
#define Qt_WA_MacShowFocusRect                    88    // Indicates that this widget should get a QFocusFrame around it. Some widgets draw their own focus halo regardless of this attribute. Not that the QWidget_focusPolicy also plays the main role in whether something is given focus or not, this only controls whether or not this gets the focus frame. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacNormalSize                       89    // Indicates the widget should have the normal size for widgets in Mac OS X. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacSmallSize                        90    // Indicates the widget should have the small size for widgets in Mac OS X. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacMiniSize                         91    // Indicates the widget should have the mini size for widgets in Mac OS X. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacVariableSize                     102   // Indicates the widget can choose between alternative sizes for widgets to avoid clipping. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacBrushedMetal                     46    // Indicates the widget should be drawn in the brushed metal style as supported by the windowing system. This attribute is only applicable to Mac OS X.
#define Qt_WA_Mapped                              11    // Indicates that the widget is mapped on screen. This is set/cleared by the Qt kernel.
#define Qt_WA_MouseNoMask                         71    // Makes the widget receive mouse events for the entire widget regardless of the currently set mask, overriding QWidget_setMask(). This is not applicable for top-level windows.
#define Qt_WA_MouseTracking                       2     // Indicates that the widget has mouse tracking enabled. See QWidget_mouseTracking.
#define Qt_WA_Moved                               43    // Indicates that the widget has an explicit position. This is set/cleared by QWidget_move() and by QWidget_setGeometry().
#define Qt_WA_MSWindowsUseDirect3D                94    // Makes drawing to a widget with this attribute set use the Direct3D paint engine, if the Direct3D paint engine is available. This functionality is experimental.
#define Qt_WA_NoBackground                        Qt_WA_OpaquePaintEvent   // This value is obsolete. Use WA_OpaquePaintEvent instead.
#define Qt_WA_NoChildEventsForParent              58    // Indicates that the widget does not want ChildAdded or ChildRemoved events sent to its parent. This is rarely necessary but can help to avoid automatic insertion widgets like splitters and layouts. This is set by a widget's author.
#define Qt_WA_NoChildEventsFromChildren           39    // Indicates that the widget does not want to receive ChildAdded or ChildRemoved events sent from its children. This is set by a widget's author.
#define Qt_WA_NoMouseReplay                       54    // Used for pop-up widgets. Indicates that the most recent mouse press event should not be replayed when the pop-up widget closes. The flag is set by the widget's author and cleared by the Qt kernel every time the widget receives a new mouse event.
#define Qt_WA_NoMousePropagation                  73    // Prohibits mouse events from being propagated to the widget's parent. This attribute is disabled by default.
#define Qt_WA_TransparentForMouseEvents           51    // When enabled, this attribute disables the delivery of mouse events to the widget and its children. Mouse events are delivered to other widgets as if the widget and its children were not present in the widget hierarchy; mouse clicks and other events effectively "pass through" them. This attribute is disabled by default.
#define Qt_WA_NoSystemBackground                  9     // Indicates that the widget has no background, i.e. when the widget receives paint events, the background is not automatically repainted. Note: Unlike WA_OpaquePaintEvent, newly exposed areas are never filled with the background (e.g., after showing a window for the first time the user can see "through" it until the application processes the paint events). This flag is set or cleared by the widget's author.
#define Qt_WA_OpaquePaintEvent                    4     // Indicates that the widget paints all its pixels when it receives a paint event. Thus, it is not required for operations like updating, resizing, scrolling and focus changes to erase the widget before generating paint events. The use of WA_OpaquePaintEvent provides a small optimization by helping to reduce flicker on systems that do not support double buffering and avoiding computational cycles necessary to erase the background prior to painting. Note: Unlike WA_NoSystemBackground, WA_OpaquePaintEvent makes an effort to avoid transparent window backgrounds. This flag is set or cleared by the widget's author.
#define Qt_WA_OutsideWSRange                      49    // Indicates that the widget is outside the valid range of the window system's coordinate system. A widget outside the valid range cannot be mapped on screen. This is set/cleared by the Qt kernel.
#define Qt_WA_PaintOnScreen                       8     // Indicates that the widget wants to draw directly onto the screen. Widgets with this attribute set do not participate in composition management, i.e. they cannot be semi-transparent or shine through semi-transparent overlapping widgets. Note: This flag is only supported on X11 and it disables double buffering. On Qt for Embedded Linux, the flag only works when set on a top-level widget and it relies on support from the active screen driver. This flag is set or cleared by the widget's author. To render outside of Qt's paint system, e.g., if you require native painting primitives, you need to reimplement QWidget_paintEngine() to return 0 and set this flag.
#define Qt_WA_PaintOutsidePaintEvent              13    // Makes it possible to use QPainter to paint on the widget outside paintEvent(). This flag is not supported on Windows, Mac OS X or Embedded Linux. We recommend that you use it only when porting Qt 3 code to Qt 4.
#define Qt_WA_PaintUnclipped                      52    // Makes all painters operating on this widget unclipped. Children of this widget or other widgets in front of it do not clip the area the painter can paint on. This flag is only supported for widgets with the WA_PaintOnScreen flag set. The preferred way to do this in a cross platform way is to create a transparent widget that lies in front of the other widgets.
#define Qt_WA_PendingMoveEvent                    34    // Indicates that a move event is pending, e.g., when a hidden widget was moved. This flag is set or cleared by the Qt kernel.
#define Qt_WA_PendingResizeEvent                  35    // Indicates that a resize event is pending, e.g., when a hidden widget was resized. This flag is set or cleared by the Qt kernel.
#define Qt_WA_QuitOnClose                         76    // Makes Qt quit the application when the last widget with the attribute set has accepted closeEvent(). This behavior can be modified with the QApplication_quitOnLastWindowClosed property. By default this attribute is set for all widgets of type #define Qt_Window.
#define Qt_WA_Resized                             42    // Indicates that the widget has an explicit size. This flag is set or cleared by QWidget_resize() and QWidget_setGeometry().
#define Qt_WA_RightToLeft                         56    // Indicates that the layout direction for the widget is right to left.
#define Qt_WA_SetCursor                           38    // Indicates that the widget has a cursor of its own. This flag is set or cleared by QWidget_setCursor() and QWidget_unsetCursor().
#define Qt_WA_SetFont                             37    // Indicates that the widget has a font of its own. This flag is set or cleared by QWidget_setFont().
#define Qt_WA_SetPalette                          36    // Indicates that the widget has a palette of its own. This flag is set or cleared by QWidget_setPalette().
#define Qt_WA_SetStyle                            86    // Indicates that the widget has a style of its own. This flag is set or cleared by QWidget_setStyle().
#define Qt_WA_ShowModal                           70    // This attribute has been deprecated. Use QWidget_windowModality instead.
#define Qt_WA_StaticContents                      5     // Indicates that the widget contents are north-west aligned and static. On resize, such a widget will receive paint events only for parts of itself that are newly visible. This flag is set or cleared by the widget's author.
#define Qt_WA_StyleSheet                          97    // Indicates that the widget is styled using a style sheet.
#define Qt_WA_TranslucentBackground               120   // Indicates that the widget should have a translucent background, i.e., any non-opaque regions of the widgets will be translucent because the widget will have an alpha channel. Setting this flag causes WA_NoSystemBackground to be set. This flag is set or cleared by the widget's author.
#define Qt_WA_UnderMouse                          1     // Indicates that the widget is under the mouse cursor. The value is not updated correctly during drag and drop operations. There is also a getter function, QWidget_underMouse(). This flag is set or cleared by the Qt kernel.
#define Qt_WA_UpdatesDisabled                     10    // Indicates that updates are blocked (including the system background). This flag is set or cleared by the Qt kernel.
// Warning: This flag must never be set or cleared by the widget's author.
#define Qt_WA_WindowModified                      41    // Indicates that the window is marked as modified. On some platforms this flag will do nothing, on others (including Mac OS X and Windows) the window will take a modified appearance. This flag is set or cleared by QWidget_setWindowModified().
#define Qt_WA_WindowPropagation                   80    // Makes a toplevel window inherit font and palette from its parent.
#define Qt_WA_MacAlwaysShowToolWindow             96    // On Mac OS X, show the tool window even when the application is not active. By default, all tool windows are hidden when the application is inactive.
#define Qt_WA_SetLocale                           87    // Indicates the locale should be taken into consideration in the widget.
#define Qt_WA_StyledBackground                    93    // Indicates the widget should be drawn using a styled background.
#define Qt_WA_ShowWithoutActivating               98    // Show the widget without making it active.
#define Qt_WA_NativeWindow                        100   // Indicates that a native window is created for the widget. Enabling this flag will also force a native window for the widget's ancestors unless #define Qt_WA_DontCreateNativeAncestors is set.
#define Qt_WA_DontCreateNativeAncestors           101   // Indicates that the widget's ancestors are kept non-native even though the widget itself is native.
#define Qt_WA_X11NetWmWindowTypeDesktop           104   // Adds _NET_WM_WINDOW_TYPE_DESKTOP to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms.
#define Qt_WA_X11NetWmWindowTypeDock              105   // Adds _NET_WM_WINDOW_TYPE_DOCK to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms.
#define Qt_WA_X11NetWmWindowTypeToolBar           106   // Adds _NET_WM_WINDOW_TYPE_TOOLBAR to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automaticaly sets this attribute for QToolBar.
#define Qt_WA_X11NetWmWindowTypeMenu              107   // Adds _NET_WM_WINDOW_TYPE_MENU to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute for QMenu when torn-off.
#define Qt_WA_X11NetWmWindowTypeUtility           108   // Adds _NET_WM_WINDOW_TYPE_UTILITY to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute for the #define Qt_Tool window type.
#define Qt_WA_X11NetWmWindowTypeSplash            109   // Adds _NET_WM_WINDOW_TYPE_SPLASH to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute for the #define Qt_SplashScreen window type.
#define Qt_WA_X11NetWmWindowTypeDialog            110   // Adds _NET_WM_WINDOW_TYPE_DIALOG to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute for the #define Qt_Dialog and #define Qt_Sheet window types.
#define Qt_WA_X11NetWmWindowTypeDropDownMenu      111   // Adds _NET_WM_WINDOW_TYPE_DROPDOWN_MENU to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. \notw Qt automatically sets this attribute for QMenus added to a QMenuBar.
#define Qt_WA_X11NetWmWindowTypePopupMenu         112   // Adds _NET_WM_WINDOW_TYPE_POPUP_MENU to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute for QMenu.
#define Qt_WA_X11NetWmWindowTypeToolTip           113   // Adds _NET_WM_WINDOW_TYPE_TOOLTIP to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute for the #define Qt_ToolTip window type.
#define Qt_WA_X11NetWmWindowTypeNotification      114   // Adds _NET_WM_WINDOW_TYPE_NOTIFICATION to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms.
#define Qt_WA_X11NetWmWindowTypeCombo             115   // Adds _NET_WM_WINDOW_TYPE_COMBO to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute for the QComboBox pop-up.
#define Qt_WA_X11NetWmWindowTypeDND               116   // Adds _NET_WM_WINDOW_TYPE_DND to the window's _NET_WM_WINDOW_TYPE X11 window property. See http://standards.freedesktop.org/wm-spec/ for more details. This attribute has no effect on non-X11 platforms. Note: Qt automatically sets this attribute on the feedback widget used during a drag.
#define Qt_WA_MacFrameworkScaled                  117   // Enables resolution independence aware mode on Mac when using Carbon. This attribute has no effect on Cocoa. The attribute is off by default and can be enabled on a per-window basis.

// enum #define Qt_WindowFrameSection
// This enum is used to describe parts of a window frame. It is returned by QGraphicsWidget_windowFrameSectionAt() to describe what section of the window frame is under the mouse.
//
#define Qt_NoSection                              0
#define Qt_LeftSection                            1
#define Qt_TopLeftSection                         2
#define Qt_TopSection                             3
#define Qt_TopRightSection                        4
#define Qt_RightSection                           5
#define Qt_BottomRightSection                     6
#define Qt_BottomSection                          7
#define Qt_BottomLeftSection                      8
#define Qt_TitleBarArea                           9
// See also QGraphicsWidget_windowFrameEvent(), QGraphicsWidget_paintWindowFrame(), and QGraphicsWidget_windowFrameSectionAt().

// enum #define Qt_WindowModality
// This enum specifies the behavior of a modal window. A modal window is one that blocks input to other windows. Note that windows that are children of a modal window are not blocked.
//
#define Qt_NonModal                               0   // The window is not modal and does not block input to other windows.
#define Qt_WindowModal                            1   // The window is modal to a single window hierarchy and blocks input to its parent window, all grandparent windows, and all siblings of its parent and grandparent windows.
#define Qt_ApplicationModal                       2   // The window is modal to the application and blocks input to all windows.
// See also QWidget_windowModality and QDialog.

// enum #define Qt_WindowState
// flags #define Qt_WindowStates
// This enum type is used to specify the current state of a top-level window.
// The states are
//
#define Qt_WindowNoState                          0x00000000   // The window has no state set (in normal state).
#define Qt_WindowMinimized                        0x00000001   // The window is minimized (i.e. iconified).
#define Qt_WindowMaximized                        0x00000002   // The window is maximized with a frame around it.
#define Qt_WindowFullScreen                       0x00000004   // The window fills the entire screen without any frame around it.
#define Qt_WindowActive                           0x00000008   // The window is the active window, i.e. it has keyboard focus.
// The WindowStates type is a typedef for QFlags<WindowState>. It stores an OR combination of WindowState values.

// enum #define Qt_WindowType
// flags #define Qt_WindowFlags
// This enum type is used to specify various window-system properties for the widget. They are fairly unusual but necessary in a few cases. Some of these flags depend on whether the underlying window manager supports them.
// The main types are
//
#define Qt_Widget                                 0x00000000               // This is the default type for QWidget. Widgets of this type are child widgets if they have a parent, and independent windows if they have no parent. // See also #define Qt_Window and #define Qt_SubWindow.
#define Qt_Window                                 0x00000001               // Indicates that the widget is a window, usually with a window system frame and a title bar, irrespective of whether the widget has a parent or not. Note that it is not possible to unset this flag if the widget does not have a parent.
#define Qt_Dialog                                 0x00000002 + Qt_Window   // Indicates that the widget is a window that should be decorated as a dialog (i.e., typically no maximize or minimize buttons in the title bar). This is the default type for QDialog. If you want to use it as a modal dialog, it should be launched from another window, or have a parent and used with the QWidget_windowModality property. If you make it modal, the dialog will prevent other top-level windows in the application from getting any input. We refer to a top-level window that has a parent as a secondary window.
#define Qt_Sheet                                  0x00000004 + Qt_Window   // Indicates that the widget is a Macintosh sheet.
#define Qt_Drawer                                 0x00000006 + Qt_Window   // Indicates that the widget is a Macintosh drawer.
#define Qt_Popup                                  0x00000008 + Qt_Window   // Indicates that the widget is a pop-up top-level window, i.e. that it is modal, but has a window system frame appropriate for pop-up menus.
#define Qt_Tool                                   0x0000000a + Qt_Window   // Indicates that the widget is a tool window. A tool window is often a small window with a smaller than usual title bar and decoration, typically used for collections of tool buttons. It there is a parent, the tool window will always be kept on top of it. If there isn't a parent, you may consider using #define Qt_WindowStaysOnTopHint as well. If the window system supports it, a tool window can be decorated with a somewhat lighter frame. It can also be combined with #define Qt_FramelessWindowHint.

// On Mac OS X, tool windows correspond to the Floating class of windows. This means that the window lives on a level above normal windows; it impossible to put a normal window on top of it. By default, tool windows will disappear when the application is inactive. This can be controlled by the #define Qt_WA_MacAlwaysShowToolWindow attribute.
//
#define Qt_ToolTip                                0x0000000c + Qt_Window   // Indicates that the widget is a tooltip. This is used internally to implement tooltips.
#define Qt_SplashScreen                           0x0000000e + Qt_Window   // Indicates that the window is a splash screen. This is the default type for QSplashScreen.
#define Qt_Desktop                                0x00000010 + Qt_Window   // Indicates that this widget is the desktop. This is the type for QDesktopWidget.
#define Qt_SubWindow                              0x00000012               // Indicates that this widget is a sub-window, such as a QMdiSubWindow widget.

// There are also a number of flags which you can use to customize the appearance of top-level
// windows. These have no effect on other windows:
//
#define Qt_MSWindowsFixedSizeDialogHint           0x00000100   // Gives the window a thin dialog border on Windows. This style is traditionally used for fixed-size dialogs.
#define Qt_MSWindowsOwnDC                         0x00000200   // Gives the window its own display context on Windows.
#define Qt_X11BypassWindowManagerHint             0x00000400   // Bypass the window manager completely. This results in a borderless window that is not managed at all (i.e., no keyboard input unless you call QWidget_activateWindow() manually).
#define Qt_FramelessWindowHint                    0x00000800   // Produces a borderless window. The user cannot move or resize a borderless window via the window system. On X11, the result of the flag is dependent on the window manager and its ability to understand Motif and/or NETWM hints. Most existing modern window managers can handle this.

// The CustomizeWindowHint flag is used to enable customization of the window controls.
// This flag must be set to allow the WindowTitleHint, WindowSystemMenuHint,
// WindowMinimizeButtonHint, WindowMaximizeButtonHint and WindowCloseButtonHint flags to be changed.
//
#define Qt_CustomizeWindowHint                    0x02000000   // Turns off the default window title hints.
#define Qt_WindowTitleHint                        0x00001000   // Gives the window a title bar.
#define Qt_WindowSystemMenuHint                   0x00002000   // Adds a window system menu, and possibly a close button (for example on Mac). If you need to hide or show a close button, it is more portable to use WindowCloseButtonHint.
#define Qt_WindowMinimizeButtonHint               0x00004000   // Adds a minimize button. On some platforms this implies #define Qt_WindowSystemMenuHint for it to work.
#define Qt_WindowMaximizeButtonHint               0x00008000   // Adds a maximize button. On some platforms this implies #define Qt_WindowSystemMenuHint for it to work.
#define Qt_WindowMinMaxButtonsHint                Qt_WindowMinimizeButtonHint + Qt_WindowMaximizeButtonHint   // Adds a minimize and a maximize button. On some platforms this implies #define Qt_WindowSystemMenuHint for it to work.
#define Qt_WindowCloseButtonHint                  0x08000000   // Adds a close button. On some platforms this implies #define Qt_WindowSystemMenuHint for it to work.
#define Qt_WindowContextHelpButtonHint            0x00010000   // Adds a context help button to dialogs. On some platforms this implies #define Qt_WindowSystemMenuHint for it to work.
#define Qt_MacWindowToolBarButtonHint             0x10000000   // On Mac OS X adds a tool bar button (i.e., the oblong button that is on the top right of windows that have toolbars.
#define Qt_BypassGraphicsProxyWidget              0x20000000   // Prevents the window and its children from automatically embedding themselves into a QGraphicsProxyWidget if the parent widget is already embedded. You can set this flag if you want your widget to always be a toplevel widget on the desktop, regardless of whether the parent widget is embedded in a scene or not.
#define Qt_WindowShadeButtonHint                  0x00020000   //
#define Qt_WindowStaysOnTopHint                   0x00040000   // Informs the window system that the window should stay on top of all other windows. Note that on some window managers on X11 you also have to pass #define Qt_X11BypassWindowManagerHint for this flag to work correctly.
#define Qt_WindowStaysOnBottomHint                0x04000000   // Informs the window system that the window should stay on bottom of all other windows. Note that on X11 this hint will work only in window managers that support _NET_WM_STATE_BELOW atom. If a window always on the bottom has a parent, the parent will also be left on the bottom. This window hint is currently not implemented for Mac OS X.
#define Qt_WindowOkButtonHint                     0x00080000   // Adds an OK button to the window decoration of a dialog. Only supported for Windows CE.
#define Qt_WindowCancelButtonHint                 0x00100000   // Adds a Cancel button to the window decoration of a dialog. Only supported for Windows CE.
#define Qt_WindowType_Mask                        0x000000ff   // A mask for extracting the window type part of the window flags.
                                                               //
#define Qt_WMouseNoMask                           0x00080000   // Use #define Qt_WA_MouseNoMask instead.
#define Qt_WDestructiveClose                      0x00100000   // Use #define Qt_WA_DeleteOnClose instead.
#define Qt_WStaticContents                        0x00200000   // Use #define Qt_WA_StaticContents instead.
#define Qt_WGroupLeader                           0x00400000   // No longer needed.
#define Qt_WShowModal                             0x00800000   // Use QWidget_windowModality instead.
#define Qt_WNoMousePropagation                    0x01000000   // Use #define Qt_WA_NoMousePropagation instead.
#define Qt_WType_TopLevel                         Qt_Window    // Use #define Qt_Window instead.
#define Qt_WType_Dialog                           Qt_Dialog    // Use #define Qt_Dialog instead.
#define Qt_WType_Popup                            Qt_Popup     // Use #define Qt_Popup instead.
#define Qt_WType_Desktop                          Qt_Desktop   // Use #define Qt_Desktop instead.
#define Qt_WType_Mask                             Qt_WindowType_Mask   // Use Qt_WindowType_Mask instead.
#define Qt_WStyle_Customize                       0            // No longer needed.
#define Qt_WStyle_NormalBorder                    0            // No longer needed.
#define Qt_WStyle_DialogBorder                    Qt_MSWindowsFixedSizeDialogHint   // Use #define Qt_MSWindowsFixedSizeDialogHint instead.
#define Qt_WStyle_NoBorder                        Qt_FramelessWindowHint        // Use #define Qt_FramelessWindowHint instead.
#define Qt_WStyle_Title                           Qt_WindowTitleHint            // Use #define Qt_WindowTitleHint instead.
#define Qt_WStyle_SysMenu                         Qt_WindowSystemMenuHint       // Use #define Qt_WindowSystemMenuHint instead.
#define Qt_WStyle_Minimize                        Qt_WindowMinimizeButtonHint   // Use #define Qt_WindowMinimizeButtonHint instead.
#define Qt_WStyle_Maximize                        Qt_WindowMaximizeButtonHint   // Use #define Qt_WindowMaximizeButtonHint instead.
#define Qt_WStyle_MinMax                          Qt_WStyle_Minimize + WStyle_Maximize   // Use #define Qt_WindowMinMaxButtonsHint instead.
#define Qt_WStyle_Tool                            Qt_Tool                        // Use #define Qt_Tool instead.
#define Qt_WStyle_StaysOnTop                      Qt_WindowStaysOnTopHint        // Use #define Qt_WindowStaysOnTopHint instead.
#define Qt_WStyle_ContextHelp                     Qt_WindowContextHelpButtonHint // Use #define Qt_WindowContextHelpButtonHint instead.
#define Qt_WPaintDesktop                          0                   // No longer needed.
#define Qt_WPaintClever                           0                   // No longer needed.
#define Qt_WX11BypassWM                           Qt_X11BypassWindowManagerHint  // Use #define Qt_X11BypassWindowManagerHint instead.
#define Qt_WWinOwnDC                              Qt_MSWindowsOwnDC   // Use #define Qt_MSWindowsOwnDC instead.
#define Qt_WMacSheet                              Qt_Sheet            // Use #define Qt_Sheet instead.
#define Qt_WMacDrawer                             Qt_Drawer           // Use #define Qt_Drawer instead.
#define Qt_WStyle_Splash                          Qt_SplashScreen     // Use #define Qt_SplashScreen instead.
#define Qt_WNoAutoErase                           0                   // No longer needed.
#define Qt_WRepaintNoErase                        0                   // No longer needed.
#define Qt_WNorthWestGravity                      Qt_WStaticContents  // Use #define Qt_WA_StaticContents instead.
#define Qt_WType_Modal                            Qt_Dialog + Qt_WShowModal   // Use Qt_Dialog and QWidget_windowModality instead.
#define Qt_WStyle_Dialog                          Qt_Dialog                   // Use Qt_Dialog instead.
#define Qt_WStyle_NoBorderEx                      Qt_FramelessWindowHint      // Use Qt_FramelessWindowHint instead.
#define Qt_WResizeNoErase                         0                   // No longer needed.
#define Qt_WMacNoSheet                            0                   // No longer needed.


// Frame Shadow
#define QFrame_Plain                              0x0010   // the frame and contents appear level with the surroundings; draws using the palette QPalette::WindowText color (without any 3D effect)
#define QFrame_Raised                             0x0020   // the frame and contents appear raised; draws a 3D raised line using the light and dark colors of the current color group
#define QFrame_Sunken                             0x0030   // the frame and contents appear sunken; draws a 3D sunken line using the light and dark colors of the current color group
// Frame Shape
#define QFrame_NoFrame                            0        // QFrame draws nothing
#define QFrame_Box                                0x0001   // QFrame draws a box around its contents
#define QFrame_Panel                              0x0002   // QFrame draws a panel to make the contents appear raised or sunken
#define QFrame_StyledPanel                        0x0006   // draws a rectangular panel with a look that depends on the current GUI style. It can be raised or sunken.
#define QFrame_HLine                              0x0004   // QFrame draws a horizontal line that frames nothing (useful as separator)
#define QFrame_VLine                              0x0005   // QFrame draws a vertical line that frames nothing (useful as separator)
#define QFrame_WinPanel                           0x0003   // draws a rectangular panel that can be
               // raised or sunken like those in Windows 95. Specifying this shape sets the
               // line width to 2 pixels. WinPanel is provided for compatibility. For GUI style
               // independence we recommend using StyledPanel instead.
// Frame Style Mask
#define QFrame_Shadow_Mask                        0x00f0   // The Shadow part of frameStyle()
#define QFrame_Shape_Mask                         0x000f   // The Shape part of frameStyle()


// enum #define QWebPage_Extension
// This enum describes the types of extensions that the page can support. Before using these extensions, you should verify that the extension is supported by calling supportsExtension().
//
#define QWebPage_ChooseMultipleFilesExtension     0    // Whether the web page supports multiple file selection. This extension is invoked when the web content requests one or more file names, for example as a result of the user clicking on a "file upload" button in a HTML form where multiple file selection is allowed.

// enum QWebPage::FindFlag
// flags QWebPage::FindFlags
// This enum describes the options available to QWebPage's findText() function. The options can be OR-ed together from the following list:
//
#define QWebPage_FindBackward                     1    // Searches backwards instead of forwards.
#define QWebPage_FindCaseSensitively              2    // By default findText() works case insensitive. Specifying this option changes the behaviour to a case sensitive find operation.
#define QWebPage_FindWrapsAroundDocument          4    // Makes findText() restart from the beginning of the document if the end was reached and the text was not found.
// The FindFlags type is a typedef for QFlags<FindFlag>. It stores an OR combination of FindFlag values.

// enum QWebPage::LinkDelegationPolicy
// This enum defines the delegation policies a webpage can have when activating links and emitting the linkClicked() signal.
//
#define QWebPage_DontDelegateLinks                0    // No links are delegated. Instead, QWebPage tries to handle them all.
#define QWebPage_DelegateExternalLinks            1    // When activating links that point to documents not stored on the local filesystem or an equivalent - such as the Qt resource system - then linkClicked() is emitted.
#define QWebPage_DelegateAllLinks                 2    // Whenever a link is activated the linkClicked() signal is emitted.

// enum QWebPage::NavigationType
// This enum describes the types of navigation available when browsing through hyperlinked documents.
//
#define QWebPage_NavigationTypeLinkClicked        0    // The user clicked on a link or pressed return on a focused link.
#define QWebPage_NavigationTypeFormSubmitted      1    // The user activated a submit button for an HTML form.
#define QWebPage_NavigationTypeBackOrForward      2    // Navigation to a previously shown document in the back or forward history is requested.
#define QWebPage_NavigationTypeReload             3    // The user activated the reload action.
#define QWebPage_NavigationTypeFormResubmitted    4    // An HTML form was submitted a second time.
#define QWebPage_NavigationTypeOther              5    // A navigation to another document using a method not listed above.

// enum QWebPage::WebAction
// This enum describes the types of action which can be performed on the web page. Actions which are related to text editing, cursor movement, and text selection only have an effect if contentEditable is true.
//
#define QWebPage_NoWebAction                      -1   // No action is triggered.
#define QWebPage_OpenLink                         0    // Open the current link.
#define QWebPage_OpenLinkInNewWindow              1    // Open the current link in a new window.
#define QWebPage_OpenFrameInNewWindow             2    // Replicate the current frame in a new window.
#define QWebPage_DownloadLinkToDisk               3    // Download the current link to the disk.
#define QWebPage_CopyLinkToClipboard              4    // Copy the current link to the clipboard.
#define QWebPage_OpenImageInNewWindow             5    // Open the highlighted image in a new window.
#define QWebPage_DownloadImageToDisk              6    // Download the highlighted image to the disk.
#define QWebPage_CopyImageToClipboard             7    // Copy the highlighted image to the clipboard.
#define QWebPage_Back                             8    // Navigate back in the history of navigated links.
#define QWebPage_Forward                          9    // Navigate forward in the history of navigated links.
#define QWebPage_Stop                             10   // Stop loading the current page.
#define QWebPage_Reload                           11   // Reload the current page.
#define QWebPage_Cut                              12   // Cut the content currently selected into the clipboard.
#define QWebPage_Copy                             13   // Copy the content currently selected into the clipboard.
#define QWebPage_Paste                            14   // Paste content from the clipboard.
#define QWebPage_Undo                             15   // Undo the last editing action.
#define QWebPage_Redo                             16   // Redo the last editing action.
#define QWebPage_MoveToNextChar                   17   // Move the cursor to the next character.
#define QWebPage_MoveToPreviousChar               18   // Move the cursor to the previous character.
#define QWebPage_MoveToNextWord                   19   // Move the cursor to the next word.
#define QWebPage_MoveToPreviousWord               20   // Move the cursor to the previous word.
#define QWebPage_MoveToNextLine                   21   // Move the cursor to the next line.
#define QWebPage_MoveToPreviousLine               22   // Move the cursor to the previous line.
#define QWebPage_MoveToStartOfLine                23   // Move the cursor to the start of the line.
#define QWebPage_MoveToEndOfLine                  24   // Move the cursor to the end of the line.
#define QWebPage_MoveToStartOfBlock               25   // Move the cursor to the start of the block.
#define QWebPage_MoveToEndOfBlock                 26   // Move the cursor to the end of the block.
#define QWebPage_MoveToStartOfDocument            27   // Move the cursor to the start of the document.
#define QWebPage_MoveToEndOfDocument              28   // Move the cursor to the end of the document.
#define QWebPage_SelectNextChar                   29   // Select to the next character.
#define QWebPage_SelectPreviousChar               30   // Select to the previous character.
#define QWebPage_SelectNextWord                   31   // Select to the next word.
#define QWebPage_SelectPreviousWord               32   // Select to the previous word.
#define QWebPage_SelectNextLine                   33   // Select to the next line.
#define QWebPage_SelectPreviousLine               34   // Select to the previous line.
#define QWebPage_SelectStartOfLine                35   // Select to the start of the line.
#define QWebPage_SelectEndOfLine                  36   // Select to the end of the line.
#define QWebPage_SelectStartOfBlock               37   // Select to the start of the block.
#define QWebPage_SelectEndOfBlock                 38   // Select to the end of the block.
#define QWebPage_SelectStartOfDocument            39   // Select to the start of the document.
#define QWebPage_SelectEndOfDocument              40   // Select to the end of the document.
#define QWebPage_DeleteStartOfWord                41   // Delete to the start of the word.
#define QWebPage_DeleteEndOfWord                  42   // Delete to the end of the word.
#define QWebPage_SetTextDirectionDefault          43   // Set the text direction to the default direction.
#define QWebPage_SetTextDirectionLeftToRight      44   // Set the text direction to left-to-right.
#define QWebPage_SetTextDirectionRightToLeft      45   // Set the text direction to right-to-left.
#define QWebPage_ToggleBold                       46   // Toggle the formatting between bold and normal weight.
#define QWebPage_ToggleItalic                     47   // Toggle the formatting between italic and normal style.
#define QWebPage_ToggleUnderline                  48   // Toggle underlining.
#define QWebPage_InspectElement                   49   // Show the Web Inspector with the currently highlighted HTML element.
#define QWebPage_InsertParagraphSeparator         50   // Insert a new paragraph.
#define QWebPage_InsertLineSeparator              51   // Insert a new line.
#define QWebPage_SelectAll                        52   // Selects all content.

// enum QWebPage::WebWindowType
//
#define QWebPage_WebBrowserWindow                 0    // The window is a regular web browser window.
#define QWebPage_WebModalDialog                   1    // The window acts as modal dialog.

/*----------------------------------------------------------------------*/

#define _HBQT_CH
#endif
