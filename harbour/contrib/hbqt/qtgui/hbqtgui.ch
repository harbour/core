/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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

#ifndef _HBQTGUI_CH
#define _HBQTGUI_CH

#define QEvent_None                               0        // Not an event.
#define QEvent_Timer                              1        // Regular timer events (QTimerEvent).
#define QEvent_MouseButtonPress                   2        // Mouse press (QMouseEvent).
#define QEvent_MouseButtonRelease                 3        // Mouse release (QMouseEvent).
#define QEvent_MouseButtonDblClick                4        // Mouse press again (QMouseEvent).
#define QEvent_MouseMove                          5        // Mouse move (QMouseEvent).
#define QEvent_KeyPress                           6        // Key press (QKeyEvent).
#define QEvent_KeyRelease                         7        // Key release (QKeyEvent).
#define QEvent_FocusIn                            8        // Widget gains keyboard focus (QFocusEvent).
#define QEvent_FocusOut                           9        // Widget loses keyboard focus (QFocusEvent).
#define QEvent_Enter                              10       // Mouse enters widget's boundaries.
#define QEvent_Leave                              11       // Mouse leaves widget's boundaries.
#define QEvent_Paint                              12       // Screen update necessary (QPaintEvent).
#define QEvent_Move                               13       // Widget's position changed (QMoveEvent).
#define QEvent_Resize                             14       // Widget's size changed (QResizeEvent).
#define QEvent_Show                               17       // Widget was shown on screen (QShowEvent).
#define QEvent_Hide                               18       // Widget was hidden (QHideEvent).
#define QEvent_Close                              19       // Widget was closed (QCloseEvent).
#define QEvent_ParentChange                       21       // The widget parent has changed.
#define QEvent_WindowActivate                     24       // Window was activated.
#define QEvent_WindowDeactivate                   25       // Window was deactivated.
#define QEvent_ShowToParent                       26       // A child widget has been shown.
#define QEvent_HideToParent                       27       // A child widget has been hidden.
#define QEvent_Wheel                              31       // Mouse wheel rolled (QWheelEvent).
#define QEvent_WindowTitleChange                  33       // The window title has changed.
#define QEvent_WindowIconChange                   34       // The window's icon has changed.
#define QEvent_ApplicationWindowIconChange        35       // The application's icon has changed.
#define QEvent_ApplicationFontChange              36       // The default application font has changed.
#define QEvent_ApplicationLayoutDirectionChange   37       // The default application layout direction has changed.
#define QEvent_ApplicationPaletteChange           38       // The default application palette has changed.
#define QEvent_PaletteChange                      39       // Palette of the widget changed.
#define QEvent_Clipboard                          40       // The clipboard contents have changed (QClipboardEvent).
#define QEvent_MetaCall                           43       // An asynchronous method invocation via QMetaObject_invokeMethod().
#define QEvent_SockAct                            50       // Socket activated, used to implement QSocketNotifier.
#define QEvent_ShortcutOverride                   51       // Key press in child, for overriding shortcut key handling (QKeyEvent).
#define QEvent_DeferredDelete                     52       // The object will be deleted after it has cleaned up.
#define QEvent_DragEnter                          60       // The cursor enters a widget during a drag and drop operation (QDragEnterEvent).
#define QEvent_DragLeave                          62       // The cursor leaves a widget during a drag and drop operation (QDragLeaveEvent).
#define QEvent_DragMove                           61       // A drag and drop operation is in progress (QDragMoveEvent).
#define QEvent_Drop                               63       // A drag and drop operation is completed (QDropEvent).
#define QEvent_ChildAdded                         68       // An object gets a child (QChildEvent).
#define QEvent_ChildPolished                      69       // A widget child gets polished (QChildEvent).
#define QEvent_ChildInserted                      70       // An object gets a child (QChildEvent). Qt3Support only, use ChildAdded instead.
#define QEvent_ChildRemoved                       71       // An object loses a child (QChildEvent).
#define QEvent_PolishRequest                      74       // The widget should be polished.
#define QEvent_Polish                             75       // The widget is polished.
#define QEvent_LayoutRequest                      76       // Widget layout needs to be redone.
#define QEvent_UpdateRequest                      77       // The widget should be repainted.
#define QEvent_UpdateLater                        78       // The widget should be queued to be repainted at a later time.
#define QEvent_ContextMenu                        82       // Context popup menu (QContextMenuEvent).
#define QEvent_InputMethod                        83       // An input method is being used (QInputMethodEvent).
#define QEvent_AccessibilityPrepare               86       // Accessibility information is requested.
#define QEvent_TabletMove                         87       // Wacom tablet move (QTabletEvent).
#define QEvent_LocaleChange                       88       // The system locale has changed.
#define QEvent_LanguageChange                     89       // The application translation changed.
#define QEvent_LayoutDirectionChange              90       // The direction of layouts changed.
#define QEvent_TabletPress                        92       // Wacom tablet press (QTabletEvent).
#define QEvent_TabletRelease                      93       // Wacom tablet release (QTabletEvent).
#define QEvent_OkRequest                          94       // Ok button in decoration pressed. Supported only for Windows CE.
#define QEvent_IconDrag                           96       // The main icon of a window has been dragged away (QIconDragEvent).
#define QEvent_FontChange                         97       // Widget's font has changed.
#define QEvent_EnabledChange                      98       // Widget's enabled state has changed.
#define QEvent_ActivationChange                   99       // A widget's top-level window activation state has changed.
#define QEvent_StyleChange                        100      // Widget's style has been changed.
#define QEvent_IconTextChange                     101      // Widget's icon text has been changed.
#define QEvent_ModifiedChange                     102      // Widgets modification state has been changed.
#define QEvent_WindowBlocked                      103      // The window is blocked by a modal dialog.
#define QEvent_WindowUnblocked                    104      // The window is unblocked after a modal dialog exited.
#define QEvent_WindowStateChange                  105      // The window's state (minimized, maximized or full-screen) has changed (QWindowStateChangeEvent).
#define QEvent_MouseTrackingChange                109      // The mouse tracking state has changed.
#define QEvent_ToolTip                            110      // A tooltip was requested (QHelpEvent).
#define QEvent_WhatsThis                          111      // The widget should reveal "What's This?" help (QHelpEvent).
#define QEvent_StatusTip                          112      // A status tip is requested (QStatusTipEvent).
#define QEvent_ActionChanged                      113      // An action has been changed (QActionEvent).
#define QEvent_ActionAdded                        114      // A new action has been added (QActionEvent).
#define QEvent_ActionRemoved                      115      // An action has been removed (QActionEvent).
#define QEvent_FileOpen                           116      // File open request (QFileOpenEvent).
#define QEvent_Shortcut                           117      // Key press in child for shortcut key handling (QShortcutEvent).
#define QEvent_WhatsThisClicked                   118      // A link in a widget's "What's This?" help was clicked.
#define QEvent_AccessibilityHelp                  119      // Used to query accessibility help texts (QAccessibleEvent).
#define QEvent_ToolBarChange                      120      // The toolbar button is toggled on Mac OS X.
#define QEvent_ApplicationActivate                121      // The application has been made available to the user.
#define QEvent_ApplicationActivated               121      // This enum has been deprecated. Use ApplicationActivate instead.
#define QEvent_ApplicationDeactivate              122      // The application has been suspended, and is unavailable to the user.
#define QEvent_QueryWhatsThis                     123      // The widget should accept the event if it has "What's This?" help.
#define QEvent_EnterWhatsThisMode                 124      // Send to toplevel widgets when the application enters "What's This?" mode.
#define QEvent_LeaveWhatsThisMode                 125      // Send to toplevel widgets when the application leaves "What's This?" mode.
#define QEvent_ZOrderChange                       126      // The widget's z-order has changed. This event is never sent to top level windows.
#define QEvent_HoverEnter                         127      // The mouse cursor enters a hover widget (QHoverEvent).
#define QEvent_HoverLeave                         128      // The mouse cursor leaves a hover widget (QHoverEvent).
#define QEvent_HoverMove                          129      // The mouse cursor moves inside a hover widget (QHoverEvent).
#define QEvent_AccessibilityDescription           130      // Used to query accessibility description texts (QAccessibleEvent).
#define QEvent_ParentAboutToChange                131      // The widget parent is about to change.
#define QEvent_WinEventAct                        132      // A Windows-specific activation event has occurred.
#define QEvent_EnterEditFocus                     150      // An editor widget gains focus for editing.
#define QEvent_LeaveEditFocus                     151      // An editor widget loses focus for editing.
#define QEvent_MenubarUpdated                     153      // The window's menu bar has been updated.
#define QEvent_GraphicsSceneMouseMove             155      // Move mouse in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneMousePress            156      // Mouse press in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneMouseRelease          157      // Mouse release in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneMouseDoubleClick      158      // Mouse press again (double click) in a graphics scene (QGraphicsSceneMouseEvent).
#define QEvent_GraphicsSceneContextMenu           159      // Context popup menu over a graphics scene (QGraphicsSceneContextMenuEvent).
#define QEvent_GraphicsSceneHoverEnter            160      // The mouse cursor enters a hover item in a graphics scene (QGraphicsSceneHoverEvent).
#define QEvent_GraphicsSceneHoverMove             161      // The mouse cursor moves inside a hover item in a graphics scene (QGraphicsSceneHoverEvent).
#define QEvent_GraphicsSceneHoverLeave            162      // The mouse cursor leaves a hover item in a graphics scene (QGraphicsSceneHoverEvent).
#define QEvent_GraphicsSceneHelp                  163      // The user requests help for a graphics scene (QHelpEvent).
#define QEvent_GraphicsSceneDragEnter             164      // The cursor enters a graphics scene during a drag and drop operation.
#define QEvent_GraphicsSceneDragMove              165      // A drag and drop operation is in progress over a scene.
#define QEvent_GraphicsSceneDragLeave             166      // The cursor leaves a graphics scene during a drag and drop operation.
#define QEvent_GraphicsSceneDrop                  167      // A drag and drop operation is completed over a scene.
#define QEvent_GraphicsSceneWheel                 168      // Mouse wheel rolled in a graphics scene (QGraphicsSceneWheelEvent).
#define QEvent_KeyboardLayoutChange               169      // The keyboard layout has changed.
#define QEvent_DynamicPropertyChange              170      // A dynamic property was added, changed or removed from the object. User events should have values b
#define QEvent_TabletEnterProximity               171      // Wacom tablet enter proximity event (QTabletEvent), sent to QApplication.
#define QEvent_TabletLeaveProximity               172      // Wacom tablet leave proximity event (QTabletEvent), sent to QApplication.
#define QEvent_NonClientAreaMouseMove             173      // A mouse move occurred outside the client area.
#define QEvent_NonClientAreaMouseButtonPress      174      // A mouse button press occurred outside the client area.
#define QEvent_NonClientAreaMouseButtonRelease    175      // A mouse button release occurred outside the client area.
#define QEvent_NonClientAreaMouseButtonDblClick   176      // A mouse double click occurred outside the client area.
#define QEvent_MacSizeChange                      177      // The user changed his widget sizes (Mac OS X only).
#define QEvent_ContentsRectChange                 178      // The margins of the widget's content rect changed.
#define QEvent_GraphicsSceneResize                181      // Widget was resized (QGraphicsSceneResizeEvent).
#define QEvent_GraphicsSceneMove                  182      // Widget was moved (QGraphicsSceneMoveEvent).
#define QEvent_CursorChange                       183      // The widget's cursor has changed.
#define QEvent_ToolTipChange                      184      // The widget's tooltip has changed.
#define QEvent_GrabMouse                          186      // Item gains mouse grab (QGraphicsItem only).
#define QEvent_UngrabMouse                        187      // Item loses mouse grab (QGraphicsItem only).
#define QEvent_GrabKeyboard                       188      // Item gains keyboard grab (QGraphicsItem only).
#define QEvent_UngrabKeyboard                     189      // Item loses keyboard grab (QGraphicsItem only).

#define QPalette_WindowText                       0        // A general foreground color.
#define QPalette_Foreground                       0        // This value is obsolete. Use WindowText instead.
#define QPalette_Button                           1        // The general button background color. This background can be different from Window as some styles require a different background color for buttons.
#define QPalette_Text                             6        // The foreground color used with Base. This is usually the same as the WindowText, in which case it must provide good contrast with Window and Base.
#define QPalette_BrightText                       7        // A text color that is very different from WindowText, and contrasts well with e.g. Dark. Typically used for text that needs to be drawn where Text or WindowText would give poor contrast, such as on pressed push buttons. Note that text colors can be used for things other than just words; text colors are usually used for text, but it's quite common to use the text color roles for lines, icons, etc.
#define QPalette_ButtonText                       8        // A foreground color used with the Button color.
#define QPalette_Base                             9        // Used mostly as the background color for text entry widgets, but can also be used for other painting - such as the background of combobox drop down lists and toolbar handles. It is usually white or another light color.
#define QPalette_Window                           10       // A general background color.
#define QPalette_Background                       10       // This value is obsolete. Use Window instead.
#define QPalette_AlternateBase                    16       // Used as the alternate background color in views with alternating row colors (see QAbstractItemView_setAlternatingRowColors()).
#define QPalette_ToolTipBase                      18       // Used as the background color for QToolTip and QWhatsThis. Tool tips use the Inactive color group of QPalette, because tool tips are not active windows.
#define QPalette_ToolTipText                      19       // Used as the foreground color for QToolTip and QWhatsThis. Tool tips use the Inactive color group of QPalette, because tool tips are not active windows.

/*
 * There are some color roles used mostly for 3D bevel and shadow effects.
 * All of these are normally derived from Window, and used in ways that depend on that
 * relationship. For example, buttons depend on it to make the bevels look attractive,
 * and Motif scroll bars depend on Mid to be slightly different from Window.
 */
#define QPalette_Light                            2        // Lighter than Button color.
#define QPalette_Midlight                         3        // Between Button and Light.
#define QPalette_Dark                             4        // Darker than Button.
#define QPalette_Mid                              5        // Between Button and Dark.
#define QPalette_Shadow                           11       // A very dark color. By default, the shadow color is Qt_black.
#define QPalette_Highlight                        12       // A color to indicate a selected item or the current item. By default, the highlight color is Qt_darkBlue.
#define QPalette_HighlightedText                  13       // A text color that contrasts with Highlight. By default, the highlighted text color is Qt_white.
#define QPalette_Link                             14       // A text color used for unvisited hyperlinks. By default, the link color is Qt_blue.
#define QPalette_LinkVisited                      15       // A text color used for already visited hyperlinks. By default, the linkvisited color is Qt_magenta.
#define QPalette_NoRole                           17       // No role; this special role is often used to indicate that a role has not been assigned.


#define QAbstractSlider_SliderNoAction            0
#define QAbstractSlider_SliderSingleStepAdd       1
#define QAbstractSlider_SliderSingleStepSub       2
#define QAbstractSlider_SliderPageStepAdd         3
#define QAbstractSlider_SliderPageStepSub         4
#define QAbstractSlider_SliderToMinimum           5
#define QAbstractSlider_SliderToMaximum           6
#define QAbstractSlider_SliderMove                7

#define QAbstractSlider_SliderRangeChange         0
#define QAbstractSlider_SliderOrientationChange   1
#define QAbstractSlider_SliderStepsChange         2
#define QAbstractSlider_SliderValueChange         3

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

#define Qt_AlignCenter                            hb_bitOR( Qt_AlignVCenter, Qt_AlignHCenter )  // Centers in both dimensions.

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
#define Qt_StrongFocus                            hb_bitOR( Qt_TabFocus, Qt_ClickFocus, 0x8 )  // the widget accepts focus by both tabbing and clicking. On Mac OS X this will also be indicate that the widget accepts tab focus when in 'Text/List focus mode'.
#define Qt_WheelFocus                             hb_bitOR( Qt_StrongFocus, 0x4 )            // like #define Qt_StrongFocus plus the widget accepts focus by using the mouse wheel.
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
#define Qt_TextEditorInteraction                  hb_bitOR( Qt_TextSelectableByMouse, Qt_TextSelectableByKeyboard, Qt_TextEditable )             // The default for a text editor.
#define Qt_TextBrowserInteraction                 hb_bitOR( Qt_TextSelectableByMouse, Qt_LinksAccessibleByMouse, Qt_LinksAccessibleByKeyboard )  // The default for QTextBrowser.
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
#define Qt_Dialog                                 hb_bitOR( 0x00000002, Qt_Window )  // Indicates that the widget is a window that should be decorated as a dialog (i.e., typically no maximize or minimize buttons in the title bar). This is the default type for QDialog. If you want to use it as a modal dialog, it should be launched from another window, or have a parent and used with the QWidget_windowModality property. If you make it modal, the dialog will prevent other top-level windows in the application from getting any input. We refer to a top-level window that has a parent as a secondary window.
#define Qt_Sheet                                  hb_bitOR( 0x00000004, Qt_Window )  // Indicates that the widget is a Macintosh sheet.
#define Qt_Drawer                                 hb_bitOR( 0x00000006, Qt_Window )  // Indicates that the widget is a Macintosh drawer.
#define Qt_Popup                                  hb_bitOR( 0x00000008, Qt_Window )  // Indicates that the widget is a pop-up top-level window, i.e. that it is modal, but has a window system frame appropriate for pop-up menus.
#define Qt_Tool                                   hb_bitOR( 0x0000000a, Qt_Window )  // Indicates that the widget is a tool window. A tool window is often a small window with a smaller than usual title bar and decoration, typically used for collections of tool buttons. It there is a parent, the tool window will always be kept on top of it. If there isn't a parent, you may consider using #define Qt_WindowStaysOnTopHint as well. If the window system supports it, a tool window can be decorated with a somewhat lighter frame. It can also be combined with #define Qt_FramelessWindowHint.

// On Mac OS X, tool windows correspond to the Floating class of windows. This means that the window lives on a level above normal windows; it impossible to put a normal window on top of it. By default, tool windows will disappear when the application is inactive. This can be controlled by the #define Qt_WA_MacAlwaysShowToolWindow attribute.
//
#define Qt_ToolTip                                hb_bitOR( 0x0000000c, Qt_Window )  // Indicates that the widget is a tooltip. This is used internally to implement tooltips.
#define Qt_SplashScreen                           hb_bitOR( 0x0000000e, Qt_Window )  // Indicates that the window is a splash screen. This is the default type for QSplashScreen.
#define Qt_Desktop                                hb_bitOR( 0x00000010, Qt_Window )  // Indicates that this widget is the desktop. This is the type for QDesktopWidget.
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
#define Qt_WindowMinMaxButtonsHint                hb_bitOR( Qt_WindowMinimizeButtonHint, Qt_WindowMaximizeButtonHint )  // Adds a minimize and a maximize button. On some platforms this implies #define Qt_WindowSystemMenuHint for it to work.
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
#define Qt_WStyle_MinMax                          hb_bitOR( Qt_WStyle_Minimize, WStyle_Maximize )  // Use #define Qt_WindowMinMaxButtonsHint instead.
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
#define Qt_WType_Modal                            hb_bitOR( Qt_Dialog, Qt_WShowModal )  // Use Qt_Dialog and QWidget_windowModality instead.
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


#define QStyle_SP_TitleBarMinButton               1    // Minimize button on title bars (e.g., in QWorkspace).
#define QStyle_SP_TitleBarMenuButton              0    // Menu button on a title bar.
#define QStyle_SP_TitleBarMaxButton               2    // Maximize button on title bars.
#define QStyle_SP_TitleBarCloseButton             3    // Close button on title bars.
#define QStyle_SP_TitleBarNormalButton            4    // Normal (restore) button on title bars.
#define QStyle_SP_TitleBarShadeButton             5    // Shade button on title bars.
#define QStyle_SP_TitleBarUnshadeButton           6    // Unshade button on title bars.
#define QStyle_SP_TitleBarContextHelpButton       7    // The Context help button on title bars.
#define QStyle_SP_MessageBoxInformation           9    // The "information" icon.
#define QStyle_SP_MessageBoxWarning               10   // The "warning" icon.
#define QStyle_SP_MessageBoxCritical              11   // The "critical" icon.
#define QStyle_SP_MessageBoxQuestion              12   // The "question" icon.
#define QStyle_SP_DesktopIcon                     13   // The "desktop" icon.
#define QStyle_SP_TrashIcon                       14   // The "trash" icon.
#define QStyle_SP_ComputerIcon                    15   // The "My computer" icon.
#define QStyle_SP_DriveFDIcon                     16   // The floppy icon.
#define QStyle_SP_DriveHDIcon                     17   // The harddrive icon.
#define QStyle_SP_DriveCDIcon                     18   // The CD icon.
#define QStyle_SP_DriveDVDIcon                    19   // The DVD icon.
#define QStyle_SP_DriveNetIcon                    20   // The network icon.
#define QStyle_SP_DirHomeIcon                     55   // The home directory icon.
#define QStyle_SP_DirOpenIcon                     21   // The open directory icon.
#define QStyle_SP_DirClosedIcon                   22   // The closed directory icon.
#define QStyle_SP_DirIcon                         37   // The directory icon.
#define QStyle_SP_DirLinkIcon                     23   // The link to directory icon.
#define QStyle_SP_FileIcon                        24   // The file icon.
#define QStyle_SP_FileLinkIcon                    25   // The link to file icon.
#define QStyle_SP_FileDialogStart                 28   // The "start" icon in a file dialog.
#define QStyle_SP_FileDialogEnd                   29   // The "end" icon in a file dialog.
#define QStyle_SP_FileDialogToParent              30   // The "parent directory" icon in a file dialog.
#define QStyle_SP_FileDialogNewFolder             31   // The "create new folder" icon in a file dialog.
#define QStyle_SP_FileDialogDetailedView          32   // The detailed view icon in a file dialog.
#define QStyle_SP_FileDialogInfoView              33   // The file info icon in a file dialog.
#define QStyle_SP_FileDialogContentsView          34   // The contents view icon in a file dialog.
#define QStyle_SP_FileDialogListView              35   // The list view icon in a file dialog.
#define QStyle_SP_FileDialogBack                  36   // The back arrow in a file dialog.
#define QStyle_SP_DockWidgetCloseButton           8    // Close button on dock windows (see also QDockWidget).
#define QStyle_SP_ToolBarHorizontalExtensionButton  26 // Extension button for horizontal toolbars.
#define QStyle_SP_ToolBarVerticalExtensionButton  27   // Extension button for vertical toolbars.
#define QStyle_SP_DialogOkButton                  38   // Icon for a standard OK button in a QDialogButtonBox.
#define QStyle_SP_DialogCancelButton              39   // Icon for a standard Cancel button in a QDialogButtonBox.
#define QStyle_SP_DialogHelpButton                40   // Icon for a standard Help button in a QDialogButtonBox.
#define QStyle_SP_DialogOpenButton                41   // Icon for a standard Open button in a QDialogButtonBox.
#define QStyle_SP_DialogSaveButton                42   // Icon for a standard Save button in a QDialogButtonBox.
#define QStyle_SP_DialogCloseButton               43   // Icon for a standard Close button in a QDialogButtonBox.
#define QStyle_SP_DialogApplyButton               44   // Icon for a standard Apply button in a QDialogButtonBox.
#define QStyle_SP_DialogResetButton               45   // Icon for a standard Reset button in a QDialogButtonBox.
#define QStyle_SP_DialogDiscardButton             46   // Icon for a standard Discard button in a QDialogButtonBox.
#define QStyle_SP_DialogYesButton                 47   // Icon for a standard Yes button in a QDialogButtonBox.
#define QStyle_SP_DialogNoButton                  48   // Icon for a standard No button in a QDialogButtonBox.
#define QStyle_SP_ArrowUp                         49   // Icon arrow pointing up.
#define QStyle_SP_ArrowDown                       50   // Icon arrow pointing down.
#define QStyle_SP_ArrowLeft                       51   // Icon arrow pointing left.
#define QStyle_SP_ArrowRight                      52   // Icon arrow pointing right.
#define QStyle_SP_ArrowBack                       53   // Equivalent to SP_ArrowLeft when the current layout direction is Qt::LeftToRight, otherwise SP_ArrowRight.
#define QStyle_SP_ArrowForward                    54   // Equivalent to SP_ArrowRight when the current layout direction is Qt::LeftToRight, otherwise SP_ArrowLeft.
#define QStyle_SP_CommandLink                     56   // Icon used to indicate a Vista style command link glyph.
#define QStyle_SP_VistaShield                     57   // Icon used to indicate UAC prompts on Windows Vista. This will return a null pixmap or icon on all other platforms.
#define QStyle_SP_BrowserReload                   58   // Icon indicating that the current page should be reloaded.
#define QStyle_SP_BrowserStop                     59   // Icon indicating that the page loading should stop.
#define QStyle_SP_MediaPlay                       60   // Icon indicating that media should begin playback.
#define QStyle_SP_MediaStop                       61   // Icon indicating that media should stop playback.
#define QStyle_SP_MediaPause                      62   // Icon indicating that media should pause playback.
#define QStyle_SP_MediaSkipForward                63   // Icon indicating that media should skip forward.
#define QStyle_SP_MediaSkipBackward               64   // Icon indicating that media should skip backward.
#define QStyle_SP_MediaSeekForward                65   // Icon indicating that media should seek forward.
#define QStyle_SP_MediaSeekBackward               66   // Icon indicating that media should seek backward.
#define QStyle_SP_MediaVolume                     67   // Icon indicating a volume control.
#define QStyle_SP_MediaVolumeMuted                68   // Icon indicating a muted volume control.
#define QStyle_SP_CustomBase                      0xf0000000   // Base value for custom standard pixmaps; custom values must be greater than this value.


#define QIcon_Normal                              0    // Display the pixmap when the user is not interacting with the icon, but the functionality represented by the icon is available.
#define QIcon_Disabled                            1    // Display the pixmap when the functionality represented by the icon is not available.
#define QIcon_Active                              2    // Display the pixmap when the functionality represented by the icon is available and the user is interacting with the icon, for example, moving the mouse over it or clicking it.
#define QIcon_Selected                            3    // Display the pixmap when the item represented by the icon is selected.

//enum #define QIcon_State
//This enum describes the state for which a pixmap is intended to be used. The state can be:
//
#define QIcon_On                                  0    // Display the pixmap when the widget is in an "on" state
#define QIcon_Off                                 1    // Display the pixmap when the widget is in an "off" state

#define QFont_MixedCase                           0   // This is the normal text rendering option where no capitalization change is applied.
#define QFont_AllUppercase                        1   // This alters the text to be rendered in all uppercase type.
#define QFont_AllLowercase                        2   // This alters the text to be rendered in all lowercase type.
#define QFont_SmallCaps                           3   // This alters the text to be rendered in small-caps type.
#define QFont_Capitalize                          4   // This alters the text to be rendered with the first character of each word as an uppercase character.

// enum #define QFont_SpacingType
//
#define QFont_PercentageSpacing                   0   // A value of 100 will keep the spacing unchanged; a value of 200 will enlarge the spacing after a character by the width of the character itself.
#define QFont_AbsoluteSpacing                     1   // A positive value increases the letter spacing by the corresponding pixels; a negative value decreases the spacing.

// enum #define QFont_Stretch
// Predefined stretch values that follow the CSS naming convention. The higher the value, the more stretched the text is.
//
#define QFont_UltraCondensed                      50
#define QFont_ExtraCondensed                      62
#define QFont_Condensed                           75
#define QFont_SemiCondensed                       87
#define QFont_Unstretched                         100
#define QFont_SemiExpanded                        112
#define QFont_Expanded                            125
#define QFont_ExtraExpanded                       150
#define QFont_UltraExpanded                       200

// enum #define QFont_Style
// This enum describes the different styles of glyphs that are used to display text.
//
#define QFont_StyleNormal                         0   // Normal glyphs used in unstyled text.
#define QFont_StyleItalic                         1   // Italic glyphs that are specifically designed for the purpose of representing italicized text.
#define QFont_StyleOblique                        2   // Glyphs with an italic appearance that are typically based on the unstyled glyphs, but are not fine-tuned for the purpose of representing italicized text.

// enum #define QFont_StyleHint
// Style hints are used by the font matching algorithm to find an appropriate default family if a selected font family is not available.
//
#define QFont_AnyStyle                            ?                  // leaves the font matching algorithm to choose the family. This is the default.
#define QFont_SansSerif                           QFont_Helvetica    // the font matcher prefer sans serif fonts.
#define QFont_Helvetica                           0                  // is a synonym for SansSerif.
#define QFont_Serif                               QFont_Times        // the font matcher prefers serif fonts.
#define QFont_Times                               ?                  // is a synonym for Serif.
#define QFont_TypeWriter                          QFont_Courier      // the font matcher prefers fixed pitch fonts.
#define QFont_Courier                             ?                  // a synonym for TypeWriter.
#define QFont_OldEnglish                          ?                  // the font matcher prefers decorative fonts.
#define QFont_Decorative                          QFont_OldEnglish   // is a synonym for OldEnglish.
#define QFont_System                              ?                  // the font matcher prefers system fonts.

// enum #define QFont_StyleStrategy
// The style strategy tells the font matching algorithm what type of fonts should be used to find an appropriate default family.
//
#define QFont_PreferDefault                       0x0001   // the default style strategy. It does not prefer any type of font.
#define QFont_PreferBitmap                        0x0002   // prefers bitmap fonts (as opposed to outline fonts).
#define QFont_PreferDevice                        0x0004   // prefers device fonts.
#define QFont_PreferOutline                       0x0008   // prefers outline fonts (as opposed to bitmap fonts).
#define QFont_ForceOutline                        0x0010   // forces the use of outline fonts.
#define QFont_NoAntialias                         0x0100   // don't antialias the fonts.
#define QFont_PreferAntialias                     0x0080   // antialias if possible.
#define QFont_OpenGLCompatible                    0x0200   // forces the use of OpenGL compatible fonts.
#define QFont_NoFontMerging                       0x8000   // If a font does not contain a character requested to draw then Qt automatically chooses a similar looking for that contains the character. This flag disables this feature.

// Any of these may be OR-ed with one of these flags:
//
#define QFont_PreferMatch                         0x0020   // prefer an exact match. The font matcher will try to use the exact font size that has been specified.
#define QFont_PreferQuality                       0x0040   // prefer the best quality font. The font matcher will use the nearest standard point size that the font supports.

#define QFont_Light                               25
#define QFont_Normal                              50
#define QFont_DemiBold                            63
#define QFont_Bold                                75
#define QFont_Black                               87


#define QFileDialog_AcceptOpen                    0
#define QFileDialog_AcceptSave                    1

// enum #define QFileDialog_DialogLabel
//
#define QFileDialog_LookIn                        0
#define QFileDialog_FileName                      1
#define QFileDialog_FileType                      2
#define QFileDialog_Accept                        3
#define QFileDialog_Reject                        4

// enum #define QFileDialog_FileMode
// This enum is used to indicate what the user may select in the file dialog;
// i.e. what the dialog will return if the user clicks OK.
//
#define QFileDialog_AnyFile                       0   // The name of a file, whether it exists or not.
#define QFileDialog_ExistingFile                  1   // The name of a single existing file.
#define QFileDialog_Directory                     2   // The name of a directory. Both files and directories are displayed.
#define QFileDialog_ExistingFiles                 3   // The names of zero or more existing files.

// The Options type is a typedef for QFlags<Option>. It stores an OR combination of Option values.
//
#define QFileDialog_ShowDirsOnly                  0x00000001   // Only show directories in the file dialog. By default both files and directories are shown. (Valid only in the Directory file mode.)
#define QFileDialog_DontResolveSymlinks           0x00000002   // Don't resolve symlinks in the file dialog. By default symlinks are resolved.
#define QFileDialog_DontConfirmOverwrite          0x00000004   // Don't ask for confirmation if an existing file is selected. By default confirmation is requested.
#define QFileDialog_DontUseNativeDialog           0x00000010   // Don't use the native file dialog. By default on Mac OS X and Windows, the native file dialog is used.
#define QFileDialog_ReadOnly                      0x00000020   // Indicates that the model is readonly.
#define QFileDialog_HideNameFilterDetails         0x00000040   // Indicates if the is hidden or not.

//enum QFileDialog::ViewMode
//This enum describes the view mode of the file dialog; i.e. what information about each file will be displayed.
//
#define QFileDialog_Detail                        0   // Displays an icon, a name, and details for each item in the directory.
#define QFileDialog_List                          1   // Displays only an icon and a name for each item in the directory.

#define QDialog_Accepted                          1
#define QDialog_Rejected                          0


// enum QAbstractPrintDialog::PrintDialogOption
// flags QAbstractPrintDialog::PrintDialogOptions
// Used to specify which parts of the print dialog should be visible.
// The PrintDialogOptions type is a typedef for QFlags<PrintDialogOption>.
// It stores an OR combination of PrintDialogOption values.
//
#define QAbstractPrintDialog_None                 0x0000   // None of the options are enabled.
#define QAbstractPrintDialog_PrintToFile          0x0001   // The print to file option is enabled.
#define QAbstractPrintDialog_PrintSelection       0x0002   // The print selection option is enabled.
#define QAbstractPrintDialog_PrintPageRange       0x0004   // The page range selection option is enabled.
#define QAbstractPrintDialog_PrintCollateCopies   0x0010

// enum QAbstractPrintDialog::PrintRange
// Used to specify the print range selection option.
//
#define QAbstractPrintDialog_AllPages             0   // All pages should be printed.
#define QAbstractPrintDialog_Selection            1   // Only the selection should be printed.
#define QAbstractPrintDialog_PageRange            2   // The specified page range should be printed.

// enum QPrinter::ColorMode
// This enum type is used to indicate whether QPrinter should print in color or not.
//
#define QPrinter_Color                            1   // print in color if available, otherwise in grayscale.
#define QPrinter_GrayScale                        0   // print in grayscale, even on color printers.

// enum QPrinter::DuplexMode
// This enum is used to indicate whether printing will occur on one or both sides of each sheet of paper (simplex or duplex printing).
//
#define QPrinter_DuplexNone                       0   // Single sided (simplex) printing only.
#define QPrinter_DuplexAuto                       1   // The printer's default setting is used to determine whether duplex printing is used.
#define QPrinter_DuplexLongSide                   2   // Both sides of each sheet of paper are used for printing. The paper is turned over its longest edge before the second side is printed
#define QPrinter_DuplexShortSide                  3   // Both sides of each sheet of paper are used for printing. The paper is turned over its shortest edge before the second side is printed

// enum QPrinter::Orientation
// This enum type (not to be confused with Orientation) is used to specify each page's orientation.
//
#define QPrinter_Portrait                         0   // the page's height is greater than its width.
#define QPrinter_Landscape                        1   // the page's width is greater than its height.
// This type interacts with QPrinter::PaperSize and QPrinter::setFullPage()
// to determine the final size of the page available to the application.

// enum QPrinter::OutputFormat
// The OutputFormat enum is used to describe the format QPrinter should use for printing.
//
#define QPrinter_NativeFormat                     0   // QPrinter will print output using a method defined by the platform it is running on. This mode is the default when printing directly to a printer.
#define QPrinter_PdfFormat                        1   // QPrinter will generate its output as a searchable PDF file. This mode is the default when printing to a file.
#define QPrinter_PostScriptFormat                 2   // QPrinter will generate its output as in the PostScript format. (This feature was introduced in Qt 4.2.)

// enum QPrinter::PageOrder
// This enum type is used by QPrinter to tell the application program how to print.
//
#define QPrinter_FirstPageFirst                   0   // the lowest-numbered page should be printed first.
#define QPrinter_LastPageFirst                    1   // the highest-numbered page should be printed first.

// enum QPrinter::PaperSize
// This enum type specifies what paper size QPrinter should use.
// QPrinter does not check that the paper size is available; it just uses this information,
// together with QPrinter::Orientation and QPrinter::setFullPage(), to determine the printable area.
//
// The defined sizes (with setFullPage(true)) are:
//
#define QPrinter_A0                               5    // 841  x 1189 mm
#define QPrinter_A1                               6    // 594  x 841  mm
#define QPrinter_A2                               7    // 420  x 594  mm
#define QPrinter_A3                               8    // 297  x 420  mm
#define QPrinter_A4                               0    // 210  x 297  mm, 8.26 x 11.69 inches
#define QPrinter_A5                               9    // 148  x 210  mm
#define QPrinter_A6                               10   // 105  x 148  mm
#define QPrinter_A7                               11   // 74   x 105  mm
#define QPrinter_A8                               12   // 52   x 74   mm
#define QPrinter_A9                               13   // 37   x 52   mm
#define QPrinter_B0                               14   // 1030 x 1456 mm
#define QPrinter_B1                               15   // 728  x 1030 mm
#define QPrinter_B2                               17   // 515  x 728  mm
#define QPrinter_B3                               18   // 364  x 515  mm
#define QPrinter_B4                               19   // 257  x 364  mm
#define QPrinter_B5                               1    // 182  x 257  mm, 7.17 x 10.13 inches
#define QPrinter_B6                               20   // 128  x 182  mm
#define QPrinter_B7                               21   // 91   x 128  mm
#define QPrinter_B8                               22   // 64   x 91   mm
#define QPrinter_B9                               23   // 45   x 64   mm
#define QPrinter_B10                              16   // 32   x 45   mm
#define QPrinter_C5E                              24   // 163  x 229  mm
#define QPrinter_Comm10E                          25   // 105  x 241  mm, U.S. Common 10 Envelope
#define QPrinter_DLE                              26   // 110  x 220  mm
#define QPrinter_Executive                        4    // 191  x 254  mm, 7.5  x 10 inches
#define QPrinter_Folio                            27   // 210  x 330  mm
#define QPrinter_Ledger                           28   // 432  x 279  mm
#define QPrinter_Legal                            3    // 216  x 356  mm, 8.5  x 14 inches
#define QPrinter_Letter                           2    // 216  x 279  mm, 8.5  x 11 inches
#define QPrinter_Tabloid                          29   // 279  x 432  mm
#define QPrinter_Custom                           30   // Unknown, or a user defined size.
// With setFullPage(false) (the default), the metrics will be a bit smaller; how much
// depends on the printer in use.


// enum QPrinter::PaperSource
//
// This enum type specifies what paper source QPrinter is to use. QPrinter does not check
// that the paper source is available; it just uses this information to try and set the paper
// source. Whether it will set the paper source depends on whether the printer has that
// particular source.
//
// Warning: This is currently only implemented for Windows.
//
#define QPrinter_Auto                             6
#define QPrinter_Cassette                         11
#define QPrinter_Envelope                         4
#define QPrinter_EnvelopeManual                   5
#define QPrinter_FormSource                       12
#define QPrinter_LargeCapacity                    10
#define QPrinter_LargeFormat                      9
#define QPrinter_Lower                            1
#define QPrinter_MaxPageSource                    13
#define QPrinter_Middle                           2
#define QPrinter_Manual                           3
#define QPrinter_OnlyOne                          0
#define QPrinter_Tractor                          7
#define QPrinter_SmallFormat                      8

// enum QPrinter::PrintRange
// Used to specify the print range selection option.
//
#define QPrinter_AllPages                         0   // All pages should be printed.
#define QPrinter_Selection                        1   // Only the selection should be printed.
#define QPrinter_PageRange                        2   // The specified page range should be printed.

// enum #define QPrinter_PrinterMode
// This enum describes the mode the printer should work in. It basically presets a certain
// resolution and working mode.
//
#define QPrinter_ScreenResolution                 0   // Sets the resolution of the print device to the screen resolution. This has the big advantage that the results obtained when painting on the printer will match more or less exactly the visible output on the screen. It is the easiest to use, as font metrics on the screen and on the printer are the same. This is the default value. ScreenResolution will produce a lower quality output than HighResolution and should only be used for drafts.
#define QPrinter_PrinterResolution                1   // This value is deprecated. Is is equivalent to ScreenResolution on Unix and HighResolution on Windows and Mac. Due do the difference between ScreenResolution and HighResolution, use of this value may lead to non-portable printer code.
#define QPrinter_HighResolution                   2   // On Windows, sets the printer resolution to that defined for the printer in use. For PostScript printing, sets the resolution of the PostScript driver to 1200 dpi.
// Note: When rendering text on a QPrinter device, it is important to realize that the size of
// text, when specified in points, is independent of the resolution specified for the device itself.
// Therefore, it may be useful to specify the font size in pixels when combining text with
// graphics to ensure that their relative sizes are what you expect.

// enum QPrinter::PrinterState
//
#define QPrinter_Idle                             0
#define QPrinter_Active                           1
#define QPrinter_Aborted                          2
#define QPrinter_Error                            3

// enum QPrinter::Unit
// This enum type is used to specify the measurement unit for page and paper sizes.
//
#define QPrinter_Millimeter                       0
#define QPrinter_Point                            1
#define QPrinter_Inch                             2
#define QPrinter_Pica                             3
#define QPrinter_Didot                            4
#define QPrinter_Cicero                           5
#define QPrinter_DevicePixel                      6
// Note the difference between Point and DevicePixel. The Point unit is defined
// to be 1/72th of an inch, while the DevicePixel unit is resolution dependant and is
// based on the actual pixels, or dots, on the printer.


#define QPrintEngine_PPK_CollateCopies            0      // A boolean value indicating whether the printout should be collated or not.
#define QPrintEngine_PPK_ColorMode                1      // Refers to QPrinter::ColorMode, either color or monochrome.
#define QPrintEngine_PPK_Creator                  2      // A string describing the document's creator.
#define QPrintEngine_PPK_Duplex                   21     // A boolean value indicating whether both sides of the printer paper should be used for the printout.
#define QPrintEngine_PPK_DocumentName             3      // A string describing the document name in the spooler.
#define QPrintEngine_PPK_FontEmbedding            19     // A boolean value indicating whether data for the document's fonts should be embedded in the data sent to the printer.
#define QPrintEngine_PPK_FullPage                 4      // A boolean describing if the printer should be full page or not.
#define QPrintEngine_PPK_NumberOfCopies           5      // An integer specifying the number of copies
#define QPrintEngine_PPK_Orientation              6      // Specifies a QPrinter::Orientation value.
#define QPrintEngine_PPK_OutputFileName           7      // The output file name as a string. An empty file name indicates that the printer should not print to a file.
#define QPrintEngine_PPK_PageOrder                8      // Specifies a QPrinter::PageOrder value.
#define QPrintEngine_PPK_PageRect                 9      // A QRect specifying the page rectangle
#define QPrintEngine_PPK_PageSize                 10     // Obsolete. Use PPK_PaperSize instead.
#define QPrintEngine_PPK_PaperRect                11     // A QRect specifying the paper rectangle.
#define QPrintEngine_PPK_PaperSource              12     // Specifies a QPrinter::PaperSource value.
#define QPrintEngine_PPK_PaperSources             22     // Specifies more than one QPrinter::PaperSource value.
#define QPrintEngine_PPK_PaperSize                QPrintEngine_PPK_PageSize   // Specifies a QPrinter::PaperSize value.
#define QPrintEngine_PPK_PrinterName              13     // A string specifying the name of the printer.
#define QPrintEngine_PPK_PrinterProgram           14     // A string specifying the name of the printer program used for printing,
#define QPrintEngine_PPK_Resolution               15     // An integer describing the dots per inch for this printer.
#define QPrintEngine_PPK_SelectionOption          16
#define QPrintEngine_PPK_SupportedResolutions     17     // A list of integer QVariants describing the set of supported resolutions that the printer has.
#define QPrintEngine_PPK_SuppressSystemPrintStatus  20   // Suppress the built-in dialog for showing printing progress. As of 4.1 this only has effect on Mac OS X where, by default, a status dialog is shown.
#define QPrintEngine_PPK_WindowsPageSize          18     // An integer specifying a DM_PAPER entry on Windows.
#define QPrintEngine_PPK_CustomPaperSize          23     // A QSizeF specifying a custom paper size in the QPrinter::Point unit.
#define QPrintEngine_PPK_PageMargins              24     // A QList<QVariant> containing the left, top, right and bottom margin values.


// enum QTextCursor::MoveMode
//
#define QTextCursor_MoveAnchor                    0      // Moves the anchor to the same position as the cursor itself.
#define QTextCursor_KeepAnchor                    1      // Keeps the anchor where it is.
// If the anchor() is kept where it is and the position() is moved, the text in between will be selected.

// enum QTextCursor::MoveOperation
//
#define QTextCursor_NoMove                        0      // Keep the cursor where it is
#define QTextCursor_Start                         1      // Move to the start of the document.
#define QTextCursor_StartOfLine                   3      // Move to the start of the current line.
#define QTextCursor_StartOfBlock                  4      // Move to the start of the current block.
#define QTextCursor_StartOfWord                   5      // Move to the start of the current word.
#define QTextCursor_PreviousBlock                 6      // Move to the start of the previous block.
#define QTextCursor_PreviousCharacter             7      // Move to the previous character.
#define QTextCursor_PreviousWord                  8      // Move to the beginning of the previous word.
#define QTextCursor_Up                            2      // Move up one line.
#define QTextCursor_Left                          9      // Move left one character.
#define QTextCursor_WordLeft                      10     // Move left one word.
#define QTextCursor_End                           11     // Move to the end of the document.
#define QTextCursor_EndOfLine                     13     // Move to the end of the current line.
#define QTextCursor_EndOfWord                     14     // Move to the end of the current word.
#define QTextCursor_EndOfBlock                    15     // Move to the end of the current block.
#define QTextCursor_NextBlock                     16     // Move to the beginning of the next block.
#define QTextCursor_NextCharacter                 17     // Move to the next character.
#define QTextCursor_NextWord                      18     // Move to the next word.
#define QTextCursor_Down                          12     // Move down one line.
#define QTextCursor_Right                         19     // Move right one character.
#define QTextCursor_WordRight                     20     // Move right one word.
#define QTextCursor_NextCell                      21     // Move to the beginning of the next table cell inside the current table. If the current cell is the last cell in the row, the cursor will move to the first cell in the next row.
#define QTextCursor_PreviousCell                  22     // Move to the beginning of the previous table cell inside the current table. If the current cell is the first cell in the row, the cursor will move to the last cell in the previous row.
#define QTextCursor_NextRow                       23     // Move to the first new cell of the next row in the current table.
#define QTextCursor_PreviousRow                   24     // Move to the last cell of the previous row in the current table.

// enum QTextCursor::SelectionType
// This enum describes the types of selection that can be applied with the select() function.
//
#define QTextCursor_Document                      3      // Selects the entire document.
#define QTextCursor_BlockUnderCursor              2      // Selects the block of text under the cursor.
#define QTextCursor_LineUnderCursor               1      // Selects the line of text under the cursor.
#define QTextCursor_WordUnderCursor               0      // Selects the word under the cursor. If the cursor is not positioned within a string of selectable characters, no text is selected.


// enum QTextCharFormat::UnderlineStyle
// This enum describes the different ways drawing underlined text.
//
#define QTextCharFormat_NoUnderline               0      // Text is draw without any underlining decoration.
#define QTextCharFormat_SingleUnderline           1      // A line is drawn using Qt::SolidLine.
#define QTextCharFormat_DashUnderline             2      // Dashes are drawn using Qt::DashLine.
#define QTextCharFormat_DotLine                   3      // Dots are drawn using Qt::DotLine;
#define QTextCharFormat_DashDotLine               4      // Dashs and dots are drawn using Qt::DashDotLine.
#define QTextCharFormat_DashDotDotLine            5      // Underlines draw drawn using Qt::DashDotDotLine.
#define QTextCharFormat_WaveUnderline             6      // The text is underlined using a wave shaped line.
#define QTextCharFormat_SpellCheckUnderline       7      // The underline is drawn depending on the QStyle::SH_SpellCeckUnderlineStyle style hint of the QApplication style. By default this is mapped to WaveUnderline, on Mac OS X it is mapped to DashDotLine.

// enum QTextCharFormat::VerticalAlignment
// This enum describes the ways that adjacent characters can be vertically aligned.
//
#define QTextCharFormat_AlignNormal               0      // Adjacent characters are positioned in the standard way for text in the writing system in use.
#define QTextCharFormat_AlignSuperScript          1      // Characters are placed above the baseline for normal text.
#define QTextCharFormat_AlignSubScript            2      // Characters are placed below the baseline for normal text.
#define QTextCharFormat_AlignMiddle               3      // The center of the object is vertically aligned with the base line. Currently, this is only implemented for inline objects.
#define QTextCharFormat_AlignBottom               5      // The bottom edge of the object is vertically aligned with the base line.
#define QTextCharFormat_AlignTop                  4      // The top edge of the object is vertically aligned with the base line.

// enum QAbstractItemView::CursorAction
// This enum describes the different ways to navigate between items,
//
#define QAbstractItemView_MoveUp                  0      // Move to the item above the current item.
#define QAbstractItemView_MoveDown                1      // Move to the item below the current item.
#define QAbstractItemView_MoveLeft                2      // Move to the item left of the current item.
#define QAbstractItemView_MoveRight               3      // Move to the item right of the current item.
#define QAbstractItemView_MoveHome                4      // Move to the top-left corner item.
#define QAbstractItemView_MoveEnd                 5      // Move to the bottom-right corner item.
#define QAbstractItemView_MovePageUp              6      // Move one page up above the current item.
#define QAbstractItemView_MovePageDown            7      // Move one page down below the current item.
#define QAbstractItemView_MoveNext                8      // Move to the item after the current item.
#define QAbstractItemView_MovePrevious            9      // Move to the item before the current item.

// enum #define QAbstractItemView_DragDropMode
// Describes the various drag and drop events the view can act upon. By default the view does not support dragging or dropping (NoDragDrop).
//
#define QAbstractItemView_NoDragDrop              0      // Does not support dragging or dropping.
#define QAbstractItemView_DragOnly                1      // The view supports dragging of its own items
#define QAbstractItemView_DropOnly                2      // The view accepts drops
#define QAbstractItemView_DragDrop                3      // The view supports both dragging and dropping
#define QAbstractItemView_InternalMove            4      // The view accepts move (not copy) operations only from itself.
// Note that the model used needs to provide support for drag and drop operations.

// enum QAbstractItemView::DropIndicatorPosition
// This enum indicates the position of the drop indicator in relation to the index at the current mouse position:
#define QAbstractItemView_OnItem                  0      // The item will be dropped on the index.
#define QAbstractItemView_AboveItem               1      // The item will be dropped above the index.
#define QAbstractItemView_BelowItem               2      // The item will be dropped below the index.
#define QAbstractItemView_OnViewport              3      // The item will be dropped onto a region of the viewport with no items. The way each view handles items dropped onto the viewport depends on the behavior of the underlying model in use.

// enum #define QAbstractItemView_EditTrigger
// flags QAbstractItemView::EditTriggers
// This enum describes actions which will initiate item editing.
//
#define QAbstractItemView_NoEditTriggers          0      // No editing possible.
#define QAbstractItemView_CurrentChanged          1      // Editing start whenever current item changes.
#define QAbstractItemView_DoubleClicked           2      // Editing starts when an item is double clicked.
#define QAbstractItemView_SelectedClicked         4      // Editing starts when clicking on an already selected item.
#define QAbstractItemView_EditKeyPressed          8      // Editing starts when the platform edit key has been pressed over an item.
#define QAbstractItemView_AnyKeyPressed           16     // Editing starts when any key is pressed over an item.
#define QAbstractItemView_AllEditTriggers         31     // Editing starts for all above actions.
// The EditTriggers type is a typedef for QFlags<EditTrigger>. It stores an OR combination of EditTrigger values.

// enum QAbstractItemView::ScrollHint
//
#define QAbstractItemView_EnsureVisible           0      // Scroll to ensure that the item is visible.
#define QAbstractItemView_PositionAtTop           1      // Scroll to position the item at the top of the viewport.
#define QAbstractItemView_PositionAtBottom        2      // Scroll to position the item at the bottom of the viewport.
#define QAbstractItemView_PositionAtCenter        3      // Scroll to position the item at the center of the viewport.

// enum #define QAbstractItemView_ScrollMode
//
#define QAbstractItemView_ScrollPerItem           0      // The view will scroll the contents one item at a time.
#define QAbstractItemView_ScrollPerPixel          1      // The view will scroll the contents one pixel at a time.

// enum #define QAbstractItemView_SelectionBehavior
//
#define QAbstractItemView_SelectItems             0      // Selecting single items.
#define QAbstractItemView_SelectRows              1      // Selecting only rows.
#define QAbstractItemView_SelectColumns           2      // Selecting only columns.

// enum #define QAbstractItemView_SelectionMode
// This enum indicates how the view responds to user selections:
//
#define QAbstractItemView_SingleSelection         1      // When the user selects an item, any already-selected item becomes unselected, and the user cannot unselect the selected item by clicking on it.
#define QAbstractItemView_ContiguousSelection     4      // When the user selects an item in the usual way, the selection is cleared and the new item selected. However, if the user presses the Shift key while clicking on an item, all items between the current item and the clicked item are selected or unselected, depending on the state of the clicked item.
#define QAbstractItemView_ExtendedSelection       3      // When the user selects an item in the usual way,
                                                         //   the selection is cleared and the new item
                                                         //   selected. However, if the user presses the Ctrl key
                                                         //   when clicking on an item, the clicked item gets toggled
                                                         //   and all other items are left untouched.
                                                         //   If the user presses the Shift key while clicking on
                                                         //   an item, all items between the current item and the
                                                         //   clicked item are selected or unselected, depending on
                                                         //   the state of the clicked item. Multiple items can be
                                                         //   selected by dragging the mouse over them.
#define QAbstractItemView_MultiSelection          2      // When the user selects an item in the usual way, the selection status of that item is toggled and the other items are left alone. Multiple items can be toggled by dragging the mouse over them.
#define QAbstractItemView_NoSelection             0      // Items cannot be selected.
// The most commonly used modes are SingleSelection and ExtendedSelection.

// enum #define QAbstractItemView_State
// Describes the different states the view can be in. This is usually only interesting when reimplementing your own view.
//
#define QAbstractItemView_NoState                 0      // The is the default state.
#define QAbstractItemView_DraggingState           1      // The user is dragging items.
#define QAbstractItemView_DragSelectingState      2      // The user is selecting items.
#define QAbstractItemView_EditingState            3      // The user is editing an item in a widget editor.
#define QAbstractItemView_ExpandingState          4      // The user is opening a branch of items.
#define QAbstractItemView_CollapsingState         5      // The user is closing a branch of items.
#define QAbstractItemView_AnimatingState          6      // The item view is performing an animation.

#define QHeaderView_Interactive                   0      // The user can resize the section. The section can also be resized programmatically using resizeSection(). The section size defaults to defaultSectionSize. (See also cascadingSectionResizes.)
#define QHeaderView_Fixed                         2      // The user cannot resize the section. The section can only be resized programmatically using resizeSection(). The section size defaults to defaultSectionSize.
#define QHeaderView_Stretch                       1      // QHeaderView will automatically resize the section to fill the available space. The size cannot be changed by the user or programmatically.
#define QHeaderView_ResizeToContents              3      // QHeaderView will automatically resize the section to its optimal size based on the contents of the entire column or row. The size cannot be changed by the user or programmatically. (This value was introduced in 4.2)

#define QSizePolicy_DefaultType                   0x00000001   // The default type, when none is specified.
#define QSizePolicy_ButtonBox                     0x00000002   // A QDialogButtonBox instance.
#define QSizePolicy_CheckBox                      0x00000004   // A QCheckBox instance.
#define QSizePolicy_ComboBox                      0x00000008   // A QComboBox instance.
#define QSizePolicy_Frame                         0x00000010   // A QFrame instance.
#define QSizePolicy_GroupBox                      0x00000020   // A QGroupBox instance.
#define QSizePolicy_Label                         0x00000040   // A QLabel instance.
#define QSizePolicy_Line                          0x00000080   // A QFrame instance with QFrame::HLine or QFrame::VLine.
#define QSizePolicy_LineEdit                      0x00000100   // A QLineEdit instance.
#define QSizePolicy_PushButton                    0x00000200   // A QPushButton instance.
#define QSizePolicy_RadioButton                   0x00000400   // A QRadioButton instance.
#define QSizePolicy_Slider                        0x00000800   // A QAbstractSlider instance.
#define QSizePolicy_SpinBox                       0x00001000   // A QAbstractSpinBox instance.
#define QSizePolicy_TabWidget                     0x00002000   // A QTabWidget instance.
#define QSizePolicy_ToolButton                    0x00004000   // A QToolButton instance.
// The ControlTypes type is a typedef for QFlags<ControlType>. It stores an OR combination of ControlType values.

// enum QSizePolicy::Policy
// This enum describes the various per-dimension sizing types used when constructing a QSizePolicy.
//
#define QSizePolicy_Fixed                         0   // The QWidget::sizeHint() is the only acceptable alternative, so the widget can never grow or shrink (e.g. the vertical direction of a push button).
#define QSizePolicy_Minimum                       QSizePolicy_GrowFlag                                                     // The sizeHint() is minimal, and sufficient. The widget can be expanded, but there is no advantage to it being larger (e.g. the horizontal direction of a push button). It cannot be smaller than the size provided by sizeHint().
#define QSizePolicy_Maximum                       QSizePolicy_ShrinkFlag                                                   // The sizeHint() is a maximum. The widget can be shrunk any amount without detriment if other widgets need the space (e.g. a separator line). It cannot be larger than the size provided by sizeHint().
#define QSizePolicy_Preferred                     hb_bitOR( QSizePolicy_GrowFlag, QSizePolicy_ShrinkFlag )                 // The sizeHint() is best, but the widget can be shrunk and still be useful. The widget can be expanded, but there is no advantage to it being larger than sizeHint() (the default QWidget policy).
#define QSizePolicy_Expanding                     hb_bitOR( QSizePolicy_GrowFlag, QSizePolicy_ShrinkFlag, QSizePolicy_ExpandFlag )  // The sizeHint() is a sensible size, but the widget can be shrunk and still be useful. The widget can make use of extra space, so it should get as much space as possible (e.g. the horizontal direction of a horizontal slider).
#define QSizePolicy_MinimumExpanding              hb_bitOR( QSizePolicy_GrowFlag, QSizePolicy_ExpandFlag )                 // The sizeHint() is minimal, and sufficient. The widget can make use of extra space, so it should get as much space as possible (e.g. the horizontal direction of a horizontal slider).
#define QSizePolicy_Ignored                       hb_bitOR( QSizePolicy_ShrinkFlag, QSizePolicy_GrowFlag, QSizePolicy_IgnoreFlag )  // The sizeHint() is ignored. The widget will get as much space as possible.

// enum QSizePolicy::PolicyFlag
// These flags are combined together to form the various Policy values:
//
#define QSizePolicy_GrowFlag                      1   // The widget can grow beyond its size hint if necessary.
#define QSizePolicy_ExpandFlag                    2   // The widget should get as much space as possible.
#define QSizePolicy_ShrinkFlag                    4   // The widget can shrink below its size hint if necessary.
#define QSizePolicy_IgnoreFlag                    8   // The widget's size hint is ignored. The widget will get as much space as possible.


// This enum controls the types of events processed by the processEvents() functions.
//
#define QEventLoop_AllEvents                      0x00   // All events. Note that DeferredDelete events are processed specially. See QObject::deleteLater() for more details.
#define QEventLoop_ExcludeUserInputEvents         0x01   // Do not process user input events, such as ButtonPress and KeyPress. Note that the events are not discarded; they will be delivered the next time processEvents() is called without the ExcludeUserInputEvents flag.
#define QEventLoop_ExcludeSocketNotifiers         0x02   // Do not process socket notifier events. Note that the events are not discarded; they will be delivered the next time processEvents() is called without the ExcludeSocketNotifiers flag.
#define QEventLoop_WaitForMoreEvents              0x04   // Wait for events if no pending events are available.
#define QEventLoop_DeferredDeletion               0x10   // deprecated - do not use.
// The ProcessEventsFlags type is a typedef for QFlags<ProcessEventsFlag>. It stores an OR combination of ProcessEventsFlag values.

#define QTextEdit_NoWrap                          0
#define QTextEdit_WidgetWidth                     1
#define QTextEdit_FixedPixelWidth                 2
#define QTextEdit_FixedColumnWidth                3

#define QDockWidget_DockWidgetClosable            0x01   // The dock widget can be closed. On some systems the dock widget always has a close button when it's floating (for example on MacOS 10.5).
#define QDockWidget_DockWidgetMovable             0x02   // The dock widget can be moved between docks by the user.
#define QDockWidget_DockWidgetFloatable           0x04   // The dock widget can be detached from the main window, and floated as an independent window.
#define QDockWidget_DockWidgetVerticalTitleBar    0x08   // The dock widget displays a vertical title bar on its left side. This can be used to increase the amount of vertical space in a QMainWindow.
#define QDockWidget_NoDockWidgetFeatures          0x00   // The dock widget cannot be closed, moved, or floated.

#define QMainWindow_AnimatedDocks                 0x01   // Identical to the animated property.
#define QMainWindow_AllowNestedDocks              0x02   // Identical to the dockNestingEnabled property.
#define QMainWindow_AllowTabbedDocks              0x04   // The user can drop one dock widget "on top" of another. The two widgets are stacked and a tab bar appears for selecting which one is visible.
#define QMainWindow_ForceTabbedDocks              0x08   // Each dock area contains a single stack of tabbed dock widgets. In other words, dock widgets cannot be placed next to each other in a dock area. If this option is set, AllowNestedDocks has no effect.
#define QMainWindow_VerticalTabs                  0x10   // The two vertical dock areas on the sides of the main window show their tabs vertically. If this option is not set, all dock areas show their tabs at the bottom. Implies AllowTabbedDocks. See also setTabPosition().

#define QTabWidget_North                          0      // The tabs are drawn above the pages.
#define QTabWidget_South                          1      // The tabs are drawn below the pages.
#define QTabWidget_West                           2      // The tabs are drawn to the left of the pages.
#define QTabWidget_East                           3      // The tabs are drawn to the right of the pages.
#define QTabWidget_Rounded                        0      // The tabs are drawn with a rounded look. This is the default shape.
#define QTabWidget_Triangular                     1      // The tabs are drawn with a triangular look.

#define QTextDocument_FindBackward                0x00001   // Search backwards instead of forwards.
#define QTextDocument_FindCaseSensitively         0x00002   // By default find works case insensitive. Specifying this option changes the behaviour to a case sensitive find operation.
#define QTextDocument_FindWholeWords              0x00004   // Makes find match only complete words.
#define QTextDocument_DocumentTitle               0         // The title of the document.
#define QTextDocument_DocumentUrl                 1         // The url of the document. The loadResource() function uses this url as the base when loading relative resources.
#define QTextDocument_HtmlResource                1         // The resource contains HTML.
#define QTextDocument_ImageResource               2         // The resource contains image data. Currently supported data types are QVariant::Pixmap and QVariant::Image. If the corresponding variant is of type QVariant::ByteArray then Qt attempts to load the image using QImage::loadFromData. QVariant::Icon is currently not supported. The icon needs to be converted to one of the supported types first, for example using QIcon::pixmap.
#define QTextDocument_StyleSheetResource          3         // The resource contains CSS.
#define QTextDocument_UserResource                100       // The first available value for user defined resource types.

#define QPlainTextEdit_NoWrap                     0
#define QPlainTextEdit_WidgetWidth                1

#define QLayout_SetDefaultConstraint              0        // The main widget's minimum size is set to minimumSize(), unless the widget already has a minimum size.
#define QLayout_SetFixedSize                      3        // The main widget's size is set to sizeHint(); it cannot be resized at all.
#define QLayout_SetMinimumSize                    2        // The main widget's minimum size is set to minimumSize(); it cannot be smaller.
#define QLayout_SetMaximumSize                    4        // The main widget's maximum size is set to maximumSize(); it cannot be larger.
#define QLayout_SetMinAndMaxSize                  5        // The main widget's minimum size is set to minimumSize() and its maximum size is set to maximumSize().
#define QLayout_SetNoConstraint                   1        // The widget is not constrained.

#define QCompleter_PopupCompletion                0        // Current completions are displayed in a popup window.
#define QCompleter_InlineCompletion               2        // Completions appear inline (as selected text).
#define QCompleter_UnfilteredPopupCompletion      1        // All possible completions are displayed in a popup window with the most likely suggestion indicated as current.

#define QCompleter_UnsortedModel                  0        // The model is unsorted.
#define QCompleter_CaseSensitivelySortedModel     1        // The model is sorted case sensitively.
#define QCompleter_CaseInsensitivelySortedModel   2        // The model is sorted case insensitively.

#define QToolButton_DelayedPopup                  0        // After pressing and holding the tool button down for a certain amount of time (the timeout is style dependant, see QStyle::SH_ToolButton_PopupDelay), the menu is displayed. A typical application example is the "back" button in some web browsers's tool bars. If the user clicks it, the browser simply browses back to the previous page. If the user presses and holds the button down for a while, the tool button shows a menu containing the current history list
#define QToolButton_MenuButtonPopup               1        // In this mode the tool button displays a special arrow to indicate that a menu is present. The menu is displayed when the arrow part of the button is pressed.
#define QToolButton_InstantPopup                  2        // The menu is displayed, without delay, when the tool button is pressed. In this mode, the button's own action is not triggered.

#define QSystemTrayIcon_Unknown                   0        // Unknown reason
#define QSystemTrayIcon_Context                   1        // The context menu for the system tray entry was requested
#define QSystemTrayIcon_DoubleClick               2        // The system tray entry was double clicked
#define QSystemTrayIcon_Trigger                   3        // The system tray entry was clicked
#define QSystemTrayIcon_MiddleClick               4        // The system tray entry was clicked with the middle mouse button

/* QMdiArea */
#define QMdiArea_SubWindowView                    0
#define QMdiArea_TabbedView                       1

#define QMdiArea_CreationOrder                    0
#define QMdiArea_StackingOrder                    1
#define QMdiArea_ActivationHistoryOrder           2

#define QMdiArea_DontMaximizeSubWindowOnActivation  1

#define QMdiSubWindow_RubberBandResize            0x04
#define QMdiSubWindow_RubberBandMove              0x08

/* QAbstractItemDelegate */
#define QAbstractItemDelegate_NoHint              0   // There is no recommended action to be performed.

//These hints let the delegate influence the behavior of the view:
#define QAbstractItemDelegate_EditNextItem        1   // The view should use the delegate to open an editor on the next item in the view.
#define QAbstractItemDelegate_EditPreviousItem    2   // The view should use the delegate to open an editor on the previous item in the view.

//The following hints are most useful when models are used that cache data, such as those that manipulate data locally in order to increase performance or conserve network bandwidth.
#define QAbstractItemDelegate_SubmitModelCache    3   // If the model caches data, it should write out cached data to the underlying data store.
#define QAbstractItemDelegate_RevertModelCache    4   // If the model caches data, it should discard cached data and replace it with data from the underlying data store.

#define QTabBar_LeftSide                          0   // Left side of the tab.
#define QTabBar_RightSide                         1   // Right side of the tab.
//This enum was introduced in Qt 4.5.

#define QTabBar_SelectLeftTab                     0   // Select the tab to the left of the one being removed.
#define QTabBar_SelectRightTab                    1   // Select the tab to the right of the one being removed.
#define QTabBar_SelectPreviousTab                 2   // Select the previously selected tab.
//This enum was introduced in Qt 4.5.

//enum QTabBar::Shape
//This enum type lists the built-in shapes supported by QTabBar. Treat these as hints as some styles may not render some of the shapes. However, position should be honored.
#define QTabBar_RoundedNorth                      0   // The normal rounded look above the pages
#define QTabBar_RoundedSouth                      1   // The normal rounded look below the pages
#define QTabBar_RoundedWest                       2   // The normal rounded look on the left side of the pages
#define QTabBar_RoundedEast                       3   // The normal rounded look on the right side the pages
#define QTabBar_TriangularNorth                   4   // Triangular tabs above the pages.
#define QTabBar_TriangularSouth                   5   // Triangular tabs similar to those used in the Excel spreadsheet, for example
#define QTabBar_TriangularWest                    6   // Triangular tabs on the left of the pages.
#define QTabBar_TriangularEast                    7   // Triangular tabs on the right of the pages.

/*----------------------------------------------------------------------*/

#define QGraphicsScene_BspTreeIndex                          0
#define QGraphicsScene_NoIndex                               -1

#define QGraphicsScene_ItemLayer                             0x1
#define QGraphicsScene_BackgroundLayer                       0x2
#define QGraphicsScene_ForegroundLayer                       0x4
#define QGraphicsScene_AllLayers                             0xffff

#define QGraphicsView_CacheNone                              0x0
#define QGraphicsView_CacheBackground                        0x1

#define QGraphicsView_NoDrag                                 0
#define QGraphicsView_ScrollHandDrag                         1
#define QGraphicsView_RubberBandDrag                         2

#define QGraphicsView_DontClipPainter                        0x1
#define QGraphicsView_DontSavePainterState                   0x2
#define QGraphicsView_DontAdjustForAntialiasing              0x4

#define QGraphicsView_NoAnchor                               0
#define QGraphicsView_AnchorViewCenter                       1
#define QGraphicsView_AnchorUnderMouse                       2

#define QGraphicsView_FullViewportUpdate                     0
#define QGraphicsView_MinimalViewportUpdate                  1
#define QGraphicsView_SmartViewportUpdate                    2
#define QGraphicsView_BoundingRectViewportUpdate             4
#define QGraphicsView_NoViewportUpdate                       3

#define QGraphicsItem_NoCache                                0
#define QGraphicsItem_ItemCoordinateCache                    1
#define QGraphicsItem_DeviceCoordinateCache                  2
#define QGraphicsItem_ItemEnabledChange                      3
#define QGraphicsItem_ItemEnabledHasChanged                  13
#define QGraphicsItem_ItemMatrixChange                       1
#define QGraphicsItem_ItemPositionChange                     0
#define QGraphicsItem_ItemPositionHasChanged                 9
#define QGraphicsItem_ItemTransformChange                    8
#define QGraphicsItem_ItemTransformHasChanged                10
#define QGraphicsItem_ItemSelectedChange                     4
#define QGraphicsItem_ItemSelectedHasChanged                 14
#define QGraphicsItem_ItemVisibleChange                      2
#define QGraphicsItem_ItemVisibleHasChanged                  12
#define QGraphicsItem_ItemParentChange                       5
#define QGraphicsItem_ItemParentHasChanged                   15
#define QGraphicsItem_ItemChildAddedChange                   6
#define QGraphicsItem_ItemChildRemovedChange                 7
#define QGraphicsItem_ItemSceneChange                        11
#define QGraphicsItem_ItemSceneHasChanged                    16
#define QGraphicsItem_ItemCursorChange                       17
#define QGraphicsItem_ItemCursorHasChanged                   18
#define QGraphicsItem_ItemToolTipChange                      19
#define QGraphicsItem_ItemToolTipHasChanged                  20
#define QGraphicsItem_ItemFlagsChange                        21
#define QGraphicsItem_ItemFlagsHaveChanged                   22
#define QGraphicsItem_ItemZValueChange                       23
#define QGraphicsItem_ItemZValueHasChanged                   24
#define QGraphicsItem_ItemOpacityChange                      25
#define QGraphicsItem_ItemOpacityHasChanged                  26
#define QGraphicsItem_ItemIsMovable                          0x1
#define QGraphicsItem_ItemIsSelectable                       0x2
#define QGraphicsItem_ItemIsFocusable                        0x4
#define QGraphicsItem_ItemClipsToShape                       0x8
#define QGraphicsItem_ItemClipsChildrenToShape               0x10
#define QGraphicsItem_ItemIgnoresTransformations             0x20
#define QGraphicsItem_ItemIgnoresParentOpacity               0x40
#define QGraphicsItem_ItemDoesntPropagateOpacityToChildren   0x80
#define QGraphicsItem_ItemStacksBehindParent                 0x100

#define QGraphicsPixmapItem_MaskShape                        0
#define QGraphicsPixmapItem_BoundingRectShape                1
#define QGraphicsPixmapItem_HeuristicMaskShape               2


#define QGraphicsSceneContextMenuEvent_Mouse                 0
#define QGraphicsSceneContextMenuEvent_Keyboard              1
#define QGraphicsSceneContextMenuEvent_Other                 2

#define QGradient_LogicalMode                                0   // This is the default mode. The gradient coordinates are specified logical space just like the object coordinates.
#define QGradient_StretchToDeviceMode                        1   // In this mode the gradient coordinates are relative to the bounding rectangle of the paint device, with (0,0) in the top left corner, and (1,1) in the bottom right corner of the paint device.
#define QGradient_ObjectBoundingMode                         2   // In this mode the gradient coordinates are relative to the bounding rectangle of the object being drawn, with (0,0) in the top left corner, and (1,1) in the bottom right corner of the object's bounding rectangle.

#define QGradient_PadSpread                                  0   // The area is filled with the closest stop color. This is the default.
#define QGradient_RepeatSpread                               2   // The gradient is repeated outside the gradient area.
#define QGradient_ReflectSpread                              1   // The gradient is reflected outside the gradient area.

#define QGradient_LinearGradient                             0   // Interpolates colors between start and end points (QLinearGradient).
#define QGradient_RadialGradient                             1   // Interpolate colors between a focal point and end points on a circle surrounding it (QRadialGradient).
#define QGradient_ConicalGradient                            2   // Interpolate colors around a center point (QConicalGradient).
#define QGradient_NoGradient                                 3   // No gradient is used.


#define QPainter_CompositionMode_SourceOver                  0
#define QPainter_CompositionMode_DestinationOver             1
#define QPainter_CompositionMode_Clear                       2
#define QPainter_CompositionMode_Source                      3
#define QPainter_CompositionMode_Destination                 4
#define QPainter_CompositionMode_SourceIn                    5
#define QPainter_CompositionMode_DestinationIn               6
#define QPainter_CompositionMode_SourceOut                   7
#define QPainter_CompositionMode_DestinationOut              8
#define QPainter_CompositionMode_SourceAtop                  9
#define QPainter_CompositionMode_DestinationAtop             10
#define QPainter_CompositionMode_Xor                         11
#define QPainter_CompositionMode_Plus                        12
#define QPainter_CompositionMode_Multiply                    13
#define QPainter_CompositionMode_Screen                      14
#define QPainter_CompositionMode_Overlay                     15
#define QPainter_CompositionMode_Darken                      16
#define QPainter_CompositionMode_Lighten                     17
#define QPainter_CompositionMode_ColorDodge                  18
#define QPainter_CompositionMode_ColorBurn                   19
#define QPainter_CompositionMode_HardLight                   20
#define QPainter_CompositionMode_SoftLight                   21
#define QPainter_CompositionMode_Difference                  22
#define QPainter_CompositionMode_Exclusion                   23
#define QPainter_RasterOp_SourceOrDestination                24
#define QPainter_RasterOp_SourceAndDestination               25
#define QPainter_RasterOp_SourceXorDestination               26
#define QPainter_RasterOp_NotSourceAndNotDestination         27
#define QPainter_RasterOp_NotSourceOrNotDestination          28
#define QPainter_RasterOp_NotSourceXorDestination            29
#define QPainter_RasterOp_NotSource                          30
#define QPainter_RasterOp_NotSourceAndDestination            31
#define QPainter_RasterOp_SourceAndNotDestination            32

#define QPainter_Antialiasing                                0x01
#define QPainter_TextAntialiasing                            0x02
#define QPainter_SmoothPixmapTransform                       0x04
#define QPainter_HighQualityAntialiasing                     0x08
#define QPainter_NonCosmeticDefaultPen                       0x10

/*----------------------------------------------------------------------*/
//                        HBQT Defined Constants
/*----------------------------------------------------------------------*/
/*
 *   DEFINES HBQt CODEBLOCKs
 *
 *   Format:
 *   HBQT_(Qt class initials)_(Qt overloaded member)
 */

#define HBQT_QAIM_data                            1001
#define HBQT_QAIM_flags                           1003
#define HBQT_QAIM_headerData                      2001
#define HBQT_QAIM_rowCount                        3001
#define HBQT_QAIM_columnCount                     3002

/*----------------------------------------------------------------------*/

#define HBQT_GRAPHICSITEM_NONE                    0
#define HBQT_GRAPHICSITEM_RECT                    1
#define HBQT_GRAPHICSITEM_LINE                    2
#define HBQT_GRAPHICSITEM_ELLIPSE                 3
#define HBQT_GRAPHICSITEM_ARC                     4
#define HBQT_GRAPHICSITEM_CHORD                   5
#define HBQT_GRAPHICSITEM_POLYGON                 6
#define HBQT_GRAPHICSITEM_PIE                     7
#define HBQT_GRAPHICSITEM_PATH                    8
#define HBQT_GRAPHICSITEM_CHART                   9
#define HBQT_GRAPHICSITEM_GRADIENT                10
#define HBQT_GRAPHICSITEM_PICTURE                 11
#define HBQT_GRAPHICSITEM_BARCODE                 12
#define HBQT_GRAPHICSITEM_TEXT                    13
#define HBQT_GRAPHICSITEM_SIMPLETEXT              14
#define HBQT_GRAPHICSITEM_ROUNDRECT               15

#define HBQT_GRAPHICSITEM_LINE_HORIZONTAL         0
#define HBQT_GRAPHICSITEM_LINE_VERTICAL           1
#define HBQT_GRAPHICSITEM_LINE_BACKWARDDIAGONAL   2
#define HBQT_GRAPHICSITEM_LINE_FORWARDDIAGONAL    3

#define HBQT_GRAPHICSITEM_TEXT_DRAW_NONE          0
#define HBQT_GRAPHICSITEM_TEXT_DRAW_TOP           1
#define HBQT_GRAPHICSITEM_TEXT_DRAW_BOTTOM        2
#define HBQT_GRAPHICSITEM_TEXT_DRAW_ABOVE         3
#define HBQT_GRAPHICSITEM_TEXT_DRAW_BELOW         4

#define HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_NONE    0
#define HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_AUTO    1
#define HBQT_GRAPHICSITEM_TEXT_SIZEPOLICY_STRETCH 2

#define HBQT_GRAPHICSITEM_IMAGE_NO_FRAME          0
#define HBQT_GRAPHICSITEM_IMAGE_PICTURE_BIND      1
#define HBQT_GRAPHICSITEM_IMAGE_PICTURE_BOX       2

#define HBQT_GRAPHICSITEM_RESIZE_ITEM_TO_PICTURE  1
#define HBQT_GRAPHICSITEM_CENTER_PICTURE_TO_ITEM  2
#define HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_KEEP_ASPECT_RATIO     3
#define HBQT_GRAPHICSITEM_RESIZE_PICTURE_TO_ITEM_IGNORE_ASPECT_RATIO   4

/*----------------------------------------------------------------------*/
//              HBQTableView:hbSetBlock() fired constants

#define HBQT_HBQTABLEVIEW_scrollContentsBy        1

/*----------------------------------------------------------------------*/

#endif
