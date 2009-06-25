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


#define Qt_AlignLeft                               1         // 0x0001  // Aligns with the left edge.
#define Qt_AlignRight                              2         // 0x0002  // Aligns with the right edge.
#define Qt_AlignHCenter                            4         // 0x0004  // Centers horizontally in the available space.
#define Qt_AlignJustify                            8         // 0x0008  // Justifies the text in the available space.

#define Qt_AlignTop                                32        // 0x0020  // Aligns with the top.
#define Qt_AlignBottom                             64        // 0x0040  // Aligns with the bottom.
#define Qt_AlignVCenter                            128       // 0x0080  // Centers vertically in the available space.

#define Qt_AlignCenter                             AlignVCenter+AlignHCenter   // Centers in both dimensions.

#define Qt_AlignAbsolute                           16        // 0x0010
           // If the widget's layout
           // direction is Qt_RightToLeft (instead of Qt_LeftToRight, the default),
           // Qt_AlignLeft refers to the right edge and Qt_AlignRight to the left edge.
           // This is normally the desired behavior. If you want Qt_AlignLeft to always mean
           // "left" and Qt_AlignRight to always mean "right", combine the flag with Qt_AlignAbsolute.
#define Qt_AlignLeading                           AlignLeft // Synonym for Qt_AlignLeft.
#define Qt_AlignTrailing                          AlignRight// Synonym for Qt_AlignRight.

#define Qt_AlignHorizontal_Mask                   AlignLeft + AlignRight + AlignHCenter + AlignJustify + AlignAbsolute
#define Qt_AlignVertical_Mask                     AlignTop + AlignBottom + AlignVCenter



#define QLineEdit_Normal                           0        // Display characters as they are entered. This is the default.
#define QLineEdit_NoEcho                           1        // Do not display anything. This may be appropriate for passwords where even the length of the password should be kept secret.
#define QLineEdit_Password                         2        // Display asterisks instead of the characters actually entered.
#define QLineEdit_PasswordEchoOnEdit               3


/* This enum type is used to define some modifier flags. Some of these flags only
 * make sense in the context of printing:
 */
#define Qt_TextSingleLine                          0x0100    // Treats all whitespace as spaces and prints just one line.
#define Qt_TextDontClip                            0x0200    // If it's impossible to stay within the given bounds, it prints outside.
#define Qt_TextExpandTabs                          0x0400    // Makes the U+0009 (ASCII tab) character move to the next tab stop.
#define Qt_TextShowMnemonic                        0x0800    // Displays the string "&P" as P (see QButton for an example). For an ampersand, use "&&".
#define Qt_TextWordWrap                            0x1000    // Breaks lines at appropriate points, e.g. at word boundaries.
#define Qt_TextWrapAnywhere                        0x2000    // Breaks lines anywhere, even within words.
#define Qt_TextHideMnemonic                        0x8000    // Same as Qt::TextShowMnemonic but doesn't draw the underlines.
#define Qt_TextDontPrint                           0x4000    // Treat this text as "hidden" and don't print it.
#define Qt_IncludeTrailingSpaces                   0x08000000// When this option is set, QTextLine::naturalTextWidth() and naturalTextRect()
                                                             // will return a value that includes the width of trailing spaces in the text; otherwise
                                                             // this width is excluded.
#define Qt_TextIncludeTrailingSpaces               0x08000000// Same as IncludeTrailingSpaces
#define Qt_TextJustificationForced                 0x10000   // Ensures that text lines are justified.

/* You can use as many modifier flags as you want, except that Qt::TextSingleLine
 * and Qt::TextWordWrap cannot be combined.
 */

/* This enum is used in widgets that can display both plain text and rich text, e.g. QLabel.
 * It is used for deciding whether a text string should be interpreted as one or the other.
 * This is normally done by passing one of the enum values to a setTextFormat() function.
 */
#define Qt_PlainText                               0        // The text string is interpreted as a plain text string.
#define Qt_RichText                                1        // The text string is interpreted as a rich text string.
#define Qt_AutoText                                2        // The text string is interpreted as for Qt::RichText
                                                            //   if Qt::mightBeRichText() returns true, otherwise as Qt::PlainText.

/* This enum specifies how a text displaying widget reacts to user input.
 */
#define Qt_NoTextInteraction                       0        // No interaction with the text is possible.
#define Qt_TextSelectableByMouse                   1        // Text can be selected with the mouse and copied to the clipboard using a context menu or standard keyboard shortcuts.
#define Qt_TextSelectableByKeyboard                2        // Text can be selected with the cursor keys on the keyboard. A text cursor is shown.
#define Qt_LinksAccessibleByMouse                  4        // Links can be highlighted and activated with the mouse.
#define Qt_LinksAccessibleByKeyboard               8        // Links can be focused using tab and activated with enter.
#define Qt_TextEditable                            16       // The text is fully editable.
#define Qt_TextEditorInteraction   ( TextSelectableByMouse + TextSelectableByKeyboard + TextEditable )  // The default for a text editor.
#define Qt_TextBrowserInteraction  ( TextSelectableByMouse + LinksAccessibleByMouse + LinksAccessibleByKeyboard )  // The default for QTextBrowser.


#define QMessageBox_InvalidRole                    -1        // The button is invalid.
#define QMessageBox_AcceptRole                     0         // Clicking the button causes the dialog to be accepted (e.g. OK).
#define QMessageBox_RejectRole                     1         // Clicking the button causes the dialog to be rejected (e.g. Cancel).
#define QMessageBox_DestructiveRole                2         // Clicking the button causes a destructive change (e.g. for Discarding Changes) and closes the dialog.
#define QMessageBox_ActionRole                     3         // Clicking the button causes changes to the elements within the dialog.
#define QMessageBox_HelpRole                       4         // The button can be clicked to request help.
#define QMessageBox_YesRole                        5         // The button is a "Yes"-like button.
#define QMessageBox_NoRole                         6         // The button is a "No"-like button.
#define QMessageBox_ApplyRole                      8         // The button applies current changes.
#define QMessageBox_ResetRole                      7         // The button resets the dialog's fields to default values.
                                                             //
#define QMessageBox_NoIcon                         0         // the message box does not have any icon.
#define QMessageBox_Question                       4         // an icon indicating that the message is asking a question.
#define QMessageBox_Information                    1         // an icon indicating that the message is nothing out of the ordinary.
#define QMessageBox_Warning                        2         // an icon indicating that the message is a warning, but can be dealt with.
#define QMessageBox_Critical                       3         // an icon indicating that the message represents a critical problem.

#define QMessageBox_Ok                             0x00000400   // An "OK" button defined with the AcceptRole.
#define QMessageBox_Open                           0x00002000   // A "Open" button defined with the AcceptRole.
#define QMessageBox_Save                           0x00000800   // A "Save" button defined with the AcceptRole.
#define QMessageBox_Cancel                         0x00400000   // A "Cancel" button defined with the RejectRole.
#define QMessageBox_Close                          0x00200000   // A "Close" button defined with the RejectRole.
#define QMessageBox_Discard                        0x00800000   // A "Discard" or "Don't Save" button, depending on the platform, defined with the DestructiveRole.
#define QMessageBox_Apply                          0x02000000   // An "Apply" button defined with the ApplyRole.
#define QMessageBox_Reset                          0x04000000   // A "Reset" button defined with the ResetRole.
#define QMessageBox_RestoreDefaults                0x08000000   // A "Restore Defaults" button defined with the ResetRole.
#define QMessageBox_Help                           0x01000000   // A "Help" button defined with the HelpRole.
#define QMessageBox_SaveAll                        0x00001000   // A "Save All" button defined with the AcceptRole.
#define QMessageBox_Yes                            0x00004000   // A "Yes" button defined with the YesRole.
#define QMessageBox_YesToAll                       0x00008000   // A "Yes to All" button defined with the YesRole.
#define QMessageBox_No                             0x00010000   // A "No" button defined with the NoRole.
#define QMessageBox_NoToAll                        0x00020000   // A "No to All" button defined with the NoRole.
#define QMessageBox_Abort                          0x00040000   // An "Abort" button defined with the RejectRole.
#define QMessageBox_Retry                          0x00080000   // A "Retry" button defined with the AcceptRole.
#define QMessageBox_Ignore                         0x00100000   // An "Ignore" button defined with the AcceptRole.
#define QMessageBox_NoButton                       0x00000000   // An invalid button.

/*----------------------------------------------------------------------*/

#define Qt_WA_AcceptDrops                          78       // Allows data from drag and drop operations to be dropped onto the widget (see QWidget::setAcceptDrops()).
#define Qt_WA_AlwaysShowToolTips                   84       // Enables tooltips for inactive windows.
#define Qt_WA_ContentsPropagated                   3        // This flag is superfluous and obsolete; it no longer has any effect. Since Qt 4.1, all widgets that do not set WA_PaintOnScreen propagate their contents.
#define Qt_WA_CustomWhatsThis                      47       // Indicates that the widget wants to continue operating normally in "What's This?" mode. This is set by the widget's author.
#define Qt_WA_DeleteOnClose                        55       // Makes Qt delete this widget when the widget has accepted the close event (see QWidget::closeEvent()).
#define Qt_WA_Disabled                             0        // Indicates that the widget is disabled, i.e. it does not receive any mouse or keyboard events. There is also a getter functions QWidget::isEnabled(). This is set/cleared by the Qt kernel.
#define Qt_WA_ForceDisabled                        32       // Indicates that the widget is explicitly disabled, i.e. it will remain disabled even when all its ancestors are set to the enabled state. This implies WA_Disabled. This is set/cleared by QWidget::setEnabled() and QWidget::setDisabled().
#define Qt_WA_ForceUpdatesDisabled                 59       // Indicates that updates are explicitly disabled for the widget; i.e. it will remain disabled even when all its ancestors are set to the updates-enabled state. This implies WA_UpdatesDisabled. This is set/cleared by QWidget::setUpdatesEnabled().
#define Qt_WA_GroupLeader                          72       // This attribute has been deprecated. Use QWidget::windowModality instead.
#define Qt_WA_Hover                                74       // Forces Qt to generate paint events when the mouse enters or leaves the widget. This feature is typically used when implementing custom styles; see the Styles example for details.
#define Qt_WA_InputMethodEnabled                   14       // Enables input methods for Asian languages. Must be set when creating custom text editing widgets. On Windows CE this flag can be used in addition to QApplication::autoSipEnabled to automatically display the SIP when entering a widget.
#define Qt_WA_KeyboardFocusChange                  77       // Set on a toplevel window when the users changes focus with the keyboard (tab, backtab, or shortcut).
#define Qt_WA_KeyCompression                       33       // Enables key event compression if set, and disables it if not set. By default key compression is off, so widgets receive one key press event for each key press (or more, since autorepeat is usually on). If you turn it on and your program doesn't keep up with key input, Qt may try to compress key events so that more than one character can be processed in each event. For example, a word processor widget might receive 2, 3 or more characters in each QKeyEvent::text(), if the layout recalculation takes too long for the CPU. If a widget supports multiple character unicode input, it is always safe to turn the compression on. Qt performs key event compression only for printable characters. Qt_Modifier keys, cursor movement keys, function keys and miscellaneous action keys (e.g. Escape, Enter, Backspace, PrintScreen) will stop key event compression, even if there are more compressible key events available. Platforms other than Mac and X11 do not support this compression, in which case turning it on will have no effect. This is set/cleared by the widget's author.
#define Qt_WA_LayoutOnEntireRect                   48       // Indicates that the widget wants QLayout to operate on the entire QWidget::rect(), not only on QWidget::contentsRect(). This is set by the widget's author.
#define Qt_WA_LayoutUsesWidgetRect                 92       // Ignore the layout item rect from the style when laying out this widget with QLayout. This makes a difference in QMacStyle and QPlastiqueStyle for some widgets.
#define Qt_WA_MacNoClickThrough                    12       // When a widget that has this attribute set is clicked, and its window is inactive, the click will make the window active but won't be seen by the widget. Typical use of this attribute is on widgets with "destructive" actions, such as a "Delete" button. WA_MacNoClickThrough also applies to all child widgets of the widget that has it set.
#define Qt_WA_MacOpaqueSizeGrip                    85       // Indicates that the native Carbon size grip should be opaque instead of transparent (the default). This attribute is only applicable to Mac OS X and is set by the widget's author.
#define Qt_WA_MacShowFocusRect                     88       // Indicates that this widget should get a QFocusFrame around it. Some widgets draw their own focus halo regardless of this attribute. Not that the QWidget::focusPolicy also plays the main role in whether something is given focus or not, this only controls whether or not this gets the focus frame. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacNormalSize                        89       // Indicates the widget should have the normal size for widgets in Mac OS X. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacSmallSize                         90       // Indicates the widget should have the small size for widgets in Mac OS X. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacMiniSize                          91       // Indicates the widget should have the mini size for widgets in Mac OS X. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacVariableSize                      102      // Indicates the widget can choose between alternative sizes for widgets to avoid clipping. This attribute is only applicable to Mac OS X.
#define Qt_WA_MacBrushedMetal                      46       // Indicates the widget should be drawn in the brushed metal style as supported by the windowing system. This attribute is only applicable to Mac OS X.
#define Qt_WA_Mapped                               11       // Indicates that the widget is mapped on screen. This is set/cleared by the Qt kernel.
#define Qt_WA_MouseNoMask                          71       // Makes the widget receive mouse events for the entire widget regardless of the currently set mask, overriding QWidget::setMask(). This is not applicable for top-level windows.
#define Qt_WA_MouseTracking                        2        // Indicates that the widget has mouse tracking enabled. See QWidget::mouseTracking.
#define Qt_WA_Moved                                43       // Indicates that the widget has an explicit position. This is set/cleared by QWidget::move() and by QWidget::setGeometry().
#define Qt_WA_MSWindowsUseDirect3D                 94       // Makes drawing to a widget with this attribute set use the Direct3D paint engine, if the Direct3D paint engine is available. This functionality is experimental.
#define Qt_WA_NoBackground        WA_OpaquePaintEvent       // This value is obsolete. Use WA_OpaquePaintEvent instead.
#define Qt_WA_NoChildEventsForParent               58       // Indicates that the widget does not want ChildAdded or ChildRemoved events sent to its parent. This is rarely necessary but can help to avoid automatic insertion widgets like splitters and layouts. This is set by a widget's author.
#define Qt_WA_NoChildEventsFromChildren            39       // Indicates that the widget does not want to receive ChildAdded or ChildRemoved events sent from its children. This is set by a widget's author.
#define Qt_WA_NoMouseReplay                        54       // Used for pop-up widgets. Indicates that the most recent mouse press event should not be replayed when the pop-up widget closes. The flag is set by the widget's author and cleared by the Qt kernel every time the widget receives a new mouse event.
#define Qt_WA_NoMousePropagation                   73       // Prohibits mouse events from being propagated to the widget's parent. This attribute is disabled by default.
#define Qt_WA_TransparentForMouseEvents            51       // When enabled, this attribute disables the delivery of mouse events to the widget and its children. Mouse events are delivered to other widgets as if the widget and its children were not present in the widget hierarchy; mouse clicks and other events effectively "pass through" them. This attribute is disabled by default.
#define Qt_WA_NoSystemBackground                   9        // Indicates that the widget has no background, i.e. when the widget receives paint events, the background is not automatically repainted. Note: Unlike WA_OpaquePaintEvent, newly exposed areas are never filled with the background (e.g., after showing a window for the first time the user can see "through" it until the application processes the paint events). This flag is set or cleared by the widget's author.
#define Qt_WA_OpaquePaintEvent                     4        // Indicates that the widget paints all its pixels when it receives a paint event. Thus, it is not required for operations like updating, resizing, scrolling and focus changes to erase the widget before generating paint events. The use of WA_OpaquePaintEvent provides a small optimization by helping to reduce flicker on systems that do not support double buffering and avoiding computational cycles necessary to erase the background prior to painting. Note: Unlike WA_NoSystemBackground, WA_OpaquePaintEvent makes an effort to avoid transparent window backgrounds. This flag is set or cleared by the widget's author.
#define Qt_WA_OutsideWSRange                       49       // Indicates that the widget is outside the valid range of the window system's coordinate system. A widget outside the valid range cannot be mapped on screen. This is set/cleared by the Qt kernel.
#define Qt_WA_PaintOnScreen                        8        // Indicates that the widget wants to draw directly onto the screen. Widgets with this attribute set do not participate in composition management, i.e. they cannot be semi-transparent or shine through semi-transparent overlapping widgets. Note: This flag is only supported on X11 and it disables double buffering. On Qt for Embedded Linux, the flag only works when set on a top-level widget and it relies on support from the active screen driver. This flag is set or cleared by the widget's author. To render outside of Qt's paint system, e.g., if you require native painting primitives, you need to reimplement QWidget::paintEngine() to return 0 and set this flag.
#define Qt_WA_PaintOutsidePaintEvent               13       // Makes it possible to use QPainter to paint on the widget outside paintEvent(). This flag is not supported on Windows, Mac OS X or Embedded Linux. We recommend that you use it only when porting Qt 3 code to Qt 4.
#define Qt_WA_PaintUnclipped                       52       // Makes all painters operating on this widget unclipped. Children of this widget or other widgets in front of it do not clip the area the painter can paint on. This flag is only supported for widgets with the WA_PaintOnScreen flag set. The preferred way to do this in a cross platform way is to create a transparent widget that lies in front of the other widgets.
#define Qt_WA_PendingMoveEvent                     34       // Indicates that a move event is pending, e.g., when a hidden widget was moved. This flag is set or cleared by the Qt kernel.
#define Qt_WA_PendingResizeEvent                   35       // Indicates that a resize event is pending, e.g., when a hidden widget was resized. This flag is set or cleared by the Qt kernel.
#define Qt_WA_QuitOnClose                          76       // Makes Qt quit the application when the last widget with the attribute set has accepted closeEvent(). This behavior can be modified with the QApplication::quitOnLastWindowClosed property. By default this attribute is set for all widgets of type Qt_Window.
#define Qt_WA_Resized                              42       // Indicates that the widget has an explicit size. This flag is set or cleared by QWidget::resize() and QWidget::setGeometry().
#define Qt_WA_RightToLeft                          56       // Indicates that the layout direction for the widget is right to left.
#define Qt_WA_SetCursor                            38       // Indicates that the widget has a cursor of its own. This flag is set or cleared by QWidget::setCursor() and QWidget::unsetCursor().
#define Qt_WA_SetFont                              37       // Indicates that the widget has a font of its own. This flag is set or cleared by QWidget::setFont().
#define Qt_WA_SetPalette                           36       // Indicates that the widget has a palette of its own. This flag is set or cleared by QWidget::setPalette().
#define Qt_WA_SetStyle                             86       // Indicates that the widget has a style of its own. This flag is set or cleared by QWidget::setStyle().
#define Qt_WA_ShowModal                            70       // This attribute has been deprecated. Use QWidget::windowModality instead.
#define Qt_WA_StaticContents                       5        // Indicates that the widget contents are north-west aligned and static. On resize, such a widget will receive paint events only for parts of itself that are newly visible. This flag is set or cleared by the widget's author.
#define Qt_WA_StyleSheet                           97       // Indicates that the widget is styled using a style sheet.
#define Qt_WA_TranslucentBackground                120      // Indicates that the widget should have a translucent background, i.e., any non-opaque regions of the widgets will be translucent because the widget will have an alpha channel. Setting this flag causes WA_NoSystemBackground to be set. This flag is set or cleared by the widget's author.
#define Qt_WA_UnderMouse                           1        // Indicates that the widget is under the mouse cursor. The value is not updated correctly during drag and drop operations. There is also a getter function, QWidget::underMouse(). This flag is set or cleared by the Qt kernel.
#define Qt_WA_UpdatesDisabled                      10       //

/*----------------------------------------------------------------------*/

#define _HBQT_CH
#endif