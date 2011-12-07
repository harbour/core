/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ xbpTreeView compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               26Nov2008
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

CLASS WvgComboBox  INHERIT  WvgListBox  /* WvgSLE */


   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgComboBox

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::WvgActiveXControl:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD
   ::className   := "BUTTON"
   ::objType     := objTypePushButton

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgComboBox

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oParent:AddChild( SELF )

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgComboBox

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD destroy() CLASS WvgComboBox

   ::wvgWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/
/*                         MSDN on ComboBox Control                     */
/*----------------------------------------------------------------------*/
#if 0

Functions
=========

DlgDirListComboBox
Replaces the contents of a combo box with the names of the subdirectories and files
in a specified directory. You can filter the list of names by specifying a set of file
attributes. The list of names can include mapped drive letters.

DlgDirSelectComboBoxEx
Retrieves the current selection from a combo box filled by using the
DlgDirListComboBox function. The selection is interpreted as a drive letter, a file,
or a directory name.

GetComboBoxInfo
Retrieves information about the specified combo box.


Macros
======

ComboBox_AddItemData
Adds item data to the list in a combo box at the specified location.
You can use this macro or send the CB_ADDSTRING message explicitly.

ComboBox_AddString
Adds a string to a list in a combo box. If the combo box does not have the CBS_SORT style,
the string is added to the end of the list. Otherwise, the string is inserted into the
list and the list is sorted. You can use this macro or send the CB_ADDSTRING message explicitly.

ComboBox_DeleteString
Deletes the item at the specified location in a list in a combo box. You can use
this macro or send the CB_DELETESTRING message explicitly.

ComboBox_Dir
Adds names to the list displayed by a combo box. The macro adds the names of
directories and files that match a specified string and set of file attributes.
It can also add mapped drive letters to the list in a combo box. You can use this
macro or send the CB_DIR message explicitly.

ComboBox_Enable
Enables or disables a combo box control.

ComboBox_FindItemData
Finds the first item in a combo box list that has the specified item data. You can use
this macro or send the CB_FINDSTRING message explicitly.

ComboBox_FindString
Finds the first string in a combo box list that begins with the specified string.
You can use this macro or send the CB_FINDSTRING message explicitly.

ComboBox_FindStringExact
Finds the first string in a combo box list that exactly matches the specified string,
except that the search is not case sensitive. You can use this macro or send the
CB_FINDSTRINGEXACT message explicitly.

ComboBox_GetCount
Gets the number of items in the list box of a combo box. You can use this macro or
send the CB_GETCOUNT message explicitly.

ComboBox_GetCueBannerText
Gets the cue banner text displayed in the edit control of a combo box. Use this
macro or send the CB_GETCUEBANNER message explicitly.

ComboBox_GetCurSel
Gets the index of the currently selected item in a combo box. You can use this
macro or send the CB_GETCURSEL message explicitly.

ComboBox_GetDroppedControlRect
Retrieves the screen coordinates of a combo box in its dropped-down state.
You can use this macro or send the CB_GETDROPPEDCONTROLRECT message explicitly.

ComboBox_GetExtendedUI
Ascertains whether a combo box is using the default user interface (UI) or the extended UI.
You can use this macro or send the CB_GETEXTENDEDUI message explicitly.

ComboBox_GetGetDroppedState
Ascertains whether the drop list in a combo box control is visible.
You can use this macro or send the CB_GETDROPPEDSTATE message explicitly.

ComboBox_GetItemData
Gets the application-defined value associated with the specified list item in a combo box.
You can use this macro or send the CB_GETITEMDATA message explicitly.

ComboBox_GetItemHeight
Retrieves the height of list items in a combo box.
You can use this macro or send the CB_GETITEMHEIGHT message explicitly.

ComboBox_GetLBText
Gets a string from a list in a combo box.
You can use this macro or send the CB_GETLBTEXT message explicitly.

ComboBox_GetLBTextLen
Gets the length of a string in the list in a combo box.
You can use this macro or send the CB_GETLBTEXTLEN message explicitly.

ComboBox_GetMinVisible
Gets the minimum number of visible items in the drop-down list of a combo box.

ComboBox_GetText
Retrieves the text from a combo box control.

ComboBox_GetTextLength
Gets the number of characters in the text of a combo box.

ComboBox_InsertItemData
Inserts item data in a list in a combo box at the specified location.
You can use this macro or send the CB_INSERTSTRING message explicitly.

ComboBox_InsertString
Adds a string to a list in a combo box at the specified location.
You can use this macro or send the CB_INSERTSTRING message explicitly.

ComboBox_LimitText
Limits the length of the text the user may type into the edit control of a combo box.
You can use this macro or send the CB_LIMITTEXT message explicitly.

ComboBox_ResetContent
Removes all items from the list box and edit control of a combo box.
You can use this macro or send the CB_RESETCONTENT message explicitly.

ComboBox_SelectItemData
Searches a list in a combo box for an item that has the specified item data.
If a matching item is found, the item is selected. You can use this macro or send the
CB_SELECTSTRING message explicitly.

ComboBox_SelectString
Searches a list in a combo box for an item that begins with the characters in a specified string.
If a matching item is found, the item is selected. You can use this macro or send the
CB_SELECTSTRING message explicitly.

ComboBox_SetCueBannerText
Sets the cue banner text that is displayed for the edit control of a combo box.

ComboBox_SetCurSel
Sets the currently selected item in a combo box. You can use this macro or send the
CB_SETCURSEL message explicitly.

ComboBox_SetExtendedUI
Selects either the default user interface (UI) or the extended UI for a combo box that has the
CBS_DROPDOWN or CBS_DROPDOWNLIST style. You can use this macro or send the
CB_SETEXTENDEDUI message explicitly.

ComboBox_SetItemData
Sets the application-defined value associated with the specified list item in a combo box.
You can use this macro or send the CB_SETITEMDATA message explicitly.

ComboBox_SetItemHeight
Sets the height of list items or the selection field in a combo box.
You can use this macro or send the CB_SETITEMHEIGHT message explicitly.

ComboBox_SetMinVisible
Sets the minimum number of visible items in the drop-down list of a combo box.

SetText
Sets the text of a combo box.

ComboBox_ShowDropdown
Shows or hides the list in a combo box.
You can use this macro or send the CB_RESETCONTENT message explicitly.

ListBox_AddItemData
Adds item data to the list box at the specified location.
You can use this macro or send the LB_ADDSTRING message explicitly.



Messages
========

CB_ADDSTRING
Adds a string to the list box of a combo box. If the combo box does not have
the CBS_SORT style, the string is added to the end of the list. Otherwise, the
string is inserted into the list, and the list is sorted.

CB_DELETESTRING
Deletes a string in the list box of a combo box.

CB_DIR
Adds names to the list displayed by the combo box. The message adds the names of
directories and files that match a specified string and set of file attributes.
CB_DIR can also add mapped drive letters to the list.

CB_FINDSTRING
Searches the list box of a combo box for an item beginning with the characters in
a specified string.

CB_FINDSTRINGEXACT
Finds the first list box string in a combo box that matches the string specified in
the lParam parameter.

CB_GETCOMBOBOXINFO
Gets information about the specified combo box.

CB_GETCOUNT
Gets the number of items in the list box of a combo box.

CB_GETCUEBANNER
Gets the cue banner text displayed in the edit control of a combo box.
Send this message explicitly or by using the ComboBox_GetCueBannerText macro.

CB_GETCURSEL
An application sends a CB_GETCURSEL message to retrieve the index of the currently
selected item, if any, in the list box of a combo box.

CB_GETDROPPEDCONTROLRECT
An application sends a CB_GETDROPPEDCONTROLRECT message to retrieve the screen coordinates of a
combo box in its dropped-down state.

CB_GETDROPPEDSTATE
Determines whether the list box of a combo box is dropped down.

CB_GETDROPPEDWIDTH
Gets the minimum allowable width, in pixels, of the list box of a combo box with the
CBS_DROPDOWN or CBS_DROPDOWNLIST style.

CB_GETEDITSEL
Gets the starting and ending character positions of the current selection in the edit
control of a combo box.

CB_GETEXTENDEDUI
Determines whether a combo box has the default user interface or the extended user interface.

CB_GETHORIZONTALEXTENT
Gets the width, in pixels, that the list box can be scrolled horizontally (the scrollable width).
This is applicable only if the list box has a horizontal scroll bar.

CB_GETITEMDATA
An application sends a CB_GETITEMDATA message to a combo box to retrieve the
application-supplied value associated with the specified item in the combo box.

CB_GETITEMHEIGHT
Determines the height of list items or the selection field in a combo box.

CB_GETLBTEXT
Gets a string from the list of a combo box.

CB_GETLBTEXTLEN
Gets the length, in characters, of a string in the list of a combo box.

CB_GETLOCALE
Gets the current locale of the combo box. The locale is used to determine the
correct sorting order of displayed text for combo boxes with the CBS_SORT style and
text added by using the CB_ADDSTRING message.

CB_GETMINVISIBLE
Gets the minimum number of visible items in the drop-down list of a combo box.

CB_GETTOPINDEX
An application sends the CB_GETTOPINDEX message to retrieve the zero-based index of
the first visible item in the list box portion of a combo box. Initially, the item with
index 0 is at the top of the list box, but if the list box contents have been scrolled,
another item may be at the top.

CB_INITSTORAGE
An application sends the CB_INITSTORAGE message before adding a large number of items
to the list box portion of a combo box. This message allocates memory for storing list box items.

CB_INSERTSTRING
Inserts a string or item data into the list of a combo box. Unlike the CB_ADDSTRING message,
the CB_INSERTSTRING message does not cause a list with the CBS_SORT style to be sorted.

CB_LIMITTEXT
Limits the length of the text the user may type into the edit control of a combo box.

CB_RESETCONTENT
Removes all items from the list box and edit control of a combo box.

CB_SELECTSTRING
Searches the list of a combo box for an item that begins with the characters in a specified string.
If a matching item is found, it is selected and copied to the edit control.

CB_SETCUEBANNER
Sets the cue banner text that is displayed for the edit control of a combo box.

CB_SETCURSEL
An application sends a CB_SETCURSEL message to select a string in the list of a combo box.
If necessary, the list scrolls the string into view. The text in the edit control of the
combo box changes to reflect the new selection, and any previous selection in the list
is removed.

CB_SETDROPPEDWIDTH
An application sends the CB_SETDROPPEDWIDTH message to set the maximum allowable width,
in pixels, of the list box of a combo box with the CBS_DROPDOWN or CBS_DROPDOWNLIST style.

CB_SETEDITSEL
An application sends a CB_SETEDITSEL message to select characters in the edit control of a combo box.

CB_SETEXTENDEDUI
An application sends a CB_SETEXTENDEDUI message to select either the default user
interface (UI) or the extended UI for a combo box that has the CBS_DROPDOWN or CBS_DROPDOWNLIST style.

CB_SETHORIZONTALEXTENT
An application sends the CB_SETHORIZONTALEXTENT message to set the width, in pixels,
by which a list box can be scrolled horizontally (the scrollable width). If the width of
the list box is smaller than this value, the horizontal scroll bar horizontally scrolls items
in the list box. If the width of the list box is equal to or greater than this value, the horizontal
scroll bar is hidden or, if the combo box has the CBS_DISABLENOSCROLL style, disabled.

CB_SETITEMDATA
An application sends a CB_SETITEMDATA message to set the value associated with the specified
item in a combo box.

CB_SETITEMHEIGHT
An application sends a CB_SETITEMHEIGHT message to set the height of list items or the
selection field in a combo box.

CB_SETLOCALE
An application sends a CB_SETLOCALE message to set the current locale of the combo box.
If the combo box has the CBS_SORT style and strings are added using CB_ADDSTRING, the locale
   of a combo box affects how list items are sorted.

CB_SETMINVISIBLE
An application sends a CB_SETMINVISIBLE message to set the minimum number of visible items
in the drop-down list of a combo box.

CB_SETTOPINDEX
An application sends the CB_SETTOPINDEX message to ensure that a particular item is visible
in the list box of a combo box. The system scrolls the list box contents so that either the
specified item appears at the top of the list box or the maximum scroll range has been reached.

CB_SHOWDROPDOWN
An application sends a CB_SHOWDROPDOWN message to show or hide the list box of a combo box
that has the CBS_DROPDOWN or CBS_DROPDOWNLIST style.



Notifications
=============

CBN_CLOSEUP
The CBN_CLOSEUP notification message is sent when the list box of a combo box has been closed.
The parent window of the combo box receives this notification message through the WM_COMMAND message.

CBN_DBLCLK
The CBN_DBLCLK notification message is sent when the user double-clicks a string in the list
box of a combo box. The parent window of the combo box receives this notification message
through the WM_COMMAND message.

CBN_DROPDOWN
The CBN_DROPDOWN notification message is sent when the list box of a combo box is about to
be made visible. The parent window of the combo box receives this notification message
through the WM_COMMAND message.

CBN_EDITCHANGE
The CBN_EDITCHANGE notification message is sent after the user has taken an action that may
have altered the text in the edit control portion of a combo box. Unlike the
CBN_EDITUPDATE notification message, this notification message is sent after the system
updates the screen. The parent window of the combo box receives this notification message
through the WM_COMMAND message.

CBN_EDITUPDATE
The CBN_EDITUPDATE notification message is sent when the edit control portion of a combo
box is about to display altered text. This notification message is sent after the control
has formatted the text, but before it displays the text. The parent window of the combo box
receives this notification message through the WM_COMMAND message.

CBN_ERRSPACE
The CBN_ERRSPACE notification message is sent when a combo box cannot allocate enough
memory to meet a specific request. The parent window of the combo box receives this
notification message through the WM_COMMAND message.

CBN_KILLFOCUS
The CBN_KILLFOCUS notification message is sent when a combo box loses the keyboard focus.
The parent window of the combo box receives this notification message through the WM_COMMAND message.

CBN_SELCHANGE
The CBN_SELCHANGE notification message is sent when the user changes the current selection
in the list box of a combo box. The user can change the selection by clicking in the list
box or by using the arrow keys. The parent window of the combo box receives this
notification in the form of a WM_COMMAND message with CBN_SELCHANGE in the high-order
word of the wParam parameter.

CBN_SELENDCANCEL
The CBN_SELENDCANCEL notification message is sent when the user selects an item,
but then selects another control or closes the dialog box. It indicates the user
s initial selection is to be ignored. The parent window of the combo box receives
this notification message through the WM_COMMAND message.

CBN_SELENDOK
The CBN_SELENDOK notification message is sent when the user selects a list item,
or selects an item and then closes the list. It indicates that the user s selection
is to be processed. The parent window of the combo box receives this notification
message through the WM_COMMAND message.

CBN_SETFOCUS
The CBN_SETFOCUS notification message is sent when a combo box receives the keyboard focus.
The parent window of the combo box receives this notification message through the WM_COMMAND message.

WM_COMPAREITEM
The system sends the WM_COMPAREITEM message to determine the relative position of a new
item in the sorted list of an owner-drawn combo box or list box. Whenever the application
adds a new item, the system sends this message to the owner of a combo box or list box created
with the CBS_SORT or LBS_SORT style.

WM_DRAWITEM

The WM_DRAWITEM message is sent to the parent window of an owner-drawn button, combo box,
list box, or menu when a visual aspect of the button, combo box, list box, or menu has changed.
A window receives this message through its WindowProc function.

WM_MEASUREITEM
The WM_MEASUREITEM message is sent to the owner window of a combo box, list box,
list view control, or menu item when the control or menu is created.
A window receives this message through its WindowProc function.

Structures
==========

COMBOBOXINFO
Contains combo box status information.

COMPAREITEMSTRUCT

Supplies the identifiers and application-supplied data for two items in a sorted,
owner-drawn list box or combo box.

Whenever an application adds a new item to an owner-drawn list box or combo box
created with the CBS_SORT or LBS_SORT style, the system sends the owner a
WM_COMPAREITEM message. The lParam parameter of the message contains a long pointer
to a COMPAREITEMSTRUCT structure. Upon receiving the message, the owner compares
the two items and returns a value indicating which item sorts before the other.

DRAWITEMSTRUCT
Provides necessary information the owner window to determine how to paint an owner-drawn
control or menu item. The owner window of the owner-drawn control or menu item receives a
pointer to this structure as the lParam parameter of the WM_DRAWITEM message.

MEASUREITEMSTRUCT
Informs the system of the dimensions of an owner-drawn control or menu item.
This allows the system to process user interaction with the control correctly.



Combo Box Styles
To create a combo box using the CreateWindow or CreateWindowEx function, specify
the COMBOBOX class, appropriate window style constants, and a combination of the
following combo box styles.

Constants

CBS_AUTOHSCROLL
Automatically scrolls the text in an edit control to the right when the user types a
character at the end of the line. If this style is not set, only text that fits within
the rectangular boundary is allowed.

CBS_DISABLENOSCROLL
Shows a disabled vertical scroll bar in the list box when the box does not contain
enough items to scroll. Without this style, the scroll bar is hidden when the list box
does not contain enough items.

CBS_DROPDOWN
Similar to CBS_SIMPLE, except that the list box is not displayed unless the user selects
an icon next to the edit control.

CBS_DROPDOWNLIST
Similar to CBS_DROPDOWN, except that the edit control is replaced by a static text
item that displays the current selection in the list box.

CBS_HASSTRINGS
Specifies that an owner-drawn combo box contains items consisting of strings.
The combo box maintains the memory and address for the strings so the application
can use the CB_GETLBTEXT message to retrieve the text for a particular item.
For accessibility issues, see Exposing Owner-Drawn Combo Box Items


CBS_LOWERCASE
Converts to lowercase all text in both the selection field and the list.

CBS_NOINTEGRALHEIGHT
Specifies that the size of the combo box is exactly the size specified by the
application when it created the combo box. Normally, the system sizes a combo box
so that it does not display partial items.

CBS_OEMCONVERT
Converts text entered in the combo box edit control from the Windows character
set to the OEM character set and then back to the Windows character set.
This ensures proper character conversion when the application calls the
CharToOem function to convert a Windows string in the combo box to OEM characters.
This style is most useful for combo boxes that contain file names and applies only
to combo boxes created with the CBS_SIMPLE or CBS_DROPDOWN style.

CBS_OWNERDRAWFIXED
Specifies that the owner of the list box is responsible for drawing its contents
and that the items in the list box are all the same height. The owner window
receives a WM_MEASUREITEM message when the combo box is created and a WM_DRAWITEM
message when a visual aspect of the combo box has changed.

CBS_OWNERDRAWVARIABLE
Specifies that the owner of the list box is responsible for drawing its contents
and that the items in the list box are variable in height. The owner window receives
a WM_MEASUREITEM message for each item in the combo box when you create the combo box
and a WM_DRAWITEM message when a visual aspect of the combo box has changed.

CBS_SIMPLE
Displays the list box at all times. The current selection in the list box is displayed
in the edit control.

CBS_SORT
Automatically sorts strings added to the list box.

CBS_UPPERCASE
Converts to uppercase all text in both the selection field and the list.

#endif
/*----------------------------------------------------------------------*/
