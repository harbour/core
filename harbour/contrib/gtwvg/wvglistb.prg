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

CLASS WvgListBox  INHERIT  WvgWindow, DataRef

   DATA     adjustHeight                          INIT .F.
   DATA     horizScroll                           INIT .F.
   DATA     markMode                              INIT WVGLISTBOX_MM_SINGLE
   DATA     multiColumn                           INIT .F.
   DATA     vertScroll                            INIT .T.
   DATA     drawMode                              INIT WVG_DRAW_NORMAL

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()

   METHOD   handleEvent( nMessage, aNM )

   METHOD   getItemHeight()                       INLINE  ::sendMessage( LB_GETITEMHEIGHT, 0, 0 )
   METHOD   getTopItem()                          INLINE  ::sendMessage( LB_GETTOPINDEX, 0, 0 )
   METHOD   getVisibleItems()                     VIRTUAL
   METHOD   numItems()                            INLINE  ::sendMessage( LB_GETCOUNT, 0, 0 )
   METHOD   setItemsHeight( nPixel )              INLINE  ::sendMessage( LB_SETITEMHEIGHT, 0, nPixel )
   METHOD   setTopItem( nIndex )                  INLINE  ::sendMessage( LB_SETTOPINDEX, nIndex-1, 0 )

   METHOD   addItem( cItem )                      INLINE  WVG_SendMessageText( ::hWnd, LB_ADDSTRING, 0, cItem )
   METHOD   clear()                               VIRTUAL
   METHOD   delItem( nIndex )                     INLINE  ::sendMessage( LB_DELETESTRING, nIndex-1, 0 )
   METHOD   getItem( nIndex )                     INLINE  WVG_LBGetText( ::hWnd, nIndex-1 )
   METHOD   getTabstops()                         VIRTUAL
   METHOD   insItem( nIndex, cItem )              INLINE  WVG_SendMessageText( ::hWnd, LB_INSERTSTRING, nIndex-1, cItem )
   METHOD   setColumnWidth()                      VIRTUAL
   METHOD   setItem( nIndex, cItem )              INLINE  ::delItem( nIndex ), ::insItem( nIndex, cItem )
   METHOD   setTabstops()                         VIRTUAL


   DATA     sl_hScroll
   ACCESS   hScroll                               INLINE ::sl_hScroll
   ASSIGN   hScroll( bBlock )                     INLINE ::sl_hScroll := bBlock

   DATA     sl_vScroll
   ACCESS   vScroll                               INLINE ::sl_vScroll
   ASSIGN   vScroll( bBlock )                     INLINE ::sl_vScroll := bBlock

   DATA     sl_itemMarked
   ACCESS   itemMarked                            INLINE ::sl_itemMarked
   ASSIGN   itemMarked( bBlock )                  INLINE ::sl_itemMarked := bBlock

   DATA     sl_itemSelected
   ACCESS   itemSelected                          INLINE ::sl_itemSelected
   ASSIGN   itemSelected( bBlock )                INLINE ::sl_itemSelected := bBlock

   DATA     sl_drawItem
   ACCESS   drawItem                              INLINE ::sl_drawItem
   ASSIGN   drawItem( bBlock )                    INLINE ::sl_drawItem := bBlock

   DATA     sl_measureItem
   ACCESS   measureItem                           INLINE ::sl_measureItem
   ASSIGN   measureItem( bBlock )                 INLINE ::sl_measureItem := bBlock

   DATA     nCurSelected                          INIT 0
   METHOD   getCurItem()                          INLINE ::getItem( ::nCurSelected )

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD WvgListBox:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + WS_OVERLAPPED + WS_TABSTOP
   ::exStyle     := WS_EX_CLIENTEDGE + WS_EX_LEFT + WS_EX_LTRREADING + WS_EX_RIGHTSCROLLBAR
   ::className   := "LISTBOX"
   ::objType     := objTypeListBox

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgListBox:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::horizScroll
      ::style += WS_HSCROLL
   ENDIF
   IF ::vertScroll
      ::style += WS_VSCROLL
   ENDIF
   IF ::multiColumn
      ::style += LBS_MULTICOLUMN
   ENDIF
   ::style += LBS_NOTIFY

   ::oParent:AddChild( Self )

   ::createControl()
#if 0
   ::SetWindowProcCallback()   /* Let parent handle the notifications otherwise remove LBS_NOTIFY bit */
#endif
   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgListBox:handleEvent( nMessage, aNM )

   DO CASE

   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )

   CASE nMessage == HB_GTE_COMMAND
      IF aNM[ 1 ] == LBN_SELCHANGE
         ::nCurSelected := WVG_LBGetCurSel( ::hWnd )+ 1
         IF hb_isBlock( ::sl_itemMarked )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            eval( ::sl_itemMarked, NIL, NIL, self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
            RETURN EVENT_UNHANDELLED
         ENDIF

      ELSEIF aNM[ 1 ] == LBN_DBLCLK
         ::editBuffer := ::nCurSelected
         IF hb_isBlock( ::sl_itemSelected )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            eval( ::sl_itemSelected, NIL, NIL, self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
            RETURN EVENT_UNHANDELLED
         ENDIF
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY

   CASE nMessage == HB_GTE_CTLCOLOR
      IF hb_isNumeric( ::clr_FG )
         WVG_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF hb_isNumeric( ::hBrushBG )
         WVG_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ELSE
         RETURN WVG_GetCurrentBrush( aNM[ 1 ] )
      ENDIF

   CASE nMessage == HB_GTE_ANY
      IF aNM[ 1 ] == WM_LBUTTONUP
         ::nCurSelected := WVG_LBGetCurSel( ::hWnd ) + 1
         IF hb_isBlock( ::sl_itemMarked )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            eval( ::sl_itemMarked, NIL, NIL, self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
         ENDIF

      ELSEIF aNM[ 1 ] == WM_LBUTTONDBLCLK
         ::editBuffer := ::nCurSelected
         IF hb_isBlock( ::sl_itemSelected )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            eval( ::sl_itemSelected, NIL, NIL, self )
            IF ::isParentCrt()
               ::setFocus()
            ENDIF
            RETURN EVENT_HANDELLED
         ENDIF

      ELSEIF aNM[ 1 ] == WM_KEYUP
         IF ::nCurSelected != WVG_LBGetCurSel( ::hWnd ) + 1
            ::nCurSelected := WVG_LBGetCurSel( ::hWnd ) + 1
            IF hb_isBlock( ::sl_itemMarked )
               IF ::isParentCrt()
                  ::oParent:setFocus()
               ENDIF
               eval( ::sl_itemMarked, NIL, NIL, self )
               IF ::isParentCrt()
                  ::setFocus()
               ENDIF
            ENDIF
         ENDIF

      ENDIF
   ENDCASE

   RETURN EVENT_UNHANDELLED

/*----------------------------------------------------------------------*/

METHOD WvgListBox:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgListBox:destroy()
   ::WvgWindow:destroy()
   RETURN NIL

/*----------------------------------------------------------------------*/
/*                         MSDN on ListBox Control                      */
/*----------------------------------------------------------------------*/
#if 0

Functions
=========

DlgDirList
Replaces the contents of a list box with the names of the subdirectories and files in a
specified directory. You can filter the list of names by specifying a set of file attributes.
The list can optionally include mapped drives.

DlgDirSelectEx
Retrieves the current selection from a single-selection list box. It assumes that the
list box has been filled by the DlgDirList function and that the selection is a drive letter,
filename, or directory name.

GetListBoxInfo
Retrieves information about the specified list box.

Messages
========

LB_ADDFILE
Adds the specified filename to a list box that contains a directory listing.

LB_ADDSTRING
Adds a string to a list box. If the list box does not have the LBS_SORT style,
the string is added to the end of the list. Otherwise, the string is inserted into the list
and the list is sorted.

LB_DELETESTRING
Deletes a string in a list box.

LB_DIR
Adds names to the list displayed by a list box. The message adds the names of directories
and files that match a specified string and set of file attributes. LB_DIR can also add
mapped drive letters to the list box.

LB_FINDSTRING
Finds the first string in a list box that begins with the specified string.

LB_FINDSTRINGEXACT
Finds the first list box string that exactly matches the specified string, except that
the search is not case sensitive.

LB_GETANCHORINDEX
Gets the index of the anchor item

LB_GETCARETINDEX
Retrieves the index of the item that has the focus rectangle in a multiple-selection list box.
The item may or may not be selected.

LB_GETCOUNT
Gets the number of items in a list box.

LB_GETCURSEL
Gets the index of the currently selected item, if any, in a single-selection list box.

LB_GETHORIZONTALEXTENT
Gets the width, in pixels, that a list box can be scrolled horizontally (the scrollable width)
if the list box has a horizontal scroll bar.

LB_GETITEMDATA
Gets the application-defined value associated with the specified list box item.

LB_GETITEMHEIGHT
Gets the height of items in a list box.

LB_GETITEMRECT
Gets the dimensions of the rectangle that bounds a list box item as it is currently displayed
in the list box.

LB_GETLISTBOXINFO
Gets the number of items per column in a specified list box.

LB_GETLOCALE
Gets the current locale of the list box. You can use the locale to determine the correct
sorting order of displayed text (for list boxes with the LBS_SORT style) and of text
added by the LB_ADDSTRING message.

LB_GETSEL
Gets the selection state of an item.

LB_GETSELCOUNT
Gets the total number of selected items in a multiple-selection list box.

LB_GETSELITEMS
Fills a buffer with an array of integers that specify the item numbers of selected items in
a multiple-selection list box.

LB_GETTEXT
Gets a string from a list box.

LB_GETTEXTLEN
Gets the length of a string in a list box.

LB_GETTOPINDEX
Gets the index of the first visible item in a list box. Initially the item with index 0
is at the top of the list box, but if the list box contents have been scrolled another item
may be at the top.

LB_INITSTORAGE
Allocates memory for storing list box items. This message is used before an application adds
a large number of items to a list box.

LB_INSERTSTRING
Inserts a string or item data into a list box. Unlike the LB_ADDSTRING message,
the LB_INSERTSTRING message does not cause a list with the LBS_SORT style to be sorted.

LB_ITEMFROMPOINT
Gets the zero-based index of the item nearest the specified point in a list box.

LB_RESETCONTENT
Removes all items from a list box.

LB_SELECTSTRING
Searches a list box for an item that begins with the characters in a specified string.
If a matching item is found, the item is selected.

LB_SELITEMRANGE
Selects or deselects one or more consecutive items in a multiple-selection list box.

LB_SELITEMRANGEEX
Selects one or more consecutive items in a multiple-selection list box.

LB_SETANCHORINDEX
Sets the anchor item

LB_SETCARETINDEX
Sets the focus rectangle to the item at the specified index in a multiple-selection list box.
If the item is not visible, it is scrolled into view.

LB_SETCOLUMNWIDTH
Sets the width, in pixels, of all columns in a multiple-column list box.

LB_SETCOUNT
Sets the count of items in a list box created with the LBS_NODATA style and not created with
the LBS_HASSTRINGS style.

LB_SETCURSEL
Selects a string and scrolls it into view, if necessary. When the new string is selected,
the list box removes the highlight from the previously selected string.

LB_SETHORIZONTALEXTENT
Sets the width, in pixels, by which a list box can be scrolled horizontally (the scrollable width).
If the width of the list box is smaller than this value, the horizontal scroll bar horizontally
scrolls items in the list box. If the width of the list box is equal to or greater than this value,
the horizontal scroll bar is hidden.

LB_SETITEMDATA
Sets a value associated with the specified item in a list box.

LB_SETITEMHEIGHT
Sets the height, in pixels, of items in a list box. If the list box has the
LBS_OWNERDRAWVARIABLE style, this message sets the height of the item specified
by the wParam parameter. Otherwise, this message sets the height of all items in the list box.

LB_SETLOCALE
Sets the current locale of the list box. You can use the locale to determine the
correct sorting order of displayed text (for list boxes with the LBS_SORT style)
and of text added by the LB_ADDSTRING message.

LB_SETSEL
Selects a string in a multiple-selection list box.

LB_SETTABSTOPS
Sets the tab-stop positions in a list box.

LB_SETTOPINDEX
Ensures that the specified item in a list box is visible.



Notifications
=============

LBN_DBLCLK
Notifies the application that the user has double-clicked an item in a list box.
The parent window of the list box receives this notification message through the WM_COMMAND message.

LBN_ERRSPACE
Notifies the application that the list box cannot allocate enough memory to meet a specific request.
The parent window of the list box receives this notification message through the WM_COMMAND message.

LBN_KILLFOCUS
Notifies the application that the list box has lost the keyboard focus. The parent window of the
list box receives this notification message through the WM_COMMAND message.

LBN_SELCANCEL
Notifies the application that the user has canceled the selection in a list box. The parent
window of the list box receives this notification message through the WM_COMMAND message.

LBN_SELCHANGE
Notifies the application that the selection in a list box has changed. The parent window of
the list box receives this notification message through the WM_COMMAND message.

LBN_SETFOCUS
Notifies the application that the list box has received the keyboard focus. The parent window
of the list box receives this notification message through the WM_COMMAND message.

WM_CHARTOITEM
Sent by a list box with the LBS_WANTKEYBOARDINPUT style to its owner in response to a WM_CHAR message.

WM_CTLCOLORLISTBOX
Sent to the parent window of a list box before the system draws the list box.
By responding to this message, the parent window can set the text and background colors
of the list box by using the specified display device context handle.

WM_DELETEITEM
Sent to the owner of a list box or combo box when the list box or combo box is destroyed
or when items are removed by the LB_DELETESTRING, LB_RESETCONTENT, CB_DELETESTRING,
or CB_RESETCONTENT message. The system sends a WM_DELETEITEM message for each deleted item.
The system sends the WM_DELETEITEM message for any deleted list box or combo box item with
nonzero item data.

WM_VKEYTOITEM
Sent by a list box with the LBS_WANTKEYBOARDINPUT style to its owner in response
to a WM_KEYDOWN message.



Structures
==========

DELETEITEMSTRUCT
Describes a deleted list box or combo box item. The lParam parameter of a
WM_DELETEITEM message contains a pointer to this structure. When an item is removed from a
list box or combo box or when a list box or combo box is destroyed, the system sends the
WM_DELETEITEM message to the owner for each deleted item.

Windows NT/2000/XP: The system sends a WM_DELETEITEM message only for items deleted from
an owner-drawn list box (with the LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE style) or
owner-drawn combo box (with the CBS_OWNERDRAWFIXED or CBS_OWNERDRAWVARIABLE style).

Windows 95/98/Me: The system sends the WM_DELETEITEM message for any deleted list box
or combo box item with nonzero item data.



List Box Styles
===============

To create a list box by using the CreateWindow or CreateWindowEx function,
use the LISTBOX class, appropriate window style constants, and the following style constants to
define the list box. After the control has been created, these styles cannot be modified, except as noted.

Constants

LBS_COMBOBOX
Notifies a list box that it is part of a combo box. This allows coordination between the
two controls so that they present a unified user interface (UI). The combo box itself
must set this style. If the style is set by anything but the combo box, the list box
will regard itself incorrectly as a child of a combo box and a failure will result.

LBS_DISABLENOSCROLL
Shows a disabled vertical scroll bar for the list box when the box does not contain
enough items to scroll. If you do not specify this style, the scroll bar is hidden
when the list box does not contain enough items.

LBS_EXTENDEDSEL
Allows multiple items to be selected by using the SHIFT key and the mouse or special key combinations.

LBS_HASSTRINGS
Specifies that a list box contains items consisting of strings. The list box maintains
the memory and addresses for the strings so that the application can use the LB_GETTEXT message
to retrieve the text for a particular item. By default, all list boxes except owner-drawn list
boxes have this style. You can create an owner-drawn list box either with or without this style.

LBS_MULTICOLUMN
Specifies a multi-columnn list box that is scrolled horizontally. The LB_SETCOLUMNWIDTH message
sets the width of the columns.

LBS_MULTIPLESEL
Turns string selection on or off each time the user clicks or double-clicks a string in the list box.
The user can select any number of strings.

LBS_NODATA
Specifies a no-data list box. Specify this style when the count of items in the list box
will exceed one thousand. A no-data list box must also have the LBS_OWNERDRAWFIXED style,
but must not have the LBS_SORT or LBS_HASSTRINGS style.
A no-data list box resembles an owner-drawn list box except that it contains no string or
bitmap data for an item. Commands to add, insert, or delete an item always ignore any
specified item data; requests to find a string within the list box always fail.
The system sends the WM_DRAWITEM message to the owner window when an item must be drawn.
The itemID member of the DRAWITEMSTRUCT structure passed with the WM_DRAWITEM message
specifies the line number of the item to be drawn. A no-data list box does not send a
WM_DELETEITEM message.


LBS_NOINTEGRALHEIGHT
Specifies that the size of the list box is exactly the size specified by the application
when it created the list box. Normally, the system sizes a list box so that the list box
does not display partial items.

LBS_NOREDRAW
Specifies that the list box s appearance is not updated when changes are made.
To change the redraw state of the control, use the WM_SETREDRAW message.


LBS_NOSEL
Specifies that the list box contains items that can be viewed but not selected.

LBS_NOTIFY
Notifies the parent window with an input message whenever the user clicks or double-clicks
a string in the list box.

LBS_OWNERDRAWFIXED
Specifies that the owner of the list box is responsible for drawing its contents and that
the items in the list box are the same height. The owner window receives a
WM_MEASUREITEM message when the list box is created and a WM_DRAWITEM message when
a visual aspect of the list box has changed.

LBS_OWNERDRAWVARIABLE
Specifies that the owner of the list box is responsible for drawing its contents and that
the items in the list box are variable in height. The owner window receives a
WM_MEASUREITEM message for each item in the combo box when the combo box is created and a
WM_DRAWITEM message when a visual aspect of the combo box has changed.

LBS_SORT
Sorts strings in the list box alphabetically.

LBS_STANDARD
Sorts strings in the list box alphabetically. The parent window receives an input
message whenever the user clicks or double-clicks a string. The list box has borders on all sides.

LBS_USETABSTOPS
Enables a list box to recognize and expand tab characters when drawing its strings.
You can use the LB_SETTABSTOPS message to specify tab stop positions. The default tab positions
are 32 dialog template units apart. Dialog template units are the device-independent units used
in dialog box templates. To convert measurements from dialog template units to screen units (pixels),
use the MapDialogRect function.

LBS_WANTKEYBOARDINPUT
Specifies that the owner of the list box receives WM_VKEYTOITEM messages whenever
the user presses a key and the list box has the input focus. This enables an application
to perform special processing on the keyboard input.

#endif
/*----------------------------------------------------------------------*/
