/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ xbpTabPage compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               01Mar2009
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

#ifndef __DBG_PARTS__
#xtranslate hb_traceLog( [<x,...>] ) =>
#endif

/*----------------------------------------------------------------------*/

CLASS WvgTabPage  INHERIT  WvgWindow

   DATA     caption                               INIT NIL /* Character string, Numeric, Object ("")                                                                           */
   DATA     clipChildren                          INIT .T. /* Determines whether Xbase Parts in the child list are clipped during graphic output.                              */
   DATA     minimized                             INIT .T. /* Determines whether the XbpTabPage is minimized after it is created (the page is not visible).                    */
   DATA     postOffset                            INIT 80  /* Determines the distance between the end of the tab and the end of the page as a percentage of the page width.    */
   DATA     preOffset                             INIT 0   /* Determines the distance between the start of the tab and the start of the page as a percentage of the page width.*/
   DATA     tabHeight                             INIT -1  /* Determines the height of the tab.                                                                                */
   DATA     type                                  INIT WVGTABPAGE_TAB_TOP /* Determines the position of the tab.                                                               */

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nMessage, aNM )

   METHOD   Minimize()
   METHOD   Maximize()

   DATA     sl_tabActivate
   METHOD   tabActivate( xParam )                  SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgTabPage

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD
   ::className   := "SysTabControl32"
   ::objType     := objTypeTabPage

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgTabPage

   ::WvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::type == WVGTABPAGE_TAB_BOTTOM
      ::style += TCS_BOTTOM
   ENDIF
   IF ::clipSiblings
      ::style += WS_CLIPSIBLINGS
   ENDIF
   /* ::style += WS_DLGFRAME */

   ::style += TCS_FOCUSNEVER

   ::oParent:AddChild( SELF )

   ::createControl()

   ::SetWindowProcCallback()

   DEFAULT ::caption TO " "

   WAPI_TabCtrl_InsertItem( ::pWnd, 0, ::caption )

   IF ::visible
      ::show()
   ENDIF

   IF ::minimized
      ::hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD handleEvent( nMessage, aNM ) CLASS WvgTabPage
   LOCAL aHdr

   hb_traceLog( "       %s:handleEvent( %i )", __ObjGetClsName( self ), nMessage )

   DO CASE

   CASE nMessage == HB_GTE_SETFOCUS
      IF HB_ISBLOCK( ::sl_tabActivate )

         RETURN EVENT_HANDELLED
      ENDIF

   CASE nMessage == HB_GTE_COMMAND


   CASE nMessage == HB_GTE_RESIZED
      ::sendMessage( WM_SIZE, 0, 0 )
      RETURN EVENT_HANDELLED

   CASE nMessage == HB_GTE_NOTIFY
      aHdr := Wvg_GetNMHdrInfo( aNM[ 2 ] )

      DO CASE
      CASE aHdr[ NMH_code ] == -551 /* TCN_SELCHANGE */

      ENDCASE

   CASE nMessage == HB_GTE_CTLCOLOR
      RETURN WVG_GetStockObject( NULL_BRUSH )

   ENDCASE

   RETURN EVENT_UNHANDELLED

/*----------------------------------------------------------------------*/

METHOD tabActivate( xParam ) CLASS WvgTabPage

   IF HB_ISBLOCK( xParam )
      ::sl_tabActivate := xParam
   ENDIF

   RETURN self

/*----------------------------------------------------------------------*/

METHOD minimize() CLASS WvgTabPage

   ::hide()

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD maximize() CLASS WvgTabPage

   ::show()

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgTabPage

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD destroy() CLASS WvgTabPage

   ::wvgWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/
/*                         MSDN on TabPage Control                      */
/*----------------------------------------------------------------------*/
#if 0

Tab
This section contains information about the programming elements used with tab controls.


Overviews
About Tab Controls
A tab control is analogous to the dividers in a notebook or the labels in a file cabinet. By using a tab control, an application can define multiple pages for the same area of a window or dialog box.

Using Tab Controls
This topic contains two examples that use tab controls.



Macros
TabCtrl_AdjustRect
Calculates a tab control s display area given a window rectangle, or calculates the window rectangle that would correspond to a specified display area. You can use this macro or send the TCM_ADJUSTRECT message explicitly.

TabCtrl_DeleteAllItems
Removes all items from a tab control. You can use this macro or send the TCM_DELETEALLITEMS message explicitly.

TabCtrl_DeleteItem
Removes an item from a tab control. You can use this macro or send the TCM_DELETEITEM message explicitly.

TabCtrl_DeselectAll
Resets items in a tab control, clearing any that were set to the TCIS_BUTTONPRESSED state. You can use this macro or send the TCM_DESELECTALL message explicitly.

TabCtrl_GetCurFocus
Returns the index of the item that has the focus in a tab control. You can use this macro or send the TCM_GETCURFOCUS message explicitly.

TabCtrl_GetCurSel
Determines the currently selected tab in a tab control. You can use this macro or send the TCM_GETCURSEL message explicitly.

TabCtrl_GetExtendedStyle
Retrieves the extended styles that are currently in use for the tab control. You can use this macro or send the TCM_GETEXTENDEDSTYLE message explicitly.

TabCtrl_GetImageList
Retrieves the image list associated with a tab control. You can use this macro or send the TCM_GETIMAGELIST message explicitly.

TabCtrl_GetItem
Retrieves information about a tab in a tab control. You can use this macro or send the TCM_GETITEM message explicitly.

TabCtrl_GetItemCount
Retrieves the number of tabs in the tab control. You can use this macro or send the TCM_GETITEMCOUNT message explicitly.

TabCtrl_GetItemRect
Retrieves the bounding rectangle for a tab in a tab control. You can use this macro or send the TCM_GETITEMRECT message explicitly.

TabCtrl_GetRowCount
Retrieves the current number of rows of tabs in a tab control. You can use this macro or send the TCM_GETROWCOUNT message explicitly.

TabCtrl_GetToolTips
Retrieves the handle to the ToolTip control associated with a tab control. You can use this macro or send the TCM_GETTOOLTIPS message explicitly.

TabCtrl_GetUnicodeFormat
Retrieves the UNICODE character format flag for the control. You can use this macro or send the TCM_GETUNICODEFORMAT message explicitly.

TabCtrl_HighlightItem
Sets the highlight state of a tab item. You can use this macro or send the TCM_HIGHLIGHTITEM message explicitly.

TabCtrl_HitTest
Determines which tab, if any, is at a specified screen position. You can use this macro or send the TCM_HITTEST message explicitly.

TabCtrl_InsertItem
Inserts a new tab in a tab control. You can use this macro or send the TCM_INSERTITEM message explicitly.

TabCtrl_RemoveImage
Removes an image from a tab control s image list. You can use this macro or send the TCM_REMOVEIMAGE message explicitly.

TabCtrl_SetCurFocus
Sets the focus to a specified tab in a tab control. You can use this macro or send the TCM_SETCURFOCUS message explicitly.

TabCtrl_SetCurSel
Selects a tab in a tab control. You can use this macro or send the TCM_SETCURSEL message explicitly.

TabCtrl_SetExtendedStyle
Sets the extended styles that the tab control will use. You can use this macro or send the TCM_SETEXTENDEDSTYLE message explicitly.

TabCtrl_SetImageList
Assigns an image list to a tab control. You can use this macro or send the TCM_SETIMAGELIST message explicitly.

TabCtrl_SetItem
Sets some or all of a tab s attributes. You can use this macro or send the TCM_SETITEM message explicitly.

TabCtrl_SetItemExtra
Sets the number of bytes per tab reserved for application-defined data in a tab control. You can use this macro or send the TCM_SETITEMEXTRA message explicitly.

TabCtrl_SetItemSize
Sets the width and height of tabs in a fixed-width or owner-drawn tab control. You can use this macro or send the TCM_SETITEMSIZE message explicitly.

TabCtrl_SetMinTabWidth
Sets the minimum width of items in a tab control. You can use this macro or send the TCM_SETMINTABWIDTH message explicitly.

TabCtrl_SetPadding
Sets the amount of space (padding) around each tab s icon and label in a tab control. You can use this macro or send the TCM_SETPADDING message explicitly.

TabCtrl_SetToolTips
Assigns a ToolTip control to a tab control. You can use this macro or send the TCM_SETTOOLTIPS message explicitly.

TabCtrl_SetUnicodeFormat
Sets the Unicode character format flag for the control. This message allows you to change the character set used by the control at run time rather than having to re-create the control. You can use this macro or send the TCM_SETUNICODEFORMAT message explicitly.



Messages
TCM_ADJUSTRECT
Calculates a tab control s display area given a window rectangle, or calculates the window rectangle that would correspond to a specified display area. You can send this message explicitly or by using the TabCtrl_AdjustRect macro.

TCM_DELETEALLITEMS
Removes all items from a tab control. You can send this message explicitly or by using the TabCtrl_DeleteAllItems macro.

TCM_DELETEITEM
Removes an item from a tab control. You can send this message explicitly or by using the TabCtrl_DeleteItem macro.

TCM_DESELECTALL
Resets items in a tab control, clearing any that were set to the TCIS_BUTTONPRESSED state. You can send this message explicitly or by using the TabCtrl_DeselectAll macro.

TCM_GETCURFOCUS
Returns the index of the item that has the focus in a tab control. You can send this message explicitly or by using the TabCtrl_GetCurFocus macro.

TCM_GETCURSEL
Determines the currently selected tab in a tab control. You can send this message explicitly or by using the TabCtrl_GetCurSel macro.

TCM_GETEXTENDEDSTYLE
Retrieves the extended styles that are currently in use for the tab control. You can send this message explicitly or by using the TabCtrl_GetExtendedStyle macro.

TCM_GETIMAGELIST
Retrieves the image list associated with a tab control. You can send this message explicitly or by using the TabCtrl_GetImageList macro.

TCM_GETITEM
Retrieves information about a tab in a tab control. You can send this message explicitly or by using the TabCtrl_GetItem macro.

TCM_GETITEMCOUNT
Retrieves the number of tabs in the tab control. You can send this message explicitly or by using the TabCtrl_GetItemCount macro.

TCM_GETITEMRECT
Retrieves the bounding rectangle for a tab in a tab control. You can send this message explicitly or by using the TabCtrl_GetItemRect macro.

TCM_GETROWCOUNT
Retrieves the current number of rows of tabs in a tab control. You can send this message explicitly or by using the TabCtrl_GetRowCount macro.

TCM_GETTOOLTIPS
Retrieves the handle to the ToolTip control associated with a tab control. You can send this message explicitly or by using the TabCtrl_GetToolTips macro.

TCM_GETUNICODEFORMAT
Retrieves the Unicode character format flag for the control. You can send this message explicitly or use the TabCtrl_GetUnicodeFormat macro.

TCM_HIGHLIGHTITEM
Sets the highlight state of a tab item. You can send this message explicitly or by using the TabCtrl_HighlightItem macro.

TCM_HITTEST
Determines which tab, if any, is at a specified screen position. You can send this message explicitly or by using the TabCtrl_HitTest macro.

TCM_INSERTITEM
Inserts a new tab in a tab control. You can send this message explicitly or by using the TabCtrl_InsertItem macro.

TCM_REMOVEIMAGE
Removes an image from a tab control s image list. You can send this message explicitly or by using the TabCtrl_RemoveImage macro.

TCM_SETCURFOCUS
Sets the focus to a specified tab in a tab control. You can send this message explicitly or by using the TabCtrl_SetCurFocus macro.

TCM_SETCURSEL
Selects a tab in a tab control. You can send this message explicitly or by using the TabCtrl_SetCurSel macro.

TCM_SETEXTENDEDSTYLE
Sets the extended styles that the tab control will use. You can send this message explicitly or by using the TabCtrl_SetExtendedStyle macro.

TCM_SETIMAGELIST
Assigns an image list to a tab control. You can send this message explicitly or by using the TabCtrl_SetImageList macro.

TCM_SETITEM
Sets some or all of a tab s attributes. You can send this message explicitly or by using the TabCtrl_SetItem macro.

TCM_SETITEMEXTRA
Sets the number of bytes per tab reserved for application-defined data in a tab control. You can send this message explicitly or by using the TabCtrl_SetItemExtra macro.

TCM_SETITEMSIZE
Sets the width and height of tabs in a fixed-width or owner-drawn tab control. You can send this message explicitly or by using the TabCtrl_SetItemSize macro.

TCM_SETMINTABWIDTH
Sets the minimum width of items in a tab control. You can send this message explicitly or by using the TabCtrl_SetMinTabWidth macro.

TCM_SETPADDING
Sets the amount of space (padding) around each tab s icon and label in a tab control. You can send this message explicitly or by using the TabCtrl_SetPadding macro.

TCM_SETTOOLTIPS
Assigns a ToolTip control to a tab control. You can send this message explicitly or by using the TabCtrl_SetToolTips macro.

TCM_SETUNICODEFORMAT
Sets the Unicode character format flag for the control. This message allows you to change the character set used by the control at run time rather than having to re-create the control. You can send this message explicitly or use the TabCtrl_SetUnicodeFormat macro.



Notifications
NM_CLICK (tab)
Notifies the parent window of a tab control that the user has clicked the left mouse button within the control. NM_CLICK (tab) is sent in the form of a WM_NOTIFY message.

NM_DBLCLK (tab)
Notifies a parent window of a tab control that the user has double-clicked the left mouse button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_RCLICK (tab)
Notifies the parent window of a tab control that the user has clicked the right mouse button within the control. NM_RCLICK (tab) is sent in the form of a WM_NOTIFY message.

NM_RDBLCLK (tab)
Notifies the parent window of a tab control that the user has double-clicked the right mouse button within the control. NM_RDBLCLK (tab) is sent in the form of a WM_NOTIFY message.

NM_RELEASEDCAPTURE (tab)
Notifies a tab control s parent window that the control is releasing mouse capture. This notification is sent in the form of a WM_NOTIFY message.

TCN_FOCUSCHANGE
Notifies a tab control s parent window that the button focus has changed.

TCN_GETOBJECT
Sent by a tab control when it has the TCS_EX_REGISTERDROP extended style and an object is dragged over a tab item in the control. This notification message is sent in the form of a WM_NOTIFY message.

TCN_KEYDOWN
Notifies a tab control s parent window that a key has been pressed. This message is sent in the form of a WM_NOTIFY message.

TCN_SELCHANGE
Notifies a tab control s parent window that the currently selected tab has changed. This message is sent in the form of a WM_NOTIFY message.

TCN_SELCHANGING
Notifies a tab control s parent window that the currently selected tab is about to change. This message is sent in the form of a WM_NOTIFY message.



Structures
NMTCKEYDOWN
Contains information about a key press in a tab control. It is used with the TCN_KEYDOWN notification message. This structure supersedes the TC_KEYDOWN structure.

TCHITTESTINFO
Contains information about a hit test. This structure supersedes the TC_HITTESTINFO structure.

TCITEM
Specifies or receives the attributes of a tab item. It is used with the TCM_INSERTITEM, TCM_GETITEM, and TCM_SETITEM messages. This structure supersedes the TC_ITEM structure.

TCITEMHEADER
Specifies or receives the attributes of a tab. It is used with the TCM_INSERTITEM, TCM_GETITEM, and TCM_SETITEM messages. This structure supersedes the TC_ITEMHEADER structure.



Constants
Tab Control Extended Styles
The tab control now supports extended styles. These styles are manipulated using the TCM_GETEXTENDEDSTYLE and TCM_SETEXTENDEDSTYLE messages and should not be confused with extended window styles that are passed to CreateWindowEx.

Tab Control Item States
Tab control items now support an item state to support the TCM_DESELECTALL message. Additionally, the TCITEM structure supports item state values.

Tab Control Styles
This section lists supported tab control styles.

TCS_BOTTOM
Version 4.70. Tabs appear at the bottom of the control. This value equals TCS_RIGHT. This style is not supported if you use ComCtl32.dll version 6.
TCS_BUTTONS
Tabs appear as buttons, and no border is drawn around the display area.
TCS_FIXEDWIDTH
All tabs are the same width. This style cannot be combined with the TCS_RIGHTJUSTIFY style.
TCS_FLATBUTTONS
Version 4.71. Selected tabs appear as being indented into the background while other tabs appear as being on the same plane as the background. This style only affects tab controls with the TCS_BUTTONS style.
TCS_FOCUSNEVER
The tab control does not receive the input focus when clicked.
TCS_FOCUSONBUTTONDOWN
The tab control receives the input focus when clicked.
TCS_FORCEICONLEFT
Icons are aligned with the left edge of each fixed-width tab. This style can only be used with the TCS_FIXEDWIDTH style.
TCS_FORCELABELLEFT
Labels are aligned with the left edge of each fixed-width tab; that is, the label is displayed immediately to the right of the icon instead of being centered. This style can only be used with the TCS_FIXEDWIDTH style, and it implies the TCS_FORCEICONLEFT style.
TCS_HOTTRACK
Version 4.70. Items under the pointer are automatically highlighted. You can check whether or not hot tracking is enabled by calling SystemParametersInfo.
TCS_MULTILINE
Multiple rows of tabs are displayed, if necessary, so all tabs are visible at once.
TCS_MULTISELECT
Version 4.70. Multiple tabs can be selected by holding down the CTRL key when clicking. This style must be used with the TCS_BUTTONS style.
TCS_OWNERDRAWFIXED
The parent window is responsible for drawing tabs.
TCS_RAGGEDRIGHT
Rows of tabs will not be stretched to fill the entire width of the control. This style is the default.
TCS_RIGHT
Version 4.70. Tabs appear vertically on the right side of controls that use the TCS_VERTICAL style. This value equals TCS_BOTTOM. This style is not supported if you use visual styles.
TCS_RIGHTJUSTIFY
The width of each tab is increased, if necessary, so that each row of tabs fills the entire width of the tab control. This window style is ignored unless the TCS_MULTILINE style is also specified.
TCS_SCROLLOPPOSITE
Version 4.70. Unneeded tabs scroll to the opposite side of the control when a tab is selected.
TCS_SINGLELINE
Only one row of tabs is displayed. The user can scroll to see more tabs, if necessary. This style is the default.
TCS_TABS
Tabs appear as tabs, and a border is drawn around the display area. This style is the default.
TCS_TOOLTIPS
The tab control has a ToolTip control associated with it.
TCS_VERTICAL
Version 4.70. Tabs appear at the left side of the control, with tab text displayed vertically. This style is valid only when used with the TCS_MULTILINE style. To make tabs appear on the right side of the control, also use the TCS_RIGHT style. This style is not supported if you use ComCtl32.dll version 6.
Remarks

The following styles can be modified after the control is created.

TCS_BOTTOM
TCS_BUTTONS
TCS_FIXEDWIDTH
TCS_FLATBUTTONS
TCS_FORCEICONLEFT
TCS_FORCELABELLEFT
TCS_MULTILINE
TCS_OWNERDRAWFIXED
TCS_RAGGEDRIGHT
TCS_RIGHT
TCS_VERTICAL

#endif
