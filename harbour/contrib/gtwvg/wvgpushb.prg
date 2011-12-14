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
 *                 Xbase++ xbpPushButton Compatible Class
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

CLASS WvgPushButton  INHERIT  WvgWindow

   DATA     autosize                              INIT .F.
   DATA     border                                INIT .T.
   DATA     caption                               INIT NIL
   DATA     pointerFocus                          INIT .T.
   DATA     preSelect                             INIT .F.
   DATA     drawMode                              INIT WVG_DRAW_NORMAL
   DATA     default                               INIT .F.
   DATA     cancel                                INIT .F.

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nMessage, aNM )

   METHOD   setCaption( xCaption, cDll )
   METHOD   activate( xParam )                    SETGET
   METHOD   draw( xParam )                        SETGET

   METHOD   setColorFG()                          INLINE NIL
   METHOD   setColorBG()                          INLINE NIL

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + BS_PUSHBUTTON  + BS_NOTIFY /* + BS_PUSHLIKE */
   ::className   := "BUTTON"
   ::objType     := objTypePushButton

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isNumeric( ::caption )
      ::style += BS_BITMAP
   ELSEIF hb_isChar( ::caption )
      IF ".ICO" == upper( right( ::caption, 4 ) )
         ::style += BS_ICON
      ELSEIF ".BMP" == upper( right( ::caption, 4 ) )
         ::style += BS_BITMAP
      ENDIF
   ENDIF
   IF ! ::border
      ::style += BS_FLAT
   ENDIF

   ::oParent:AddChild( Self )

   ::createControl()
#if 0
   ::SetWindowProcCallback()  /* Let parent take control of it */
#endif

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   ::setCaption( ::caption )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:handleEvent( nMessage, aNM )

   DO CASE
   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )
      IF hb_isBlock( ::sl_resize )
         eval( ::sl_resize, NIL, NIL, self )
      ENDIF

   CASE nMessage == HB_GTE_COMMAND
      IF aNM[ 1 ] == BN_CLICKED
         IF hb_isBlock( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            eval( ::sl_lbClick, NIL, NIL, self )
            IF ::pointerFocus
               ::setFocus()
            ENDIF
         ENDIF
         RETURN EVENT_HANDELLED
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY
      // Will never be issued because pushbutton sends WM_COMMAND

   CASE nMessage == HB_GTE_CTLCOLOR
      IF hb_isNumeric( ::clr_FG )
         WVG_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF hb_isNumeric( ::hBrushBG )
         WVG_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ENDIF

#if 0  /* Must not reach here if WndProc is not installed */
   CASE nMessage == HB_GTE_ANY
      IF aNM[ 1 ] == WM_LBUTTONUP
         IF hb_isBlock( ::sl_lbClick )
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            eval( ::sl_lbClick, NIL, NIL, Self )
         ENDIF
      ENDIF
#endif
   ENDCASE

   RETURN EVENT_UNHANDELLED

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:destroy()
   ::wvgWindow:destroy()
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:setCaption( xCaption, cDll )

   DEFAULT xCaption TO ::caption
   HB_SYMBOL_UNUSED( cDll )

   IF hb_isChar( xCaption )
      ::caption := xCaption
      IF ".ICO" == upper( right( ::caption, 4 ) )
         WVG_SendMessage( ::hWnd, BM_SETIMAGE, IMAGE_ICON, WVG_LoadImage( ::caption, 2, IMAGE_ICON ) )
      ELSEIF ".BMP" == upper( right( ::caption, 4 ) )
         WVG_SendMessage( ::hWnd, BM_SETIMAGE, IMAGE_BITMAP, WVG_LoadImage( ::caption, 2, IMAGE_BITMAP ) )
      ELSE
         WVG_SendMessageText( ::hWnd, WM_SETTEXT, 0, ::caption )
      ENDIF

   ELSEIF hb_isNumeric( xCaption )  /* Handle to the bitmap */
      ::caption := xCaption
      WVG_SendMessage( ::hWnd, BM_SETIMAGE, IMAGE_BITMAP, ::caption )

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:activate( xParam )

   IF hb_isBlock( xParam ) .OR. ( xParam == NIL )
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgPushButton:draw( xParam )

   IF hb_isBlock( xParam ) .or. ( xParam == NIL )
      ::sl_paint := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*                         MSDN on Button Control                       */
/*----------------------------------------------------------------------*/
#if 0

Functions
=========

CheckDlgButton
Changes the check state of a button control.

CheckRadioButton
Adds a check mark to (checks) a specified radio button in a group and removes a check mark from
(clears) all other radio buttons in the group.

IsDlgButtonChecked
The IsDlgButtonChecked function determines whether a button control is checked or whether a
three-state button control is checked, unchecked, or indeterminate.


Macros
======

Button_Enable
Enables or disables a button.

Button_GetCheck
Gets the check state of a radio button or check box. You can use this macro or send the BM_GETCHECK
message explicitly.

Button_GetIdealSize
Gets the size of the button that best fits the text and image, if an image list is present.
You can use this macro or send the BCM_GETIDEALSIZE message explicitly.

Button_GetImageList
Gets the BUTTON_IMAGELIST structure that describes the image list that is set for a button control.
You can use this macro or send the BCM_GETIMAGELIST message explicitly.

Button_GetNote
Gets the text of the note associated with a command link button. You can use this macro or send
the BCM_GETNOTE message explicitly.

Button_GetNoteLength
Gets the length of the note text that may be displayed in the description for a command link.
Use this macro or send the BCM_GETNOTELENGTH message explicitly.

Button_GetSplitInfo
Gets information for a specified split button control. Use this macro or send the
BCM_GETSPLITINFO message explicitly.

Button_GetState
Gets the check state of a radio button or check box. You can use this macro or send the
BM_GETSTATE message explicitly.

Button_GetText
Gets the text of a button.

Button_GetTextLength
Gets the number of characters in the text of a button.

Button_GetTextMargin
Gets the margins used to draw text in a button control. You can use this macro or send the
BCM_GETTEXTMARGIN message explicitly.

Button_SetCheck
Sets the check state of a radio button or check box. You can use this macro or send the
BM_SETCHECK message explicitly.

Button_SetDropDownState
Sets the drop down state for a specified button with style of BS_SPLITBUTTON.
Use this macro or send the BCM_SETDROPDOWNSTATE message explicitly.

Button_SetElevationRequiredState
Sets the elevation required state for a specified button or command link to display an elevated icon.
Use this macro or send the BCM_SETSHIELD message explicitly.

Button_SetImageList
Assigns an image list to a button control. You can use this macro or send the
BCM_SETIMAGELIST message explicitly.

Button_SetNote
Sets the text of the note associated with a specified command link button.
You can use this macro or send the BCM_SETNOTE message explicitly.

Button_SetSplitInfo
Sets information for a specified split button control. Use this macro or send the
BCM_SETSPLITINFO message explicitly.

Button_SetState
Sets the highlight state of a button. The highlight state indicates whether the button
is highlighted as if the user had pushed it. You can use this macro or send the
BM_SETSTATE message explicitly.

Button_SetStyle
Sets the style of a button. You can use this macro or send the BM_SETSTYLE message explicitly.

Button_SetText
Sets the text of a button.

Button_SetTextMargin
Sets the margins for drawing text in a button control. You can use this macro or send the
BCM_SETTEXTMARGIN message explicitly.


Messages
========

BCM_GETIDEALSIZE
Gets the size of the button that best fits its text and image, if an image list is present.
You can send this message explicitly or use the Button_GetIdealSize macro.

BCM_GETIMAGELIST
Gets the BUTTON_IMAGELIST structure that describes the image list assigned to a button control.
You can send this message explicitly or use the Button_GetImageList macro.

BCM_GETNOTE
Gets the text of the note associated with a command link button. You can send this message
explicitly or use the Button_GetNote macro.

BCM_GETNOTELENGTH
Gets the length of the note text that may be displayed in the description for a command link button.
Send this message explicitly or by using the Button_GetNoteLength macro.

BCM_GETSPLITINFO
Gets information for a split button control. Send this message explicitly or by using the
Button_GetSplitInfo macro.

BCM_GETTEXTMARGIN
Gets the margins used to draw text in a button control. You can send this message explicitly or
use the Button_GetTextMargin macro.

BCM_SETDROPDOWNSTATE
Sets the drop down state for a button with style TBSTYLE_DROPDOWN. Send this message explicitly
or by using the Button_SetDropDownState macro.

BCM_SETIMAGELIST
Assigns an image list to a button control. You can send this message explicitly or use the
Button_SetImageList macro.

BCM_SETNOTE
Sets the text of the note associated with a command link button. You can send this message
explicitly or use the Button_SetNote macro.

BCM_SETSHIELD
Sets the elevation required state for a specified button or command link to display an elevated icon.
Send this message explicitly or by using the Button_SetElevationRequiredState macro.

BCM_SETSPLITINFO
Sets information for a split button control. Send this message explicitly or by using the
Button_SetSplitInfo macro.

BCM_SETTEXTMARGIN
The BCM_SETTEXTMARGIN message sets the margins for drawing text in a button control.

BM_CLICK
Simulates the user clicking a button. This message causes the button to receive the
WM_LBUTTONDOWN and WM_LBUTTONUP messages, and the button s parent window to receive a
BN_CLICKED notification message.

BM_GETCHECK
Gets the check state of a radio button or check box. You can send this message explicitly or
use the Button_GetCheck macro.

BM_GETIMAGE
Retrieves a handle to the image (icon or bitmap) associated with the button.

BM_GETSTATE
Retrieves the state of a button or check box. You can send this message explicitly or use the
Button_GetState macro.

BM_SETCHECK
Sets the check state of a radio button or check box. You can send this message explicitly or by
using the Button_SetCheck macro.

BM_SETDONTCLICK
Sets a flag on a radio button that controls the generation of BN_CLICKED messages when the
button receives focus.

BM_SETIMAGE
Associates a new image (icon or bitmap) with the button.

BM_SETSTATE
Sets the highlight state of a button. The highlight state indicates whether the button is
highlighted as if the user had pushed it. You can send this message explicitly or use the
Button_SetState macro.

BM_SETSTYLE
Sets the style of a button. You can send this message explicitly or use the Button_SetStyle macro.


Notifications
=============

BCN_DROPDOWN
Sent when the user clicks a drop down arrow on a button. The parent window of the control receives
this notification message in the form of a WM_NOTIFY message.

BCN_HOTITEMCHANGE
Notifies the button control owner that the mouse is entering or leaving the client area of the
button control. The button control sends this notification in the form of a WM_NOTIFY message.

BN_CLICKED
Sent when the user clicks a button.
The parent window of the button receives the BN_CLICKED notification code through the WM_COMMAND message.

BN_DBLCLK
Sent when the user double-clicks a button. This notification is sent automatically for
BS_USERBUTTON, BS_RADIOBUTTON, and BS_OWNERDRAW buttons. Other button types send
BN_DBLCLK only if they have the BS_NOTIFY style.
The parent window of the button receives the BN_DBLCLK notification code through the WM_COMMAND message.

BN_DISABLE
Sent when a button is disabled.
Note  This notification message is provided only for compatibility with 16-bit versions of
Microsoft Windows earlier than version 3.0. Applications should use the BS_OWNERDRAW button style
and the DRAWITEMSTRUCT structure for this task.
The parent window of the button receives the BN_DISABLE notification code through the WM_COMMAND message.

BN_DOUBLECLICKED
Sent when the user double-clicks a button. This notification is sent automatically for
BS_USERBUTTON, BS_RADIOBUTTON, and BS_OWNERDRAW buttons. Other button types send
BN_DOUBLECLICKED only if they have the BS_NOTIFY style.
The parent window of the button receives the BN_DOUBLECLICKED notification code through the
WM_COMMAND message.

BN_HILITE
Sent when the user selects a button.
Note  This notification message is provided only for compatibility with 16-bit versions
of Windows earlier than version 3.0. Applications should use the BS_OWNERDRAW button style
and the DRAWITEMSTRUCT structure for this task.
The parent window of the button receives the BN_HILITE notification code through the WM_COMMAND message.

BN_KILLFOCUS
Sent when a button loses the keyboard focus. The button must have the
BS_NOTIFY style to send this notification message.

The parent window of the button receives the BN_KILLFOCUS notification code through the
WM_COMMAND message.

BN_PAINT
Sent when a button should be painted.
Note   This notification message is provided only for compatibility with 16-bit versions of
Windows earlier than version 3.0. Applications should use the BS_OWNERDRAW button style and
the DRAWITEMSTRUCT structure for this task.
The parent window of the button receives the BN_PAINT notification code through the WM_COMMAND message.

BN_PUSHED
Sent when the push state of a button is set to pushed.
Note  This notification message is provided only for compatibility with 16-bit versions
of Windows earlier than version 3.0. Applications should use the BS_OWNERDRAW button
style and the DRAWITEMSTRUCT structure for this task.
The parent window of the button receives the BN_PUSHED notification code through the WM_COMMAND message.

BN_SETFOCUS
Sent when a button receives the keyboard focus. The button must have the BS_NOTIFY style to
send this notification message.
The parent window of the button receives the BN_SETFOCUS notification code through the WM_COMMAND message.

BN_UNHILITE
Sent when the highlight should be removed from a button.
Note  This notification message is provided only for compatibility with 16-bit versions of
Windows earlier than version 3.0. Applications should use the BS_OWNERDRAW button style and
the DRAWITEMSTRUCT structure for this task.
The parent window of the button receives the BN_UNHILITE notification code through the WM_COMMAND message.

BN_UNPUSHED
Sent when the push state of a button is set to unpushed.
Note  This notification message is provided only for compatibility with 16-bit versions
of Windows earlier than version 3.0. Applications should use the BS_OWNERDRAW button style
and the DRAWITEMSTRUCT structure for this task.
The parent window of the button receives the BN_UNPUSHED notification code through the WM_COMMAND message.

NM_CUSTOMDRAW (button)
Notifies the parent window of a button control about custom draw operations on the button.
The button control sends this notification in the form of a WM_NOTIFY message.

WM_CTLCOLORBTN
The WM_CTLCOLORBTN message is sent to the parent window of a button before drawing the button.
The parent window can change the button s text and background colors. However, only owner-drawn
buttons respond to the parent window processing this message.


Structures
==========

BUTTON_IMAGELIST
Contains information about an image list that is used with a button control.

BUTTON_SPLITINFO
Contains information that defines a split button (BS_SPLITBUTTON and BS_DEFSPLITBUTTON styles).
Used with the BCM_GETSPLITINFO and BCM_SETSPLITINFO messages.

NMBCDROPDOWN
Contains information about a BCN_DROPDOWN notification.

NMBCHOTITEM
Contains information about the movement of the mouse over a button control.


Constants
=========

Button Styles
Specifies a combination of button styles. If you create a button using the BUTTON class
with the CreateWindow or CreateWindowEx function, you can specify any of the button styles listed below.

#endif
/*----------------------------------------------------------------------*/
