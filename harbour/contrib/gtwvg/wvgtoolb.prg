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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                  Xbase++ xbpToolBar Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              23Nov2008
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

CLASS WvgToolBar  INHERIT  WvgWindow /*WvgActiveXControl*/

   DATA     appearance
   DATA     style                                 INIT WVGTOOLBAR_STYLE_STANDARD
   DATA     allowCustomize                        INIT .T.
   DATA     enabled                               INIT .T.
   DATA     showToolTips                          INIT .T.
   DATA     borderStyle                           INIT WVGFRAME_NONE
   DATA     wrappable                             INIT .T.
   DATA     buttonWidth                           INIT 0
   DATA     buttonHeight                          INIT 0
   DATA     textAlign                             INIT WVGALIGN_BOTTOM
   DATA     imageWidth                            INIT 0
   DATA     imageHeight                           INIT 0
   DATA     transparentColor                      INIT 0

   DATA     aItems                                INIT {}
   DATA     hImageList
   DATA     lSized                                INIT .F.

   DATA     sl_change
   DATA     sl_buttonMenuClick
   DATA     sl_buttonDropDown

   METHOD   numItems()                            INLINE Len( ::aItems )

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   handleEvent( nMessage, aNM )
   METHOD   destroy()
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   sendToolbarMessage( nMsg, p1, p2 )
   METHOD   addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, cKey, nMapRGB )
   METHOD   delItem()

   METHOD   getItem()
   METHOD   clear()
   METHOD   customize()
   METHOD   loadImageSet()
   METHOD   saveToolbar()
   METHOD   restToolbar()
   METHOD   setPosAndSize()
   METHOD   setSize()

   METHOD   buttonClick( xParam )                 SETGET
   METHOD   change( xParam )                      SETGET
   METHOD   buttonMenuClick( xParam )             SETGET
   METHOD   buttonDropDown( xParam )              SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   #if 0
   + TBSTYLE_LIST   caption to the right, otherwise caption to the bottom
   ::style       := WS_CHILD + TBSTYLE_FLAT + CCS_ADJUSTABLE + CCS_NODIVIDER + CCS_VERT
   #endif

   ::exStyle     := TBSTYLE_EX_DOUBLEBUFFER + TBSTYLE_EX_MIXEDBUTTONS
   ::className   := TOOLBARCLASSNAME
   ::objType     := objTypeToolBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::style == WVGTOOLBAR_STYLE_FLAT
      ::style := TBSTYLE_FLAT
   ELSEIF ::style == WVGTOOLBAR_STYLE_VERTICAL
      ::style := CCS_VERT
   ELSE
      ::style := 0
   ENDIF
   ::style += WS_CHILD

   IF ::wrappable
      ::style += TBSTYLE_WRAPABLE
   ENDIF
   IF ::showToolTips
      ::style += TBSTYLE_TOOLTIPS
   ENDIF
   IF ::borderStyle == WVGFRAME_RECT
      ::style += WS_BORDER
   ENDIF
   #if 0
   IF ::appearance == WVG_APPEARANCE_3D
   ENDIF
   #endif

   ::oParent:AddChild( SELF )

   ::createControl()

   #if 0
   /* Should not be defined as we only require its notifications
    * so the parent of toolbar will process them anyway
    * All other functionality should be default until ownerdraw is introduced.
    */
      ::SetWindowProcCallback()
   #endif

   IF !empty( ::hWnd )
      ::SendToolbarMessage( TB_BUTTONSTRUCTSIZE )
      ::hImageList := WAPI_ImageList_Create( ::imageWidth, ::imageHeight, ILC_COLOR32+ILC_MASK, 0, 1 )
      ::SendToolbarMessage( TB_SETIMAGELIST, ::hImageList )

      ::SendToolbarMessage( TB_BUTTONSTRUCTSIZE )
      /* ::SendToolbarMessage( TB_SETINDENT, 10 ) */
   ENDIF

   IF ::visible
      ::show()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:handleEvent( nMessage, aNM )
   LOCAL nObj, aNMMouse

   SWITCH nMessage

   CASE HB_GTE_RESIZED
      IF ::isParentCrt()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )
      RETURN EVENT_HANDELLED

   CASE HB_GTE_COMMAND
      EXIT

   CASE HB_GTE_NOTIFY
      aNMMouse := Wvg_GetNMMouseInfo( aNM[ 2 ] )

      DO CASE

      CASE aNMMouse[ NMH_code ] == NM_CLICK
         IF ( nObj := ascan( ::aItems, {|e_| e_[ 1 ] == aNMMouse[ NMH_dwItemSpec ] } ) ) > 0
            IF hb_isBlock( ::sl_lbClick )
               IF ::isParentCrt()
                  ::oParent:setFocus()
               ENDIF
               Eval( ::sl_lbClick, ::aItems[ nObj,2 ], NIL, Self )

            ENDIF
         ENDIF
         RETURN EVENT_HANDELLED

      OTHERWISE
         RETURN EVENT_UNHANDELLED

      ENDCASE

      EXIT
   END

   RETURN EVENT_UNHANDELLED

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:destroy()
   LOCAL i, nItems

   IF ( nItems := Len( ::aItems ) ) > 0
      FOR i := 1 TO nItems
         IF ::aItems[ i,2 ]:image <> NIL
            WVG_DeleteObject( ::aItems[ i,2 ]:image )
         ENDIF
         IF ::aItems[ i,2 ]:disabledImage <> NIL
            WVG_DeleteObject( ::aItems[ i,2 ]:disabledImage )
         ENDIF
         IF ::aItems[ i,2 ]:hotImage <> NIL
            WVG_DeleteObject( ::aItems[ i,2 ]:hotImage )
         ENDIF
      NEXT
   ENDIF

   IF !empty( ::hImageList )
      WAPI_ImageList_Destroy( ::hImageList )
   ENDIF

   ::wvgWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:sendToolbarMessage( nMsg, p1, p2 )

   RETURN WVG_SendToolbarMessage( ::pWnd, nMsg, p1, p2 )

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:addItem( cCaption, xImage, xDisabledImage, xHotImage, cDLL, nStyle, cKey, nMapRGB )
   LOCAL oBtn, pBitmap, cType, nBtn

   HB_SYMBOL_UNUSED( xDisabledImage )
   HB_SYMBOL_UNUSED( xHotImage )
   HB_SYMBOL_UNUSED( cDLL )

   /* Issue this at the begining of first item */
   IF ! ::lSized
      #if 0
      ::SendToolbarMessage( TB_SETBUTTONWIDTH, ::buttonWidth, ::buttonWidth )
      #endif
      ::lSized := .t.
   ENDIF

   oBtn := WvgToolbarButton():new( cCaption, nStyle, cKey )

   oBtn:index   := ::numItems + 1
   oBtn:command := 100 + oBtn:index

   cType   := valtype( xImage )

   DO CASE

   CASE cType == "C"
      IF ( "." $ xImage ) .or. ( "/" $ xImage ) .or. ( "\" $ xImage ) .or. ( ":" $ xImage ) .or. file( xImage )
         pBitmap := Wvg_PrepareBitmapFromFile( xImage, ::imageWidth, ::imageHeight, .t., ::hWnd )
      ELSE
         pBitmap := Wvg_PrepareBitmapFromResourceName( xImage, ::imageWidth, ::imageHeight, .t., ::hWnd )
      ENDIF

   CASE cType == "N"
      pBitmap := Wvg_PrepareBitmapFromResourceID( xImage, ::imageWidth, ::imageHeight, .t., ::hWnd )

   ENDCASE

   IF ! empty( pBitmap )
      /* oBtn:image := pBitmap */

      IF hb_isNumeric( nMapRGB )
         nBtn := WAPI_ImageList_AddMasked( ::hImageList, pBitmap, nMapRGB )
      ELSE
         nBtn := WAPI_ImageList_Add( ::hImageList, pBitmap )
      ENDIF
      WVG_DeleteObject( pBitmap )

      WVG_AddToolbarButton( ::pWnd, nBtn, oBtn:caption, oBtn:command, 1, ::showToolTips )

      /* Set Button Size */
      ::SendToolbarMessage( TB_SETBUTTONSIZE, ::buttonWidth, ::buttonHeight )

      #if 0
      SendMessage( hWndTB, TB_SETPADDING, ( WPARAM ) 0, ( LPARAM ) MAKELPARAM(  10,10 ) );
      ::sendToolbarMessage( TB_SETPADDING, 10, 10 )
      #endif
      ::sendToolbarMessage( TB_AUTOSIZE )
   ELSE
      Wvg_AddToolbarButton( ::pWnd, , , oBtn:command, 3, .f. )

   ENDIF

   aadd( ::aItems, { oBtn:command, oBtn } )

   RETURN oBtn

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:delItem()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:getItem()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:clear()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:customize()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:loadImageSet()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:saveToolbar()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:restToolbar()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:setPosAndSize()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:setSize()

   ::sendMessage( TB_AUTOSIZE, 0, 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:buttonClick( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:change( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_change := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:buttonMenuClick( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonMenuClick := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgToolBar:buttonDropDown( xParam )

   IF hb_isBlock( xParam ) .or. hb_isNil( xParam )
      ::sl_buttonDropDown := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *       WvgToolbarButton() Class compatible with XbpToolbarButton()
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvgToolBarButton

   DATA     enabled                               INIT .T.
   DATA     index                                 INIT 0
   DATA     key                                   INIT ""
   DATA     style                                 INIT WVGTOOLBAR_BUTTON_DEFAULT
   DATA     caption                               INIT ""
   DATA     image                                 INIT NIL
   DATA     disabledImage                         INIT NIL
   DATA     hotImage                              INIT NIL
   DATA     mixedState                            INIT .F.
   DATA     pressed                               INIT .F.
   DATA     visible                               INIT .T.
   DATA     left                                  INIT 0
   DATA     bottom                                INIT 0
   DATA     top                                   INIT 0
   DATA     width                                 INIT 0
   DATA     height                                INIT 0
   DATA     description                           INIT ""
   DATA     tooltipText                           INIT ""
   DATA     command                               INIT 0

   METHOD   new( cCaption, nStyle, cKey )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgToolBarButton:new( cCaption, nStyle, cKey )

   DEFAULT cCaption       TO ::caption
   DEFAULT nStyle         TO ::style
   DEFAULT cKey           TO ::key

   ::caption        := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self

/*----------------------------------------------------------------------*/
/*                         MSDN on ToolBar Control                      */
/*----------------------------------------------------------------------*/
#if 0

MESSAGES
========

TB_ADDBITMAP
Adds one or more images to the list of button images available for a toolbar.

TB_ADDBUTTONS
Adds one or more buttons to a toolbar.

TB_ADDSTRING
Adds a new string to the toolbars string pool.

TB_AUTOSIZE
Causes a toolbar to be resized.

TB_BUTTONCOUNT
Retrieves a count of the buttons currently in the toolbar.

TB_BUTTONSTRUCTSIZE
Specifies the size of the TBBUTTON structure.

TB_CHANGEBITMAP
Changes the bitmap for a button in a toolbar.

TB_CHECKBUTTON
Checks or unchecks a given button in a toolbar.

TB_COMMANDTOINDEX
Retrieves the zero-based index for the button associated with the specified command identifier.

TB_CUSTOMIZE
Displays the Customize Toolbar dialog box.

TB_DELETEBUTTON
Deletes a button from the toolbar.

TB_ENABLEBUTTON
Enables or disables the specified button in a toolbar.

TB_GETANCHORHIGHLIGHT
Retrieves the anchor highlight setting for a toolbar.

TB_GETBITMAP
Retrieves the index of the bitmap associated with a button in a toolbar.

TB_GETBITMAPFLAGS
Retrieves the flags that describe the type of bitmap to be used.

TB_GETBUTTON
Retrieves information about the specified button in a toolbar.

TB_GETBUTTONINFO
Retrieves extended information for a button in a toolbar.

TB_GETBUTTONSIZE
Retrieves the current width and height of toolbar buttons, in pixels.

TB_GETBUTTONTEXT
Retrieves the display text of a button on a toolbar.

TB_GETCOLORSCHEME
Retrieves the color scheme information from the toolbar control.

TB_GETDISABLEDIMAGELIST
Retrieves the image list that a toolbar control uses to display inactive buttons.

TB_GETEXTENDEDSTYLE
Retrieves the extended styles for a toolbar control.

TB_GETHOTIMAGELIST
Retrieves the image list that a toolbar control uses to display hot buttons.

TB_GETHOTITEM
Retrieves the index of the hot item in a toolbar.

TB_GETIDEALSIZE
Gets the ideal size of the toolbar.

TB_GETIMAGELIST
Retrieves the image list that a toolbar control uses to display buttons in their default state.
A toolbar control uses this image list to display buttons when they are not hot or disabled.

TB_GETIMAGELISTCOUNT
Gets the number of image lists associated with the toolbar.

TB_GETINSERTMARK
Retrieves the current insertion mark for the toolbar.

TB_GETINSERTMARKCOLOR
Retrieves the color used to draw the insertion mark for the toolbar.

TB_GETITEMDROPDOWNRECT

Gets the bounding rectangle of the dropdown window for a toolbar item with style BTNS_DROPDOWN.

TB_GETITEMRECT
Retrieves the bounding rectangle of a button in a toolbar.

TB_GETMAXSIZE
Retrieves the total size of all of the visible buttons and separators in the toolbar.

TB_GETMETRICS
Retrieves the metrics of a toolbar control.

TB_GETOBJECT
Retrieves the IDropTarget for a toolbar control.

TB_GETPADDING
Retrieves the padding for a toolbar control.

TB_GETPRESSEDIMAGELIST
Gets the image list that a toolbar control uses to display buttons in a pressed state.

TB_GETRECT
Retrieves the bounding rectangle for a specified toolbar button.

TB_GETROWS
Retrieves the number of rows of buttons in a toolbar with the TBSTYLE_WRAPABLE style.

TB_GETSTATE
Retrieves information about the state of the specified button in a toolbar, such as whether it
is enabled, pressed, or checked.

TB_GETSTRING
Retrieves a string from a toolbars string pool.

TB_GETSTYLE
Retrieves the styles currently in use for a toolbar control.

TB_GETTEXTROWS
Retrieves the maximum number of text rows that can be displayed on a toolbar button.

TB_GETTOOLTIPS
Retrieves the handle to the ToolTip control, if any, associated with the toolbar.

TB_GETUNICODEFORMAT
Retrieves the Unicode character format flag for the control.

TB_HIDEBUTTON
Hides or shows the specified button in a toolbar.

TB_HITTEST
Determines where a point lies in a toolbar control.

TB_INDETERMINATE
Sets or clears the indeterminate state of the specified button in a toolbar.

TB_INSERTBUTTON
Inserts a button in a toolbar.

TB_INSERTMARKHITTEST
Retrieves the insertion mark information for a point in a toolbar.

TB_ISBUTTONCHECKED
Determines whether the specified button in a toolbar is checked.

TB_ISBUTTONENABLED
Determines whether the specified button in a toolbar is enabled.

TB_ISBUTTONHIDDEN
Determines whether the specified button in a toolbar is hidden.

TB_ISBUTTONHIGHLIGHTED
Checks the highlight state of a toolbar button.

TB_ISBUTTONINDETERMINATE
Determines whether the specified button in a toolbar is indeterminate.

TB_ISBUTTONPRESSED
Determines whether the specified button in a toolbar is pressed.

TB_LOADIMAGES
Loads system-defined button images into a toolbar controls image list.

TB_MAPACCELERATOR
Determines the ID of the button that corresponds to the specified accelerator character.

TB_MARKBUTTON
Sets the highlight state of a given button in a toolbar control.

TB_MOVEBUTTON
Moves a button from one index to another.

TB_PRESSBUTTON
Presses or releases the specified button in a toolbar.

TB_REPLACEBITMAP
Replaces an existing bitmap with a new bitmap.

TB_SAVERESTORE
Send this message to initiate saving or restoring a toolbar state.

TB_SETANCHORHIGHLIGHT
Sets the anchor highlight setting for a toolbar.

TB_SETBITMAPSIZE
Sets the size of the bitmapped images to be added to a toolbar.

TB_SETBUTTONINFO
Sets the information for an existing button in a toolbar.

TB_SETBUTTONSIZE
Sets the size of buttons on a toolbar.

TB_SETBUTTONWIDTH
Sets the minimum and maximum button widths in the toolbar control.

TB_SETCMDID
Sets the command identifier of a toolbar button.

TB_SETCOLORSCHEME
Sets the color scheme information for the toolbar control.

TB_SETDISABLEDIMAGELIST
Sets the image list that the toolbar control will use to display disabled buttons.

TB_SETDRAWTEXTFLAGS
Sets the text drawing flags for the toolbar.

TB_SETEXTENDEDSTYLE
Sets the extended styles for a toolbar control.

TB_SETHOTIMAGELIST
Sets the image list that the toolbar control will use to display hot buttons.

TB_SETHOTITEM
Sets the hot item in a toolbar.

TB_SETHOTITEM2
Sets the hot item in a toolbar.

TB_SETIMAGELIST
Sets the image list that the toolbar uses to display buttons that are in their default state.

TB_SETINDENT
Sets the indentation for the first button in a toolbar control.

TB_SETINSERTMARK
Sets the current insertion mark for the toolbar.

TB_SETINSERTMARKCOLOR
Sets the color used to draw the insertion mark for the toolbar.

TB_SETLISTGAP
Sets the distance between the toolbar buttons on a specific toolbar.

TB_SETMAXTEXTROWS
Sets the maximum number of text rows displayed on a toolbar button.

TB_SETMETRICS
Sets the metrics of a toolbar control.

TB_SETPADDING
Sets the padding for a toolbar control.

TB_SETPARENT
Sets the window to which the toolbar control sends notification messages.

TB_SETPRESSEDIMAGELIST
Sets the image list that the toolbar uses to display buttons that are in a pressed state.

TB_SETROWS
Sets the number of rows of buttons in a toolbar.

TB_SETSTATE
Sets the state for the specified button in a toolbar.

TB_SETSTYLE
Sets the style for a toolbar control.

TB_SETTOOLTIPS
Associates a ToolTip control with a toolbar.

TB_SETUNICODEFORMAT
Sets the Unicode character format flag for the control. This message allows you to change the
character set used by the control at run time rather than having to re-create the control.

TB_SETWINDOWTHEME
Sets the visual style of a toolbar control.

TB_TRANSLATEACCELERATOR
Passes a keyboard message to the toolbar.


NOTIFICATIONS
=============

NM_CHAR (toolbar)
Sent by the toolbar when it receives a WM_CHAR message. This notification message is sent
in the form of a WM_NOTIFY message.

NM_CLICK (toolbar)
Sent by a toolbar control when the user clicks an item with the left mouse button.
This notification message is sent in the form of a WM_NOTIFY message.

NM_CUSTOMDRAW (toolbar)
Sent by the toolbar to notify its parent window about drawing operations. This notification
is sent in the form of a WM_NOTIFY message.

NM_DBLCLK (toolbar)
Notifies the parent window of a toolbar control that the user has double-clicked the left
mouse button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_KEYDOWN (toolbar)
Sent by a control when the control has the keyboard focus and the user presses a key.
This notification message is sent in the form of a WM_NOTIFY message.

NM_LDOWN
Notifies a toolbars parent window that the left mouse button has been pressed.

NM_RCLICK (toolbar)
Sent by a toolbar control when the user clicks the toolbar with the right mouse button.
This notification message is sent in the form of a WM_NOTIFY message.

NM_RDBLCLK (toolbar)
Notifies a control s parent window that the user has double-clicked the right mouse button
within the control. NM_RDBLCLK (toolbar) is sent in the form of a WM_NOTIFY message.

NM_RELEASEDCAPTURE (toolbar)
Notifies a toolbar control s parent window that the control is releasing mouse capture.
This notification is sent in the form of a WM_NOTIFY message.

NM_TOOLTIPSCREATED (toolbar)
Notifies a toolbar s parent window that the toolbar has created a ToolTip control.
This notification is sent in the form of a WM_NOTIFY message.

TBN_BEGINADJUST
Notifies a toolbar s parent window that the user has begun customizing a toolbar.
This message is sent in the form of a WM_NOTIFY message.

TBN_BEGINDRAG
Notifies a toolbar s parent window that the user has begun dragging a button in a toolbar.
This message is sent in the form of a WM_NOTIFY message.

TBN_CUSTHELP
Notifies a toolbar s parent window that the user has chosen the Help button in the
Customize Toolbar dialog box. This message is sent in the form of a WM_NOTIFY message.

TBN_DELETINGBUTTON
Sent by a toolbar control when a button is about to be deleted.

TBN_DRAGOUT
Sent by a toolbar control when the user clicks a button and then moves the cursor off the button.
This notification message is sent in the form of a WM_NOTIFY message.

TBN_DRAGOVER
Ascertains whether a TB_MARKBUTTON message should be sent for a button that is being dragged over..

TBN_DROPDOWN
Sent by a toolbar control when the user clicks a dropdown button.
This notification message is sent in the form of a WM_NOTIFY message.

TBN_DUPACCELERATOR
Ascertains whether an accelerator key can be used on two or more active toolbars.

TBN_ENDADJUST
Notifies a toolbar s parent window that the user has stopped customizing a toolbar.
This message is sent in the form of a WM_NOTIFY message.

TBN_ENDDRAG
Notifies the toolbar s parent window that the user has stopped dragging a button in a toolbar.
This message is sent in the form of a WM_NOTIFY message.

TBN_GETBUTTONINFO
Retrieves toolbar customization information and notifies the toolbar s parent window of any
changes being made to the toolbar. This is sent in the form of a WM_NOTIFY message.

TBN_GETDISPINFO
Retrieves display information for a toolbar item. This notification is sent in the form of a
WM_NOTIFY message.

TBN_GETINFOTIP
Retrieves infotip information for a toolbar item. This notification is sent in the form of a
WM_NOTIFY message.

TBN_GETOBJECT
Sent by a toolbar control that uses the TBSTYLE_REGISTERDROP style to request a drop target
object when the pointer passes over one of its buttons.

TBN_HOTITEMCHANGE
Sent by a toolbar control when the hot (highlighted) item changes. This notification message
is sent in the form of a WM_NOTIFY message.

TBN_INITCUSTOMIZE
Notifies a toolbar s parent window that customizing has started.

TBN_MAPACCELERATOR
Requests the index of the button in the toolbar corresponding to the specified accelerator character.

TBN_QUERYDELETE
Notifies the toolbar s parent window whether a button may be deleted from a toolbar while the
user is customizing the toolbar. This message is sent in the form of a WM_NOTIFY message.

TBN_QUERYINSERT
Notifies the toolbar s parent window whether a button may be inserted to the left of the
specified button while the user is customizing a toolbar. This message is sent in the form of a
WM_NOTIFY message.

TBN_RESET
Notifies the toolbar s parent window that the user has reset the content of the Customize
Toolbar dialog box. This message is sent in the form of a WM_NOTIFY message.

TBN_RESTORE
Notifies a toolbar s parent window that a toolbar is in the process of being restored.

TBN_SAVE
Notifies a toolbar s parent window that a toolbar is in the process of being saved.

TBN_TOOLBARCHANGE
Notifies the toolbar s parent window that the user has customized a toolbar. This message is sent
in the form of a WM_NOTIFY message.

TBN_WRAPACCELERATOR
Requests the index of the button in one or more toolbars corresponding to the specified accelerator
character.

TBN_WRAPHOTITEM
Notifies an application with two or more toolbars that the hot item is about to change.


BUTTON STATES
=============

TBSTATE_CHECKED
The button has the TBSTYLE_CHECK style and is being clicked.

TBSTATE_ELLIPSES
Version 4.70. The button s text is cut off and an ellipsis is displayed.

TBSTATE_ENABLED
The button accepts user input. A button that doesn t have this state is grayed.

TBSTATE_HIDDEN
The button is not visible and cannot receive user input.

TBSTATE_INDETERMINATE
The button is grayed.

TBSTATE_MARKED
Version 4.71. The button is marked. The interpretation of a marked item is dependent upon the application.

TBSTATE_PRESSED
The button is being clicked.

TBSTATE_WRAP
The button is followed by a line break. The button must also have the TBSTATE_ENABLED state.


STYLES
======

TBSTYLE_ALTDRAG
Allows users to change a toolbar button s position by dragging it while holding down the ALT key.
If this style is not specified, the user must hold down the SHIFT key while dragging a button. Note
that the CCS_ADJUSTABLE style must be specified to enable toolbar buttons to be dragged.

TBSTYLE_CUSTOMERASE
Version 4.70. Generates NM_CUSTOMDRAW notification messages when the toolbar processes
WM_ERASEBKGND messages.

TBSTYLE_FLAT
Version 4.70. Creates a flat toolbar. In a flat toolbar, both the toolbar and the buttons are
transparent and hot-tracking is enabled. Button text appears under button bitmaps. To prevent
repainting problems, this style should be set before the toolbar control becomes visible.

TBSTYLE_LIST
Version 4.70. Creates a flat toolbar with button text to the right of the bitmap. Otherwise,
this style is identical to TBSTYLE_FLAT. To prevent repainting problems, this style should be set
before the toolbar control becomes visible.

TBSTYLE_REGISTERDROP
Version 4.71. Generates TBN_GETOBJECT notification messages to request drop target objects
when the cursor passes over toolbar buttons.

TBSTYLE_TOOLTIPS
Creates a ToolTip control that an application can use to display descriptive text for
the buttons in the toolbar.

TBSTYLE_TRANSPARENT
Version 4.71. Creates a transparent toolbar. In a transparent toolbar, the toolbar is transparent
but the buttons are not. Button text appears under button bitmaps. To prevent repainting problems,
this style should be set before the toolbar control becomes visible.

TBSTYLE_WRAPABLE
Creates a toolbar that can have multiple lines of buttons. Toolbar buttons can "wrap" to
the next line when the toolbar becomes too narrow to include all buttons on the same line.
When the toolbar is wrapped, the break will occur on either the rightmost separator or the rightmost
button if there are no separators on the bar. This style must be set to display a vertical toolbar
control when the toolbar is part of a vertical rebar control. This style cannot be combined with CCS_VERT.


BUTTONS
=======

Note: A toolbar button can have a combination of the following styles. To specify a button style,
set the appropriate flags in the fsStyle member of the button s TBBUTTON structure. Not all styles
can be combined.
For Shell version 4.72 and earlier, both window and button style flags had the form TBSTYLE_XXX.
If you are compiling an application with version 4.72 or earlier of Commctrl.h, you must
use the TBSTYLE_XXX button style flags. For version 5.80 and later, all button styles have
the form BTNS_XXX. All of the TBSTYLE_XXX values have equivalent BTNS_XXX values,
with the same meaning and numerical value. For convenience, both forms are given in the following table.


BTNS_AUTOSIZE
Version 5.80. Specifies that the toolbar control should not assign the standard width to
the button. Instead, the button s width will be calculated based on the width of the text
plus the image of the button. Use the equivalent style flag, TBSTYLE_AUTOSIZE,
for version 4.72 and earlier.

BTNS_BUTTON
Version 5.80. Creates a standard button. Use the equivalent style flag, TBSTYLE_BUTTON,
for version 4.72 and earlier. This flag is defined as 0, and should be used to signify that no
other flags are set.

BTNS_CHECK
Version 5.80. Creates a dual-state push button that toggles between the pressed and nonpressed
states each time the user clicks it. The button has a different background color when it is in
the pressed state. Use the equivalent style flag, TBSTYLE_CHECK, for version 4.72 and earlier.

BTNS_CHECKGROUP
Version 5.80. Creates a button that stays pressed until another button in the group is pressed,
similar to option buttons (also known as radio buttons). It is equivalent to combining
BTNS_CHECK and BTNS_GROUP. Use the equivalent style flag, TBSTYLE_CHECKGROUP,
for version 4.72 and earlier.

BTNS_DROPDOWN
Version 5.80. Creates a drop-down style button that can display a list when the button is clicked.
Instead of the WM_COMMAND message used for normal buttons, drop-down buttons send a TBN_DROPDOWN
notification. An application can then have the notification handler display a list of options.
Use the equivalent style flag, TBSTYLE_DROPDOWN, for version 4.72 and earlier.
If the toolbar has the TBSTYLE_EX_DRAWDDARROWS extended style, drop-down buttons will have a
drop-down arrow displayed in a separate section to their right. If the arrow is clicked,
a TBN_DROPDOWN notification will be sent. If the associated button is clicked, a WM_COMMAND
message will be sent.

BTNS_GROUP
Version 5.80. When combined with BTNS_CHECK, creates a button that stays pressed until another
button in the group is pressed. Use the equivalent style flag, TBSTYLE_GROUP,
for version 4.72 and earlier.

BTNS_NOPREFIX
Version 5.80. Specifies that the button text will not have an accelerator prefix associated with it.
Use the equivalent style flag, TBSTYLE_NOPREFIX, for version 4.72 and earlier.

BTNS_SEP
Version 5.80. Creates a separator, providing a small gap between button groups. A button that has
this style does not receive user input. Use the equivalent style flag, TBSTYLE_SEP, for version 4.72
and earlier.

BTNS_SHOWTEXT
Version 5.81. Specifies that button text should be displayed. All buttons can have text,
but only those buttons with the BTNS_SHOWTEXT button style will display it. This button style must
be used with the TBSTYLE_LIST style and the TBSTYLE_EX_MIXEDBUTTONS extended style.
If you set text for buttons that do not have the BTNS_SHOWTEXT style, the toolbar control will
automatically display it as a ToolTip when the cursor hovers over the button. This feature allows
your application to avoid handling the TBN_GETINFOTIP or TTN_GETDISPINFO notification for the toolbar.

BTNS_WHOLEDROPDOWN
Version 5.80. Specifies that the button will have a drop-down arrow, but not as a separate section.
Buttons with this style behave the same, regardless of whether the TBSTYLE_EX_DRAWDDARROWS extended
style is set.

TBSTYLE_AUTOSIZE
Equivalent to BTNS_AUTOSIZE. Use TBSTYLE_AUTOSIZE for version 4.72 and earlier.

TBSTYLE_BUTTON
Equivalent to BTNS_BUTTON. Use TBSTYLE_BUTTON for version 4.72 and earlier.

TBSTYLE_CHECK
Equivalent to BTNS_CHECK. Use TBSTYLE_CHECK for version 4.72 and earlier.

TBSTYLE_CHECKGROUP
Equivalent to BTNS_CHECKGROUP. Use TBSTYLE_CHECKGROUP for version 4.72 and earlier.

TBSTYLE_DROPDOWN
Equivalent to BTNS_DROPDOWN. Use TBSTYLE_DROPDOWN for version 4.72 and earlier.

TBSTYLE_GROUP
Equivalent to BTNS_GROUP. Use TBSTYLE_GROUP for version 4.72 and earlier.

TBSTYLE_NOPREFIX
Equivalent to BTNS_NOPREFIX. Use TBSTYLE_NOPREFIX for version 4.72 and earlier.

TBSTYLE_SEP
Equivalent to BTNS_SEP. Use TBSTYLE_SEP for version 4.72 and earlier.

#endif
/*----------------------------------------------------------------------*/
