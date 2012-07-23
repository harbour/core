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
 *                 Xbase++ xbpStatusBar Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              25Nov2008
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

CLASS WvgStatusBar  INHERIT  WvgWindow /* WvgActiveXControl */

   DATA     caption                               INIT ""
   DATA     sizeGrip                              INIT .T.

   DATA     aItems                                INIT {}

   METHOD   numItems()                            INLINE Len( ::aItems )

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nMessage, aNM )

   METHOD   addItem( cCaption, xImage, cDLL, nStyle, cKey, nMode )
   METHOD   delItem( nItemORcKey )
   METHOD   getItem( nItemORcKey )
   METHOD   clear()
   METHOD   panelClick( xParam )                  SETGET
   METHOD   panelDblClick( xParam )               SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + WS_BORDER + SBARS_TOOLTIPS
   ::className   := STATUSCLASSNAME
   ::objType     := objTypeStatusBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::sizeGrip
      ::style += SBARS_SIZEGRIP
   ENDIF

   ::oParent:AddChild( SELF )

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF

   ::addItem( , , , , , -1 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:handleEvent( nMessage, aNM )
   LOCAL nHandled := 1
   LOCAL nObj, aNMH

   hb_traceLog( "       %s:handleEvent( %i )", __ObjGetClsName( self ), nMessage )

   DO CASE

   CASE nMessage == HB_GTE_RESIZED
      ::sendMessage( WM_SIZE, 0, 0 )
      RETURN 0

   CASE nMessage == HB_GTE_COMMAND
      IF HB_ISBLOCK( ::sl_lbClick )
         eval( ::sl_lbClick, NIL, NIL, self )
         RETURN 0
      ENDIF

   CASE nMessage == HB_GTE_NOTIFY
      aNMH := Wvg_GetNMMouseInfo( aNM[ 2 ] )

      DO CASE

      CASE aNMH[ NMH_code ] == NM_CLICK

         IF HB_ISBLOCK( ::sl_lbClick )
            IF aNMH[ NMH_dwItemSpec ] >= 0
               nObj := aNMH[ NMH_dwItemSpec ] + 1

               Eval( ::sl_lbClick, ::aItems[ nObj ], NIL, Self )

            ENDIF

            nHandled := 0
         ENDIF

      ENDCASE

   CASE nMessage == HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         WVG_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF HB_ISNUMERIC( ::hBrushBG )
         WVG_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ELSE
         RETURN WVG_GetCurrentBrush( aNM[ 1 ] )
      ENDIF

   ENDCASE

   RETURN nHandled

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:destroy()
   LOCAL i, nItems

   hb_traceLog( "          %s:destroy()", __objGetClsName() )

   IF ( nItems := Len( ::aItems ) ) > 0
      FOR i := 1 TO nItems

      NEXT
   ENDIF

   ::wvgWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:addItem( cCaption, xImage, cDLL, nStyle, cKey, nMode )
   LOCAL oPanel, lSuccess

   DEFAULT nMode TO 0

   HB_SYMBOL_UNUSED( xImage )
   HB_SYMBOL_UNUSED( cDLL )

   oPanel := WvgStatusBarPanel():new( cCaption, nStyle, cKey )
   oPanel:oParent := self

   oPanel:index := ::numItems + 1

   lSuccess := Wvg_StatusBarCreatePanel( ::hWnd, nMode )

   IF lSuccess
      aadd( ::aItems, oPanel )
   ELSE
      RETURN nil
   endif

   RETURN oPanel

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:delItem( nItemORcKey )
   LOCAL nIndex := 0

   IF HB_ISNUMERIC( nItemORcKey )
      nIndex := ascan( ::aItems, {|o| o:key == nItemORcKey } )
   ELSEIF HB_ISNUMERIC( nItemORcKey )
      nIndex := nItemORcKey
   ENDIF

   IF nIndex > 0
      /* Delete panel by window */
      adel( ::aItems, nIndex )
      asize( ::aItems, len( ::aItems ) - 1 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:getItem( nItemORcKey )
   LOCAL nIndex := 0, oPanel

   IF HB_ISSTRING( nItemORcKey  )
      nIndex := ascan( ::aItems, {|o| o:key == nItemORcKey } )

   ELSEIF HB_ISNUMERIC(  nItemORcKey  )
      nIndex := nItemORcKey

   ENDIF

   IF nIndex > 0
      oPanel := ::aItems[ nIndex ]
   ENDIF

   RETURN oPanel

/*----------------------------------------------------------------------*/
METHOD WvgStatusBar:clear()
   LOCAL i

   FOR i := 1 TO ::numItems
      /* Remove off window */

   NEXT

   ::aItems := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:panelClick( xParam )

   IF HB_ISBLOCK( xParam ) .or. HB_ISNIL( xParam )
      ::sl_lbClick := xParam
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvgStatusBar:panelDblClick( xParam )

   IF HB_ISBLOCK( xParam ) .or. HB_ISNIL( xParam )
      ::sl_lbDblClick := xParam
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

CLASS WvgStatusBarPanel

   DATA     alignment                             INIT WVGALIGN_LEFT
   DATA     autosize                              INIT WVGSTATUSBAR_AUTOSIZE_NONE
   DATA     bevel                                 INIT WVGSTATUSBAR_BEVEL_INSET
   DATA     enabled                               INIT .T.
   DATA     index                                 INIT 0
   DATA     key                                   INIT ""
   DATA     style                                 INIT WVGSTATUSBAR_PANEL_TEXT
   DATA     sl_caption                            INIT ""
   DATA     image                                 INIT NIL
   DATA     tooltipText                           INIT ""
   DATA     visible                               INIT .T.
   DATA     left                                  INIT 0
   DATA     width                                 INIT 0
   DATA     minWidth                              INIT 0

   METHOD   new( cCaption, nStyle, cKey )
   METHOD   caption( cCaption )                   SETGET

   DATA     oParent

   ENDCLASS
/*----------------------------------------------------------------------*/
METHOD WvgStatusBarPanel:new( cCaption, nStyle, cKey )

   DEFAULT cCaption       TO ::sl_caption
   DEFAULT nStyle         TO ::style
   DEFAULT cKey           TO ::key

   ::sl_caption     := cCaption
   ::style          := nStyle
   ::key            := cKey

   RETURN Self
/*----------------------------------------------------------------------*/
METHOD WvgStatusBarPanel:caption( cCaption )

   IF cCaption == NIL
      RETURN ::sl_caption

   ELSE
      DEFAULT cCaption TO ::sl_caption

      ::sl_caption := cCaption

      Wvg_StatusBarSetText( ::oParent:hWnd, ::index, cCaption )
   ENDIF

   RETURN Self
/*----------------------------------------------------------------------*/
/*                         MSDN on StatusBar Control                    */
/*----------------------------------------------------------------------*/
#if 0

Messages
========

SB_GETBORDERS
Retrieves the current widths of the horizontal and vertical borders of a status window.

SB_GETICON
Retrieves the icon for a part in a status bar.

SB_GETPARTS
Retrieves a count of the parts in a status window. The message also retrieves the coordinate
of the right edge of the specified number of parts.

SB_GETRECT
Retrieves the bounding rectangle of a part in a status window.

SB_GETTEXT
The SB_GETTEXT message retrieves the text from the specified part of a status window.

SB_GETTEXTLENGTH
The SB_GETTEXTLENGTH message retrieves the length, in characters, of the text from the specified
part of a status window.

SB_GETTIPTEXT
Retrieves the ToolTip text for a part in a status bar. The status bar must be created with the
SBT_TOOLTIPS style to enable ToolTips.

SB_GETUNICODEFORMAT
Retrieves the Unicode character format flag for the control.

SB_ISSIMPLE
Checks a status bar control to determine if it is in simple mode.

SB_SETBKCOLOR
Sets the background color in a status bar.

SB_SETICON
Sets the icon for a part in a status bar.

SB_SETMINHEIGHT
Sets the minimum height of a status windows drawing area.

SB_SETPARTS
Sets the number of parts in a status window and the coordinate of the right edge of each part.

SB_SETTEXT
The SB_SETTEXT message sets the text in the specified part of a status window.

SB_SETTIPTEXT
Sets the ToolTip text for a part in a status bar. The status bar must have been created with
the SBT_TOOLTIPS style to enable ToolTips.

SB_SETUNICODEFORMAT
Sets the Unicode character format flag for the control. This message allows you to change the
character set used by the control at run time rather than having to re-create the control.

SB_SIMPLE
Specifies whether a status window displays simple text or displays all window parts set by a
previous SB_SETPARTS message.

Notifications
=============

NM_CLICK (status bar)
Notifies the parent window of a status bar control that the user has clicked the left mouse
button within the control. NM_CLICK (status bar) is sent in the form of a WM_NOTIFY message.

NM_DBLCLK (status bar)
Notifies the parent window of a a status bar control that the user has double-clicked the
left mouse button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_RCLICK (status bar)
Notifies the parent window of a status bar control that the user has clicked the right mouse
button within the control. This notification is sent in the form of a WM_NOTIFY message.

NM_RDBLCLK (status bar)
Notifies the parent windows of a status bar control that the user has double-clicked the right
mouse button within the control. NM_RDBLCLK (status bar) is sent in the form of a WM_NOTIFY message.

SBN_SIMPLEMODECHANGE
Sent by a status bar control when the simple mode changes due to a SB_SIMPLE message.
This notification is sent in the form of a WM_NOTIFY message.

Styles
======

SBARS_SIZEGRIP
The status bar control will include a sizing grip at the right end of the status bar.
A sizing grip is similar to a sizing border; it is a rectangular area that the user can click and
drag to resize the parent window.

SBT_TOOLTIPS
Version 4.71.Use this style to enable ToolTips.

SBARS_TOOLTIPS
Version 5.80.Identical to SBT_TOOLTIPS. Use this flag for versions 5.00 or later.

#endif
/*----------------------------------------------------------------------*/
