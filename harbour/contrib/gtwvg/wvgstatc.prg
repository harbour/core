/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                                EkOnkar
//                          ( The LORD is ONE )
//
//                  Xbase++ xbpTreeView compatible Class
//
//                  Pritpal Bedi <pritpal@vouchcac.com>
//                               26Nov2008
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include 'hbclass.ch'
#include 'common.ch'
#include 'inkey.ch'
#include 'hbgtinfo.ch'

#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'wvgparts.ch'

//----------------------------------------------------------------------//

#ifndef __DBG_PARTS__
//#xtranslate hb_ToOutDebug( [<x,...>] ) =>
#endif

//----------------------------------------------------------------------//

CLASS WvgStatic  INHERIT  WvgWindow

   DATA     autoSize                              INIT .F.
   DATA     caption                               INIT ''
   DATA     clipParent                            INIT .T.
   DATA     clipSiblings                          INIT .F.
   DATA     options                               INIT WVGSTATIC_TEXT_LEFT
   DATA     type                                  INIT WVGSTATIC_TYPE_TEXT


   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   setCaption( xCaption, cDll )

   METHOD   handleEvent()

   ENDCLASS
//----------------------------------------------------------------------//

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgStatic

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::WvgWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   // + SS_NOTIFY + SS_ETCHEDFRAME //SS_SUNKEN //+ SS_WHITERECT
   //
   ::style       := WS_CHILD + WS_CLIPCHILDREN
   ::exStyle     := WS_EX_NOPARENTNOTIFY
   ::className   := 'STATIC'
   ::objType     := objTypeStatic

   RETURN Self

//----------------------------------------------------------------------//

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgStatic
   LOCAL wm_:= {}

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::visible
      ::style += WS_VISIBLE
   ENDIF

   #if 0
   SS_ETCHEDFRAME
   SS_SUNKEN
   SS_LEFTNOWORDWRAP
   SS_SIMPLE
   SS_CENTERIMAGE
   SS_REALSIZEIMAGE
   SS_ENHMETAFILE
   SS_ETCHEDHORZ
   SS_ETCHEDVERT
   SS_RIGHTJUST
   #endif


   SWITCH ::type
   CASE WVGSTATIC_TYPE_TEXT
      IF ( hb_bitAnd( ::options, WVGSTATIC_TEXT_LEFT ) == WVGSTATIC_TEXT_LEFT )
         ::style += SS_LEFT //+ SS_LEFTNOWORDWRAP
      ENDIF
      IF ( hb_bitAnd( ::options, WVGSTATIC_TEXT_RIGHT ) == WVGSTATIC_TEXT_RIGHT )
         ::style += SS_RIGHT
      ENDIF
      IF ( hb_bitAnd( ::options, WVGSTATIC_TEXT_CENTER ) == WVGSTATIC_TEXT_CENTER )
         ::style += SS_CENTER
      ENDIF
      IF ( hb_bitAnd( ::options, WVGSTATIC_TEXT_WORDBREAK ) == WVGSTATIC_TEXT_WORDBREAK )
         ::style -= SS_LEFTNOWORDWRAP
      ENDIF
      EXIT

   CASE WVGSTATIC_TYPE_GROUPBOX
      EXIT
   CASE WVGSTATIC_TYPE_ICON
      ::style += SS_ICON
      EXIT
   CASE WVGSTATIC_TYPE_SYSICON
      EXIT
   CASE WVGSTATIC_TYPE_BITMAP
      ::style += SS_BITMAP
      IF     ::options == WVGSTATIC_BITMAP_TILED

      ELSEIF ::options == WVGSTATIC_BITMAP_SCALED

      ELSE

      ENDIF
      EXIT
   CASE WVGSTATIC_TYPE_FGNDRECT
      ::style += SS_WHITERECT
      EXIT
   CASE WVGSTATIC_TYPE_BGNDRECT
      ::style += SS_BLACKRECT
      EXIT
   CASE WVGSTATIC_TYPE_HALFTONERECT
      ::style += SS_GRAYRECT
      EXIT
   CASE WVGSTATIC_TYPE_FGNDFRAME
      ::style += SS_WHITEFRAME
      EXIT
   CASE WVGSTATIC_TYPE_BGNDFRAME
      ::style += SS_BLACKFRAME
      EXIT
   CASE WVGSTATIC_TYPE_HALFTONEFRAME
      ::style += SS_GRAYFRAME
      EXIT

   CASE WVGSTATIC_TYPE_RAISEDBOX
      EXIT
   CASE WVGSTATIC_TYPE_RECESSEDBOX
      EXIT

   CASE WVGSTATIC_TYPE_RAISEDRECT
      EXIT
   CASE WVGSTATIC_TYPE_RECESSEDRECT
      EXIT

   CASE WVGSTATIC_TYPE_RAISEDLINE
      EXIT
   CASE WVGSTATIC_TYPE_RECESSEDLINE
      EXIT
   END  // ::type

   // Options
   IF ( ascan( { WVGSTATIC_TYPE_FGNDFRAME, WVGSTATIC_TYPE_BGNDFRAME, WVGSTATIC_TYPE_HALFTONEFRAME }, ::type ) > 0 )
      IF     ( hb_bitAnd( ::options, WVGSTATIC_FRAMETHIN ) == WVGSTATIC_FRAMETHIN )
         ::style += WS_BORDER

      ELSEIF ( hb_bitAnd( ::options, WVGSTATIC_FRAMETHICK ) == WVGSTATIC_FRAMETHICK )
         ::style += WS_DLGFRAME

      ENDIF
   ENDIF

   //------------------- API request to create control ----------------//
   ::oParent:addChild( SELF )

   ::createControl()

   ::nWndProc := hb_AsCallBack( 'CONTROLWNDPROC', Self, 4 )
   ::nOldProc := Win_SetWndProc( ::hWnd, ::nWndProc )

   IF ::visible
      ::show()
   ENDIF

   //------------------- After creation settings------------------------//
   ::setCaption( ::caption )

   RETURN Self

//----------------------------------------------------------------------//

METHOD handleEvent( nMessage/*, aNM */) CLASS WvgStatic

   hb_ToOutDebug( "       %s:handleEvent( %i )", __ObjGetClsName( self ), nMessage )

   SWITCH nMessage

   CASE WM_SIZE
      IF hb_isBlock( ::sl_resize )
         eval( ::sl_resize, NIL, NIL, self )
         RETURN ( 0 )
      ENDIF
      EXIT

   END

   RETURN ( 1 )

//----------------------------------------------------------------------//

METHOD destroy() CLASS WvgStatic

   hb_ToOutDebug( "          %s:destroy()", __objGetClsName() )

   IF len( ::aChildren ) > 0
      aeval( ::aChildren, {|o| o:destroy() } )
   ENDIF
   IF Win_IsWindow( ::hWnd )
      Win_DestroyWindow( ::hWnd )
   ENDIF
   HB_FreeCallback( ::nWndProc )

   RETURN NIL

//----------------------------------------------------------------------//

METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgStatic

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

//----------------------------------------------------------------------//

METHOD setCaption( xCaption, cDll )

   HB_SYMBOL_UNUSED( cDll )

   DEFAULT xCaption TO ::caption
   ::caption := xCaption

   DO CASE
   CASE ::type == WVGSTATIC_TYPE_TEXT
      Win_SendMessageText( ::hWnd, WM_SETTEXT, 0, ::caption )

   ENDCASE

   RETURN Self

//----------------------------------------------------------------------//
//                         MSDN on Static Control
//----------------------------------------------------------------------//
#if 0

Static Control Styles
Using Static Controls

This topic provides an example that uses a static control.



Messages
========

STM_GETICON
An application sends the STM_GETICON message to retrieve a handle to the icon associated
with a static control that has the SS_ICON style.

STM_GETIMAGE
An application sends an STM_GETIMAGE message to retrieve a handle to the image
(icon or bitmap) associated with a static control.

STM_SETICON
An application sends the STM_SETICON message to associate an icon with an icon control.

STM_SETIMAGE
An application sends an STM_SETIMAGE message to associate a new image with a static control.


Notifications
=============

STN_CLICKED
The STN_CLICKED notification message is sent when the user clicks a static control
that has the SS_NOTIFY style. The parent window of the control receives this
notification message through the WM_COMMAND message.

STN_DBLCLK
The STN_DBLCLK notification message is sent when the user double-clicks a static
control that has the SS_NOTIFY style. The parent window of the control receives this
notification message through the WM_COMMAND message.

STN_DISABLE
The STN_DISABLE notification message is sent when a static control is disabled.
The static control must have the SS_NOTIFY style to receive this notification message.
The parent window of the control receives this notification message through the WM_COMMAND message.

STN_ENABLE
The STN_ENABLE notification message is sent when a static control is enabled.
The static control must have the SS_NOTIFY style to receive this notification message.
The parent window of the control receives this notification message through the WM_COMMAND message.

WM_CTLCOLORSTATIC
A static control, or an edit control that is read-only or disabled, sends the
WM_CTLCOLORSTATIC message to its parent window when the control is about to be drawn.
By responding to this message, the parent window can use the specified device context
handle to set the text and background colors of the static control.
A window receives this message through its WindowProc function.


#endif
//----------------------------------------------------------------------//



