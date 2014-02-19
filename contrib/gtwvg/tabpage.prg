/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ xbpTabPage compatible Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
 *                               01Mar2009
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgTabPage  INHERIT  WvgWindow

   VAR    caption                               INIT NIL /* Character string, Numeric, Object ("")                                                                           */
   VAR    clipChildren                          INIT .T. /* Determines whether Xbase Parts in the child list are clipped during graphic output.                              */
   VAR    minimized                             INIT .T. /* Determines whether the XbpTabPage is minimized after it is created (the page is not visible).                    */
   VAR    postOffset                            INIT 80  /* Determines the distance between the end of the tab and the end of the page as a percentage of the page width.    */
   VAR    preOffset                             INIT 0   /* Determines the distance between the start of the tab and the start of the page as a percentage of the page width.*/
   VAR    tabHeight                             INIT -1  /* Determines the height of the tab.                                                                                */
   VAR    type                                  INIT WVGTABPAGE_TAB_TOP /* Determines the position of the tab.                                                               */

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD handleEvent( nMessage, aNM )

   METHOD Minimize()
   METHOD Maximize()

   VAR    sl_tabActivate
   METHOD tabActivate( xParam )                  SETGET

ENDCLASS

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgTabPage

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD
   ::className   := "SysTabControl32"
   ::objType     := objTypeTabPage

   RETURN Self

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgTabPage

   ::WvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::type == WVGTABPAGE_TAB_BOTTOM
      ::style += TCS_BOTTOM
   ENDIF
   IF ::clipSiblings
      ::style += WS_CLIPSIBLINGS
   ENDIF
#if 0
   ::style += WS_DLGFRAME
#endif

   ::style += TCS_FOCUSNEVER

   ::oParent:AddChild( SELF )

   ::createControl()

   ::SetWindowProcCallback()

   __defaultNIL( @::caption, " " )

   wapi_TabCtrl_InsertItem( ::pWnd, 0, ::caption )

   IF ::visible
      ::show()
   ENDIF

   IF ::minimized
      ::hide()
   ENDIF

   RETURN Self

METHOD handleEvent( nMessage, aNM ) CLASS WvgTabPage

   LOCAL aHdr

   DO CASE
   CASE nMessage == HB_GTE_SETFOCUS
      IF HB_ISEVALITEM( ::sl_tabActivate )

         RETURN EVENT_HANDLED
      ENDIF

   CASE nMessage == HB_GTE_COMMAND


   CASE nMessage == HB_GTE_RESIZED
      ::sendMessage( WM_SIZE, 0, 0 )
      RETURN EVENT_HANDLED

   CASE nMessage == HB_GTE_NOTIFY
      aHdr := Wvg_GetNMHdrInfo( aNM[ 2 ] )

      DO CASE
      CASE aHdr[ NMH_code ] == -551 /* TCN_SELCHANGE */

      ENDCASE

   CASE nMessage == HB_GTE_CTLCOLOR
      RETURN Wvg_GetStockObject( NULL_BRUSH )

   ENDCASE

   RETURN EVENT_UNHANDLED

METHOD tabActivate( xParam ) CLASS WvgTabPage

   IF HB_ISEVALITEM( xParam )
      ::sl_tabActivate := xParam
   ENDIF

   RETURN self

METHOD minimize() CLASS WvgTabPage

   ::hide()

   RETURN .F.

METHOD maximize() CLASS WvgTabPage

   ::show()

   RETURN .T.

METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgTabPage

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD destroy() CLASS WvgTabPage

   ::wvgWindow:destroy()

   RETURN NIL
