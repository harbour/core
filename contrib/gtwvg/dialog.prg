/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                  Xbase++ Compatible xbpDialog Class
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                             17Nov2008
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

CREATE CLASS WvgDialog FROM WvgWindow

   VAR    oMenu
   VAR    aRect
   VAR    drawingArea
   VAR    tasklist                              INIT  .T.

   METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD destroy()
   METHOD setFrameState( nState )
   METHOD getFrameState()
   METHOD menuBar()

   METHOD showModal()                           INLINE NIL
   METHOD setTitle( cTitle )                    INLINE ::title := cTitle, hb_gtInfo( HB_GTI_WINTITLE, cTitle )
   METHOD getTitle()                            INLINE hb_gtInfo( HB_GTI_WINTITLE )
   METHOD calcClientRect()                      INLINE ::aRect := Wvg_GetClientRect( ::hWnd ), { 0, 0, ::aRect[ 3 ], ::aRect[ 4 ] }
   METHOD calcFrameRect()                       INLINE ::aRect := Wvg_GetWindowRect( ::hWnd ), { ::aRect[ 1 ], ::aRect[ 2 ], ::aRect[ 3 ] - ::aRect[ 1 ], ::aRect[ 4 ] - ::aRect[ 2 ] }

ENDCLASS

METHOD WvgDialog:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::WvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::className   := "WVGDIALOG"
   ::resizeMode  := 0
   ::mouseMode   := 0
   ::objType     := objTypeDialog

   ::style       := WS_THICKFRAME + WS_OVERLAPPED + WS_CAPTION + WS_SYSMENU + WS_MINIMIZEBOX + WS_MAXIMIZEBOX

   RETURN Self

METHOD WvgDialog:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   LOCAL oW

   ::WvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::lModal
      ::pGT  := hb_gtCreate( "WGU" )
      ::pGTp := hb_gtSelect( ::pGT )
   ELSE
      hb_gtReload( "WGU" )
      ::pGT := hb_gtSelect()
   ENDIF

   hb_gtInfo( HB_GTI_PRESPARAMS, { ::exStyle, ::style, ::aPos[ 1 ], ::aPos[ 2 ], ;
      ::aSize[ 1 ], ::aSize[ 2 ], ::pGTp, .F., .F., HB_WNDTYPE_DIALOG } )

   IF ::visible
      hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, SW_NORMAL )
   ELSE
      hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, SW_HIDE   )
   ENDIF

   ::hWnd := hb_gtInfo( HB_GTI_SPEC, HB_GTS_WINDOWHANDLE )

   hb_gtInfo( HB_GTI_RESIZABLE, ::resizable )
   hb_gtInfo( HB_GTI_CLOSABLE, ::closable  )
   hb_gtInfo( HB_GTI_WINTITLE, ::title     )

   IF ! Empty( ::icon )
      IF HB_ISNUMERIC( ::icon )
         hb_gtInfo( HB_GTI_ICONRES, ::icon )

      ELSEIF HB_ISSTRING( ::icon )
         hb_gtInfo( HB_GTI_ICONFILE, ::icon )

      ENDIF
   ENDIF

   IF ::lModal
      hb_gtInfo( HB_GTI_DISABLE, ::pGTp )
   ENDIF

   IF ::visible
      ::lHasInputFocus := .T.
   ENDIF

   oW := WvgDrawingArea():new( Self ):create( , , { 0, 0 }, ::currentSize(), , .F. )
   IF ! Empty( oW:hWnd )
      ::drawingArea := oW
   ELSE
      ::drawingArea := Self
   ENDIF

   hb_gtInfo( HB_GTI_NOTIFIERBLOCK, {| nEvent, ... | ::notifier( nEvent, ... ) } )

   RETURN Self

METHOD WvgDialog:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::WvgWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

METHOD WvgDialog:destroy()

   IF HB_ISOBJECT( ::oMenu )
      ::oMenu:destroy()
   ENDIF

   IF Len( ::aChildren ) > 0
      AEval( ::aChildren, {| o | o:destroy() } )
   ENDIF

   IF ! Empty( ::hBrushBG )
      Wvg_DeleteObject( ::hBrushBG )
   ENDIF

   ::pGT  := NIL
   ::pGTp := NIL

   RETURN Self

METHOD WvgDialog:setFrameState( nState )

   LOCAL lSuccess := .F.

   DO CASE

   CASE nState == WVGDLG_FRAMESTAT_MINIMIZED
      RETURN ::sendMessage( WM_SYSCOMMAND, SC_MINIMIZE, 0 ) != 0

   CASE nState == WVGDLG_FRAMESTAT_MAXIMIZED
      RETURN ::sendMessage( WM_SYSCOMMAND, SC_MAXIMIZE, 0 ) != 0

   CASE nState == WVGDLG_FRAMESTAT_NORMALIZED
      RETURN ::sendMessage( WM_SYSCOMMAND, SC_RESTORE, 0 ) != 0

   ENDCASE

   RETURN lSuccess

METHOD WvgDialog:getFrameState()

   IF Wvg_IsIconic( ::hWnd )
      RETURN WVGDLG_FRAMESTAT_MINIMIZED
   ENDIF
   IF Wvg_IsZoomed( ::hWnd )
      RETURN WVGDLG_FRAMESTAT_MAXIMIZED
   ENDIF

   RETURN WVGDLG_FRAMESTAT_NORMALIZED

METHOD WvgDialog:menuBar()

   IF ! HB_ISOBJECT( ::oMenu )
      ::oMenu := WvgMenuBar():New( self ):create()
   ENDIF

   RETURN ::oMenu
