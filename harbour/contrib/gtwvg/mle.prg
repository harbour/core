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

//
//
//
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                    Xbase++ xbpMLE compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               07Dec2008
 */
//
//
//

#include "hbclass.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

#include "hbgtwvg.ch"
#include "wvtwin.ch"
#include "wvgparts.ch"

//

#ifndef __DBG_PARTS__
#xtranslate hb_traceLog( [<x,...>] ) =>
#endif

//

CREATE CLASS WvgMLE INHERIT WvgWindow, WvgDataRef

   VAR      border                                INIT    .T.
   VAR      editable                              INIT    .T.
   VAR      horizScroll                           INIT    .T.
   VAR      vertScroll                            INIT    .T.
   VAR      wordWrap                              INIT    .T.
   VAR      ignoreTab                             INIT    .F.

   VAR      bufferLength                          INIT    32000

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )  VIRTUAL
   METHOD   destroy()
   METHOD   handleEvent( nMessage, aNM )

   METHOD   CLEAR()
   METHOD   copyMarked()
   METHOD   cutMarked()
   METHOD   deleteMarked()                        VIRTUAL
   METHOD   DELETE()                              VIRTUAL
   METHOD   pasteMarked()                         VIRTUAL
   METHOD   queryFirstChar()                      VIRTUAL
   METHOD   queryMarked()                         VIRTUAL
   METHOD   setFirstChar()                        VIRTUAL
   METHOD   setMarked()                           VIRTUAL
   METHOD   insert()                              VIRTUAL
   METHOD   charFromLine( nLine )                 VIRTUAL
   METHOD   lineFromChar()                        VIRTUAL
   METHOD   pos()                                 VIRTUAL

   VAR      sl_undo                               INIT    .T.
   ACCESS   undo                                  INLINE  iif( ::sl_undo, NIL, NIL )
   ASSIGN   undo( lUndo )                         INLINE  ::sl_undo := lUndo

   METHOD   setEditable()                         VIRTUAL
   METHOD   setWrap()                             VIRTUAL

   VAR      sl_hScroll
   ACCESS   hScroll                               INLINE  ::sl_hScroll
   ASSIGN   hScroll( bBlock )                     INLINE  ::sl_hScroll := bBlock

   VAR      sl_vScroll
   ACCESS   vScroll                               INLINE  ::sl_vScroll
   ASSIGN   vScroll( bBlock )                     INLINE  ::sl_vScroll := bBlock

   METHOD   changed( lChanged )                   SETGET

ENDCLASS

//

METHOD WvgMLE:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD + ES_MULTILINE + ES_WANTRETURN
   ::exStyle     := WS_EX_CLIENTEDGE
   ::className   := "EDIT"
   ::objType     := objTypeMLE

   RETURN Self

//

METHOD WvgMLE:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF ::tabStop
      ::style += WS_TABSTOP
   ENDIF
   IF !::editable
      ::style += ES_READONLY
   ENDIF
   IF ::border
      ::style += WS_BORDER
   ENDIF
   IF !( ::wordWrap )
      IF ::horizScroll
         ::style += WS_HSCROLL
      ELSE
         ::style += ES_AUTOHSCROLL
      ENDIF
   ENDIF
   IF ::vertScroll
      ::style += WS_VSCROLL
   ELSE
      ::style += ES_AUTOVSCROLL
   ENDIF

   ::oParent:addChild( Self )

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   RETURN Self

//

METHOD WvgMLE:handleEvent( nMessage, aNM )

   hb_traceLog( "       %s:handleEvent( %i )", __objGetClsName( self ), nMessage )

   DO CASE

   CASE nMessage == HB_GTE_COMMAND
      DO CASE
      CASE aNM[ NMH_code ] == EN_KILLFOCUS
         IF HB_ISBLOCK( ::sl_killInputFocus )
            Eval( ::sl_killInputFocus, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_SETFOCUS
         IF HB_ISBLOCK( ::sl_setInputFocus )
            Eval( ::sl_setInputFocus, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_HSCROLL
         IF HB_ISBLOCK( ::sl_hScroll )
            Eval( ::sl_hScroll, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_VSCROLL
         IF HB_ISBLOCK( ::sl_vScroll )
            Eval( ::sl_vScroll, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_CHANGE
      CASE aNM[ NMH_code ] == EN_UPDATE
      CASE aNM[ NMH_code ] == EN_MAXTEXT

      ENDCASE

   CASE nMessage ==  HB_GTE_CTLCOLOR
      IF HB_ISNUMERIC( ::clr_FG )
         WVG_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF HB_ISNUMERIC( ::hBrushBG )
         WVG_SetBkMode( aNM[ 1 ], 1 )
         RETURN ::hBrushBG
      ELSE
         RETURN WVG_GetCurrentBrush( aNM[ 1 ] )
      ENDIF

   CASE nMessage ==  HB_GTE_ANY
      IF ::isParentCrt()

         DO CASE
         CASE aNM[ NMH_code ] == WM_KEYDOWN
            IF aNM[ 2 ] == VK_TAB
               ::oParent:setFocus()
               RETURN EVENT_HANDELLED
            ENDIF

         CASE aNM[ NMH_code ] == WM_KILLFOCUS
            IF HB_ISBLOCK( ::sl_killInputFocus )
               Eval( ::sl_killInputFocus, NIL, NIL, Self )
            ENDIF

         CASE aNM[ NMH_code ] == WM_SETFOCUS
            IF HB_ISBLOCK( ::sl_setInputFocus )
               Eval( ::sl_setInputFocus, NIL, NIL, Self )
            ENDIF

         CASE aNM[ NMH_code ] == WM_HSCROLL
            IF HB_ISBLOCK( ::sl_hScroll )
               Eval( ::sl_hScroll, NIL, NIL, Self )
            ENDIF

         CASE aNM[ NMH_code ] == WM_VSCROLL
            IF HB_ISBLOCK( ::sl_vScroll )
               Eval( ::sl_vScroll, NIL, NIL, Self )
            ENDIF

         ENDCASE
      ENDIF

   ENDCASE

   RETURN EVENT_UNHANDELLED

//

METHOD WvgMLE:destroy()

   hb_traceLog( "          %s:destroy()", __objGetClsName( self ) )

   ::wvgWindow:destroy()

   RETURN NIL

//

METHOD WvgMLE:changed( lChanged )

   LOCAL lChg := ::sendMessage( EM_GETMODIFY, 0, 0 )

   IF HB_ISLOGICAL( lChanged )
      ::sendMessage( EM_SETMODIFY, iif( lChanged, 0, 1 ), 0 )
   ENDIF

   RETURN lChg

//

METHOD WvgMLE:clear()

   LOCAL cText := ::getData()

   ::setData( "" )

   RETURN Len( cText )

//

METHOD WvgMLE:copyMarked()

   LOCAL n, nB, nE

   n := ::sendMessage( EM_GETSEL )
   nB := WVG_LOWORD( n )
   nE := WVG_HIWORD( n )

   IF ( n := nE - nB ) > 0
      Wvt_SetClipboard( SubStr( ::getData(), nB, n ) )
   ENDIF

   RETURN n

//

METHOD WvgMLE:cutMarked()

   LOCAL n, nB, nE, cText

   n := ::sendMessage( EM_GETSEL )
   nB := WVG_LOWORD( n )
   nE := WVG_HIWORD( n )

   IF ( n := nE - nB ) > 0
      cText := ::getData()
      ::setData( SubStr( cText, 1, nB - 1 ) + SubStr( cText, nE ) )
   ENDIF

   RETURN n

//
