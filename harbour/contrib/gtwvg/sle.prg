/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008-2011 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                    Xbase++ xbpSLE compatible Class
 *
 *                  Pritpal Bedi <bedipritpal@hotmail.com>
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

CLASS WvgSLE INHERIT WvgWindow, WvgDataRef

   DATA     align                                 INIT WVGSLE_LEFT
   DATA     autoKeyboard                          INIT .T.
   DATA     autoSize                              INIT .F.
   DATA     autoTab                               INIT .F.
   DATA     border                                INIT .T.
   DATA     bufferLength                          INIT 32
   DATA     editable                              INIT .T.
   DATA     unReadable                            INIT .F.

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )  VIRTUAL
   METHOD   destroy()
   METHOD   handleEvent( nMessage, aNM )

   METHOD   CLEAR()
   METHOD   copyMarked()
   METHOD   cutMarked()
   METHOD   delMarked()                           VIRTUAL
   METHOD   editBuffer()                          VIRTUAL
   METHOD   pasteMarked()                         VIRTUAL
   METHOD   queryFirstChar()                      VIRTUAL
   METHOD   queryMarked()                         VIRTUAL
   METHOD   setFirstChar()                        VIRTUAL
   METHOD   setMarked()                           VIRTUAL

   METHOD   setInsertMode( lInsertMode )          VIRTUAL

   DATA     sl_hScroll
   ACCESS   hScroll                               INLINE  ::sl_hScroll
   ASSIGN   hScroll( bBlock )                     INLINE  ::sl_hScroll := bBlock

   DATA     sl_typeOut
   ACCESS   typeOut                               INLINE  ::sl_typeOut
   ASSIGN   typeOut( bBlock )                     INLINE  ::sl_typeOut := bBlock

   METHOD   changed( lChanged )                   SETGET

   DATA     sl_returnPressed
   METHOD   returnPressed( ... )                  SETGET

ENDCLASS

//

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgSLE

   ::wvgWindow:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style       := WS_CHILD
   ::className   := "EDIT"
   ::objType     := objTypeSLE

   RETURN Self

//

METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgSLE

   LOCAL es_ := { ES_LEFT, ES_RIGHT, ES_CENTER }

   ::wvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::style += es_[ ::align ]
   ::style += ES_AUTOHSCROLL

   IF ::tabStop
      ::style += WS_TABSTOP
   ENDIF
   IF ::autoSize

   ENDIF
   IF ! ::editable
      ::style += ES_READONLY
   ENDIF
   IF ::unReadable
      ::style += ES_PASSWORD
   ENDIF
   IF ::border
      ::style += WS_BORDER
   ENDIF

   ::oParent:addChild( Self )

   ::createControl()

   ::SetWindowProcCallback()

   IF ::visible
      ::show()
   ENDIF
   ::setPosAndSize()

   IF HB_ISOBJECT( ::datalink )
      Eval( ::datalink )
   ENDIF

   ::sendMessage( EM_SETLIMITTEXT, ::bufferLength )

   RETURN Self

//

METHOD handleEvent( nMessage, aNM ) CLASS WvgSLE

   DO CASE
   CASE nMessage == HB_GTE_RESIZED
      IF ::isParentCrt()
         ::oParent:setFocus()
         ::rePosition()
      ENDIF
      ::sendMessage( WM_SIZE, 0, 0 )

   CASE nMessage == HB_GTE_COMMAND
      DO CASE
      CASE aNM[ NMH_code ] == EN_CHANGE

      CASE aNM[ NMH_code ] == EN_UPDATE

      CASE aNM[ NMH_code ] == EN_MAXTEXT

      CASE aNM[ NMH_code ] == EN_KILLFOCUS
         IF HB_ISBLOCK( ::sl_killInputFocus )
            Eval( ::sl_killInputFocus, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_SETFOCUS
         IF HB_ISBLOCK( ::sl_setInputFocus )
            Eval( ::sl_setInputFocus, NIL, NIL, Self )
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

   CASE nMessage == HB_GTE_ANY
      DO CASE
      CASE aNM[ NMH_code ] == WM_KILLFOCUS
         IF HB_ISBLOCK( ::sl_killInputFocus )
            Eval( ::sl_killInputFocus, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == WM_SETFOCUS
         IF HB_ISBLOCK( ::sl_setInputFocus )
            Eval( ::sl_setInputFocus, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == WM_KEYDOWN
         IF aNM[ 2 ] == K_ENTER
            IF ::isParentCrt()
               ::oParent:setFocus()
            ENDIF
            IF HB_ISBLOCK( ::sl_returnPressed )
               Eval( ::sl_returnPressed, NIL, NIL, Self )
            ENDIF
         ELSEIF aNM[ 2 ] == VK_TAB
            IF ::isParentCrt()
               ::oParent:setFocus()
               RETURN EVENT_HANDELLED
            ENDIF
         ELSEIF aNM[ 2 ] == 65
            // RETURN EVENT_HANDELLED
         ENDIF

      ENDCASE

   ENDCASE

   RETURN EVENT_UNHANDELLED

//

METHOD destroy() CLASS WvgSLE

   hb_traceLog( "          %s:destroy()", __objGetClsName( self ) )

   ::wvgWindow:destroy()

   RETURN NIL

//

METHOD WvgSLE:changed( lChanged )

   LOCAL lChg := ::sendMessage( EM_GETMODIFY, 0, 0 )

   IF HB_ISLOGICAL( lChanged )
      ::sendMessage( EM_SETMODIFY, iif( lChanged, 0, 1 ), 0 )
   ENDIF

   RETURN lChg

//

METHOD WvgSLE:clear()

   LOCAL cText := ::getData()

   ::setData( "" )

   RETURN Len( cText )

//

METHOD WvgSLE:copyMarked()

   LOCAL n, nB, nE

   n := ::sendMessage( EM_GETSEL )
   nB := WVG_LOWORD( n )
   nE := WVG_HIWORD( n )

   IF ( n := nE - nB ) > 0
      Wvt_SetClipboard( SubStr( ::getData(), nB, n ) )
   ENDIF

   RETURN n

//

METHOD WvgSLE:cutMarked()

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

METHOD WvgSLE:returnPressed( ... )

   LOCAL a_ := hb_AParams()

   IF Len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_returnPressed := a_[ 1 ]
   ELSEIF Len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_returnPressed )
      Eval( ::sl_returnPressed, NIL, NIL, Self )
   ENDIF

   RETURN Self

//
