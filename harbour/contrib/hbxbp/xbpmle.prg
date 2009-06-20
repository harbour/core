/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                    Xbase++ xbpMLE compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               19Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpMLE INHERIT XbpWindow, XbpDataRef

   DATA     border                                INIT    .T.
   DATA     editable                              INIT    .T.
   DATA     horizScroll                           INIT    .T.
   DATA     vertScroll                            INIT    .T.
   DATA     wordWrap                              INIT    .T.
   DATA     ignoreTab                             INIT    .F.

   DATA     changed                               INIT    .F.

   METHOD   new()
   METHOD   create()
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()
   METHOD   exeBlock()
   METHOD   handleEvent()

   METHOD   clear()                               VIRTUAL
   METHOD   copyMarked()                          VIRTUAL
   METHOD   cutMarked()                           VIRTUAL
   METHOD   deleteMarked()                        VIRTUAL
   METHOD   delete()                              VIRTUAL
   METHOD   pasteMarked()                         VIRTUAL
   METHOD   queryFirstChar()                      VIRTUAL
   METHOD   queryMarked()                         VIRTUAL
   METHOD   setFirstChar()                        VIRTUAL
   METHOD   setMarked()                           VIRTUAL
   METHOD   insert()                              VIRTUAL
   METHOD   charFromLine()                        VIRTUAL
   METHOD   lineFromChar()                        VIRTUAL
   METHOD   pos()                                 VIRTUAL

   DATA     sl_undo                               INIT    .T.
   ACCESS   undo                                  INLINE  IF( ::sl_undo, NIL, NIL )
   ASSIGN   undo()                                INLINE  ::sl_undo := .t.

   METHOD   setEditable( lYes )                   INLINE  ::xDummy := ::oWidget:readOnly(), ;
                                                          ::oWidget:setReadOnly( !lYes ), ::xDummy
   METHOD   setWrap( lWrap )                      INLINE  ::xDummy := ::oWidget:lineWrapMode(),;
                                                          ::oWidget:setLineWrapMode( IF( lWrap,1,0 ) ),;
                                                          ::xDummy == 1

   DATA     sl_hScroll
   ACCESS   hScroll                               INLINE  ::sl_hScroll
   ASSIGN   hScroll( bBlock )                     INLINE  ::sl_hScroll := bBlock

   DATA     sl_vScroll
   ACCESS   vScroll                               INLINE  ::sl_vScroll
   ASSIGN   vScroll( bBlock )                     INLINE  ::sl_vScroll := bBlock

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpMLE:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMLE:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QTextEdit():new( ::pParent )

   IF !( ::editable )
      ::oWidget:setReadOnly( .t. )
   ELSE
      ::oWidget:setReadOnly( .f. )
   ENDIF

   IF !( ::wordWrap )
      ::oWidget:setLineWrapMode( 0 )
   ELSE
      ::oWidget:setLineWrapMode( 1 )
   ENDIF

   #if 0
   IF ::tabStop
      ::style += WS_TABSTOP
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
   #endif

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   IF hb_isBlock( ::datalink )
      eval( ::datalink )
   ENDIF

   ::oParent:addChild( Self )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMLE:exeBlock()

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD XbpMLE:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/


   #if 0
   DO CASE
   CASE nMessage == HB_GTE_COMMAND
      DO CASE
      CASE aNM[ NMH_code ] == EN_CHANGE
         ::changed := .t.

      CASE aNM[ NMH_code ] == EN_UPDATE

      CASE aNM[ NMH_code ] == EN_MAXTEXT

      CASE aNM[ NMH_code ] == EN_KILLFOCUS
         IF hb_isBlock( ::sl_killInputFocus )
            eval( ::sl_killInputFocus, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_SETFOCUS
         IF hb_isBlock( ::sl_setInputFocus )
            eval( ::sl_setInputFocus, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_HSCROLL
         IF hb_isBlock( ::sl_hScroll )
            eval( ::sl_hScroll, NIL, NIL, Self )
         ENDIF

      CASE aNM[ NMH_code ] == EN_VSCROLL
         IF hb_isBlock( ::sl_vScroll )
            eval( ::sl_vScroll, NIL, NIL, Self )
         ENDIF

      ENDCASE

   CASE nMessage ==  HB_GTE_CTLCOLOR
      IF hb_isNumeric( ::clr_FG )
         WVG_SetTextColor( aNM[ 1 ], ::clr_FG )
      ENDIF
      IF hb_isNumeric( ::hBrushBG )
         WVG_SetBkMode( aNM[ 1 ], 1 )
         RETURN ( ::hBrushBG )
      ELSE
         RETURN WVG_GetCurrentBrush( aNM[ 1 ] )
      ENDIF

   ENDCASE
   #endif

/*----------------------------------------------------------------------*/

METHOD XbpMLE:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/
