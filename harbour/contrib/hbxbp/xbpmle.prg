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
   METHOD   hbCreateFromQtPtr()
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()
   METHOD   exeBlock()
   METHOD   handleEvent()
   METHOD   setStyle()

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

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMLE:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QPlainTextEdit():new( ::pParent )

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

   ::setStyle()
   ::oParent:addChild( Self )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpMLE:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      ::oWidget := QPlainTextEdit()
      ::oWidget:pPtr := pQtObject

   ENDIF

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

METHOD XbpMLE:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpMLE:setStyle()
   LOCAL s, txt_:={}

   aadd( txt_, ' ' )
   aadd( txt_, ' QTextEdit {                                               ' )
   //aadd( txt_, '     background-color: white;                              ' )
   //aadd( txt_, '     background-image: url(new.png);                       ' )
   //aadd( txt_, '     background-attachment: scroll;                        ' )
   aadd( txt_, '   background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, ' )
   aadd( txt_, '                      stop:0 white, stop:1 darkgray);  ' )
   aadd( txt_, ' }                                                         ' )
   #if 0
   aadd( txt_, 'If the background-image is to be fixed with the viewport:  ' )
   aadd( txt_, '                                                           ' )
   aadd( txt_, ' QTextEdit {                                               ' )
   aadd( txt_, '     background-color: yellow;                             ' )
   aadd( txt_, '     background-image: url(new.png);                       ' )
   aadd( txt_, '     background-attachment: fixed;                         ' )
   aadd( txt_, ' }                                                         ' )
   #endif
   aadd( txt_, ' ' )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   ::oWidget:setStyleSheet( s )

   RETURN self

/*----------------------------------------------------------------------*/
