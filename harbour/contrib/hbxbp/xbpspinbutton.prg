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
 *                    Xbase++ XbpSpinButton compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               17Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpSpinButton INHERIT XbpWindow, XbpDataRef

   DATA     fastSpin                              INIT    .f.
   DATA     master
   DATA     padWithZero                           INIT    .f.

   DATA     align                                 INIT    XBPSLE_LEFT
   DATA     autoKeyboard                          INIT    .T.
   DATA     autoSize                              INIT    .F.
   DATA     autoTab                               INIT    .F.
   DATA     border                                INIT    .T.
   DATA     bufferLength                          INIT    32
   DATA     editable                              INIT    .T.
   DATA     unReadable                            INIT    .F.

   DATA     changed                               INIT    .F.

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) VIRTUAL
   METHOD   destroy()
   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   exeBlock( nMsg, p1, p2 )

   METHOD   clear()                               INLINE  ::oWidget:clear()
   METHOD   copyMarked()                          INLINE  ::oWidget:copy()
   METHOD   cutMarked()                           INLINE  ::oWidget:cut()
   METHOD   delMarked()                           INLINE  ::oWidget:del()
   METHOD   editBuffer()                          INLINE  ::oWidget:text()
   METHOD   pasteMarked()                         INLINE  ::oWidget:paste()
   METHOD   queryFirstChar()                      VIRTUAL
   METHOD   queryMarked()                         INLINE  { ::oWidget:selectionStart(), ::oWidget:selectionEnd() }
   METHOD   setFirstChar( nPos )                  VIRTUAL
   METHOD   setMarked( aStartEnd )                INLINE  ::setSelection( aStartEnd[ 1 ], aStartEnd[ 2 ] )

   METHOD   spinDown( nDecrement )                INLINE  ::oWidget:stepBy( -nDecrement )
   METHOD   spinUp( nIncrement )                  INLINE  ::oWidget:stepBy( nIncrement )
   METHOD   setNumLimits( nMin, nMax )            INLINE  ::oWidget:setRange( nMin, nMax )

   DATA     sl_xbeSpinDown
   ACCESS   down                                  INLINE  ::sl_hScroll
   ASSIGN   down( bBlock )                        INLINE  ::sl_hScroll := bBlock

   DATA     sl_xbeSpinUp
   ACCESS   up                                    INLINE  ::sl_xbeSpinUp
   ASSIGN   up( bBlock )                          INLINE  ::sl_xbeSpinUp := bBlock

   DATA     sl_xbeSpinEndSpin
   ACCESS   endSpin                               INLINE  ::sl_xbeSpinEndSpin
   ASSIGN   endSpin( bBlock )                     INLINE  ::sl_xbeSpinEndSpin := bBlock

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL es_:= { Qt_AlignLeft, Qt_AlignRight, Qt_AlignHCenter }

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QSpinBox():new( ::pParent )
   ::oWidget:setKeyboardTracking( .t. )

   IF ::fastSpin
      ::oWidget:setAccelerated( .t. )
   ENDIF
   ::oWidget:setReadOnly( ! ::editable )

   ::oWidget:setFrame( ::border )

   ::oWidget:setAlignment( es_[ ::align ] )

   #if 0   ////////////////////////////////////
   ::oWidget:installEventFilter( ::pEvents )

HBXBP_DEBUG( "XbpSpinButton:create  2" )
   ::connectEvent( ::pWidget, QEvent_FocusIn , {|o,e| ::exeBlock( 7, e, o ) } )
   ::connectEvent( ::pWidget, QEvent_FocusOut, {|o,e| ::exeBlock( 8, e, o ) } )
   ::connectEvent( ::pWidget, QEvent_KeyPress, {|o,e| ::exeBlock( 9, e, o ) } )
   #endif

   ::connect( ::pWidget, "valueChanged(int)" , {|| ::sl_editBuffer := ::oWidget:value() } )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   ::setData()
   #if 0
   IF hb_isBlock( ::datalink )
      eval( ::datalink )
   ENDIF
   #endif
   ::oParent:addChild( Self )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      ::oWidget := QSpinBox()
      ::oWidget:pPtr := pQtObject

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:exeBlock( nMsg, p1, p2 )

   HB_SYMBOL_UNUSED( p1 )
   HB_SYMBOL_UNUSED( p2 )

   ::sl_editBuffer := ::oWidget:value()

   DO CASE
   CASE nMsg == 9    // valueChanged
      IF hb_isBlock( ::sl_keyboard )
         eval( ::sl_keyboard, NIL, NIL, self )
      ENDIF

   CASE nMsg == 7    // QEvent_FocusIn
      IF hb_isBlock( ::sl_setInputFocus )
         eval( ::sl_setInputFocus, NIL, NIL, Self )
      ENDIF

   CASE nMsg == 8    // QEvent_FocusOut
      IF hb_isBlock( ::sl_killInputFocus )
         eval( ::sl_killInputFocus, NIL, NIL, Self )
      ENDIF

   ENDCASE

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/
