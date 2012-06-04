/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                  Xbase++ XbpSpinButton compatible Class
 *
 *                             Pritpal Bedi
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

CLASS XbpSpinButton INHERIT XbpWindow, DataRef

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

   DATA     nOldValue                             INIT    0

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) VIRTUAL
   METHOD   destroy()
   METHOD   connect()
   METHOD   disconnect()
   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   execSlot( cSlot, p )

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
   DATA     sl_xbeSpinUp
   DATA     sl_xbeSpinEndSpin

   METHOD   down( ... )                           SETGET
   METHOD   up( ... )                             SETGET
   METHOD   endSpin( ... )                        SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL es_:= { Qt_AlignLeft, Qt_AlignRight, Qt_AlignHCenter }

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QSpinBox( ::pParent )
   ::oWidget:setKeyboardTracking( .t. )
   IF ::fastSpin
      ::oWidget:setAccelerated( .t. )
   ENDIF
   ::oWidget:setReadOnly( ! ::editable )
   ::oWidget:setFrame( ::border )
   ::oWidget:setAlignment( es_[ ::align ] )

   ::connect()
   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:addChild( Self )
   ::postCreate()

   ::setData()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:execSlot( cSlot, p )

   HB_SYMBOL_UNUSED( p )

   DO CASE
   CASE cSlot == "valueChanged(int)"
      ::sl_editBuffer := ::oWidget:value()
      IF p < ::nOldValue
         ::down()
      ELSEIF p > ::nOldValue
         ::up()
      ENDIF
      ::nOldValue := ::sl_editBuffer

   CASE cSlot == "QEvent_KeyPress"
      ::keyboard()

   CASE cSlot == "QEvent_FocusIn"
      ::setInputFocus()

   CASE cSlot == "QEvent_FocusOut"
      ::killInputFocus()

   ENDCASE

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:connect()

   #if 0
   ::oWidget:connect( QEvent_FocusIn , {|| ::execSlot( "QEvent_FocusIn"  ) } )
   ::oWidget:connect( QEvent_FocusOut, {|| ::execSlot( "QEvent_FocusOut" ) } )
   ::oWidget:connect( QEvent_KeyPress, {|| ::execSlot( "QEvent_KeyPress" ) } )
   #endif

   ::oWidget:connect( "valueChanged(int)" , {|i| ::execSlot( "valueChanged(int)", i ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:disconnect()

   #if 0
   ::oWidget:connect( QEvent_FocusIn  )
   ::oWidget:connect( QEvent_FocusOut )
   ::oWidget:connect( QEvent_KeyPress )
   #endif

   ::oWidget:disconnect( "valueChanged(int)" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:destroy()

   ::disconnect()
   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:down( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_xbeSpinDown := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_xbeSpinDown )
      eval( ::sl_xbeSpinDown, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:up( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_xbeSpinUp := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_xbeSpinUp )
      eval( ::sl_xbeSpinUp, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSpinButton:endSpin( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. HB_ISBLOCK( a_[ 1 ] )
      ::sl_xbeSpinEndSpin := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. HB_ISBLOCK( ::sl_xbeSpinEndSpin )
      eval( ::sl_xbeSpinEndSpin, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
