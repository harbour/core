/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                    Xbase++ xbpSLE compatible Class
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

CLASS XbpSLE INHERIT XbpWindow, DataRef

   DATA     align                                 INIT XBPSLE_LEFT
   DATA     autoKeyboard                          INIT .T.
   DATA     autoSize                              INIT .F.
   DATA     autoTab                               INIT .F.
   DATA     border                                INIT .T.
   DATA     bufferLength                          INIT 32
   DATA     editable                              INIT .T.
   DATA     unReadable                            INIT .F.

   DATA     changed                               INIT .F.

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) VIRTUAL
   METHOD   destroy()
   METHOD   handleEvent( nEvent, mp1, mp2 )
   METHOD   execSlot( cSlot, p, p2 )
   METHOD   connect()
   METHOD   disconnect()

   METHOD   clear()                               INLINE  ::oWidget:clear()
   METHOD   copyMarked()                          INLINE  ::oWidget:copy()
   METHOD   cutMarked()                           INLINE  ::oWidget:cut()
   METHOD   delMarked()                           INLINE  ::oWidget:del()
   METHOD   editBuffer()                          INLINE  ::oWidget:text()
   METHOD   pasteMarked()                         INLINE  ::oWidget:paste()
   METHOD   queryFirstChar()                      VIRTUAL
   METHOD   queryMarked()                         INLINE  { ::oWidget:selectionStart(), ::oWidget:selectionEnd() }
   METHOD   setFirstChar( nPos )                  VIRTUAL
   METHOD   setMarked( aStartEnd )                INLINE  ::oWidget:setSelection( aStartEnd[ 1 ], aStartEnd[ 2 ] )

   METHOD   setInsertMode( lInsertMode )          VIRTUAL

   DATA     sl_typeOut
   DATA     sl_hScroll
   DATA     sl_returnPressed

   METHOD   typeOut( ... )                        SETGET
   METHOD   hScroll( ... )                        SETGET
   METHOD   returnPressed( ... )                  SETGET

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpSLE:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL es_:= { Qt_AlignLeft, Qt_AlignRight, Qt_AlignHCenter }

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QLineEdit( ::pParent )
   ::oWidget:setFocusPolicy( Qt_StrongFocus )

   ::oWidget:setAlignment( es_[ ::align ] )
   IF !::editable
      ::oWidget:setReadOnly( .t. )
   ENDIF

   IF ::unReadable
      ::oWidget:setEchoMode( QLineEdit_Password )
   ENDIF

   ::oWidget:setFrame( ::border )
   ::oWidget:setMaxLength( ::bufferLength )

   ::connect()
   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   IF hb_isBlock( ::datalink )
      eval( ::datalink )
   ENDIF

   ::oParent:addChild( Self )
   ::postCreate()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      ::oWidget := QLineEditFromPointer( pQtObject )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:connect()
   #if 0
   ::oWidget:connect( QEvent_FocusIn                   , {|e| ::execSlot( "QEvent_FocusIn" , e ) } )
   ::oWidget:connect( QEvent_FocusOut                  , {|e| ::execSlot( "QEvent_FocusOut", e ) } )
   #endif

   ::oWidget:connect( "cursorPositionChanged(int,int)" , {|i,ii| ::execSlot( "cursorPositionChanged(int,int)", i, ii ) } )
*  ::oWidget:connect( "editingFinished()"              , {|    | ::execSlot( "editingFinished()"       ) } )
   ::oWidget:connect( "returnPressed()"                , {|    | ::execSlot( "returnPressed()"         ) } )
*  ::oWidget:connect( "selectionChanged()"             , {|    | ::execSlot( "selectionChanged()"      ) } )
   ::oWidget:connect( "textChanged(QString)"           , {|s   | ::execSlot( "textChanged(QString)", s ) } )
   ::oWidget:connect( "textEdited(QString)"            , {|s   | ::execSlot( "textEdited(QString)" , s ) } )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:disconnect()
   #if 0
   ::oWidget:disconnect( QEvent_FocusIn                   )
   ::oWidget:disconnect( QEvent_FocusOut                  )
   #endif
   ::oWidget:disconnect( "cursorPositionChanged(int,int)" )
*  ::oWidget:disconnect( "editingFinished()"              )
   ::oWidget:disconnect( "returnPressed()"                )
*  ::oWidget:disconnect( "selectionChanged()"             )
   ::oWidget:disconnect( "textChanged(QString)"           )
   ::oWidget:disconnect( "textEdited(QString)"            )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:destroy()

   ::disconnect()
   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpSLE:execSlot( cSlot, p, p2 )

   HB_SYMBOL_UNUSED( p )

   DO CASE
   CASE cSlot == "cursorPositionChanged(int,int)"
      ::hScroll()
      IF p2 == ::bufferLength
         ::typeOut()
      ENDIF

   CASE cSlot == "editingFinished()"

   CASE cSlot == "returnPressed()"
      ::sl_editBuffer := ::oWidget:text()
      ::returnPressed()

   CASE cSlot == "selectionChanged()"

   CASE cSlot == "textChanged(QString)"
      ::changed := .t.

   CASE cSlot == "textEdited(QString)"
      ::changed := .t.

   CASE cSlot == "QEvent_FocusIn"
      ::setInputFocus()

   CASE cSlot == "QEvent_FocusOut"
      ::killInputFocus()

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpSLE:returnPressed( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_returnPressed := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_returnPressed )
      eval( ::sl_returnPressed, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:hScroll( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_hScroll := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_hScroll )
      eval( ::sl_hScroll, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:typeOut( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_typeOut := a_[ 1 ]
   ELSEIF len( a_ ) >= 0 .AND. hb_isBlock( ::sl_typeOut )
      eval( ::sl_typeOut, NIL, NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
