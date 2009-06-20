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
#include "apig.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpSLE INHERIT XbpWindow, XbpDataRef

   DATA     align                                 INIT XBPSLE_LEFT
   DATA     autoKeyboard                          INIT .T.
   DATA     autoSize                              INIT .F.
   DATA     autoTab                               INIT .F.
   DATA     border                                INIT .T.
   DATA     bufferLength                          INIT 32
   DATA     editable                              INIT .T.
   DATA     unReadable                            INIT .F.

   DATA     changed                               INIT .F.

   METHOD   new()
   METHOD   create()
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()
   METHOD   handleEvent()
   METHOD   exeBlock()

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

   METHOD   setInsertMode( lInsertMode )          VIRTUAL

   DATA     sl_hScroll
   ACCESS   hScroll                               INLINE  ::sl_hScroll
   ASSIGN   hScroll( bBlock )                     INLINE  ::sl_hScroll := bBlock

   DATA     sl_typeOut
   ACCESS   typeOut                               INLINE  ::sl_typeOut
   ASSIGN   typeOut( bBlock )                     INLINE  ::sl_typeOut := bBlock

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpSLE:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::className   := "XBPSLE"
   ::objType     := objTypeSLE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpSLE:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL es_:= { Qt_AlignLeft, Qt_AlignRight, Qt_AlignHCenter }

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QLineEdit():new( ::pParent )

   ::oWidget:setAlignment( es_[ ::align ] )
   IF !::editable
      ::oWidget:setReadOnly( .t. )
   ENDIF

   IF ::unReadable
      ::oWidget:setEchoMode( QLineEdit_Password )
   ENDIF

   ::oWidget:setFrame( ::border )
   ::oWidget:setMaxLength( ::bufferLength )

   QT_QObject_InstallEventFilter( ::pWidget, SetEventFilter() )

   ::connectEvent( ::pWidget, QEvent_FocusIn , {|o,e| ::exeBlock( 7, e, o ) } )
   ::connectEvent( ::pWidget, QEvent_FocusOut, {|o,e| ::exeBlock( 8, e, o ) } )

   ::connect( ::pWidget, "cursorPositionChanged(int,int)" , {|o,i,ii| ::exeBlock( 1, i, ii, o ) } )
   //::connect( ::pWidget, "editingFinished()"              , {|      | ::exeBlock( 2 ) } )
   //::connect( ::pWidget, "returnPressed()"                , {|      | ::exeBlock( 3 ) } )
   //::connect( ::pWidget, "selectionChanged()"             , {|      | ::exeBlock( 4 ) } )
   ::connect( ::pWidget, "textChanged(QString)"           , {|o,s   | ::exeBlock( 5, s, o ) } )
   ::connect( ::pWidget, "textEdited(QString)"            , {|o,s   | ::exeBlock( 6, s, o ) } )

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

METHOD XbpSLE:exeBlock( nMsg, p1, p2 )

   HB_SYMBOL_UNUSED( p1 )

   DO CASE
   CASE nMsg == 1    // "cursorPositionChanged(int,int)"
      IF hb_isBlock( ::sl_hScroll )
         eval( ::sl_hScroll, NIL, NIL, self )
      ENDIF
      IF p2 == ::bufferLength
         IF hb_isBlock( ::sl_typeOut )
            eval( ::sl_typeOut, NIL, NIL, Self )
         ENDIF
      ENDIF

   CASE nMsg == 2    // "editingFinished()"

   CASE nMsg == 3    // "returnPressed()"
      ::sl_editBuffer := ::oWidget:text()

   CASE nMsg == 4    // "selectionChanged()"

   CASE nMsg == 5    // "textEdited(QString)"
      ::changed := .t.

   CASE nMsg == 6    // "textEdited(QString)"
      ::changed := .t.

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

METHOD XbpSLE:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpSLE:destroy()
   ::xbpWindow:destroy()
   RETURN NIL

/*----------------------------------------------------------------------*/
