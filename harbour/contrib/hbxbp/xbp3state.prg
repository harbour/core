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
 *                  Xbase++ xbp3State Compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               14Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS Xbp3State  INHERIT  XbpWindow, XbpDataRef

   DATA     autosize                              INIT .F.
   DATA     caption                               INIT ""
   DATA     pointerFocus                          INIT .T.
   DATA     selection                             INIT .F.

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nEvent, mp1, mp2 )

   METHOD   setCaption( xCaption )

   ACCESS   selected                              INLINE ::sl_lbClick
   ASSIGN   selected( bBlock )                    INLINE ::sl_lbClick := bBlock

   METHOD   exeBlock( iState )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD Xbp3State:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Xbp3State:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QCheckBox():New( ::oParent:oWidget )

   ::Connect( ::pWidget, "stateChanged(int)", {|o,i| ::exeBlock( i,o ) } )

   ::oWidget:setTriState( .t. )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   ::setCaption( ::caption )

   IF ::selection
      ::oWidget:setCheckState( 2 )
   ENDIF

   IF ::visible
      ::show()
   ENDIF

   ::editBuffer := ::oWidget:checkState()

   ::oParent:AddChild( SELF )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Xbp3State:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      ::oWidget := QCheckBox()
      ::oWidget:pPtr := pQtObject

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Xbp3State:exeBlock( iState )

   ::sl_editBuffer := IF( iState == 2, 1, IF( iState == 1, 2, iState ) )

   IF hb_isBlock( ::sl_lbClick )
      eval( ::sl_lbClick, ::sl_editBuffer, NIL, self )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD Xbp3State:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN 0

/*----------------------------------------------------------------------*/

METHOD Xbp3State:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD Xbp3State:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Xbp3State:setCaption( xCaption )

   IF hb_isChar( xCaption )
      ::caption := xCaption
      ::oWidget:setText( xCaption )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
