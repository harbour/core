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
 *                  Xbase++ xbpComboBox compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               20Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

//CLASS XbpComboBox  INHERIT  XbpSLE, XbpListBox
CLASS XbpComboBox  INHERIT  XbpWindow

   DATA     type                                  INIT    XBPCOMBO_DROPDOWN
   DATA     drawMode                              INIT    XBP_DRAW_NORMAL

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) VIRTUAL
   METHOD   destroy()
   METHOD   handleEvent( nEvent, mp1, mp2 )       VIRTUAL
   METHOD   exeBlock( nMsg, p1 )

   METHOD   listBoxFocus( lFocus )                VIRTUAL      // -> lOldFocus
   METHOD   sleSize()                             VIRTUAL      // -> aOldSize

   METHOD   addItem( cItem )                      INLINE  ::oWidget:addItem( cItem )
   METHOD   setIcon( nItem,cIcon )                INLINE  ::oWidget:setItemIcon( nItem-1,cIcon )

   #if 0
   METHOD   clear()                               INLINE  ::oStrList:clear(),;
                                                          ::oStrModel:setStringList( ::oStrList )
   METHOD   delItem( nIndex )                     INLINE  ::oStrList:removeAt( nIndex-1 ),;
                                                          ::oStrModel:setStringList( ::oStrList )
   METHOD   getItem( nIndex )                     INLINE  ::oStrList:at( nIndex-1 )
   METHOD   insItem( nIndex, cItem )              INLINE  ::oStrList:insert( nIndex-1, cItem ),;
                                                          ::oStrModel:setStringList( ::oStrList )
   METHOD   setItem( nIndex, cItem )              INLINE  ::oStrModel:replace( nIndex-1, cItem ),;
                                                          ::oStrModel:setStringList( ::oStrList )
   #endif


   DATA     oSLE
   ACCESS   XbpSLE                                INLINE  ::oSLE
   DATA     oLB
   ACCESS   XbpListBox                            INLINE  ::oLB

   DATA     sl_itemMarked
   ACCESS   itemMarked                            INLINE ::sl_itemMarked
   ASSIGN   itemMarked( bBlock )                  INLINE ::sl_itemMarked := bBlock

   DATA     sl_itemSelected
   ACCESS   itemSelected                          INLINE ::sl_itemSelected
   ASSIGN   itemSelected( bBlock )                INLINE ::sl_itemSelected := bBlock

   DATA     sl_xbePDrawItem
   ACCESS   drawItem                              INLINE  ::sl_xbePDrawItem
   ASSIGN   drawItem( bBlock )                    INLINE  ::sl_xbePDrawItem := bBlock

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpComboBox:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oSLE := XbpSLE():new():create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oLB  := XbpListBox():new():create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QComboBox():New( ::pParent )

   ::oWidget:setLineEdit( ::XbpSLE:oWidget:pPtr )

   ::oWidget:setEditable( ::XbpSLE:editable )

   ::oWidget:setFrame( ::XbpSLE:border )

   ::connect( ::pWidget, "highlighted(int)"        , {|o,i| ::exeBlock( 1,i,o ) } )
   //::connect( ::pWidget, "currentIndexChanged(int)", {|o,i| ::exeBlock( 2,i,o ) } )
   ::connect( ::pWidget, "activated(int)"          , {|o,i| ::exeBlock( 2,i,o ) } )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:AddChild( SELF )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpComboBox:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      ::oWidget := QComboBox()
      ::oWidget:pPtr := pQtObject

   ELSE
      ::oSLE := XbpSLE():new():create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
      ::oLB  := XbpListBox():new():create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

      ::oWidget := QComboBox():New( ::pParent )
      ::setPosAndSize()
      IF ::visible
         ::show()
      ENDIF
      ::oWidget:setLineEdit( ::XbpSLE:oWidget:pPtr )
      ::oWidget:setEditable( ::XbpSLE:editable )
      ::oWidget:setFrame( ::XbpSLE:border )

   ENDIF

   ::connect( ::pWidget, "highlighted(int)"        , {|o,i| ::exeBlock( 1,i,o ) } )
   ::connect( ::pWidget, "activated(int)"          , {|o,i| ::exeBlock( 2,i,o ) } )
   //::connect( ::pWidget, "currentIndexChanged(int)", {|o,i| ::exeBlock( 2,i,o ) } )

   ::AddAsChild( SELF )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpComboBox:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpComboBox:exeBlock( nMsg, p1 )

   HB_SYMBOL_UNUSED( p1 )

   DO CASE
   CASE nMsg == 1
      IF hb_isBlock( ::sl_itemMarked )
         eval( ::sl_itemMarked, NIL, NIL, self )
      ENDIF
   CASE nMsg == 2
      IF hb_isBlock( ::sl_itemSelected )
         eval( ::sl_itemSelected, NIL, NIL, self )
      ENDIF
   ENDCASE

   RETURN .t.

/*----------------------------------------------------------------------*/
