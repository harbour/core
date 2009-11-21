/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                  Xbase++ xbpTreeView compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               26Nov2008
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpListBox  INHERIT  XbpWindow, XbpDataRef

   DATA     adjustHeight                          INIT .F.
   DATA     horizScroll                           INIT .F.
   DATA     markMode                              INIT XBPLISTBOX_MM_SINGLE
   DATA     multiColumn                           INIT .F.
   DATA     vertScroll                            INIT .T.
   DATA     drawMode                              INIT XBP_DRAW_NORMAL

   DATA     oStrList
   DATA     oStrModel

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   METHOD   handleEvent()
   METHOD   exeBlock()
   METHOD   setStyle()

   METHOD   setItemsHeight( nPixel )              VIRTUAL
   METHOD   getItemHeight()                       VIRTUAL
   METHOD   getTopItem()                          VIRTUAL
   METHOD   getVisibleItems()                     VIRTUAL
   METHOD   numItems()                            VIRTUAL
   METHOD   setTopItem( nIndex )                  VIRTUAL

   METHOD   addItem( cItem )                      INLINE  ::oStrList:append( cItem ),;
                                                          ::oStrModel:setStringList( QT_PTROF( ::oStrList ) )
   METHOD   clear()                               INLINE  ::oStrList:clear(),;
                                                          ::oStrModel:setStringList( QT_PTROF( ::oStrList ) )
   METHOD   delItem( nIndex )                     INLINE  ::oStrList:removeAt( nIndex-1 ),;
                                                          ::oStrModel:setStringList( QT_PTROF( ::oStrList ) )
   METHOD   getItem( nIndex )                     INLINE  ::oStrList:at( nIndex-1 )
   METHOD   insItem( nIndex, cItem )              INLINE  ::oStrList:insert( nIndex-1, cItem ),;
                                                          ::oStrModel:setStringList( QT_PTROF( ::oStrList ) )
   METHOD   setItem( nIndex, cItem )              INLINE  ::oStrModel:replace( nIndex-1, cItem ),;
                                                          ::oStrModel:setStringList( QT_PTROF( ::oStrList ) )

   METHOD   getTabstops()                         VIRTUAL
   METHOD   setColumnWidth()                      VIRTUAL
   METHOD   setTabstops()                         VIRTUAL


   DATA     sl_hScroll
   ACCESS   hScroll                               INLINE ::sl_hScroll
   ASSIGN   hScroll( bBlock )                     INLINE ::sl_hScroll := bBlock

   DATA     sl_vScroll
   ACCESS   vScroll                               INLINE ::sl_vScroll
   ASSIGN   vScroll( bBlock )                     INLINE ::sl_vScroll := bBlock

   DATA     sl_itemMarked
   ACCESS   itemMarked                            INLINE ::sl_itemMarked
   ASSIGN   itemMarked( bBlock )                  INLINE ::sl_itemMarked := bBlock

   DATA     sl_itemSelected
   ACCESS   itemSelected                          INLINE ::sl_itemSelected
   ASSIGN   itemSelected( bBlock )                INLINE ::sl_itemSelected := bBlock

   DATA     sl_drawItem
   ACCESS   drawItem                              INLINE ::sl_drawItem
   ASSIGN   drawItem( bBlock )                    INLINE ::sl_drawItem := bBlock

   DATA     sl_measureItem
   ACCESS   measureItem                           INLINE ::sl_measureItem
   ASSIGN   measureItem( bBlock )                 INLINE ::sl_measureItem := bBlock

   DATA     nCurSelected                          INIT   0
   METHOD   getCurItem()                          INLINE ::getItem( ::nCurSelected )

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpListBox:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpListBox:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   #if 0
   IF ::horizScroll
      ::style += WS_HSCROLL
   ENDIF
   IF ::vertScroll
      ::style += WS_VSCROLL
   ENDIF
   IF ::multiColumn
      ::style += LBS_MULTICOLUMN
   ENDIF
   #endif


   ::oWidget  := QListView():New( ::pParent )

   ::Connect( ::pWidget, "clicked(QModelIndex)"      , {|o,i| ::exeBlock( 1,i,o ) } )
   ::Connect( ::pWidget, "doubleClicked(QModelIndex)", {|o,i| ::exeBlock( 2,i,o ) } )
   ::Connect( ::pWidget, "entered(QModelIndex)"      , {|o,i| ::exeBlock( 3,i,o ) } )

   ::oStrList  := QStringList():new( ::pWidget )
   ::oStrModel := QStringListModel():new( ::pWidget )
   ::oStrModel:setStringList( QT_PTROF( ::oStrList ) )
   ::oWidget:setModel( QT_PTROF( ::oStrModel ) )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF

   //::setStyle()
   ::oParent:AddChild( SELF )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpListBox:exeBlock( nMode, pModel )
   LOCAL oModel

   IF hb_isPointer( pModel )
      oModel := QModelIndex():new()
      oModel:pPtr := pModel
      ::nCurSelected := oModel:row()+1
      ::sl_editBuffer := oModel:row()+1
   ENDIF
   IF nMode == 1
      IF hb_isBlock( ::sl_itemMarked )
         eval( ::sl_itemMarked, NIL, NIL, self )
      ENDIF
   ELSEIF nMode == 2 .or. nMode == 3
      IF hb_isBlock( ::sl_itemSelected )
         eval( ::sl_itemSelected, NIL, NIL, self )
      ENDIF
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpListBox:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpListBox:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpListBox:destroy()

   ::disconnect()
   ::oStrList:pPtr := 0
   ::oStrModel:pPtr := 0

   ::oStrList := NIL
   ::oStrModel := NIL

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpListBox:setStyle()
   LOCAL s, txt_:={}

   aadd( txt_, '                                                                                             ' )
   aadd( txt_, ' QListView {                                                                                 ' )
   aadd( txt_, '     alternate-background-color: yellow;                                                     ' )
   aadd( txt_, ' }                                                                                           ' )
   aadd( txt_, '                                                                                             ' )
   aadd( txt_, ' QListView {                                                                                 ' )
   aadd( txt_, '     show-decoration-selected: 1; /* make the selection span the entire width of the view */ ' )
   aadd( txt_, ' }                                                                                           ' )
   aadd( txt_, '                                                                                             ' )
   aadd( txt_, ' QListView::item:alternate {                                                                 ' )
   aadd( txt_, '     background: #EEEEEE;                                                                    ' )
   aadd( txt_, ' }                                                                                           ' )
   aadd( txt_, '                                                                                             ' )
   aadd( txt_, ' QListView::item:selected {                                                                  ' )
   aadd( txt_, '     border: 1px solid #6a6ea9;                                                              ' )
   aadd( txt_, ' }                                                                                           ' )
   aadd( txt_, '                                                                                             ' )
   aadd( txt_, ' QListView::item:selected:!active {                                                          ' )
   aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                                 ' )
   aadd( txt_, '                                 stop: 0 #ABAFE5, stop: 1 #8588B2);                          ' )
   aadd( txt_, ' }                                                                                           ' )
   aadd( txt_, '                                                                                             ' )
   aadd( txt_, ' QListView::item:selected:active {                                                           ' )
   aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                                 ' )
   aadd( txt_, '                                 stop: 0 #6a6ea9, stop: 1 #888dd9);                          ' )
   aadd( txt_, ' }                                                                                           ' )
   aadd( txt_, '                                                                                             ' )
   aadd( txt_, ' QListView::item:hover {                                                                     ' )
   aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                                 ' )
   aadd( txt_, '                                 stop: 0 #FAFBFE, stop: 1 #DCDEF1);                          ' )
   aadd( txt_, '}                                                                                            ' )
   aadd( txt_, '                                                                                             ' )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   ::oWidget:setStyleSheet( s )

   RETURN self

/*----------------------------------------------------------------------*/
