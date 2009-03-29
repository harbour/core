/*
 * $Id$
 */
   
/* 
 * Harbour Project source code:
 * QT wrapper main header
 * 
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500



#include <QtGui/QTreeWidgetItem>




/*
 * QTreeWidgetItem ( int type = Type )
 * QTreeWidgetItem ( const QStringList & strings, int type = Type )
 * QTreeWidgetItem ( QTreeWidget * parent, int type = Type )
 * QTreeWidgetItem ( QTreeWidget * parent, const QStringList & strings, int type = Type )
 * QTreeWidgetItem ( QTreeWidget * parent, QTreeWidgetItem * preceding, int type = Type )
 * QTreeWidgetItem ( QTreeWidgetItem * parent, int type = Type )
 * QTreeWidgetItem ( QTreeWidgetItem * parent, const QStringList & strings, int type = Type )
 * QTreeWidgetItem ( QTreeWidgetItem * parent, QTreeWidgetItem * preceding, int type = Type )
 * QTreeWidgetItem ( const QTreeWidgetItem & other )
 * virtual ~QTreeWidgetItem ()
 */
HB_FUNC( QT_QTREEWIDGETITEM )
{
   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
   {
      hb_retptr( ( QTreeWidgetItem* ) new QTreeWidgetItem( hb_parni( 1 ) ) );
   }
   else
   {
      hb_retptr( ( QTreeWidgetItem* ) new QTreeWidgetItem( hbqt_par_QTreeWidget( 1 ), hb_parni( 2 ) ) );
   }
}


/*
 * void addChild ( QTreeWidgetItem * child )
 */
HB_FUNC( QT_QTREEWIDGETITEM_ADDCHILD )
{
   hbqt_par_QTreeWidgetItem( 1 )->addChild( hbqt_par_QTreeWidgetItem( 1 ) );
}

/*
 * Qt::CheckState checkState ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHECKSTATE )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->checkState( hb_parni( 1 ) ) );
}

/*
 * QTreeWidgetItem * child ( int index ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHILD )
{
   hb_retptr( ( QTreeWidgetItem* ) hbqt_par_QTreeWidgetItem( 1 )->child( hb_parni( 1 ) ) );
}

/*
 * int childCount () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHILDCOUNT )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->childCount(  ) );
}

/*
 * QTreeWidgetItem::ChildIndicatorPolicy childIndicatorPolicy () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHILDINDICATORPOLICY )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->childIndicatorPolicy(  ) );
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_COLUMNCOUNT )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->columnCount(  ) );
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_FLAGS )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->flags(  ) );
}

/*
 * int indexOfChild ( QTreeWidgetItem * child ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_INDEXOFCHILD )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->indexOfChild( hbqt_par_QTreeWidgetItem( 1 ) ) );
}

/*
 * void insertChild ( int index, QTreeWidgetItem * child )
 */
HB_FUNC( QT_QTREEWIDGETITEM_INSERTCHILD )
{
   hbqt_par_QTreeWidgetItem( 1 )->insertChild( hb_parni( 1 ), hbqt_par_QTreeWidgetItem( 2 ) );
}

/*
 * bool isDisabled () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISDISABLED )
{
   hb_retl( hbqt_par_QTreeWidgetItem( 1 )->isDisabled(  ) );
}

/*
 * bool isExpanded () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISEXPANDED )
{
   hb_retl( hbqt_par_QTreeWidgetItem( 1 )->isExpanded(  ) );
}

/*
 * bool isFirstColumnSpanned () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISFIRSTCOLUMNSPANNED )
{
   hb_retl( hbqt_par_QTreeWidgetItem( 1 )->isFirstColumnSpanned(  ) );
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISHIDDEN )
{
   hb_retl( hbqt_par_QTreeWidgetItem( 1 )->isHidden(  ) );
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISSELECTED )
{
   hb_retl( hbqt_par_QTreeWidgetItem( 1 )->isSelected(  ) );
}

/*
 * QTreeWidgetItem * parent () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_PARENT )
{
   hb_retptr( ( QTreeWidgetItem* ) hbqt_par_QTreeWidgetItem( 1 )->parent(  ) );
}

/*
 * void removeChild ( QTreeWidgetItem * child )
 */
HB_FUNC( QT_QTREEWIDGETITEM_REMOVECHILD )
{
   hbqt_par_QTreeWidgetItem( 1 )->removeChild( hbqt_par_QTreeWidgetItem( 1 ) );
}

/*
 * void setCheckState ( int column, Qt::CheckState state )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETCHECKSTATE )
{
   hbqt_par_QTreeWidgetItem( 1 )->setCheckState( hb_parni( 1 ), ( Qt::CheckState ) hb_parni( 2 ) );
}

/*
 * void setChildIndicatorPolicy ( QTreeWidgetItem::ChildIndicatorPolicy policy )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETCHILDINDICATORPOLICY )
{
   hbqt_par_QTreeWidgetItem( 1 )->setChildIndicatorPolicy( ( QTreeWidgetItem::ChildIndicatorPolicy ) hb_parni( 1 ) );
}

/*
 * void setDisabled ( bool disabled )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETDISABLED )
{
   hbqt_par_QTreeWidgetItem( 1 )->setDisabled( hb_parl( 1 ) );
}

/*
 * void setExpanded ( bool expand )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETEXPANDED )
{
   hbqt_par_QTreeWidgetItem( 1 )->setExpanded( hb_parl( 1 ) );
}

/*
 * void setFirstColumnSpanned ( bool span )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETFIRSTCOLUMNSPANNED )
{
   hbqt_par_QTreeWidgetItem( 1 )->setFirstColumnSpanned( hb_parl( 1 ) );
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETFLAGS )
{
   hbqt_par_QTreeWidgetItem( 1 )->setFlags( ( Qt::ItemFlags ) hb_parni( 1 ) );
}

/*
 * void setHidden ( bool hide )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETHIDDEN )
{
   hbqt_par_QTreeWidgetItem( 1 )->setHidden( hb_parl( 1 ) );
}

/*
 * void setIcon ( int column, const QIcon & icon )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETICON )
{
   hbqt_par_QTreeWidgetItem( 1 )->setIcon( hb_parni( 1 ), QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setSelected ( bool select )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETSELECTED )
{
   hbqt_par_QTreeWidgetItem( 1 )->setSelected( hb_parl( 1 ) );
}

/*
 * void setStatusTip ( int column, const QString & statusTip )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETSTATUSTIP )
{
   hbqt_par_QTreeWidgetItem( 1 )->setStatusTip( hb_parni( 1 ), hbqt_par_QString( 2 ) );
}

/*
 * void setText ( int column, const QString & text )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETTEXT )
{
   hbqt_par_QTreeWidgetItem( 1 )->setText( hb_parni( 1 ), hbqt_par_QString( 2 ) );
}

/*
 * void setTextAlignment ( int column, int alignment )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETTEXTALIGNMENT )
{
   hbqt_par_QTreeWidgetItem( 1 )->setTextAlignment( hb_parni( 1 ), hb_parni( 2 ) );
}

/*
 * void setToolTip ( int column, const QString & toolTip )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETTOOLTIP )
{
   hbqt_par_QTreeWidgetItem( 1 )->setToolTip( hb_parni( 1 ), hbqt_par_QString( 2 ) );
}

/*
 * void setWhatsThis ( int column, const QString & whatsThis )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETWHATSTHIS )
{
   hbqt_par_QTreeWidgetItem( 1 )->setWhatsThis( hb_parni( 1 ), hbqt_par_QString( 2 ) );
}

/*
 * void sortChildren ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SORTCHILDREN )
{
   hbqt_par_QTreeWidgetItem( 1 )->sortChildren( hb_parni( 1 ), ( Qt::SortOrder ) hb_parni( 2 ) );
}

/*
 * QString statusTip ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_STATUSTIP )
{
   hb_retc( hbqt_par_QTreeWidgetItem( 1 )->statusTip( hb_parni( 1 )).toLatin1().data() );
}

/*
 * QTreeWidgetItem * takeChild ( int index )
 */
HB_FUNC( QT_QTREEWIDGETITEM_TAKECHILD )
{
   hb_retptr( ( QTreeWidgetItem* ) hbqt_par_QTreeWidgetItem( 1 )->takeChild( hb_parni( 1 ) ) );
}

/*
 * QString text ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TEXT )
{
   hb_retc( hbqt_par_QTreeWidgetItem( 1 )->text( hb_parni( 1 )).toLatin1().data() );
}

/*
 * int textAlignment ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TEXTALIGNMENT )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->textAlignment( hb_parni( 1 ) ) );
}

/*
 * QString toolTip ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TOOLTIP )
{
   hb_retc( hbqt_par_QTreeWidgetItem( 1 )->toolTip( hb_parni( 1 )).toLatin1().data() );
}

/*
 * QTreeWidget * treeWidget () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TREEWIDGET )
{
   hb_retptr( ( QTreeWidget* ) hbqt_par_QTreeWidgetItem( 1 )->treeWidget(  ) );
}

/*
 * int type () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TYPE )
{
   hb_retni( hbqt_par_QTreeWidgetItem( 1 )->type(  ) );
}

/*
 * QString whatsThis ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_WHATSTHIS )
{
   hb_retc( hbqt_par_QTreeWidgetItem( 1 )->whatsThis( hb_parni( 1 )).toLatin1().data() );
}



#endif
/*----------------------------------------------------------------------*/

