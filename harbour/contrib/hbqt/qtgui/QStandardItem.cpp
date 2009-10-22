/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ItemType { Type, UserType }
 */

/*
 *  Constructed[ 71/79 [ 89.87% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void appendColumn ( const QList<QStandardItem *> & items )
 *  void appendRow ( const QList<QStandardItem *> & items )
 *  void appendRows ( const QList<QStandardItem *> & items )
 *  void insertColumn ( int column, const QList<QStandardItem *> & items )
 *  void insertRow ( int row, const QList<QStandardItem *> & items )
 *  void insertRows ( int row, const QList<QStandardItem *> & items )
 *  QList<QStandardItem *> takeColumn ( int column )
 *  QList<QStandardItem *> takeRow ( int row )
 */

#include <QtCore/QPointer>

#include <QtGui/QStandardItem>


/*
 * QStandardItem ()
 * QStandardItem ( const QString & text )
 * QStandardItem ( const QIcon & icon, const QString & text )
 * QStandardItem ( int rows, int columns = 1 )
 * virtual ~QStandardItem ()
 */

QT_G_FUNC( release_QStandardItem )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QStandardItem" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      delete ( ( QStandardItem * ) ph );
      ph = NULL;
   }
}

HB_FUNC( QT_QSTANDARDITEM )
{
   void * pObj = NULL;

   pObj = ( QStandardItem* ) new QStandardItem() ;

   hb_retptr( pObj );
}
/*
 * QString accessibleDescription () const
 */
HB_FUNC( QT_QSTANDARDITEM_ACCESSIBLEDESCRIPTION )
{
   hb_retc( hbqt_par_QStandardItem( 1 )->accessibleDescription().toAscii().data() );
}

/*
 * QString accessibleText () const
 */
HB_FUNC( QT_QSTANDARDITEM_ACCESSIBLETEXT )
{
   hb_retc( hbqt_par_QStandardItem( 1 )->accessibleText().toAscii().data() );
}

/*
 * void appendRow ( QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_APPENDROW )
{
   hbqt_par_QStandardItem( 1 )->appendRow( hbqt_par_QStandardItem( 2 ) );
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QSTANDARDITEM_BACKGROUND )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QStandardItem( 1 )->background() ), release_QBrush ) );
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QSTANDARDITEM_CHECKSTATE )
{
   hb_retni( ( Qt::CheckState ) hbqt_par_QStandardItem( 1 )->checkState() );
}

/*
 * QStandardItem * child ( int row, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEM_CHILD )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItem( 1 )->child( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * virtual QStandardItem * clone () const
 */
HB_FUNC( QT_QSTANDARDITEM_CLONE )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItem( 1 )->clone() );
}

/*
 * int column () const
 */
HB_FUNC( QT_QSTANDARDITEM_COLUMN )
{
   hb_retni( hbqt_par_QStandardItem( 1 )->column() );
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QSTANDARDITEM_COLUMNCOUNT )
{
   hb_retni( hbqt_par_QStandardItem( 1 )->columnCount() );
}

/*
 * virtual QVariant data ( int role = Qt::UserRole + 1 ) const
 */
HB_FUNC( QT_QSTANDARDITEM_DATA )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QVariant( hbqt_par_QStandardItem( 1 )->data( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : Qt::UserRole + 1 ) ) ), release_QVariant ) );
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QSTANDARDITEM_FLAGS )
{
   hb_retni( ( Qt::ItemFlags ) hbqt_par_QStandardItem( 1 )->flags() );
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QSTANDARDITEM_FONT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QFont( hbqt_par_QStandardItem( 1 )->font() ), release_QFont ) );
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QSTANDARDITEM_FOREGROUND )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QStandardItem( 1 )->foreground() ), release_QBrush ) );
}

/*
 * bool hasChildren () const
 */
HB_FUNC( QT_QSTANDARDITEM_HASCHILDREN )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->hasChildren() );
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QSTANDARDITEM_ICON )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QIcon( hbqt_par_QStandardItem( 1 )->icon() ), release_QIcon ) );
}

/*
 * QModelIndex index () const
 */
HB_FUNC( QT_QSTANDARDITEM_INDEX )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QModelIndex( hbqt_par_QStandardItem( 1 )->index() ), release_QModelIndex ) );
}

/*
 * void insertColumns ( int column, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTCOLUMNS )
{
   hbqt_par_QStandardItem( 1 )->insertColumns( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void insertRow ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTROW )
{
   hbqt_par_QStandardItem( 1 )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/*
 * void insertRows ( int row, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTROWS )
{
   hbqt_par_QStandardItem( 1 )->insertRows( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISCHECKABLE )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->isCheckable() );
}

/*
 * bool isDragEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISDRAGENABLED )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->isDragEnabled() );
}

/*
 * bool isDropEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISDROPENABLED )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->isDropEnabled() );
}

/*
 * bool isEditable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISEDITABLE )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->isEditable() );
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISENABLED )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->isEnabled() );
}

/*
 * bool isSelectable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISSELECTABLE )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->isSelectable() );
}

/*
 * bool isTristate () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISTRISTATE )
{
   hb_retl( hbqt_par_QStandardItem( 1 )->isTristate() );
}

/*
 * QStandardItemModel * model () const
 */
HB_FUNC( QT_QSTANDARDITEM_MODEL )
{
   hb_retptr( ( QStandardItemModel* ) hbqt_par_QStandardItem( 1 )->model() );
}

/*
 * QStandardItem * parent () const
 */
HB_FUNC( QT_QSTANDARDITEM_PARENT )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItem( 1 )->parent() );
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QSTANDARDITEM_READ )
{
   hbqt_par_QStandardItem( 1 )->read( *hbqt_par_QDataStream( 2 ) );
}

/*
 * void removeColumn ( int column )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVECOLUMN )
{
   hbqt_par_QStandardItem( 1 )->removeColumn( hb_parni( 2 ) );
}

/*
 * void removeColumns ( int column, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVECOLUMNS )
{
   hbqt_par_QStandardItem( 1 )->removeColumns( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void removeRow ( int row )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVEROW )
{
   hbqt_par_QStandardItem( 1 )->removeRow( hb_parni( 2 ) );
}

/*
 * void removeRows ( int row, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVEROWS )
{
   hbqt_par_QStandardItem( 1 )->removeRows( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * int row () const
 */
HB_FUNC( QT_QSTANDARDITEM_ROW )
{
   hb_retni( hbqt_par_QStandardItem( 1 )->row() );
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QSTANDARDITEM_ROWCOUNT )
{
   hb_retni( hbqt_par_QStandardItem( 1 )->rowCount() );
}

/*
 * void setAccessibleDescription ( const QString & accessibleDescription )
 */
HB_FUNC( QT_QSTANDARDITEM_SETACCESSIBLEDESCRIPTION )
{
   hbqt_par_QStandardItem( 1 )->setAccessibleDescription( hbqt_par_QString( 2 ) );
}

/*
 * void setAccessibleText ( const QString & accessibleText )
 */
HB_FUNC( QT_QSTANDARDITEM_SETACCESSIBLETEXT )
{
   hbqt_par_QStandardItem( 1 )->setAccessibleText( hbqt_par_QString( 2 ) );
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QSTANDARDITEM_SETBACKGROUND )
{
   hbqt_par_QStandardItem( 1 )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHECKSTATE )
{
   hbqt_par_QStandardItem( 1 )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
}

/*
 * void setCheckable ( bool checkable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHECKABLE )
{
   hbqt_par_QStandardItem( 1 )->setCheckable( hb_parl( 2 ) );
}

/*
 * void setChild ( int row, int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHILD )
{
   hbqt_par_QStandardItem( 1 )->setChild( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) );
}

/*
 * void setChild ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHILD_1 )
{
   hbqt_par_QStandardItem( 1 )->setChild( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCOLUMNCOUNT )
{
   hbqt_par_QStandardItem( 1 )->setColumnCount( hb_parni( 2 ) );
}

/*
 * virtual void setData ( const QVariant & value, int role = Qt::UserRole + 1 )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDATA )
{
   hbqt_par_QStandardItem( 1 )->setData( *hbqt_par_QVariant( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : Qt::UserRole + 1 ) );
}

/*
 * void setDragEnabled ( bool dragEnabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDRAGENABLED )
{
   hbqt_par_QStandardItem( 1 )->setDragEnabled( hb_parl( 2 ) );
}

/*
 * void setDropEnabled ( bool dropEnabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDROPENABLED )
{
   hbqt_par_QStandardItem( 1 )->setDropEnabled( hb_parl( 2 ) );
}

/*
 * void setEditable ( bool editable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETEDITABLE )
{
   hbqt_par_QStandardItem( 1 )->setEditable( hb_parl( 2 ) );
}

/*
 * void setEnabled ( bool enabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETENABLED )
{
   hbqt_par_QStandardItem( 1 )->setEnabled( hb_parl( 2 ) );
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFLAGS )
{
   hbqt_par_QStandardItem( 1 )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFONT )
{
   hbqt_par_QStandardItem( 1 )->setFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFOREGROUND )
{
   hbqt_par_QStandardItem( 1 )->setForeground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QSTANDARDITEM_SETICON )
{
   hbqt_par_QStandardItem( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QSTANDARDITEM_SETROWCOUNT )
{
   hbqt_par_QStandardItem( 1 )->setRowCount( hb_parni( 2 ) );
}

/*
 * void setSelectable ( bool selectable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSELECTABLE )
{
   hbqt_par_QStandardItem( 1 )->setSelectable( hb_parl( 2 ) );
}

/*
 * void setSizeHint ( const QSize & size )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSIZEHINT )
{
   hbqt_par_QStandardItem( 1 )->setSizeHint( *hbqt_par_QSize( 2 ) );
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSTATUSTIP )
{
   hbqt_par_QStandardItem( 1 )->setStatusTip( hbqt_par_QString( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTEXT )
{
   hbqt_par_QStandardItem( 1 )->setText( hbqt_par_QString( 2 ) );
}

/*
 * void setTextAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTEXTALIGNMENT )
{
   hbqt_par_QStandardItem( 1 )->setTextAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTOOLTIP )
{
   hbqt_par_QStandardItem( 1 )->setToolTip( hbqt_par_QString( 2 ) );
}

/*
 * void setTristate ( bool tristate )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTRISTATE )
{
   hbqt_par_QStandardItem( 1 )->setTristate( hb_parl( 2 ) );
}

/*
 * void setWhatsThis ( const QString & whatsThis )
 */
HB_FUNC( QT_QSTANDARDITEM_SETWHATSTHIS )
{
   hbqt_par_QStandardItem( 1 )->setWhatsThis( hbqt_par_QString( 2 ) );
}

/*
 * QSize sizeHint () const
 */
HB_FUNC( QT_QSTANDARDITEM_SIZEHINT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QSize( hbqt_par_QStandardItem( 1 )->sizeHint() ), release_QSize ) );
}

/*
 * void sortChildren ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QSTANDARDITEM_SORTCHILDREN )
{
   hbqt_par_QStandardItem( 1 )->sortChildren( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QSTANDARDITEM_STATUSTIP )
{
   hb_retc( hbqt_par_QStandardItem( 1 )->statusTip().toAscii().data() );
}

/*
 * QStandardItem * takeChild ( int row, int column = 0 )
 */
HB_FUNC( QT_QSTANDARDITEM_TAKECHILD )
{
   hb_retptr( ( QStandardItem* ) hbqt_par_QStandardItem( 1 )->takeChild( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QSTANDARDITEM_TEXT )
{
   hb_retc( hbqt_par_QStandardItem( 1 )->text().toAscii().data() );
}

/*
 * Qt::Alignment textAlignment () const
 */
HB_FUNC( QT_QSTANDARDITEM_TEXTALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QStandardItem( 1 )->textAlignment() );
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QSTANDARDITEM_TOOLTIP )
{
   hb_retc( hbqt_par_QStandardItem( 1 )->toolTip().toAscii().data() );
}

/*
 * virtual int type () const
 */
HB_FUNC( QT_QSTANDARDITEM_TYPE )
{
   hb_retni( hbqt_par_QStandardItem( 1 )->type() );
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QSTANDARDITEM_WHATSTHIS )
{
   hb_retc( hbqt_par_QStandardItem( 1 )->whatsThis().toAscii().data() );
}

/*
 * virtual void write ( QDataStream & out ) const
 */
HB_FUNC( QT_QSTANDARDITEM_WRITE )
{
   hbqt_par_QStandardItem( 1 )->write( *hbqt_par_QDataStream( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
