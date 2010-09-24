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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ItemType { Type, UserType }
 */

/*
 *  Constructed[ 73/79 [ 92.41% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void appendColumn ( const QList<QStandardItem *> & items )
 *  void appendRow ( const QList<QStandardItem *> & items )
 *  void appendRows ( const QList<QStandardItem *> & items )
 *  void insertColumn ( int column, const QList<QStandardItem *> & items )
 *  void insertRow ( int row, const QList<QStandardItem *> & items )
 *  void insertRows ( int row, const QList<QStandardItem *> & items )
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

typedef struct
{
   QStandardItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStandardItem;

HBQT_GC_FUNC( hbqt_gcRelease_QStandardItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStandardItem   /.\\", p->ph ) );
         delete ( ( QStandardItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStandardItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStandardItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStandardItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStandardItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStandardItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStandardItem;
   p->type = HBQT_TYPE_QStandardItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStandardItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStandardItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTANDARDITEM )
{
   QStandardItem * pObj = NULL;

   pObj =  new QStandardItem() ;

   hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( void * ) pObj, true ) );
}

/*
 * QString accessibleDescription () const
 */
HB_FUNC( QT_QSTANDARDITEM_ACCESSIBLEDESCRIPTION )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->accessibleDescription().toUtf8().data() );
   }
}

/*
 * QString accessibleText () const
 */
HB_FUNC( QT_QSTANDARDITEM_ACCESSIBLETEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->accessibleText().toUtf8().data() );
   }
}

/*
 * void appendRow ( QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_APPENDROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->appendRow( hbqt_par_QStandardItem( 2 ) );
   }
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QSTANDARDITEM_BACKGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   }
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QSTANDARDITEM_CHECKSTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( Qt::CheckState ) ( p )->checkState() );
   }
}

/*
 * QStandardItem * child ( int row, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEM_CHILD )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->child( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * virtual QStandardItem * clone () const
 */
HB_FUNC( QT_QSTANDARDITEM_CLONE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->clone(), false ) );
   }
}

/*
 * int column () const
 */
HB_FUNC( QT_QSTANDARDITEM_COLUMN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( p )->column() );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QSTANDARDITEM_COLUMNCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( p )->columnCount() );
   }
}

/*
 * virtual QVariant data ( int role = Qt::UserRole + 1 ) const
 */
HB_FUNC( QT_QSTANDARDITEM_DATA )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parnidef( 2, Qt::UserRole + 1 ) ) ), true ) );
   }
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QSTANDARDITEM_FLAGS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( Qt::ItemFlags ) ( p )->flags() );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QSTANDARDITEM_FONT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   }
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QSTANDARDITEM_FOREGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) );
   }
}

/*
 * bool hasChildren () const
 */
HB_FUNC( QT_QSTANDARDITEM_HASCHILDREN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->hasChildren() );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QSTANDARDITEM_ICON )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   }
}

/*
 * QModelIndex index () const
 */
HB_FUNC( QT_QSTANDARDITEM_INDEX )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index() ), true ) );
   }
}

/*
 * void insertColumns ( int column, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTCOLUMNS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->insertColumns( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void insertRow ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   }
}

/*
 * void insertRows ( int row, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTROWS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->insertRows( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISCHECKABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isCheckable() );
   }
}

/*
 * bool isDragEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISDRAGENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isDragEnabled() );
   }
}

/*
 * bool isDropEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISDROPENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isDropEnabled() );
   }
}

/*
 * bool isEditable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISEDITABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isEditable() );
   }
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isEnabled() );
   }
}

/*
 * bool isSelectable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISSELECTABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isSelectable() );
   }
}

/*
 * bool isTristate () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISTRISTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retl( ( p )->isTristate() );
   }
}

/*
 * QStandardItemModel * model () const
 */
HB_FUNC( QT_QSTANDARDITEM_MODEL )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItemModel( ( p )->model(), false ) );
   }
}

/*
 * QStandardItem * parent () const
 */
HB_FUNC( QT_QSTANDARDITEM_PARENT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->parent(), false ) );
   }
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QSTANDARDITEM_READ )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->read( *hbqt_par_QDataStream( 2 ) );
   }
}

/*
 * void removeColumn ( int column )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVECOLUMN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->removeColumn( hb_parni( 2 ) );
   }
}

/*
 * void removeColumns ( int column, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVECOLUMNS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->removeColumns( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void removeRow ( int row )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVEROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->removeRow( hb_parni( 2 ) );
   }
}

/*
 * void removeRows ( int row, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVEROWS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->removeRows( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * int row () const
 */
HB_FUNC( QT_QSTANDARDITEM_ROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( p )->row() );
   }
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QSTANDARDITEM_ROWCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( p )->rowCount() );
   }
}

/*
 * void setAccessibleDescription ( const QString & accessibleDescription )
 */
HB_FUNC( QT_QSTANDARDITEM_SETACCESSIBLEDESCRIPTION )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setAccessibleDescription( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setAccessibleText ( const QString & accessibleText )
 */
HB_FUNC( QT_QSTANDARDITEM_SETACCESSIBLETEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setAccessibleText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QSTANDARDITEM_SETBACKGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   }
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHECKSTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
   }
}

/*
 * void setCheckable ( bool checkable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHECKABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setCheckable( hb_parl( 2 ) );
   }
}

/*
 * void setChild ( int row, int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHILD )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setChild( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) );
   }
}

/*
 * void setChild ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHILD_1 )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setChild( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCOLUMNCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setColumnCount( hb_parni( 2 ) );
   }
}

/*
 * virtual void setData ( const QVariant & value, int role = Qt::UserRole + 1 )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDATA )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setData( *hbqt_par_QVariant( 2 ), hb_parnidef( 3, Qt::UserRole + 1 ) );
   }
}

/*
 * void setDragEnabled ( bool dragEnabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDRAGENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setDragEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setDropEnabled ( bool dropEnabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDROPENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setDropEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setEditable ( bool editable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETEDITABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setEditable( hb_parl( 2 ) );
   }
}

/*
 * void setEnabled ( bool enabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFLAGS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFONT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   }
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFOREGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setForeground( *hbqt_par_QBrush( 2 ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QSTANDARDITEM_SETICON )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
   }
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QSTANDARDITEM_SETROWCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setRowCount( hb_parni( 2 ) );
   }
}

/*
 * void setSelectable ( bool selectable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSELECTABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setSelectable( hb_parl( 2 ) );
   }
}

/*
 * void setSizeHint ( const QSize & size )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSIZEHINT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setSizeHint( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSTATUSTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStatusTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setTextAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTEXTALIGNMENT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setTextAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   }
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTOOLTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setTristate ( bool tristate )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTRISTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->setTristate( hb_parl( 2 ) );
   }
}

/*
 * void setWhatsThis ( const QString & whatsThis )
 */
HB_FUNC( QT_QSTANDARDITEM_SETWHATSTHIS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWhatsThis( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QSize sizeHint () const
 */
HB_FUNC( QT_QSTANDARDITEM_SIZEHINT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   }
}

/*
 * void sortChildren ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QSTANDARDITEM_SORTCHILDREN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->sortChildren( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
   }
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QSTANDARDITEM_STATUSTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->statusTip().toUtf8().data() );
   }
}

/*
 * QStandardItem * takeChild ( int row, int column = 0 )
 */
HB_FUNC( QT_QSTANDARDITEM_TAKECHILD )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeChild( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * QList<QStandardItem *> takeColumn ( int column )
 */
HB_FUNC( QT_QSTANDARDITEM_TAKECOLUMN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeColumn( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QList<QStandardItem *> takeRow ( int row )
 */
HB_FUNC( QT_QSTANDARDITEM_TAKEROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeRow( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QSTANDARDITEM_TEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
   }
}

/*
 * Qt::Alignment textAlignment () const
 */
HB_FUNC( QT_QSTANDARDITEM_TEXTALIGNMENT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->textAlignment() );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QSTANDARDITEM_TOOLTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
   }
}

/*
 * virtual int type () const
 */
HB_FUNC( QT_QSTANDARDITEM_TYPE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retni( ( p )->type() );
   }
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QSTANDARDITEM_WHATSTHIS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->whatsThis().toUtf8().data() );
   }
}

/*
 * virtual void write ( QDataStream & out ) const
 */
HB_FUNC( QT_QSTANDARDITEM_WRITE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
   {
      ( p )->write( *hbqt_par_QDataStream( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
