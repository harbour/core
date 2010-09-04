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
 *  -----------------------------
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
      hb_retc( ( p )->accessibleDescription().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ACCESSIBLEDESCRIPTION FP=hb_retc( ( p )->accessibleDescription().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString accessibleText () const
 */
HB_FUNC( QT_QSTANDARDITEM_ACCESSIBLETEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retc( ( p )->accessibleText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ACCESSIBLETEXT FP=hb_retc( ( p )->accessibleText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void appendRow ( QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_APPENDROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->appendRow( hbqt_par_QStandardItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_APPENDROW FP=( p )->appendRow( hbqt_par_QStandardItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QSTANDARDITEM_BACKGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_BACKGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QSTANDARDITEM_CHECKSTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( Qt::CheckState ) ( p )->checkState() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_CHECKSTATE FP=hb_retni( ( Qt::CheckState ) ( p )->checkState() ); p is NULL" ) );
   }
}

/*
 * QStandardItem * child ( int row, int column = 0 ) const
 */
HB_FUNC( QT_QSTANDARDITEM_CHILD )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->child( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_CHILD FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->child( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QStandardItem * clone () const
 */
HB_FUNC( QT_QSTANDARDITEM_CLONE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->clone(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_CLONE FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->clone(), false ) ); p is NULL" ) );
   }
}

/*
 * int column () const
 */
HB_FUNC( QT_QSTANDARDITEM_COLUMN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( p )->column() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_COLUMN FP=hb_retni( ( p )->column() ); p is NULL" ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QSTANDARDITEM_COLUMNCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( p )->columnCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_COLUMNCOUNT FP=hb_retni( ( p )->columnCount() ); p is NULL" ) );
   }
}

/*
 * virtual QVariant data ( int role = Qt::UserRole + 1 ) const
 */
HB_FUNC( QT_QSTANDARDITEM_DATA )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parnidef( 2, Qt::UserRole + 1 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_DATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parnidef( 2, Qt::UserRole + 1 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QSTANDARDITEM_FLAGS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( Qt::ItemFlags ) ( p )->flags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_FLAGS FP=hb_retni( ( Qt::ItemFlags ) ( p )->flags() ); p is NULL" ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QSTANDARDITEM_FONT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QSTANDARDITEM_FOREGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_FOREGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool hasChildren () const
 */
HB_FUNC( QT_QSTANDARDITEM_HASCHILDREN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->hasChildren() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_HASCHILDREN FP=hb_retl( ( p )->hasChildren() ); p is NULL" ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QSTANDARDITEM_ICON )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) ); p is NULL" ) );
   }
}

/*
 * QModelIndex index () const
 */
HB_FUNC( QT_QSTANDARDITEM_INDEX )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_INDEX FP=hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index() ), true ) ); p is NULL" ) );
   }
}

/*
 * void insertColumns ( int column, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTCOLUMNS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->insertColumns( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_INSERTCOLUMNS FP=( p )->insertColumns( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void insertRow ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_INSERTROW FP=( p )->insertRow( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * void insertRows ( int row, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_INSERTROWS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->insertRows( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_INSERTROWS FP=( p )->insertRows( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool isCheckable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISCHECKABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->isCheckable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ISCHECKABLE FP=hb_retl( ( p )->isCheckable() ); p is NULL" ) );
   }
}

/*
 * bool isDragEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISDRAGENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->isDragEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ISDRAGENABLED FP=hb_retl( ( p )->isDragEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isDropEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISDROPENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->isDropEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ISDROPENABLED FP=hb_retl( ( p )->isDropEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isEditable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISEDITABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->isEditable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ISEDITABLE FP=hb_retl( ( p )->isEditable() ); p is NULL" ) );
   }
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ISENABLED FP=hb_retl( ( p )->isEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isSelectable () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISSELECTABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->isSelectable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ISSELECTABLE FP=hb_retl( ( p )->isSelectable() ); p is NULL" ) );
   }
}

/*
 * bool isTristate () const
 */
HB_FUNC( QT_QSTANDARDITEM_ISTRISTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retl( ( p )->isTristate() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ISTRISTATE FP=hb_retl( ( p )->isTristate() ); p is NULL" ) );
   }
}

/*
 * QStandardItemModel * model () const
 */
HB_FUNC( QT_QSTANDARDITEM_MODEL )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItemModel( ( p )->model(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_MODEL FP=hb_retptrGC( hbqt_gcAllocate_QStandardItemModel( ( p )->model(), false ) ); p is NULL" ) );
   }
}

/*
 * QStandardItem * parent () const
 */
HB_FUNC( QT_QSTANDARDITEM_PARENT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->parent(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_PARENT FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->parent(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QSTANDARDITEM_READ )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->read( *hbqt_par_QDataStream( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_READ FP=( p )->read( *hbqt_par_QDataStream( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeColumn ( int column )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVECOLUMN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->removeColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_REMOVECOLUMN FP=( p )->removeColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeColumns ( int column, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVECOLUMNS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->removeColumns( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_REMOVECOLUMNS FP=( p )->removeColumns( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void removeRow ( int row )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVEROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->removeRow( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_REMOVEROW FP=( p )->removeRow( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeRows ( int row, int count )
 */
HB_FUNC( QT_QSTANDARDITEM_REMOVEROWS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->removeRows( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_REMOVEROWS FP=( p )->removeRows( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * int row () const
 */
HB_FUNC( QT_QSTANDARDITEM_ROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( p )->row() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ROW FP=hb_retni( ( p )->row() ); p is NULL" ) );
   }
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QSTANDARDITEM_ROWCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( p )->rowCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_ROWCOUNT FP=hb_retni( ( p )->rowCount() ); p is NULL" ) );
   }
}

/*
 * void setAccessibleDescription ( const QString & accessibleDescription )
 */
HB_FUNC( QT_QSTANDARDITEM_SETACCESSIBLEDESCRIPTION )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setAccessibleDescription( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETACCESSIBLEDESCRIPTION FP=( p )->setAccessibleDescription( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAccessibleText ( const QString & accessibleText )
 */
HB_FUNC( QT_QSTANDARDITEM_SETACCESSIBLETEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setAccessibleText( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETACCESSIBLETEXT FP=( p )->setAccessibleText( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QSTANDARDITEM_SETBACKGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETBACKGROUND FP=( p )->setBackground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHECKSTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETCHECKSTATE FP=( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCheckable ( bool checkable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHECKABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setCheckable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETCHECKABLE FP=( p )->setCheckable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setChild ( int row, int column, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHILD )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setChild( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETCHILD FP=( p )->setChild( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QStandardItem( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setChild ( int row, QStandardItem * item )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCHILD_1 )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setChild( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETCHILD_1 FP=( p )->setChild( hb_parni( 2 ), hbqt_par_QStandardItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setColumnCount ( int columns )
 */
HB_FUNC( QT_QSTANDARDITEM_SETCOLUMNCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setColumnCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETCOLUMNCOUNT FP=( p )->setColumnCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setData ( const QVariant & value, int role = Qt::UserRole + 1 )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDATA )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setData( *hbqt_par_QVariant( 2 ), hb_parnidef( 3, Qt::UserRole + 1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETDATA FP=( p )->setData( *hbqt_par_QVariant( 2 ), hb_parnidef( 3, Qt::UserRole + 1 ) ); p is NULL" ) );
   }
}

/*
 * void setDragEnabled ( bool dragEnabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDRAGENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setDragEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETDRAGENABLED FP=( p )->setDragEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDropEnabled ( bool dropEnabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETDROPENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setDropEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETDROPENABLED FP=( p )->setDropEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEditable ( bool editable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETEDITABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setEditable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETEDITABLE FP=( p )->setEditable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEnabled ( bool enabled )
 */
HB_FUNC( QT_QSTANDARDITEM_SETENABLED )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETENABLED FP=( p )->setEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFLAGS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETFLAGS FP=( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFONT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QSTANDARDITEM_SETFOREGROUND )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setForeground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETFOREGROUND FP=( p )->setForeground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QSTANDARDITEM_SETICON )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETICON FP=( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setRowCount ( int rows )
 */
HB_FUNC( QT_QSTANDARDITEM_SETROWCOUNT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setRowCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETROWCOUNT FP=( p )->setRowCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelectable ( bool selectable )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSELECTABLE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setSelectable( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETSELECTABLE FP=( p )->setSelectable( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeHint ( const QSize & size )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSIZEHINT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setSizeHint( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETSIZEHINT FP=( p )->setSizeHint( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QSTANDARDITEM_SETSTATUSTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setStatusTip( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETSTATUSTIP FP=( p )->setStatusTip( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setText( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETTEXT FP=( p )->setText( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTEXTALIGNMENT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setTextAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETTEXTALIGNMENT FP=( p )->setTextAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTOOLTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setToolTip( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETTOOLTIP FP=( p )->setToolTip( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTristate ( bool tristate )
 */
HB_FUNC( QT_QSTANDARDITEM_SETTRISTATE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setTristate( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETTRISTATE FP=( p )->setTristate( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWhatsThis ( const QString & whatsThis )
 */
HB_FUNC( QT_QSTANDARDITEM_SETWHATSTHIS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->setWhatsThis( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SETWHATSTHIS FP=( p )->setWhatsThis( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSize sizeHint () const
 */
HB_FUNC( QT_QSTANDARDITEM_SIZEHINT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * void sortChildren ( int column, Qt::SortOrder order = Qt::AscendingOrder )
 */
HB_FUNC( QT_QSTANDARDITEM_SORTCHILDREN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->sortChildren( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_SORTCHILDREN FP=( p )->sortChildren( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::SortOrder ) hb_parni( 3 ) : ( Qt::SortOrder ) Qt::AscendingOrder ) ); p is NULL" ) );
   }
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QSTANDARDITEM_STATUSTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retc( ( p )->statusTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_STATUSTIP FP=hb_retc( ( p )->statusTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStandardItem * takeChild ( int row, int column = 0 )
 */
HB_FUNC( QT_QSTANDARDITEM_TAKECHILD )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeChild( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_TAKECHILD FP=hb_retptrGC( hbqt_gcAllocate_QStandardItem( ( p )->takeChild( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QList<QStandardItem *> takeColumn ( int column )
 */
HB_FUNC( QT_QSTANDARDITEM_TAKECOLUMN )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeColumn( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_TAKECOLUMN FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeColumn( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QStandardItem *> takeRow ( int row )
 */
HB_FUNC( QT_QSTANDARDITEM_TAKEROW )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeRow( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_TAKEROW FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QStandardItem *>( ( p )->takeRow( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QSTANDARDITEM_TEXT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Qt::Alignment textAlignment () const
 */
HB_FUNC( QT_QSTANDARDITEM_TEXTALIGNMENT )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->textAlignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_TEXTALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->textAlignment() ); p is NULL" ) );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QSTANDARDITEM_TOOLTIP )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retc( ( p )->toolTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_TOOLTIP FP=hb_retc( ( p )->toolTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual int type () const
 */
HB_FUNC( QT_QSTANDARDITEM_TYPE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retni( ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_TYPE FP=hb_retni( ( p )->type() ); p is NULL" ) );
   }
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QSTANDARDITEM_WHATSTHIS )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      hb_retc( ( p )->whatsThis().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_WHATSTHIS FP=hb_retc( ( p )->whatsThis().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual void write ( QDataStream & out ) const
 */
HB_FUNC( QT_QSTANDARDITEM_WRITE )
{
   QStandardItem * p = hbqt_par_QStandardItem( 1 );
   if( p )
      ( p )->write( *hbqt_par_QDataStream( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTANDARDITEM_WRITE FP=( p )->write( *hbqt_par_QDataStream( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
