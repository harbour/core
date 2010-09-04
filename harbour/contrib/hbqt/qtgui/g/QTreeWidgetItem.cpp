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
 *  enum ChildIndicatorPolicy { ShowIndicator, DontShowIndicator, DontShowIndicatorWhenChildless }
 *  enum ItemType { Type, UserType }
 */

/*
 *  Constructed[ 53/56 [ 94.64% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *  void addChildren ( const QList<QTreeWidgetItem *> & children )
 *  void insertChildren ( int index, const QList<QTreeWidgetItem *> & children )
 */

#include <QtCore/QPointer>

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

typedef struct
{
   QTreeWidgetItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTreeWidgetItem;

HBQT_GC_FUNC( hbqt_gcRelease_QTreeWidgetItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTreeWidgetItem   /.\\", p->ph ) );
         delete ( ( QTreeWidgetItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTreeWidgetItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTreeWidgetItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTreeWidgetItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTreeWidgetItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTreeWidgetItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTreeWidgetItem;
   p->type = HBQT_TYPE_QTreeWidgetItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTreeWidgetItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTreeWidgetItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTREEWIDGETITEM )
{
   QTreeWidgetItem * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
   {
      pObj =  new QTreeWidgetItem( hb_parni( 1 ) ) ;
   }
   else
   {
      pObj =  new QTreeWidgetItem( hbqt_par_QTreeWidget( 1 ), hb_parni( 2 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( void * ) pObj, true ) );
}

/*
 * void addChild ( QTreeWidgetItem * child )
 */
HB_FUNC( QT_QTREEWIDGETITEM_ADDCHILD )
{
   HBQT_GC_T_QTreeWidgetItem * q = ( HBQT_GC_T_QTreeWidgetItem * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );

   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QTREEWIDGETITEM_ADDCHILD()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QTOOLBAR_ADDACTION() Qt oject: %p is attached to: %p", ( void * ) p->ph, ( void * ) q->ph ) );
      p->bNew = HB_FALSE;
      ( q->ph )->addChild( ( QTreeWidgetItem * ) p->ph );
   }
}

/*
 * QBrush background ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_BACKGROUND )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_BACKGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::CheckState checkState ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHECKSTATE )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::CheckState ) ( p )->checkState( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_CHECKSTATE FP=hb_retni( ( Qt::CheckState ) ( p )->checkState( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * child ( int index ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHILD )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->child( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_CHILD FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->child( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * int childCount () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHILDCOUNT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->childCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_CHILDCOUNT FP=hb_retni( ( p )->childCount() ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem::ChildIndicatorPolicy childIndicatorPolicy () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CHILDINDICATORPOLICY )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( QTreeWidgetItem::ChildIndicatorPolicy ) ( p )->childIndicatorPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_CHILDINDICATORPOLICY FP=hb_retni( ( QTreeWidgetItem::ChildIndicatorPolicy ) ( p )->childIndicatorPolicy() ); p is NULL" ) );
   }
}

/*
 * virtual QTreeWidgetItem * clone () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_CLONE )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->clone(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_CLONE FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->clone(), false ) ); p is NULL" ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_COLUMNCOUNT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->columnCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_COLUMNCOUNT FP=hb_retni( ( p )->columnCount() ); p is NULL" ) );
   }
}

/*
 * virtual QVariant data ( int column, int role ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_DATA )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_DATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_FLAGS )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::ItemFlags ) ( p )->flags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_FLAGS FP=hb_retni( ( Qt::ItemFlags ) ( p )->flags() ); p is NULL" ) );
   }
}

/*
 * QFont font ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_FONT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QBrush foreground ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_FOREGROUND )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_FOREGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QIcon icon ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ICON )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int indexOfChild ( QTreeWidgetItem * child ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_INDEXOFCHILD )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->indexOfChild( hbqt_par_QTreeWidgetItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_INDEXOFCHILD FP=hb_retni( ( p )->indexOfChild( hbqt_par_QTreeWidgetItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void insertChild ( int index, QTreeWidgetItem * child )
 */
HB_FUNC( QT_QTREEWIDGETITEM_INSERTCHILD )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->insertChild( hb_parni( 2 ), hbqt_par_QTreeWidgetItem( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_INSERTCHILD FP=( p )->insertChild( hb_parni( 2 ), hbqt_par_QTreeWidgetItem( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool isDisabled () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISDISABLED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isDisabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_ISDISABLED FP=hb_retl( ( p )->isDisabled() ); p is NULL" ) );
   }
}

/*
 * bool isExpanded () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISEXPANDED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isExpanded() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_ISEXPANDED FP=hb_retl( ( p )->isExpanded() ); p is NULL" ) );
   }
}

/*
 * bool isFirstColumnSpanned () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISFIRSTCOLUMNSPANNED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isFirstColumnSpanned() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_ISFIRSTCOLUMNSPANNED FP=hb_retl( ( p )->isFirstColumnSpanned() ); p is NULL" ) );
   }
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISHIDDEN )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isHidden() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_ISHIDDEN FP=hb_retl( ( p )->isHidden() ); p is NULL" ) );
   }
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_ISSELECTED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isSelected() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_ISSELECTED FP=hb_retl( ( p )->isSelected() ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * parent () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_PARENT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->parent(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_PARENT FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->parent(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QTREEWIDGETITEM_READ )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->read( *hbqt_par_QDataStream( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_READ FP=( p )->read( *hbqt_par_QDataStream( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeChild ( QTreeWidgetItem * child )
 */
HB_FUNC( QT_QTREEWIDGETITEM_REMOVECHILD )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->removeChild( hbqt_par_QTreeWidgetItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_REMOVECHILD FP=( p )->removeChild( hbqt_par_QTreeWidgetItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackground ( int column, const QBrush & brush )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETBACKGROUND )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setBackground( hb_parni( 2 ), *hbqt_par_QBrush( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETBACKGROUND FP=( p )->setBackground( hb_parni( 2 ), *hbqt_par_QBrush( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setCheckState ( int column, Qt::CheckState state )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETCHECKSTATE )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setCheckState( hb_parni( 2 ), ( Qt::CheckState ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETCHECKSTATE FP=( p )->setCheckState( hb_parni( 2 ), ( Qt::CheckState ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setChildIndicatorPolicy ( QTreeWidgetItem::ChildIndicatorPolicy policy )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETCHILDINDICATORPOLICY )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setChildIndicatorPolicy( ( QTreeWidgetItem::ChildIndicatorPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETCHILDINDICATORPOLICY FP=( p )->setChildIndicatorPolicy( ( QTreeWidgetItem::ChildIndicatorPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setData ( int column, int role, const QVariant & value )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETDATA )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setData( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QVariant( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETDATA FP=( p )->setData( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QVariant( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setDisabled ( bool disabled )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETDISABLED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setDisabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETDISABLED FP=( p )->setDisabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setExpanded ( bool expand )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETEXPANDED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setExpanded( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETEXPANDED FP=( p )->setExpanded( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFirstColumnSpanned ( bool span )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETFIRSTCOLUMNSPANNED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setFirstColumnSpanned( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETFIRSTCOLUMNSPANNED FP=( p )->setFirstColumnSpanned( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETFLAGS )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETFLAGS FP=( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( int column, const QFont & font )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETFONT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setFont( hb_parni( 2 ), *hbqt_par_QFont( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETFONT FP=( p )->setFont( hb_parni( 2 ), *hbqt_par_QFont( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setForeground ( int column, const QBrush & brush )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETFOREGROUND )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setForeground( hb_parni( 2 ), *hbqt_par_QBrush( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETFOREGROUND FP=( p )->setForeground( hb_parni( 2 ), *hbqt_par_QBrush( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setHidden ( bool hide )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETHIDDEN )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setHidden( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETHIDDEN FP=( p )->setHidden( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIcon ( int column, const QIcon & icon )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETICON )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setIcon( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QIcon( 3 ) : QIcon( hbqt_par_QString( 3 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETICON FP=( p )->setIcon( hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QIcon( 3 ) : QIcon( hbqt_par_QString( 3 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setSelected ( bool select )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETSELECTED )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setSelected( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETSELECTED FP=( p )->setSelected( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeHint ( int column, const QSize & size )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETSIZEHINT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setSizeHint( hb_parni( 2 ), *hbqt_par_QSize( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETSIZEHINT FP=( p )->setSizeHint( hb_parni( 2 ), *hbqt_par_QSize( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setStatusTip ( int column, const QString & statusTip )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETSTATUSTIP )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setStatusTip( hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETSTATUSTIP FP=( p )->setStatusTip( hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setText ( int column, const QString & text )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETTEXT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setText( hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETTEXT FP=( p )->setText( hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTextAlignment ( int column, int alignment )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETTEXTALIGNMENT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setTextAlignment( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETTEXTALIGNMENT FP=( p )->setTextAlignment( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( int column, const QString & toolTip )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETTOOLTIP )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setToolTip( hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETTOOLTIP FP=( p )->setToolTip( hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setWhatsThis ( int column, const QString & whatsThis )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SETWHATSTHIS )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->setWhatsThis( hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SETWHATSTHIS FP=( p )->setWhatsThis( hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * QSize sizeHint ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_SIZEHINT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void sortChildren ( int column, Qt::SortOrder order )
 */
HB_FUNC( QT_QTREEWIDGETITEM_SORTCHILDREN )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      ( p )->sortChildren( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_SORTCHILDREN FP=( p )->sortChildren( hb_parni( 2 ), ( Qt::SortOrder ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QString statusTip ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_STATUSTIP )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->statusTip( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_STATUSTIP FP=hb_retc( ( p )->statusTip( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTreeWidgetItem * takeChild ( int index )
 */
HB_FUNC( QT_QTREEWIDGETITEM_TAKECHILD )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->takeChild( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_TAKECHILD FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidgetItem( ( p )->takeChild( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QList<QTreeWidgetItem *> takeChildren ()
 */
HB_FUNC( QT_QTREEWIDGETITEM_TAKECHILDREN )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->takeChildren() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_TAKECHILDREN FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTreeWidgetItem *>( ( p )->takeChildren() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString text ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TEXT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->text( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_TEXT FP=hb_retc( ( p )->text( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int textAlignment ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TEXTALIGNMENT )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->textAlignment( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_TEXTALIGNMENT FP=hb_retni( ( p )->textAlignment( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString toolTip ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TOOLTIP )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->toolTip( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_TOOLTIP FP=hb_retc( ( p )->toolTip( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTreeWidget * treeWidget () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TREEWIDGET )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTreeWidget( ( p )->treeWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_TREEWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QTreeWidget( ( p )->treeWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * int type () const
 */
HB_FUNC( QT_QTREEWIDGETITEM_TYPE )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_TYPE FP=hb_retni( ( p )->type() ); p is NULL" ) );
   }
}

/*
 * QString whatsThis ( int column ) const
 */
HB_FUNC( QT_QTREEWIDGETITEM_WHATSTHIS )
{
   QTreeWidgetItem * p = hbqt_par_QTreeWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->whatsThis( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTREEWIDGETITEM_WHATSTHIS FP=hb_retc( ( p )->whatsThis( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
