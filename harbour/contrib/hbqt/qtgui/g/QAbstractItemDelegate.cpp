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
 *  enum EndEditHint { NoHint, EditNextItem, EditPreviousItem, SubmitModelCache, RevertModelCache }
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractItemDelegate>


/* QAbstractItemDelegate ( QObject * parent = 0 )
 * virtual ~QAbstractItemDelegate ()
 */

typedef struct
{
   QPointer< QAbstractItemDelegate > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractItemDelegate;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractItemDelegate )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractItemDelegate( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractItemDelegate * p = ( HBQT_GC_T_QAbstractItemDelegate * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractItemDelegate ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractItemDelegate >( ( QAbstractItemDelegate * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractItemDelegate;
   p->type = HBQT_TYPE_QAbstractItemDelegate;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractItemDelegate  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractItemDelegate", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTITEMDELEGATE )
{
   // hb_retptr( new QAbstractItemDelegate( 0 ) );
}

/*
 * virtual QWidget * createEditor ( QWidget * parent, const QStyleOptionViewItem & option, const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_CREATEEDITOR )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->createEditor( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) ), false ) );
   }
}

/*
 * virtual bool editorEvent ( QEvent * event, QAbstractItemModel * model, const QStyleOptionViewItem & option, const QModelIndex & index )
 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_EDITOREVENT )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
   {
      hb_retl( ( p )->editorEvent( hbqt_par_QEvent( 2 ), hbqt_par_QAbstractItemModel( 3 ), *hbqt_par_QStyleOptionViewItem( 4 ), *hbqt_par_QModelIndex( 5 ) ) );
   }
}

/*
 * virtual void paint ( QPainter * painter, const QStyleOptionViewItem & option, const QModelIndex & index ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_PAINT )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
   {
      ( p )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
   }
}

/*
 * virtual void setEditorData ( QWidget * editor, const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_SETEDITORDATA )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
   {
      ( p )->setEditorData( hbqt_par_QWidget( 2 ), *hbqt_par_QModelIndex( 3 ) );
   }
}

/*
 * virtual void setModelData ( QWidget * editor, QAbstractItemModel * model, const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_SETMODELDATA )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
   {
      ( p )->setModelData( hbqt_par_QWidget( 2 ), hbqt_par_QAbstractItemModel( 3 ), *hbqt_par_QModelIndex( 4 ) );
   }
}

/*
 * virtual QSize sizeHint ( const QStyleOptionViewItem & option, const QModelIndex & index ) const = 0
 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_SIZEHINT )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint( *hbqt_par_QStyleOptionViewItem( 2 ), *hbqt_par_QModelIndex( 3 ) ) ), true ) );
   }
}

/*
 * virtual void updateEditorGeometry ( QWidget * editor, const QStyleOptionViewItem & option, const QModelIndex & index ) const
 */
HB_FUNC( QT_QABSTRACTITEMDELEGATE_UPDATEEDITORGEOMETRY )
{
   QAbstractItemDelegate * p = hbqt_par_QAbstractItemDelegate( 1 );
   if( p )
   {
      ( p )->updateEditorGeometry( hbqt_par_QWidget( 2 ), *hbqt_par_QStyleOptionViewItem( 3 ), *hbqt_par_QModelIndex( 4 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
