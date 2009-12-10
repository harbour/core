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
 *  enum SelectionFlag { NoUpdate, Clear, Select, Deselect, ..., ClearAndSelect }
 *  flags SelectionFlags
 */

#include <QtCore/QPointer>

#include <QtGui/QItemSelectionModel>


/* QItemSelectionModel ( QAbstractItemModel * model )
 * QItemSelectionModel ( QAbstractItemModel * model, QObject * parent )
 * virtual ~QItemSelectionModel ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QItemSelectionModel > pq;
} QGC_POINTER_QItemSelectionModel;

QT_G_FUNC( release_QItemSelectionModel )
{
   QGC_POINTER_QItemSelectionModel * p = ( QGC_POINTER_QItemSelectionModel * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QItemSelectionModel          p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QItemSelectionModel         ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QItemSelectionModel * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QItemSelectionModel * ) p->ph )->~QItemSelectionModel();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QItemSelectionModel * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QItemSelectionModel         Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QItemSelectionModel         Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QItemSelectionModel         Object Already deleted!" ) );
   }
}

void * gcAllocate_QItemSelectionModel( void * pObj )
{
   QGC_POINTER_QItemSelectionModel * p = ( QGC_POINTER_QItemSelectionModel * ) hb_gcAllocate( sizeof( QGC_POINTER_QItemSelectionModel ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QItemSelectionModel;
   new( & p->pq ) QPointer< QItemSelectionModel >( ( QItemSelectionModel * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QItemSelectionModel         %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QITEMSELECTIONMODEL )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QItemSelectionModel( hbqt_par_QAbstractItemModel( 1 ) ) ;
   }

   hb_retptrGC( gcAllocate_QItemSelectionModel( pObj ) );
}
/*
 * bool columnIntersectsSelection ( int column, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_COLUMNINTERSECTSSELECTION )
{
   hb_retl( hbqt_par_QItemSelectionModel( 1 )->columnIntersectsSelection( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_CURRENTINDEX )
{
   hb_retptrGC( gcAllocate_QModelIndex( new QModelIndex( hbqt_par_QItemSelectionModel( 1 )->currentIndex() ) ) );
}

/*
 * bool hasSelection () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_HASSELECTION )
{
   hb_retl( hbqt_par_QItemSelectionModel( 1 )->hasSelection() );
}

/*
 * bool isColumnSelected ( int column, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISCOLUMNSELECTED )
{
   hb_retl( hbqt_par_QItemSelectionModel( 1 )->isColumnSelected( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/*
 * bool isRowSelected ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISROWSELECTED )
{
   hb_retl( hbqt_par_QItemSelectionModel( 1 )->isRowSelected( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/*
 * bool isSelected ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISSELECTED )
{
   hb_retl( hbqt_par_QItemSelectionModel( 1 )->isSelected( *hbqt_par_QModelIndex( 2 ) ) );
}

/*
 * const QAbstractItemModel * model () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_MODEL )
{
   hb_retptr( ( QAbstractItemModel* ) hbqt_par_QItemSelectionModel( 1 )->model() );
}

/*
 * bool rowIntersectsSelection ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ROWINTERSECTSSELECTION )
{
   hb_retl( hbqt_par_QItemSelectionModel( 1 )->rowIntersectsSelection( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/*
 * const QItemSelection selection () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECTION )
{
   hb_retptrGC( gcAllocate_QItemSelection( new QItemSelection( hbqt_par_QItemSelectionModel( 1 )->selection() ) ) );
}

/*
 * virtual void clear ()
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_CLEAR )
{
   hbqt_par_QItemSelectionModel( 1 )->clear();
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_CLEARSELECTION )
{
   hbqt_par_QItemSelectionModel( 1 )->clearSelection();
}

/*
 * virtual void reset ()
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_RESET )
{
   hbqt_par_QItemSelectionModel( 1 )->reset();
}

/*
 * virtual void select ( const QModelIndex & index, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECT )
{
   hbqt_par_QItemSelectionModel( 1 )->select( *hbqt_par_QModelIndex( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/*
 * virtual void select ( const QItemSelection & selection, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECT_1 )
{
   hbqt_par_QItemSelectionModel( 1 )->select( *hbqt_par_QItemSelection( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/*
 * void setCurrentIndex ( const QModelIndex & index, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SETCURRENTINDEX )
{
   hbqt_par_QItemSelectionModel( 1 )->setCurrentIndex( *hbqt_par_QModelIndex( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
