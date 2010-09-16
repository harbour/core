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
   QPointer< QItemSelectionModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QItemSelectionModel;

HBQT_GC_FUNC( hbqt_gcRelease_QItemSelectionModel )
{
   QItemSelectionModel  * ph = NULL ;
   HBQT_GC_T_QItemSelectionModel * p = ( HBQT_GC_T_QItemSelectionModel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QItemSelectionModel   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QItemSelectionModel   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QItemSelectionModel          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QItemSelectionModel    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QItemSelectionModel    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QItemSelectionModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QItemSelectionModel * p = ( HBQT_GC_T_QItemSelectionModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QItemSelectionModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QItemSelectionModel >( ( QItemSelectionModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QItemSelectionModel;
   p->type = HBQT_TYPE_QItemSelectionModel;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QItemSelectionModel  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QItemSelectionModel", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QITEMSELECTIONMODEL )
{
   QItemSelectionModel * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QItemSelectionModel( hbqt_par_QAbstractItemModel( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QItemSelectionModel( ( void * ) pObj, true ) );
}

/*
 * bool columnIntersectsSelection ( int column, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_COLUMNINTERSECTSSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retl( ( p )->columnIntersectsSelection( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
   }
}

/*
 * QModelIndex currentIndex () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_CURRENTINDEX )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) );
   }
}

/*
 * bool hasSelection () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_HASSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retl( ( p )->hasSelection() );
   }
}

/*
 * bool isColumnSelected ( int column, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISCOLUMNSELECTED )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retl( ( p )->isColumnSelected( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
   }
}

/*
 * bool isRowSelected ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISROWSELECTED )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retl( ( p )->isRowSelected( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
   }
}

/*
 * bool isSelected ( const QModelIndex & index ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISSELECTED )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retl( ( p )->isSelected( *hbqt_par_QModelIndex( 2 ) ) );
   }
}

/*
 * const QAbstractItemModel * model () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_MODEL )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( void * ) ( p )->model(), false ) );
   }
}

/*
 * bool rowIntersectsSelection ( int row, const QModelIndex & parent ) const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_ROWINTERSECTSSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retl( ( p )->rowIntersectsSelection( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
   }
}

/*
 * const QItemSelection selection () const
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->selection() ), true ) );
   }
}

/*
 * virtual void clear ()
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_CLEAR )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_CLEARSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      ( p )->clearSelection();
   }
}

/*
 * virtual void reset ()
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_RESET )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      ( p )->reset();
   }
}

/*
 * virtual void select ( const QModelIndex & index, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECT )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      ( p )->select( *hbqt_par_QModelIndex( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
   }
}

/*
 * virtual void select ( const QItemSelection & selection, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECT_1 )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      ( p )->select( *hbqt_par_QItemSelection( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
   }
}

/*
 * void setCurrentIndex ( const QModelIndex & index, QItemSelectionModel::SelectionFlags command )
 */
HB_FUNC( QT_QITEMSELECTIONMODEL_SETCURRENTINDEX )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
   {
      ( p )->setCurrentIndex( *hbqt_par_QModelIndex( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
