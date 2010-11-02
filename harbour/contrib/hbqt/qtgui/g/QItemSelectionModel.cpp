/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum SelectionFlag { NoUpdate, Clear, Select, Deselect, ..., ClearAndSelect }
 *  flags SelectionFlags
 */

/*
 *  Constructed[ 15/15 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QModelIndexList selectedColumns ( int row = 0 ) const
 *  // QModelIndexList selectedIndexes () const
 *  // QModelIndexList selectedRows ( int column = 0 ) const
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
   HBQT_GC_T_QItemSelectionModel * p = ( HBQT_GC_T_QItemSelectionModel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QItemSelectionModel * ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QItemSelectionModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QItemSelectionModel * p = ( HBQT_GC_T_QItemSelectionModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QItemSelectionModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QItemSelectionModel >( ( QItemSelectionModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QItemSelectionModel;
   p->type = HBQT_TYPE_QItemSelectionModel;

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

/* bool columnIntersectsSelection ( int column, const QModelIndex & parent ) const */
HB_FUNC( QT_QITEMSELECTIONMODEL_COLUMNINTERSECTSSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retl( ( p )->columnIntersectsSelection( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/* QModelIndex currentIndex () const */
HB_FUNC( QT_QITEMSELECTIONMODEL_CURRENTINDEX )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->currentIndex() ), true ) );
}

/* bool hasSelection () const */
HB_FUNC( QT_QITEMSELECTIONMODEL_HASSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retl( ( p )->hasSelection() );
}

/* bool isColumnSelected ( int column, const QModelIndex & parent ) const */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISCOLUMNSELECTED )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retl( ( p )->isColumnSelected( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/* bool isRowSelected ( int row, const QModelIndex & parent ) const */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISROWSELECTED )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retl( ( p )->isRowSelected( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/* bool isSelected ( const QModelIndex & index ) const */
HB_FUNC( QT_QITEMSELECTIONMODEL_ISSELECTED )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retl( ( p )->isSelected( *hbqt_par_QModelIndex( 2 ) ) );
}

/* const QAbstractItemModel * model () const */
HB_FUNC( QT_QITEMSELECTIONMODEL_MODEL )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( void * ) ( p )->model(), false ) );
}

/* bool rowIntersectsSelection ( int row, const QModelIndex & parent ) const */
HB_FUNC( QT_QITEMSELECTIONMODEL_ROWINTERSECTSSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retl( ( p )->rowIntersectsSelection( hb_parni( 2 ), *hbqt_par_QModelIndex( 3 ) ) );
}

/* const QItemSelection selection () const */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->selection() ), true ) );
}

/* virtual void clear () */
HB_FUNC( QT_QITEMSELECTIONMODEL_CLEAR )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      ( p )->clear();
}

/* void clearSelection () */
HB_FUNC( QT_QITEMSELECTIONMODEL_CLEARSELECTION )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      ( p )->clearSelection();
}

/* virtual void reset () */
HB_FUNC( QT_QITEMSELECTIONMODEL_RESET )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      ( p )->reset();
}

/* virtual void select ( const QModelIndex & index, QItemSelectionModel::SelectionFlags command ) */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECT )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      ( p )->select( *hbqt_par_QModelIndex( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/* virtual void select ( const QItemSelection & selection, QItemSelectionModel::SelectionFlags command ) */
HB_FUNC( QT_QITEMSELECTIONMODEL_SELECT_1 )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      ( p )->select( *hbqt_par_QItemSelection( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/* void setCurrentIndex ( const QModelIndex & index, QItemSelectionModel::SelectionFlags command ) */
HB_FUNC( QT_QITEMSELECTIONMODEL_SETCURRENTINDEX )
{
   QItemSelectionModel * p = hbqt_par_QItemSelectionModel( 1 );
   if( p )
      ( p )->setCurrentIndex( *hbqt_par_QModelIndex( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
