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
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QModelIndexList indexes () const
 *  // void split ( const QItemSelectionRange & range, const QItemSelectionRange & other, QItemSelection * result )
 */

#include <QtCore/QPointer>

#include <QtGui/QItemSelection>


/* QItemSelection ()
 * QItemSelection ( const QModelIndex & topLeft, const QModelIndex & bottomRight )
 */

typedef struct
{
   QItemSelection * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QItemSelection;

HBQT_GC_FUNC( hbqt_gcRelease_QItemSelection )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QItemSelection * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QItemSelection( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QItemSelection * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QItemSelection;
   p->type = HBQT_TYPE_QItemSelection;

   return p;
}

HB_FUNC( QT_QITEMSELECTION )
{
   QItemSelection * pObj = NULL;

   pObj = new QItemSelection() ;

   hb_retptrGC( hbqt_gcAllocate_QItemSelection( ( void * ) pObj, true ) );
}

/* bool contains ( const QModelIndex & index ) const */
HB_FUNC( QT_QITEMSELECTION_CONTAINS )
{
   QItemSelection * p = hbqt_par_QItemSelection( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QModelIndex( 2 ) ) );
}

/* void merge ( const QItemSelection & other, QItemSelectionModel::SelectionFlags command ) */
HB_FUNC( QT_QITEMSELECTION_MERGE )
{
   QItemSelection * p = hbqt_par_QItemSelection( 1 );
   if( p )
      ( p )->merge( *hbqt_par_QItemSelection( 2 ), ( QItemSelectionModel::SelectionFlags ) hb_parni( 3 ) );
}

/* void select ( const QModelIndex & topLeft, const QModelIndex & bottomRight ) */
HB_FUNC( QT_QITEMSELECTION_SELECT )
{
   QItemSelection * p = hbqt_par_QItemSelection( 1 );
   if( p )
      ( p )->select( *hbqt_par_QModelIndex( 2 ), *hbqt_par_QModelIndex( 3 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
