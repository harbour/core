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
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractProxyModel>
#include <QtGui/QItemSelection>

/* QAbstractProxyModel ( QObject * parent = 0 )
 * ~QAbstractProxyModel ()
 */

typedef struct
{
   QPointer< QAbstractProxyModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractProxyModel;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractProxyModel )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractProxyModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractProxyModel * p = ( HBQT_GC_T_QAbstractProxyModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractProxyModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractProxyModel >( ( QAbstractProxyModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractProxyModel;
   p->type = HBQT_TYPE_QAbstractProxyModel;

   return p;
}

HB_FUNC( QT_QABSTRACTPROXYMODEL )
{
   // __HB_RETPTRGC__( new QAbstractProxyModel() );
}

/* virtual QModelIndex mapFromSource ( const QModelIndex & sourceIndex ) const = 0 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPFROMSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapFromSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* virtual QItemSelection mapSelectionFromSource ( const QItemSelection & sourceSelection ) const */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPSELECTIONFROMSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->mapSelectionFromSource( *hbqt_par_QItemSelection( 2 ) ) ), true ) );
}

/* virtual QItemSelection mapSelectionToSource ( const QItemSelection & proxySelection ) const */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPSELECTIONTOSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( p )->mapSelectionToSource( *hbqt_par_QItemSelection( 2 ) ) ), true ) );
}

/* virtual QModelIndex mapToSource ( const QModelIndex & proxyIndex ) const = 0 */
HB_FUNC( QT_QABSTRACTPROXYMODEL_MAPTOSOURCE )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->mapToSource( *hbqt_par_QModelIndex( 2 ) ) ), true ) );
}

/* virtual void setSourceModel ( QAbstractItemModel * sourceModel ) */
HB_FUNC( QT_QABSTRACTPROXYMODEL_SETSOURCEMODEL )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      ( p )->setSourceModel( hbqt_par_QAbstractItemModel( 2 ) );
}

/* QAbstractItemModel * sourceModel () const */
HB_FUNC( QT_QABSTRACTPROXYMODEL_SOURCEMODEL )
{
   QAbstractProxyModel * p = hbqt_par_QAbstractProxyModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( p )->sourceModel(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
