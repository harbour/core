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

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 1/1 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QAbstractListModel>


/*
 * QAbstractListModel ( QObject * parent = 0 )
 * ~QAbstractListModel ()
 */

typedef struct
{
   QPointer< QAbstractListModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractListModel;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractListModel )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractListModel( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractListModel * p = ( HBQT_GC_T_QAbstractListModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractListModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractListModel >( ( QAbstractListModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractListModel;
   p->type = HBQT_TYPE_QAbstractListModel;

   return p;
}

HB_FUNC( QT_QABSTRACTLISTMODEL )
{
   //__HB_RETPTRGC__( new QAbstractListModel() );
}

/* virtual QModelIndex index ( int row, int column = 0, const QModelIndex & parent = QModelIndex() ) const */
HB_FUNC( QT_QABSTRACTLISTMODEL_INDEX )
{
   QAbstractListModel * p = hbqt_par_QAbstractListModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
