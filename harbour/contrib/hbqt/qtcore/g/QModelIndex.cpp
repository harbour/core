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
 *  Constructed[ 11/11 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QModelIndex>


/*
 * QModelIndex ()
 * QModelIndex ( const QModelIndex & other )
 * ~QModelIndex ()
 */

typedef struct
{
   QModelIndex * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QModelIndex;

HBQT_GC_FUNC( hbqt_gcRelease_QModelIndex )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QModelIndex * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QModelIndex( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QModelIndex * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QModelIndex;
   p->type = HBQT_TYPE_QModelIndex;

   return p;
}

HB_FUNC( QT_QMODELINDEX )
{
   QModelIndex * pObj = NULL;

   pObj = new QModelIndex() ;

   hb_retptrGC( hbqt_gcAllocate_QModelIndex( ( void * ) pObj, true ) );
}

/* QModelIndex child ( int row, int column ) const */
HB_FUNC( QT_QMODELINDEX_CHILD )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->child( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
}

/* int column () const */
HB_FUNC( QT_QMODELINDEX_COLUMN )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retni( ( p )->column() );
}

/* QVariant data ( int role = Qt::DisplayRole ) const */
HB_FUNC( QT_QMODELINDEX_DATA )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parnidef( 2, Qt::DisplayRole ) ) ), true ) );
}

/* Qt::ItemFlags flags () const */
HB_FUNC( QT_QMODELINDEX_FLAGS )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retni( ( Qt::ItemFlags ) ( p )->flags() );
}

/* qint64 internalId () const */
HB_FUNC( QT_QMODELINDEX_INTERNALID )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retnint( ( p )->internalId() );
}

/* void * internalPointer () const */
HB_FUNC( QT_QMODELINDEX_INTERNALPOINTER )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      ( p )->internalPointer();
}

/* bool isValid () const */
HB_FUNC( QT_QMODELINDEX_ISVALID )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* const QAbstractItemModel * model () const */
HB_FUNC( QT_QMODELINDEX_MODEL )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractItemModel( ( void * ) ( p )->model(), false ) );
}

/* QModelIndex parent () const */
HB_FUNC( QT_QMODELINDEX_PARENT )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->parent() ), true ) );
}

/* int row () const */
HB_FUNC( QT_QMODELINDEX_ROW )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retni( ( p )->row() );
}

/* QModelIndex sibling ( int row, int column ) const */
HB_FUNC( QT_QMODELINDEX_SIBLING )
{
   QModelIndex * p = hbqt_par_QModelIndex( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->sibling( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
