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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QAbstractItemModel>
#include "hbqt_hbqabstractitemmodel.h"


/*
 * HBQAbstractItemModel ( ( PHB_ITEM ) hb_param( 1, HB_IT_BLOCK ) )
 * ~HBQAbstractItemModel ()
 */

typedef struct
{
   QPointer< HBQAbstractItemModel > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQAbstractItemModel;

HBQT_GC_FUNC( hbqt_gcRelease_HBQAbstractItemModel )
{
   HBQAbstractItemModel  * ph = NULL;
   HBQT_GC_T_HBQAbstractItemModel * p = ( HBQT_GC_T_HBQAbstractItemModel * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_HBQAbstractItemModel( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQAbstractItemModel * p = ( HBQT_GC_T_HBQAbstractItemModel * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQAbstractItemModel ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQAbstractItemModel >( ( HBQAbstractItemModel * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQAbstractItemModel;
   p->type = HBQT_TYPE_HBQAbstractItemModel;

   return p;
}

HB_FUNC( QT_HBQABSTRACTITEMMODEL )
{
   HBQAbstractItemModel * pObj = NULL;

   pObj = new HBQAbstractItemModel( ( PHB_ITEM ) hb_param( 1, HB_IT_BLOCK ) ) ;

   hb_retptrGC( hbqt_gcAllocate_HBQAbstractItemModel( ( void * ) pObj, true ) );
}

/* void reset() */
HB_FUNC( QT_HBQABSTRACTITEMMODEL_RESET )
{
   HBQAbstractItemModel * p = hbqt_par_HBQAbstractItemModel( 1 );
   if( p )
      ( p )->reset();
}

/* QModelIndex index( int row, int column, const QModelIndex & parent = QModelIndex() ) const */
HB_FUNC( QT_HBQABSTRACTITEMMODEL_INDEX )
{
   HBQAbstractItemModel * p = hbqt_par_HBQAbstractItemModel( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( p )->index( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QModelIndex( 4 ) : QModelIndex() ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
