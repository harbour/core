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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionGraphicsItem>
#include <QtGui/QMatrix>
#include <QtCore/QRectF>


/*
 * QStyleOptionGraphicsItem ()
 * QStyleOptionGraphicsItem ( const QStyleOptionGraphicsItem & other )
 */

typedef struct
{
   QStyleOptionGraphicsItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionGraphicsItem;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionGraphicsItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionGraphicsItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionGraphicsItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionGraphicsItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionGraphicsItem;
   p->type = HBQT_TYPE_QStyleOptionGraphicsItem;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONGRAPHICSITEM )
{
   QStyleOptionGraphicsItem * pObj = NULL;

   pObj = new QStyleOptionGraphicsItem() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionGraphicsItem( ( void * ) pObj, true ) );
}

/* QRectF exposedRect */
HB_FUNC( QT_QSTYLEOPTIONGRAPHICSITEM_EXPOSEDRECT )
{
   QStyleOptionGraphicsItem * p = hbqt_par_QStyleOptionGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->exposedRect ), true ) );
}

/* qreal levelOfDetail */
HB_FUNC( QT_QSTYLEOPTIONGRAPHICSITEM_LEVELOFDETAIL )
{
   QStyleOptionGraphicsItem * p = hbqt_par_QStyleOptionGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->levelOfDetail );
}

/* QMatrix matrix */
HB_FUNC( QT_QSTYLEOPTIONGRAPHICSITEM_MATRIX )
{
   QStyleOptionGraphicsItem * p = hbqt_par_QStyleOptionGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->matrix ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
