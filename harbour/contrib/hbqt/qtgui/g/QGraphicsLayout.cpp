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
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsLayout>


/*
 * QGraphicsLayout ( QGraphicsLayoutItem * parent = 0 )
 * ~QGraphicsLayout ()
 */

typedef struct
{
   QGraphicsLayout * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsLayout )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsLayout( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsLayout * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsLayout;
   p->type = HBQT_TYPE_QGraphicsLayout;

   return p;
}

HB_FUNC( QT_QGRAPHICSLAYOUT )
{
   // __HB_RETPTRGC__( new QGraphicsLayout() );
}

/* void activate () */
HB_FUNC( QT_QGRAPHICSLAYOUT_ACTIVATE )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      ( p )->activate();
}

/* virtual int count () const = 0 */
HB_FUNC( QT_QGRAPHICSLAYOUT_COUNT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* virtual void invalidate () */
HB_FUNC( QT_QGRAPHICSLAYOUT_INVALIDATE )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      ( p )->invalidate();
}

/* bool isActivated () const */
HB_FUNC( QT_QGRAPHICSLAYOUT_ISACTIVATED )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      hb_retl( ( p )->isActivated() );
}

/* virtual QGraphicsLayoutItem * itemAt ( int i ) const = 0 */
HB_FUNC( QT_QGRAPHICSLAYOUT_ITEMAT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLayoutItem( ( p )->itemAt( hb_parni( 2 ) ), false ) );
}

/* virtual void removeAt ( int index ) = 0 */
HB_FUNC( QT_QGRAPHICSLAYOUT_REMOVEAT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      ( p )->removeAt( hb_parni( 2 ) );
}

/* void setContentsMargins ( qreal left, qreal top, qreal right, qreal bottom ) */
HB_FUNC( QT_QGRAPHICSLAYOUT_SETCONTENTSMARGINS )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      ( p )->setContentsMargins( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* virtual void widgetEvent ( QEvent * e ) */
HB_FUNC( QT_QGRAPHICSLAYOUT_WIDGETEVENT )
{
   QGraphicsLayout * p = hbqt_par_QGraphicsLayout( 1 );
   if( p )
      ( p )->widgetEvent( hbqt_par_QEvent( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
