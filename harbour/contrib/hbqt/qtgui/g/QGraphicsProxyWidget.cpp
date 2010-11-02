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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsProxyWidget>


/*
 * QGraphicsProxyWidget ( QGraphicsItem * parent = 0, Qt::WindowFlags wFlags = 0 )
 * ~QGraphicsProxyWidget ()
 */

typedef struct
{
   QPointer< QGraphicsProxyWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsProxyWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsProxyWidget )
{
   HBQT_GC_T_QGraphicsProxyWidget * p = ( HBQT_GC_T_QGraphicsProxyWidget * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QGraphicsProxyWidget * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsProxyWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QGraphicsProxyWidget * p = ( HBQT_GC_T_QGraphicsProxyWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGraphicsProxyWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGraphicsProxyWidget >( ( QGraphicsProxyWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsProxyWidget;
   p->type = HBQT_TYPE_QGraphicsProxyWidget;

   return p;
}

HB_FUNC( QT_QGRAPHICSPROXYWIDGET )
{
   QGraphicsProxyWidget * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QGraphicsProxyWidget( hbqt_par_QGraphicsItem( 1 ), ( Qt::WindowFlags ) ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 0 ) ) ;
   }
   else
   {
      pObj = new QGraphicsProxyWidget() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsProxyWidget( ( void * ) pObj, true ) );
}

/* QGraphicsProxyWidget * createProxyForChildWidget ( QWidget * child ) */
HB_FUNC( QT_QGRAPHICSPROXYWIDGET_CREATEPROXYFORCHILDWIDGET )
{
   QGraphicsProxyWidget * p = hbqt_par_QGraphicsProxyWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsProxyWidget( ( p )->createProxyForChildWidget( hbqt_par_QWidget( 2 ) ), false ) );
}

/* void setWidget ( QWidget * widget ) */
HB_FUNC( QT_QGRAPHICSPROXYWIDGET_SETWIDGET )
{
   QGraphicsProxyWidget * p = hbqt_par_QGraphicsProxyWidget( 1 );
   if( p )
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
}

/* QRectF subWidgetRect ( const QWidget * widget ) const */
HB_FUNC( QT_QGRAPHICSPROXYWIDGET_SUBWIDGETRECT )
{
   QGraphicsProxyWidget * p = hbqt_par_QGraphicsProxyWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->subWidgetRect( hbqt_par_QWidget( 2 ) ) ), true ) );
}

/* QWidget * widget () const */
HB_FUNC( QT_QGRAPHICSPROXYWIDGET_WIDGET )
{
   QGraphicsProxyWidget * p = hbqt_par_QGraphicsProxyWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
