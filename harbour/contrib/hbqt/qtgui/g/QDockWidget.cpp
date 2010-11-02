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
 *  enum DockWidgetFeature { DockWidgetClosable, DockWidgetMovable, DockWidgetFloatable, DockWidgetVerticalTitleBar, AllDockWidgetFeatures, NoDockWidgetFeatures }
 *  flags DockWidgetFeatures
 */

/*
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QDockWidget>


/*
 * QDockWidget ( const QString & title, QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * QDockWidget ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QDockWidget ()
 */

typedef struct
{
   QPointer< QDockWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDockWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QDockWidget )
{
   HBQT_GC_T_QDockWidget * p = ( HBQT_GC_T_QDockWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QDockWidget * ph = p->ph;
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

void * hbqt_gcAllocate_QDockWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QDockWidget * p = ( HBQT_GC_T_QDockWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDockWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDockWidget >( ( QDockWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDockWidget;
   p->type = HBQT_TYPE_QDockWidget;

   return p;
}

HB_FUNC( QT_QDOCKWIDGET )
{
   QDockWidget * pObj = NULL;

   pObj = new QDockWidget( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDockWidget( ( void * ) pObj, true ) );
}

/* Qt::DockWidgetAreas allowedAreas () const */
HB_FUNC( QT_QDOCKWIDGET_ALLOWEDAREAS )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retni( ( Qt::DockWidgetAreas ) ( p )->allowedAreas() );
}

/* DockWidgetFeatures features () const */
HB_FUNC( QT_QDOCKWIDGET_FEATURES )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retni( ( QDockWidget::DockWidgetFeatures ) ( p )->features() );
}

/* bool isAreaAllowed ( Qt::DockWidgetArea area ) const */
HB_FUNC( QT_QDOCKWIDGET_ISAREAALLOWED )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retl( ( p )->isAreaAllowed( ( Qt::DockWidgetArea ) hb_parni( 2 ) ) );
}

/* bool isFloating () const */
HB_FUNC( QT_QDOCKWIDGET_ISFLOATING )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retl( ( p )->isFloating() );
}

/* void setAllowedAreas ( Qt::DockWidgetAreas areas ) */
HB_FUNC( QT_QDOCKWIDGET_SETALLOWEDAREAS )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setAllowedAreas( ( Qt::DockWidgetAreas ) hb_parni( 2 ) );
}

/* void setFeatures ( DockWidgetFeatures features ) */
HB_FUNC( QT_QDOCKWIDGET_SETFEATURES )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setFeatures( ( QDockWidget::DockWidgetFeatures ) hb_parni( 2 ) );
}

/* void setFloating ( bool floating ) */
HB_FUNC( QT_QDOCKWIDGET_SETFLOATING )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setFloating( hb_parl( 2 ) );
}

/* void setTitleBarWidget ( QWidget * widget ) */
HB_FUNC( QT_QDOCKWIDGET_SETTITLEBARWIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setTitleBarWidget( hbqt_par_QWidget( 2 ) );
}

/* void setWidget ( QWidget * widget ) */
HB_FUNC( QT_QDOCKWIDGET_SETWIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
}

/* QWidget * titleBarWidget () const */
HB_FUNC( QT_QDOCKWIDGET_TITLEBARWIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->titleBarWidget(), false ) );
}

/* QAction * toggleViewAction () const */
HB_FUNC( QT_QDOCKWIDGET_TOGGLEVIEWACTION )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->toggleViewAction(), false ) );
}

/* QWidget * widget () const */
HB_FUNC( QT_QDOCKWIDGET_WIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
