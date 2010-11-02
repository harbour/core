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

#include <QtGui/QWidgetAction>


/*
 * QWidgetAction ( QObject * parent )
 * virtual ~QWidgetAction ()
 */

typedef struct
{
   QPointer< QWidgetAction > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWidgetAction;

HBQT_GC_FUNC( hbqt_gcRelease_QWidgetAction )
{
   HBQT_GC_T_QWidgetAction * p = ( HBQT_GC_T_QWidgetAction * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QWidgetAction * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWidgetAction( void * pObj, bool bNew )
{
   HBQT_GC_T_QWidgetAction * p = ( HBQT_GC_T_QWidgetAction * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWidgetAction ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWidgetAction >( ( QWidgetAction * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWidgetAction;
   p->type = HBQT_TYPE_QWidgetAction;

   return p;
}

HB_FUNC( QT_QWIDGETACTION )
{
   QWidgetAction * pObj = NULL;

   pObj = new QWidgetAction( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWidgetAction( ( void * ) pObj, true ) );
}

/* QWidget * defaultWidget () const */
HB_FUNC( QT_QWIDGETACTION_DEFAULTWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->defaultWidget(), false ) );
}

/* void releaseWidget ( QWidget * widget ) */
HB_FUNC( QT_QWIDGETACTION_RELEASEWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      ( p )->releaseWidget( hbqt_par_QWidget( 2 ) );
}

/* QWidget * requestWidget ( QWidget * parent ) */
HB_FUNC( QT_QWIDGETACTION_REQUESTWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->requestWidget( hbqt_par_QWidget( 2 ) ), false ) );
}

/* void setDefaultWidget ( QWidget * widget ) */
HB_FUNC( QT_QWIDGETACTION_SETDEFAULTWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      ( p )->setDefaultWidget( hbqt_par_QWidget( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
