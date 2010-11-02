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
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStackedWidget>


/*
 * QStackedWidget ( QWidget * parent = 0 )
 * ~QStackedWidget ()
 *
 */

typedef struct
{
   QPointer< QStackedWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStackedWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QStackedWidget )
{
   HBQT_GC_T_QStackedWidget * p = ( HBQT_GC_T_QStackedWidget * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QStackedWidget * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStackedWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QStackedWidget * p = ( HBQT_GC_T_QStackedWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QStackedWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStackedWidget >( ( QStackedWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStackedWidget;
   p->type = HBQT_TYPE_QStackedWidget;

   return p;
}

HB_FUNC( QT_QSTACKEDWIDGET )
{
   QStackedWidget * pObj = NULL;

   pObj = new QStackedWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QStackedWidget( ( void * ) pObj, true ) );
}

/* int addWidget ( QWidget * widget ) */
HB_FUNC( QT_QSTACKEDWIDGET_ADDWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->addWidget( hbqt_par_QWidget( 2 ) ) );
}

/* int count () const */
HB_FUNC( QT_QSTACKEDWIDGET_COUNT )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* int currentIndex () const */
HB_FUNC( QT_QSTACKEDWIDGET_CURRENTINDEX )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->currentIndex() );
}

/* QWidget * currentWidget () const */
HB_FUNC( QT_QSTACKEDWIDGET_CURRENTWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->currentWidget(), false ) );
}

/* int indexOf ( QWidget * widget ) const */
HB_FUNC( QT_QSTACKEDWIDGET_INDEXOF )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/* int insertWidget ( int index, QWidget * widget ) */
HB_FUNC( QT_QSTACKEDWIDGET_INSERTWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) ) );
}

/* void removeWidget ( QWidget * widget ) */
HB_FUNC( QT_QSTACKEDWIDGET_REMOVEWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      ( p )->removeWidget( hbqt_par_QWidget( 2 ) );
}

/* QWidget * widget ( int index ) const */
HB_FUNC( QT_QSTACKEDWIDGET_WIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) );
}

/* void setCurrentIndex ( int index ) */
HB_FUNC( QT_QSTACKEDWIDGET_SETCURRENTINDEX )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      ( p )->setCurrentIndex( hb_parni( 2 ) );
}

/* void setCurrentWidget ( QWidget * widget ) */
HB_FUNC( QT_QSTACKEDWIDGET_SETCURRENTWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      ( p )->setCurrentWidget( hbqt_par_QWidget( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
