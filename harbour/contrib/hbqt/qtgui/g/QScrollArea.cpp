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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QScrollArea>


/*
 * QScrollArea ( QWidget * parent = 0 )
 * ~QScrollArea ()
 */

typedef struct
{
   QPointer< QScrollArea > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QScrollArea;

HBQT_GC_FUNC( hbqt_gcRelease_QScrollArea )
{
   HBQT_GC_T_QScrollArea * p = ( HBQT_GC_T_QScrollArea * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QScrollArea * ph = p->ph;
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

void * hbqt_gcAllocate_QScrollArea( void * pObj, bool bNew )
{
   HBQT_GC_T_QScrollArea * p = ( HBQT_GC_T_QScrollArea * ) hb_gcAllocate( sizeof( HBQT_GC_T_QScrollArea ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QScrollArea >( ( QScrollArea * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QScrollArea;
   p->type = HBQT_TYPE_QScrollArea;

   return p;
}

HB_FUNC( QT_QSCROLLAREA )
{
   QScrollArea * pObj = NULL;

   pObj = new QScrollArea( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QScrollArea( ( void * ) pObj, true ) );
}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QSCROLLAREA_ALIGNMENT )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* void ensureVisible ( int x, int y, int xmargin = 50, int ymargin = 50 ) */
HB_FUNC( QT_QSCROLLAREA_ENSUREVISIBLE )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      ( p )->ensureVisible( hb_parni( 2 ), hb_parni( 3 ), hb_parnidef( 4, 50 ), hb_parnidef( 5, 50 ) );
}

/* void ensureWidgetVisible ( QWidget * childWidget, int xmargin = 50, int ymargin = 50 ) */
HB_FUNC( QT_QSCROLLAREA_ENSUREWIDGETVISIBLE )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      ( p )->ensureWidgetVisible( hbqt_par_QWidget( 2 ), hb_parnidef( 3, 50 ), hb_parnidef( 4, 50 ) );
}

/* void setAlignment ( Qt::Alignment ) */
HB_FUNC( QT_QSCROLLAREA_SETALIGNMENT )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setWidget ( QWidget * widget ) */
HB_FUNC( QT_QSCROLLAREA_SETWIDGET )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
}

/* void setWidgetResizable ( bool resizable ) */
HB_FUNC( QT_QSCROLLAREA_SETWIDGETRESIZABLE )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      ( p )->setWidgetResizable( hb_parl( 2 ) );
}

/* QWidget * takeWidget () */
HB_FUNC( QT_QSCROLLAREA_TAKEWIDGET )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->takeWidget(), false ) );
}

/* QWidget * widget () const */
HB_FUNC( QT_QSCROLLAREA_WIDGET )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
}

/* bool widgetResizable () const */
HB_FUNC( QT_QSCROLLAREA_WIDGETRESIZABLE )
{
   QScrollArea * p = hbqt_par_QScrollArea( 1 );
   if( p )
      hb_retl( ( p )->widgetResizable() );
}


#endif /* #if QT_VERSION >= 0x040500 */
