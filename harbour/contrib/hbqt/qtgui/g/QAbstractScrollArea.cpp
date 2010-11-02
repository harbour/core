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
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QWidgetList scrollBarWidgets ( Qt::Alignment alignment )
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractScrollArea>


/*
 * QAbstractScrollArea ( QWidget * parent = 0 )
 * ~QAbstractScrollArea ()
 */

typedef struct
{
   QPointer< QAbstractScrollArea > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractScrollArea;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractScrollArea )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractScrollArea( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractScrollArea * p = ( HBQT_GC_T_QAbstractScrollArea * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractScrollArea ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractScrollArea >( ( QAbstractScrollArea * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractScrollArea;
   p->type = HBQT_TYPE_QAbstractScrollArea;

   return p;
}

HB_FUNC( QT_QABSTRACTSCROLLAREA )
{

}

/* void addScrollBarWidget ( QWidget * widget, Qt::Alignment alignment ) */
HB_FUNC( QT_QABSTRACTSCROLLAREA_ADDSCROLLBARWIDGET )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      ( p )->addScrollBarWidget( hbqt_par_QWidget( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
}

/* QWidget * cornerWidget () const */
HB_FUNC( QT_QABSTRACTSCROLLAREA_CORNERWIDGET )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->cornerWidget(), false ) );
}

/* QScrollBar * horizontalScrollBar () const */
HB_FUNC( QT_QABSTRACTSCROLLAREA_HORIZONTALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QScrollBar( ( p )->horizontalScrollBar(), false ) );
}

/* Qt::ScrollBarPolicy horizontalScrollBarPolicy () const */
HB_FUNC( QT_QABSTRACTSCROLLAREA_HORIZONTALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      hb_retni( ( Qt::ScrollBarPolicy ) ( p )->horizontalScrollBarPolicy() );
}

/* QSize maximumViewportSize () const */
HB_FUNC( QT_QABSTRACTSCROLLAREA_MAXIMUMVIEWPORTSIZE )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumViewportSize() ), true ) );
}

/* void setCornerWidget ( QWidget * widget ) */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETCORNERWIDGET )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      ( p )->setCornerWidget( hbqt_par_QWidget( 2 ) );
}

/* void setHorizontalScrollBar ( QScrollBar * scrollBar ) */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETHORIZONTALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      ( p )->setHorizontalScrollBar( hbqt_par_QScrollBar( 2 ) );
}

/* void setHorizontalScrollBarPolicy ( Qt::ScrollBarPolicy ) */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETHORIZONTALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      ( p )->setHorizontalScrollBarPolicy( ( Qt::ScrollBarPolicy ) hb_parni( 2 ) );
}

/* void setVerticalScrollBar ( QScrollBar * scrollBar ) */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVERTICALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      ( p )->setVerticalScrollBar( hbqt_par_QScrollBar( 2 ) );
}

/* void setVerticalScrollBarPolicy ( Qt::ScrollBarPolicy ) */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVERTICALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      ( p )->setVerticalScrollBarPolicy( ( Qt::ScrollBarPolicy ) hb_parni( 2 ) );
}

/* void setViewport ( QWidget * widget ) */
HB_FUNC( QT_QABSTRACTSCROLLAREA_SETVIEWPORT )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      ( p )->setViewport( hbqt_par_QWidget( 2 ) );
}

/* QScrollBar * verticalScrollBar () const */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VERTICALSCROLLBAR )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QScrollBar( ( p )->verticalScrollBar(), false ) );
}

/* Qt::ScrollBarPolicy verticalScrollBarPolicy () const */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VERTICALSCROLLBARPOLICY )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      hb_retni( ( Qt::ScrollBarPolicy ) ( p )->verticalScrollBarPolicy() );
}

/* QWidget * viewport () const */
HB_FUNC( QT_QABSTRACTSCROLLAREA_VIEWPORT )
{
   QAbstractScrollArea * p = hbqt_par_QAbstractScrollArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->viewport(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
