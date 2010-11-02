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

#include <QtGui/QWheelEvent>


/*
 * QWheelEvent ( const QPoint & pos, int delta, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Qt::Orientation orient = Qt::Vertical )
 * QWheelEvent ( const QPoint & pos, const QPoint & globalPos, int delta, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Qt::Orientation orient = Qt::Vertical )
 */

typedef struct
{
   QWheelEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWheelEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QWheelEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWheelEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWheelEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWheelEvent;
   p->type = HBQT_TYPE_QWheelEvent;

   return p;
}

HB_FUNC( QT_QWHEELEVENT )
{
   // __HB_RETPTRGC__( new QWheelEvent() );
}

/* Qt::MouseButtons buttons () const */
HB_FUNC( QT_QWHEELEVENT_BUTTONS )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->buttons() );
}

/* int delta () const */
HB_FUNC( QT_QWHEELEVENT_DELTA )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->delta() );
}

/* const QPoint & globalPos () const */
HB_FUNC( QT_QWHEELEVENT_GLOBALPOS )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) );
}

/* int globalX () const */
HB_FUNC( QT_QWHEELEVENT_GLOBALX )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->globalX() );
}

/* int globalY () const */
HB_FUNC( QT_QWHEELEVENT_GLOBALY )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->globalY() );
}

/* Qt::Orientation orientation () const */
HB_FUNC( QT_QWHEELEVENT_ORIENTATION )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
}

/* const QPoint & pos () const */
HB_FUNC( QT_QWHEELEVENT_POS )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* int x () const */
HB_FUNC( QT_QWHEELEVENT_X )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->x() );
}

/* int y () const */
HB_FUNC( QT_QWHEELEVENT_Y )
{
   QWheelEvent * p = hbqt_par_QWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->y() );
}


#endif /* #if QT_VERSION >= 0x040500 */
