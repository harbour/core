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
 *  enum Reason { Mouse, Keyboard, Other }
 */

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QContextMenuEvent>


/* QContextMenuEvent ( Reason reason, const QPoint & pos, const QPoint & globalPos, Qt::KeyboardModifiers modifiers )
 * QContextMenuEvent ( Reason reason, const QPoint & pos, const QPoint & globalPos )
 * QContextMenuEvent ( Reason reason, const QPoint & pos )
 */

typedef struct
{
   QContextMenuEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QContextMenuEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QContextMenuEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QContextMenuEvent * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QContextMenuEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QContextMenuEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QContextMenuEvent;
   p->type = HBQT_TYPE_QContextMenuEvent;

   return p;
}

HB_FUNC( QT_QCONTEXTMENUEVENT )
{
   QContextMenuEvent * pObj = NULL;

   pObj = new QContextMenuEvent( ( QContextMenuEvent::Reason ) hb_parni( 1 ), *hbqt_par_QPoint( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QContextMenuEvent( ( void * ) pObj, true ) );
}

/* const QPoint & globalPos () const */
HB_FUNC( QT_QCONTEXTMENUEVENT_GLOBALPOS )
{
   QContextMenuEvent * p = hbqt_par_QContextMenuEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) );
}

/* int globalX () const */
HB_FUNC( QT_QCONTEXTMENUEVENT_GLOBALX )
{
   QContextMenuEvent * p = hbqt_par_QContextMenuEvent( 1 );
   if( p )
      hb_retni( ( p )->globalX() );
}

/* int globalY () const */
HB_FUNC( QT_QCONTEXTMENUEVENT_GLOBALY )
{
   QContextMenuEvent * p = hbqt_par_QContextMenuEvent( 1 );
   if( p )
      hb_retni( ( p )->globalY() );
}

/* const QPoint & pos () const */
HB_FUNC( QT_QCONTEXTMENUEVENT_POS )
{
   QContextMenuEvent * p = hbqt_par_QContextMenuEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* Reason reason () const */
HB_FUNC( QT_QCONTEXTMENUEVENT_REASON )
{
   QContextMenuEvent * p = hbqt_par_QContextMenuEvent( 1 );
   if( p )
      hb_retni( ( QContextMenuEvent::Reason ) ( p )->reason() );
}

/* int x () const */
HB_FUNC( QT_QCONTEXTMENUEVENT_X )
{
   QContextMenuEvent * p = hbqt_par_QContextMenuEvent( 1 );
   if( p )
      hb_retni( ( p )->x() );
}

/* int y () const */
HB_FUNC( QT_QCONTEXTMENUEVENT_Y )
{
   QContextMenuEvent * p = hbqt_par_QContextMenuEvent( 1 );
   if( p )
      hb_retni( ( p )->y() );
}


#endif /* #if QT_VERSION >= 0x040500 */
