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
 *  enum Type { None, AccessibilityDescription, AccessibilityHelp, AccessibilityPrepare, ..., MaxUser }
 */

/*
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QMouseEvent>


/*
 * QMouseEvent ( Type type, const QPoint & position, Qt::MouseButton button, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers )
 * QMouseEvent ( Type type, const QPoint & pos, const QPoint & globalPos, Qt::MouseButton button, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers )
 * ~QMouseEvent ()
 */

typedef struct
{
   QMouseEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMouseEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QMouseEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QMouseEvent * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMouseEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMouseEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMouseEvent;
   p->type = HBQT_TYPE_QMouseEvent;

   return p;
}

HB_FUNC( QT_QMOUSEEVENT )
{
   QMouseEvent * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMouseEvent( *hbqt_par_QMouseEvent( 1 ) ) ;
   }
   else if( hb_pcount() == 5 )
   {
      pObj = new QMouseEvent( ( QEvent::Type ) hb_parni( 1 ), *hbqt_par_QPoint( 2 ), ( Qt::MouseButton ) hb_parni( 3 ), ( Qt::MouseButtons ) hb_parni( 4 ), ( Qt::KeyboardModifiers ) hb_parni( 5 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMouseEvent( ( void * ) pObj, true ) );
}

/* Qt::MouseButton button () const */
HB_FUNC( QT_QMOUSEEVENT_BUTTON )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButton ) ( p )->button() );
}

/* Qt::MouseButtons buttons () const */
HB_FUNC( QT_QMOUSEEVENT_BUTTONS )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->buttons() );
}

/* const QPoint & globalPos () const */
HB_FUNC( QT_QMOUSEEVENT_GLOBALPOS )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) );
}

/* int globalX () const */
HB_FUNC( QT_QMOUSEEVENT_GLOBALX )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->globalX() );
}

/* int globalY () const */
HB_FUNC( QT_QMOUSEEVENT_GLOBALY )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->globalY() );
}

/* const QPoint & pos () const */
HB_FUNC( QT_QMOUSEEVENT_POS )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* QPointF posF () const */
HB_FUNC( QT_QMOUSEEVENT_POSF )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->posF() ), true ) );
}

/* int x () const */
HB_FUNC( QT_QMOUSEEVENT_X )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->x() );
}

/* int y () const */
HB_FUNC( QT_QMOUSEEVENT_Y )
{
   QMouseEvent * p = hbqt_par_QMouseEvent( 1 );
   if( p )
      hb_retni( ( p )->y() );
}


#endif /* #if QT_VERSION >= 0x040500 */
