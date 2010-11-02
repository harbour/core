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

#include <QtGui/QDropEvent>


/*
 * QDropEvent ( const QPoint & pos, Qt::DropActions actions, const QMimeData * data, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Type type = Drop )
 */

typedef struct
{
   QDropEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDropEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QDropEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDropEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDropEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDropEvent;
   p->type = HBQT_TYPE_QDropEvent;

   return p;
}

HB_FUNC( QT_QDROPEVENT )
{
   // __HB_RETPTRGC__( new QDropEvent() );
}

/* void acceptProposedAction () */
HB_FUNC( QT_QDROPEVENT_ACCEPTPROPOSEDACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      ( p )->acceptProposedAction();
}

/* Qt::DropAction dropAction () const */
HB_FUNC( QT_QDROPEVENT_DROPACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->dropAction() );
}

/* Qt::KeyboardModifiers keyboardModifiers () const */
HB_FUNC( QT_QDROPEVENT_KEYBOARDMODIFIERS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->keyboardModifiers() );
}

/* const QMimeData * mimeData () const */
HB_FUNC( QT_QDROPEVENT_MIMEDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QMimeData( ( void* ) hbqt_par_QDropEvent( 1 )->mimeData(), false ) );
}

/* Qt::MouseButtons mouseButtons () const */
HB_FUNC( QT_QDROPEVENT_MOUSEBUTTONS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->mouseButtons() );
}

/* const QPoint & pos () const */
HB_FUNC( QT_QDROPEVENT_POS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* Qt::DropActions possibleActions () const */
HB_FUNC( QT_QDROPEVENT_POSSIBLEACTIONS )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropActions ) ( p )->possibleActions() );
}

/* Qt::DropAction proposedAction () const */
HB_FUNC( QT_QDROPEVENT_PROPOSEDACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->proposedAction() );
}

/* void setDropAction ( Qt::DropAction action ) */
HB_FUNC( QT_QDROPEVENT_SETDROPACTION )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      ( p )->setDropAction( ( Qt::DropAction ) hb_parni( 2 ) );
}

/* QWidget * source () const */
HB_FUNC( QT_QDROPEVENT_SOURCE )
{
   QDropEvent * p = hbqt_par_QDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->source(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
