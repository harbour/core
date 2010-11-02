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
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QDragMoveEvent>


/*
 * QDragMoveEvent ( const QPoint & pos, Qt::DropActions actions, const QMimeData * data, Qt::MouseButtons buttons, Qt::KeyboardModifiers modifiers, Type type = DragMove )
 * ~QDragMoveEvent ()
 */

typedef struct
{
   QDragMoveEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDragMoveEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QDragMoveEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDragMoveEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDragMoveEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDragMoveEvent;
   p->type = HBQT_TYPE_QDragMoveEvent;

   return p;
}

HB_FUNC( QT_QDRAGMOVEEVENT )
{
   // __HB_RETPTRGC__( new QDragMoveEvent() );
}

/* void accept ( const QRect & rectangle ) */
HB_FUNC( QT_QDRAGMOVEEVENT_ACCEPT )
{
   QDragMoveEvent * p = hbqt_par_QDragMoveEvent( 1 );
   if( p )
      ( p )->accept( *hbqt_par_QRect( 2 ) );
}

/* void accept () */
HB_FUNC( QT_QDRAGMOVEEVENT_ACCEPT_1 )
{
   QDragMoveEvent * p = hbqt_par_QDragMoveEvent( 1 );
   if( p )
      ( p )->accept();
}

/* QRect answerRect () const */
HB_FUNC( QT_QDRAGMOVEEVENT_ANSWERRECT )
{
   QDragMoveEvent * p = hbqt_par_QDragMoveEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->answerRect() ), true ) );
}

/* void ignore ( const QRect & rectangle ) */
HB_FUNC( QT_QDRAGMOVEEVENT_IGNORE )
{
   QDragMoveEvent * p = hbqt_par_QDragMoveEvent( 1 );
   if( p )
      ( p )->ignore( *hbqt_par_QRect( 2 ) );
}

/* void ignore () */
HB_FUNC( QT_QDRAGMOVEEVENT_IGNORE_1 )
{
   QDragMoveEvent * p = hbqt_par_QDragMoveEvent( 1 );
   if( p )
      ( p )->ignore();
}


#endif /* #if QT_VERSION >= 0x040500 */
