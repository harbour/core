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

#if QT_VERSION >= 0x040500

/*
 *  enum Type { None, AccessibilityDescription, AccessibilityHelp, AccessibilityPrepare, ..., MaxUser }
 */

/*
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QEvent>


/*
 * QEvent ( Type type )
 * ~QEvent ()
 */

typedef struct
{
   QEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QEvent * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QEvent;
   p->type = HBQT_TYPE_QEvent;

   return p;
}

HB_FUNC( QT_QEVENT )
{
   QEvent * pObj = NULL;

   pObj = new QEvent( ( QEvent::Type ) hb_parni( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QEvent( ( void * ) pObj, true ) );
}

/* void accept () */
HB_FUNC( QT_QEVENT_ACCEPT )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      ( p )->accept();
}

/* void ignore () */
HB_FUNC( QT_QEVENT_IGNORE )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      ( p )->ignore();
}

/* bool isAccepted () const */
HB_FUNC( QT_QEVENT_ISACCEPTED )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retl( ( p )->isAccepted() );
}

/* void setAccepted ( bool accepted ) */
HB_FUNC( QT_QEVENT_SETACCEPTED )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      ( p )->setAccepted( hb_parl( 2 ) );
}

/* bool spontaneous () const */
HB_FUNC( QT_QEVENT_SPONTANEOUS )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retl( ( p )->spontaneous() );
}

/* Type type () const */
HB_FUNC( QT_QEVENT_TYPE )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retni( ( QEvent::Type ) ( p )->type() );
}

/* int registerEventType ( int hint = -1 ) */
HB_FUNC( QT_QEVENT_REGISTEREVENTTYPE )
{
   QEvent * p = hbqt_par_QEvent( 1 );
   if( p )
      hb_retni( ( p )->registerEventType( hb_parnidef( 2, -1 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
