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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QFocusFrame>


/*
 * QFocusFrame ( QWidget * parent = 0 )
 * ~QFocusFrame ()
 */

typedef struct
{
   QPointer< QFocusFrame > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFocusFrame;

HBQT_GC_FUNC( hbqt_gcRelease_QFocusFrame )
{
   HBQT_GC_T_QFocusFrame * p = ( HBQT_GC_T_QFocusFrame * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QFocusFrame * ph = p->ph;
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

void * hbqt_gcAllocate_QFocusFrame( void * pObj, bool bNew )
{
   HBQT_GC_T_QFocusFrame * p = ( HBQT_GC_T_QFocusFrame * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFocusFrame ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFocusFrame >( ( QFocusFrame * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFocusFrame;
   p->type = HBQT_TYPE_QFocusFrame;

   return p;
}

HB_FUNC( QT_QFOCUSFRAME )
{
   QFocusFrame * pObj = NULL;

   pObj = new QFocusFrame( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFocusFrame( ( void * ) pObj, true ) );
}

/* void setWidget ( QWidget * widget ) */
HB_FUNC( QT_QFOCUSFRAME_SETWIDGET )
{
   QFocusFrame * p = hbqt_par_QFocusFrame( 1 );
   if( p )
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
}

/* QWidget * widget () const */
HB_FUNC( QT_QFOCUSFRAME_WIDGET )
{
   QFocusFrame * p = hbqt_par_QFocusFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
