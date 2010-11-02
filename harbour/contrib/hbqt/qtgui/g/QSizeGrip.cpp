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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSizeGrip>


/*
 * QSizeGrip ( QWidget * parent )
 * ~QSizeGrip ()
 */

typedef struct
{
   QPointer< QSizeGrip > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSizeGrip;

HBQT_GC_FUNC( hbqt_gcRelease_QSizeGrip )
{
   HBQT_GC_T_QSizeGrip * p = ( HBQT_GC_T_QSizeGrip * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QSizeGrip * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSizeGrip( void * pObj, bool bNew )
{
   HBQT_GC_T_QSizeGrip * p = ( HBQT_GC_T_QSizeGrip * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSizeGrip ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSizeGrip >( ( QSizeGrip * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSizeGrip;
   p->type = HBQT_TYPE_QSizeGrip;

   return p;
}

HB_FUNC( QT_QSIZEGRIP )
{
   QSizeGrip * pObj = NULL;

   pObj = new QSizeGrip( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSizeGrip( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
