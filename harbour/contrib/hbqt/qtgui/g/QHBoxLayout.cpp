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

#include <QtGui/QHBoxLayout>


/*
 * QHBoxLayout ()
 * QHBoxLayout ( QWidget * parent )
 * ~QHBoxLayout ()
 */

typedef struct
{
   QPointer< QHBoxLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHBoxLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QHBoxLayout )
{
   QHBoxLayout  * ph = NULL;
   HBQT_GC_T_QHBoxLayout * p = ( HBQT_GC_T_QHBoxLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
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

void * hbqt_gcAllocate_QHBoxLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QHBoxLayout * p = ( HBQT_GC_T_QHBoxLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QHBoxLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QHBoxLayout >( ( QHBoxLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHBoxLayout;
   p->type = HBQT_TYPE_QHBoxLayout;

   return p;
}

HB_FUNC( QT_QHBOXLAYOUT )
{
   QHBoxLayout * pObj = NULL;

   pObj = new QHBoxLayout( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QHBoxLayout( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
