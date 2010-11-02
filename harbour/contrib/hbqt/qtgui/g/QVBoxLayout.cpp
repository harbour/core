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

#include <QtGui/QVBoxLayout>


/*
 * QVBoxLayout ()
 * QVBoxLayout ( QWidget * parent )
 * ~QVBoxLayout ()
 */

typedef struct
{
   QPointer< QVBoxLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QVBoxLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QVBoxLayout )
{
   HBQT_GC_T_QVBoxLayout * p = ( HBQT_GC_T_QVBoxLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QVBoxLayout * ph = p->ph;
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

void * hbqt_gcAllocate_QVBoxLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QVBoxLayout * p = ( HBQT_GC_T_QVBoxLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QVBoxLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QVBoxLayout >( ( QVBoxLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QVBoxLayout;
   p->type = HBQT_TYPE_QVBoxLayout;

   return p;
}

HB_FUNC( QT_QVBOXLAYOUT )
{
   QVBoxLayout * pObj = NULL;

   pObj = new QVBoxLayout( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QVBoxLayout( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
