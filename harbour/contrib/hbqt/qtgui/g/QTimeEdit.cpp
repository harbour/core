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

#include <QtGui/QTimeEdit>


/*
 * QTimeEdit ( QWidget * parent = 0 )
 * QTimeEdit ( const QTime & time, QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QTimeEdit > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTimeEdit;

HBQT_GC_FUNC( hbqt_gcRelease_QTimeEdit )
{
   HBQT_GC_T_QTimeEdit * p = ( HBQT_GC_T_QTimeEdit * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QTimeEdit * ph = p->ph;
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

void * hbqt_gcAllocate_QTimeEdit( void * pObj, bool bNew )
{
   HBQT_GC_T_QTimeEdit * p = ( HBQT_GC_T_QTimeEdit * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTimeEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTimeEdit >( ( QTimeEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTimeEdit;
   p->type = HBQT_TYPE_QTimeEdit;

   return p;
}

HB_FUNC( QT_QTIMEEDIT )
{
   QTimeEdit * pObj = NULL;

   pObj = new QTimeEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTimeEdit( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
