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

#include <QtGui/QDateEdit>


/*
 * QDateEdit ( QWidget * parent = 0 )
 * QDateEdit ( const QDate & date, QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QDateEdit > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDateEdit;

HBQT_GC_FUNC( hbqt_gcRelease_QDateEdit )
{
   QDateEdit  * ph = NULL;
   HBQT_GC_T_QDateEdit * p = ( HBQT_GC_T_QDateEdit * ) Cargo;

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

void * hbqt_gcAllocate_QDateEdit( void * pObj, bool bNew )
{
   HBQT_GC_T_QDateEdit * p = ( HBQT_GC_T_QDateEdit * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDateEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDateEdit >( ( QDateEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDateEdit;
   p->type = HBQT_TYPE_QDateEdit;

   return p;
}

HB_FUNC( QT_QDATEEDIT )
{
   QDateEdit * pObj = NULL;

   pObj = new QDateEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDateEdit( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
