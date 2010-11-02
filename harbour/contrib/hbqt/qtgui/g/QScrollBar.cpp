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

#include <QtGui/QScrollBar>


/*
 * QScrollBar ( QWidget * parent = 0 )
 * QScrollBar ( Qt::Orientation orientation, QWidget * parent = 0 )
 * ~QScrollBar ()
 */

typedef struct
{
   QPointer< QScrollBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QScrollBar;

HBQT_GC_FUNC( hbqt_gcRelease_QScrollBar )
{
   HBQT_GC_T_QScrollBar * p = ( HBQT_GC_T_QScrollBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QScrollBar * ph = p->ph;
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

void * hbqt_gcAllocate_QScrollBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QScrollBar * p = ( HBQT_GC_T_QScrollBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QScrollBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QScrollBar >( ( QScrollBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QScrollBar;
   p->type = HBQT_TYPE_QScrollBar;

   return p;
}

HB_FUNC( QT_QSCROLLBAR )
{
   QScrollBar * pObj = NULL;

   pObj = new QScrollBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QScrollBar( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
