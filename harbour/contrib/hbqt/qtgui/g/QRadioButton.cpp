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

#include <QtGui/QRadioButton>


/*
 * QRadioButton ( QWidget * parent = 0 )
 * QRadioButton ( const QString & text, QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QRadioButton > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRadioButton;

HBQT_GC_FUNC( hbqt_gcRelease_QRadioButton )
{
   HBQT_GC_T_QRadioButton * p = ( HBQT_GC_T_QRadioButton * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QRadioButton * ph = p->ph;
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

void * hbqt_gcAllocate_QRadioButton( void * pObj, bool bNew )
{
   HBQT_GC_T_QRadioButton * p = ( HBQT_GC_T_QRadioButton * ) hb_gcAllocate( sizeof( HBQT_GC_T_QRadioButton ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QRadioButton >( ( QRadioButton * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRadioButton;
   p->type = HBQT_TYPE_QRadioButton;

   return p;
}

HB_FUNC( QT_QRADIOBUTTON )
{
   QRadioButton * pObj = NULL;

   if( HB_ISCHAR( 1 ) )
      pObj = new QRadioButton( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = new QRadioButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QRadioButton( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
