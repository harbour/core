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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QDial>


/*
 * QDial ( QWidget * parent = 0 )
 * ~QDial ()
 */

typedef struct
{
   QPointer< QDial > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDial;

HBQT_GC_FUNC( hbqt_gcRelease_QDial )
{
   HBQT_GC_T_QDial * p = ( HBQT_GC_T_QDial * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QDial * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDial( void * pObj, bool bNew )
{
   HBQT_GC_T_QDial * p = ( HBQT_GC_T_QDial * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDial ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDial >( ( QDial * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDial;
   p->type = HBQT_TYPE_QDial;

   return p;
}

HB_FUNC( QT_QDIAL )
{
   QDial * pObj = NULL;

   pObj = new QDial( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDial( ( void * ) pObj, true ) );
}

/* int notchSize () const */
HB_FUNC( QT_QDIAL_NOTCHSIZE )
{
   QDial * p = hbqt_par_QDial( 1 );
   if( p )
      hb_retni( ( p )->notchSize() );
}

/* qreal notchTarget () const */
HB_FUNC( QT_QDIAL_NOTCHTARGET )
{
   QDial * p = hbqt_par_QDial( 1 );
   if( p )
      hb_retnd( ( p )->notchTarget() );
}

/* bool notchesVisible () const */
HB_FUNC( QT_QDIAL_NOTCHESVISIBLE )
{
   QDial * p = hbqt_par_QDial( 1 );
   if( p )
      hb_retl( ( p )->notchesVisible() );
}

/* void setNotchTarget ( double target ) */
HB_FUNC( QT_QDIAL_SETNOTCHTARGET )
{
   QDial * p = hbqt_par_QDial( 1 );
   if( p )
      ( p )->setNotchTarget( hb_parnd( 2 ) );
}

/* bool wrapping () const */
HB_FUNC( QT_QDIAL_WRAPPING )
{
   QDial * p = hbqt_par_QDial( 1 );
   if( p )
      hb_retl( ( p )->wrapping() );
}

/* void setNotchesVisible ( bool visible ) */
HB_FUNC( QT_QDIAL_SETNOTCHESVISIBLE )
{
   QDial * p = hbqt_par_QDial( 1 );
   if( p )
      ( p )->setNotchesVisible( hb_parl( 2 ) );
}

/* void setWrapping ( bool on ) */
HB_FUNC( QT_QDIAL_SETWRAPPING )
{
   QDial * p = hbqt_par_QDial( 1 );
   if( p )
      ( p )->setWrapping( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
