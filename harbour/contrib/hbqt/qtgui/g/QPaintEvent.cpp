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

#include <QtGui/QPaintEvent>


/*
 * QPaintEvent ( const QRegion & paintRegion )
 * QPaintEvent ( const QRect & paintRect )
 */

typedef struct
{
   QPaintEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPaintEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QPaintEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPaintEvent * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPaintEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPaintEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPaintEvent;
   p->type = HBQT_TYPE_QPaintEvent;

   return p;
}

HB_FUNC( QT_QPAINTEVENT )
{
   QPaintEvent * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = ( QString ) hbqt_par_QString( 1 );

      if( objName == ( QString ) "QRect" )
      {
         pObj = new QPaintEvent( *hbqt_par_QRect( 1 ) ) ;
      }
      else if( objName == ( QString ) "QRegion" )
      {
         pObj = new QPaintEvent( *hbqt_par_QRegion( 1 ) ) ;
      }
   }

   hb_retptrGC( hbqt_gcAllocate_QPaintEvent( ( void * ) pObj, true ) );
}

/* const QRect & rect () const */
HB_FUNC( QT_QPAINTEVENT_RECT )
{
   QPaintEvent * p = hbqt_par_QPaintEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) );
}

/* const QRegion & region () const */
HB_FUNC( QT_QPAINTEVENT_REGION )
{
   QPaintEvent * p = hbqt_par_QPaintEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->region() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
