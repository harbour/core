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

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QObject>
#include "hbqt_hbqevents.h"


/*
 * HBQEvents ()
 * ~HBQEvents ()
 *
 */

typedef struct
{
   QPointer< HBQEvents > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQEvents;

HBQT_GC_FUNC( hbqt_gcRelease_HBQEvents )
{
   HBQT_GC_T_HBQEvents * p = ( HBQT_GC_T_HBQEvents * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      HBQEvents * ph = p->ph;
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

void * hbqt_gcAllocate_HBQEvents( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQEvents * p = ( HBQT_GC_T_HBQEvents * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQEvents ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQEvents >( ( HBQEvents * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQEvents;
   p->type = HBQT_TYPE_HBQEvents;

   return p;
}

HB_FUNC( QT_HBQEVENTS )
{
   HBQEvents * pObj = NULL;

   pObj = new HBQEvents() ;

   hb_retptrGC( hbqt_gcAllocate_HBQEvents( ( void * ) pObj, true ) );
}

/* bool hbConnect( PHB_ITEM obj, int event, PHB_ITEM block ) */
HB_FUNC( QT_HBQEVENTS_HBCONNECT )
{
   HBQEvents * p = hbqt_par_HBQEvents( 1 );
   if( p )
      hb_retl( ( p )->hbConnect( hb_param( 2, HB_IT_ANY ), hb_parni( 3 ), hb_param( 4, HB_IT_ANY ) ) );
}

/* bool hbDisconnect( PHB_ITEM obj, int event ) */
HB_FUNC( QT_HBQEVENTS_HBDISCONNECT )
{
   HBQEvents * p = hbqt_par_HBQEvents( 1 );
   if( p )
      hb_retl( ( p )->hbDisconnect( hb_param( 2, HB_IT_ANY ), hb_parni( 3 ) ) );
}

/* bool hbClear() */
HB_FUNC( QT_HBQEVENTS_HBCLEAR )
{
   HBQEvents * p = hbqt_par_HBQEvents( 1 );
   if( p )
      hb_retl( ( p )->hbClear() );
}


#endif /* #if QT_VERSION >= 0x040500 */
