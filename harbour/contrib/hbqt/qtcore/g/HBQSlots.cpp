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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QObject>
#include "hbqt_hbqslots.h"


/*
 * HBQSlots ()
 * ~HBQSlots ()
 */

typedef struct
{
   QPointer< HBQSlots > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQSlots;

HBQT_GC_FUNC( hbqt_gcRelease_HBQSlots )
{
   HBQT_GC_T_HBQSlots * p = ( HBQT_GC_T_HBQSlots * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      HBQSlots * ph = p->ph;
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

void * hbqt_gcAllocate_HBQSlots( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQSlots * p = ( HBQT_GC_T_HBQSlots * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQSlots ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQSlots >( ( HBQSlots * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQSlots;
   p->type = HBQT_TYPE_HBQSlots;

   return p;
}

HB_FUNC( QT_HBQSLOTS )
{
   HBQSlots * pObj = NULL;

   pObj = new HBQSlots() ;

   hb_retptrGC( hbqt_gcAllocate_HBQSlots( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
