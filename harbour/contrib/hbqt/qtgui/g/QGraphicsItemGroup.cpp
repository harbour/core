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

#include <QtGui/QGraphicsItemGroup>


/*
 * QGraphicsItemGroup ( QGraphicsItem * parent = 0 )
 * ~QGraphicsItemGroup ()
 */

typedef struct
{
   QGraphicsItemGroup * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsItemGroup;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsItemGroup )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGraphicsItemGroup * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsItemGroup( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsItemGroup * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsItemGroup;
   p->type = HBQT_TYPE_QGraphicsItemGroup;

   return p;
}

HB_FUNC( QT_QGRAPHICSITEMGROUP )
{
   QGraphicsItemGroup * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QGraphicsItemGroup( hbqt_par_QGraphicsItem( 1 ) ) ;
   }
   else
   {
      pObj = new QGraphicsItemGroup() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsItemGroup( ( void * ) pObj, true ) );
}

/* void addToGroup ( QGraphicsItem * item ) */
HB_FUNC( QT_QGRAPHICSITEMGROUP_ADDTOGROUP )
{
   QGraphicsItemGroup * p = hbqt_par_QGraphicsItemGroup( 1 );
   if( p )
      ( p )->addToGroup( hbqt_par_QGraphicsItem( 2 ) );
}

/* void removeFromGroup ( QGraphicsItem * item ) */
HB_FUNC( QT_QGRAPHICSITEMGROUP_REMOVEFROMGROUP )
{
   QGraphicsItemGroup * p = hbqt_par_QGraphicsItemGroup( 1 );
   if( p )
      ( p )->removeFromGroup( hbqt_par_QGraphicsItem( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
