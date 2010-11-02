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

#include <QtGui/QWidgetItem>


/*
 * QWidgetItem ( QWidget * widget )
 */

typedef struct
{
   QWidgetItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWidgetItem;

HBQT_GC_FUNC( hbqt_gcRelease_QWidgetItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QWidgetItem * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWidgetItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWidgetItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWidgetItem;
   p->type = HBQT_TYPE_QWidgetItem;

   return p;
}

HB_FUNC( QT_QWIDGETITEM )
{
   QWidgetItem * pObj = NULL;

   pObj = new QWidgetItem( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWidgetItem( ( void * ) pObj, true ) );
}

/* virtual bool isEmpty () const */
HB_FUNC( QT_QWIDGETITEM_ISEMPTY )
{
   QWidgetItem * p = hbqt_par_QWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* virtual QWidget * widget () */
HB_FUNC( QT_QWIDGETITEM_WIDGET )
{
   QWidgetItem * p = hbqt_par_QWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
