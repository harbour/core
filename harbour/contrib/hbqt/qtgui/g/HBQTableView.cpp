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
 *  Constructed[ 1/1 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTableView>
#include "hbqt_hbqtableview.h"


/*
 * HBQTableView ( QWidget * parent = 0 )
 * ~HBQTableView ()
 *
 */

typedef struct
{
   QPointer< HBQTableView > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQTableView;

HBQT_GC_FUNC( hbqt_gcRelease_HBQTableView )
{
   HBQT_GC_T_HBQTableView * p = ( HBQT_GC_T_HBQTableView * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      HBQTableView * ph = p->ph;
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

void * hbqt_gcAllocate_HBQTableView( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQTableView * p = ( HBQT_GC_T_HBQTableView * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQTableView ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQTableView >( ( HBQTableView * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQTableView;
   p->type = HBQT_TYPE_HBQTableView;

   return p;
}

HB_FUNC( QT_HBQTABLEVIEW )
{
   HBQTableView * pObj = NULL;

   pObj = new HBQTableView( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_HBQTableView( ( void * ) pObj, true ) );
}

/* void hbSetBlock( PHB_ITEM block ) */
HB_FUNC( QT_HBQTABLEVIEW_HBSETBLOCK )
{
   HBQTableView * p = hbqt_par_HBQTableView( 1 );
   if( p )
      ( p )->hbSetBlock( hb_param( 2, HB_IT_ANY ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
