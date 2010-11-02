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
 *     enum Magnet { Left, Right, Top, Bottom, Vertical, Horizontal }
 */

/*
 *  Constructed[ 19/19 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsScene>
#include "hbqt_hbqgraphicsscene.h"

/*
 * HBQGraphicsScene ()
 * HBQGraphicsScene ( QObject * parent )
 * ~HBQGraphicsScene ()
 */

typedef struct
{
   QPointer< HBQGraphicsScene > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQGraphicsScene;

HBQT_GC_FUNC( hbqt_gcRelease_HBQGraphicsScene )
{
   HBQT_GC_T_HBQGraphicsScene * p = ( HBQT_GC_T_HBQGraphicsScene * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         HBQGraphicsScene * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQGraphicsScene( void * pObj, bool bNew )
{
   HBQT_GC_T_HBQGraphicsScene * p = ( HBQT_GC_T_HBQGraphicsScene * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQGraphicsScene ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQGraphicsScene >( ( HBQGraphicsScene * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQGraphicsScene;
   p->type = HBQT_TYPE_HBQGraphicsScene;

   return p;
}

HB_FUNC( QT_HBQGRAPHICSSCENE )
{
   HBQGraphicsScene * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new HBQGraphicsScene( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj = new HBQGraphicsScene() ;
   }

   hb_retptrGC( hbqt_gcAllocate_HBQGraphicsScene( ( void * ) pObj, true ) );
}

/* void                     hbSetBlock( PHB_ITEM block ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_HBSETBLOCK )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->hbSetBlock( hb_param( 2, HB_IT_ANY ) );
}

/* virtual int              pageSize() */
HB_FUNC( QT_HBQGRAPHICSSCENE_PAGESIZE )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->pageSize() );
}

/* virtual void             setPageSize( int pageSize ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETPAGESIZE )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setPageSize( hb_parni( 2 ) );
}

/* QRectF                   paperRect() */
HB_FUNC( QT_HBQGRAPHICSSCENE_PAPERRECT )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->paperRect() ), true ) );
}

/* void                     setPaperRect( const QRectF & paperRect ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETPAPERRECT )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setPaperRect( *hbqt_par_QRectF( 2 ) );
}

/* virtual int              orientation() */
HB_FUNC( QT_HBQGRAPHICSSCENE_ORIENTATION )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->orientation() );
}

/* virtual void             setOrientation( int orientation ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETORIENTATION )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setOrientation( hb_parni( 2 ) );
}

/* virtual QRectF           geometry() */
HB_FUNC( QT_HBQGRAPHICSSCENE_GEOMETRY )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) );
}

/* virtual void             setGeometry( const QRectF & rect ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETGEOMETRY )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRectF( 2 ) );
}

/* int                      magnetArea() */
HB_FUNC( QT_HBQGRAPHICSSCENE_MAGNETAREA )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->magnetArea() );
}

/* void                     setMagnetArea( int magnetArea ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETMAGNETAREA )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setMagnetArea( hb_parni( 2 ) );
}

/* virtual bool             showGrid() */
HB_FUNC( QT_HBQGRAPHICSSCENE_SHOWGRID )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->showGrid() );
}

/* virtual void             setShowGrid( bool showGrid ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETSHOWGRID )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setShowGrid( hb_parl( 2 ) );
}

/* virtual void             setLeftMagnet( bool magneted ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETLEFTMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setLeftMagnet( hb_parl( 2 ) );
}

/* virtual void             setRightMagnet( bool magneted ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETRIGHTMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setRightMagnet( hb_parl( 2 ) );
}

/* virtual void             setTopMagnet( bool magneted ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETTOPMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setTopMagnet( hb_parl( 2 ) );
}

/* virtual void             setBottomMagnet( bool magneted ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETBOTTOMMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setBottomMagnet( hb_parl( 2 ) );
}

/* virtual void             setHorizontalMagnet( bool magneted ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETHORIZONTALMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setHorizontalMagnet( hb_parl( 2 ) );
}

/* virtual void             setVerticalMagnet( bool magneted ) */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETVERTICALMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setVerticalMagnet( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
