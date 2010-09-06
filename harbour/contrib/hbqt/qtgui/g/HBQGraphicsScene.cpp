/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *     enum Magnet { Left, Right, Top, Bottom, Vertical, Horizontal }
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsScene>
#include "hbqt_hbqgraphicsscene.h"

/*
 * HBQGraphicsScene()
 * HBQGraphicsScene( QObject * parent )
 * ~HBQGraphicsScene()
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
   HBQGraphicsScene  * ph = NULL ;
   HBQT_GC_T_HBQGraphicsScene * p = ( HBQT_GC_T_HBQGraphicsScene * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQGraphicsScene   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQGraphicsScene   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_HBQGraphicsScene          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_HBQGraphicsScene    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_HBQGraphicsScene    :    Object not created with new=true", ph ) );
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

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_HBQGraphicsScene  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_HBQGraphicsScene", pObj ) );
   }
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

/*
 * void                     hbSetBlock( PHB_ITEM block )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_HBSETBLOCK )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->hbSetBlock( hb_param( 2, HB_IT_ANY ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_HBSETBLOCK FP=( p )->hbSetBlock( hb_param( 2, HB_IT_ANY ) ); p is NULL" ) );
   }
}

/*
 * virtual int              pageSize()
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_PAGESIZE )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->pageSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_PAGESIZE FP=hb_retni( ( p )->pageSize() ); p is NULL" ) );
   }
}

/*
 * virtual void             setPageSize( int pageSize )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETPAGESIZE )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setPageSize( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETPAGESIZE FP=( p )->setPageSize( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QRectF                   paperRect()
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_PAPERRECT )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->paperRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_PAPERRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->paperRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * void                     setPaperRect( const QRectF & paperRect )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETPAPERRECT )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setPaperRect( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETPAPERRECT FP=( p )->setPaperRect( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual int              orientation()
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_ORIENTATION )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->orientation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_ORIENTATION FP=hb_retni( ( p )->orientation() ); p is NULL" ) );
   }
}

/*
 * virtual void             setOrientation( int orientation )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETORIENTATION )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setOrientation( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETORIENTATION FP=( p )->setOrientation( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QRectF           geometry()
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_GEOMETRY )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_GEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void             setGeometry( const QRectF & rect )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETGEOMETRY )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETGEOMETRY FP=( p )->setGeometry( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * int                      magnetArea()
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_MAGNETAREA )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->magnetArea() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_MAGNETAREA FP=hb_retni( ( p )->magnetArea() ); p is NULL" ) );
   }
}

/*
 * void                     setMagnetArea( int magnetArea )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETMAGNETAREA )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setMagnetArea( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETMAGNETAREA FP=( p )->setMagnetArea( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual bool             showGrid()
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SHOWGRID )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->showGrid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SHOWGRID FP=hb_retl( ( p )->showGrid() ); p is NULL" ) );
   }
}

/*
 * virtual void             setShowGrid( bool showGrid )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETSHOWGRID )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setShowGrid( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETSHOWGRID FP=( p )->setShowGrid( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void             setLeftMagnet( bool magneted )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETLEFTMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setLeftMagnet( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETLEFTMAGNET FP=( p )->setLeftMagnet( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void             setRightMagnet( bool magneted )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETRIGHTMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setRightMagnet( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETRIGHTMAGNET FP=( p )->setRightMagnet( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void             setTopMagnet( bool magneted )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETTOPMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setTopMagnet( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETTOPMAGNET FP=( p )->setTopMagnet( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void             setBottomMagnet( bool magneted )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETBOTTOMMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setBottomMagnet( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETBOTTOMMAGNET FP=( p )->setBottomMagnet( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void             setHorizontalMagnet( bool magneted )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETHORIZONTALMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setHorizontalMagnet( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETHORIZONTALMAGNET FP=( p )->setHorizontalMagnet( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void             setVerticalMagnet( bool magneted )
 */
HB_FUNC( QT_HBQGRAPHICSSCENE_SETVERTICALMAGNET )
{
   HBQGraphicsScene * p = hbqt_par_HBQGraphicsScene( 1 );
   if( p )
      ( p )->setVerticalMagnet( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQGRAPHICSSCENE_SETVERTICALMAGNET FP=( p )->setVerticalMagnet( hb_parl( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
