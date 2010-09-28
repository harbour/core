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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 16/16 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsLinearLayout>


/*
 * QGraphicsLinearLayout ( QGraphicsLayoutItem * parent = 0 )
 * QGraphicsLinearLayout ( Qt::Orientation orientation, QGraphicsLayoutItem * parent = 0 )
 * virtual ~QGraphicsLinearLayout ()
 */

typedef struct
{
   QGraphicsLinearLayout * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsLinearLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsLinearLayout )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QGraphicsLinearLayout   /.\\", p->ph ) );
         delete ( ( QGraphicsLinearLayout * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QGraphicsLinearLayout   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGraphicsLinearLayout    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGraphicsLinearLayout    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsLinearLayout( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsLinearLayout * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsLinearLayout;
   p->type = HBQT_TYPE_QGraphicsLinearLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsLinearLayout", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsLinearLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSLINEARLAYOUT )
{
   QGraphicsLinearLayout * pObj = NULL;

   if( hb_pcount() >= 1 )
   {
      if( HB_ISNUM( 1 ) )
      {
         pObj = new QGraphicsLinearLayout( ( Qt::Orientation ) hb_parni( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsLayoutItem( 2 ) : 0 ) ) ;
      }
      else if( HB_ISPOINTER( 1 ) )
      {
         pObj = new QGraphicsLinearLayout( hbqt_par_QGraphicsLayoutItem( 1 ) ) ;
      }
      else
      {
         pObj = new QGraphicsLinearLayout() ;
      }
   }
   else
   {
      pObj = new QGraphicsLinearLayout() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsLinearLayout( ( void * ) pObj, true ) );
}

/*
 * void addItem ( QGraphicsLayoutItem * item )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ADDITEM )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->addItem( hbqt_par_QGraphicsLayoutItem( 2 ) );
   }
}

/*
 * void addStretch ( int stretch = 1 )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ADDSTRETCH )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->addStretch( hb_parnidef( 2, 1 ) );
   }
}

/*
 * Qt::Alignment alignment ( QGraphicsLayoutItem * item ) const
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ALIGNMENT )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->alignment( hbqt_par_QGraphicsLayoutItem( 2 ) ) );
   }
}

/*
 * void insertItem ( int index, QGraphicsLayoutItem * item )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_INSERTITEM )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->insertItem( hb_parni( 2 ), hbqt_par_QGraphicsLayoutItem( 3 ) );
   }
}

/*
 * void insertStretch ( int index, int stretch = 1 )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_INSERTSTRETCH )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->insertStretch( hb_parni( 2 ), hb_parnidef( 3, 1 ) );
   }
}

/*
 * qreal itemSpacing ( int index ) const
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ITEMSPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->itemSpacing( hb_parni( 2 ) ) );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_ORIENTATION )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   }
}

/*
 * virtual void removeAt ( int index )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_REMOVEAT )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->removeAt( hb_parni( 2 ) );
   }
}

/*
 * void removeItem ( QGraphicsLayoutItem * item )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_REMOVEITEM )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->removeItem( hbqt_par_QGraphicsLayoutItem( 2 ) );
   }
}

/*
 * void setAlignment ( QGraphicsLayoutItem * item, Qt::Alignment alignment )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETALIGNMENT )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->setAlignment( hbqt_par_QGraphicsLayoutItem( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
   }
}

/*
 * void setItemSpacing ( int index, qreal spacing )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETITEMSPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->setItemSpacing( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setOrientation ( Qt::Orientation orientation )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETORIENTATION )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
   }
}

/*
 * void setSpacing ( qreal spacing )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETSPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->setSpacing( hb_parnd( 2 ) );
   }
}

/*
 * void setStretchFactor ( QGraphicsLayoutItem * item, int stretch )
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SETSTRETCHFACTOR )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      ( p )->setStretchFactor( hbqt_par_QGraphicsLayoutItem( 2 ), hb_parni( 3 ) );
   }
}

/*
 * qreal spacing () const
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_SPACING )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->spacing() );
   }
}

/*
 * int stretchFactor ( QGraphicsLayoutItem * item ) const
 */
HB_FUNC( QT_QGRAPHICSLINEARLAYOUT_STRETCHFACTOR )
{
   QGraphicsLinearLayout * p = hbqt_par_QGraphicsLinearLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->stretchFactor( hbqt_par_QGraphicsLayoutItem( 2 ) ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
