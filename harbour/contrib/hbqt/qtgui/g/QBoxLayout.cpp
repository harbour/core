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
 *  enum Direction { LeftToRight, RightToLeft, TopToBottom, BottomToTop }
 */

/*
 *  Constructed[ 20/20 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QBoxLayout>


/*
 * QBoxLayout ( Direction dir, QWidget * parent = 0 )
 * ~QBoxLayout ()
 */

typedef struct
{
   QPointer< QBoxLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QBoxLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QBoxLayout )
{
   QBoxLayout  * ph = NULL ;
   HBQT_GC_T_QBoxLayout * p = ( HBQT_GC_T_QBoxLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QBoxLayout   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QBoxLayout   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QBoxLayout          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QBoxLayout    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QBoxLayout    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBoxLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QBoxLayout * p = ( HBQT_GC_T_QBoxLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QBoxLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QBoxLayout >( ( QBoxLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBoxLayout;
   p->type = HBQT_TYPE_QBoxLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QBoxLayout  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QBoxLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QBOXLAYOUT )
{
   QBoxLayout * pObj = NULL;

   pObj =  new QBoxLayout( ( QBoxLayout::Direction ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QBoxLayout( ( void * ) pObj, true ) );
}

/*
 * void addLayout ( QLayout * layout, int stretch = 0 )   [*D=1*]
 */
HB_FUNC( QT_QBOXLAYOUT_ADDLAYOUT )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->addLayout( hbqt_par_QLayout( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void addSpacerItem ( QSpacerItem * spacerItem )   [*D=1*]
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSPACERITEM )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->addSpacerItem( hbqt_par_QSpacerItem( 2 ) );
   }
}

/*
 * void addSpacing ( int size )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->addSpacing( hb_parni( 2 ) );
   }
}

/*
 * void addStretch ( int stretch = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSTRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->addStretch( hb_parni( 2 ) );
   }
}

/*
 * void addStrut ( int size )
 */
HB_FUNC( QT_QBOXLAYOUT_ADDSTRUT )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->addStrut( hb_parni( 2 ) );
   }
}

/*
 * void addWidget ( QWidget * widget, int stretch = 0, Qt::Alignment alignment = 0 )   [*D=1*]
 */
HB_FUNC( QT_QBOXLAYOUT_ADDWIDGET )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 2 );
      if( q && q->ph )
      {
         q->bNew = false;
      }
      ( p )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), ( Qt::Alignment ) hb_parni( 4 ) );
   }
}

/*
 * Direction direction () const
 */
HB_FUNC( QT_QBOXLAYOUT_DIRECTION )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hb_retni( ( QBoxLayout::Direction ) ( p )->direction() );
   }
}

/*
 * void insertLayout ( int index, QLayout * layout, int stretch = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTLAYOUT )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->insertLayout( hb_parni( 2 ), hbqt_par_QLayout( 3 ), hb_parni( 4 ) );
   }
}

/*
 * void insertSpacerItem ( int index, QSpacerItem * spacerItem )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTSPACERITEM )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->insertSpacerItem( hb_parni( 2 ), hbqt_par_QSpacerItem( 3 ) );
   }
}

/*
 * void insertSpacing ( int index, int size )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTSPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->insertSpacing( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void insertStretch ( int index, int stretch = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTSTRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->insertStretch( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void insertWidget ( int index, QWidget * widget, int stretch = 0, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QBOXLAYOUT_INSERTWIDGET )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
   }
}

/*
 * virtual void invalidate ()
 */
HB_FUNC( QT_QBOXLAYOUT_INVALIDATE )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->invalidate();
   }
}

/*
 * void setDirection ( Direction direction )
 */
HB_FUNC( QT_QBOXLAYOUT_SETDIRECTION )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->setDirection( ( QBoxLayout::Direction ) hb_parni( 2 ) );
   }
}

/*
 * void setSpacing ( int spacing )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->setSpacing( hb_parni( 2 ) );
   }
}

/*
 * void setStretch ( int index, int stretch )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      ( p )->setStretch( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * bool setStretchFactor ( QWidget * widget, int stretch )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCHFACTOR )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hb_retl( ( p )->setStretchFactor( hbqt_par_QWidget( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * bool setStretchFactor ( QLayout * layout, int stretch )
 */
HB_FUNC( QT_QBOXLAYOUT_SETSTRETCHFACTOR_1 )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hb_retl( ( p )->setStretchFactor( hbqt_par_QLayout( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QBOXLAYOUT_SPACING )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->spacing() );
   }
}

/*
 * int stretch ( int index ) const
 */
HB_FUNC( QT_QBOXLAYOUT_STRETCH )
{
   QBoxLayout * p = hbqt_par_QBoxLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->stretch( hb_parni( 2 ) ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
