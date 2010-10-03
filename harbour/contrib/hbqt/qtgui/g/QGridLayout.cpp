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
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGridLayout>


/*
 * QGridLayout ( QWidget * parent )
 * QGridLayout ()
 * ~QGridLayout ()
 */

typedef struct
{
   QPointer< QGridLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGridLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QGridLayout )
{
   QGridLayout  * ph = NULL ;
   HBQT_GC_T_QGridLayout * p = ( HBQT_GC_T_QGridLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGridLayout   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QGridLayout   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QGridLayout          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGridLayout    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGridLayout    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGridLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QGridLayout * p = ( HBQT_GC_T_QGridLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGridLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGridLayout >( ( QGridLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGridLayout;
   p->type = HBQT_TYPE_QGridLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGridLayout  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGridLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRIDLAYOUT )
{
   QGridLayout * pObj = NULL;

   pObj = new QGridLayout( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QGridLayout( ( void * ) pObj, true ) );
}

/*
 * void addItem ( QLayoutItem * item, int row, int column, int rowSpan = 1, int columnSpan = 1, Qt::Alignment alignment = 0 )   [*D=1*]
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDITEM )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addItem( hbqt_par_QLayoutItem( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 1 ), hb_parnidef( 6, 1 ), ( Qt::Alignment ) hb_parni( 7 ) );
   }
}

/*
 * void addLayout ( QLayout * layout, int row, int column, Qt::Alignment alignment = 0 )   [*D=1*]
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDLAYOUT )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addLayout( hbqt_par_QLayout( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
   }
}

/*
 * void addLayout ( QLayout * layout, int row, int column, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )   [*D=1*]
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDLAYOUT_1 )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addLayout( hbqt_par_QLayout( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( Qt::Alignment ) hb_parni( 7 ) );
   }
}

/*
 * void addWidget ( QWidget * widget, int row, int column, Qt::Alignment alignment = 0 )   [*D=1*]
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDWIDGET )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
   }
}

/*
 * void addWidget ( QWidget * widget, int fromRow, int fromColumn, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )   [*D=1*]
 */
HB_FUNC( QT_QGRIDLAYOUT_ADDWIDGET_1 )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addWidget( hbqt_par_QWidget( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( Qt::Alignment ) hb_parni( 7 ) );
   }
}

/*
 * QRect cellRect ( int row, int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_CELLRECT )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cellRect( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QGRIDLAYOUT_COLUMNCOUNT )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->columnCount() );
   }
}

/*
 * int columnMinimumWidth ( int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_COLUMNMINIMUMWIDTH )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->columnMinimumWidth( hb_parni( 2 ) ) );
   }
}

/*
 * int columnStretch ( int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_COLUMNSTRETCH )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->columnStretch( hb_parni( 2 ) ) );
   }
}

/*
 * void getItemPosition ( int index, int * row, int * column, int * rowSpan, int * columnSpan )
 */
HB_FUNC( QT_QGRIDLAYOUT_GETITEMPOSITION )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   int iRow = 0;
   int iColumn = 0;
   int iRowSpan = 0;
   int iColumnSpan = 0;

   if( p )
   {
      ( p )->getItemPosition( hb_parni( 2 ), &iRow, &iColumn, &iRowSpan, &iColumnSpan );
   }

   hb_storni( iRow, 3 );
   hb_storni( iColumn, 4 );
   hb_storni( iRowSpan, 5 );
   hb_storni( iColumnSpan, 6 );
}

/*
 * int horizontalSpacing () const
 */
HB_FUNC( QT_QGRIDLAYOUT_HORIZONTALSPACING )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->horizontalSpacing() );
   }
}

/*
 * QLayoutItem * itemAtPosition ( int row, int column ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_ITEMATPOSITION )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLayoutItem( ( p )->itemAtPosition( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * Qt::Corner originCorner () const
 */
HB_FUNC( QT_QGRIDLAYOUT_ORIGINCORNER )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( Qt::Corner ) ( p )->originCorner() );
   }
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QGRIDLAYOUT_ROWCOUNT )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->rowCount() );
   }
}

/*
 * int rowMinimumHeight ( int row ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_ROWMINIMUMHEIGHT )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->rowMinimumHeight( hb_parni( 2 ) ) );
   }
}

/*
 * int rowStretch ( int row ) const
 */
HB_FUNC( QT_QGRIDLAYOUT_ROWSTRETCH )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->rowStretch( hb_parni( 2 ) ) );
   }
}

/*
 * void setColumnMinimumWidth ( int column, int minSize )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETCOLUMNMINIMUMWIDTH )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnMinimumWidth( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setColumnStretch ( int column, int stretch )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETCOLUMNSTRETCH )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnStretch( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setHorizontalSpacing ( int spacing )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETHORIZONTALSPACING )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setHorizontalSpacing( hb_parni( 2 ) );
   }
}

/*
 * void setOriginCorner ( Qt::Corner corner )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETORIGINCORNER )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setOriginCorner( ( Qt::Corner ) hb_parni( 2 ) );
   }
}

/*
 * void setRowMinimumHeight ( int row, int minSize )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETROWMINIMUMHEIGHT )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setRowMinimumHeight( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setRowStretch ( int row, int stretch )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETROWSTRETCH )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setRowStretch( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setSpacing ( int spacing )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETSPACING )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setSpacing( hb_parni( 2 ) );
   }
}

/*
 * void setVerticalSpacing ( int spacing )
 */
HB_FUNC( QT_QGRIDLAYOUT_SETVERTICALSPACING )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      ( p )->setVerticalSpacing( hb_parni( 2 ) );
   }
}

/*
 * int spacing () const
 */
HB_FUNC( QT_QGRIDLAYOUT_SPACING )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->spacing() );
   }
}

/*
 * int verticalSpacing () const
 */
HB_FUNC( QT_QGRIDLAYOUT_VERTICALSPACING )
{
   QGridLayout * p = hbqt_par_QGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->verticalSpacing() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
