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

#include <QtCore/QPointer>

#include <QtGui/QLayoutItem>


/*
 * QLayoutItem ( Qt::Alignment alignment = 0 )
 * virtual ~QLayoutItem ()
 */

typedef struct
{
   QLayoutItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLayoutItem;

HBQT_GC_FUNC( hbqt_gcRelease_QLayoutItem )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLayoutItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QLayoutItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLayoutItem;
   p->type = HBQT_TYPE_QLayoutItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLayoutItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLayoutItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLAYOUTITEM )
{

}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QLAYOUTITEM_ALIGNMENT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_ALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->alignment() ); p is NULL" ) );
   }
}

/*
 * QSizePolicy::ControlTypes controlTypes () const
 */
HB_FUNC( QT_QLAYOUTITEM_CONTROLTYPES )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( QSizePolicy::ControlTypes ) ( p )->controlTypes() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_CONTROLTYPES FP=hb_retni( ( QSizePolicy::ControlTypes ) ( p )->controlTypes() ); p is NULL" ) );
   }
}

/*
 * virtual Qt::Orientations expandingDirections () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_EXPANDINGDIRECTIONS )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_EXPANDINGDIRECTIONS FP=hb_retni( ( Qt::Orientations ) ( p )->expandingDirections() ); p is NULL" ) );
   }
}

/*
 * virtual QRect geometry () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_GEOMETRY )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_GEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual bool hasHeightForWidth () const
 */
HB_FUNC( QT_QLAYOUTITEM_HASHEIGHTFORWIDTH )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retl( ( p )->hasHeightForWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_HASHEIGHTFORWIDTH FP=hb_retl( ( p )->hasHeightForWidth() ); p is NULL" ) );
   }
}

/*
 * virtual int heightForWidth ( int w ) const
 */
HB_FUNC( QT_QLAYOUTITEM_HEIGHTFORWIDTH )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( p )->heightForWidth( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_HEIGHTFORWIDTH FP=hb_retni( ( p )->heightForWidth( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void invalidate ()
 */
HB_FUNC( QT_QLAYOUTITEM_INVALIDATE )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      ( p )->invalidate();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_INVALIDATE FP=( p )->invalidate(); p is NULL" ) );
   }
}

/*
 * virtual bool isEmpty () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_ISEMPTY )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_ISEMPTY FP=hb_retl( ( p )->isEmpty() ); p is NULL" ) );
   }
}

/*
 * virtual QLayout * layout ()
 */
HB_FUNC( QT_QLAYOUTITEM_LAYOUT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->layout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_LAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->layout(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize maximumSize () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_MAXIMUMSIZE )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_MAXIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual int minimumHeightForWidth ( int w ) const
 */
HB_FUNC( QT_QLAYOUTITEM_MINIMUMHEIGHTFORWIDTH )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retni( ( p )->minimumHeightForWidth( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_MINIMUMHEIGHTFORWIDTH FP=hb_retni( ( p )->minimumHeightForWidth( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize minimumSize () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_MINIMUMSIZE )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_MINIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QLAYOUTITEM_SETALIGNMENT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_SETALIGNMENT FP=( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setGeometry ( const QRect & r ) = 0
 */
HB_FUNC( QT_QLAYOUTITEM_SETGEOMETRY )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_SETGEOMETRY FP=( p )->setGeometry( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize sizeHint () const = 0
 */
HB_FUNC( QT_QLAYOUTITEM_SIZEHINT )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QSpacerItem * spacerItem ()
 */
HB_FUNC( QT_QLAYOUTITEM_SPACERITEM )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSpacerItem( ( p )->spacerItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_SPACERITEM FP=hb_retptrGC( hbqt_gcAllocate_QSpacerItem( ( p )->spacerItem(), false ) ); p is NULL" ) );
   }
}

/*
 * virtual QWidget * widget ()
 */
HB_FUNC( QT_QLAYOUTITEM_WIDGET )
{
   QLayoutItem * p = hbqt_par_QLayoutItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLAYOUTITEM_WIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
