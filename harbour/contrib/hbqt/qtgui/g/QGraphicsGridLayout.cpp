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

#include <QtGui/QGraphicsGridLayout>


/*
 * QGraphicsGridLayout ( QGraphicsLayoutItem * parent = 0 )
 * virtual ~QGraphicsGridLayout ()
 */

typedef struct
{
   QGraphicsGridLayout * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsGridLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsGridLayout )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QGraphicsGridLayout   /.\\", p->ph ) );
         delete ( ( QGraphicsGridLayout * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QGraphicsGridLayout   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QGraphicsGridLayout    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QGraphicsGridLayout    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsGridLayout( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsGridLayout * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsGridLayout;
   p->type = HBQT_TYPE_QGraphicsGridLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsGridLayout", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsGridLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSGRIDLAYOUT )
{
   QGraphicsGridLayout * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QGraphicsGridLayout( hbqt_par_QGraphicsLayoutItem( 1 ) ) ;
   }
   else
   {
      pObj = new QGraphicsGridLayout() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsGridLayout( ( void * ) pObj, true ) );
}

/*
 * void addItem ( QGraphicsLayoutItem * item, int row, int column, int rowSpan, int columnSpan, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ADDITEM )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->addItem( hbqt_par_QGraphicsLayoutItem( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( Qt::Alignment ) hb_parni( 7 ) );
   }
}

/*
 * void addItem ( QGraphicsLayoutItem * item, int row, int column, Qt::Alignment alignment = 0 )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ADDITEM_1 )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->addItem( hbqt_par_QGraphicsLayoutItem( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( Qt::Alignment ) hb_parni( 5 ) );
   }
}

/*
 * Qt::Alignment alignment ( QGraphicsLayoutItem * item ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ALIGNMENT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->alignment( hbqt_par_QGraphicsLayoutItem( 2 ) ) );
   }
}

/*
 * Qt::Alignment columnAlignment ( int column ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COLUMNALIGNMENT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->columnAlignment( hb_parni( 2 ) ) );
   }
}

/*
 * int columnCount () const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COLUMNCOUNT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->columnCount() );
   }
}

/*
 * qreal columnMaximumWidth ( int column ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COLUMNMAXIMUMWIDTH )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->columnMaximumWidth( hb_parni( 2 ) ) );
   }
}

/*
 * qreal columnMinimumWidth ( int column ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COLUMNMINIMUMWIDTH )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->columnMinimumWidth( hb_parni( 2 ) ) );
   }
}

/*
 * qreal columnPreferredWidth ( int column ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COLUMNPREFERREDWIDTH )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->columnPreferredWidth( hb_parni( 2 ) ) );
   }
}

/*
 * qreal columnSpacing ( int column ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COLUMNSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->columnSpacing( hb_parni( 2 ) ) );
   }
}

/*
 * int columnStretchFactor ( int column ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COLUMNSTRETCHFACTOR )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->columnStretchFactor( hb_parni( 2 ) ) );
   }
}

/*
 * virtual int count () const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_COUNT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->count() );
   }
}

/*
 * qreal horizontalSpacing () const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_HORIZONTALSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->horizontalSpacing() );
   }
}

/*
 * QGraphicsLayoutItem * itemAt ( int row, int column ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ITEMAT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLayoutItem( ( p )->itemAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * virtual QGraphicsLayoutItem * itemAt ( int index ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ITEMAT_1 )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLayoutItem( ( p )->itemAt( hb_parni( 2 ) ), false ) );
   }
}

/*
 * virtual void removeAt ( int index )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_REMOVEAT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->removeAt( hb_parni( 2 ) );
   }
}

/*
 * Qt::Alignment rowAlignment ( int row ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ROWALIGNMENT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->rowAlignment( hb_parni( 2 ) ) );
   }
}

/*
 * int rowCount () const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ROWCOUNT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->rowCount() );
   }
}

/*
 * qreal rowMaximumHeight ( int row ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ROWMAXIMUMHEIGHT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->rowMaximumHeight( hb_parni( 2 ) ) );
   }
}

/*
 * qreal rowMinimumHeight ( int row ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ROWMINIMUMHEIGHT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->rowMinimumHeight( hb_parni( 2 ) ) );
   }
}

/*
 * qreal rowPreferredHeight ( int row ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ROWPREFERREDHEIGHT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->rowPreferredHeight( hb_parni( 2 ) ) );
   }
}

/*
 * qreal rowSpacing ( int row ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ROWSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->rowSpacing( hb_parni( 2 ) ) );
   }
}

/*
 * int rowStretchFactor ( int row ) const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_ROWSTRETCHFACTOR )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->rowStretchFactor( hb_parni( 2 ) ) );
   }
}

/*
 * void setAlignment ( QGraphicsLayoutItem * item, Qt::Alignment alignment )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETALIGNMENT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setAlignment( hbqt_par_QGraphicsLayoutItem( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
   }
}

/*
 * void setColumnAlignment ( int column, Qt::Alignment alignment )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETCOLUMNALIGNMENT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnAlignment( hb_parni( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
   }
}

/*
 * void setColumnFixedWidth ( int column, qreal width )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETCOLUMNFIXEDWIDTH )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnFixedWidth( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setColumnMaximumWidth ( int column, qreal width )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETCOLUMNMAXIMUMWIDTH )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnMaximumWidth( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setColumnMinimumWidth ( int column, qreal width )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETCOLUMNMINIMUMWIDTH )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnMinimumWidth( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setColumnPreferredWidth ( int column, qreal width )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETCOLUMNPREFERREDWIDTH )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnPreferredWidth( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setColumnSpacing ( int column, qreal spacing )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETCOLUMNSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnSpacing( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setColumnStretchFactor ( int column, int stretch )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETCOLUMNSTRETCHFACTOR )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setColumnStretchFactor( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * virtual void setGeometry ( const QRectF & rect )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETGEOMETRY )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setGeometry( *hbqt_par_QRectF( 2 ) );
   }
}

/*
 * void setHorizontalSpacing ( qreal spacing )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETHORIZONTALSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setHorizontalSpacing( hb_parnd( 2 ) );
   }
}

/*
 * void setRowAlignment ( int row, Qt::Alignment alignment )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETROWALIGNMENT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setRowAlignment( hb_parni( 2 ), ( Qt::Alignment ) hb_parni( 3 ) );
   }
}

/*
 * void setRowFixedHeight ( int row, qreal height )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETROWFIXEDHEIGHT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setRowFixedHeight( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setRowMaximumHeight ( int row, qreal height )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETROWMAXIMUMHEIGHT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setRowMaximumHeight( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setRowMinimumHeight ( int row, qreal height )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETROWMINIMUMHEIGHT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setRowMinimumHeight( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setRowPreferredHeight ( int row, qreal height )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETROWPREFERREDHEIGHT )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setRowPreferredHeight( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setRowSpacing ( int row, qreal spacing )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETROWSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setRowSpacing( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setRowStretchFactor ( int row, int stretch )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETROWSTRETCHFACTOR )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setRowStretchFactor( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setSpacing ( qreal spacing )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setSpacing( hb_parnd( 2 ) );
   }
}

/*
 * void setVerticalSpacing ( qreal spacing )
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_SETVERTICALSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      ( p )->setVerticalSpacing( hb_parnd( 2 ) );
   }
}

/*
 * qreal verticalSpacing () const
 */
HB_FUNC( QT_QGRAPHICSGRIDLAYOUT_VERTICALSPACING )
{
   QGraphicsGridLayout * p = hbqt_par_QGraphicsGridLayout( 1 );
   if( p )
   {
      hb_retnd( ( p )->verticalSpacing() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
