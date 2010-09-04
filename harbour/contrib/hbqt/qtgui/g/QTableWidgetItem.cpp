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
 *  enum ItemType { Type, UserType }
 */

#include <QtCore/QPointer>

#include <QtGui/QTableWidgetItem>


/*
 * QTableWidgetItem ( int type = Type )
 * QTableWidgetItem ( const QString & text, int type = Type )
 * QTableWidgetItem ( const QIcon & icon, const QString & text, int type = Type )
 * QTableWidgetItem ( const QTableWidgetItem & other )
 * virtual ~QTableWidgetItem ()
 */

typedef struct
{
   QTableWidgetItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTableWidgetItem;

HBQT_GC_FUNC( hbqt_gcRelease_QTableWidgetItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTableWidgetItem   /.\\", p->ph ) );
         delete ( ( QTableWidgetItem * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTableWidgetItem   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTableWidgetItem    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTableWidgetItem    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTableWidgetItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTableWidgetItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTableWidgetItem;
   p->type = HBQT_TYPE_QTableWidgetItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTableWidgetItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTableWidgetItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTABLEWIDGETITEM )
{
   QTableWidgetItem * pObj = NULL;

   if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QTableWidgetItem( hbqt_par_QString( 1 ), hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new QTableWidgetItem( hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QTableWidgetItem( *hbqt_par_QTableWidgetItem( 1 ) ) ;
   }
   else
   {
      pObj = new QTableWidgetItem( 0 ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( void * ) pObj, true ) );
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_BACKGROUND )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_BACKGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_CHECKSTATE )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::CheckState ) ( p )->checkState() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_CHECKSTATE FP=hb_retni( ( Qt::CheckState ) ( p )->checkState() ); p is NULL" ) );
   }
}

/*
 * virtual QTableWidgetItem * clone () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_CLONE )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->clone(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_CLONE FP=hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( ( p )->clone(), false ) ); p is NULL" ) );
   }
}

/*
 * int column () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_COLUMN )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->column() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_COLUMN FP=hb_retni( ( p )->column() ); p is NULL" ) );
   }
}

/*
 * virtual QVariant data ( int role ) const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_DATA )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_DATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_FLAGS )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retni( ( Qt::ItemFlags ) ( p )->flags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_FLAGS FP=hb_retni( ( Qt::ItemFlags ) ( p )->flags() ); p is NULL" ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_FONT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_FOREGROUND )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_FOREGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) ); p is NULL" ) );
   }
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ICON )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_ICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->icon() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ISSELECTED )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retl( ( p )->isSelected() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_ISSELECTED FP=hb_retl( ( p )->isSelected() ); p is NULL" ) );
   }
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_READ )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->read( *hbqt_par_QDataStream( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_READ FP=( p )->read( *hbqt_par_QDataStream( 2 ) ); p is NULL" ) );
   }
}

/*
 * int row () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ROW )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->row() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_ROW FP=hb_retni( ( p )->row() ); p is NULL" ) );
   }
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETBACKGROUND )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETBACKGROUND FP=( p )->setBackground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETCHECKSTATE )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETCHECKSTATE FP=( p )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setData ( int role, const QVariant & value )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETDATA )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETDATA FP=( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETFLAGS )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETFLAGS FP=( p )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETFONT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETFOREGROUND )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setForeground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETFOREGROUND FP=( p )->setForeground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETICON )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETICON FP=( p )->setIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setSelected ( bool select )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETSELECTED )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setSelected( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETSELECTED FP=( p )->setSelected( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeHint ( const QSize & size )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETSIZEHINT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setSizeHint( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETSIZEHINT FP=( p )->setSizeHint( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETSTATUSTIP )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setStatusTip( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETSTATUSTIP FP=( p )->setStatusTip( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETTEXT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setText( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETTEXT FP=( p )->setText( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextAlignment ( int alignment )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETTEXTALIGNMENT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setTextAlignment( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETTEXTALIGNMENT FP=( p )->setTextAlignment( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETTOOLTIP )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setToolTip( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETTOOLTIP FP=( p )->setToolTip( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWhatsThis ( const QString & whatsThis )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETWHATSTHIS )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->setWhatsThis( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SETWHATSTHIS FP=( p )->setWhatsThis( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSize sizeHint () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SIZEHINT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_STATUSTIP )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->statusTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_STATUSTIP FP=hb_retc( ( p )->statusTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTableWidget * tableWidget () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TABLEWIDGET )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTableWidget( ( p )->tableWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_TABLEWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QTableWidget( ( p )->tableWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TEXT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int textAlignment () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TEXTALIGNMENT )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->textAlignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_TEXTALIGNMENT FP=hb_retni( ( p )->textAlignment() ); p is NULL" ) );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TOOLTIP )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->toolTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_TOOLTIP FP=hb_retc( ( p )->toolTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int type () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TYPE )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retni( ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_TYPE FP=hb_retni( ( p )->type() ); p is NULL" ) );
   }
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_WHATSTHIS )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      hb_retc( ( p )->whatsThis().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_WHATSTHIS FP=hb_retc( ( p )->whatsThis().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual void write ( QDataStream & out ) const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_WRITE )
{
   QTableWidgetItem * p = hbqt_par_QTableWidgetItem( 1 );
   if( p )
      ( p )->write( *hbqt_par_QDataStream( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTABLEWIDGETITEM_WRITE FP=( p )->write( *hbqt_par_QDataStream( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
