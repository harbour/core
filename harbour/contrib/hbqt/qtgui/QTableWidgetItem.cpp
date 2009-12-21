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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "../hbqt.h"

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

QT_G_FUNC( hbqt_gcRelease_QTableWidgetItem )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QTableWidgetItem             p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QTableWidgetItem            ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QTableWidgetItem * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES hbqt_gcRelease_QTableWidgetItem            Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QTableWidgetItem            Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QTableWidgetItem( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QTableWidgetItem;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QTableWidgetItem            %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QTABLEWIDGETITEM )
{
   void * pObj = NULL;

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

   hb_retptrGC( hbqt_gcAllocate_QTableWidgetItem( pObj ) );
}
/*
 * QBrush background () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_BACKGROUND )
{
   hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( hbqt_par_QTableWidgetItem( 1 )->background() ) ) );
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_CHECKSTATE )
{
   hb_retni( ( Qt::CheckState ) hbqt_par_QTableWidgetItem( 1 )->checkState() );
}

/*
 * virtual QTableWidgetItem * clone () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_CLONE )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidgetItem( 1 )->clone() );
}

/*
 * int column () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_COLUMN )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->column() );
}

/*
 * virtual QVariant data ( int role ) const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_DATA )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QTableWidgetItem( 1 )->data( hb_parni( 2 ) ) ) ) );
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_FLAGS )
{
   hb_retni( ( Qt::ItemFlags ) hbqt_par_QTableWidgetItem( 1 )->flags() );
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_FONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QTableWidgetItem( 1 )->font() ) ) );
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_FOREGROUND )
{
   hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( hbqt_par_QTableWidgetItem( 1 )->foreground() ) ) );
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QTableWidgetItem( 1 )->icon() ) ) );
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ISSELECTED )
{
   hb_retl( hbqt_par_QTableWidgetItem( 1 )->isSelected() );
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_READ )
{
   hbqt_par_QTableWidgetItem( 1 )->read( *hbqt_par_QDataStream( 2 ) );
}

/*
 * int row () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ROW )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->row() );
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETBACKGROUND )
{
   hbqt_par_QTableWidgetItem( 1 )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETCHECKSTATE )
{
   hbqt_par_QTableWidgetItem( 1 )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
}

/*
 * virtual void setData ( int role, const QVariant & value )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETDATA )
{
   hbqt_par_QTableWidgetItem( 1 )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETFLAGS )
{
   hbqt_par_QTableWidgetItem( 1 )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETFONT )
{
   hbqt_par_QTableWidgetItem( 1 )->setFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETFOREGROUND )
{
   hbqt_par_QTableWidgetItem( 1 )->setForeground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETICON )
{
   hbqt_par_QTableWidgetItem( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setSelected ( bool select )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETSELECTED )
{
   hbqt_par_QTableWidgetItem( 1 )->setSelected( hb_parl( 2 ) );
}

/*
 * void setSizeHint ( const QSize & size )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETSIZEHINT )
{
   hbqt_par_QTableWidgetItem( 1 )->setSizeHint( *hbqt_par_QSize( 2 ) );
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETSTATUSTIP )
{
   hbqt_par_QTableWidgetItem( 1 )->setStatusTip( hbqt_par_QString( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETTEXT )
{
   hbqt_par_QTableWidgetItem( 1 )->setText( hbqt_par_QString( 2 ) );
}

/*
 * void setTextAlignment ( int alignment )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETTEXTALIGNMENT )
{
   hbqt_par_QTableWidgetItem( 1 )->setTextAlignment( hb_parni( 2 ) );
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETTOOLTIP )
{
   hbqt_par_QTableWidgetItem( 1 )->setToolTip( hbqt_par_QString( 2 ) );
}

/*
 * void setWhatsThis ( const QString & whatsThis )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETWHATSTHIS )
{
   hbqt_par_QTableWidgetItem( 1 )->setWhatsThis( hbqt_par_QString( 2 ) );
}

/*
 * QSize sizeHint () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SIZEHINT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QTableWidgetItem( 1 )->sizeHint() ) ) );
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_STATUSTIP )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->statusTip().toAscii().data() );
}

/*
 * QTableWidget * tableWidget () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TABLEWIDGET )
{
   hb_retptr( ( QTableWidget* ) hbqt_par_QTableWidgetItem( 1 )->tableWidget() );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TEXT )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->text().toAscii().data() );
}

/*
 * int textAlignment () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TEXTALIGNMENT )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->textAlignment() );
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TOOLTIP )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->toolTip().toAscii().data() );
}

/*
 * int type () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TYPE )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->type() );
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_WHATSTHIS )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->whatsThis().toAscii().data() );
}

/*
 * virtual void write ( QDataStream & out ) const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_WRITE )
{
   hbqt_par_QTableWidgetItem( 1 )->write( *hbqt_par_QDataStream( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
