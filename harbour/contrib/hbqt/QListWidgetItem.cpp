/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtGui/QListWidgetItem>


/*
 * QListWidgetItem ( QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QString & text, QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QIcon & icon, const QString & text, QListWidget * parent = 0, int type = Type )
 * QListWidgetItem ( const QListWidgetItem & other )
 * virtual ~QListWidgetItem ()
 */
HB_FUNC( QT_QLISTWIDGETITEM )
{
   hb_retptr( new QListWidgetItem( hbqt_par_QListWidget( 1 ), hb_parni( 2 ) ) );
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_BACKGROUND )
{
   hb_retptr( new QBrush( hbqt_par_QListWidgetItem( 1 )->background() ) );
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_CHECKSTATE )
{
   hb_retni( ( Qt::CheckState ) hbqt_par_QListWidgetItem( 1 )->checkState() );
}

/*
 * virtual QListWidgetItem * clone () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_CLONE )
{
   hb_retptr( ( QListWidgetItem* ) hbqt_par_QListWidgetItem( 1 )->clone() );
}

/*
 * virtual QVariant data ( int role ) const
 */
HB_FUNC( QT_QLISTWIDGETITEM_DATA )
{
   hb_retptr( new QVariant( hbqt_par_QListWidgetItem( 1 )->data( hb_parni( 2 ) ) ) );
}

/*
 * Qt::ItemFlags flags () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_FLAGS )
{
   hb_retni( ( Qt::ItemFlags ) hbqt_par_QListWidgetItem( 1 )->flags() );
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_FONT )
{
   hb_retptr( new QFont( hbqt_par_QListWidgetItem( 1 )->font() ) );
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_FOREGROUND )
{
   hb_retptr( new QBrush( hbqt_par_QListWidgetItem( 1 )->foreground() ) );
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_ICON )
{
   hb_retptr( new QIcon( hbqt_par_QListWidgetItem( 1 )->icon() ) );
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_ISHIDDEN )
{
   hb_retl( hbqt_par_QListWidgetItem( 1 )->isHidden() );
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_ISSELECTED )
{
   hb_retl( hbqt_par_QListWidgetItem( 1 )->isSelected() );
}

/*
 * QListWidget * listWidget () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_LISTWIDGET )
{
   hb_retptr( ( QListWidget* ) hbqt_par_QListWidgetItem( 1 )->listWidget() );
}

/*
 * virtual void read ( QDataStream & in )
 */
HB_FUNC( QT_QLISTWIDGETITEM_READ )
{
   hbqt_par_QListWidgetItem( 1 )->read( *hbqt_par_QDataStream( 2 ) );
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETBACKGROUND )
{
   hbqt_par_QListWidgetItem( 1 )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETCHECKSTATE )
{
   hbqt_par_QListWidgetItem( 1 )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
}

/*
 * virtual void setData ( int role, const QVariant & value )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETDATA )
{
   hbqt_par_QListWidgetItem( 1 )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/*
 * void setFlags ( Qt::ItemFlags flags )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETFLAGS )
{
   hbqt_par_QListWidgetItem( 1 )->setFlags( ( Qt::ItemFlags ) hb_parni( 2 ) );
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETFONT )
{
   hbqt_par_QListWidgetItem( 1 )->setFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETFOREGROUND )
{
   hbqt_par_QListWidgetItem( 1 )->setForeground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setHidden ( bool hide )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETHIDDEN )
{
   hbqt_par_QListWidgetItem( 1 )->setHidden( hb_parl( 2 ) );
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETICON )
{
   hbqt_par_QListWidgetItem( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setSelected ( bool select )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETSELECTED )
{
   hbqt_par_QListWidgetItem( 1 )->setSelected( hb_parl( 2 ) );
}

/*
 * void setSizeHint ( const QSize & size )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETSIZEHINT )
{
   hbqt_par_QListWidgetItem( 1 )->setSizeHint( *hbqt_par_QSize( 2 ) );
}

/*
 * void setStatusTip ( const QString & statusTip )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETSTATUSTIP )
{
   hbqt_par_QListWidgetItem( 1 )->setStatusTip( hbqt_par_QString( 2 ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETTEXT )
{
   hbqt_par_QListWidgetItem( 1 )->setText( hbqt_par_QString( 2 ) );
}

/*
 * void setTextAlignment ( int alignment )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETTEXTALIGNMENT )
{
   hbqt_par_QListWidgetItem( 1 )->setTextAlignment( hb_parni( 2 ) );
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETTOOLTIP )
{
   hbqt_par_QListWidgetItem( 1 )->setToolTip( hbqt_par_QString( 2 ) );
}

/*
 * void setWhatsThis ( const QString & whatsThis )
 */
HB_FUNC( QT_QLISTWIDGETITEM_SETWHATSTHIS )
{
   hbqt_par_QListWidgetItem( 1 )->setWhatsThis( hbqt_par_QString( 2 ) );
}

/*
 * QSize sizeHint () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_SIZEHINT )
{
   hb_retptr( new QSize( hbqt_par_QListWidgetItem( 1 )->sizeHint() ) );
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_STATUSTIP )
{
   hb_retc( hbqt_par_QListWidgetItem( 1 )->statusTip().toLatin1().data() );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TEXT )
{
   hb_retc( hbqt_par_QListWidgetItem( 1 )->text().toLatin1().data() );
}

/*
 * int textAlignment () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TEXTALIGNMENT )
{
   hb_retni( hbqt_par_QListWidgetItem( 1 )->textAlignment() );
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TOOLTIP )
{
   hb_retc( hbqt_par_QListWidgetItem( 1 )->toolTip().toLatin1().data() );
}

/*
 * int type () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_TYPE )
{
   hb_retni( hbqt_par_QListWidgetItem( 1 )->type() );
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QLISTWIDGETITEM_WHATSTHIS )
{
   hb_retc( hbqt_par_QListWidgetItem( 1 )->whatsThis().toLatin1().data() );
}

/*
 * virtual void write ( QDataStream & out ) const
 */
HB_FUNC( QT_QLISTWIDGETITEM_WRITE )
{
   hbqt_par_QListWidgetItem( 1 )->write( *hbqt_par_QDataStream( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

