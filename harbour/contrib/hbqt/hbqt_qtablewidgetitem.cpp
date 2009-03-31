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


/*
 *  Constructed[ 22/33 [ 66.67% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  QBrush background () const
 *  virtual QVariant data ( int role ) const
 *  QFont font () const
 *  QBrush foreground () const
 *  QIcon icon () const
 *  virtual void read ( QDataStream & in )
 *  void setBackground ( const QBrush & brush )
 *  virtual void setData ( int role, const QVariant & value )
 *  void setFont ( const QFont & font )
 *  void setForeground ( const QBrush & brush )
 *  virtual void write ( QDataStream & out ) const
 */ 


#include <QtGui/QTableWidgetItem>


/*
 * QTableWidgetItem ( int type = Type )
 * QTableWidgetItem ( const QString & text, int type = Type )
 * QTableWidgetItem ( const QIcon & icon, const QString & text, int type = Type )
 * QTableWidgetItem ( const QTableWidgetItem & other )
 * virtual ~QTableWidgetItem ()
 */
HB_FUNC( QT_QTABLEWIDGETITEM )
{
   if( hb_pcount() >= 2 && HB_ISCHAR( 1 ) )
   {
      hb_retptr( ( QTableWidgetItem* ) new QTableWidgetItem( hbqt_par_QString( 1 ), hb_parni( 2 ) ) );
   }
   else
   {
      hb_retptr( ( QTableWidgetItem* ) new QTableWidgetItem( hb_parni( 1 ) ) );
   }
   
   #if 0
   hb_retptr( ( QTableWidgetItem* ) new QTableWidgetItem( QIcon( hbqt_par_QString( 1 ) ),hbqt_par_QString( 2 ), hb_parni( 3 ) ) );
   hb_retptr( ( QTableWidgetItem* ) new QTableWidgetItem( hbqt_par_QTableWidgetItem( 1 ) ) );
   #endif
}

/*
 * Qt::CheckState checkState () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_CHECKSTATE )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->checkState(  ) );
}

/*
 * virtual QTableWidgetItem * clone () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_CLONE )
{
   hb_retptr( ( QTableWidgetItem* ) hbqt_par_QTableWidgetItem( 1 )->clone(  ) );
}

/*
 * int column () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_COLUMN )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->column(  ) );
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ISSELECTED )
{
   hb_retl( hbqt_par_QTableWidgetItem( 1 )->isSelected(  ) );
}

/*
 * int row () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_ROW )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->row(  ) );
}

/*
 * void setCheckState ( Qt::CheckState state )
 */
HB_FUNC( QT_QTABLEWIDGETITEM_SETCHECKSTATE )
{
   hbqt_par_QTableWidgetItem( 1 )->setCheckState( ( Qt::CheckState ) hb_parni( 2 ) );
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
   hbqt_par_QTableWidgetItem( 1 )->setSizeHint( hbqt_const_QSize( 2 ) );
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
   hbqt_ret_QSize( hbqt_par_QTableWidgetItem( 1 )->sizeHint(  ) );
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_STATUSTIP )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->statusTip( ).toLatin1().data() );
}

/*
 * QTableWidget * tableWidget () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TABLEWIDGET )
{
   hb_retptr( ( QTableWidget* ) hbqt_par_QTableWidgetItem( 1 )->tableWidget(  ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TEXT )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->text( ).toLatin1().data() );
}

/*
 * int textAlignment () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TEXTALIGNMENT )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->textAlignment(  ) );
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TOOLTIP )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->toolTip( ).toLatin1().data() );
}

/*
 * int type () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_TYPE )
{
   hb_retni( hbqt_par_QTableWidgetItem( 1 )->type(  ) );
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QTABLEWIDGETITEM_WHATSTHIS )
{
   hb_retc( hbqt_par_QTableWidgetItem( 1 )->whatsThis( ).toLatin1().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

