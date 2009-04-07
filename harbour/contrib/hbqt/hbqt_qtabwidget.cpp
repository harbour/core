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


#include <QtGui/QTabWidget>


/*
 * QTabWidget ( QWidget * parent = 0 )
 * ~QTabWidget ()
 */
HB_FUNC( QT_QTABWIDGET )
{
   hb_retptr( ( QTabWidget* ) new QTabWidget( hbqt_par_QWidget( 1 ) ) );
}

/*
 * int addTab ( QWidget * page, const QString & label )
 */
HB_FUNC( QT_QTABWIDGET_ADDTAB )
{
   hb_retni( hbqt_par_QTabWidget( 1 )->addTab( hbqt_par_QWidget( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * int addTab ( QWidget * page, const QIcon & icon, const QString & label )
 */
HB_FUNC( QT_QTABWIDGET_ADDTAB_1 )
{
   hb_retni( hbqt_par_QTabWidget( 1 )->addTab( hbqt_par_QWidget( 2 ), QIcon( hbqt_par_QString( 3 ) ), hbqt_par_QString( 4 ) ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTABWIDGET_CLEAR )
{
   hbqt_par_QTabWidget( 1 )->clear();
}

/*
 * QWidget * cornerWidget ( Qt::Corner corner = Qt::TopRightCorner ) const
 */
HB_FUNC( QT_QTABWIDGET_CORNERWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QTabWidget( 1 )->cornerWidget( ( HB_ISNIL( 2 ) ? ( Qt::Corner ) Qt::TopRightCorner : ( Qt::Corner ) hb_parni( 2 ) ) ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QTABWIDGET_COUNT )
{
   hb_retni( hbqt_par_QTabWidget( 1 )->count() );
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QTABWIDGET_CURRENTINDEX )
{
   hb_retni( hbqt_par_QTabWidget( 1 )->currentIndex() );
}

/*
 * QWidget * currentWidget () const
 */
HB_FUNC( QT_QTABWIDGET_CURRENTWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QTabWidget( 1 )->currentWidget() );
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QTABWIDGET_DOCUMENTMODE )
{
   hb_retl( hbqt_par_QTabWidget( 1 )->documentMode() );
}

/*
 * Qt::TextElideMode elideMode () const
 */
HB_FUNC( QT_QTABWIDGET_ELIDEMODE )
{
   hb_retni( ( Qt::TextElideMode ) hbqt_par_QTabWidget( 1 )->elideMode() );
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QTABWIDGET_ICONSIZE )
{
   hb_retptr( new QSize( hbqt_par_QTabWidget( 1 )->iconSize() ) );
}

/*
 * int indexOf ( QWidget * w ) const
 */
HB_FUNC( QT_QTABWIDGET_INDEXOF )
{
   hb_retni( hbqt_par_QTabWidget( 1 )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/*
 * int insertTab ( int index, QWidget * page, const QString & label )
 */
HB_FUNC( QT_QTABWIDGET_INSERTTAB )
{
   hb_retni( hbqt_par_QTabWidget( 1 )->insertTab( hb_parni( 2 ), hbqt_par_QWidget( 3 ), hbqt_par_QString( 4 ) ) );
}

/*
 * int insertTab ( int index, QWidget * page, const QIcon & icon, const QString & label )
 */
HB_FUNC( QT_QTABWIDGET_INSERTTAB_1 )
{
   hb_retni( hbqt_par_QTabWidget( 1 )->insertTab( hb_parni( 2 ), hbqt_par_QWidget( 3 ), QIcon( hbqt_par_QString( 4 ) ), hbqt_par_QString( 5 ) ) );
}

/*
 * bool isMovable () const
 */
HB_FUNC( QT_QTABWIDGET_ISMOVABLE )
{
   hb_retl( hbqt_par_QTabWidget( 1 )->isMovable() );
}

/*
 * bool isTabEnabled ( int index ) const
 */
HB_FUNC( QT_QTABWIDGET_ISTABENABLED )
{
   hb_retl( hbqt_par_QTabWidget( 1 )->isTabEnabled( hb_parni( 2 ) ) );
}

/*
 * void removeTab ( int index )
 */
HB_FUNC( QT_QTABWIDGET_REMOVETAB )
{
   hbqt_par_QTabWidget( 1 )->removeTab( hb_parni( 2 ) );
}

/*
 * void setCornerWidget ( QWidget * widget, Qt::Corner corner = Qt::TopRightCorner )
 */
HB_FUNC( QT_QTABWIDGET_SETCORNERWIDGET )
{
   hbqt_par_QTabWidget( 1 )->setCornerWidget( hbqt_par_QWidget( 2 ), ( HB_ISNIL( 3 ) ? ( Qt::Corner ) Qt::TopRightCorner : ( Qt::Corner ) hb_parni( 3 ) ) );
}

/*
 * void setDocumentMode ( bool set )
 */
HB_FUNC( QT_QTABWIDGET_SETDOCUMENTMODE )
{
   hbqt_par_QTabWidget( 1 )->setDocumentMode( hb_parl( 2 ) );
}

/*
 * void setElideMode ( Qt::TextElideMode )
 */
HB_FUNC( QT_QTABWIDGET_SETELIDEMODE )
{
   hbqt_par_QTabWidget( 1 )->setElideMode( ( Qt::TextElideMode ) hb_parni( 2 ) );
}

/*
 * void setIconSize ( const QSize & size )
 */
HB_FUNC( QT_QTABWIDGET_SETICONSIZE )
{
   hbqt_par_QTabWidget( 1 )->setIconSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setMovable ( bool movable )
 */
HB_FUNC( QT_QTABWIDGET_SETMOVABLE )
{
   hbqt_par_QTabWidget( 1 )->setMovable( hb_parl( 2 ) );
}

/*
 * void setTabEnabled ( int index, bool enable )
 */
HB_FUNC( QT_QTABWIDGET_SETTABENABLED )
{
   hbqt_par_QTabWidget( 1 )->setTabEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setTabIcon ( int index, const QIcon & icon )
 */
HB_FUNC( QT_QTABWIDGET_SETTABICON )
{
   hbqt_par_QTabWidget( 1 )->setTabIcon( hb_parni( 2 ), QIcon( hbqt_par_QString( 3 ) ) );
}

/*
 * void setTabPosition ( TabPosition )
 */
HB_FUNC( QT_QTABWIDGET_SETTABPOSITION )
{
   hbqt_par_QTabWidget( 1 )->setTabPosition( ( QTabWidget::TabPosition ) hb_parni( 2 ) );
}

/*
 * void setTabShape ( TabShape s )
 */
HB_FUNC( QT_QTABWIDGET_SETTABSHAPE )
{
   hbqt_par_QTabWidget( 1 )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) );
}

/*
 * void setTabText ( int index, const QString & label )
 */
HB_FUNC( QT_QTABWIDGET_SETTABTEXT )
{
   hbqt_par_QTabWidget( 1 )->setTabText( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setTabToolTip ( int index, const QString & tip )
 */
HB_FUNC( QT_QTABWIDGET_SETTABTOOLTIP )
{
   hbqt_par_QTabWidget( 1 )->setTabToolTip( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setTabWhatsThis ( int index, const QString & text )
 */
HB_FUNC( QT_QTABWIDGET_SETTABWHATSTHIS )
{
   hbqt_par_QTabWidget( 1 )->setTabWhatsThis( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void setTabsClosable ( bool closeable )
 */
HB_FUNC( QT_QTABWIDGET_SETTABSCLOSABLE )
{
   hbqt_par_QTabWidget( 1 )->setTabsClosable( hb_parl( 2 ) );
}

/*
 * void setUsesScrollButtons ( bool useButtons )
 */
HB_FUNC( QT_QTABWIDGET_SETUSESSCROLLBUTTONS )
{
   hbqt_par_QTabWidget( 1 )->setUsesScrollButtons( hb_parl( 2 ) );
}

/*
 * QIcon tabIcon ( int index ) const
 */
HB_FUNC( QT_QTABWIDGET_TABICON )
{
   hb_retptr( new QIcon( hbqt_par_QTabWidget( 1 )->tabIcon( hb_parni( 2 ) ) ) );
}

/*
 * TabPosition tabPosition () const
 */
HB_FUNC( QT_QTABWIDGET_TABPOSITION )
{
   hb_retni( ( QTabWidget::TabPosition ) hbqt_par_QTabWidget( 1 )->tabPosition() );
}

/*
 * TabShape tabShape () const
 */
HB_FUNC( QT_QTABWIDGET_TABSHAPE )
{
   hb_retni( ( QTabWidget::TabShape ) hbqt_par_QTabWidget( 1 )->tabShape() );
}

/*
 * QString tabText ( int index ) const
 */
HB_FUNC( QT_QTABWIDGET_TABTEXT )
{
   hb_retc( hbqt_par_QTabWidget( 1 )->tabText( hb_parni( 2 ) ).toLatin1().data() );
}

/*
 * QString tabToolTip ( int index ) const
 */
HB_FUNC( QT_QTABWIDGET_TABTOOLTIP )
{
   hb_retc( hbqt_par_QTabWidget( 1 )->tabToolTip( hb_parni( 2 ) ).toLatin1().data() );
}

/*
 * QString tabWhatsThis ( int index ) const
 */
HB_FUNC( QT_QTABWIDGET_TABWHATSTHIS )
{
   hb_retc( hbqt_par_QTabWidget( 1 )->tabWhatsThis( hb_parni( 2 ) ).toLatin1().data() );
}

/*
 * bool tabsClosable () const
 */
HB_FUNC( QT_QTABWIDGET_TABSCLOSABLE )
{
   hb_retl( hbqt_par_QTabWidget( 1 )->tabsClosable() );
}

/*
 * bool usesScrollButtons () const
 */
HB_FUNC( QT_QTABWIDGET_USESSCROLLBUTTONS )
{
   hb_retl( hbqt_par_QTabWidget( 1 )->usesScrollButtons() );
}

/*
 * QWidget * widget ( int index ) const
 */
HB_FUNC( QT_QTABWIDGET_WIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QTabWidget( 1 )->widget( hb_parni( 2 ) ) );
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QTABWIDGET_SETCURRENTINDEX )
{
   hbqt_par_QTabWidget( 1 )->setCurrentIndex( hb_parni( 2 ) );
}

/*
 * void setCurrentWidget ( QWidget * widget )
 */
HB_FUNC( QT_QTABWIDGET_SETCURRENTWIDGET )
{
   hbqt_par_QTabWidget( 1 )->setCurrentWidget( hbqt_par_QWidget( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

