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

#if QT_VERSION >= 0x040500

#include <QtGui/QToolBar>
#include <QtGui/QIcon>

/*----------------------------------------------------------------------*/

HB_FUNC( QT_QTOOLBAR )
{
   if( hb_param( 1, HB_IT_STRING ) )
      hb_retptr( ( QToolBar* ) new QToolBar( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) );
   else
      hb_retptr( ( QToolBar* ) new QToolBar( hbqt_par_QWidget( 1 ) ) );
}

/*
QToolBar ( const QString & title, QWidget * parent = 0 ) [1]
QToolBar ( QWidget * parent = 0 ) [2]
*/
HB_FUNC( QT_QTOOLBAR_1 )
{
   hb_retptr( ( QToolBar* ) new QToolBar( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) );
}

HB_FUNC( QT_QTOOLBAR_2 )
{
   hb_retptr( ( QToolBar* ) new QToolBar( hbqt_par_QWidget( 1 ) ) );
}

/*
QAction * actionAt ( int x, int y ) const
*/
HB_FUNC( QT_QTOOLBAR_ACTIONAT_1 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->actionAt( hb_parni(2), hb_parni(3) ) );
}

/*
void addAction ( QAction * action )
*/
HB_FUNC( QT_QTOOLBAR_ADDACTION_1 )
{
   hbqt_par_QToolBar( 1 )->addAction( hbqt_par_QAction( 2 ) );
}

/*
QAction * addAction ( const QString & text )
*/
HB_FUNC( QT_QTOOLBAR_ADDACTION_2 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addAction( hbqt_par_QString( 2 ) ) );
}

/*
QAction * addAction ( const QIcon & icon, const QString & text )
*/
HB_FUNC( QT_QTOOLBAR_ADDACTION_3 )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}

/*
QAction * addSeparator ()
*/
HB_FUNC( QT_QTOOLBAR_ADDSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addSeparator() );
}

/*
QAction * addWidget ( QWidget * widget )
*/
HB_FUNC( QT_QTOOLBAR_ADDWIDGET )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->addWidget( hbqt_par_QWidget( 2 ) ) );
}

/*
Qt::ToolBarAreas allowedAreas () const
*/
HB_FUNC( QT_QTOOLBAR_ALLOWEDAREAS )
{
   hb_retni( hbqt_par_QToolBar( 1 )->allowedAreas() );
}

/*
void clear ()
*/
HB_FUNC( QT_QTOOLBAR_CLEAR )
{
   hbqt_par_QToolBar( 1 )->clear();
}

/*
QAction * insertSeparator ( QAction * before )
*/
HB_FUNC( QT_QTOOLBAR_INSERTSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->insertSeparator( hbqt_par_QAction( 2 ) ) );
}

/*
QAction * insertWidget ( QAction * before, QWidget * widget )
*/
HB_FUNC( QT_QTOOLBAR_INSERTWIDGET )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->insertWidget( hbqt_par_QAction( 2 ), hbqt_par_QWidget( 3 ) ) );
}

/*
bool isAreaAllowed( Qt::ToolBarArea area ) const
*/
HB_FUNC( QT_QTOOLBAR_ISAREAALLOWED )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isAreaAllowed( ( Qt::ToolBarArea ) hb_parni(2) ) );
}

/*
bool isFloatable () const
*/
HB_FUNC( QT_QTOOLBAR_ISFLOATABLE )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isFloatable() );
}

/*
bool isFloating () const
*/
HB_FUNC( QT_QTOOLBAR_ISFLOATING )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isFloating() );
}

/*
bool isMovable () const
*/
HB_FUNC( QT_QTOOLBAR_ISMOVABLE )
{
   hb_retl( hbqt_par_QToolBar( 1 )->isMovable() );
}

/*
Qt::Orientation orientation () const
*/
HB_FUNC( QT_QTOOLBAR_ORIENTATION )
{
   hb_retni( hbqt_par_QToolBar( 1 )->orientation() );
}

/*
void setAllowedAreas( Qt::ToolBarAreas areas )
*/
HB_FUNC( QT_QTOOLBAR_SETALLOWEDAREAS )
{
   hbqt_par_QToolBar( 1 )->setAllowedAreas( ( Qt::ToolBarAreas ) hb_parni(2) );
}

/*
void setFloatable ( bool floatable )
*/
HB_FUNC( QT_QTOOLBAR_SETFLOATABLE )
{
   hbqt_par_QToolBar( 1 )->setFloatable( hb_parl(2) );
}

/*
void setMovable ( bool movable )
*/
HB_FUNC( QT_QTOOLBAR_SETMOVABLE )
{
   hbqt_par_QToolBar( 1 )->setMovable( hb_parl(2) );
}

/*
void setOrientation ( Qt::Orientation orientation )
*/
HB_FUNC( QT_QTOOLBAR_SETORIENTATION )
{
   hbqt_par_QToolBar( 1 )->setOrientation( ( Qt::Orientation ) hb_parni(2) );
}

/*
QAction * toggleViewAction () const
*/
HB_FUNC( QT_QTOOLBAR_TOGGLEVIEWACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QToolBar( 1 )->toggleViewAction() );
}

/*
Qt::ToolButtonStyle toolButtonStyle () const
*/
HB_FUNC( QT_QTOOLBAR_TOOLBUTTONSTYLE )
{
   hb_retni( hbqt_par_QToolBar( 1 )->toolButtonStyle() );
}

/*
QWidget * widgetForAction ( QAction * action ) const
*/
HB_FUNC( QT_QTOOLBAR_WIDGETFORACTION )
{
   hb_retptr( ( QWidget* ) hbqt_par_QToolBar( 1 )->widgetForAction( hbqt_par_QAction( 2 ) ) );
}

/*
void setToolButtonStyle ( Qt::ToolButtonStyle toolButtonStyle )
*/
HB_FUNC( QT_QTOOLBAR_SETTOOLBUTTONSTYLE )
{
   hbqt_par_QToolBar( 1 )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni(2) );
}

/*----------------------------------------------------------------------*/
#endif
