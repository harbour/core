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

#include <QtGui/QMenu>

/*----------------------------------------------------------------------*/
/*
QMenu ( QWidget * parent = 0 ) [1]
QMenu ( const QString & title, QWidget * parent = 0 ) [2]
*/
HB_FUNC( QT_QMENU )
{
  if( hb_param( 1, HB_IT_POINTER ) )
     hb_retptr( ( QMenu* ) new QMenu( hbqt_par_QWidget( 1 ) ) );
  else if( hb_param( 1, HB_IT_STRING ) && hb_param( 2, HB_IT_POINTER ) )
     hb_retptr( ( QMenu* ) new QMenu( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) );
  else
     hb_retptr( NULL );
}
#if 0
HB_FUNC( QT_QMENU_2 )
{
  hb_retptr( ( QMenu* ) new QMenu( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) );
}
#endif

/*
QAction * activeAction () const
*/
HB_FUNC( QT_QMENU_ACTIVEACTION )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->activeAction() );
}

/*
QAction * addAction ( const QString & text )
*/
HB_FUNC( QT_QMENU_ADDACTION )
{
  if( hb_pcount() == 2 && hb_param( 2, HB_IT_STRING ) )
     hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addAction( hbqt_par_QString( 2 ) ) );
  else if( hb_pcount() == 2 && hb_param( 2, HB_IT_POINTER ) )
     hbqt_par_QMenu( 1 )->addAction( hbqt_par_QAction( 2 ) );
  else if( hb_pcount() == 3 && hb_param( 2, HB_IT_STRING ) && hb_param( 3, HB_IT_STRING ) )
     hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}

#if 0
/*
QAction * addAction ( const QIcon & icon, const QString & text )
*/
HB_FUNC( QT_QMENU_ADDACTION_2 )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}

/*
void addAction ( QAction * action )
*/
HB_FUNC( QT_QMENU_ADDACTION_5 )
{
  hbqt_par_QMenu( 1 )->addAction( hbqt_par_QAction( 2 ) );
}
#endif

HB_FUNC( QT_QMENU_ADDMENU )
{
  if( hb_pcount() == 2 && hb_param( 2, HB_IT_POINTER ) )
    hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addMenu( hbqt_par_QMenu( 2 ) ) );
  else if( hb_pcount() == 2 && hb_param( 2, HB_IT_STRING ) )
    hb_retptr( ( QMenu* ) hbqt_par_QMenu( 1 )->addMenu( hbqt_par_QString( 2 ) ) );
  else if( hb_pcount() == 3 && hb_param( 2, HB_IT_STRING ) && hb_param( 3, HB_IT_STRING ) )
    hb_retptr( (QMenu *) hbqt_par_QMenu( 1 )->addMenu ( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
  else
    hb_retptr( NULL );
}

#if 0
/*
QAction * addMenu ( QMenu * menu )
*/
HB_FUNC( QT_QMENU_ADDMENU_1 )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addMenu( hbqt_par_QMenu( 2 ) ) );
}

/*
QMenu * addMenu ( const QString & title )
*/
HB_FUNC( QT_QMENU_ADDMENU_2 )
{
  hb_retptr( ( QMenu* ) hbqt_par_QMenu( 1 )->addMenu( hbqt_par_QString( 2 ) ) );
}

/*
QMenu * addMenu ( const QIcon & icon, const QString & title )
*/
HB_FUNC( QT_QMENU_ADDMENU_3 )
{
  hb_retptr( (QMenu *) hbqt_par_QMenu( 1 )->addMenu ( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}
#endif

/*
QAction * addSeparator ()
*/
HB_FUNC( QT_QMENU_ADDSEPARATOR )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addSeparator() );
}

/*
void clear ()
*/
HB_FUNC( QT_QMENU_CLEAR )
{
  hbqt_par_QMenu( 1 )->clear();
}

/*
QAction * defaultAction () const
*/
HB_FUNC( QT_QMENU_DEFAULTACTION )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->defaultAction() );
}

/*
QAction * exec ()
*/
HB_FUNC( QT_QMENU_EXEC_1 )
{
  hb_retptr( (QAction *) hbqt_par_QMenu( 1 )->exec() );
}

/*
void hideTearOffMenu ()
*/
HB_FUNC( QT_QMENU_HIDETEAROFFMENU )
{
  hbqt_par_QMenu( 1 )->hideTearOffMenu (  );
}

/*
QAction * insertMenu ( QAction * before, QMenu * menu )
*/
HB_FUNC( QT_QMENU_INSERTMENU )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ) );
}

/*
QAction * insertSeparator ( QAction * before )
*/
HB_FUNC( QT_QMENU_INSERTSEPARATOR )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->insertSeparator( hbqt_par_QAction( 2 ) ) );
}

/*
bool isEmpty () const
*/
HB_FUNC( QT_QMENU_ISEMPTY )
{
  hb_retl( hbqt_par_QMenu( 1 )->isEmpty() );
}

/*
bool isTearOffEnabled () const
*/
HB_FUNC( QT_QMENU_ISTEAROFFENABLED )
{
  hb_retl( hbqt_par_QMenu( 1 )->isTearOffEnabled() );
}

/*
bool isTearOffMenuVisible () const
*/
HB_FUNC( QT_QMENU_ISTEAROFFMENUVISIBLE )
{
  hb_retl( hbqt_par_QMenu( 1 )->isTearOffMenuVisible() );
}

/*
QAction * menuAction () const
*/
HB_FUNC( QT_QMENU_MENUACTION )
{
  hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->menuAction() );
}

/*
bool separatorsCollapsible () const
*/
HB_FUNC( QT_QMENU_SEPARATORSCOLLAPSIBLE )
{
  hb_retl( hbqt_par_QMenu( 1 )->separatorsCollapsible() );
}

/*
void setActiveAction ( QAction * act )
*/
HB_FUNC( QT_QMENU_SETACTIVEACTION )
{
  hbqt_par_QMenu( 1 )->setActiveAction( hbqt_par_QAction( 2 ) );
}

/*
void setDefaultAction ( QAction * act )
*/
HB_FUNC( QT_QMENU_SETDEFAULTACTION )
{
  hbqt_par_QMenu( 1 )->setDefaultAction( hbqt_par_QAction( 2 ) );
}

/*
void setIcon ( const QIcon & icon )
*/
HB_FUNC( QT_QMENU_SETICON )
{
  hbqt_par_QMenu( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
void setSeparatorsCollapsible ( bool collapse )
*/
HB_FUNC( QT_QMENU_SETSEPARATORSCOLLAPSIBLE )
{
  hbqt_par_QMenu( 1 )->setSeparatorsCollapsible( hb_parl( 2 ) );
}

/*
void setTearOffEnabled ( bool )
*/
HB_FUNC( QT_QMENU_SETTEAROFFENABLED )
{
  hbqt_par_QMenu( 1 )->setTearOffEnabled( hb_parl( 2 ) );
}

/*
void setTitle ( const QString & title )
*/
HB_FUNC( QT_QMENU_SETTITLE )
{
  hbqt_par_QMenu( 1 )->setTitle( hbqt_par_QString( 2 ) );
}

/*
QString title () const
*/
HB_FUNC( QT_QMENU_TITLE )
{
  hb_retc( hbqt_par_QMenu( 1 )->title().toLatin1().data() );
}

/*----------------------------------------------------------------------*/
#endif
