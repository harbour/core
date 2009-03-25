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

#include <QtGui/QMenuBar>

/*----------------------------------------------------------------------*/
/*
QMenuBar ( QWidget * parent = 0 )
*/
HB_FUNC( QT_QMENUBAR )
{
   hb_retptr( ( QMenuBar* ) new QMenuBar( hbqt_par_QWidget( 1 ) ) );
}

/*
QAction * activeAction () const
*/
HB_FUNC( QT_QMENUBAR_ACTIVEACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenuBar( 1 )->activeAction() );
}

HB_FUNC( QT_QMENUBAR_ADDACTION )
{
   if( hb_pcount() == 2 && hb_param( 2, HB_IT_STRING ) )
      hb_retptr( (QAction *) hbqt_par_QMenuBar( 1 )->addAction( hbqt_par_QString( 2 ) ) );
   else if( hb_pcount() == 2 && hb_param( 2, HB_IT_POINTER ) )
      hbqt_par_QMenuBar( 1 )->addAction ( hbqt_par_QAction( 2 ) );
   else
      hb_retptr( NULL );
}

/*
QAction * addAction ( const QString & text )
*/
HB_FUNC( QT_QMENUBAR_ADDACTION_1 )
{
   hb_retptr( (QAction *) hbqt_par_QMenuBar( 1 )->addAction( hbqt_par_QString( 2 ) ) );
}

/*
void addAction ( QAction * action )
*/
HB_FUNC( QT_QMENUBAR_ADDACTION_2 )
{
   hbqt_par_QMenuBar( 1 )->addAction ( hbqt_par_QAction( 2 ) );
}

HB_FUNC( QT_QMENUBAR_ADDMENU )
{
   if( hb_pcount() == 2 && hb_param( 2, HB_IT_POINTER ) )
      hb_retptr( ( QAction* ) hbqt_par_QMenuBar( 1 )->addMenu( hbqt_par_QMenu( 2 ) ) );
   else if( hb_pcount() == 2 && hb_param( 2, HB_IT_STRING ) )
      hb_retptr( (QMenu *) hbqt_par_QMenuBar( 1 )->addMenu( hbqt_par_QString( 2 ) ) );
   else if( hb_pcount() == 3 && hb_param( 2, HB_IT_STRING ) && hb_param( 3, HB_IT_STRING ) )
      hb_retptr( ( QMenu* ) hbqt_par_QMenuBar( 1 )->addMenu( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
   else
      hb_retptr( NULL );
}

/*
QAction * addMenu ( QMenu * menu )
*/
HB_FUNC( QT_QMENUBAR_ADDMENU_1 )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenuBar( 1 )->addMenu( hbqt_par_QMenu( 2 ) ) );
}

/*
QMenu * addMenu ( const QString & title )
*/
HB_FUNC( QT_QMENUBAR_ADDMENU_2 )
{
   hb_retptr( (QMenu *) hbqt_par_QMenuBar( 1 )->addMenu( hbqt_par_QString( 2 ) ) );
}

/*
QMenu * addMenu ( const QIcon & icon, const QString & title )
*/
HB_FUNC( QT_QMENUBAR_ADDMENU_3 )
{
   hb_retptr( ( QMenu* ) hbqt_par_QMenuBar( 1 )->addMenu( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}

/*
QAction * addSeparator ()
*/
HB_FUNC( QT_QMENUBAR_ADDSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenuBar( 1 )->addSeparator() );
}

/*
void clear ()
*/
HB_FUNC( QT_QMENUBAR_CLEAR )
{
   hbqt_par_QMenuBar( 1 )->clear();
}

/*
QAction * insertMenu ( QAction * before, QMenu * menu )
*/
HB_FUNC( QT_QMENUBAR_INSERTMENU )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenuBar( 1 )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ) );
}

/*
QAction * insertSeparator ( QAction * before )
*/
HB_FUNC( QT_QMENUBAR_INSERTSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenuBar( 1 )->insertSeparator( hbqt_par_QAction( 2 ) ) );
}

/*
bool isDefaultUp () const
*/
HB_FUNC( QT_QMENUBAR_ISDEFAULTUP )
{
   hb_retl( hbqt_par_QMenuBar( 1 )->isDefaultUp() );
}

/*
void setActiveAction ( QAction * act )
*/
HB_FUNC( QT_QMENUBAR_SETACTIVEACTION )
{
   hbqt_par_QMenuBar( 1 )->setActiveAction( hbqt_par_QAction( 2 ) );
}

/*
void setDefaultUp ( bool )
*/
HB_FUNC( QT_QMENUBAR_SETDEFAULTUP )
{
   hbqt_par_QMenuBar( 1 )->setDefaultUp( hb_parl(2) );
}

/*----------------------------------------------------------------------*/
#endif
