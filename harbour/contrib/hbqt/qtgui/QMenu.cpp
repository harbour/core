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
 *  Constructed[ 33/35 [ 94.29% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  OSMenuRef macMenu ( OSMenuRef merge = 0 )
 *  HMENU wceMenu ( bool create = false )
 */

#include <QtCore/QPointer>

#include <QtGui/QMenu>


/*
 * QMenu ( QWidget * parent = 0 )
 * QMenu ( const QString & title, QWidget * parent = 0 )
 * ~QMenu ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QMenu > pq;
} QGC_POINTER_QMenu;

QT_G_FUNC( hbqt_gcRelease_QMenu )
{
   QGC_POINTER_QMenu * p = ( QGC_POINTER_QMenu * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QMenu                        p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QMenu                       ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QMenu * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QMenu * ) p->ph )->~QMenu();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QMenu * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QMenu                       Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QMenu                       Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QMenu                       Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QMenu( void * pObj )
{
   QGC_POINTER_QMenu * p = ( QGC_POINTER_QMenu * ) hb_gcAllocate( sizeof( QGC_POINTER_QMenu ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QMenu;
   new( & p->pq ) QPointer< QMenu >( ( QMenu * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QMenu                       %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QMENU )
{
   void * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISCHAR( 1 ) )
   {
      pObj = ( QMenu* ) new QMenu( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj = ( QMenu* ) new QMenu( hbqt_par_QWidget( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMenu( pObj ) );
}
/*
 * QAction * actionAt ( const QPoint & pt ) const
 */
HB_FUNC( QT_QMENU_ACTIONAT )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->actionAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QRect actionGeometry ( QAction * act ) const
 */
HB_FUNC( QT_QMENU_ACTIONGEOMETRY )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QMenu( 1 )->actionGeometry( hbqt_par_QAction( 2 ) ) ) ) );
}

/*
 * QAction * activeAction () const
 */
HB_FUNC( QT_QMENU_ACTIVEACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->activeAction() );
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QMENU_ADDACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addAction( hbqt_par_QString( 2 ) ) );
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QMENU_ADDACTION_1 )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}

/*
 * QAction * addAction ( const QString & text, const QObject * receiver, const char * member, const QKeySequence & shortcut = 0 )
 */
HB_FUNC( QT_QMENU_ADDACTION_2 )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addAction( hbqt_par_QString( 2 ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ), *hbqt_par_QKeySequence( 5 ) ) );
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text, const QObject * receiver, const char * member, const QKeySequence & shortcut = 0 )
 */
HB_FUNC( QT_QMENU_ADDACTION_3 )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ), hbqt_par_QObject( 4 ), hbqt_par_char( 5 ), *hbqt_par_QKeySequence( 6 ) ) );
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QMENU_ADDACTION_4 )
{
   hbqt_par_QMenu( 1 )->addAction( hbqt_par_QAction( 2 ) );
}

/*
 * QAction * addMenu ( QMenu * menu )
 */
HB_FUNC( QT_QMENU_ADDMENU )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addMenu( hbqt_par_QMenu( 2 ) ) );
}

/*
 * QMenu * addMenu ( const QString & title )
 */
HB_FUNC( QT_QMENU_ADDMENU_1 )
{
   hb_retptr( ( QMenu* ) hbqt_par_QMenu( 1 )->addMenu( hbqt_par_QString( 2 ) ) );
}

/*
 * QMenu * addMenu ( const QIcon & icon, const QString & title )
 */
HB_FUNC( QT_QMENU_ADDMENU_2 )
{
   hb_retptr( ( QMenu* ) hbqt_par_QMenu( 1 )->addMenu( QIcon( hbqt_par_QString( 2 ) ), hbqt_par_QString( 3 ) ) );
}

/*
 * QAction * addSeparator ()
 */
HB_FUNC( QT_QMENU_ADDSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->addSeparator() );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QMENU_CLEAR )
{
   hbqt_par_QMenu( 1 )->clear();
}

/*
 * QAction * defaultAction () const
 */
HB_FUNC( QT_QMENU_DEFAULTACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->defaultAction() );
}

/*
 * QAction * exec ()
 */
HB_FUNC( QT_QMENU_EXEC )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->exec() );
}

/*
 * QAction * exec ( const QPoint & p, QAction * action = 0 )
 */
HB_FUNC( QT_QMENU_EXEC_1 )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->exec( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) ) );
}

/*
 * void hideTearOffMenu ()
 */
HB_FUNC( QT_QMENU_HIDETEAROFFMENU )
{
   hbqt_par_QMenu( 1 )->hideTearOffMenu();
}

/*
 * QIcon icon () const
 */
HB_FUNC( QT_QMENU_ICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QMenu( 1 )->icon() ) ) );
}

/*
 * QAction * insertMenu ( QAction * before, QMenu * menu )
 */
HB_FUNC( QT_QMENU_INSERTMENU )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ) );
}

/*
 * QAction * insertSeparator ( QAction * before )
 */
HB_FUNC( QT_QMENU_INSERTSEPARATOR )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->insertSeparator( hbqt_par_QAction( 2 ) ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QMENU_ISEMPTY )
{
   hb_retl( hbqt_par_QMenu( 1 )->isEmpty() );
}

/*
 * bool isTearOffEnabled () const
 */
HB_FUNC( QT_QMENU_ISTEAROFFENABLED )
{
   hb_retl( hbqt_par_QMenu( 1 )->isTearOffEnabled() );
}

/*
 * bool isTearOffMenuVisible () const
 */
HB_FUNC( QT_QMENU_ISTEAROFFMENUVISIBLE )
{
   hb_retl( hbqt_par_QMenu( 1 )->isTearOffMenuVisible() );
}

/*
 * QAction * menuAction () const
 */
HB_FUNC( QT_QMENU_MENUACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QMenu( 1 )->menuAction() );
}

/*
 * void popup ( const QPoint & p, QAction * atAction = 0 )
 */
HB_FUNC( QT_QMENU_POPUP )
{
   hbqt_par_QMenu( 1 )->popup( *hbqt_par_QPoint( 2 ), hbqt_par_QAction( 3 ) );
}

/*
 * bool separatorsCollapsible () const
 */
HB_FUNC( QT_QMENU_SEPARATORSCOLLAPSIBLE )
{
   hb_retl( hbqt_par_QMenu( 1 )->separatorsCollapsible() );
}

/*
 * void setActiveAction ( QAction * act )
 */
HB_FUNC( QT_QMENU_SETACTIVEACTION )
{
   hbqt_par_QMenu( 1 )->setActiveAction( hbqt_par_QAction( 2 ) );
}

/*
 * void setDefaultAction ( QAction * act )
 */
HB_FUNC( QT_QMENU_SETDEFAULTACTION )
{
   hbqt_par_QMenu( 1 )->setDefaultAction( hbqt_par_QAction( 2 ) );
}

/*
 * void setIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QMENU_SETICON )
{
   hbqt_par_QMenu( 1 )->setIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setSeparatorsCollapsible ( bool collapse )
 */
HB_FUNC( QT_QMENU_SETSEPARATORSCOLLAPSIBLE )
{
   hbqt_par_QMenu( 1 )->setSeparatorsCollapsible( hb_parl( 2 ) );
}

/*
 * void setTearOffEnabled ( bool )
 */
HB_FUNC( QT_QMENU_SETTEAROFFENABLED )
{
   hbqt_par_QMenu( 1 )->setTearOffEnabled( hb_parl( 2 ) );
}

/*
 * void setTitle ( const QString & title )
 */
HB_FUNC( QT_QMENU_SETTITLE )
{
   hbqt_par_QMenu( 1 )->setTitle( hbqt_par_QString( 2 ) );
}

/*
 * QString title () const
 */
HB_FUNC( QT_QMENU_TITLE )
{
   hb_retc( hbqt_par_QMenu( 1 )->title().toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
