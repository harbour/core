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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QMenuBar>


/*
 * QMenuBar ( QWidget * parent = 0 )
 * ~QMenuBar ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QMenuBar > pq;
} QGC_POINTER_QMenuBar;

QT_G_FUNC( hbqt_gcRelease_QMenuBar )
{
   QGC_POINTER_QMenuBar * p = ( QGC_POINTER_QMenuBar * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QMenuBar   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QMenuBar * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QMenuBar   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QMenuBarph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QMenuBar    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QMenuBar    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMenuBar( void * pObj, bool bNew )
{
   QGC_POINTER_QMenuBar * p = ( QGC_POINTER_QMenuBar * ) hb_gcAllocate( sizeof( QGC_POINTER_QMenuBar ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMenuBar;

   if( bNew )
   {
      new( & p->pq ) QPointer< QMenuBar >( ( QMenuBar * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QMenuBar                   ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QMENUBAR )
{
   void * pObj = NULL;

   pObj = ( QMenuBar* ) new QMenuBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QMenuBar( pObj, true ) );
}

/*
 * QAction * activeAction () const
 */
HB_FUNC( QT_QMENUBAR_ACTIVEACTION )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QMenuBar( 1 )->activeAction(), false ) );
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QMENUBAR_ADDACTION )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QMenuBar( 1 )->addAction( QMenuBar::tr( hb_parc( 2 ) ) ), false ) );
}

/*
 * QAction * addAction ( const QString & text, const QObject * receiver, const char * member )
 */
HB_FUNC( QT_QMENUBAR_ADDACTION_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QMenuBar( 1 )->addAction( QMenuBar::tr( hb_parc( 2 ) ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) ), false ) );
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QMENUBAR_ADDACTION_2 )
{
   hbqt_par_QMenuBar( 1 )->addAction( hbqt_par_QAction( 2 ) );
}

/*
 * QAction * addMenu ( QMenu * menu )
 */
HB_FUNC( QT_QMENUBAR_ADDMENU )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QMenuBar( 1 )->addMenu( hbqt_par_QMenu( 2 ) ), false ) );
}

/*
 * QMenu * addMenu ( const QString & title )
 */
HB_FUNC( QT_QMENUBAR_ADDMENU_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QMenuBar( 1 )->addMenu( QMenuBar::tr( hb_parc( 2 ) ) ), false ) );
}

/*
 * QMenu * addMenu ( const QIcon & icon, const QString & title )
 */
HB_FUNC( QT_QMENUBAR_ADDMENU_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QMenuBar( 1 )->addMenu( QIcon( hbqt_par_QString( 2 ) ), QMenuBar::tr( hb_parc( 3 ) ) ), false ) );
}

/*
 * QAction * addSeparator ()
 */
HB_FUNC( QT_QMENUBAR_ADDSEPARATOR )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QMenuBar( 1 )->addSeparator(), false ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QMENUBAR_CLEAR )
{
   hbqt_par_QMenuBar( 1 )->clear();
}

/*
 * QAction * insertMenu ( QAction * before, QMenu * menu )
 */
HB_FUNC( QT_QMENUBAR_INSERTMENU )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QMenuBar( 1 )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ), false ) );
}

/*
 * QAction * insertSeparator ( QAction * before )
 */
HB_FUNC( QT_QMENUBAR_INSERTSEPARATOR )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QMenuBar( 1 )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
}

/*
 * bool isDefaultUp () const
 */
HB_FUNC( QT_QMENUBAR_ISDEFAULTUP )
{
   hb_retl( hbqt_par_QMenuBar( 1 )->isDefaultUp() );
}

/*
 * void setActiveAction ( QAction * act )
 */
HB_FUNC( QT_QMENUBAR_SETACTIVEACTION )
{
   hbqt_par_QMenuBar( 1 )->setActiveAction( hbqt_par_QAction( 2 ) );
}

/*
 * void setDefaultUp ( bool )
 */
HB_FUNC( QT_QMENUBAR_SETDEFAULTUP )
{
   hbqt_par_QMenuBar( 1 )->setDefaultUp( hb_parl( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
