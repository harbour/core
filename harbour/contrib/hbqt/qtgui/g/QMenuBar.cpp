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

#include <QtCore/QPointer>

#include <QtGui/QMenuBar>


/*
 * QMenuBar ( QWidget * parent = 0 )
 * ~QMenuBar ()
 */

typedef struct
{
   QPointer< QMenuBar > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMenuBar;

HBQT_GC_FUNC( hbqt_gcRelease_QMenuBar )
{
   QMenuBar  * ph = NULL ;
   HBQT_GC_T_QMenuBar * p = ( HBQT_GC_T_QMenuBar * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMenuBar   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMenuBar   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMenuBar          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMenuBar    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMenuBar    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMenuBar( void * pObj, bool bNew )
{
   HBQT_GC_T_QMenuBar * p = ( HBQT_GC_T_QMenuBar * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMenuBar ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMenuBar >( ( QMenuBar * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMenuBar;
   p->type = HBQT_TYPE_QMenuBar;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMenuBar  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMenuBar", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMENUBAR )
{
   QMenuBar * pObj = NULL;

   pObj =  new QMenuBar( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QMenuBar( ( void * ) pObj, true ) );
}

/*
 * QAction * activeAction () const
 */
HB_FUNC( QT_QMENUBAR_ACTIVEACTION )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->activeAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ACTIVEACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->activeAction(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QMENUBAR_ADDACTION )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenuBar::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ADDACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenuBar::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QString & text, const QObject * receiver, const char * member )
 */
HB_FUNC( QT_QMENUBAR_ADDACTION_1 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenuBar::tr( hb_parc( 2 ) ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ADDACTION_1 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QMenuBar::tr( hb_parc( 2 ) ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QMENUBAR_ADDACTION_2 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->addAction( hbqt_par_QAction( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ADDACTION_2 FP=( p )->addAction( hbqt_par_QAction( 2 ) ); p is NULL" ) );
   }
}

/*
 * QAction * addMenu ( QMenu * menu )
 */
HB_FUNC( QT_QMENUBAR_ADDMENU )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addMenu( hbqt_par_QMenu( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ADDMENU FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addMenu( hbqt_par_QMenu( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QMenu * addMenu ( const QString & title )
 */
HB_FUNC( QT_QMENUBAR_ADDMENU_1 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( QMenuBar::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ADDMENU_1 FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( QMenuBar::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QMenu * addMenu ( const QIcon & icon, const QString & title )
 */
HB_FUNC( QT_QMENUBAR_ADDMENU_2 )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenuBar::tr( hb_parc( 3 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ADDMENU_2 FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->addMenu( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QMenuBar::tr( hb_parc( 3 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addSeparator ()
 */
HB_FUNC( QT_QMENUBAR_ADDSEPARATOR )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ADDSEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addSeparator(), false ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QMENUBAR_CLEAR )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * QAction * insertMenu ( QAction * before, QMenu * menu )
 */
HB_FUNC( QT_QMENUBAR_INSERTMENU )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_INSERTMENU FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertMenu( hbqt_par_QAction( 2 ), hbqt_par_QMenu( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * insertSeparator ( QAction * before )
 */
HB_FUNC( QT_QMENUBAR_INSERTSEPARATOR )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_INSERTSEPARATOR FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->insertSeparator( hbqt_par_QAction( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * bool isDefaultUp () const
 */
HB_FUNC( QT_QMENUBAR_ISDEFAULTUP )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      hb_retl( ( p )->isDefaultUp() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_ISDEFAULTUP FP=hb_retl( ( p )->isDefaultUp() ); p is NULL" ) );
   }
}

/*
 * void setActiveAction ( QAction * act )
 */
HB_FUNC( QT_QMENUBAR_SETACTIVEACTION )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->setActiveAction( hbqt_par_QAction( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_SETACTIVEACTION FP=( p )->setActiveAction( hbqt_par_QAction( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultUp ( bool )
 */
HB_FUNC( QT_QMENUBAR_SETDEFAULTUP )
{
   QMenuBar * p = hbqt_par_QMenuBar( 1 );
   if( p )
      ( p )->setDefaultUp( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMENUBAR_SETDEFAULTUP FP=( p )->setDefaultUp( hb_parl( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
