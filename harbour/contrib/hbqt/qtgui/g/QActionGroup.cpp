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

#include <QtGui/QActionGroup>


/* QActionGroup ( QObject * parent )
 * ~QActionGroup ()
 */

typedef struct
{
   QPointer< QActionGroup > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QActionGroup;

HBQT_GC_FUNC( hbqt_gcRelease_QActionGroup )
{
   QActionGroup  * ph = NULL ;
   HBQT_GC_T_QActionGroup * p = ( HBQT_GC_T_QActionGroup * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QActionGroup   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QActionGroup   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QActionGroup          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QActionGroup    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QActionGroup    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QActionGroup( void * pObj, bool bNew )
{
   HBQT_GC_T_QActionGroup * p = ( HBQT_GC_T_QActionGroup * ) hb_gcAllocate( sizeof( HBQT_GC_T_QActionGroup ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QActionGroup >( ( QActionGroup * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QActionGroup;
   p->type = HBQT_TYPE_QActionGroup;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QActionGroup  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QActionGroup", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QACTIONGROUP )
{
   QActionGroup * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QActionGroup( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QActionGroup( ( void * ) pObj, true ) );
}

/*
 * QList<QAction *> actions () const
 */
HB_FUNC( QT_QACTIONGROUP_ACTIONS )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAction *>( ( p )->actions() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_ACTIONS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAction *>( ( p )->actions() ), true ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( QAction * action )
 */
HB_FUNC( QT_QACTIONGROUP_ADDACTION )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hbqt_par_QAction( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_ADDACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( hbqt_par_QAction( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QACTIONGROUP_ADDACTION_1 )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QActionGroup::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_ADDACTION_1 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( QActionGroup::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QACTIONGROUP_ADDACTION_2 )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QActionGroup::tr( hb_parc( 3 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_ADDACTION_2 FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->addAction( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ), QActionGroup::tr( hb_parc( 3 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * checkedAction () const
 */
HB_FUNC( QT_QACTIONGROUP_CHECKEDACTION )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->checkedAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_CHECKEDACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->checkedAction(), false ) ); p is NULL" ) );
   }
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QACTIONGROUP_ISENABLED )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_ISENABLED FP=hb_retl( ( p )->isEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isExclusive () const
 */
HB_FUNC( QT_QACTIONGROUP_ISEXCLUSIVE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retl( ( p )->isExclusive() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_ISEXCLUSIVE FP=hb_retl( ( p )->isExclusive() ); p is NULL" ) );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QACTIONGROUP_ISVISIBLE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_ISVISIBLE FP=hb_retl( ( p )->isVisible() ); p is NULL" ) );
   }
}

/*
 * void removeAction ( QAction * action )
 */
HB_FUNC( QT_QACTIONGROUP_REMOVEACTION )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->removeAction( hbqt_par_QAction( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_REMOVEACTION FP=( p )->removeAction( hbqt_par_QAction( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDisabled ( bool b )
 */
HB_FUNC( QT_QACTIONGROUP_SETDISABLED )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setDisabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_SETDISABLED FP=( p )->setDisabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEnabled ( bool )
 */
HB_FUNC( QT_QACTIONGROUP_SETENABLED )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_SETENABLED FP=( p )->setEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setExclusive ( bool )
 */
HB_FUNC( QT_QACTIONGROUP_SETEXCLUSIVE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setExclusive( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_SETEXCLUSIVE FP=( p )->setExclusive( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVisible ( bool )
 */
HB_FUNC( QT_QACTIONGROUP_SETVISIBLE )
{
   QActionGroup * p = hbqt_par_QActionGroup( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QACTIONGROUP_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
