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

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 12/13 [ 92.31% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QAction *> actions () const
 */

#include <QtCore/QPointer>

#include <QtGui/QActionGroup>


/* QActionGroup ( QObject * parent )
 * ~QActionGroup ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QActionGroup > pq;
} QGC_POINTER_QActionGroup;

QT_G_FUNC( hbqt_gcRelease_QActionGroup )
{
   QGC_POINTER_QActionGroup * p = ( QGC_POINTER_QActionGroup * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QActionGroup   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QActionGroup * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QActionGroup   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QActionGroupph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QActionGroup    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QActionGroup    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QActionGroup( void * pObj, bool bNew )
{
   QGC_POINTER_QActionGroup * p = ( QGC_POINTER_QActionGroup * ) hb_gcAllocate( sizeof( QGC_POINTER_QActionGroup ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QActionGroup;

   if( bNew )
   {
      new( & p->pq ) QPointer< QActionGroup >( ( QActionGroup * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QActionGroup               ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QACTIONGROUP )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QActionGroup( hbqt_par_QObject( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QActionGroup( pObj, true ) );
}

/*
 * QAction * addAction ( QAction * action )
 */
HB_FUNC( QT_QACTIONGROUP_ADDACTION )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QActionGroup( 1 )->addAction( hbqt_par_QAction( 2 ) ), false ) );
}

/*
 * QAction * addAction ( const QString & text )
 */
HB_FUNC( QT_QACTIONGROUP_ADDACTION_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QActionGroup( 1 )->addAction( QActionGroup::tr( hb_parc( 2 ) ) ), false ) );
}

/*
 * QAction * addAction ( const QIcon & icon, const QString & text )
 */
HB_FUNC( QT_QACTIONGROUP_ADDACTION_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QActionGroup( 1 )->addAction( QIcon( hbqt_par_QString( 2 ) ), QActionGroup::tr( hb_parc( 3 ) ) ), false ) );
}

/*
 * QAction * checkedAction () const
 */
HB_FUNC( QT_QACTIONGROUP_CHECKEDACTION )
{
   hb_retptrGC( hbqt_gcAllocate_QAction( hbqt_par_QActionGroup( 1 )->checkedAction(), false ) );
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QACTIONGROUP_ISENABLED )
{
   hb_retl( hbqt_par_QActionGroup( 1 )->isEnabled() );
}

/*
 * bool isExclusive () const
 */
HB_FUNC( QT_QACTIONGROUP_ISEXCLUSIVE )
{
   hb_retl( hbqt_par_QActionGroup( 1 )->isExclusive() );
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QACTIONGROUP_ISVISIBLE )
{
   hb_retl( hbqt_par_QActionGroup( 1 )->isVisible() );
}

/*
 * void removeAction ( QAction * action )
 */
HB_FUNC( QT_QACTIONGROUP_REMOVEACTION )
{
   hbqt_par_QActionGroup( 1 )->removeAction( hbqt_par_QAction( 2 ) );
}

/*
 * void setDisabled ( bool b )
 */
HB_FUNC( QT_QACTIONGROUP_SETDISABLED )
{
   hbqt_par_QActionGroup( 1 )->setDisabled( hb_parl( 2 ) );
}

/*
 * void setEnabled ( bool )
 */
HB_FUNC( QT_QACTIONGROUP_SETENABLED )
{
   hbqt_par_QActionGroup( 1 )->setEnabled( hb_parl( 2 ) );
}

/*
 * void setExclusive ( bool )
 */
HB_FUNC( QT_QACTIONGROUP_SETEXCLUSIVE )
{
   hbqt_par_QActionGroup( 1 )->setExclusive( hb_parl( 2 ) );
}

/*
 * void setVisible ( bool )
 */
HB_FUNC( QT_QACTIONGROUP_SETVISIBLE )
{
   hbqt_par_QActionGroup( 1 )->setVisible( hb_parl( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
