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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QButtonGroup>


/*
 * QButtonGroup ( QObject * parent = 0 )
 * ~QButtonGroup ()
 */

typedef struct
{
   QPointer< QButtonGroup > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QButtonGroup;

QT_G_FUNC( hbqt_gcRelease_QButtonGroup )
{
   QButtonGroup  * ph = NULL ;
   QGC_POINTER_QButtonGroup * p = ( QGC_POINTER_QButtonGroup * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QButtonGroup   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QButtonGroup   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QButtonGroup          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QButtonGroup    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QButtonGroup    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QButtonGroup( void * pObj, bool bNew )
{
   QGC_POINTER_QButtonGroup * p = ( QGC_POINTER_QButtonGroup * ) hb_gcAllocate( sizeof( QGC_POINTER_QButtonGroup ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QButtonGroup >( ( QButtonGroup * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QButtonGroup;
   p->type = HBQT_TYPE_QButtonGroup;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QButtonGroup  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QButtonGroup", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QBUTTONGROUP )
{
   QButtonGroup * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QButtonGroup( hbqt_par_QObject( 1 ) ) ;
   }
   else
   {
      pObj =  new QButtonGroup() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QButtonGroup( ( void * ) pObj, true ) );
}

/*
 * void addButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QBUTTONGROUP_ADDBUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->addButton( hbqt_par_QAbstractButton( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_ADDBUTTON FP=( p )->addButton( hbqt_par_QAbstractButton( 2 ) ); p is NULL" ) );
   }
}

/*
 * void addButton ( QAbstractButton * button, int id )
 */
HB_FUNC( QT_QBUTTONGROUP_ADDBUTTON_1 )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->addButton( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_ADDBUTTON_1 FP=( p )->addButton( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * QAbstractButton * button ( int id ) const
 */
HB_FUNC( QT_QBUTTONGROUP_BUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->button( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_BUTTON FP=hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->button( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QList<QAbstractButton *> buttons () const
 */
HB_FUNC( QT_QBUTTONGROUP_BUTTONS )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAbstractButton *>( ( p )->buttons() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_BUTTONS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAbstractButton *>( ( p )->buttons() ), true ) ); p is NULL" ) );
   }
}

/*
 * QAbstractButton * checkedButton () const
 */
HB_FUNC( QT_QBUTTONGROUP_CHECKEDBUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->checkedButton(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_CHECKEDBUTTON FP=hb_retptrGC( hbqt_gcAllocate_QAbstractButton( ( p )->checkedButton(), false ) ); p is NULL" ) );
   }
}

/*
 * int checkedId () const
 */
HB_FUNC( QT_QBUTTONGROUP_CHECKEDID )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retni( ( p )->checkedId() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_CHECKEDID FP=hb_retni( ( p )->checkedId() ); p is NULL" ) );
   }
}

/*
 * bool exclusive () const
 */
HB_FUNC( QT_QBUTTONGROUP_EXCLUSIVE )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retl( ( p )->exclusive() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_EXCLUSIVE FP=hb_retl( ( p )->exclusive() ); p is NULL" ) );
   }
}

/*
 * int id ( QAbstractButton * button ) const
 */
HB_FUNC( QT_QBUTTONGROUP_ID )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      hb_retni( ( p )->id( hbqt_par_QAbstractButton( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_ID FP=hb_retni( ( p )->id( hbqt_par_QAbstractButton( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void removeButton ( QAbstractButton * button )
 */
HB_FUNC( QT_QBUTTONGROUP_REMOVEBUTTON )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->removeButton( hbqt_par_QAbstractButton( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_REMOVEBUTTON FP=( p )->removeButton( hbqt_par_QAbstractButton( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setExclusive ( bool )
 */
HB_FUNC( QT_QBUTTONGROUP_SETEXCLUSIVE )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->setExclusive( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_SETEXCLUSIVE FP=( p )->setExclusive( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setId ( QAbstractButton * button, int id )
 */
HB_FUNC( QT_QBUTTONGROUP_SETID )
{
   QButtonGroup * p = hbqt_par_QButtonGroup( 1 );
   if( p )
      ( p )->setId( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QBUTTONGROUP_SETID FP=( p )->setId( hbqt_par_QAbstractButton( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
