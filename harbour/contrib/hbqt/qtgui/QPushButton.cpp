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

#include <QtCore/QPointer>

#include <QtGui/QPushButton>


/*
 * QPushButton ( QWidget * parent = 0 )
 * QPushButton ( const QString & text, QWidget * parent = 0 )
 * QPushButton ( const QIcon & icon, const QString & text, QWidget * parent = 0 )
 * ~QPushButton ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QPushButton > pq;
} QGC_POINTER_QPushButton;

QT_G_FUNC( hbqt_gcRelease_QPushButton )
{
   QGC_POINTER_QPushButton * p = ( QGC_POINTER_QPushButton * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QPushButton * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QPushButton                ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QPushButton                ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QPushButton                 Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QPushButton                 Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPushButton( void * pObj, bool bNew )
{
   QGC_POINTER_QPushButton * p = ( QGC_POINTER_QPushButton * ) hb_gcAllocate( sizeof( QGC_POINTER_QPushButton ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPushButton;

   if( bNew )
   {
      new( & p->pq ) QPointer< QPushButton >( ( QPushButton * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QPushButton                ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QPUSHBUTTON )
{
   void * pObj = NULL;

    pObj = ( QPushButton* ) new QPushButton( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QPushButton( pObj, true ) );
}
/*
 * bool autoDefault () const
 */
HB_FUNC( QT_QPUSHBUTTON_AUTODEFAULT )
{
   hb_retl( hbqt_par_QPushButton( 1 )->autoDefault() );
}

/*
 * bool isDefault () const
 */
HB_FUNC( QT_QPUSHBUTTON_ISDEFAULT )
{
   hb_retl( hbqt_par_QPushButton( 1 )->isDefault() );
}

/*
 * bool isFlat () const
 */
HB_FUNC( QT_QPUSHBUTTON_ISFLAT )
{
   hb_retl( hbqt_par_QPushButton( 1 )->isFlat() );
}

/*
 * QMenu * menu () const
 */
HB_FUNC( QT_QPUSHBUTTON_MENU )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QPushButton( 1 )->menu(), false ) );
}

/*
 * void setAutoDefault ( bool )
 */
HB_FUNC( QT_QPUSHBUTTON_SETAUTODEFAULT )
{
   hbqt_par_QPushButton( 1 )->setAutoDefault( hb_parl( 2 ) );
}

/*
 * void setDefault ( bool )
 */
HB_FUNC( QT_QPUSHBUTTON_SETDEFAULT )
{
   hbqt_par_QPushButton( 1 )->setDefault( hb_parl( 2 ) );
}

/*
 * void setFlat ( bool )
 */
HB_FUNC( QT_QPUSHBUTTON_SETFLAT )
{
   hbqt_par_QPushButton( 1 )->setFlat( hb_parl( 2 ) );
}

/*
 * void setMenu ( QMenu * menu )
 */
HB_FUNC( QT_QPUSHBUTTON_SETMENU )
{
   hbqt_par_QPushButton( 1 )->setMenu( hbqt_par_QMenu( 2 ) );
}

/*
 * void showMenu ()
 */
HB_FUNC( QT_QPUSHBUTTON_SHOWMENU )
{
   hbqt_par_QPushButton( 1 )->showMenu();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
