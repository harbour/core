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
 *  enum DialogCode { Accepted, Rejected }
 */

#include <QtCore/QPointer>

#include  <QtGui/QDialog>


/*
 * QDialog ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QDialog ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QDialog > pq;
} QGC_POINTER_QDialog;

QT_G_FUNC( hbqt_gcRelease_QDialog )
{
   QGC_POINTER_QDialog * p = ( QGC_POINTER_QDialog * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QDialog * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QDialog                    ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QDialog                    ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QDialog                     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QDialog                     Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDialog( void * pObj, bool bNew )
{
   QGC_POINTER_QDialog * p = ( QGC_POINTER_QDialog * ) hb_gcAllocate( sizeof( QGC_POINTER_QDialog ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDialog;

   if( bNew )
   {
      new( & p->pq ) QPointer< QDialog >( ( QDialog * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QDialog                    ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QDIALOG )
{
   void * pObj = NULL;

   pObj = new QDialog( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDialog( pObj, true ) );
}
/*
 * bool isSizeGripEnabled () const
 */
HB_FUNC( QT_QDIALOG_ISSIZEGRIPENABLED )
{
   hb_retl( hbqt_par_QDialog( 1 )->isSizeGripEnabled() );
}

/*
 * int result () const
 */
HB_FUNC( QT_QDIALOG_RESULT )
{
   hb_retni( hbqt_par_QDialog( 1 )->result() );
}

/*
 * void setModal ( bool modal )
 */
HB_FUNC( QT_QDIALOG_SETMODAL )
{
   hbqt_par_QDialog( 1 )->setModal( hb_parl( 2 ) );
}

/*
 * void setResult ( int i )
 */
HB_FUNC( QT_QDIALOG_SETRESULT )
{
   hbqt_par_QDialog( 1 )->setResult( hb_parni( 2 ) );
}

/*
 * void setSizeGripEnabled ( bool )
 */
HB_FUNC( QT_QDIALOG_SETSIZEGRIPENABLED )
{
   hbqt_par_QDialog( 1 )->setSizeGripEnabled( hb_parl( 2 ) );
}

/*
 * virtual void accept ()
 */
HB_FUNC( QT_QDIALOG_ACCEPT )
{
   hbqt_par_QDialog( 1 )->accept();
}

/*
 * virtual void done ( int r )
 */
HB_FUNC( QT_QDIALOG_DONE )
{
   hbqt_par_QDialog( 1 )->done( hb_parni( 2 ) );
}

/*
 * int exec ()
 */
HB_FUNC( QT_QDIALOG_EXEC )
{
   hb_retni( hbqt_par_QDialog( 1 )->exec() );
}

/*
 * void open ()
 */
HB_FUNC( QT_QDIALOG_OPEN )
{
   hbqt_par_QDialog( 1 )->open();
}

/*
 * virtual void reject ()
 */
HB_FUNC( QT_QDIALOG_REJECT )
{
   hbqt_par_QDialog( 1 )->reject();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
