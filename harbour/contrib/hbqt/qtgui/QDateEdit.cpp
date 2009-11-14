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

#include <QtCore/QPointer>

#include <QtGui/QDateEdit>


/*
 * QDateEdit ( QWidget * parent = 0 )
 * QDateEdit ( const QDate & date, QWidget * parent = 0 )
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QDateEdit > pq;
} QGC_POINTER_QDateEdit;

QT_G_FUNC( release_QDateEdit )
{
   QGC_POINTER_QDateEdit * p = ( QGC_POINTER_QDateEdit * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QDateEdit                    p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QDateEdit                   ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QDateEdit * ) p->ph )->~QDateEdit();
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QDateEdit                   Object deleted!" ) );
         #if defined(__debug__)
            just_debug( "  YES release_QDateEdit                   %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QDateEdit                   Object Name Missing!" ) );
         #if defined(__debug__)
            just_debug( "  NO  release_QDateEdit" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QDateEdit                   Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QDateEdit" );
      #endif
   }
}

void * gcAllocate_QDateEdit( void * pObj )
{
   QGC_POINTER_QDateEdit * p = ( QGC_POINTER_QDateEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_QDateEdit ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QDateEdit;
   new( & p->pq ) QPointer< QDateEdit >( ( QDateEdit * ) pObj );
   #if defined(__debug__)
      just_debug( "          new_QDateEdit                   %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QDATEEDIT )
{
   void * pObj = NULL;

   pObj = ( QDateEdit* ) new QDateEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QDateEdit( pObj ) );
}

/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
