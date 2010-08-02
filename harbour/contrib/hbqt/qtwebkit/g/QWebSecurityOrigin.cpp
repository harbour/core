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
#include "hbqtwebkit.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtWebKit/QWebSecurityOrigin>


/*
 * QWebSecurityOrigin ( const QWebSecurityOrigin & other )
 * ~QWebSecurityOrigin ()
 */

typedef struct
{
   QWebSecurityOrigin * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebSecurityOrigin;

HBQT_GC_FUNC( hbqt_gcRelease_QWebSecurityOrigin )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QWebSecurityOrigin   /.\\", p->ph ) );
         delete ( ( QWebSecurityOrigin * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QWebSecurityOrigin   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QWebSecurityOrigin    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QWebSecurityOrigin    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebSecurityOrigin( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWebSecurityOrigin * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebSecurityOrigin;
   p->type = HBQT_TYPE_QWebSecurityOrigin;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWebSecurityOrigin", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWebSecurityOrigin", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWEBSECURITYORIGIN )
{
   QWebSecurityOrigin * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QWebSecurityOrigin( *hbqt_par_QWebSecurityOrigin( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QWebSecurityOrigin( ( void * ) pObj, true ) );
}

/*
 * qint64 databaseQuota () const
 */
HB_FUNC( QT_QWEBSECURITYORIGIN_DATABASEQUOTA )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retnint( ( p )->databaseQuota() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSECURITYORIGIN_DATABASEQUOTA FP=hb_retnint( ( p )->databaseQuota() ); p is NULL" ) );
   }
}

/*
 * qint64 databaseUsage () const
 */
HB_FUNC( QT_QWEBSECURITYORIGIN_DATABASEUSAGE )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retnint( ( p )->databaseUsage() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSECURITYORIGIN_DATABASEUSAGE FP=hb_retnint( ( p )->databaseUsage() ); p is NULL" ) );
   }
}

/*
 * QString host () const
 */
HB_FUNC( QT_QWEBSECURITYORIGIN_HOST )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retc( ( p )->host().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSECURITYORIGIN_HOST FP=hb_retc( ( p )->host().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int port () const
 */
HB_FUNC( QT_QWEBSECURITYORIGIN_PORT )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retni( ( p )->port() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSECURITYORIGIN_PORT FP=hb_retni( ( p )->port() ); p is NULL" ) );
   }
}

/*
 * QString scheme () const
 */
HB_FUNC( QT_QWEBSECURITYORIGIN_SCHEME )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retc( ( p )->scheme().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSECURITYORIGIN_SCHEME FP=hb_retc( ( p )->scheme().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setDatabaseQuota ( qint64 quota )
 */
HB_FUNC( QT_QWEBSECURITYORIGIN_SETDATABASEQUOTA )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      ( p )->setDatabaseQuota( hb_parnint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSECURITYORIGIN_SETDATABASEQUOTA FP=( p )->setDatabaseQuota( hb_parnint( 2 ) ); p is NULL" ) );
   }
}

/*
 * QList<QWebSecurityOrigin> allOrigins ()
 */
HB_FUNC( QT_QWEBSECURITYORIGIN_ALLORIGINS )
{
   QWebSecurityOrigin * p = hbqt_par_QWebSecurityOrigin( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebSecurityOrigin>( ( p )->allOrigins() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBSECURITYORIGIN_ALLORIGINS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QWebSecurityOrigin>( ( p )->allOrigins() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
