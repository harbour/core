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

/*
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtWebKit/QWebHistoryInterface>


/*
 * QWebHistoryInterface ( QObject * parent = 0 )
 * ~QWebHistoryInterface ()
 */

typedef struct
{
   QPointer< QWebHistoryInterface > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWebHistoryInterface;

HBQT_GC_FUNC( hbqt_gcRelease_QWebHistoryInterface )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebHistoryInterface( void * pObj, bool bNew )
{
   HBQT_GC_T_QWebHistoryInterface * p = ( HBQT_GC_T_QWebHistoryInterface * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWebHistoryInterface ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWebHistoryInterface >( ( QWebHistoryInterface * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebHistoryInterface;
   p->type = HBQT_TYPE_QWebHistoryInterface;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWebHistoryInterface  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWebHistoryInterface", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWEBHISTORYINTERFACE )
{
   //hb_retptr( ( QWebHistoryInterface* ) new QWebHistoryInterface( hbqt_par_QObject( 1 ) ) );
}

/*
 * virtual void addHistoryEntry ( const QString & url ) = 0
 */
HB_FUNC( QT_QWEBHISTORYINTERFACE_ADDHISTORYENTRY )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
   {
      void * pText;
      ( p )->addHistoryEntry( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * virtual bool historyContains ( const QString & url ) const = 0
 */
HB_FUNC( QT_QWEBHISTORYINTERFACE_HISTORYCONTAINS )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->historyContains( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QWebHistoryInterface * defaultInterface ()
 */
HB_FUNC( QT_QWEBHISTORYINTERFACE_DEFAULTINTERFACE )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWebHistoryInterface( ( p )->defaultInterface(), false ) );
   }
}

/*
 * void setDefaultInterface ( QWebHistoryInterface * defaultInterface )
 */
HB_FUNC( QT_QWEBHISTORYINTERFACE_SETDEFAULTINTERFACE )
{
   QWebHistoryInterface * p = hbqt_par_QWebHistoryInterface( 1 );
   if( p )
   {
      ( p )->setDefaultInterface( hbqt_par_QWebHistoryInterface( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
