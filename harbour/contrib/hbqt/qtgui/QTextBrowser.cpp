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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QTextBrowser>


/*
 * QTextBrowser ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QTextBrowser > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextBrowser;

QT_G_FUNC( hbqt_gcRelease_QTextBrowser )
{
   QTextBrowser  * ph = NULL ;
   QGC_POINTER_QTextBrowser * p = ( QGC_POINTER_QTextBrowser * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextBrowser   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextBrowser   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTextBrowser          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextBrowser    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextBrowser    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBrowser( void * pObj, bool bNew )
{
   QGC_POINTER_QTextBrowser * p = ( QGC_POINTER_QTextBrowser * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextBrowser ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextBrowser >( ( QTextBrowser * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBrowser;
   p->type = QT_TYPE_QTextBrowser;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextBrowser  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextBrowser", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTBROWSER )
{
   QTextBrowser * pObj = NULL;

   pObj =  new QTextBrowser( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextBrowser( ( void * ) pObj, true ) );
}

/*
 * int backwardHistoryCount () const
 */
HB_FUNC( QT_QTEXTBROWSER_BACKWARDHISTORYCOUNT )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retni( ( p )->backwardHistoryCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_BACKWARDHISTORYCOUNT FP=hb_retni( ( p )->backwardHistoryCount() ); p is NULL" ) );
   }
}

/*
 * void clearHistory ()
 */
HB_FUNC( QT_QTEXTBROWSER_CLEARHISTORY )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->clearHistory();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_CLEARHISTORY FP=( p )->clearHistory(); p is NULL" ) );
   }
}

/*
 * int forwardHistoryCount () const
 */
HB_FUNC( QT_QTEXTBROWSER_FORWARDHISTORYCOUNT )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retni( ( p )->forwardHistoryCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_FORWARDHISTORYCOUNT FP=hb_retni( ( p )->forwardHistoryCount() ); p is NULL" ) );
   }
}

/*
 * QString historyTitle ( int i ) const
 */
HB_FUNC( QT_QTEXTBROWSER_HISTORYTITLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retc( ( p )->historyTitle( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_HISTORYTITLE FP=hb_retc( ( p )->historyTitle( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QUrl historyUrl ( int i ) const
 */
HB_FUNC( QT_QTEXTBROWSER_HISTORYURL )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->historyUrl( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_HISTORYURL FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->historyUrl( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isBackwardAvailable () const
 */
HB_FUNC( QT_QTEXTBROWSER_ISBACKWARDAVAILABLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->isBackwardAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_ISBACKWARDAVAILABLE FP=hb_retl( ( p )->isBackwardAvailable() ); p is NULL" ) );
   }
}

/*
 * bool isForwardAvailable () const
 */
HB_FUNC( QT_QTEXTBROWSER_ISFORWARDAVAILABLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->isForwardAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_ISFORWARDAVAILABLE FP=hb_retl( ( p )->isForwardAvailable() ); p is NULL" ) );
   }
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QTEXTBROWSER_LOADRESOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_LOADRESOURCE FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool openExternalLinks () const
 */
HB_FUNC( QT_QTEXTBROWSER_OPENEXTERNALLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->openExternalLinks() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_OPENEXTERNALLINKS FP=hb_retl( ( p )->openExternalLinks() ); p is NULL" ) );
   }
}

/*
 * bool openLinks () const
 */
HB_FUNC( QT_QTEXTBROWSER_OPENLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retl( ( p )->openLinks() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_OPENLINKS FP=hb_retl( ( p )->openLinks() ); p is NULL" ) );
   }
}

/*
 * QStringList searchPaths () const
 */
HB_FUNC( QT_QTEXTBROWSER_SEARCHPATHS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_SEARCHPATHS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setOpenExternalLinks ( bool open )
 */
HB_FUNC( QT_QTEXTBROWSER_SETOPENEXTERNALLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setOpenExternalLinks( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_SETOPENEXTERNALLINKS FP=( p )->setOpenExternalLinks( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOpenLinks ( bool open )
 */
HB_FUNC( QT_QTEXTBROWSER_SETOPENLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setOpenLinks( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_SETOPENLINKS FP=( p )->setOpenLinks( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSearchPaths ( const QStringList & paths )
 */
HB_FUNC( QT_QTEXTBROWSER_SETSEARCHPATHS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setSearchPaths( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_SETSEARCHPATHS FP=( p )->setSearchPaths( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * QUrl source () const
 */
HB_FUNC( QT_QTEXTBROWSER_SOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->source() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_SOURCE FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->source() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void backward ()
 */
HB_FUNC( QT_QTEXTBROWSER_BACKWARD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->backward();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_BACKWARD FP=( p )->backward(); p is NULL" ) );
   }
}

/*
 * virtual void forward ()
 */
HB_FUNC( QT_QTEXTBROWSER_FORWARD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->forward();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_FORWARD FP=( p )->forward(); p is NULL" ) );
   }
}

/*
 * virtual void home ()
 */
HB_FUNC( QT_QTEXTBROWSER_HOME )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->home();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_HOME FP=( p )->home(); p is NULL" ) );
   }
}

/*
 * virtual void reload ()
 */
HB_FUNC( QT_QTEXTBROWSER_RELOAD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->reload();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_RELOAD FP=( p )->reload(); p is NULL" ) );
   }
}

/*
 * virtual void setSource ( const QUrl & name )
 */
HB_FUNC( QT_QTEXTBROWSER_SETSOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
      ( p )->setSource( *hbqt_par_QUrl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBROWSER_SETSOURCE FP=( p )->setSource( *hbqt_par_QUrl( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
