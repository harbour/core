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

#include <QtGui/QTextBrowser>


/*
 * QTextBrowser ( QWidget * parent = 0 )
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QTextBrowser > pq;
} QGC_POINTER_QTextBrowser;

QT_G_FUNC( release_QTextBrowser )
{
   QGC_POINTER_QTextBrowser * p = ( QGC_POINTER_QTextBrowser * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QTextBrowser                 p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QTextBrowser                ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QTextBrowser * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QTextBrowser * ) p->ph )->~QTextBrowser();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QTextBrowser * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QTextBrowser                Object deleted!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  YES release_QTextBrowser                %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QTextBrowser                Object Name Missing!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  NO  release_QTextBrowser" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QTextBrowser                Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QTextBrowser" );
      #endif
   }
}

void * gcAllocate_QTextBrowser( void * pObj )
{
   QGC_POINTER_QTextBrowser * p = ( QGC_POINTER_QTextBrowser * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextBrowser ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QTextBrowser;
   new( & p->pq ) QPointer< QTextBrowser >( ( QTextBrowser * ) pObj );
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QTextBrowser                %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QTEXTBROWSER )
{
   void * pObj = NULL;

   pObj = ( QTextBrowser* ) new QTextBrowser( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QTextBrowser( pObj ) );
}
/*
 * int backwardHistoryCount () const
 */
HB_FUNC( QT_QTEXTBROWSER_BACKWARDHISTORYCOUNT )
{
   hb_retni( hbqt_par_QTextBrowser( 1 )->backwardHistoryCount() );
}

/*
 * void clearHistory ()
 */
HB_FUNC( QT_QTEXTBROWSER_CLEARHISTORY )
{
   hbqt_par_QTextBrowser( 1 )->clearHistory();
}

/*
 * int forwardHistoryCount () const
 */
HB_FUNC( QT_QTEXTBROWSER_FORWARDHISTORYCOUNT )
{
   hb_retni( hbqt_par_QTextBrowser( 1 )->forwardHistoryCount() );
}

/*
 * QString historyTitle ( int i ) const
 */
HB_FUNC( QT_QTEXTBROWSER_HISTORYTITLE )
{
   hb_retc( hbqt_par_QTextBrowser( 1 )->historyTitle( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QUrl historyUrl ( int i ) const
 */
HB_FUNC( QT_QTEXTBROWSER_HISTORYURL )
{
   hb_retptrGC( gcAllocate_QUrl( new QUrl( hbqt_par_QTextBrowser( 1 )->historyUrl( hb_parni( 2 ) ) ) ) );
}

/*
 * bool isBackwardAvailable () const
 */
HB_FUNC( QT_QTEXTBROWSER_ISBACKWARDAVAILABLE )
{
   hb_retl( hbqt_par_QTextBrowser( 1 )->isBackwardAvailable() );
}

/*
 * bool isForwardAvailable () const
 */
HB_FUNC( QT_QTEXTBROWSER_ISFORWARDAVAILABLE )
{
   hb_retl( hbqt_par_QTextBrowser( 1 )->isForwardAvailable() );
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QTEXTBROWSER_LOADRESOURCE )
{
   hb_retptrGC( gcAllocate_QVariant( new QVariant( hbqt_par_QTextBrowser( 1 )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ) ) );
}

/*
 * bool openExternalLinks () const
 */
HB_FUNC( QT_QTEXTBROWSER_OPENEXTERNALLINKS )
{
   hb_retl( hbqt_par_QTextBrowser( 1 )->openExternalLinks() );
}

/*
 * bool openLinks () const
 */
HB_FUNC( QT_QTEXTBROWSER_OPENLINKS )
{
   hb_retl( hbqt_par_QTextBrowser( 1 )->openLinks() );
}

/*
 * QStringList searchPaths () const
 */
HB_FUNC( QT_QTEXTBROWSER_SEARCHPATHS )
{
   hb_retptrGC( gcAllocate_QStringList( new QStringList( hbqt_par_QTextBrowser( 1 )->searchPaths() ) ) );
}

/*
 * void setOpenExternalLinks ( bool open )
 */
HB_FUNC( QT_QTEXTBROWSER_SETOPENEXTERNALLINKS )
{
   hbqt_par_QTextBrowser( 1 )->setOpenExternalLinks( hb_parl( 2 ) );
}

/*
 * void setOpenLinks ( bool open )
 */
HB_FUNC( QT_QTEXTBROWSER_SETOPENLINKS )
{
   hbqt_par_QTextBrowser( 1 )->setOpenLinks( hb_parl( 2 ) );
}

/*
 * void setSearchPaths ( const QStringList & paths )
 */
HB_FUNC( QT_QTEXTBROWSER_SETSEARCHPATHS )
{
   hbqt_par_QTextBrowser( 1 )->setSearchPaths( *hbqt_par_QStringList( 2 ) );
}

/*
 * QUrl source () const
 */
HB_FUNC( QT_QTEXTBROWSER_SOURCE )
{
   hb_retptrGC( gcAllocate_QUrl( new QUrl( hbqt_par_QTextBrowser( 1 )->source() ) ) );
}

/*
 * virtual void backward ()
 */
HB_FUNC( QT_QTEXTBROWSER_BACKWARD )
{
   hbqt_par_QTextBrowser( 1 )->backward();
}

/*
 * virtual void forward ()
 */
HB_FUNC( QT_QTEXTBROWSER_FORWARD )
{
   hbqt_par_QTextBrowser( 1 )->forward();
}

/*
 * virtual void home ()
 */
HB_FUNC( QT_QTEXTBROWSER_HOME )
{
   hbqt_par_QTextBrowser( 1 )->home();
}

/*
 * virtual void reload ()
 */
HB_FUNC( QT_QTEXTBROWSER_RELOAD )
{
   hbqt_par_QTextBrowser( 1 )->reload();
}

/*
 * virtual void setSource ( const QUrl & name )
 */
HB_FUNC( QT_QTEXTBROWSER_SETSOURCE )
{
   hbqt_par_QTextBrowser( 1 )->setSource( *hbqt_par_QUrl( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
