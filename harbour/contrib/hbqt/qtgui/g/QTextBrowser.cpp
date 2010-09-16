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

#include <QtGui/QTextBrowser>


/*
 * QTextBrowser ( QWidget * parent = 0 )
 */

typedef struct
{
   QPointer< QTextBrowser > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextBrowser;

HBQT_GC_FUNC( hbqt_gcRelease_QTextBrowser )
{
   QTextBrowser  * ph = NULL ;
   HBQT_GC_T_QTextBrowser * p = ( HBQT_GC_T_QTextBrowser * ) Cargo;

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
   HBQT_GC_T_QTextBrowser * p = ( HBQT_GC_T_QTextBrowser * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTextBrowser ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextBrowser >( ( QTextBrowser * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBrowser;
   p->type = HBQT_TYPE_QTextBrowser;

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
   {
      hb_retni( ( p )->backwardHistoryCount() );
   }
}

/*
 * void clearHistory ()
 */
HB_FUNC( QT_QTEXTBROWSER_CLEARHISTORY )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->clearHistory();
   }
}

/*
 * int forwardHistoryCount () const
 */
HB_FUNC( QT_QTEXTBROWSER_FORWARDHISTORYCOUNT )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retni( ( p )->forwardHistoryCount() );
   }
}

/*
 * QString historyTitle ( int i ) const
 */
HB_FUNC( QT_QTEXTBROWSER_HISTORYTITLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->historyTitle( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QUrl historyUrl ( int i ) const
 */
HB_FUNC( QT_QTEXTBROWSER_HISTORYURL )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->historyUrl( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * bool isBackwardAvailable () const
 */
HB_FUNC( QT_QTEXTBROWSER_ISBACKWARDAVAILABLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retl( ( p )->isBackwardAvailable() );
   }
}

/*
 * bool isForwardAvailable () const
 */
HB_FUNC( QT_QTEXTBROWSER_ISFORWARDAVAILABLE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retl( ( p )->isForwardAvailable() );
   }
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QTEXTBROWSER_LOADRESOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
   }
}

/*
 * bool openExternalLinks () const
 */
HB_FUNC( QT_QTEXTBROWSER_OPENEXTERNALLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retl( ( p )->openExternalLinks() );
   }
}

/*
 * bool openLinks () const
 */
HB_FUNC( QT_QTEXTBROWSER_OPENLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retl( ( p )->openLinks() );
   }
}

/*
 * QStringList searchPaths () const
 */
HB_FUNC( QT_QTEXTBROWSER_SEARCHPATHS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->searchPaths() ), true ) );
   }
}

/*
 * void setOpenExternalLinks ( bool open )
 */
HB_FUNC( QT_QTEXTBROWSER_SETOPENEXTERNALLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->setOpenExternalLinks( hb_parl( 2 ) );
   }
}

/*
 * void setOpenLinks ( bool open )
 */
HB_FUNC( QT_QTEXTBROWSER_SETOPENLINKS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->setOpenLinks( hb_parl( 2 ) );
   }
}

/*
 * void setSearchPaths ( const QStringList & paths )
 */
HB_FUNC( QT_QTEXTBROWSER_SETSEARCHPATHS )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->setSearchPaths( *hbqt_par_QStringList( 2 ) );
   }
}

/*
 * QUrl source () const
 */
HB_FUNC( QT_QTEXTBROWSER_SOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->source() ), true ) );
   }
}

/*
 * virtual void backward ()
 */
HB_FUNC( QT_QTEXTBROWSER_BACKWARD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->backward();
   }
}

/*
 * virtual void forward ()
 */
HB_FUNC( QT_QTEXTBROWSER_FORWARD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->forward();
   }
}

/*
 * virtual void home ()
 */
HB_FUNC( QT_QTEXTBROWSER_HOME )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->home();
   }
}

/*
 * virtual void reload ()
 */
HB_FUNC( QT_QTEXTBROWSER_RELOAD )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->reload();
   }
}

/*
 * virtual void setSource ( const QUrl & name )
 */
HB_FUNC( QT_QTEXTBROWSER_SETSOURCE )
{
   QTextBrowser * p = hbqt_par_QTextBrowser( 1 );
   if( p )
   {
      ( p )->setSource( *hbqt_par_QUrl( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
