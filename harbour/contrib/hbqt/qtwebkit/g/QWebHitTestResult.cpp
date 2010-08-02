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

#include <QtWebKit/QWebHitTestResult>


/*
 * QWebHitTestResult ()
 * QWebHitTestResult ( const QWebHitTestResult & other )
 * ~QWebHitTestResult ()
 */

typedef struct
{
   QWebHitTestResult * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QWebHitTestResult;

QT_G_FUNC( hbqt_gcRelease_QWebHitTestResult )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QWebHitTestResult   /.\\", p->ph ) );
         delete ( ( QWebHitTestResult * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QWebHitTestResult   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QWebHitTestResult    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QWebHitTestResult    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWebHitTestResult( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QWebHitTestResult * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWebHitTestResult;
   p->type = HBQT_TYPE_QWebHitTestResult;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWebHitTestResult", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWebHitTestResult", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWEBHITTESTRESULT )
{
   QWebHitTestResult * pObj = NULL;

   pObj =  new QWebHitTestResult() ;

   hb_retptrGC( hbqt_gcAllocate_QWebHitTestResult( ( void * ) pObj, true ) );
}

/*
 * QString alternateText () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_ALTERNATETEXT )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retc( ( p )->alternateText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_ALTERNATETEXT FP=hb_retc( ( p )->alternateText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QRect boundingRect () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_BOUNDINGRECT )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QWebFrame * frame () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_FRAME )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->frame(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_FRAME FP=hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->frame(), false ) ); p is NULL" ) );
   }
}

/*
 * QUrl imageUrl () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_IMAGEURL )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->imageUrl() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_IMAGEURL FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->imageUrl() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isContentEditable () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_ISCONTENTEDITABLE )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retl( ( p )->isContentEditable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_ISCONTENTEDITABLE FP=hb_retl( ( p )->isContentEditable() ); p is NULL" ) );
   }
}

/*
 * bool isContentSelected () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_ISCONTENTSELECTED )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retl( ( p )->isContentSelected() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_ISCONTENTSELECTED FP=hb_retl( ( p )->isContentSelected() ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_ISNULL )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * QWebFrame * linkTargetFrame () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKTARGETFRAME )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->linkTargetFrame(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_LINKTARGETFRAME FP=hb_retptrGC( hbqt_gcAllocate_QWebFrame( ( p )->linkTargetFrame(), false ) ); p is NULL" ) );
   }
}

/*
 * QString linkText () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKTEXT )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retc( ( p )->linkText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_LINKTEXT FP=hb_retc( ( p )->linkText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QUrl linkTitle () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKTITLE )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->linkTitle() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_LINKTITLE FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->linkTitle() ), true ) ); p is NULL" ) );
   }
}

/*
 * QUrl linkUrl () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_LINKURL )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->linkUrl() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_LINKURL FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->linkUrl() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPixmap pixmap () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_PIXMAP )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_PIXMAP FP=hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint pos () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_POS )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_POS FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString title () const
 */
HB_FUNC( QT_QWEBHITTESTRESULT_TITLE )
{
   QWebHitTestResult * p = hbqt_par_QWebHitTestResult( 1 );
   if( p )
      hb_retc( ( p )->title().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWEBHITTESTRESULT_TITLE FP=hb_retc( ( p )->title().toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
