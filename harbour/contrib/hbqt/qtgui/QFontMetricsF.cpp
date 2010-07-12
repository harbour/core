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

#include <QtGui/QFontMetricsF>


/*
 * QFontMetricsF ( const QFont & font )
 * QFontMetricsF ( const QFont & font, QPaintDevice * paintdevice )
 * QFontMetricsF ( const QFontMetrics & fontMetrics )
 * QFontMetricsF ( const QFontMetricsF & fm )
 * ~QFontMetricsF ()
 */

typedef struct
{
   QFontMetricsF * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFontMetricsF;

QT_G_FUNC( hbqt_gcRelease_QFontMetricsF )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFontMetricsF   /.\\", p->ph ) );
         delete ( ( QFontMetricsF * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFontMetricsF   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFontMetricsF    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFontMetricsF    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontMetricsF( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QFontMetricsF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontMetricsF;
   p->type = HBQT_TYPE_QFontMetricsF;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFontMetricsF", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFontMetricsF", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONTMETRICSF )
{
   QFontMetricsF * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QFontMetricsF( *hbqt_par_QFontMetricsF( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      if( hbqt_par_QString( 1 ) == ( QString ) "QFont" )
      {
         pObj =  new QFontMetricsF( *hbqt_par_QFont( 2 ) ) ;
      }
      else if( hbqt_par_QString( 1 ) == ( QString ) "QFontMetrics" )
      {
         pObj =  new QFontMetricsF( *hbqt_par_QFontMetrics( 2 ) ) ;
      }
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QFontMetricsF( *hbqt_par_QFont( 1 ), hbqt_par_QPaintDevice( 2 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFontMetricsF( ( void * ) pObj, true ) );
}

/*
 * qreal ascent () const
 */
HB_FUNC( QT_QFONTMETRICSF_ASCENT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->ascent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_ASCENT FP=hb_retnd( ( p )->ascent() ); p is NULL" ) );
   }
}

/*
 * qreal averageCharWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_AVERAGECHARWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->averageCharWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_AVERAGECHARWIDTH FP=hb_retnd( ( p )->averageCharWidth() ); p is NULL" ) );
   }
}

/*
 * QRectF boundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF boundingRect ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT_1 )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_BOUNDINGRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF boundingRect ( const QRectF & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT_2 )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   int iTabArray = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hb_parni( 5 ), &iTabArray ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_BOUNDINGRECT_2 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hb_parni( 5 ), &iTabArray ) ), true ) ); p is NULL" ) );
   }

   hb_storni( iTabArray, 6 );
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QFONTMETRICSF_DESCENT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_DESCENT FP=hb_retnd( ( p )->descent() ); p is NULL" ) );
   }
}

/*
 * QString elidedText ( const QString & text, Qt::TextElideMode mode, qreal width, int flags = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICSF_ELIDEDTEXT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retc( ( p )->elidedText( hbqt_par_QString( 2 ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parnd( 4 ), hb_parni( 5 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_ELIDEDTEXT FP=hb_retc( ( p )->elidedText( hbqt_par_QString( 2 ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parnd( 4 ), hb_parni( 5 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QFONTMETRICSF_HEIGHT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_HEIGHT FP=hb_retnd( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * bool inFont ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_INFONT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retl( ( p )->inFont( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_INFONT FP=hb_retl( ( p )->inFont( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * qreal leading () const
 */
HB_FUNC( QT_QFONTMETRICSF_LEADING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->leading() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_LEADING FP=hb_retnd( ( p )->leading() ); p is NULL" ) );
   }
}

/*
 * qreal leftBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_LEFTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->leftBearing( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_LEFTBEARING FP=hb_retnd( ( p )->leftBearing( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * qreal lineSpacing () const
 */
HB_FUNC( QT_QFONTMETRICSF_LINESPACING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->lineSpacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_LINESPACING FP=hb_retnd( ( p )->lineSpacing() ); p is NULL" ) );
   }
}

/*
 * qreal lineWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_LINEWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->lineWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_LINEWIDTH FP=hb_retnd( ( p )->lineWidth() ); p is NULL" ) );
   }
}

/*
 * qreal maxWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_MAXWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->maxWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_MAXWIDTH FP=hb_retnd( ( p )->maxWidth() ); p is NULL" ) );
   }
}

/*
 * qreal minLeftBearing () const
 */
HB_FUNC( QT_QFONTMETRICSF_MINLEFTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->minLeftBearing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_MINLEFTBEARING FP=hb_retnd( ( p )->minLeftBearing() ); p is NULL" ) );
   }
}

/*
 * qreal minRightBearing () const
 */
HB_FUNC( QT_QFONTMETRICSF_MINRIGHTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->minRightBearing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_MINRIGHTBEARING FP=hb_retnd( ( p )->minRightBearing() ); p is NULL" ) );
   }
}

/*
 * qreal overlinePos () const
 */
HB_FUNC( QT_QFONTMETRICSF_OVERLINEPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->overlinePos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_OVERLINEPOS FP=hb_retnd( ( p )->overlinePos() ); p is NULL" ) );
   }
}

/*
 * qreal rightBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_RIGHTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->rightBearing( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_RIGHTBEARING FP=hb_retnd( ( p )->rightBearing( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QSizeF size ( int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICSF_SIZE )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   int iTabArray = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->size( hb_parni( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ), &iTabArray ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_SIZE FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->size( hb_parni( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ), &iTabArray ) ), true ) ); p is NULL" ) );
   }

   hb_storni( iTabArray, 5 );
}

/*
 * qreal strikeOutPos () const
 */
HB_FUNC( QT_QFONTMETRICSF_STRIKEOUTPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->strikeOutPos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_STRIKEOUTPOS FP=hb_retnd( ( p )->strikeOutPos() ); p is NULL" ) );
   }
}

/*
 * QRectF tightBoundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_TIGHTBOUNDINGRECT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->tightBoundingRect( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_TIGHTBOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->tightBoundingRect( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal underlinePos () const
 */
HB_FUNC( QT_QFONTMETRICSF_UNDERLINEPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->underlinePos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_UNDERLINEPOS FP=hb_retnd( ( p )->underlinePos() ); p is NULL" ) );
   }
}

/*
 * qreal width ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_WIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->width( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_WIDTH FP=hb_retnd( ( p )->width( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * qreal width ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_WIDTH_1 )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->width( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_WIDTH_1 FP=hb_retnd( ( p )->width( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * qreal xHeight () const
 */
HB_FUNC( QT_QFONTMETRICSF_XHEIGHT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->xHeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICSF_XHEIGHT FP=hb_retnd( ( p )->xHeight() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
