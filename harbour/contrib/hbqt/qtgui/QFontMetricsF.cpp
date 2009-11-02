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

#include <QtGui/QFontMetricsF>


/*
 * QFontMetricsF ( const QFont & font )
 * QFontMetricsF ( const QFont & font, QPaintDevice * paintdevice )
 * QFontMetricsF ( const QFontMetrics & fontMetrics )
 * QFontMetricsF ( const QFontMetricsF & fm )
 * ~QFontMetricsF ()
 */

QT_G_FUNC( release_QFontMetricsF )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QFontMetricsF               %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      ( ( QFontMetricsF * ) ph )->~QFontMetricsF();
      ph = NULL;
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QFontMetricsF" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QFONTMETRICSF )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   void * pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QFontMetricsF               %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QFontMetricsF* ) new QFontMetricsF( *hbqt_par_QFontMetricsF( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      if( hbqt_par_QString( 1 ) == ( QString ) "QFont" )
      {
         pObj = ( QFontMetricsF* ) new QFontMetricsF( *hbqt_par_QFont( 2 ) ) ;
      }
      else if( hbqt_par_QString( 1 ) == ( QString ) "QFontMetrics" )
      {
         pObj = ( QFontMetricsF* ) new QFontMetricsF( *hbqt_par_QFontMetrics( 2 ) ) ;
      }
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = ( QFontMetricsF* ) new QFontMetricsF( *hbqt_par_QFont( 1 ), hbqt_par_QPaintDevice( 2 ) ) ;
   }

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QFontMetricsF;

   hb_retptrGC( p );
}
/*
 * qreal ascent () const
 */
HB_FUNC( QT_QFONTMETRICSF_ASCENT )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->ascent() );
}

/*
 * qreal averageCharWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_AVERAGECHARWIDTH )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->averageCharWidth() );
}

/*
 * QRectF boundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRectF( hbqt_par_QFontMetricsF( 1 )->boundingRect( hbqt_par_QString( 2 ) ) ), release_QRectF ) );
}

/*
 * QRectF boundingRect ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT_1 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRectF( hbqt_par_QFontMetricsF( 1 )->boundingRect( hb_parni( 2 ) ) ), release_QRectF ) );
}

/*
 * QRectF boundingRect ( const QRectF & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT_2 )
{
   int iTabArray = 0;

   hb_retptrGC( hbqt_ptrTOgcpointer( new QRectF( hbqt_par_QFontMetricsF( 1 )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hb_parni( 5 ), &iTabArray ) ), release_QRectF ) );

   hb_storni( iTabArray, 6 );
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QFONTMETRICSF_DESCENT )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->descent() );
}

/*
 * QString elidedText ( const QString & text, Qt::TextElideMode mode, qreal width, int flags = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICSF_ELIDEDTEXT )
{
   hb_retc( hbqt_par_QFontMetricsF( 1 )->elidedText( hbqt_par_QString( 2 ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parnd( 4 ), hb_parni( 5 ) ).toAscii().data() );
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QFONTMETRICSF_HEIGHT )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->height() );
}

/*
 * bool inFont ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_INFONT )
{
   hb_retl( hbqt_par_QFontMetricsF( 1 )->inFont( hb_parni( 2 ) ) );
}

/*
 * qreal leading () const
 */
HB_FUNC( QT_QFONTMETRICSF_LEADING )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->leading() );
}

/*
 * qreal leftBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_LEFTBEARING )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->leftBearing( hb_parni( 2 ) ) );
}

/*
 * qreal lineSpacing () const
 */
HB_FUNC( QT_QFONTMETRICSF_LINESPACING )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->lineSpacing() );
}

/*
 * qreal lineWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_LINEWIDTH )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->lineWidth() );
}

/*
 * qreal maxWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_MAXWIDTH )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->maxWidth() );
}

/*
 * qreal minLeftBearing () const
 */
HB_FUNC( QT_QFONTMETRICSF_MINLEFTBEARING )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->minLeftBearing() );
}

/*
 * qreal minRightBearing () const
 */
HB_FUNC( QT_QFONTMETRICSF_MINRIGHTBEARING )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->minRightBearing() );
}

/*
 * qreal overlinePos () const
 */
HB_FUNC( QT_QFONTMETRICSF_OVERLINEPOS )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->overlinePos() );
}

/*
 * qreal rightBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_RIGHTBEARING )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->rightBearing( hb_parni( 2 ) ) );
}

/*
 * QSizeF size ( int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICSF_SIZE )
{
   int iTabArray = 0;

   hb_retptrGC( hbqt_ptrTOgcpointer( new QSizeF( hbqt_par_QFontMetricsF( 1 )->size( hb_parni( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ), &iTabArray ) ), release_QSizeF ) );

   hb_storni( iTabArray, 5 );
}

/*
 * qreal strikeOutPos () const
 */
HB_FUNC( QT_QFONTMETRICSF_STRIKEOUTPOS )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->strikeOutPos() );
}

/*
 * QRectF tightBoundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_TIGHTBOUNDINGRECT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRectF( hbqt_par_QFontMetricsF( 1 )->tightBoundingRect( hbqt_par_QString( 2 ) ) ), release_QRectF ) );
}

/*
 * qreal underlinePos () const
 */
HB_FUNC( QT_QFONTMETRICSF_UNDERLINEPOS )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->underlinePos() );
}

/*
 * qreal width ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_WIDTH )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->width( hbqt_par_QString( 2 ) ) );
}

/*
 * qreal width ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_WIDTH_1 )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->width( hb_parni( 2 ) ) );
}

/*
 * qreal xHeight () const
 */
HB_FUNC( QT_QFONTMETRICSF_XHEIGHT )
{
   hb_retnd( hbqt_par_QFontMetricsF( 1 )->xHeight() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
