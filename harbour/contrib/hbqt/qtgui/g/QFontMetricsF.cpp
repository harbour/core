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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFontMetricsF;

HBQT_GC_FUNC( hbqt_gcRelease_QFontMetricsF )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
   {
      hb_retnd( ( p )->ascent() );
   }
}

/*
 * qreal averageCharWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_AVERAGECHARWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->averageCharWidth() );
   }
}

/*
 * QRectF boundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QRectF boundingRect ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT_1 )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) );
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
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ), hb_parni( 5 ), &iTabArray ) ), true ) );
      hb_strfree( pText );
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
   {
      hb_retnd( ( p )->descent() );
   }
}

/*
 * QString elidedText ( const QString & text, Qt::TextElideMode mode, qreal width, int flags = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICSF_ELIDEDTEXT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->elidedText( hb_parstr_utf8( 2, &pText, NULL ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parnd( 4 ), hb_parni( 5 ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QFONTMETRICSF_HEIGHT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->height() );
   }
}

/*
 * bool inFont ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_INFONT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retl( ( p )->inFont( *hbqt_par_QChar( 2 ) ) );
   }
}

/*
 * qreal leading () const
 */
HB_FUNC( QT_QFONTMETRICSF_LEADING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->leading() );
   }
}

/*
 * qreal leftBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_LEFTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->leftBearing( *hbqt_par_QChar( 2 ) ) );
   }
}

/*
 * qreal lineSpacing () const
 */
HB_FUNC( QT_QFONTMETRICSF_LINESPACING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->lineSpacing() );
   }
}

/*
 * qreal lineWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_LINEWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->lineWidth() );
   }
}

/*
 * qreal maxWidth () const
 */
HB_FUNC( QT_QFONTMETRICSF_MAXWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->maxWidth() );
   }
}

/*
 * qreal minLeftBearing () const
 */
HB_FUNC( QT_QFONTMETRICSF_MINLEFTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->minLeftBearing() );
   }
}

/*
 * qreal minRightBearing () const
 */
HB_FUNC( QT_QFONTMETRICSF_MINRIGHTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->minRightBearing() );
   }
}

/*
 * qreal overlinePos () const
 */
HB_FUNC( QT_QFONTMETRICSF_OVERLINEPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->overlinePos() );
   }
}

/*
 * qreal rightBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_RIGHTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->rightBearing( *hbqt_par_QChar( 2 ) ) );
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
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->size( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parni( 4 ), &iTabArray ) ), true ) );
      hb_strfree( pText );
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
   {
      hb_retnd( ( p )->strikeOutPos() );
   }
}

/*
 * QRectF tightBoundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_TIGHTBOUNDINGRECT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->tightBoundingRect( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * qreal underlinePos () const
 */
HB_FUNC( QT_QFONTMETRICSF_UNDERLINEPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->underlinePos() );
   }
}

/*
 * qreal width ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICSF_WIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      void * pText;
      hb_retnd( ( p )->width( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * qreal width ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICSF_WIDTH_1 )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->width( *hbqt_par_QChar( 2 ) ) );
   }
}

/*
 * qreal xHeight () const
 */
HB_FUNC( QT_QFONTMETRICSF_XHEIGHT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
   {
      hb_retnd( ( p )->xHeight() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
