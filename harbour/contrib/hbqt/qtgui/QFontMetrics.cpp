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

#include <QtGui/QFontMetrics>


/*
 * QFontMetrics ( const QFont & font )
 * QFontMetrics ( const QFont & font, QPaintDevice * paintdevice )
 * QFontMetrics ( const QFontMetrics & fm )
 * ~QFontMetrics ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QFontMetrics;

QT_G_FUNC( hbqt_gcRelease_QFontMetrics )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QFontMetrics * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QFontMetrics               ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QFontMetrics                Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QFontMetrics                Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontMetrics( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontMetrics;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QFontMetrics               ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QFONTMETRICS )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFontMetrics( *hbqt_par_QFontMetrics( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      if( hbqt_par_QString( 1 ) == ( QString ) "QFont" )
      {
         pObj = new QFontMetrics( *hbqt_par_QFont( 2 ) ) ;
      }
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QFontMetrics( *hbqt_par_QFont( 1 ), hbqt_par_QPaintDevice( 2 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFontMetrics( pObj, true ) );
}

/*
 * int ascent () const
 */
HB_FUNC( QT_QFONTMETRICS_ASCENT )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->ascent() );
}

/*
 * int averageCharWidth () const
 */
HB_FUNC( QT_QFONTMETRICS_AVERAGECHARWIDTH )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->averageCharWidth() );
}

/*
 * QRect boundingRect ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QFontMetrics( 1 )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) );
}

/*
 * QRect boundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QFontMetrics( 1 )->boundingRect( hbqt_par_QString( 2 ) ) ), true ) );
}

/*
 * QRect boundingRect ( int x, int y, int width, int height, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_2 )
{
   int iTabArray = 0;

   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QFontMetrics( 1 )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ), hb_parni( 8 ), &iTabArray ) ), true ) );

   hb_storni( iTabArray, 9 );
}

/*
 * QRect boundingRect ( const QRect & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_3 )
{
   int iTabArray = 0;

   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QFontMetrics( 1 )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hb_parni( 5 ), &iTabArray ) ), true ) );

   hb_storni( iTabArray, 6 );
}

/*
 * int descent () const
 */
HB_FUNC( QT_QFONTMETRICS_DESCENT )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->descent() );
}

/*
 * QString elidedText ( const QString & text, Qt::TextElideMode mode, int width, int flags = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_ELIDEDTEXT )
{
   hb_retc( hbqt_par_QFontMetrics( 1 )->elidedText( hbqt_par_QString( 2 ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ).toAscii().data() );
}

/*
 * int height () const
 */
HB_FUNC( QT_QFONTMETRICS_HEIGHT )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->height() );
}

/*
 * bool inFont ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_INFONT )
{
   hb_retl( hbqt_par_QFontMetrics( 1 )->inFont( *hbqt_par_QChar( 2 ) ) );
}

/*
 * int leading () const
 */
HB_FUNC( QT_QFONTMETRICS_LEADING )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->leading() );
}

/*
 * int leftBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_LEFTBEARING )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->leftBearing( *hbqt_par_QChar( 2 ) ) );
}

/*
 * int lineSpacing () const
 */
HB_FUNC( QT_QFONTMETRICS_LINESPACING )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->lineSpacing() );
}

/*
 * int lineWidth () const
 */
HB_FUNC( QT_QFONTMETRICS_LINEWIDTH )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->lineWidth() );
}

/*
 * int maxWidth () const
 */
HB_FUNC( QT_QFONTMETRICS_MAXWIDTH )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->maxWidth() );
}

/*
 * int minLeftBearing () const
 */
HB_FUNC( QT_QFONTMETRICS_MINLEFTBEARING )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->minLeftBearing() );
}

/*
 * int minRightBearing () const
 */
HB_FUNC( QT_QFONTMETRICS_MINRIGHTBEARING )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->minRightBearing() );
}

/*
 * int overlinePos () const
 */
HB_FUNC( QT_QFONTMETRICS_OVERLINEPOS )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->overlinePos() );
}

/*
 * int rightBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_RIGHTBEARING )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->rightBearing( *hbqt_par_QChar( 2 ) ) );
}

/*
 * QSize size ( int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_SIZE )
{
   int iTabArray = 0;

   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QFontMetrics( 1 )->size( hb_parni( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ), &iTabArray ) ), true ) );

   hb_storni( iTabArray, 5 );
}

/*
 * int strikeOutPos () const
 */
HB_FUNC( QT_QFONTMETRICS_STRIKEOUTPOS )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->strikeOutPos() );
}

/*
 * QRect tightBoundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICS_TIGHTBOUNDINGRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QFontMetrics( 1 )->tightBoundingRect( hbqt_par_QString( 2 ) ) ), true ) );
}

/*
 * int underlinePos () const
 */
HB_FUNC( QT_QFONTMETRICS_UNDERLINEPOS )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->underlinePos() );
}

/*
 * int width ( const QString & text, int len = -1 ) const
 */
HB_FUNC( QT_QFONTMETRICS_WIDTH )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->width( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int width ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_WIDTH_1 )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->width( *hbqt_par_QChar( 2 ) ) );
}

/*
 * int xHeight () const
 */
HB_FUNC( QT_QFONTMETRICS_XHEIGHT )
{
   hb_retni( hbqt_par_QFontMetrics( 1 )->xHeight() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
