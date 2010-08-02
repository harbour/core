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

#include <QtGui/QFontMetrics>


/*
 * QFontMetrics ( const QFont & font )
 * QFontMetrics ( const QFont & font, QPaintDevice * paintdevice )
 * QFontMetrics ( const QFontMetrics & fm )
 * ~QFontMetrics ()
 */

typedef struct
{
   QFontMetrics * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFontMetrics;

HBQT_GC_FUNC( hbqt_gcRelease_QFontMetrics )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFontMetrics   /.\\", p->ph ) );
         delete ( ( QFontMetrics * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFontMetrics   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFontMetrics    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFontMetrics    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontMetrics( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QFontMetrics * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontMetrics;
   p->type = HBQT_TYPE_QFontMetrics;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFontMetrics", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFontMetrics", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONTMETRICS )
{
   QFontMetrics * pObj = NULL;

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

   hb_retptrGC( hbqt_gcAllocate_QFontMetrics( ( void * ) pObj, true ) );
}

/*
 * int ascent () const
 */
HB_FUNC( QT_QFONTMETRICS_ASCENT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->ascent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_ASCENT FP=hb_retni( ( p )->ascent() ); p is NULL" ) );
   }
}

/*
 * int averageCharWidth () const
 */
HB_FUNC( QT_QFONTMETRICS_AVERAGECHARWIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->averageCharWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_AVERAGECHARWIDTH FP=hb_retni( ( p )->averageCharWidth() ); p is NULL" ) );
   }
}

/*
 * QRect boundingRect ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect boundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_1 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_BOUNDINGRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect boundingRect ( int x, int y, int width, int height, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_2 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   int iTabArray = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ), hb_parni( 8 ), &iTabArray ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_BOUNDINGRECT_2 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ), hb_parni( 8 ), &iTabArray ) ), true ) ); p is NULL" ) );
   }

   hb_storni( iTabArray, 9 );
}

/*
 * QRect boundingRect ( const QRect & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_3 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   int iTabArray = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hb_parni( 5 ), &iTabArray ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_BOUNDINGRECT_3 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hb_parni( 5 ), &iTabArray ) ), true ) ); p is NULL" ) );
   }

   hb_storni( iTabArray, 6 );
}

/*
 * int descent () const
 */
HB_FUNC( QT_QFONTMETRICS_DESCENT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->descent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_DESCENT FP=hb_retni( ( p )->descent() ); p is NULL" ) );
   }
}

/*
 * QString elidedText ( const QString & text, Qt::TextElideMode mode, int width, int flags = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_ELIDEDTEXT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retc( ( p )->elidedText( hbqt_par_QString( 2 ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_ELIDEDTEXT FP=hb_retc( ( p )->elidedText( hbqt_par_QString( 2 ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int height () const
 */
HB_FUNC( QT_QFONTMETRICS_HEIGHT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_HEIGHT FP=hb_retni( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * bool inFont ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_INFONT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retl( ( p )->inFont( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_INFONT FP=hb_retl( ( p )->inFont( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int leading () const
 */
HB_FUNC( QT_QFONTMETRICS_LEADING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->leading() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_LEADING FP=hb_retni( ( p )->leading() ); p is NULL" ) );
   }
}

/*
 * int leftBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_LEFTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->leftBearing( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_LEFTBEARING FP=hb_retni( ( p )->leftBearing( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int lineSpacing () const
 */
HB_FUNC( QT_QFONTMETRICS_LINESPACING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->lineSpacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_LINESPACING FP=hb_retni( ( p )->lineSpacing() ); p is NULL" ) );
   }
}

/*
 * int lineWidth () const
 */
HB_FUNC( QT_QFONTMETRICS_LINEWIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->lineWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_LINEWIDTH FP=hb_retni( ( p )->lineWidth() ); p is NULL" ) );
   }
}

/*
 * int maxWidth () const
 */
HB_FUNC( QT_QFONTMETRICS_MAXWIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->maxWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_MAXWIDTH FP=hb_retni( ( p )->maxWidth() ); p is NULL" ) );
   }
}

/*
 * int minLeftBearing () const
 */
HB_FUNC( QT_QFONTMETRICS_MINLEFTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->minLeftBearing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_MINLEFTBEARING FP=hb_retni( ( p )->minLeftBearing() ); p is NULL" ) );
   }
}

/*
 * int minRightBearing () const
 */
HB_FUNC( QT_QFONTMETRICS_MINRIGHTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->minRightBearing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_MINRIGHTBEARING FP=hb_retni( ( p )->minRightBearing() ); p is NULL" ) );
   }
}

/*
 * int overlinePos () const
 */
HB_FUNC( QT_QFONTMETRICS_OVERLINEPOS )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->overlinePos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_OVERLINEPOS FP=hb_retni( ( p )->overlinePos() ); p is NULL" ) );
   }
}

/*
 * int rightBearing ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_RIGHTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->rightBearing( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_RIGHTBEARING FP=hb_retni( ( p )->rightBearing( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QSize size ( int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const
 */
HB_FUNC( QT_QFONTMETRICS_SIZE )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   int iTabArray = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size( hb_parni( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ), &iTabArray ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_SIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size( hb_parni( 2 ), hbqt_par_QString( 3 ), hb_parni( 4 ), &iTabArray ) ), true ) ); p is NULL" ) );
   }

   hb_storni( iTabArray, 5 );
}

/*
 * int strikeOutPos () const
 */
HB_FUNC( QT_QFONTMETRICS_STRIKEOUTPOS )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->strikeOutPos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_STRIKEOUTPOS FP=hb_retni( ( p )->strikeOutPos() ); p is NULL" ) );
   }
}

/*
 * QRect tightBoundingRect ( const QString & text ) const
 */
HB_FUNC( QT_QFONTMETRICS_TIGHTBOUNDINGRECT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->tightBoundingRect( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_TIGHTBOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->tightBoundingRect( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int underlinePos () const
 */
HB_FUNC( QT_QFONTMETRICS_UNDERLINEPOS )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->underlinePos() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_UNDERLINEPOS FP=hb_retni( ( p )->underlinePos() ); p is NULL" ) );
   }
}

/*
 * int width ( const QString & text, int len = -1 ) const
 */
HB_FUNC( QT_QFONTMETRICS_WIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->width( hbqt_par_QString( 2 ), hb_parnidef( 3, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_WIDTH FP=hb_retni( ( p )->width( hbqt_par_QString( 2 ), hb_parnidef( 3, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * int width ( QChar ch ) const
 */
HB_FUNC( QT_QFONTMETRICS_WIDTH_1 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->width( *hbqt_par_QChar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_WIDTH_1 FP=hb_retni( ( p )->width( *hbqt_par_QChar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int xHeight () const
 */
HB_FUNC( QT_QFONTMETRICS_XHEIGHT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->xHeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTMETRICS_XHEIGHT FP=hb_retni( ( p )->xHeight() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
