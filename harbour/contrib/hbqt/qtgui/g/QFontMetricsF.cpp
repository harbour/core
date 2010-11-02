/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 25/25 [ 100.00% ] ]
 *
 */

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
         delete ( ( QFontMetricsF * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QFontMetricsF( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QFontMetricsF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontMetricsF;
   p->type = HBQT_TYPE_QFontMetricsF;

   return p;
}

HB_FUNC( QT_QFONTMETRICSF )
{
   QFontMetricsF * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFontMetricsF( *hbqt_par_QFontMetricsF( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      if( hbqt_par_QString( 1 ) == ( QString ) "QFont" )
      {
         pObj = new QFontMetricsF( *hbqt_par_QFont( 2 ) ) ;
      }
      else if( hbqt_par_QString( 1 ) == ( QString ) "QFontMetrics" )
      {
         pObj = new QFontMetricsF( *hbqt_par_QFontMetrics( 2 ) ) ;
      }
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QFontMetricsF( *hbqt_par_QFont( 1 ), hbqt_par_QPaintDevice( 2 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFontMetricsF( ( void * ) pObj, true ) );
}

/* qreal ascent () const */
HB_FUNC( QT_QFONTMETRICSF_ASCENT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->ascent() );
}

/* qreal averageCharWidth () const */
HB_FUNC( QT_QFONTMETRICSF_AVERAGECHARWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->averageCharWidth() );
}

/* QRectF boundingRect ( const QString & text ) const */
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

/* QRectF boundingRect ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICSF_BOUNDINGRECT_1 )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) );
}

/* QRectF boundingRect ( const QRectF & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const */
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

/* qreal descent () const */
HB_FUNC( QT_QFONTMETRICSF_DESCENT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
}

/* QString elidedText ( const QString & text, Qt::TextElideMode mode, qreal width, int flags = 0 ) const */
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

/* qreal height () const */
HB_FUNC( QT_QFONTMETRICSF_HEIGHT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->height() );
}

/* bool inFont ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICSF_INFONT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retl( ( p )->inFont( *hbqt_par_QChar( 2 ) ) );
}

/* qreal leading () const */
HB_FUNC( QT_QFONTMETRICSF_LEADING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->leading() );
}

/* qreal leftBearing ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICSF_LEFTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->leftBearing( *hbqt_par_QChar( 2 ) ) );
}

/* qreal lineSpacing () const */
HB_FUNC( QT_QFONTMETRICSF_LINESPACING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->lineSpacing() );
}

/* qreal lineWidth () const */
HB_FUNC( QT_QFONTMETRICSF_LINEWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->lineWidth() );
}

/* qreal maxWidth () const */
HB_FUNC( QT_QFONTMETRICSF_MAXWIDTH )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->maxWidth() );
}

/* qreal minLeftBearing () const */
HB_FUNC( QT_QFONTMETRICSF_MINLEFTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->minLeftBearing() );
}

/* qreal minRightBearing () const */
HB_FUNC( QT_QFONTMETRICSF_MINRIGHTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->minRightBearing() );
}

/* qreal overlinePos () const */
HB_FUNC( QT_QFONTMETRICSF_OVERLINEPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->overlinePos() );
}

/* qreal rightBearing ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICSF_RIGHTBEARING )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->rightBearing( *hbqt_par_QChar( 2 ) ) );
}

/* QSizeF size ( int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const */
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

/* qreal strikeOutPos () const */
HB_FUNC( QT_QFONTMETRICSF_STRIKEOUTPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->strikeOutPos() );
}

/* QRectF tightBoundingRect ( const QString & text ) const */
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

/* qreal underlinePos () const */
HB_FUNC( QT_QFONTMETRICSF_UNDERLINEPOS )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->underlinePos() );
}

/* qreal width ( const QString & text ) const */
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

/* qreal width ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICSF_WIDTH_1 )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->width( *hbqt_par_QChar( 2 ) ) );
}

/* qreal xHeight () const */
HB_FUNC( QT_QFONTMETRICSF_XHEIGHT )
{
   QFontMetricsF * p = hbqt_par_QFontMetricsF( 1 );
   if( p )
      hb_retnd( ( p )->xHeight() );
}


#endif /* #if QT_VERSION >= 0x040500 */
