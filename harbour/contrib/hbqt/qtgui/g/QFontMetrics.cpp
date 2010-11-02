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
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 */

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

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QFontMetrics * ) p->ph );
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

/* int ascent () const */
HB_FUNC( QT_QFONTMETRICS_ASCENT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->ascent() );
}

/* int averageCharWidth () const */
HB_FUNC( QT_QFONTMETRICS_AVERAGECHARWIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->averageCharWidth() );
}

/* QRect boundingRect ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QChar( 2 ) ) ), true ) );
}

/* QRect boundingRect ( const QString & text ) const */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_1 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QRect boundingRect ( int x, int y, int width, int height, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_2 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   int iTabArray = 0;

   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parstr_utf8( 7, &pText, NULL ), hb_parni( 8 ), &iTabArray ) ), true ) );
      hb_strfree( pText );
   }

   hb_storni( iTabArray, 9 );
}

/* QRect boundingRect ( const QRect & rect, int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const */
HB_FUNC( QT_QFONTMETRICS_BOUNDINGRECT_3 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   int iTabArray = 0;

   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ), hb_parni( 5 ), &iTabArray ) ), true ) );
      hb_strfree( pText );
   }

   hb_storni( iTabArray, 6 );
}

/* int descent () const */
HB_FUNC( QT_QFONTMETRICS_DESCENT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->descent() );
}

/* QString elidedText ( const QString & text, Qt::TextElideMode mode, int width, int flags = 0 ) const */
HB_FUNC( QT_QFONTMETRICS_ELIDEDTEXT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->elidedText( hb_parstr_utf8( 2, &pText, NULL ), ( Qt::TextElideMode ) hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/* int height () const */
HB_FUNC( QT_QFONTMETRICS_HEIGHT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->height() );
}

/* bool inFont ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICS_INFONT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retl( ( p )->inFont( *hbqt_par_QChar( 2 ) ) );
}

/* int leading () const */
HB_FUNC( QT_QFONTMETRICS_LEADING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->leading() );
}

/* int leftBearing ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICS_LEFTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->leftBearing( *hbqt_par_QChar( 2 ) ) );
}

/* int lineSpacing () const */
HB_FUNC( QT_QFONTMETRICS_LINESPACING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->lineSpacing() );
}

/* int lineWidth () const */
HB_FUNC( QT_QFONTMETRICS_LINEWIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->lineWidth() );
}

/* int maxWidth () const */
HB_FUNC( QT_QFONTMETRICS_MAXWIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->maxWidth() );
}

/* int minLeftBearing () const */
HB_FUNC( QT_QFONTMETRICS_MINLEFTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->minLeftBearing() );
}

/* int minRightBearing () const */
HB_FUNC( QT_QFONTMETRICS_MINRIGHTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->minRightBearing() );
}

/* int overlinePos () const */
HB_FUNC( QT_QFONTMETRICS_OVERLINEPOS )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->overlinePos() );
}

/* int rightBearing ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICS_RIGHTBEARING )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->rightBearing( *hbqt_par_QChar( 2 ) ) );
}

/* QSize size ( int flags, const QString & text, int tabStops = 0, int * tabArray = 0 ) const */
HB_FUNC( QT_QFONTMETRICS_SIZE )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   int iTabArray = 0;

   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parni( 4 ), &iTabArray ) ), true ) );
      hb_strfree( pText );
   }

   hb_storni( iTabArray, 5 );
}

/* int strikeOutPos () const */
HB_FUNC( QT_QFONTMETRICS_STRIKEOUTPOS )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->strikeOutPos() );
}

/* QRect tightBoundingRect ( const QString & text ) const */
HB_FUNC( QT_QFONTMETRICS_TIGHTBOUNDINGRECT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->tightBoundingRect( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* int underlinePos () const */
HB_FUNC( QT_QFONTMETRICS_UNDERLINEPOS )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->underlinePos() );
}

/* int width ( const QString & text, int len = -1 ) const */
HB_FUNC( QT_QFONTMETRICS_WIDTH )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->width( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, -1 ) ) );
      hb_strfree( pText );
   }
}

/* int width ( QChar ch ) const */
HB_FUNC( QT_QFONTMETRICS_WIDTH_1 )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->width( *hbqt_par_QChar( 2 ) ) );
}

/* int xHeight () const */
HB_FUNC( QT_QFONTMETRICS_XHEIGHT )
{
   QFontMetrics * p = hbqt_par_QFontMetrics( 1 );
   if( p )
      hb_retni( ( p )->xHeight() );
}


#endif /* #if QT_VERSION >= 0x040500 */
