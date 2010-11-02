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
 *  enum CursorMode { SkipCharacters, SkipWords }
 */

/*
 *  Constructed[ 29/31 [ 93.55% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void draw ( QPainter * p, const QPointF & pos, const QVector<FormatRange> & selections = QVector<FormatRange> (), const QRectF & clip = QRectF() ) const
 *  void setAdditionalFormats ( const QList<FormatRange> & formatList )
 *
 *  *** Commented out protostypes ***
 *
 *  //QList<FormatRange> additionalFormats () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextLayout>


/*
 * QTextLayout ()
 * QTextLayout ( const QString & text )
 * QTextLayout ( const QString & text, const QFont & font, QPaintDevice * paintdevice = 0 )
 * ~QTextLayout ()
 */

typedef struct
{
   QTextLayout * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QTextLayout )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextLayout * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextLayout( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextLayout * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextLayout;
   p->type = HBQT_TYPE_QTextLayout;

   return p;
}

HB_FUNC( QT_QTEXTLAYOUT )
{
   QTextLayout * pObj = NULL;

   pObj = new QTextLayout() ;

   hb_retptrGC( hbqt_gcAllocate_QTextLayout( ( void * ) pObj, true ) );
}

/* void beginLayout () */
HB_FUNC( QT_QTEXTLAYOUT_BEGINLAYOUT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->beginLayout();
}

/* QRectF boundingRect () const */
HB_FUNC( QT_QTEXTLAYOUT_BOUNDINGRECT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
}

/* bool cacheEnabled () const */
HB_FUNC( QT_QTEXTLAYOUT_CACHEENABLED )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retl( ( p )->cacheEnabled() );
}

/* void clearAdditionalFormats () */
HB_FUNC( QT_QTEXTLAYOUT_CLEARADDITIONALFORMATS )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->clearAdditionalFormats();
}

/* void clearLayout () */
HB_FUNC( QT_QTEXTLAYOUT_CLEARLAYOUT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->clearLayout();
}

/* QTextLine createLine () */
HB_FUNC( QT_QTEXTLAYOUT_CREATELINE )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->createLine() ), true ) );
}

/* void drawCursor ( QPainter * painter, const QPointF & position, int cursorPosition, int width ) const */
HB_FUNC( QT_QTEXTLAYOUT_DRAWCURSOR )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->drawCursor( hbqt_par_QPainter( 2 ), *hbqt_par_QPointF( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void drawCursor ( QPainter * painter, const QPointF & position, int cursorPosition ) const */
HB_FUNC( QT_QTEXTLAYOUT_DRAWCURSOR_1 )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->drawCursor( hbqt_par_QPainter( 2 ), *hbqt_par_QPointF( 3 ), hb_parni( 4 ) );
}

/* void endLayout () */
HB_FUNC( QT_QTEXTLAYOUT_ENDLAYOUT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->endLayout();
}

/* QFont font () const */
HB_FUNC( QT_QTEXTLAYOUT_FONT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* bool isValidCursorPosition ( int pos ) const */
HB_FUNC( QT_QTEXTLAYOUT_ISVALIDCURSORPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retl( ( p )->isValidCursorPosition( hb_parni( 2 ) ) );
}

/* QTextLine lineAt ( int i ) const */
HB_FUNC( QT_QTEXTLAYOUT_LINEAT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->lineAt( hb_parni( 2 ) ) ), true ) );
}

/* int lineCount () const */
HB_FUNC( QT_QTEXTLAYOUT_LINECOUNT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->lineCount() );
}

/* QTextLine lineForTextPosition ( int pos ) const */
HB_FUNC( QT_QTEXTLAYOUT_LINEFORTEXTPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->lineForTextPosition( hb_parni( 2 ) ) ), true ) );
}

/* qreal maximumWidth () const */
HB_FUNC( QT_QTEXTLAYOUT_MAXIMUMWIDTH )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retnd( ( p )->maximumWidth() );
}

/* qreal minimumWidth () const */
HB_FUNC( QT_QTEXTLAYOUT_MINIMUMWIDTH )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retnd( ( p )->minimumWidth() );
}

/* int nextCursorPosition ( int oldPos, CursorMode mode = SkipCharacters ) const */
HB_FUNC( QT_QTEXTLAYOUT_NEXTCURSORPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->nextCursorPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLayout::CursorMode ) hb_parni( 3 ) : ( QTextLayout::CursorMode ) QTextLayout::SkipCharacters ) ) );
}

/* QPointF position () const */
HB_FUNC( QT_QTEXTLAYOUT_POSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->position() ), true ) );
}

/* int preeditAreaPosition () const */
HB_FUNC( QT_QTEXTLAYOUT_PREEDITAREAPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->preeditAreaPosition() );
}

/* QString preeditAreaText () const */
HB_FUNC( QT_QTEXTLAYOUT_PREEDITAREATEXT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retstr_utf8( ( p )->preeditAreaText().toUtf8().data() );
}

/* int previousCursorPosition ( int oldPos, CursorMode mode = SkipCharacters ) const */
HB_FUNC( QT_QTEXTLAYOUT_PREVIOUSCURSORPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->previousCursorPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLayout::CursorMode ) hb_parni( 3 ) : ( QTextLayout::CursorMode ) QTextLayout::SkipCharacters ) ) );
}

/* void setCacheEnabled ( bool enable ) */
HB_FUNC( QT_QTEXTLAYOUT_SETCACHEENABLED )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setCacheEnabled( hb_parl( 2 ) );
}

/* void setFont ( const QFont & font ) */
HB_FUNC( QT_QTEXTLAYOUT_SETFONT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setPosition ( const QPointF & p ) */
HB_FUNC( QT_QTEXTLAYOUT_SETPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setPosition( *hbqt_par_QPointF( 2 ) );
}

/* void setPreeditArea ( int position, const QString & text ) */
HB_FUNC( QT_QTEXTLAYOUT_SETPREEDITAREA )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPreeditArea( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setText ( const QString & string ) */
HB_FUNC( QT_QTEXTLAYOUT_SETTEXT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTextOption ( const QTextOption & option ) */
HB_FUNC( QT_QTEXTLAYOUT_SETTEXTOPTION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setTextOption( *hbqt_par_QTextOption( 2 ) );
}

/* QString text () const */
HB_FUNC( QT_QTEXTLAYOUT_TEXT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* QTextOption textOption () const */
HB_FUNC( QT_QTEXTLAYOUT_TEXTOPTION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextOption( new QTextOption( ( p )->textOption() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
