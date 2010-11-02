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
 *  enum CursorPosition { CursorBetweenCharacters, CursorOnCharacter }
 *  enum Edge { Leading, Trailing }
 */

/*
 *  Constructed[ 21/21 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //void draw ( QPainter * painter, const QPointF & position, const QTextLayout::FormatRange * selection = 0 ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextLine>


/*
 * QTextLine ()
 */

typedef struct
{
   QTextLine * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextLine;

HBQT_GC_FUNC( hbqt_gcRelease_QTextLine )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextLine * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextLine( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextLine * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextLine;
   p->type = HBQT_TYPE_QTextLine;

   return p;
}

HB_FUNC( QT_QTEXTLINE )
{
   QTextLine * pObj = NULL;

   pObj = new QTextLine() ;

   hb_retptrGC( hbqt_gcAllocate_QTextLine( ( void * ) pObj, true ) );
}

/* qreal ascent () const */
HB_FUNC( QT_QTEXTLINE_ASCENT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->ascent() );
}

/* qreal cursorToX ( int * cursorPos, Edge edge = Leading ) const */
HB_FUNC( QT_QTEXTLINE_CURSORTOX )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   int iCursorPos = 0;

   if( p )
      hb_retnd( ( p )->cursorToX( &iCursorPos, ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );

   hb_storni( iCursorPos, 2 );
}

/* qreal cursorToX ( int cursorPos, Edge edge = Leading ) const */
HB_FUNC( QT_QTEXTLINE_CURSORTOX_1 )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->cursorToX( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );
}

/* qreal descent () const */
HB_FUNC( QT_QTEXTLINE_DESCENT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
}

/* qreal height () const */
HB_FUNC( QT_QTEXTLINE_HEIGHT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->height() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTLINE_ISVALID )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int lineNumber () const */
HB_FUNC( QT_QTEXTLINE_LINENUMBER )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->lineNumber() );
}

/* QRectF naturalTextRect () const */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTRECT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->naturalTextRect() ), true ) );
}

/* qreal naturalTextWidth () const */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTWIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->naturalTextWidth() );
}

/* QPointF position () const */
HB_FUNC( QT_QTEXTLINE_POSITION )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->position() ), true ) );
}

/* QRectF rect () const */
HB_FUNC( QT_QTEXTLINE_RECT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
}

/* void setLineWidth ( qreal width ) */
HB_FUNC( QT_QTEXTLINE_SETLINEWIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setLineWidth( hb_parnd( 2 ) );
}

/* void setNumColumns ( int numColumns ) */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setNumColumns( hb_parni( 2 ) );
}

/* void setNumColumns ( int numColumns, qreal alignmentWidth ) */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS_1 )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setNumColumns( hb_parni( 2 ), hb_parnd( 3 ) );
}

/* void setPosition ( const QPointF & pos ) */
HB_FUNC( QT_QTEXTLINE_SETPOSITION )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setPosition( *hbqt_par_QPointF( 2 ) );
}

/* int textLength () const */
HB_FUNC( QT_QTEXTLINE_TEXTLENGTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->textLength() );
}

/* int textStart () const */
HB_FUNC( QT_QTEXTLINE_TEXTSTART )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->textStart() );
}

/* qreal width () const */
HB_FUNC( QT_QTEXTLINE_WIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->width() );
}

/* qreal x () const */
HB_FUNC( QT_QTEXTLINE_X )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->x() );
}

/* int xToCursor ( qreal x, CursorPosition cpos = CursorBetweenCharacters ) const */
HB_FUNC( QT_QTEXTLINE_XTOCURSOR )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->xToCursor( hb_parnd( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::CursorPosition ) hb_parni( 3 ) : ( QTextLine::CursorPosition ) QTextLine::CursorBetweenCharacters ) ) );
}

/* qreal y () const */
HB_FUNC( QT_QTEXTLINE_Y )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->y() );
}


#endif /* #if QT_VERSION >= 0x040500 */
