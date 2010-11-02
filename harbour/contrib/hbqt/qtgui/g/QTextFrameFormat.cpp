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
 *  flags PageBreakFlags
 *  enum FormatType { InvalidFormat, BlockFormat, CharFormat, ListFormat, ..., UserFormat }
 *  enum ObjectTypes { NoObject, ImageObject, TableObject, TableCellObject, UserObject }
 *  enum PageBreakFlag { PageBreak_Auto, PageBreak_AlwaysBefore, PageBreak_AlwaysAfter }
 *  enum Property { ObjectIndex, CssFloat, LayoutDirection, OutlinePen, ..., UserProperty }
 *  enum BorderStyle { BorderStyle_None, BorderStyle_Dotted, BorderStyle_Dashed, BorderStyle_Solid, ..., BorderStyle_Outset }
 *  enum Position { InFlow, FloatLeft, FloatRight }
 */

/*
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextFrameFormat>


/*
 * QTextFrameFormat ()
 */

typedef struct
{
   QTextFrameFormat * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextFrameFormat;

HBQT_GC_FUNC( hbqt_gcRelease_QTextFrameFormat )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextFrameFormat * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFrameFormat( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextFrameFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFrameFormat;
   p->type = HBQT_TYPE_QTextFrameFormat;

   return p;
}

HB_FUNC( QT_QTEXTFRAMEFORMAT )
{
   QTextFrameFormat * pObj = NULL;

   pObj = new QTextFrameFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( ( void * ) pObj, true ) );
}

/* qreal border () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDER )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->border() );
}

/* QBrush borderBrush () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDERBRUSH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->borderBrush() ), true ) );
}

/* BorderStyle borderStyle () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDERSTYLE )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retni( ( QTextFrameFormat::BorderStyle ) ( p )->borderStyle() );
}

/* qreal bottomMargin () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BOTTOMMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->bottomMargin() );
}

/* QTextLength height () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_HEIGHT )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->height() ), true ) );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_ISVALID )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* qreal leftMargin () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_LEFTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->leftMargin() );
}

/* qreal margin () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_MARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->margin() );
}

/* qreal padding () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_PADDING )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->padding() );
}

/* PageBreakFlags pageBreakPolicy () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_PAGEBREAKPOLICY )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retni( ( QTextFrameFormat::PageBreakFlags ) ( p )->pageBreakPolicy() );
}

/* Position position () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_POSITION )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retni( ( QTextFrameFormat::Position ) ( p )->position() );
}

/* qreal rightMargin () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_RIGHTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->rightMargin() );
}

/* void setBorder ( qreal width ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDER )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBorder( hb_parnd( 2 ) );
}

/* void setBorderBrush ( const QBrush & brush ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDERBRUSH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBorderBrush( *hbqt_par_QBrush( 2 ) );
}

/* void setBorderStyle ( BorderStyle style ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDERSTYLE )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBorderStyle( ( QTextFrameFormat::BorderStyle ) hb_parni( 2 ) );
}

/* void setBottomMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBOTTOMMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBottomMargin( hb_parnd( 2 ) );
}

/* void setHeight ( const QTextLength & height ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETHEIGHT )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setHeight( *hbqt_par_QTextLength( 2 ) );
}

/* void setHeight ( qreal height ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETHEIGHT_1 )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setHeight( hb_parnd( 2 ) );
}

/* void setLeftMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETLEFTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setLeftMargin( hb_parnd( 2 ) );
}

/* void setMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setMargin( hb_parnd( 2 ) );
}

/* void setPadding ( qreal width ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPADDING )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setPadding( hb_parnd( 2 ) );
}

/* void setPageBreakPolicy ( PageBreakFlags policy ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPAGEBREAKPOLICY )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setPageBreakPolicy( ( QTextFrameFormat::PageBreakFlags ) hb_parni( 2 ) );
}

/* void setPosition ( Position policy ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPOSITION )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setPosition( ( QTextFrameFormat::Position ) hb_parni( 2 ) );
}

/* void setRightMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETRIGHTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setRightMargin( hb_parnd( 2 ) );
}

/* void setTopMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETTOPMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setTopMargin( hb_parnd( 2 ) );
}

/* void setWidth ( const QTextLength & width ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETWIDTH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setWidth( *hbqt_par_QTextLength( 2 ) );
}

/* void setWidth ( qreal width ) */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETWIDTH_1 )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
}

/* qreal topMargin () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_TOPMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->topMargin() );
}

/* QTextLength width () const */
HB_FUNC( QT_QTEXTFRAMEFORMAT_WIDTH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->width() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
