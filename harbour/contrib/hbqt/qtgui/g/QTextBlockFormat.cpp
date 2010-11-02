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
 *  enum FormatType { InvalidFormat, BlockFormat, CharFormat, ListFormat, ..., UserFormat }
 *  enum ObjectTypes { NoObject, ImageObject, TableObject, TableCellObject, UserObject }
 *  enum PageBreakFlag { PageBreak_Auto, PageBreak_AlwaysBefore, PageBreak_AlwaysAfter }
 *  flags PageBreakFlags
 *  enum Property { ObjectIndex, CssFloat, LayoutDirection, OutlinePen, ..., UserProperty }
 */

/*
 *  Constructed[ 19/21 [ 90.48% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void setTabPositions ( const QList<QTextOption::Tab> & tabs )
 *  QList<QTextOption::Tab> tabPositions () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextBlockFormat>


/*
 * QTextBlockFormat ()
 *
 */

typedef struct
{
   QTextBlockFormat * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextBlockFormat;

HBQT_GC_FUNC( hbqt_gcRelease_QTextBlockFormat )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextBlockFormat * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBlockFormat( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextBlockFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBlockFormat;
   p->type = HBQT_TYPE_QTextBlockFormat;

   return p;
}

HB_FUNC( QT_QTEXTBLOCKFORMAT )
{
   QTextBlockFormat * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QTextBlockFormat( *hbqt_par_QTextBlockFormat( 1 ) ) ;
   }
   else
   {
      pObj = new QTextBlockFormat() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( ( void * ) pObj, true ) );
}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_ALIGNMENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* qreal bottomMargin () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_BOTTOMMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->bottomMargin() );
}

/* int indent () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_INDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retni( ( p )->indent() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_ISVALID )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* qreal leftMargin () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_LEFTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->leftMargin() );
}

/* bool nonBreakableLines () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_NONBREAKABLELINES )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retl( ( p )->nonBreakableLines() );
}

/* PageBreakFlags pageBreakPolicy () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_PAGEBREAKPOLICY )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retni( ( QTextBlockFormat::PageBreakFlags ) ( p )->pageBreakPolicy() );
}

/* qreal rightMargin () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_RIGHTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->rightMargin() );
}

/* void setAlignment ( Qt::Alignment alignment ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETALIGNMENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setBottomMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETBOTTOMMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setBottomMargin( hb_parnd( 2 ) );
}

/* void setIndent ( int indentation ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setIndent( hb_parni( 2 ) );
}

/* void setLeftMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETLEFTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setLeftMargin( hb_parnd( 2 ) );
}

/* void setNonBreakableLines ( bool b ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETNONBREAKABLELINES )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setNonBreakableLines( hb_parl( 2 ) );
}

/* void setPageBreakPolicy ( PageBreakFlags policy ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETPAGEBREAKPOLICY )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setPageBreakPolicy( ( QTextBlockFormat::PageBreakFlags ) hb_parni( 2 ) );
}

/* void setRightMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETRIGHTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setRightMargin( hb_parnd( 2 ) );
}

/* void setTextIndent ( qreal indent ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTEXTINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setTextIndent( hb_parnd( 2 ) );
}

/* void setTopMargin ( qreal margin ) */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTOPMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setTopMargin( hb_parnd( 2 ) );
}

/* qreal textIndent () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TEXTINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->textIndent() );
}

/* qreal topMargin () const */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TOPMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->topMargin() );
}


#endif /* #if QT_VERSION >= 0x040500 */
