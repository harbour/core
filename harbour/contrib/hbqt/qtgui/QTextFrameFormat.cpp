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

/*
 *  flags PageBreakFlags
 *  enum FormatType { InvalidFormat, BlockFormat, CharFormat, ListFormat, ..., UserFormat }
 *  enum ObjectTypes { NoObject, ImageObject, TableObject, TableCellObject, UserObject }
 *  enum PageBreakFlag { PageBreak_Auto, PageBreak_AlwaysBefore, PageBreak_AlwaysAfter }
 *  enum Property { ObjectIndex, CssFloat, LayoutDirection, OutlinePen, ..., UserProperty }
 *  enum BorderStyle { BorderStyle_None, BorderStyle_Dotted, BorderStyle_Dashed, BorderStyle_Solid, ..., BorderStyle_Outset }
 *  enum Position { InFlow, FloatLeft, FloatRight }
 */

#include <QtCore/QPointer>

#include <QtGui/QTextFrameFormat>


/*
 * QTextFrameFormat ()
 */

QT_G_FUNC( release_QTextFrameFormat )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QTextFrameFormat" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      delete ( ( QTextFrameFormat * ) ph );
      ph = NULL;
   }
}

HB_FUNC( QT_QTEXTFRAMEFORMAT )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   void * pObj = NULL;

   pObj = ( QTextFrameFormat* ) new QTextFrameFormat() ;

   p->ph = pObj;
   p->func = release_QTextFrameFormat;

   hb_retptrGC( p );
}
/*
 * qreal border () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDER )
{
   hb_retnd( hbqt_par_QTextFrameFormat( 1 )->border() );
}

/*
 * QBrush borderBrush () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDERBRUSH )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QBrush( hbqt_par_QTextFrameFormat( 1 )->borderBrush() ), release_QBrush ) );
}

/*
 * BorderStyle borderStyle () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDERSTYLE )
{
   hb_retni( ( QTextFrameFormat::BorderStyle ) hbqt_par_QTextFrameFormat( 1 )->borderStyle() );
}

/*
 * qreal bottomMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BOTTOMMARGIN )
{
   hb_retnd( hbqt_par_QTextFrameFormat( 1 )->bottomMargin() );
}

/*
 * QTextLength height () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_HEIGHT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTextLength( hbqt_par_QTextFrameFormat( 1 )->height() ), release_QTextLength ) );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_ISVALID )
{
   hb_retl( hbqt_par_QTextFrameFormat( 1 )->isValid() );
}

/*
 * qreal leftMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_LEFTMARGIN )
{
   hb_retnd( hbqt_par_QTextFrameFormat( 1 )->leftMargin() );
}

/*
 * qreal margin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_MARGIN )
{
   hb_retnd( hbqt_par_QTextFrameFormat( 1 )->margin() );
}

/*
 * qreal padding () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_PADDING )
{
   hb_retnd( hbqt_par_QTextFrameFormat( 1 )->padding() );
}

/*
 * PageBreakFlags pageBreakPolicy () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_PAGEBREAKPOLICY )
{
   hb_retni( ( QTextFrameFormat::PageBreakFlags ) hbqt_par_QTextFrameFormat( 1 )->pageBreakPolicy() );
}

/*
 * Position position () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_POSITION )
{
   hb_retni( ( QTextFrameFormat::Position ) hbqt_par_QTextFrameFormat( 1 )->position() );
}

/*
 * qreal rightMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_RIGHTMARGIN )
{
   hb_retnd( hbqt_par_QTextFrameFormat( 1 )->rightMargin() );
}

/*
 * void setBorder ( qreal width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDER )
{
   hbqt_par_QTextFrameFormat( 1 )->setBorder( hb_parnd( 2 ) );
}

/*
 * void setBorderBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDERBRUSH )
{
   hbqt_par_QTextFrameFormat( 1 )->setBorderBrush( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setBorderStyle ( BorderStyle style )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDERSTYLE )
{
   hbqt_par_QTextFrameFormat( 1 )->setBorderStyle( ( QTextFrameFormat::BorderStyle ) hb_parni( 2 ) );
}

/*
 * void setBottomMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBOTTOMMARGIN )
{
   hbqt_par_QTextFrameFormat( 1 )->setBottomMargin( hb_parnd( 2 ) );
}

/*
 * void setHeight ( const QTextLength & height )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETHEIGHT )
{
   hbqt_par_QTextFrameFormat( 1 )->setHeight( *hbqt_par_QTextLength( 2 ) );
}

/*
 * void setHeight ( qreal height )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETHEIGHT_1 )
{
   hbqt_par_QTextFrameFormat( 1 )->setHeight( hb_parnd( 2 ) );
}

/*
 * void setLeftMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETLEFTMARGIN )
{
   hbqt_par_QTextFrameFormat( 1 )->setLeftMargin( hb_parnd( 2 ) );
}

/*
 * void setMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETMARGIN )
{
   hbqt_par_QTextFrameFormat( 1 )->setMargin( hb_parnd( 2 ) );
}

/*
 * void setPadding ( qreal width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPADDING )
{
   hbqt_par_QTextFrameFormat( 1 )->setPadding( hb_parnd( 2 ) );
}

/*
 * void setPageBreakPolicy ( PageBreakFlags policy )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPAGEBREAKPOLICY )
{
   hbqt_par_QTextFrameFormat( 1 )->setPageBreakPolicy( ( QTextFrameFormat::PageBreakFlags ) hb_parni( 2 ) );
}

/*
 * void setPosition ( Position policy )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPOSITION )
{
   hbqt_par_QTextFrameFormat( 1 )->setPosition( ( QTextFrameFormat::Position ) hb_parni( 2 ) );
}

/*
 * void setRightMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETRIGHTMARGIN )
{
   hbqt_par_QTextFrameFormat( 1 )->setRightMargin( hb_parnd( 2 ) );
}

/*
 * void setTopMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETTOPMARGIN )
{
   hbqt_par_QTextFrameFormat( 1 )->setTopMargin( hb_parnd( 2 ) );
}

/*
 * void setWidth ( const QTextLength & width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETWIDTH )
{
   hbqt_par_QTextFrameFormat( 1 )->setWidth( *hbqt_par_QTextLength( 2 ) );
}

/*
 * void setWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETWIDTH_1 )
{
   hbqt_par_QTextFrameFormat( 1 )->setWidth( hb_parnd( 2 ) );
}

/*
 * qreal topMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_TOPMARGIN )
{
   hb_retnd( hbqt_par_QTextFrameFormat( 1 )->topMargin() );
}

/*
 * QTextLength width () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_WIDTH )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTextLength( hbqt_par_QTextFrameFormat( 1 )->width() ), release_QTextLength ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
