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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

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

typedef struct
{
   QTextFrameFormat * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextFrameFormat;

QT_G_FUNC( hbqt_gcRelease_QTextFrameFormat )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextFrameFormat   /.\\", p->ph ) );
         delete ( ( QTextFrameFormat * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextFrameFormat   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextFrameFormat    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextFrameFormat    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFrameFormat( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextFrameFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFrameFormat;
   p->type = HBQT_TYPE_QTextFrameFormat;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextFrameFormat", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextFrameFormat", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTFRAMEFORMAT )
{
   QTextFrameFormat * pObj = NULL;

   pObj =  new QTextFrameFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( ( void * ) pObj, true ) );
}

/*
 * qreal border () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDER )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->border() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_BORDER FP=hb_retnd( ( p )->border() ); p is NULL" ) );
   }
}

/*
 * QBrush borderBrush () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDERBRUSH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->borderBrush() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_BORDERBRUSH FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->borderBrush() ), true ) ); p is NULL" ) );
   }
}

/*
 * BorderStyle borderStyle () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BORDERSTYLE )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retni( ( QTextFrameFormat::BorderStyle ) ( p )->borderStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_BORDERSTYLE FP=hb_retni( ( QTextFrameFormat::BorderStyle ) ( p )->borderStyle() ); p is NULL" ) );
   }
}

/*
 * qreal bottomMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_BOTTOMMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->bottomMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_BOTTOMMARGIN FP=hb_retnd( ( p )->bottomMargin() ); p is NULL" ) );
   }
}

/*
 * QTextLength height () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_HEIGHT )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->height() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_HEIGHT FP=hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->height() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_ISVALID )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * qreal leftMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_LEFTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->leftMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_LEFTMARGIN FP=hb_retnd( ( p )->leftMargin() ); p is NULL" ) );
   }
}

/*
 * qreal margin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_MARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->margin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_MARGIN FP=hb_retnd( ( p )->margin() ); p is NULL" ) );
   }
}

/*
 * qreal padding () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_PADDING )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->padding() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_PADDING FP=hb_retnd( ( p )->padding() ); p is NULL" ) );
   }
}

/*
 * PageBreakFlags pageBreakPolicy () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_PAGEBREAKPOLICY )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retni( ( QTextFrameFormat::PageBreakFlags ) ( p )->pageBreakPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_PAGEBREAKPOLICY FP=hb_retni( ( QTextFrameFormat::PageBreakFlags ) ( p )->pageBreakPolicy() ); p is NULL" ) );
   }
}

/*
 * Position position () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_POSITION )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retni( ( QTextFrameFormat::Position ) ( p )->position() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_POSITION FP=hb_retni( ( QTextFrameFormat::Position ) ( p )->position() ); p is NULL" ) );
   }
}

/*
 * qreal rightMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_RIGHTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->rightMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_RIGHTMARGIN FP=hb_retnd( ( p )->rightMargin() ); p is NULL" ) );
   }
}

/*
 * void setBorder ( qreal width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDER )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBorder( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETBORDER FP=( p )->setBorder( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBorderBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDERBRUSH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBorderBrush( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETBORDERBRUSH FP=( p )->setBorderBrush( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBorderStyle ( BorderStyle style )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBORDERSTYLE )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBorderStyle( ( QTextFrameFormat::BorderStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETBORDERSTYLE FP=( p )->setBorderStyle( ( QTextFrameFormat::BorderStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBottomMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETBOTTOMMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setBottomMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETBOTTOMMARGIN FP=( p )->setBottomMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHeight ( const QTextLength & height )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETHEIGHT )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setHeight( *hbqt_par_QTextLength( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETHEIGHT FP=( p )->setHeight( *hbqt_par_QTextLength( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHeight ( qreal height )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETHEIGHT_1 )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setHeight( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETHEIGHT_1 FP=( p )->setHeight( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLeftMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETLEFTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setLeftMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETLEFTMARGIN FP=( p )->setLeftMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETMARGIN FP=( p )->setMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPadding ( qreal width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPADDING )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setPadding( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETPADDING FP=( p )->setPadding( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPageBreakPolicy ( PageBreakFlags policy )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPAGEBREAKPOLICY )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setPageBreakPolicy( ( QTextFrameFormat::PageBreakFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETPAGEBREAKPOLICY FP=( p )->setPageBreakPolicy( ( QTextFrameFormat::PageBreakFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPosition ( Position policy )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETPOSITION )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setPosition( ( QTextFrameFormat::Position ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETPOSITION FP=( p )->setPosition( ( QTextFrameFormat::Position ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRightMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETRIGHTMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setRightMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETRIGHTMARGIN FP=( p )->setRightMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTopMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETTOPMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setTopMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETTOPMARGIN FP=( p )->setTopMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidth ( const QTextLength & width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETWIDTH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setWidth( *hbqt_par_QTextLength( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETWIDTH FP=( p )->setWidth( *hbqt_par_QTextLength( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_SETWIDTH_1 )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_SETWIDTH_1 FP=( p )->setWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * qreal topMargin () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_TOPMARGIN )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retnd( ( p )->topMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_TOPMARGIN FP=hb_retnd( ( p )->topMargin() ); p is NULL" ) );
   }
}

/*
 * QTextLength width () const
 */
HB_FUNC( QT_QTEXTFRAMEFORMAT_WIDTH )
{
   QTextFrameFormat * p = hbqt_par_QTextFrameFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->width() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAMEFORMAT_WIDTH FP=hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->width() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
