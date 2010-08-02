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
 *  -----------------------------
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextBlockFormat;

QT_G_FUNC( hbqt_gcRelease_QTextBlockFormat )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextBlockFormat   /.\\", p->ph ) );
         delete ( ( QTextBlockFormat * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextBlockFormat   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextBlockFormat    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextBlockFormat    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBlockFormat( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextBlockFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBlockFormat;
   p->type = HBQT_TYPE_QTextBlockFormat;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextBlockFormat", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextBlockFormat", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTBLOCKFORMAT )
{
   QTextBlockFormat * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QTextBlockFormat( *hbqt_par_QTextBlockFormat( 1 ) ) ;
   }
   else
   {
      pObj =  new QTextBlockFormat() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( ( void * ) pObj, true ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_ALIGNMENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_ALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->alignment() ); p is NULL" ) );
   }
}

/*
 * qreal bottomMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_BOTTOMMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->bottomMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_BOTTOMMARGIN FP=hb_retnd( ( p )->bottomMargin() ); p is NULL" ) );
   }
}

/*
 * int indent () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_INDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retni( ( p )->indent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_INDENT FP=hb_retni( ( p )->indent() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_ISVALID )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * qreal leftMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_LEFTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->leftMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_LEFTMARGIN FP=hb_retnd( ( p )->leftMargin() ); p is NULL" ) );
   }
}

/*
 * bool nonBreakableLines () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_NONBREAKABLELINES )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retl( ( p )->nonBreakableLines() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_NONBREAKABLELINES FP=hb_retl( ( p )->nonBreakableLines() ); p is NULL" ) );
   }
}

/*
 * PageBreakFlags pageBreakPolicy () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_PAGEBREAKPOLICY )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retni( ( QTextBlockFormat::PageBreakFlags ) ( p )->pageBreakPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_PAGEBREAKPOLICY FP=hb_retni( ( QTextBlockFormat::PageBreakFlags ) ( p )->pageBreakPolicy() ); p is NULL" ) );
   }
}

/*
 * qreal rightMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_RIGHTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->rightMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_RIGHTMARGIN FP=hb_retnd( ( p )->rightMargin() ); p is NULL" ) );
   }
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETALIGNMENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETALIGNMENT FP=( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBottomMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETBOTTOMMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setBottomMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETBOTTOMMARGIN FP=( p )->setBottomMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIndent ( int indentation )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setIndent( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETINDENT FP=( p )->setIndent( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLeftMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETLEFTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setLeftMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETLEFTMARGIN FP=( p )->setLeftMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNonBreakableLines ( bool b )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETNONBREAKABLELINES )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setNonBreakableLines( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETNONBREAKABLELINES FP=( p )->setNonBreakableLines( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPageBreakPolicy ( PageBreakFlags policy )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETPAGEBREAKPOLICY )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setPageBreakPolicy( ( QTextBlockFormat::PageBreakFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETPAGEBREAKPOLICY FP=( p )->setPageBreakPolicy( ( QTextBlockFormat::PageBreakFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRightMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETRIGHTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setRightMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETRIGHTMARGIN FP=( p )->setRightMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextIndent ( qreal indent )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTEXTINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setTextIndent( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETTEXTINDENT FP=( p )->setTextIndent( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTopMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTOPMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      ( p )->setTopMargin( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_SETTOPMARGIN FP=( p )->setTopMargin( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * qreal textIndent () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TEXTINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->textIndent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_TEXTINDENT FP=hb_retnd( ( p )->textIndent() ); p is NULL" ) );
   }
}

/*
 * qreal topMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TOPMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
      hb_retnd( ( p )->topMargin() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCKFORMAT_TOPMARGIN FP=hb_retnd( ( p )->topMargin() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
