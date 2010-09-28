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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
   {
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   }
}

/*
 * qreal bottomMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_BOTTOMMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retnd( ( p )->bottomMargin() );
   }
}

/*
 * int indent () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_INDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retni( ( p )->indent() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_ISVALID )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * qreal leftMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_LEFTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retnd( ( p )->leftMargin() );
   }
}

/*
 * bool nonBreakableLines () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_NONBREAKABLELINES )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retl( ( p )->nonBreakableLines() );
   }
}

/*
 * PageBreakFlags pageBreakPolicy () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_PAGEBREAKPOLICY )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retni( ( QTextBlockFormat::PageBreakFlags ) ( p )->pageBreakPolicy() );
   }
}

/*
 * qreal rightMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_RIGHTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retnd( ( p )->rightMargin() );
   }
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETALIGNMENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   }
}

/*
 * void setBottomMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETBOTTOMMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setBottomMargin( hb_parnd( 2 ) );
   }
}

/*
 * void setIndent ( int indentation )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setIndent( hb_parni( 2 ) );
   }
}

/*
 * void setLeftMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETLEFTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setLeftMargin( hb_parnd( 2 ) );
   }
}

/*
 * void setNonBreakableLines ( bool b )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETNONBREAKABLELINES )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setNonBreakableLines( hb_parl( 2 ) );
   }
}

/*
 * void setPageBreakPolicy ( PageBreakFlags policy )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETPAGEBREAKPOLICY )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setPageBreakPolicy( ( QTextBlockFormat::PageBreakFlags ) hb_parni( 2 ) );
   }
}

/*
 * void setRightMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETRIGHTMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setRightMargin( hb_parnd( 2 ) );
   }
}

/*
 * void setTextIndent ( qreal indent )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTEXTINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setTextIndent( hb_parnd( 2 ) );
   }
}

/*
 * void setTopMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTOPMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      ( p )->setTopMargin( hb_parnd( 2 ) );
   }
}

/*
 * qreal textIndent () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TEXTINDENT )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retnd( ( p )->textIndent() );
   }
}

/*
 * qreal topMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TOPMARGIN )
{
   QTextBlockFormat * p = hbqt_par_QTextBlockFormat( 1 );
   if( p )
   {
      hb_retnd( ( p )->topMargin() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
