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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QTextBlockFormat;

QT_G_FUNC( hbqt_gcRelease_QTextBlockFormat )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextBlockFormat * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextBlockFormat           ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextBlockFormat            Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextBlockFormat            Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBlockFormat( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBlockFormat;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextBlockFormat           ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTBLOCKFORMAT )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QTextBlockFormat* ) new QTextBlockFormat( *hbqt_par_QTextBlockFormat( 1 ) ) ;
   }
   else
   {
      pObj = ( QTextBlockFormat* ) new QTextBlockFormat() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( pObj, true ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_ALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QTextBlockFormat( 1 )->alignment() );
}

/*
 * qreal bottomMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_BOTTOMMARGIN )
{
   hb_retnd( hbqt_par_QTextBlockFormat( 1 )->bottomMargin() );
}

/*
 * int indent () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_INDENT )
{
   hb_retni( hbqt_par_QTextBlockFormat( 1 )->indent() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_ISVALID )
{
   hb_retl( hbqt_par_QTextBlockFormat( 1 )->isValid() );
}

/*
 * qreal leftMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_LEFTMARGIN )
{
   hb_retnd( hbqt_par_QTextBlockFormat( 1 )->leftMargin() );
}

/*
 * bool nonBreakableLines () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_NONBREAKABLELINES )
{
   hb_retl( hbqt_par_QTextBlockFormat( 1 )->nonBreakableLines() );
}

/*
 * PageBreakFlags pageBreakPolicy () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_PAGEBREAKPOLICY )
{
   hb_retni( ( QTextBlockFormat::PageBreakFlags ) hbqt_par_QTextBlockFormat( 1 )->pageBreakPolicy() );
}

/*
 * qreal rightMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_RIGHTMARGIN )
{
   hb_retnd( hbqt_par_QTextBlockFormat( 1 )->rightMargin() );
}

/*
 * void setAlignment ( Qt::Alignment alignment )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETALIGNMENT )
{
   hbqt_par_QTextBlockFormat( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setBottomMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETBOTTOMMARGIN )
{
   hbqt_par_QTextBlockFormat( 1 )->setBottomMargin( hb_parnd( 2 ) );
}

/*
 * void setIndent ( int indentation )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETINDENT )
{
   hbqt_par_QTextBlockFormat( 1 )->setIndent( hb_parni( 2 ) );
}

/*
 * void setLeftMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETLEFTMARGIN )
{
   hbqt_par_QTextBlockFormat( 1 )->setLeftMargin( hb_parnd( 2 ) );
}

/*
 * void setNonBreakableLines ( bool b )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETNONBREAKABLELINES )
{
   hbqt_par_QTextBlockFormat( 1 )->setNonBreakableLines( hb_parl( 2 ) );
}

/*
 * void setPageBreakPolicy ( PageBreakFlags policy )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETPAGEBREAKPOLICY )
{
   hbqt_par_QTextBlockFormat( 1 )->setPageBreakPolicy( ( QTextBlockFormat::PageBreakFlags ) hb_parni( 2 ) );
}

/*
 * void setRightMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETRIGHTMARGIN )
{
   hbqt_par_QTextBlockFormat( 1 )->setRightMargin( hb_parnd( 2 ) );
}

/*
 * void setTextIndent ( qreal indent )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTEXTINDENT )
{
   hbqt_par_QTextBlockFormat( 1 )->setTextIndent( hb_parnd( 2 ) );
}

/*
 * void setTopMargin ( qreal margin )
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_SETTOPMARGIN )
{
   hbqt_par_QTextBlockFormat( 1 )->setTopMargin( hb_parnd( 2 ) );
}

/*
 * qreal textIndent () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TEXTINDENT )
{
   hb_retnd( hbqt_par_QTextBlockFormat( 1 )->textIndent() );
}

/*
 * qreal topMargin () const
 */
HB_FUNC( QT_QTEXTBLOCKFORMAT_TOPMARGIN )
{
   hb_retnd( hbqt_par_QTextBlockFormat( 1 )->topMargin() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
