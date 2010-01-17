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
 *  enum CursorPosition { CursorBetweenCharacters, CursorOnCharacter }
 *  enum Edge { Leading, Trailing }
 */

#include <QtCore/QPointer>

#include <QtGui/QTextLine>


/*
 * QTextLine ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QTextLine;

QT_G_FUNC( hbqt_gcRelease_QTextLine )
{
      QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextLine * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextLine                  ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextLine                   Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextLine                   Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextLine( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextLine;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextLine                  ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTLINE )
{
   void * pObj = NULL;

   pObj = ( QTextLine* ) new QTextLine() ;

   hb_retptrGC( hbqt_gcAllocate_QTextLine( pObj, true ) );
}
/*
 * qreal ascent () const
 */
HB_FUNC( QT_QTEXTLINE_ASCENT )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->ascent() );
}

/*
 * qreal cursorToX ( int * cursorPos, Edge edge = Leading ) const
 */
HB_FUNC( QT_QTEXTLINE_CURSORTOX )
{
   int iCursorPos = 0;

   hb_retnd( hbqt_par_QTextLine( 1 )->cursorToX( &iCursorPos, ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );

   hb_storni( iCursorPos, 2 );
}

/*
 * qreal cursorToX ( int cursorPos, Edge edge = Leading ) const
 */
HB_FUNC( QT_QTEXTLINE_CURSORTOX_1 )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->cursorToX( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QTEXTLINE_DESCENT )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->descent() );
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QTEXTLINE_HEIGHT )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->height() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTLINE_ISVALID )
{
   hb_retl( hbqt_par_QTextLine( 1 )->isValid() );
}

/*
 * int lineNumber () const
 */
HB_FUNC( QT_QTEXTLINE_LINENUMBER )
{
   hb_retni( hbqt_par_QTextLine( 1 )->lineNumber() );
}

/*
 * QRectF naturalTextRect () const
 */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( hbqt_par_QTextLine( 1 )->naturalTextRect() ), true ) );
}

/*
 * qreal naturalTextWidth () const
 */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTWIDTH )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->naturalTextWidth() );
}

/*
 * QPointF position () const
 */
HB_FUNC( QT_QTEXTLINE_POSITION )
{
   hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( hbqt_par_QTextLine( 1 )->position() ), true ) );
}

/*
 * QRectF rect () const
 */
HB_FUNC( QT_QTEXTLINE_RECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( hbqt_par_QTextLine( 1 )->rect() ), true ) );
}

/*
 * void setLineWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTLINE_SETLINEWIDTH )
{
   hbqt_par_QTextLine( 1 )->setLineWidth( hb_parnd( 2 ) );
}

/*
 * void setNumColumns ( int numColumns )
 */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS )
{
   hbqt_par_QTextLine( 1 )->setNumColumns( hb_parni( 2 ) );
}

/*
 * void setNumColumns ( int numColumns, qreal alignmentWidth )
 */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS_1 )
{
   hbqt_par_QTextLine( 1 )->setNumColumns( hb_parni( 2 ), hb_parnd( 3 ) );
}

/*
 * void setPosition ( const QPointF & pos )
 */
HB_FUNC( QT_QTEXTLINE_SETPOSITION )
{
   hbqt_par_QTextLine( 1 )->setPosition( *hbqt_par_QPointF( 2 ) );
}

/*
 * int textLength () const
 */
HB_FUNC( QT_QTEXTLINE_TEXTLENGTH )
{
   hb_retni( hbqt_par_QTextLine( 1 )->textLength() );
}

/*
 * int textStart () const
 */
HB_FUNC( QT_QTEXTLINE_TEXTSTART )
{
   hb_retni( hbqt_par_QTextLine( 1 )->textStart() );
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QTEXTLINE_WIDTH )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->width() );
}

/*
 * qreal x () const
 */
HB_FUNC( QT_QTEXTLINE_X )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->x() );
}

/*
 * int xToCursor ( qreal x, CursorPosition cpos = CursorBetweenCharacters ) const
 */
HB_FUNC( QT_QTEXTLINE_XTOCURSOR )
{
   hb_retni( hbqt_par_QTextLine( 1 )->xToCursor( hb_parnd( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::CursorPosition ) hb_parni( 3 ) : ( QTextLine::CursorPosition ) QTextLine::CursorBetweenCharacters ) ) );
}

/*
 * qreal y () const
 */
HB_FUNC( QT_QTEXTLINE_Y )
{
   hb_retnd( hbqt_par_QTextLine( 1 )->y() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
