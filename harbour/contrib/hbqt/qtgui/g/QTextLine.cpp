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

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextLine   /.\\", p->ph ) );
         delete ( ( QTextLine * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextLine   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextLine    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextLine    :    Object not created with new=true", p->ph ) );
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

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextLine", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextLine", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTLINE )
{
   QTextLine * pObj = NULL;

   pObj =  new QTextLine() ;

   hb_retptrGC( hbqt_gcAllocate_QTextLine( ( void * ) pObj, true ) );
}

/*
 * qreal ascent () const
 */
HB_FUNC( QT_QTEXTLINE_ASCENT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->ascent() );
   }
}

/*
 * qreal cursorToX ( int * cursorPos, Edge edge = Leading ) const
 */
HB_FUNC( QT_QTEXTLINE_CURSORTOX )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   int iCursorPos = 0;

   if( p )
   {
      hb_retnd( ( p )->cursorToX( &iCursorPos, ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );
   }

   hb_storni( iCursorPos, 2 );
}

/*
 * qreal cursorToX ( int cursorPos, Edge edge = Leading ) const
 */
HB_FUNC( QT_QTEXTLINE_CURSORTOX_1 )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->cursorToX( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );
   }
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QTEXTLINE_DESCENT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->descent() );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QTEXTLINE_HEIGHT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->height() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTLINE_ISVALID )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * int lineNumber () const
 */
HB_FUNC( QT_QTEXTLINE_LINENUMBER )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retni( ( p )->lineNumber() );
   }
}

/*
 * QRectF naturalTextRect () const
 */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTRECT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->naturalTextRect() ), true ) );
   }
}

/*
 * qreal naturalTextWidth () const
 */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTWIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->naturalTextWidth() );
   }
}

/*
 * QPointF position () const
 */
HB_FUNC( QT_QTEXTLINE_POSITION )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->position() ), true ) );
   }
}

/*
 * QRectF rect () const
 */
HB_FUNC( QT_QTEXTLINE_RECT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
   }
}

/*
 * void setLineWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTLINE_SETLINEWIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      ( p )->setLineWidth( hb_parnd( 2 ) );
   }
}

/*
 * void setNumColumns ( int numColumns )
 */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      ( p )->setNumColumns( hb_parni( 2 ) );
   }
}

/*
 * void setNumColumns ( int numColumns, qreal alignmentWidth )
 */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS_1 )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      ( p )->setNumColumns( hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setPosition ( const QPointF & pos )
 */
HB_FUNC( QT_QTEXTLINE_SETPOSITION )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      ( p )->setPosition( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * int textLength () const
 */
HB_FUNC( QT_QTEXTLINE_TEXTLENGTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retni( ( p )->textLength() );
   }
}

/*
 * int textStart () const
 */
HB_FUNC( QT_QTEXTLINE_TEXTSTART )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retni( ( p )->textStart() );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QTEXTLINE_WIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->width() );
   }
}

/*
 * qreal x () const
 */
HB_FUNC( QT_QTEXTLINE_X )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->x() );
   }
}

/*
 * int xToCursor ( qreal x, CursorPosition cpos = CursorBetweenCharacters ) const
 */
HB_FUNC( QT_QTEXTLINE_XTOCURSOR )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retni( ( p )->xToCursor( hb_parnd( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::CursorPosition ) hb_parni( 3 ) : ( QTextLine::CursorPosition ) QTextLine::CursorBetweenCharacters ) ) );
   }
}

/*
 * qreal y () const
 */
HB_FUNC( QT_QTEXTLINE_Y )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
   {
      hb_retnd( ( p )->y() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
