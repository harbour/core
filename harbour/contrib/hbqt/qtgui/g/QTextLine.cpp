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
   QTextLine * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextLine;

QT_G_FUNC( hbqt_gcRelease_QTextLine )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

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
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

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
      hb_retnd( ( p )->ascent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_ASCENT FP=hb_retnd( ( p )->ascent() ); p is NULL" ) );
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
      hb_retnd( ( p )->cursorToX( &iCursorPos, ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_CURSORTOX FP=hb_retnd( ( p )->cursorToX( &iCursorPos, ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) ); p is NULL" ) );
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
      hb_retnd( ( p )->cursorToX( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_CURSORTOX_1 FP=hb_retnd( ( p )->cursorToX( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::Edge ) hb_parni( 3 ) : ( QTextLine::Edge ) QTextLine::Leading ) ) ); p is NULL" ) );
   }
}

/*
 * qreal descent () const
 */
HB_FUNC( QT_QTEXTLINE_DESCENT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->descent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_DESCENT FP=hb_retnd( ( p )->descent() ); p is NULL" ) );
   }
}

/*
 * qreal height () const
 */
HB_FUNC( QT_QTEXTLINE_HEIGHT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_HEIGHT FP=hb_retnd( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTLINE_ISVALID )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * int lineNumber () const
 */
HB_FUNC( QT_QTEXTLINE_LINENUMBER )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->lineNumber() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_LINENUMBER FP=hb_retni( ( p )->lineNumber() ); p is NULL" ) );
   }
}

/*
 * QRectF naturalTextRect () const
 */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTRECT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->naturalTextRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_NATURALTEXTRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->naturalTextRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal naturalTextWidth () const
 */
HB_FUNC( QT_QTEXTLINE_NATURALTEXTWIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->naturalTextWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_NATURALTEXTWIDTH FP=hb_retnd( ( p )->naturalTextWidth() ); p is NULL" ) );
   }
}

/*
 * QPointF position () const
 */
HB_FUNC( QT_QTEXTLINE_POSITION )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->position() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_POSITION FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->position() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF rect () const
 */
HB_FUNC( QT_QTEXTLINE_RECT )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_RECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) ); p is NULL" ) );
   }
}

/*
 * void setLineWidth ( qreal width )
 */
HB_FUNC( QT_QTEXTLINE_SETLINEWIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setLineWidth( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_SETLINEWIDTH FP=( p )->setLineWidth( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNumColumns ( int numColumns )
 */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setNumColumns( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_SETNUMCOLUMNS FP=( p )->setNumColumns( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setNumColumns ( int numColumns, qreal alignmentWidth )
 */
HB_FUNC( QT_QTEXTLINE_SETNUMCOLUMNS_1 )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setNumColumns( hb_parni( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_SETNUMCOLUMNS_1 FP=( p )->setNumColumns( hb_parni( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setPosition ( const QPointF & pos )
 */
HB_FUNC( QT_QTEXTLINE_SETPOSITION )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      ( p )->setPosition( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_SETPOSITION FP=( p )->setPosition( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * int textLength () const
 */
HB_FUNC( QT_QTEXTLINE_TEXTLENGTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->textLength() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_TEXTLENGTH FP=hb_retni( ( p )->textLength() ); p is NULL" ) );
   }
}

/*
 * int textStart () const
 */
HB_FUNC( QT_QTEXTLINE_TEXTSTART )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->textStart() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_TEXTSTART FP=hb_retni( ( p )->textStart() ); p is NULL" ) );
   }
}

/*
 * qreal width () const
 */
HB_FUNC( QT_QTEXTLINE_WIDTH )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_WIDTH FP=hb_retnd( ( p )->width() ); p is NULL" ) );
   }
}

/*
 * qreal x () const
 */
HB_FUNC( QT_QTEXTLINE_X )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->x() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_X FP=hb_retnd( ( p )->x() ); p is NULL" ) );
   }
}

/*
 * int xToCursor ( qreal x, CursorPosition cpos = CursorBetweenCharacters ) const
 */
HB_FUNC( QT_QTEXTLINE_XTOCURSOR )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retni( ( p )->xToCursor( hb_parnd( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::CursorPosition ) hb_parni( 3 ) : ( QTextLine::CursorPosition ) QTextLine::CursorBetweenCharacters ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_XTOCURSOR FP=hb_retni( ( p )->xToCursor( hb_parnd( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLine::CursorPosition ) hb_parni( 3 ) : ( QTextLine::CursorPosition ) QTextLine::CursorBetweenCharacters ) ) ); p is NULL" ) );
   }
}

/*
 * qreal y () const
 */
HB_FUNC( QT_QTEXTLINE_Y )
{
   QTextLine * p = hbqt_par_QTextLine( 1 );
   if( p )
      hb_retnd( ( p )->y() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLINE_Y FP=hb_retnd( ( p )->y() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
