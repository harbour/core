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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum CursorMode { SkipCharacters, SkipWords }
 */

/*
 *  Constructed[ 29/32 [ 90.63% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void draw ( QPainter * p, const QPointF & pos, const QVector<FormatRange> & selections = QVector<FormatRange> (), const QRectF & clip = QRectF() ) const
 *  void setAdditionalFormats ( const QList<FormatRange> & formatList )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //QList<FormatRange> additionalFormats () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextLayout>


/*
 * QTextLayout ()
 * QTextLayout ( const QString & text )
 * QTextLayout ( const QString & text, const QFont & font, QPaintDevice * paintdevice = 0 )
 * ~QTextLayout ()
 */

typedef struct
{
   QTextLayout * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextLayout;

QT_G_FUNC( hbqt_gcRelease_QTextLayout )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextLayout   /.\\", p->ph ) );
         delete ( ( QTextLayout * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextLayout   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextLayout    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextLayout    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextLayout( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextLayout * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextLayout;
   p->type = HBQT_TYPE_QTextLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextLayout", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTLAYOUT )
{
   QTextLayout * pObj = NULL;

   pObj =  new QTextLayout() ;

   hb_retptrGC( hbqt_gcAllocate_QTextLayout( ( void * ) pObj, true ) );
}

/*
 * void beginLayout ()
 */
HB_FUNC( QT_QTEXTLAYOUT_BEGINLAYOUT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->beginLayout();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_BEGINLAYOUT FP=( p )->beginLayout(); p is NULL" ) );
   }
}

/*
 * QRectF boundingRect () const
 */
HB_FUNC( QT_QTEXTLAYOUT_BOUNDINGRECT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool cacheEnabled () const
 */
HB_FUNC( QT_QTEXTLAYOUT_CACHEENABLED )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retl( ( p )->cacheEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_CACHEENABLED FP=hb_retl( ( p )->cacheEnabled() ); p is NULL" ) );
   }
}

/*
 * void clearAdditionalFormats ()
 */
HB_FUNC( QT_QTEXTLAYOUT_CLEARADDITIONALFORMATS )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->clearAdditionalFormats();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_CLEARADDITIONALFORMATS FP=( p )->clearAdditionalFormats(); p is NULL" ) );
   }
}

/*
 * void clearLayout ()
 */
HB_FUNC( QT_QTEXTLAYOUT_CLEARLAYOUT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->clearLayout();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_CLEARLAYOUT FP=( p )->clearLayout(); p is NULL" ) );
   }
}

/*
 * QTextLine createLine ()
 */
HB_FUNC( QT_QTEXTLAYOUT_CREATELINE )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->createLine() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_CREATELINE FP=hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->createLine() ), true ) ); p is NULL" ) );
   }
}

/*
 * void drawCursor ( QPainter * painter, const QPointF & position, int cursorPosition, int width ) const
 */
HB_FUNC( QT_QTEXTLAYOUT_DRAWCURSOR )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->drawCursor( hbqt_par_QPainter( 2 ), *hbqt_par_QPointF( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_DRAWCURSOR FP=( p )->drawCursor( hbqt_par_QPainter( 2 ), *hbqt_par_QPointF( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void drawCursor ( QPainter * painter, const QPointF & position, int cursorPosition ) const
 */
HB_FUNC( QT_QTEXTLAYOUT_DRAWCURSOR_1 )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->drawCursor( hbqt_par_QPainter( 2 ), *hbqt_par_QPointF( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_DRAWCURSOR_1 FP=( p )->drawCursor( hbqt_par_QPainter( 2 ), *hbqt_par_QPointF( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void endLayout ()
 */
HB_FUNC( QT_QTEXTLAYOUT_ENDLAYOUT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->endLayout();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_ENDLAYOUT FP=( p )->endLayout(); p is NULL" ) );
   }
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QTEXTLAYOUT_FONT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isValidCursorPosition ( int pos ) const
 */
HB_FUNC( QT_QTEXTLAYOUT_ISVALIDCURSORPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retl( ( p )->isValidCursorPosition( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_ISVALIDCURSORPOSITION FP=hb_retl( ( p )->isValidCursorPosition( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QTextLine lineAt ( int i ) const
 */
HB_FUNC( QT_QTEXTLAYOUT_LINEAT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->lineAt( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_LINEAT FP=hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->lineAt( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int lineCount () const
 */
HB_FUNC( QT_QTEXTLAYOUT_LINECOUNT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->lineCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_LINECOUNT FP=hb_retni( ( p )->lineCount() ); p is NULL" ) );
   }
}

/*
 * QTextLine lineForTextPosition ( int pos ) const
 */
HB_FUNC( QT_QTEXTLAYOUT_LINEFORTEXTPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->lineForTextPosition( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_LINEFORTEXTPOSITION FP=hb_retptrGC( hbqt_gcAllocate_QTextLine( new QTextLine( ( p )->lineForTextPosition( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal maximumWidth () const
 */
HB_FUNC( QT_QTEXTLAYOUT_MAXIMUMWIDTH )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retnd( ( p )->maximumWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_MAXIMUMWIDTH FP=hb_retnd( ( p )->maximumWidth() ); p is NULL" ) );
   }
}

/*
 * qreal minimumWidth () const
 */
HB_FUNC( QT_QTEXTLAYOUT_MINIMUMWIDTH )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retnd( ( p )->minimumWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_MINIMUMWIDTH FP=hb_retnd( ( p )->minimumWidth() ); p is NULL" ) );
   }
}

/*
 * int nextCursorPosition ( int oldPos, CursorMode mode = SkipCharacters ) const
 */
HB_FUNC( QT_QTEXTLAYOUT_NEXTCURSORPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->nextCursorPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLayout::CursorMode ) hb_parni( 3 ) : ( QTextLayout::CursorMode ) QTextLayout::SkipCharacters ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_NEXTCURSORPOSITION FP=hb_retni( ( p )->nextCursorPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLayout::CursorMode ) hb_parni( 3 ) : ( QTextLayout::CursorMode ) QTextLayout::SkipCharacters ) ) ); p is NULL" ) );
   }
}

/*
 * QPointF position () const
 */
HB_FUNC( QT_QTEXTLAYOUT_POSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->position() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_POSITION FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->position() ), true ) ); p is NULL" ) );
   }
}

/*
 * int preeditAreaPosition () const
 */
HB_FUNC( QT_QTEXTLAYOUT_PREEDITAREAPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->preeditAreaPosition() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_PREEDITAREAPOSITION FP=hb_retni( ( p )->preeditAreaPosition() ); p is NULL" ) );
   }
}

/*
 * QString preeditAreaText () const
 */
HB_FUNC( QT_QTEXTLAYOUT_PREEDITAREATEXT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retc( ( p )->preeditAreaText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_PREEDITAREATEXT FP=hb_retc( ( p )->preeditAreaText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int previousCursorPosition ( int oldPos, CursorMode mode = SkipCharacters ) const
 */
HB_FUNC( QT_QTEXTLAYOUT_PREVIOUSCURSORPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retni( ( p )->previousCursorPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLayout::CursorMode ) hb_parni( 3 ) : ( QTextLayout::CursorMode ) QTextLayout::SkipCharacters ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_PREVIOUSCURSORPOSITION FP=hb_retni( ( p )->previousCursorPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextLayout::CursorMode ) hb_parni( 3 ) : ( QTextLayout::CursorMode ) QTextLayout::SkipCharacters ) ) ); p is NULL" ) );
   }
}

/*
 * void setCacheEnabled ( bool enable )
 */
HB_FUNC( QT_QTEXTLAYOUT_SETCACHEENABLED )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setCacheEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_SETCACHEENABLED FP=( p )->setCacheEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QTEXTLAYOUT_SETFONT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPosition ( const QPointF & p )
 */
HB_FUNC( QT_QTEXTLAYOUT_SETPOSITION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setPosition( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_SETPOSITION FP=( p )->setPosition( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPreeditArea ( int position, const QString & text )
 */
HB_FUNC( QT_QTEXTLAYOUT_SETPREEDITAREA )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setPreeditArea( hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_SETPREEDITAREA FP=( p )->setPreeditArea( hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & string )
 */
HB_FUNC( QT_QTEXTLAYOUT_SETTEXT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setText( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_SETTEXT FP=( p )->setText( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextOption ( const QTextOption & option )
 */
HB_FUNC( QT_QTEXTLAYOUT_SETTEXTOPTION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      ( p )->setTextOption( *hbqt_par_QTextOption( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_SETTEXTOPTION FP=( p )->setTextOption( *hbqt_par_QTextOption( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTEXTLAYOUT_TEXT )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextOption textOption () const
 */
HB_FUNC( QT_QTEXTLAYOUT_TEXTOPTION )
{
   QTextLayout * p = hbqt_par_QTextLayout( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextOption( new QTextOption( ( p )->textOption() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTLAYOUT_TEXTOPTION FP=hb_retptrGC( hbqt_gcAllocate_QTextOption( new QTextOption( ( p )->textOption() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
