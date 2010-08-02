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
 *  enum MoveMode { MoveAnchor, KeepAnchor }
 *  enum MoveOperation { NoMove, Start, StartOfLine, StartOfBlock, ..., PreviousRow }
 *  enum SelectionType { Document, BlockUnderCursor, LineUnderCursor, WordUnderCursor }
 */

#include <QtCore/QPointer>

#include <QtGui/QTextBlock>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocumentFragment>


/*
 * QTextCursor ()
 * QTextCursor ( QTextDocument * document )
 * QTextCursor ( QTextFrame * frame )
 * QTextCursor ( const QTextBlock & block )
 * QTextCursor ( const QTextCursor & cursor )
 * ~QTextCursor ()
 */

typedef struct
{
   QTextCursor * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextCursor;

QT_G_FUNC( hbqt_gcRelease_QTextCursor )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextCursor   /.\\", p->ph ) );
         delete ( ( QTextCursor * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextCursor   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextCursor    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextCursor    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextCursor( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextCursor * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextCursor;
   p->type = HBQT_TYPE_QTextCursor;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextCursor", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextCursor", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTCURSOR )
{
   QTextCursor * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QTextCursor( *hbqt_par_QTextCursor( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString object = hbqt_par_QString( 1 );

      if( object == ( QString ) "QTextDocument" )
      {
         pObj =  new QTextCursor( hbqt_par_QTextDocument( 2 ) ) ;
      }
      if( object == ( QString ) "QTextBlock" )
      {
         pObj =  new QTextCursor( *hbqt_par_QTextBlock( 2 ) ) ;
      }
      if( object == ( QString ) "QTextFrame" )
      {
         pObj =  new QTextCursor( hbqt_par_QTextFrame( 2 ) ) ;
      }
      else
      {
         pObj =  new QTextCursor() ;
      }
   }
   else
   {
      pObj =  new QTextCursor() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTextCursor( ( void * ) pObj, true ) );
}

/*
 * int anchor () const
 */
HB_FUNC( QT_QTEXTCURSOR_ANCHOR )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retni( ( p )->anchor() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ANCHOR FP=hb_retni( ( p )->anchor() ); p is NULL" ) );
   }
}

/*
 * bool atBlockEnd () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATBLOCKEND )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->atBlockEnd() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ATBLOCKEND FP=hb_retl( ( p )->atBlockEnd() ); p is NULL" ) );
   }
}

/*
 * bool atBlockStart () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATBLOCKSTART )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->atBlockStart() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ATBLOCKSTART FP=hb_retl( ( p )->atBlockStart() ); p is NULL" ) );
   }
}

/*
 * bool atEnd () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATEND )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->atEnd() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ATEND FP=hb_retl( ( p )->atEnd() ); p is NULL" ) );
   }
}

/*
 * bool atStart () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATSTART )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->atStart() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ATSTART FP=hb_retl( ( p )->atStart() ); p is NULL" ) );
   }
}

/*
 * void beginEditBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_BEGINEDITBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->beginEditBlock();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_BEGINEDITBLOCK FP=( p )->beginEditBlock(); p is NULL" ) );
   }
}

/*
 * QTextBlock block () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->block() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_BLOCK FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->block() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat blockCharFormat () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCKCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->blockCharFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_BLOCKCHARFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->blockCharFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextBlockFormat blockFormat () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCKFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->blockFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_BLOCKFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->blockFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * int blockNumber () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCKNUMBER )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retni( ( p )->blockNumber() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_BLOCKNUMBER FP=hb_retni( ( p )->blockNumber() ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat charFormat () const
 */
HB_FUNC( QT_QTEXTCURSOR_CHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_CHARFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QTEXTCURSOR_CLEARSELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->clearSelection();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_CLEARSELECTION FP=( p )->clearSelection(); p is NULL" ) );
   }
}

/*
 * int columnNumber () const
 */
HB_FUNC( QT_QTEXTCURSOR_COLUMNNUMBER )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retni( ( p )->columnNumber() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_COLUMNNUMBER FP=hb_retni( ( p )->columnNumber() ); p is NULL" ) );
   }
}

/*
 * QTextList * createList ( const QTextListFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_CREATELIST )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->createList( *hbqt_par_QTextListFormat( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_CREATELIST FP=hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->createList( *hbqt_par_QTextListFormat( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextList * createList ( QTextListFormat::Style style )
 */
HB_FUNC( QT_QTEXTCURSOR_CREATELIST_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->createList( ( QTextListFormat::Style ) hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_CREATELIST_1 FP=hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->createList( ( QTextListFormat::Style ) hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextFrame * currentFrame () const
 */
HB_FUNC( QT_QTEXTCURSOR_CURRENTFRAME )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->currentFrame(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_CURRENTFRAME FP=hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->currentFrame(), false ) ); p is NULL" ) );
   }
}

/*
 * QTextList * currentList () const
 */
HB_FUNC( QT_QTEXTCURSOR_CURRENTLIST )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->currentList(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_CURRENTLIST FP=hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->currentList(), false ) ); p is NULL" ) );
   }
}

/*
 * void deleteChar ()
 */
HB_FUNC( QT_QTEXTCURSOR_DELETECHAR )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->deleteChar();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_DELETECHAR FP=( p )->deleteChar(); p is NULL" ) );
   }
}

/*
 * void deletePreviousChar ()
 */
HB_FUNC( QT_QTEXTCURSOR_DELETEPREVIOUSCHAR )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->deletePreviousChar();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_DELETEPREVIOUSCHAR FP=( p )->deletePreviousChar(); p is NULL" ) );
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTCURSOR_DOCUMENT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_DOCUMENT FP=hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) ); p is NULL" ) );
   }
}

/*
 * void endEditBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_ENDEDITBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->endEditBlock();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ENDEDITBLOCK FP=( p )->endEditBlock(); p is NULL" ) );
   }
}

/*
 * bool hasComplexSelection () const
 */
HB_FUNC( QT_QTEXTCURSOR_HASCOMPLEXSELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->hasComplexSelection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_HASCOMPLEXSELECTION FP=hb_retl( ( p )->hasComplexSelection() ); p is NULL" ) );
   }
}

/*
 * bool hasSelection () const
 */
HB_FUNC( QT_QTEXTCURSOR_HASSELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->hasSelection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_HASSELECTION FP=hb_retl( ( p )->hasSelection() ); p is NULL" ) );
   }
}

/*
 * void insertBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertBlock();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTBLOCK FP=( p )->insertBlock(); p is NULL" ) );
   }
}

/*
 * void insertBlock ( const QTextBlockFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTBLOCK_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertBlock( *hbqt_par_QTextBlockFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTBLOCK_1 FP=( p )->insertBlock( *hbqt_par_QTextBlockFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void insertBlock ( const QTextBlockFormat & format, const QTextCharFormat & charFormat )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTBLOCK_2 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertBlock( *hbqt_par_QTextBlockFormat( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTBLOCK_2 FP=( p )->insertBlock( *hbqt_par_QTextBlockFormat( 2 ), *hbqt_par_QTextCharFormat( 3 ) ); p is NULL" ) );
   }
}

/*
 * void insertFragment ( const QTextDocumentFragment & fragment )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTFRAGMENT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertFragment( *hbqt_par_QTextDocumentFragment( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTFRAGMENT FP=( p )->insertFragment( *hbqt_par_QTextDocumentFragment( 2 ) ); p is NULL" ) );
   }
}

/*
 * QTextFrame * insertFrame ( const QTextFrameFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTFRAME )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->insertFrame( *hbqt_par_QTextFrameFormat( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTFRAME FP=hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->insertFrame( *hbqt_par_QTextFrameFormat( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void insertHtml ( const QString & html )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTHTML )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertHtml( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTHTML FP=( p )->insertHtml( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void insertImage ( const QString & name )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertImage( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTIMAGE FP=( p )->insertImage( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void insertImage ( const QTextImageFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertImage( *hbqt_par_QTextImageFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTIMAGE_1 FP=( p )->insertImage( *hbqt_par_QTextImageFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void insertImage ( const QTextImageFormat & format, QTextFrameFormat::Position alignment )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE_2 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertImage( *hbqt_par_QTextImageFormat( 2 ), ( QTextFrameFormat::Position ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTIMAGE_2 FP=( p )->insertImage( *hbqt_par_QTextImageFormat( 2 ), ( QTextFrameFormat::Position ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void insertImage ( const QImage & image, const QString & name = QString() )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE_3 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertImage( *hbqt_par_QImage( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTIMAGE_3 FP=( p )->insertImage( *hbqt_par_QImage( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * QTextList * insertList ( const QTextListFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTLIST )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->insertList( *hbqt_par_QTextListFormat( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTLIST FP=hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->insertList( *hbqt_par_QTextListFormat( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextList * insertList ( QTextListFormat::Style style )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTLIST_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->insertList( ( QTextListFormat::Style ) hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTLIST_1 FP=hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->insertList( ( QTextListFormat::Style ) hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void insertText ( const QString & text )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTTEXT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertText( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTTEXT FP=( p )->insertText( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void insertText ( const QString & text, const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTTEXT_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->insertText( hbqt_par_QString( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_INSERTTEXT_1 FP=( p )->insertText( hbqt_par_QString( 2 ), *hbqt_par_QTextCharFormat( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool isCopyOf ( const QTextCursor & other ) const
 */
HB_FUNC( QT_QTEXTCURSOR_ISCOPYOF )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->isCopyOf( *hbqt_par_QTextCursor( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ISCOPYOF FP=hb_retl( ( p )->isCopyOf( *hbqt_par_QTextCursor( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QTEXTCURSOR_ISNULL )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * void joinPreviousEditBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_JOINPREVIOUSEDITBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->joinPreviousEditBlock();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_JOINPREVIOUSEDITBLOCK FP=( p )->joinPreviousEditBlock(); p is NULL" ) );
   }
}

/*
 * void mergeBlockCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QTEXTCURSOR_MERGEBLOCKCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->mergeBlockCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_MERGEBLOCKCHARFORMAT FP=( p )->mergeBlockCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void mergeBlockFormat ( const QTextBlockFormat & modifier )
 */
HB_FUNC( QT_QTEXTCURSOR_MERGEBLOCKFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->mergeBlockFormat( *hbqt_par_QTextBlockFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_MERGEBLOCKFORMAT FP=( p )->mergeBlockFormat( *hbqt_par_QTextBlockFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void mergeCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QTEXTCURSOR_MERGECHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->mergeCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_MERGECHARFORMAT FP=( p )->mergeCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool movePosition ( MoveOperation operation, MoveMode mode = MoveAnchor, int n = 1 )
 */
HB_FUNC( QT_QTEXTCURSOR_MOVEPOSITION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->movePosition( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ), hb_parnidef( 4, 1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_MOVEPOSITION FP=hb_retl( ( p )->movePosition( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ), hb_parnidef( 4, 1 ) ) ); p is NULL" ) );
   }
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTCURSOR_POSITION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retni( ( p )->position() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_POSITION FP=hb_retni( ( p )->position() ); p is NULL" ) );
   }
}

/*
 * void removeSelectedText ()
 */
HB_FUNC( QT_QTEXTCURSOR_REMOVESELECTEDTEXT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->removeSelectedText();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_REMOVESELECTEDTEXT FP=( p )->removeSelectedText(); p is NULL" ) );
   }
}

/*
 * void select ( SelectionType selection )
 */
HB_FUNC( QT_QTEXTCURSOR_SELECT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->select( ( QTextCursor::SelectionType ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SELECT FP=( p )->select( ( QTextCursor::SelectionType ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void selectedTableCells ( int * firstRow, int * numRows, int * firstColumn, int * numColumns ) const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTEDTABLECELLS )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   int iFirstRow = 0;
   int iNumRows = 0;
   int iFirstColumn = 0;
   int iNumColumns = 0;

   if( p )
      ( p )->selectedTableCells( &iFirstRow, &iNumRows, &iFirstColumn, &iNumColumns );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SELECTEDTABLECELLS FP=( p )->selectedTableCells( &iFirstRow, &iNumRows, &iFirstColumn, &iNumColumns ); p is NULL" ) );
   }

   hb_storni( iFirstRow, 2 );
   hb_storni( iNumRows, 3 );
   hb_storni( iFirstColumn, 4 );
   hb_storni( iNumColumns, 5 );
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTEDTEXT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retc( ( p )->selectedText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SELECTEDTEXT FP=hb_retc( ( p )->selectedText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextDocumentFragment selection () const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( ( p )->selection() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SELECTION FP=hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( ( p )->selection() ), true ) ); p is NULL" ) );
   }
}

/*
 * int selectionEnd () const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTIONEND )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retni( ( p )->selectionEnd() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SELECTIONEND FP=hb_retni( ( p )->selectionEnd() ); p is NULL" ) );
   }
}

/*
 * int selectionStart () const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTIONSTART )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retni( ( p )->selectionStart() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SELECTIONSTART FP=hb_retni( ( p )->selectionStart() ); p is NULL" ) );
   }
}

/*
 * void setBlockCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_SETBLOCKCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->setBlockCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SETBLOCKCHARFORMAT FP=( p )->setBlockCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBlockFormat ( const QTextBlockFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_SETBLOCKFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->setBlockFormat( *hbqt_par_QTextBlockFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SETBLOCKFORMAT FP=( p )->setBlockFormat( *hbqt_par_QTextBlockFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_SETCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->setCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SETCHARFORMAT FP=( p )->setCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPosition ( int pos, MoveMode m = MoveAnchor )
 */
HB_FUNC( QT_QTEXTCURSOR_SETPOSITION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->setPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SETPOSITION FP=( p )->setPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) ); p is NULL" ) );
   }
}

/*
 * void setVisualNavigation ( bool b )
 */
HB_FUNC( QT_QTEXTCURSOR_SETVISUALNAVIGATION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      ( p )->setVisualNavigation( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_SETVISUALNAVIGATION FP=( p )->setVisualNavigation( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool visualNavigation () const
 */
HB_FUNC( QT_QTEXTCURSOR_VISUALNAVIGATION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
      hb_retl( ( p )->visualNavigation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTCURSOR_VISUALNAVIGATION FP=hb_retl( ( p )->visualNavigation() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
