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
 *  enum MoveMode { MoveAnchor, KeepAnchor }
 *  enum MoveOperation { NoMove, Start, StartOfLine, StartOfBlock, ..., PreviousRow }
 *  enum SelectionType { Document, BlockUnderCursor, LineUnderCursor, WordUnderCursor }
 */

/*
 *  Constructed[ 58/58 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QTextTable * currentTable () const
 *  //QTextTable * insertTable ( int rows, int columns, const QTextTableFormat & format )
 *  //QTextTable * insertTable ( int rows, int columns )
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextCursor;

HBQT_GC_FUNC( hbqt_gcRelease_QTextCursor )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
      pObj = new QTextCursor( *hbqt_par_QTextCursor( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString object = hbqt_par_QString( 1 );

      if( object == ( QString ) "QTextDocument" )
      {
         pObj = new QTextCursor( hbqt_par_QTextDocument( 2 ) ) ;
      }
      if( object == ( QString ) "QTextBlock" )
      {
         pObj = new QTextCursor( *hbqt_par_QTextBlock( 2 ) ) ;
      }
      if( object == ( QString ) "QTextFrame" )
      {
         pObj = new QTextCursor( hbqt_par_QTextFrame( 2 ) ) ;
      }
      else
      {
         pObj = new QTextCursor() ;
      }
   }
   else
   {
      pObj = new QTextCursor() ;
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
   {
      hb_retni( ( p )->anchor() );
   }
}

/*
 * bool atBlockEnd () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATBLOCKEND )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->atBlockEnd() );
   }
}

/*
 * bool atBlockStart () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATBLOCKSTART )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->atBlockStart() );
   }
}

/*
 * bool atEnd () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATEND )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->atEnd() );
   }
}

/*
 * bool atStart () const
 */
HB_FUNC( QT_QTEXTCURSOR_ATSTART )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->atStart() );
   }
}

/*
 * void beginEditBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_BEGINEDITBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->beginEditBlock();
   }
}

/*
 * QTextBlock block () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->block() ), true ) );
   }
}

/*
 * QTextCharFormat blockCharFormat () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCKCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->blockCharFormat() ), true ) );
   }
}

/*
 * QTextBlockFormat blockFormat () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCKFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->blockFormat() ), true ) );
   }
}

/*
 * int blockNumber () const
 */
HB_FUNC( QT_QTEXTCURSOR_BLOCKNUMBER )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retni( ( p )->blockNumber() );
   }
}

/*
 * QTextCharFormat charFormat () const
 */
HB_FUNC( QT_QTEXTCURSOR_CHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) );
   }
}

/*
 * void clearSelection ()
 */
HB_FUNC( QT_QTEXTCURSOR_CLEARSELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->clearSelection();
   }
}

/*
 * int columnNumber () const
 */
HB_FUNC( QT_QTEXTCURSOR_COLUMNNUMBER )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retni( ( p )->columnNumber() );
   }
}

/*
 * QTextList * createList ( const QTextListFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_CREATELIST )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->createList( *hbqt_par_QTextListFormat( 2 ) ), false ) );
   }
}

/*
 * QTextList * createList ( QTextListFormat::Style style )
 */
HB_FUNC( QT_QTEXTCURSOR_CREATELIST_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->createList( ( QTextListFormat::Style ) hb_parni( 2 ) ), false ) );
   }
}

/*
 * QTextFrame * currentFrame () const
 */
HB_FUNC( QT_QTEXTCURSOR_CURRENTFRAME )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->currentFrame(), false ) );
   }
}

/*
 * QTextList * currentList () const
 */
HB_FUNC( QT_QTEXTCURSOR_CURRENTLIST )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->currentList(), false ) );
   }
}

/*
 * void deleteChar ()
 */
HB_FUNC( QT_QTEXTCURSOR_DELETECHAR )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->deleteChar();
   }
}

/*
 * void deletePreviousChar ()
 */
HB_FUNC( QT_QTEXTCURSOR_DELETEPREVIOUSCHAR )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->deletePreviousChar();
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTCURSOR_DOCUMENT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   }
}

/*
 * void endEditBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_ENDEDITBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->endEditBlock();
   }
}

/*
 * bool hasComplexSelection () const
 */
HB_FUNC( QT_QTEXTCURSOR_HASCOMPLEXSELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->hasComplexSelection() );
   }
}

/*
 * bool hasSelection () const
 */
HB_FUNC( QT_QTEXTCURSOR_HASSELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->hasSelection() );
   }
}

/*
 * void insertBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->insertBlock();
   }
}

/*
 * void insertBlock ( const QTextBlockFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTBLOCK_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->insertBlock( *hbqt_par_QTextBlockFormat( 2 ) );
   }
}

/*
 * void insertBlock ( const QTextBlockFormat & format, const QTextCharFormat & charFormat )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTBLOCK_2 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->insertBlock( *hbqt_par_QTextBlockFormat( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
   }
}

/*
 * void insertFragment ( const QTextDocumentFragment & fragment )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTFRAGMENT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->insertFragment( *hbqt_par_QTextDocumentFragment( 2 ) );
   }
}

/*
 * QTextFrame * insertFrame ( const QTextFrameFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTFRAME )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->insertFrame( *hbqt_par_QTextFrameFormat( 2 ) ), false ) );
   }
}

/*
 * void insertHtml ( const QString & html )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTHTML )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertHtml( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void insertImage ( const QString & name )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertImage( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void insertImage ( const QTextImageFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->insertImage( *hbqt_par_QTextImageFormat( 2 ) );
   }
}

/*
 * void insertImage ( const QTextImageFormat & format, QTextFrameFormat::Position alignment )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE_2 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->insertImage( *hbqt_par_QTextImageFormat( 2 ), ( QTextFrameFormat::Position ) hb_parni( 3 ) );
   }
}

/*
 * void insertImage ( const QImage & image, const QString & name = QString() )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTIMAGE_3 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertImage( *hbqt_par_QImage( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QTextList * insertList ( const QTextListFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTLIST )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->insertList( *hbqt_par_QTextListFormat( 2 ) ), false ) );
   }
}

/*
 * QTextList * insertList ( QTextListFormat::Style style )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTLIST_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->insertList( ( QTextListFormat::Style ) hb_parni( 2 ) ), false ) );
   }
}

/*
 * void insertText ( const QString & text )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTTEXT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void insertText ( const QString & text, const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_INSERTTEXT_1 )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertText( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QTextCharFormat( 3 ) );
      hb_strfree( pText );
   }
}

/*
 * bool isCopyOf ( const QTextCursor & other ) const
 */
HB_FUNC( QT_QTEXTCURSOR_ISCOPYOF )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->isCopyOf( *hbqt_par_QTextCursor( 2 ) ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QTEXTCURSOR_ISNULL )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * void joinPreviousEditBlock ()
 */
HB_FUNC( QT_QTEXTCURSOR_JOINPREVIOUSEDITBLOCK )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->joinPreviousEditBlock();
   }
}

/*
 * void mergeBlockCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QTEXTCURSOR_MERGEBLOCKCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->mergeBlockCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * void mergeBlockFormat ( const QTextBlockFormat & modifier )
 */
HB_FUNC( QT_QTEXTCURSOR_MERGEBLOCKFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->mergeBlockFormat( *hbqt_par_QTextBlockFormat( 2 ) );
   }
}

/*
 * void mergeCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QTEXTCURSOR_MERGECHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->mergeCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * bool movePosition ( MoveOperation operation, MoveMode mode = MoveAnchor, int n = 1 )
 */
HB_FUNC( QT_QTEXTCURSOR_MOVEPOSITION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->movePosition( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ), hb_parnidef( 4, 1 ) ) );
   }
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTCURSOR_POSITION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retni( ( p )->position() );
   }
}

/*
 * void removeSelectedText ()
 */
HB_FUNC( QT_QTEXTCURSOR_REMOVESELECTEDTEXT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->removeSelectedText();
   }
}

/*
 * void select ( SelectionType selection )
 */
HB_FUNC( QT_QTEXTCURSOR_SELECT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->select( ( QTextCursor::SelectionType ) hb_parni( 2 ) );
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
   {
      ( p )->selectedTableCells( &iFirstRow, &iNumRows, &iFirstColumn, &iNumColumns );
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
   {
      hb_retstr_utf8( ( p )->selectedText().toUtf8().data() );
   }
}

/*
 * QTextDocumentFragment selection () const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( ( p )->selection() ), true ) );
   }
}

/*
 * int selectionEnd () const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTIONEND )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retni( ( p )->selectionEnd() );
   }
}

/*
 * int selectionStart () const
 */
HB_FUNC( QT_QTEXTCURSOR_SELECTIONSTART )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retni( ( p )->selectionStart() );
   }
}

/*
 * void setBlockCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_SETBLOCKCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->setBlockCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * void setBlockFormat ( const QTextBlockFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_SETBLOCKFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->setBlockFormat( *hbqt_par_QTextBlockFormat( 2 ) );
   }
}

/*
 * void setCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTCURSOR_SETCHARFORMAT )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->setCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * void setPosition ( int pos, MoveMode m = MoveAnchor )
 */
HB_FUNC( QT_QTEXTCURSOR_SETPOSITION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->setPosition( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) );
   }
}

/*
 * void setVisualNavigation ( bool b )
 */
HB_FUNC( QT_QTEXTCURSOR_SETVISUALNAVIGATION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      ( p )->setVisualNavigation( hb_parl( 2 ) );
   }
}

/*
 * bool visualNavigation () const
 */
HB_FUNC( QT_QTEXTCURSOR_VISUALNAVIGATION )
{
   QTextCursor * p = hbqt_par_QTextCursor( 1 );
   if( p )
   {
      hb_retl( ( p )->visualNavigation() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
