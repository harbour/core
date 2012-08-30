/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour-Qt wrapper generator.
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
 * Copyright 2009 Gancov Kostya <kossne@mail.ru>
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
/*
 *  The code below is puled from TextEdit.cpp of QWriter by Gancov Kotsya
 *
 *  and adopted for Harbour's hbIDE interface. The code has been intensively
 *  formatted and changed to suit hbIDE and Harbour's wrappers for Qt.
 *  The special hilight for this adoption is <braces matching>, current line
 *  coloring and bookmarks.
 *
 *  So a big thank you.
 *
 *  Pritpal Bedi
*/
/*----------------------------------------------------------------------*/

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqplaintextedit.h"

HB_EXTERN_BEGIN
extern void * hbqt_gcAllocate_QKeyEvent( void * pObj, bool bNew );
HB_EXTERN_END

#include <QtGui/QApplication>

#define selectionMode_none                        0
#define selectionMode_stream                      1
#define selectionMode_column                      2
#define selectionMode_line                        3

#define selectionDisplay_none                     0
#define selectionDisplay_qt                       1
#define selectionDisplay_ide                      2

#define mouseMode_none                            0
#define mouseMode_select                          1
#define mouseMode_drag                            2

/*----------------------------------------------------------------------*/

HBQPlainTextEdit::HBQPlainTextEdit( QWidget * parent ) : QPlainTextEdit( parent )
{
   m_currentLineColor.setNamedColor( "#e8e8ff" );
   m_lineAreaBkColor.setNamedColor( "#e4e4e4" );
   m_horzRulerBkColor.setNamedColor( "whitesmoke" );
   m_matchBracesAll         = false;

   spaces                   = 3;
   spacesTab                = "";
   styleHightlighter        = "prg";
   numberBlock              = true;
   lineNumberArea           = new LineNumberArea( this );
   isTipActive              = false;
   columnBegins             = -1;
   columnEnds               = -1;
   rowBegins                = -1;
   rowEnds                  = -1;
   selectionMode            = selectionMode_stream;
   selectionDisplay         = selectionDisplay_none;
   isColumnSelectionON      = false;
   isLineSelectionON        = false;
   horzRulerHeight          = 20;
   horzRuler                = new HorzRuler( this );
   caretState               = 0;
   isSelectionByApplication = false;
   hitTestRow               = -1;
   hitTestColumn            = -1;
   highlight                = QRect( -1, -1, -1, -1 );
   isSelectionPersistent    = false;
   isShiftPressed           = false;
   isAliasCompleter         = false;
   isCodeCompletionActive   = true;
   isCompletionTipsActive   = true;
   isInDrag                 = false;
   dragStartPosition        = QPoint();
   clickPos                 = QPoint();
   iClicks                  = 0;
   mouseMode                = mouseMode_none;
   m_currentBlockNumber     = -1;
   m_braceHiliteColor       = QColor( Qt::yellow ).lighter( 160 );

   connect( this, SIGNAL( blockCountChanged( int ) )           , this, SLOT( hbUpdateLineNumberAreaWidth( int ) ) );
   connect( this, SIGNAL( updateRequest( const QRect &, int ) ), this, SLOT( hbUpdateLineNumberArea( const QRect &, int ) ) );

   hbUpdateLineNumberAreaWidth( 0 );

   connect( this, SIGNAL( cursorPositionChanged() )            , this, SLOT( hbSlotCursorPositionChanged() ) );
   connect( this, SIGNAL( updateRequest( const QRect &, int ) ), this, SLOT( hbUpdateHorzRuler( const QRect &, int ) ) );

   horzRuler->setFrameShape( QFrame::Panel );
   horzRuler->setFrameShadow( QFrame::Sunken );

   QPalette pl( QPlainTextEdit::palette() );
   m_selectionColor = pl.color( QPalette::Highlight );

   setContentsMargins( 0,0,0,0 );

   QTextDocument * doc = document();
   doc->setDocumentMargin( 0 );

   highlighter = NULL;
   block = NULL;

   setAcceptDrops( true );
}

/*----------------------------------------------------------------------*/

HBQPlainTextEdit::~HBQPlainTextEdit()
{
   #if 0
   if( timer )
      timer->stop();
   #endif

   delete lineNumberArea;
   delete horzRuler;

   if( block )
      hb_itemRelease( block );
}

/*----------------------------------------------------------------------*/

int HBQPlainTextEdit::hbFirstVisibleColumn()
{
   return ( horizontalScrollBar()->value() / fontMetrics().averageCharWidth() );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbGetViewportInfo()
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21017 );
      PHB_ITEM p2 = hb_itemNew( NULL );

      hb_arrayNew( p2, 6 );

      int    t = firstVisibleBlock().blockNumber();
      int    c = hbFirstVisibleColumn();
      int rows = viewport()->height() / fontMetrics().height();
      int cols = viewport()->width()  / fontMetrics().averageCharWidth();

      hb_arraySetNI( p2, 1, t    );
      hb_arraySetNI( p2, 2, c    );
      hb_arraySetNI( p2, 3, rows );
      hb_arraySetNI( p2, 4, cols );
      hb_arraySetNI( p2, 5, textCursor().blockNumber() );
      hb_arraySetNI( p2, 6, textCursor().columnNumber() );

      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbShowPrototype( const QString & tip, int rows, int cols )
{
   Q_UNUSED( rows );
   Q_UNUSED( cols );

   if( tip == ( QString ) "" )
   {
      QToolTip::hideText();
      return;
   }
   QToolTip::showText( viewport()->mapToGlobal( QPoint( cursorRect().x(), cursorRect().y() ) ), tip );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetEventBlock( PHB_ITEM pBlock )
{
   if( pBlock )
   {
      block = hb_itemNew( pBlock );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbApplyKey( int key, Qt::KeyboardModifiers modifiers, const QString & txt )
{
   QKeyEvent * ev = new QKeyEvent( QEvent::KeyPress, key, modifiers, txt );
   QPlainTextEdit::keyPressEvent( ev );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbRefresh()
{
   repaint();
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::event( QEvent *event )
{
   if( event->type() == QEvent::ToolTip )
   {
      event->ignore();
      #if 0
      QHelpEvent * helpEvent = static_cast<QHelpEvent *>( event );

      if( helpEvent && isTipActive )
      {
         event->ignore();
      }
      #endif
      return false;//true;
   }
   return QPlainTextEdit::event( event );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbHighlightArea( int top, int left, int bottom, int right, int mode )
{
   HB_SYMBOL_UNUSED( mode );

   highlight.setTop( top );
   highlight.setLeft( left );
   highlight.setBottom( bottom );
   highlight.setRight( right );

   repaint();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetSelectionColor( const QColor & color )
{
   m_selectionColor = color;

   QPalette pl( QPlainTextEdit::palette() );
   pl.setColor( QPalette::Highlight, m_selectionColor );
   pl.setColor( QPalette::HighlightedText, QColor( 0,0,0 ) );
   setPalette( pl );
}

/*----------------------------------------------------------------------*/

static bool isNavableKey( int k )
{
   return ( k == Qt::Key_Right || k == Qt::Key_Left || k == Qt::Key_Up     || k == Qt::Key_Down     ||
            k == Qt::Key_Home  || k == Qt::Key_End  || k == Qt::Key_PageUp || k == Qt::Key_PageDown );
}

/*----------------------------------------------------------------------*/
/*                          Selection Manipulation                      */
/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbTogglePersistentSelection()
{
   isSelectionPersistent = ! isSelectionPersistent;
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::isCursorInSelection()
{
   int cb = columnBegins <= columnEnds ? columnBegins : columnEnds;
   int ce = columnBegins <= columnEnds ? columnEnds   : columnBegins;
   int rb = rowBegins    <= rowEnds    ? rowBegins    : rowEnds;
   int re = rowBegins    <= rowEnds    ? rowEnds      : rowBegins;

   QTextCursor c = textCursor();
   int col = c.columnNumber();
   int row = c.blockNumber();

   if( selectionMode == selectionMode_column )
   {
      HB_TRACE( HB_TR_DEBUG, ( "isCursorInSelection( Modif %i %i %i %i RC %i %i Cur %i %i", rb, cb, re, ce, rowBegins, columnBegins, row, col ) );
      return( col >= cb && col <= ce && row >= rb && row <= re );
   }
   else
   {
      if( row == rb )
      {
         return( col >= cb );
      }
      else if( row == re )
      {
         return( col <= ce );
      }
      else if( row >= rb && row <= re )
      {
         return( true );
      }
   }
   return( false );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbPostSelectionInfo()
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21000 );
      PHB_ITEM p2 = hb_itemNew( NULL );

      hb_arrayNew( p2, 7 );

      hb_arraySetNI( p2, 1, rowBegins      );
      hb_arraySetNI( p2, 2, columnBegins   );
      hb_arraySetNI( p2, 3, rowEnds        );
      hb_arraySetNI( p2, 4, columnEnds     );
      hb_arraySetNI( p2, 5, selectionMode  );
      hb_arraySetNI( p2, 6, 0              );
      hb_arraySetNI( p2, 7, 0              );

      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
   emit selectionChanged();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbClearSelection()
{
   rowBegins    = -1;
   rowEnds      = -1;
   columnBegins = -1;
   columnEnds   = -1;
   hbPostSelectionInfo();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSelectAll()
{
   rowBegins    = 0;
   rowEnds      = document()->blockCount();
   columnBegins = 0;
   columnEnds   = 0;
   hbPostSelectionInfo();
   repaint();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetSelectionInfo( PHB_ITEM selectionInfo )
{
   rowBegins     = hb_arrayGetNI( selectionInfo, 1 );
   columnBegins  = hb_arrayGetNI( selectionInfo, 2 );
   rowEnds       = hb_arrayGetNI( selectionInfo, 3 );
   columnEnds    = hb_arrayGetNI( selectionInfo, 4 );
   selectionMode = hb_arrayGetNI( selectionInfo, 5 );
   hbPostSelectionInfo();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetSelectionMode( int mode, bool byApplication )
{
   if( byApplication )
   {
      if( mode == 0 )
      {
         isSelectionByApplication = false;
         isStreamSelectionON      = false;
         isColumnSelectionON      = false;
         isLineSelectionON        = false;
         hbClearSelection();
         repaint();
         return;
      }

      isSelectionByApplication = ! isSelectionByApplication;

      if( ! isSelectionByApplication )
      {
         isStreamSelectionON = false;
         isColumnSelectionON = false;
         isLineSelectionON   = false;

         if( mode == selectionMode_column )
         {
            QTextCursor c( textCursor() );
            c.movePosition( QTextCursor::EndOfLine );
            if( c.columnNumber() > columnEnds )
            {
               c.movePosition( QTextCursor::StartOfLine );
               c.movePosition( QTextCursor::Right, QTextCursor::MoveAnchor, columnEnds );
            }
            setTextCursor( c );
         }
      }
      else
      {
         switch( mode )
         {
            case selectionMode_stream:
            {
               selectionMode       = selectionMode_stream;
               isStreamSelectionON = true;
               isColumnSelectionON = false;
               isLineSelectionON   = false;

               QTextCursor c( textCursor() );

               rowBegins     = c.blockNumber();
               rowEnds       = rowBegins;
               columnBegins  = c.columnNumber();
               columnEnds    = columnBegins;
               break;
            }
            case selectionMode_column:
            {
               selectionMode       = selectionMode_column;
               isStreamSelectionON = false;
               isColumnSelectionON = true;
               isLineSelectionON   = false;

               QTextCursor c( textCursor() );

               rowBegins     = c.blockNumber();
               rowEnds       = rowBegins;
               columnBegins  = c.columnNumber();
               columnEnds    = columnBegins;

               break;
            }
            case selectionMode_line:
            {
               selectionMode       = selectionMode_line;
               isStreamSelectionON = false;
               isColumnSelectionON = false;
               isLineSelectionON   = true;

               QTextCursor c( textCursor() );

               rowBegins     = c.blockNumber();
               rowEnds       = rowBegins;
               columnBegins  = 0;
               columnEnds    = 0;
               break;
            }
         }
      }
   }
   else
   {
      if( ! isSelectionByApplication )
      {
         switch( mode )
         {
            case selectionMode_stream:
            {
               if( columnBegins >= 0 )
               {
                  hbToStream();
               }
               selectionMode       = selectionMode_stream;
               isColumnSelectionON = false;
               isLineSelectionON   = false;
               break;
            }
            case selectionMode_column:
            {
               selectionMode       = selectionMode_column;
               isColumnSelectionON = true;
               isLineSelectionON   = false;
               break;
            }
         }
      }
   }
   hbPostSelectionInfo();
   repaint();   /* Only once when mode is changed from stream to column , so no issues */
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbToStream()
{
   int rb = rowBegins <= rowEnds ? rowBegins : rowEnds;
   int re = rowBegins <= rowEnds ? rowEnds   : rowBegins;

   if( selectionMode == selectionMode_line )
   {
      QTextCursor c = textCursor();

      c.movePosition( QTextCursor::Start                                            );
      c.movePosition( QTextCursor::Down     , QTextCursor::MoveAnchor, rb           );
      c.movePosition( QTextCursor::Right    , QTextCursor::MoveAnchor, columnBegins );
      c.movePosition( QTextCursor::Down     , QTextCursor::MoveAnchor, re - rb      );
      c.movePosition( QTextCursor::EndOfLine, QTextCursor::MoveAnchor               );
      int cce = c.columnNumber();
      if( cce > columnEnds )
      {
         c.movePosition( QTextCursor::StartOfLine, QTextCursor::MoveAnchor             );
         c.movePosition( QTextCursor::Right      , QTextCursor::MoveAnchor, columnEnds );
      }
      else
      {
         columnEnds = cce;
      }
      columnBegins = 0; rowBegins = rb; rowEnds = re;
      setTextCursor( c );
   }
   else if( selectionMode == selectionMode_column )
   {
      QTextCursor c = textCursor();

      c.movePosition( QTextCursor::Start );
      c.movePosition( QTextCursor::Down     , QTextCursor::MoveAnchor, re );
      c.movePosition( QTextCursor::EndOfLine, QTextCursor::MoveAnchor     );
      if( c.columnNumber() > columnEnds )
      {
         c.movePosition( QTextCursor::StartOfLine, QTextCursor::MoveAnchor             );
         c.movePosition( QTextCursor::Right      , QTextCursor::MoveAnchor, columnEnds );
      }
      columnEnds = c.columnNumber(); rowBegins = rb; rowEnds = re;
      setTextCursor( c );
   }
   else if( selectionMode == selectionMode_stream )
   {
      QTextCursor c = textCursor();
      rowBegins     = c.blockNumber();
      rowEnds       = rowBegins;
      columnBegins  = c.columnNumber();
      columnEnds    = columnBegins;
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbHitTest( const QPoint & pt )
{
   QTextCursor ct = cursorForPosition( QPoint( 2,2 ) );
   int          t = ct.blockNumber();
   int          c = ct.columnNumber();

   hitTestRow    = t + ( pt.y() / fontMetrics().height() );
   hitTestColumn = c + ( pt.x() / fontMetrics().averageCharWidth() );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbCut( int k )
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21014 );
      PHB_ITEM p2 = hb_itemPutNI( NULL, k );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
   else
   {
      QPlainTextEdit::cut();
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbCopy()
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21011 );
      hb_vmEvalBlockV( block, 1, p1 );
      hb_itemRelease( p1 );
   }
   else
   {
      QPlainTextEdit::copy();
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbPaste()
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21012 );
      hb_vmEvalBlockV( block, 1, p1 );
      hb_itemRelease( p1 );

      if( ! isSelectionPersistent )
      {
         hbClearSelection();
      }
   }
   else
   {
      QPlainTextEdit::paste();
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::dropEvent( QDropEvent *event )
{
   if( event->dropAction() == Qt::CopyAction || event->dropAction() == Qt::MoveAction )
   {
      if( event->source() == this )
      {
         QPoint p( event->pos() );

         event->ignore();

         QTextCursor c = cursorForPosition( p );
         int row = c.blockNumber();
         int col = c.columnNumber();

         if( ( selectionMode == selectionMode_stream || selectionMode == selectionMode_line ) && row >= rowBegins && row <= rowEnds )
         {
            setTextCursor( c );
            mouseMode = mouseMode_select;
         }
         else if( selectionMode == selectionMode_column && row >= rowBegins && row <= rowEnds && col >= columnBegins && col <= columnEnds )
         {
            setTextCursor( c );
            mouseMode = mouseMode_select;
         }
         else
         {
            mouseMode = mouseMode_none;
            hbCopy();
            if( event->dropAction() != Qt::CopyAction )
            {
               int rBgn = rowBegins;
               int rEnd = rowEnds;
               int cBgn = columnBegins;
               int cEnd = columnEnds;
               int linesBefore = blockCount();
               hbCut( Qt::Key_Delete );
               int linesAfter = blockCount();
               QTextCursor cc( textCursor() );
               cc.movePosition( QTextCursor::Start );

               if( row == rBgn ) /* Only in case of column selection */
               {
                  cc.movePosition( QTextCursor::Down, QTextCursor::MoveAnchor, row );
                  if( col > cEnd )
                  {
                     cc.movePosition( QTextCursor::Right, QTextCursor::MoveAnchor, col - ( cEnd - cBgn ) );
                  }
                  else if( col < cBgn )
                  {
                     cc.movePosition( QTextCursor::Right, QTextCursor::MoveAnchor, col );
                  }
               }
               else
               {
                  if( rBgn > row )
                  {
                     cc.movePosition( QTextCursor::Down, QTextCursor::MoveAnchor, row );
                  }
                  else if( row > rEnd )
                  {
                     cc.movePosition( QTextCursor::Down, QTextCursor::MoveAnchor, row - ( linesBefore - linesAfter ) );
                  }
                  cc.movePosition( QTextCursor::Right, QTextCursor::MoveAnchor, col );
               }
               setTextCursor( cc );
            }
            else
            {
               setTextCursor( c );
            }
            hbClearSelection();
            hbPaste();
            hbPostSelectionInfo();
         }
         /* It is a hack. Without this editing caret is lost ??? */
         QMimeData * data = new QMimeData();
         QDropEvent * ev = new QDropEvent( p, Qt::CopyAction, data, 0, 0 );
         QPlainTextEdit::dropEvent( ev );

         return;
      }
   }
   QPlainTextEdit::dropEvent( event );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::dragMoveEvent( QDragMoveEvent *event )
{
   if( event->mimeData()->hasText() )
   {
      if( event->source() == this )
      {
         event->accept();
      }
      else
      {
         event->acceptProposedAction();
      }
   }
   else
   {
      event->ignore();
   }
   QPlainTextEdit::dragMoveEvent( event );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::dragEnterEvent( QDragEnterEvent *event )
{
   if( event->mimeData()->hasText() )
   {
      if( event->source() == this )
      {
         event->accept();
      }
      else
      {
         event->acceptProposedAction();
      }
   }
   else
   {
      event->ignore();
   }
   QPlainTextEdit::dragEnterEvent( event );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::mouseDoubleClickEvent( QMouseEvent *event )
{
   QPlainTextEdit::mouseDoubleClickEvent( event );

   QTextCursor c( textCursor() );
   if( c.hasSelection() )
   {
      rowBegins      = c.blockNumber();
      rowEnds        = rowBegins;
      columnEnds     = c.columnNumber();
      columnBegins   = columnEnds - ( c.selectionEnd() - c.selectionStart() );
      selectionMode  = selectionMode_stream;
      mouseMode      = mouseMode_select;
      c.clearSelection();
      setTextCursor( c );
      hbPostSelectionInfo();
      clickPos = event->pos();
      iClicks = 2;
      repaint();
   }
   /* Required because few actions are bound by it */
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, QEvent::MouseButtonDblClick );
      PHB_ITEM p2 = hb_itemPutNI( NULL, event->globalX() );
      PHB_ITEM p3 = hb_itemPutNI( NULL, event->globalY() );
      hb_vmEvalBlockV( block, 3, p1, p2, p3 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::mouseReleaseEvent( QMouseEvent *event )
{
   if( isSelectionByApplication )
   {
      if( isLineSelectionON )
      {
         QPlainTextEdit::mouseReleaseEvent( event );
         rowEnds = textCursor().blockNumber();
      }
      else if( isColumnSelectionON )
      {
         event->accept();
         hbHitTest( event->pos() );
         rowEnds    = hitTestRow;
         columnEnds = hitTestColumn;
      }
      else if( isStreamSelectionON )
      {
         QPlainTextEdit::mouseReleaseEvent( event );
         rowEnds    = textCursor().blockNumber();
         columnEnds = textCursor().columnNumber();
      }
      repaint();
   }
   else
   {
      QPlainTextEdit::mouseReleaseEvent( event );
   }
#if 0
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, QEvent::MouseButtonRelease );
      PHB_ITEM p2 = hb_itemPutNI( NULL, event->globalX() );
      PHB_ITEM p3 = hb_itemPutNI( NULL, event->globalY() );
      hb_vmEvalBlockV( block, 3, p1, p2, p3 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );
   }
#endif
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::mousePressEvent( QMouseEvent *event )
{
   if( isSelectionByApplication )
   {
      if( isColumnSelectionON )
      {
         event->accept();
      }
      else
      {
         QPlainTextEdit::mousePressEvent( event );
      }
      return;
   }
   else if( event->modifiers() & Qt::ShiftModifier )
   {
      QTextCursor c( textCursor() );
      rowBegins    = c.blockNumber();
      columnBegins = c.columnNumber();

      QPlainTextEdit::mousePressEvent( event );

      c = textCursor();
      rowEnds    = c.blockNumber();
      columnEnds = c.columnNumber();
      c.clearSelection();
      setTextCursor( c );
      selectionMode = selectionMode_stream;
      hbPostSelectionInfo();
      repaint();
   }
   else
   {
      if( event->buttons() & Qt::LeftButton )
      {
         QTextCursor c( cursorForPosition( event->pos() ) );

         if( iClicks == 2 )      /* Handle Tripple-click */
         {
            iClicks = 0;
            if( ( event->pos() - clickPos ).manhattanLength() < QApplication::startDragDistance() )
            {
               selectionMode = selectionMode_stream;
               c.movePosition( QTextCursor::EndOfLine );
               columnBegins = 0;
               columnEnds = c.columnNumber();
               hbPostSelectionInfo();
               setTextCursor( c );
               event->accept();
               repaint();
            }
            else
            {
               QPlainTextEdit::mousePressEvent( event );
               hbClearSelection();
            }
         }
         else
         {
            QPlainTextEdit::mousePressEvent( event );
#if 0
            if( block )
            {
               PHB_ITEM p1 = hb_itemPutNI( NULL, QEvent::MouseButtonPress );
               PHB_ITEM p2 = hb_itemPutNI( NULL, event->globalX() );
               PHB_ITEM p3 = hb_itemPutNI( NULL, event->globalY() );
               hb_vmEvalBlockV( block, 3, p1, p2, p3 );
               hb_itemRelease( p1 );
               hb_itemRelease( p2 );
               hb_itemRelease( p3 );
            }
#endif
            dragStartPosition = event->pos();
            if( mouseMode == mouseMode_select && isCursorInSelection() )
            {
               mouseMode = mouseMode_drag;
            }
            else
            {
               mouseMode = mouseMode_none;
               hbClearSelection();
               repaint();
            }
         }
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::mouseMoveEvent( QMouseEvent *event )
{
   if( isSelectionByApplication )
   {
      event->accept();
      return;
   }

   if( selectionMode == selectionMode_line )
   {
      selectionMode = selectionMode_stream;
      hbPostSelectionInfo();
   }
   if( event->buttons() & Qt::LeftButton )
   {
      if( mouseMode == mouseMode_drag && ( event->pos() - dragStartPosition ).manhattanLength() < QApplication::startDragDistance() )
      {
         QTextCursor c( cursorForPosition( event->pos() ) );
         int row = c.blockNumber();
         if( row >= rowBegins && row <= rowEnds )
         {
            event->ignore();

            QDrag * qDrag = new QDrag( this );
            QMimeData * qMimeData = new QMimeData();
            hbCopy();
            qMimeData->setText( QApplication::clipboard()->text() );
            qDrag->setMimeData( qMimeData );

            QPixmap pmap = QPixmap::grabWidget( this->viewport(), hbGetSelectionRect() );
            pmap.setMask( pmap.createMaskFromColor( m_selectionColor, Qt::MaskInColor ) );
            pmap.setMask( pmap.createMaskFromColor( palette().color( QPalette::Base ), Qt::MaskInColor ) );
            pmap.setMask( pmap.createMaskFromColor( m_currentLineColor, Qt::MaskInColor ) );
            qDrag->setPixmap( pmap );
            qDrag->setHotSpot( QPoint( 5,5 ) );

            qDrag->exec( Qt::MoveAction | Qt::CopyAction );
            delete qDrag;
            return;
         }
      }

      if( columnBegins == -1 )
      {
         QTextCursor c( textCursor() );
         rowBegins    = c.blockNumber();
         columnBegins = c.columnNumber();
         rowEnds      = rowBegins;
         columnEnds   = columnBegins;
         mouseMode    = mouseMode_select;
         QPlainTextEdit::mouseMoveEvent( event );
      }
      else if( mouseMode == mouseMode_select )
      {
         extraSelections.clear();
         setExtraSelections( extraSelections );

         if( selectionMode == selectionMode_column )
         {
            QTextCursor c( cursorForPosition( QPoint( 1,1 ) ) );
            rowEnds    = c.blockNumber()  + ( event->y() / fontMetrics().height() );
            columnEnds = c.columnNumber() + ( event->x() / fontMetrics().averageCharWidth() );
         }
         QPlainTextEdit::mouseMoveEvent( event );
         QTextCursor c = textCursor();
         if( selectionMode != selectionMode_column )
         {
            rowEnds    = c.blockNumber();
            columnEnds = c.columnNumber();
         }
         c.clearSelection();
         setTextCursor( c );
      }
      hbPostSelectionInfo();
   }
#if 0
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, QEvent::MouseMove );
      PHB_ITEM p2 = hb_itemPutNI( NULL, event->globalX() );
      PHB_ITEM p3 = hb_itemPutNI( NULL, event->globalY() );
      hb_vmEvalBlockV( block, 3, p1, p2, p3 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
      hb_itemRelease( p3 );
   }
#endif
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::keyReleaseEvent( QKeyEvent * event )
{
   QPlainTextEdit::keyReleaseEvent( event );

   if( ( event->modifiers() & Qt::ControlModifier ) && event->text() == "" )
   {
      hbPostSelectionInfo();
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbHandleKey( QKeyEvent * event, int k, int selMode, bool shift )
{
   Q_UNUSED( selMode );
   Q_UNUSED( shift );

   switch( k )
   {
      case Qt::Key_Right:
      {
         event->ignore();
         QTextCursor c( textCursor() );
         QTextCursor cc( textCursor() );
         cc.movePosition( QTextCursor::EndOfLine );
         if( columnEnds < cc.columnNumber() )
         {
            c.movePosition( QTextCursor::Right );
            setTextCursor( c );
         }
         columnEnds++;
         break;
      }
      case Qt::Key_Left:
      {
         event->ignore();
         QTextCursor c( textCursor() );
         if( columnEnds >= 0 )
         {
            if( columnEnds <= c.columnNumber() )
            {
               c.movePosition( QTextCursor::Left );
               setTextCursor( c );
            }
            columnEnds--;
         }
         break;
      }
      case Qt::Key_Home:
      case Qt::Key_End:
      {
         QPlainTextEdit::keyPressEvent( event );
         columnEnds = textCursor().columnNumber();
         break;
      }
      case Qt::Key_Up:
      case Qt::Key_PageUp:
      case Qt::Key_Down:
      case Qt::Key_PageDown:
      {
         QPlainTextEdit::keyPressEvent( event );
         rowEnds = textCursor().blockNumber();
         break;
      }
   }
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::hbKeyPressSelection( QKeyEvent * event )
{
   int k;
   bool ctrl, shift, isNavable;

   if( isSelectionByApplication )
   {
      return hbKeyPressSelectionByApplication( event );
   }
   k = event->key();
   ctrl  = event->modifiers() & Qt::ControlModifier;
   shift = event->modifiers() & Qt::ShiftModifier;
   isNavable = isNavableKey( k );

   if( ctrl && shift && ! isNavable )
   {
      return false;
   }
   if( ctrl && event->text().isEmpty() && ! isNavable )
   {
      return false;
   }
   if( ctrl && ( k == Qt::Key_C || k == Qt::Key_V || k == Qt::Key_X ||
                 k == Qt::Key_A || k == Qt::Key_Z || k == Qt::Key_Y ) )
   {
      event->ignore();
      return true;
   }

   bool bClear = false;

   if( shift && isNavable )
   {
      if( selectionMode == selectionMode_line )
      {
         selectionMode = selectionMode_stream;
         hbPostSelectionInfo();
      }

      isShiftPressed = true;

      event->accept();
      QTextCursor c( textCursor() );
      c.clearSelection();
      setTextCursor( c );

      if( columnBegins == -1 || columnEnds == -1 || rowBegins == -1 || rowEnds == -1 )
      {
         rowBegins      = c.blockNumber();
         columnBegins   = c.columnNumber();
         rowEnds        = rowBegins;
         columnEnds     = columnBegins;
         hbPostSelectionInfo();
      }

      /* Push key back to system without the shift modifier - it will position position the cursor as intended */
      QKeyEvent * ev = new QKeyEvent( event->type(), event->key(), ctrl ? Qt::ControlModifier : Qt::NoModifier, event->text() );
      keyPressEvent( ev );
      return true;
   }

   if( isShiftPressed && isNavable )
   {
      isShiftPressed = false;

      if( selectionMode == selectionMode_stream )
      {
         QPlainTextEdit::keyPressEvent( event );
         rowEnds    = textCursor().blockNumber();
         columnEnds = textCursor().columnNumber();
      }
      else if( selectionMode == selectionMode_column )
      {
         hbHandleKey( event, k, selectionMode_column, true );
      }
      hbPostSelectionInfo();
      repaint();                     /* A Must Here , otherwise selection will not be reflected */
      return true;
   }
   else if( ctrl && isNavable && selectionMode == selectionMode_column && columnBegins >= 0 && columnBegins == columnEnds )
   {
      hbHandleKey( event, k, selectionMode_column, false );
      columnBegins = columnEnds;
      hbPostSelectionInfo();
      repaint();
      return true;
   }
   else if( ! ctrl && k >= ' ' && k < 127 && columnBegins >= 0 && selectionMode == selectionMode_column )
   {
      if( block )
      {
         PHB_ITEM p1 = hb_itemPutNI( NULL, 21013 );
         PHB_ITEM p2 = hbqt_bindGetHbObject( NULL, ( void * ) event, "HB_QKEYEVENT", NULL, 0 ) ;
         hb_vmEvalBlockV( block, 2, p1, p2 );
         hb_itemRelease( p1 );
         hb_itemRelease( p2 );

         if( columnBegins == columnEnds )
         {
            columnBegins++;
            columnEnds++;
            hbPostSelectionInfo();
         }
         event->accept();
         repaint();
         return true;
      }
      else
      {
         bClear = true;
      }
   }
   else if( ! ctrl && ( k == Qt::Key_Backspace || k == Qt::Key_Delete ) && columnBegins >= 0 )
   {
      if( selectionMode == selectionMode_column )
      {
         hbCut( k );
         if( k == Qt::Key_Backspace )
         {
            columnBegins--;
            columnEnds--;
         }
         else
         {
            columnEnds = columnBegins;
         }
         event->accept();
         hbPostSelectionInfo();
         repaint();
         return true;
      }
      else   /* selectionMode == selectionMode_stream || selectionMode == selectionMode_line */
      {
         hbCut( Qt::Key_Delete );
         repaint();
         hbPostSelectionInfo();
         if( k == Qt::Key_Delete )
         {
            event->accept();
            return true;
         }
      }
   }
   else if( ! ctrl && k >= ' ' && k < 127 && columnBegins >= 0 && selectionMode == selectionMode_stream )
   {
      hbCut( Qt::Key_Delete );
      hbClearSelection();
   }
   else if( ! ctrl && k >= ' ' && k < 127 )
   {
      bClear = true;
   }
   else if( isNavable )
   {
      bClear = true;
   }

   if( bClear )
   {
      if( isSelectionPersistent )
      {
         if( columnBegins >= 0 )
         {
            if( columnEnds == columnBegins )
            {
               hbClearSelection();
            }
            hbPostSelectionInfo();
         }
      }
      else
      {
         if( columnBegins >= 0 )
         {
            hbClearSelection();
            hbPostSelectionInfo();
            repaint();
         }
      }
   }
   return false;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::keyPressEvent( QKeyEvent * event )
{
   extraSelections.clear();
   setExtraSelections( extraSelections );

   if( hbHandlePopup( event ) )
   {
      return;
   }
   if( hbKeyPressSelection( event ) )
   {
      return;
   }

   QPlainTextEdit::keyPressEvent( event );

   if( ! isCodeCompletionActive )
   {
      if( c ){
         c->popup()->hide();
      }
      return;
   }

   if( ! c )
   {
      return;
   }
   if( isTipActive )
   {
      c->popup()->hide();
      return;
   }

   if( ! isAliasCompleter )
   {
      hbRefreshCompleter( hbTextAlias() );
   }

   if( ( event->modifiers() & ( Qt::ControlModifier | Qt::AltModifier ) ) )
   {
      c->popup()->hide();
      return;
   }
   const bool ctrlOrShift = event->modifiers() & ( Qt::ControlModifier | Qt::ShiftModifier );
   if( ctrlOrShift && event->text().isEmpty() )
   {
      return;
   }
   static  QString            eow( " ~!@#$%^&*()+{}|:\"<>?,./;'[]\\-=" );               /* end of word */
   bool    hasModifier      = ( event->modifiers() != Qt::NoModifier ) && !ctrlOrShift;
   QString completionPrefix = hbTextUnderCursor( true );
   /*QString completionPrefix = hbTextUnderCursor( false );*/

   if( hasModifier ||
         event->text().isEmpty() ||
            completionPrefix.length() < ( isAliasCompleter ? 0 : 1 ) ||
                eow.contains( event->text().right( 1 ) ) )
   {
      c->popup()->hide();
      return;
   }

   if( completionPrefix != c->completionPrefix() )
   {
      c->setCompletionPrefix( completionPrefix );
      c->popup()->setCurrentIndex( c->completionModel()->index( 0, 0 ) );
   }
   QRect cr = cursorRect();

   c->popup()->setMaximumWidth( viewport()->width() );
   cr.setWidth( c->popup()->sizeHintForColumn( 0 ) + c->popup()->verticalScrollBar()->sizeHint().width() );
   cr.setTop( cr.top() + horzRulerHeight + 5 );
   cr.setBottom( cr.bottom() + horzRulerHeight + 5 );

   c->complete( cr ); /* pop it up! */
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::hbKeyPressSelectionByApplication( QKeyEvent * event )
{
   bool shift = event->modifiers() & Qt::ShiftModifier;
   int k = event->key();

   if( isNavableKey( k ) && shift )
   {
      event->accept();
      QTextCursor c( textCursor() );
      c.clearSelection();
      setTextCursor( c );
      QKeyEvent * ev = new QKeyEvent( event->type(), event->key(), Qt::NoModifier, event->text() );
      keyPressEvent( ev );
      return true;
   }

   if( isNavableKey( k ) )
   {
      if( selectionMode == selectionMode_stream )
      {
         QPlainTextEdit::keyPressEvent( event );

         QTextCursor c( textCursor() );
         rowEnds    = c.blockNumber();
         columnEnds = c.columnNumber();
      }
      else if( selectionMode == selectionMode_column )
      {
         switch( k )
         {
         case Qt::Key_Right:
         {
            QTextCursor c( textCursor() );
            c.movePosition( QTextCursor::EndOfLine );
            if( c.columnNumber() <= columnEnds )
            {
               setTextCursor( c );
            }
            event->ignore();
            columnEnds++;
            break;
         }
         case Qt::Key_Left:
         {
            QTextCursor c( textCursor() );
            int col = c.columnNumber();
            if( col < columnEnds - 1 )
            {
               c.movePosition( QTextCursor::Left );
               columnEnds--;
            }
            else if( columnEnds - 1 >= 0 )
            {
               columnEnds--;
            }
            event->ignore();
            break;
         }
         case Qt::Key_Home:
         case Qt::Key_End:
         {
            QPlainTextEdit::keyPressEvent( event );
            columnEnds = textCursor().columnNumber();
            break;
         }
         case Qt::Key_Up:
         case Qt::Key_PageUp:
         case Qt::Key_Down:
         case Qt::Key_PageDown:
            QPlainTextEdit::keyPressEvent( event );
            rowEnds = textCursor().blockNumber();
            break;
         default:
            event->ignore();
            break;
         }
      }
      else if( selectionMode == selectionMode_line )
      {
         QPlainTextEdit::keyPressEvent( event );
         QTextCursor c( textCursor() );
         rowEnds = c.blockNumber();
      }
//    repaint();
//    update();
   }
   else
   {
      event->ignore();
   }
   return true;
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::hbHandlePopup( QKeyEvent * event )
{
   if( c && c->popup()->isVisible() )
   {
      /* The following keys are forwarded by the completer to the widget */
      switch( event->key() )
      {
         case Qt::Key_Enter   :
         case Qt::Key_Return  :
         case Qt::Key_Escape  :
         case Qt::Key_Tab     :
         case Qt::Key_Backtab :
         {
            event->ignore();
            return true;
         }
      }
   }
   return false;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbRefreshCompleter( const QString & alias )
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21041 );
      PHB_ITEM p2 = hb_itemPutC( NULL, alias.toLatin1().data() );
      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
}

/*------------------------------------------------------------------------*/

QString HBQPlainTextEdit::hbTextUnderCursor( bool bCodeComplete )
{
   QTextCursor tc( textCursor() );
   if( bCodeComplete )
   {
      tc.movePosition( QTextCursor::PreviousCharacter, QTextCursor::KeepAnchor, 1 );
      QString txt = tc.selectedText();
      tc.clearSelection();
      if( txt == ( QString ) ' ' )
      {
         tc.select( QTextCursor::WordUnderCursor );
         txt = tc.selectedText() + ' ';
         return txt;
      }
      else
      {
         tc = textCursor();
         tc.select( QTextCursor::WordUnderCursor );
         return tc.selectedText();
      }
   }
   else
   {
      tc.select( QTextCursor::WordUnderCursor );
   }
   return tc.selectedText();
}

/*----------------------------------------------------------------------*/

QString HBQPlainTextEdit::hbTextAlias()
{
   QTextCursor tc( textCursor() );

   tc.movePosition( QTextCursor::PreviousCharacter, QTextCursor::KeepAnchor, 2 );
   QString txt = tc.selectedText();
   tc.clearSelection();
   if( txt == ( QString ) "->" )
   {
      tc.movePosition( QTextCursor::PreviousCharacter, QTextCursor::KeepAnchor, 1 );
      tc.select( QTextCursor::WordUnderCursor );
      txt = tc.selectedText();
      return txt;
   }
   return "";
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::resizeEvent( QResizeEvent *e )
{
   setContentsMargins( 0,0,0,0 );
   viewport()->setContentsMargins( 0,0,0,0 );

   QPlainTextEdit::resizeEvent( e );

   QRect cr = contentsRect();
   lineNumberArea->setGeometry( QRect( cr.left(), cr.top() + horzRulerHeight, hbLineNumberAreaWidth(), cr.height() ) );

   horzRuler->setGeometry( QRect( cr.left(), cr.top(), cr.width(), horzRulerHeight ) );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::focusInEvent( QFocusEvent * event )
{
   if( c )
      c->setWidget( this );

   QPlainTextEdit::focusInEvent( event );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::paintEvent( QPaintEvent * event )
{
   QPainter painter( viewport() );

   QTextBlock tblock = firstVisibleBlock();
   int blockNumber   = tblock.blockNumber();
   int height        = ( int ) blockBoundingRect( tblock ).height();
   int top           = ( int ) blockBoundingGeometry( tblock ).translated( contentOffset() ).top();
   int bottom        = top + height;
   int curBlock;
   if( textCursor().isNull() )
   {
      curBlock = blockNumber;
   }
   else
   {
      curBlock = textCursor().blockNumber();
   }
   int rTop    = event->rect().top();
   int rBottom = event->rect().bottom();
   int width   = viewport()->width();

   while( tblock.isValid() && top <= rBottom )
   {
      if( tblock.isVisible() && bottom >= rTop )
      {
         int index = bookMarksGoto.indexOf( blockNumber + 1 );
         if( index != -1 )
         {
            QRect r( 0, top, width, height );
            painter.fillRect( r, brushForBookmark( index ) );
         }
         else if( curBlock == blockNumber && m_currentLineColor.isValid() )
         {
            if( highlightCurLine == true )
            {
               QRect r = HBQPlainTextEdit::cursorRect();
               r.setX( 0 );
               r.setWidth( width );
               painter.fillRect( r, QBrush( m_currentLineColor ) );
            }
         }
      }
      tblock = tblock.next();
      top    = bottom;
      bottom = top + height;
      ++blockNumber;
   }
   painter.end();

   this->hbPaintHighlight( event );
   this->hbPaintSelection( event );

   QPlainTextEdit::paintEvent( event );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbPaintHighlight( QPaintEvent * event )
{
   HB_SYMBOL_UNUSED( event );

   if( highlight.top() > -1 )
   {
      int fontHeight = fontMetrics().height();
      int          t = firstVisibleBlock().blockNumber();
      int          b = t + ( viewport()->height() / fontHeight ) + 1;
      int         rb = highlight.top();
      int         re = highlight.bottom();

      if( re >= t && rb < b )
      {
         QPainter p( viewport() );

         int    top = ( ( rb <= t ) ? 0 : ( ( rb - t ) * fontHeight ) );
         int    btm = ( ( re - t + 1 ) * fontHeight ) - top;

         btm = btm > viewport()->height() ? viewport()->height() : btm;

         QRect r( 0, top, viewport()->width(), btm );
         p.fillRect( r, QBrush( QColor( 255,255,0 ) ) );

         p.end();
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbPaintSelection( QPaintEvent * event )
{
   HB_SYMBOL_UNUSED( event );

   if( rowBegins >= 0 && rowEnds >= 0 )
   {
      int cb = columnBegins <= columnEnds ? columnBegins : columnEnds;
      int ce = columnBegins <= columnEnds ? columnEnds   : columnBegins;
      int rb = rowBegins    <= rowEnds    ? rowBegins    : rowEnds;
      int re = rowBegins    <= rowEnds    ? rowEnds      : rowBegins;

      int       ttop = ( int ) blockBoundingGeometry( firstVisibleBlock() ).translated( contentOffset() ).top();

      int          t = firstVisibleBlock().blockNumber();
      int          c = hbFirstVisibleColumn();
      int fontHeight = fontMetrics().height();
      int          b = t + ( ( viewport()->height() - ttop ) / fontHeight ) + 1;

      re = re > b ? b : re;

      if( re >= t && rb < b )
      {
         QPainter p( viewport() );

         int fontWidth = fontMetrics().averageCharWidth();

         int top = ( ( rb <= t ) ? 0 : ( ( rb - t ) * fontHeight ) ) + ttop;
         int btm = ( ( re - t + 1 ) * fontHeight ) - top + ttop;
         btm = btm > viewport()->height() ? viewport()->height() : btm;
         QBrush br( m_selectionColor );

         switch( selectionMode )
         {
            case selectionMode_column:
            {
               int x = ( ( cb - c ) * fontWidth );
               int w = ( cb == ce ? 1 : ( ( ce - cb ) * fontWidth ) );
               QRect r( x, top, w, btm );
               p.fillRect( r, br );
            }
            break;
            case selectionMode_line:
            {
               QRect r( 0, top, viewport()->width(), btm );
               p.fillRect( r, QBrush( m_selectionColor ) );
            }
            break;
            case selectionMode_stream:
            {
               int width = viewport()->width();
               int   i;
               QRect r;
               for( i = ( rb >= t ? rb : t ); i <= re; i++ )
               {
                  if( rowBegins > rowEnds )
                  {
                     if( i == rowEnds )
                     {
                        if( rb == re )
                        {
                           int x = ( ( cb - c ) * fontWidth );
                           int w = ( ce - cb ) * fontWidth ;
                           r = QRect( x, top, w, fontHeight );
                        }
                        else
                        {
                           int x = ( ( columnEnds - c ) * fontWidth );
                           r = QRect( x, top, width + abs( x ), fontHeight );
                        }
                     }
                     else if( i == rowBegins )
                     {
                        int x = ( ( columnBegins - c ) * fontWidth );
                        r = QRect( 0, top, x, fontHeight );
                     }
                     else
                     {
                        r = QRect( 0, top, width, fontHeight );
                     }
                  }
                  else
                  {
                     if( i == rowBegins )
                     {
                        if( rb == re )
                        {
                           int x = ( ( cb - c ) * fontWidth );
                           int w = ( ce - cb ) * fontWidth;
                           r = QRect( x, top, w, fontHeight );
                        }
                        else
                        {
                           int x = ( ( columnBegins - c ) * fontWidth );
                           r = QRect( x, top, width + abs( x ), fontHeight );
                        }
                     }
                     else if( i == rowEnds )
                     {
                        int x = ( ( columnEnds - c ) * fontWidth );
                        r = QRect( 0, top, x, fontHeight );
                     }
                     else
                     {
                        r = QRect( 0, top, width, fontHeight );
                     }
                  }
                  p.fillRect( r, br );
                  top += fontHeight;
               }
            }
            break;
         }
         p.end();
      }
   }
}

/*----------------------------------------------------------------------*/

QRect HBQPlainTextEdit::hbGetSelectionRect()
{
   QRect r = QRect();

   if( rowBegins >= 0 && rowEnds >= 0 )
   {
      int rb = rowBegins    <= rowEnds    ? rowBegins    : rowEnds;
      int re = rowBegins    <= rowEnds    ? rowEnds      : rowBegins;

      int       ttop = ( int ) blockBoundingGeometry( firstVisibleBlock() ).translated( contentOffset() ).top();

      int          t = firstVisibleBlock().blockNumber();
      int fontHeight = fontMetrics().height();
      int          b = t + ( ( viewport()->height() - ttop ) / fontHeight ) + 1;

      re = re > b ? b : re;

      if( re >= t && rb < b )
      {
         int top = ( ( rb <= t ) ? 0 : ( ( rb - t ) * fontHeight ) ) + ttop;
         int btm = ( ( re - t + 1 ) * fontHeight ) - top + ttop;
         btm = btm > viewport()->height() ? viewport()->height() : btm;

         if( selectionMode == selectionMode_column )
         {
            int cb = columnBegins <= columnEnds ? columnBegins : columnEnds;
            int ce = columnBegins <= columnEnds ? columnEnds   : columnBegins;
            int c  = hbFirstVisibleColumn();
            int fontWidth = fontMetrics().averageCharWidth();
            int x  = ( ( cb - c ) * fontWidth );
            int w  = ( cb == ce ? 1 : ( ( ce - cb ) * fontWidth ) );
            r = QRect( x, top, w, btm );
         }
         else
         {
            r = QRect( 0, top, viewport()->width(), btm );
         }
      }
   }
   return r;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbDrawCursor( QPaintEvent *event )
{
   HB_SYMBOL_UNUSED( event );

   QAbstractTextDocumentLayout::PaintContext pc = getPaintContext();
   {
      if( caretState == 1 )
      {
         QRect r( cursorRect( textCursor() ) );
         r.setLeft( r.left() + 100 );
         r.setRight( r.right() + 100 );

         QPainter p( viewport() );
         p.fillRect( r, QBrush( QColor( caretState == 1 ? Qt::red : Qt::blue ) ) );
         p.end();
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUpdateCaret()
{
   caretState = caretState == 0 ? 1 : 0;

   QRect r( cursorRect( textCursor() ) );
   r.setX( 0 );
   r.setWidth( viewport()->width() );
   repaint( r );
}

/*----------------------------------------------------------------------*/

int HBQPlainTextEdit::lastVisibleBlockNumber()
{
   QTextBlock block = firstVisibleBlock();
   int blockNumber  = block.blockNumber();
   int top          = ( int ) blockBoundingGeometry( block ).translated( contentOffset() ).top();
   int height       = ( int ) blockBoundingRect( block ).height();
   int vpHeight     = viewport()->height();

   while( block.isValid() && top < vpHeight )
   {
      top += height;
      ++blockNumber;
      block = block.next();
   }
   return blockNumber;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::horzRulerPaintEvent( QPaintEvent *event )
{
   int   fontWidth = fontMetrics().averageCharWidth();
   QRect        cr = event->rect();
   QPainter painter( horzRuler );

   painter.fillRect( cr, m_horzRulerBkColor );
   painter.setPen( Qt::gray );
   painter.drawLine( cr.left(), cr.bottom(), cr.width(), cr.bottom() );
   painter.setPen( Qt::black );

   int left = cr.left() + ( fontWidth / 2 ) + ( lineNumberArea->isVisible() ? lineNumberArea->width() : 0 );

   QRect rc( cursorRect( textCursor() ) );
   QTextCursor cursor( cursorForPosition( QPoint( 1, rc.top() + 1 ) ) );

   int i;
   for( i = hbFirstVisibleColumn(); left < cr.width(); i++ )
   {
      if( i % 10 == 0 )
      {
         painter.drawLine( left, cr.bottom()-3, left, cr.bottom()-5 );
         QString number = QString::number( i );
         painter.drawText( left - fontWidth, cr.top()-2, fontWidth * 2, 17, Qt::AlignCenter, number );
      }
      else if( i % 5 == 0 )
      {
         painter.drawLine( left, cr.bottom()-3, left, cr.bottom()-5 );
      }
      else
      {
         painter.drawLine( left, cr.bottom()-3, left, cr.bottom()-4 );
      }
      if( i == textCursor().columnNumber() )
      {
         painter.fillRect( QRect( left, cr.top() + 2, fontWidth, 11 ), QColor( 100,100,100 ) );
      }
      left += fontWidth;
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::lineNumberAreaPaintEvent( QPaintEvent *event )
{
   QPainter painter( lineNumberArea );
   painter.fillRect( event->rect(), m_lineAreaBkColor );

   QTextBlock block = firstVisibleBlock();
   int blockNumber  = block.blockNumber();
   int top          = ( int ) blockBoundingGeometry( block ).translated( contentOffset() ).top();
   int bottom       = top +( int ) blockBoundingRect( block ).height();
   int fontHeight   = fontMetrics().height();

   while( block.isValid() && top <= event->rect().bottom() )
   {
      if( block.isVisible() && bottom >= event->rect().top() )
      {
         int iNumber = blockNumber + 1;
         int index = bookMarksGoto.indexOf( iNumber );
         if( index != -1 )
         {
            painter.fillRect( 0, top, lineNumberArea->width()-2, fontHeight, brushForBookmark( index ) );
         }
         painter.setPen( iNumber % 10 == 0 ? Qt::red : Qt::black );
         painter.drawText( 0, top, lineNumberArea->width()-2, fontHeight, Qt::AlignRight, QString::number( iNumber ) );
      }
      block  = block.next();
      top    = bottom;
      bottom = top +( int ) blockBoundingRect( block ).height();
      ++blockNumber;
   }
}

/*----------------------------------------------------------------------*/

QBrush HBQPlainTextEdit::brushForBookmark( int index )
{
   QBrush br;

   if(      index == 0 )
      br = QBrush( QColor( 255, 255, 127 ) );
   else if( index == 1 )
      br = QBrush( QColor( 175, 175, 255 ) );
   else if( index == 2 )
      br = QBrush( QColor( 255, 175, 175 ) );
   else if( index == 3 )
      br = QBrush( QColor( 175, 255, 175 ) );
   else if( index == 4 )
      br = QBrush( QColor( 255, 190, 125 ) );
   else if( index == 5 )
      br = QBrush( QColor( 175, 255, 255 ) );
   else
      br = QBrush( m_currentLineColor );

   return br;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbBookmarks( int block )
{
   int found = bookMark.indexOf( block );
   if( found == -1 )
   {
      bookMark.push_back( block );
      qSort( bookMark );
   }
   else
   {
      bookMark.remove( found );
   }

   found = -1;
   int i = 0;
   for( i = 0; i < bookMarksGoto.size(); i++ )
   {
      if( bookMarksGoto[ i ] == block )
      {
         bookMarksGoto.removeAt( i );
         found = i;
         break;
      }
   }

   if( found == -1 )
   {
      bookMarksGoto.append( block );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbGotoBookmark( int block )
{
   if( bookMarksGoto.size() > 0 )
   {
      int i;
      for( i = 0; i < bookMarksGoto.size(); i++ )
      {
         if( bookMarksGoto[ i ] == block )
         {
            QTextCursor cursor( document()->findBlockByNumber( block - 1 ) );
            setTextCursor( cursor );
            break;
         }
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbNextBookmark( int block )
{
   if( bookMark.count() > 0 )
   {
      QVector<int>::iterator i = qUpperBound( bookMark.begin(), bookMark.end(), block );
      if( i != bookMark.end() )
      {
         QTextCursor cursor( document()->findBlockByNumber( *i - 1 ) );
         setTextCursor( cursor );
      }
      else
      {
         QTextCursor cursor( document()->findBlockByNumber( *bookMark.begin() - 1 ) );
         setTextCursor( cursor );
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbPrevBookmark( int block )
{
   if( bookMark.count() > 0 )
   {
      QVector<int>::iterator i = qUpperBound( bookMark.begin(), bookMark.end(), block );
      i -= 2;
      if( i >= bookMark.begin() )
      {
         QTextCursor cursor( document()->findBlockByNumber( *i - 1 ) );
         setTextCursor( cursor );
      }
      else
      {
         QVector<int>::iterator it = bookMark.end();
         --it;
         QTextCursor cursor( document()->findBlockByNumber( *it - 1 ) );
         setTextCursor( cursor );
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbNumberBlockVisible( bool b )
{
   numberBlock = b;
   if( b )
   {
      lineNumberArea->show();
      hbUpdateLineNumberAreaWidth( hbLineNumberAreaWidth() );
   }
   else
   {
      lineNumberArea->hide();
      hbUpdateLineNumberAreaWidth( 0 );
   }
   update();
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::hbNumberBlockVisible()
{
   return numberBlock;
}

/*----------------------------------------------------------------------*/

int HBQPlainTextEdit::hbLineNumberAreaWidth()
{
   int digits = 1;
   int max = qMax( 1, blockCount() );
   while( max >= 10 )
   {
      max /= 10;
      ++digits;
   }
   int width  = lineNumberArea->fontMetrics().width( QLatin1Char( '9' ) );
   int space  = ( width * digits ) + 2;

   return space;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUpdateHorzRulerHeight( int height )
{
   horzRulerHeight = height;
   setViewportMargins( hbLineNumberAreaWidth(), horzRulerHeight, 0, 0 );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUpdateLineNumberAreaWidth( int )
{
   if( numberBlock )
   {
      setViewportMargins( hbLineNumberAreaWidth(), horzRulerHeight, 0, 0 );
   }
   else
   {
      setViewportMargins( 0, horzRulerHeight, 0, 0 );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUpdateHorzRuler( const QRect & rect, int dy )
{
   HB_SYMBOL_UNUSED( rect );

   setTabStopWidth( spaces * fontMetrics().averageCharWidth() );

   if( dy == 0 )
   {
      horzRuler->update();
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbHighlightPage()
{
#if QT_VERSION >= 0x040600
   if( highlighter )
   {
      int iLastVisBlockNum = lastVisibleBlockNumber();
      QTextBlock block = firstVisibleBlock();
      if( block.isValid() )
      {
         int i;
         for( i = block.blockNumber(); i < iLastVisBlockNum; i++ )
         {
            highlighter->rehighlightBlock( block );
            block = block.next();
            if( ! block.isValid() )
               break;
         }
      }
   }
#endif
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUpdateLineNumberArea( const QRect &rect, int dy )
{
   if( dy )
   {
      lineNumberArea->scroll( 0, dy );

      if( highlighter )
      {
#if QT_VERSION >= 0x040600
         int rows = abs( dy / fontMetrics().height() );
         int i;
         QTextBlock block;

         if( dy < 0 )
         {

            int iLastVisBlockNum = lastVisibleBlockNumber();

            for( i = iLastVisBlockNum - rows; i <= iLastVisBlockNum; i++ )
            {
               block = document()->findBlockByNumber( i );
               if( block.isValid() )
               {
                  highlighter->rehighlightBlock( block );
               }
            }
         }
         else
         {
            block = firstVisibleBlock();
            for( i = 0; i < rows; i++ )
            {
               highlighter->rehighlightBlock( block );
               block = block.next();
            }
         }
#else
         highlighter->rehighlight();
#endif
      }
   }
   else
      lineNumberArea->update( 0, rect.y(), lineNumberArea->width(), rect.height() );

   if( rect.contains( viewport()->rect() ) )
   {
      hbUpdateLineNumberAreaWidth( 0 );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetSpaces( int newSpaces )
{
   spaces = newSpaces;
   spacesTab = "";

   if( spaces > 0 )
   {
      for( int i = 0; i < spaces; ++i )
          spacesTab += " ";
   }
   else
   {
      if( spaces == -101 )
         spacesTab = "\t";
   }
}

/*----------------------------------------------------------------------*/

int HBQPlainTextEdit::hbGetIndex( const QTextCursor &crQTextCursor )
{
   QTextBlock b;
   int column = 1;
   b = crQTextCursor.block();
   column = crQTextCursor.position() - b.position();
   return column;
}

/*----------------------------------------------------------------------*/

int HBQPlainTextEdit::hbGetLine( const QTextCursor &crQTextCursor )
{
   QTextBlock b,cb;
   int line = 1;
   cb = crQTextCursor.block();
   for( b = document()->begin();b!=document()->end();b = b.next() )
   {
      if( b==cb )
         return line;
      line++;
   }
   return line;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSlotCursorPositionChanged()
{
   if( m_currentBlockNumber != textCursor().blockNumber() )
   {
      m_currentBlockNumber = textCursor().blockNumber();
      if( m_currentLineColor.isValid() )
      {
         viewport()->update();
      }
   }

   if( styleHightlighter != "none" && columnBegins == -1 )
   {
      hbBraceHighlight();
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetStyleHightlighter( const QString &style )
{
   styleHightlighter = style;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbShowHighlighter( const QString &style, bool b )
{
   if( b )
   {
      if( styleHightlighter != "none" )
      {
         delete highlighter;
         highlighter = 0;
      }
      highlighter = new HBQSyntaxHighlighter( document() );
   }
   else
   {
      delete highlighter;
      highlighter = 0;
   }
   styleHightlighter = style;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbEscapeQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "'" ), QString( "\\\'" ) );
   insertPlainText( txt );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbEscapeDQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "\"" ), QString( "\\\"" ) );
   insertPlainText( txt );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUnescapeQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "\\\'" ), QString( "'" ) );
   insertPlainText( txt );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUnescapeDQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "\\\"" ), QString( "\"" ) );
   insertPlainText( txt );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbCaseUpper()
{
   QTextCursor cursor = textCursor();
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   int b = cursor.selectionStart();
   int e = cursor.selectionEnd();
   cursor.beginEditBlock();

   insertPlainText( selTxt.toUpper() );

   cursor.setPosition( b );
   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor, e-b );
   cursor.endEditBlock();
   setTextCursor( cursor );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbCaseLower()
{
   QTextCursor cursor = textCursor();
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   int b = cursor.selectionStart();
   int e = cursor.selectionEnd();
   cursor.beginEditBlock();

   insertPlainText( selTxt.toLower() );

   cursor.setPosition( b );
   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor, e-b );
   cursor.endEditBlock();
   setTextCursor( cursor );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbConvertQuotes()
{
   QTextCursor cursor = textCursor();
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   int b = cursor.selectionStart();
   int e = cursor.selectionEnd();
   cursor.beginEditBlock();

   insertPlainText( selTxt.replace( QString( "\"" ), QString( "\'" ) ) );

   cursor.setPosition( b );
   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor, e-b );
   cursor.endEditBlock();
   setTextCursor( cursor );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbConvertDQuotes()
{
   QTextCursor cursor = textCursor();
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   int b = cursor.selectionStart();
   int e = cursor.selectionEnd();
   cursor.beginEditBlock();

   insertPlainText( selTxt.replace( QString( "\'" ), QString( "\"" ) ) );

   cursor.setPosition( b );
   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor, e-b );
   cursor.endEditBlock();
   setTextCursor( cursor );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbReplaceSelection( const QString & txt )
{
   QTextCursor cursor = textCursor();
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   int b = cursor.selectionStart();
   cursor.beginEditBlock();

   insertPlainText( txt );

   cursor.setPosition( b );
   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor, txt.length() );
   cursor.endEditBlock();
   setTextCursor( cursor );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbStreamComment()
{
   QTextCursor cursor = textCursor();
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   int b = cursor.selectionStart();
   int e = cursor.selectionEnd();
   cursor.beginEditBlock();

   insertPlainText( "/*" + selTxt + "*/"  );

   cursor.setPosition( b );
   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor, e-b+4 );
   cursor.endEditBlock();
   setTextCursor( cursor );
}

/*----------------------------------------------------------------------*/

QString HBQPlainTextEdit::hbGetSelectedText()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return "";

   QString txt = selTxt.replace( 0x2029, QString( "\n" ) );
   return txt;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbInsertTab( int mode )
{
   QTextCursor cursor = textCursor();
   QTextCursor c( cursor );

   c.setPosition( cursor.position() );
   setTextCursor( c );

   if( mode == 0 )
   {
      insertPlainText( spacesTab );
   }
   else
   {
      int icol = c.columnNumber();
      int ioff = qMin( icol, spaces );
      c.setPosition( c.position() - ioff );
   }
   setTextCursor( c );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbMoveLine( int iDirection )
{
   QTextCursor cursor = textCursor();
   QTextCursor c = cursor;

   cursor.beginEditBlock();

   cursor.movePosition( QTextCursor::StartOfLine );
   cursor.movePosition( QTextCursor::EndOfLine, QTextCursor::KeepAnchor );
   QString textCurrentLine = cursor.selectedText();

   if( iDirection == -1 && cursor.blockNumber() > 0 )
   {
      cursor.movePosition( QTextCursor::StartOfLine );
      cursor.movePosition( QTextCursor::Up );
      cursor.movePosition( QTextCursor::EndOfLine, QTextCursor::KeepAnchor );
      QString textPrevLine = cursor.selectedText();
      setTextCursor( cursor );
      insertPlainText( textCurrentLine );
      cursor.movePosition( QTextCursor::Down );
      cursor.movePosition( QTextCursor::StartOfLine );
      cursor.movePosition( QTextCursor::EndOfLine, QTextCursor::KeepAnchor );
      setTextCursor( cursor );
      insertPlainText( textPrevLine );
      c.movePosition( QTextCursor::Up );
   }
   else if( iDirection == 1 && cursor.blockNumber() < cursor.document()->blockCount() - 1 )
   {
      cursor.movePosition( QTextCursor::StartOfLine );
      cursor.movePosition( QTextCursor::Down );
      cursor.movePosition( QTextCursor::EndOfLine, QTextCursor::KeepAnchor );
      QString textPrevLine = cursor.selectedText();
      setTextCursor( cursor );
      insertPlainText( textCurrentLine );
      cursor.movePosition( QTextCursor::Up );
      cursor.movePosition( QTextCursor::StartOfLine );
      cursor.movePosition( QTextCursor::EndOfLine, QTextCursor::KeepAnchor );
      setTextCursor( cursor );
      insertPlainText( textPrevLine );
      c.movePosition( QTextCursor::Down );
   }
   cursor.endEditBlock();
   setTextCursor( c );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbDeleteLine()
{
   QTextCursor cursor = textCursor();
   QTextCursor c = cursor;

   cursor.beginEditBlock();

   cursor.movePosition( QTextCursor::StartOfLine );
   cursor.movePosition( QTextCursor::EndOfLine, QTextCursor::KeepAnchor );
   cursor.movePosition( QTextCursor::Down, QTextCursor::KeepAnchor );

   QString textUnderCursor = cursor.selectedText();
   setTextCursor( cursor );
   insertPlainText( "" );
   cursor.endEditBlock();

   setTextCursor( c );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbBlockIndent( int steps )
{
   QTextCursor cursor = textCursor();

   if( cursor.hasSelection() )
   {
      QTextCursor c = cursor;
      QTextDocument * doc = c.document();

      int bs = doc->findBlock( c.selectionStart() ).blockNumber();
      int be = doc->findBlock( c.selectionEnd() ).blockNumber();

      cursor.beginEditBlock();

      cursor.movePosition( QTextCursor::Start );
      cursor.movePosition( QTextCursor::NextBlock, QTextCursor::MoveAnchor, bs );

      int s = abs( steps );
      int i, j;
      for( i = bs; i <= be; i++ )
      {
         setTextCursor( cursor );
         for( j = 0; j < s; j++ )
         {
            cursor.movePosition( QTextCursor::StartOfLine );

            if( steps < 0 )
            {
               cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor );
               QString textUnderCursor = cursor.selectedText();
               if( textUnderCursor == " " )
               {
                  setTextCursor( cursor );
                  insertPlainText( "" );
               }
            }
            else
            {
               setTextCursor( cursor );
               insertPlainText( " " );
            }
         }
         cursor.movePosition( QTextCursor::NextBlock, QTextCursor::MoveAnchor, 1 );
      }
      cursor.endEditBlock();

      setTextCursor( c );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbBlockComment()
{
   QTextCursor cursor = textCursor();
   QTextCursor c = cursor;
   QTextDocument * doc = c.document();

   int bs = doc->findBlock( c.selectionStart() ).blockNumber();
   int be = doc->findBlock( c.selectionEnd() ).blockNumber();

   cursor.beginEditBlock();

   cursor.movePosition( QTextCursor::Start );
   cursor.movePosition( QTextCursor::NextBlock, QTextCursor::MoveAnchor, bs );
   int i;
   for( i = bs; i <= be; i++ )
   {
      setTextCursor( cursor );

      cursor.movePosition( QTextCursor::StartOfLine );
      cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor );
      cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor );
      QString textUnderCursor = cursor.selectedText();
      if( textUnderCursor == "//" )
      {
         setTextCursor( cursor );
         insertPlainText( "" );
      }
      else
      {
         cursor.movePosition( QTextCursor::StartOfLine );
         insertPlainText( "//" );
      }
      cursor.movePosition( QTextCursor::NextBlock, QTextCursor::MoveAnchor, 1 );
   }
   cursor.endEditBlock();
   setTextCursor( c );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbDuplicateLine()
{
   QTextCursor cursor = textCursor();
   QTextCursor c = cursor;
   cursor.movePosition( QTextCursor::StartOfLine );
   cursor.movePosition( QTextCursor::EndOfLine, QTextCursor::KeepAnchor );
   QString textUnderCursor = cursor.selectedText();
   cursor.movePosition( QTextCursor::EndOfLine );
   setTextCursor( cursor );
   insertPlainText( "\n" + textUnderCursor );
   setTextCursor( c );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbBraceHighlight()
{
   extraSelections.clear();
   setExtraSelections( extraSelections );
   selection.format.setBackground( m_braceHiliteColor );

   QTextCursor cursor = textCursor();

   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor );
   QString brace = cursor.selectedText();

   if(    ( brace == "{" ) || ( brace == "}" )
       || ( brace == "[" ) || ( brace == "]" )
       || ( brace == "(" ) || ( brace == ")" )
       || ( brace == "<" ) || ( brace == ">" ) )
   {
      QString openBrace;
      QString closeBrace;

      if( ( brace == "{" ) || ( brace == "}" ) )
      {
         openBrace = "{";
         closeBrace = "}";
      }
      else if( ( brace == "[" ) || ( brace == "]" ) )
      {
         openBrace = "[";
         closeBrace = "]";
      }
      else if( ( brace == "(" ) || ( brace == ")" ) )
      {
         openBrace = "(";
         closeBrace = ")";
      }
      else if( ( brace == "<" ) || ( brace == ">" ) )
      {
         openBrace = "<";
         closeBrace = ">";
      }
      matchPair( cursor, brace, openBrace, closeBrace, m_matchBracesAll, 0 );
   }
   else
   {
      cursor = textCursor();
      cursor.select( QTextCursor::WordUnderCursor );
      QString brace = cursor.selectedText().toUpper();

      if( brace == "IF"       || brace == "ENDIF"     || brace == "IFDEF"  ||
          brace == "FOR"      || brace == "NEXT"      ||
          brace == "SWITCH"   || brace == "ENDSWITCH" ||
          brace == "DO"       || brace == "ENDCASE"   || brace == "ENDDO"  ||
          brace == "CLASS"    || brace == "ENDCLASS"  ||
          brace == "FUNCTION" || brace == "RETURN"    || brace == "METHOD" || brace == "PROCEDURE" )
      {
         QString openBrace;
         QString closeBrace;

         if( ( brace == "IF" ) || ( brace == "ENDIF" ) )
         {
            QTextCursor c( cursor );
            c.movePosition( QTextCursor::PreviousWord, QTextCursor::MoveAnchor );
            c.movePosition( QTextCursor::PreviousWord, QTextCursor::MoveAnchor );
            c.movePosition( QTextCursor::EndOfWord, QTextCursor::KeepAnchor );
            QString brc = c.selectedText();
            if( brc == "#" )
            {
               c.movePosition( QTextCursor::EndOfWord, QTextCursor::KeepAnchor );
               cursor = c;
               brace = "#" + brace;
               if( brace == "#IF" )
               {
                  openBrace  = "#IF";
                  closeBrace = "#ENDIF";
               }
               else
               {
                  QRegExp openBrace( "(#IFDEF|#IF)" );
                  openBrace.setCaseSensitivity( Qt::CaseInsensitive );
                  QRegExp closeBrace( "#ENDIF" );
                  closeBrace.setCaseSensitivity( Qt::CaseInsensitive );
                  matchPair( cursor, QRegExp(), openBrace, closeBrace, true, 0, false );
                  return;
               }
            }
            else
            {
               openBrace  = "IF";
               closeBrace = "ENDIF";
            }
         }
         else if( ( brace == "IFDEF" ) )
         {
            QTextCursor c( cursor );
            c.movePosition( QTextCursor::PreviousWord, QTextCursor::MoveAnchor );
            c.movePosition( QTextCursor::PreviousWord, QTextCursor::MoveAnchor );
            c.movePosition( QTextCursor::EndOfWord, QTextCursor::KeepAnchor );
            QString brc = c.selectedText();
            if( brc == "#" )
            {
               c.movePosition( QTextCursor::EndOfWord, QTextCursor::KeepAnchor );
               cursor = c;
               brace = "#" + brace;
               openBrace  = "#IFDEF";
               closeBrace = "#ENDIF";
            }
         }
         else if( ( brace == "FOR" ) || ( brace == "NEXT" ) )
         {
            openBrace  = "FOR";
            closeBrace = "NEXT";
         }
         else if( ( brace == "SWITCH" ) || ( brace == "ENDSWITCH" ) )
         {
            openBrace  = "SWITCH";
            closeBrace = "ENDSWITCH";
         }
         else if( ( brace == "DO" ) || ( brace == "ENDCASE" ) || ( brace == "ENDDO" ) )
         {
            if( brace == "DO" )
            {
               cursor.movePosition( QTextCursor::NextWord, QTextCursor::KeepAnchor );
               cursor.movePosition( QTextCursor::EndOfWord, QTextCursor::KeepAnchor );
               brace = cursor.selectedText();
               if( brace == "DO CASE" )
               {
                  openBrace = "DO CASE";
                  closeBrace = "ENDCASE";
               }
               else if( brace == "DO WHILE" )
               {
                  openBrace = "DO WHILE";
                  closeBrace = "ENDDO";
               }
            }
            else if( brace == "ENDCASE" )
            {
               openBrace  = "DO CASE";
               closeBrace = "ENDCASE";
            }
            else
            {
               openBrace  = "DO WHILE";
               closeBrace = "ENDDO";
            }
         }
         else if( ( brace == "FUNCTION" ) )
         {
            openBrace  = "FUNCTION";
            closeBrace = "RETURN";
         }
         else if( ( brace == "METHOD" ) )
         {
            openBrace  = "METHOD";
            closeBrace = "RETURN";
         }
         else if( ( brace == "PROCEDURE" ) )
         {
            openBrace  = "PROCEDURE";
            closeBrace = "RETURN";
         }
         else if( ( brace == "RETURN" ) )
         {
            QRegExp openBrace( "\\b(FUNCTION|METHOD|PROCEDURE)\\b" );
            openBrace.setCaseSensitivity( Qt::CaseInsensitive );
            QRegExp closeBrace( "\\bRETURN\\b" );
            closeBrace.setCaseSensitivity( Qt::CaseInsensitive );
            matchPair( cursor, QRegExp(), openBrace, closeBrace, true, QTextDocument::FindWholeWords, false );
            return;
         }
         else if( ( brace == "CLASS" ) || ( brace == "ENDCLASS" ) )
         {
            openBrace  = "CLASS";
            closeBrace = "ENDCLASS";
         }
         matchPair( cursor, brace, openBrace, closeBrace, true, QTextDocument::FindWholeWords );
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::matchPair( QTextCursor cursor, QString brace, QString openBrace, QString closeBrace, bool bBraceAll, QTextDocument::FindFlags flags )
{
   QTextDocument *doc = document();
   QTextCursor cursorC;
   QTextCursor cursorO;
   QTextCursor matches;

   if( brace == openBrace )
   {
      cursorC = doc->find( closeBrace, cursor, flags );
      cursorO = doc->find( openBrace, cursor, flags );
      if( cursorO.isNull() )
      {
         matches = cursorC;
      }
      else
      {
         while( cursorC.position() > cursorO.position() )
         {
            cursorC = doc->find( closeBrace, cursorC, flags );
            cursorO = doc->find( openBrace, cursorO, flags );
            if( cursorO.isNull() )
                break;
         }
         matches = cursorC;
      }
   }
   else if( brace == closeBrace )
   {
      cursorO = doc->find( openBrace, cursor, QTextDocument::FindBackward | flags );
      cursorC = doc->find( closeBrace, cursor, QTextDocument::FindBackward | flags );
      if( cursorC.isNull() )
      {
         matches = cursorO;
      }
      else
      {
         while( cursorO.position() < cursorC.position() )
         {
            cursorO = doc->find( openBrace, cursorO, QTextDocument::FindBackward | flags );
            cursorC = doc->find( closeBrace, cursorC, QTextDocument::FindBackward | flags );
            if( cursorC.isNull() )
                break;
         }
         matches = cursorO;
      }
   }
   if( ! matches.isNull() )
   {
      if( bBraceAll )
      {
         selection.cursor = cursor;
         extraSelections.append( selection );
      }
      selection.cursor = matches;
      extraSelections.append( selection );
      setExtraSelections( extraSelections );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::matchPair( QTextCursor cursor, QRegExp brace, QRegExp openBrace, QRegExp closeBrace, bool bBraceAll, QTextDocument::FindFlags flags, bool bForward )
{
   QTextDocument *doc = document();
   QTextCursor cursorC;
   QTextCursor cursorO;
   QTextCursor matches;
   Q_UNUSED( brace );

   if( bForward )
   {
      cursorC = doc->find( closeBrace, cursor, flags );
      cursorO = doc->find( openBrace, cursor, flags );
      if( cursorO.isNull() )
      {
         matches = cursorC;
      }
      else
      {
         while( cursorC.position() > cursorO.position() )
         {
            cursorC = doc->find( closeBrace, cursorC, flags );
            cursorO = doc->find( openBrace, cursorO, flags );
            if( cursorO.isNull() )
                break;
         }
         matches = cursorC;
      }
   }
   else
   {
      cursorO = doc->find( openBrace, cursor, QTextDocument::FindBackward | flags );
      cursorC = doc->find( closeBrace, cursor, QTextDocument::FindBackward | flags );
      if( cursorC.isNull() )
      {
         matches = cursorO;
      }
      else
      {
         while( cursorO.position() < cursorC.position() )
         {
            cursorO = doc->find( openBrace, cursorO, QTextDocument::FindBackward | flags );
            cursorC = doc->find( closeBrace, cursorC, QTextDocument::FindBackward | flags );
            if( cursorC.isNull() )
                break;
         }
         matches = cursorO;
      }
   }
   if( ! matches.isNull() )
   {
      if( bBraceAll )
      {
         selection.cursor = cursor;
         extraSelections.append( selection );
      }
      selection.cursor = matches;
      extraSelections.append( selection );
      setExtraSelections( extraSelections );
   }
}
#endif
