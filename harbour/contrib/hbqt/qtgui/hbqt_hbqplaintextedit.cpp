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

#define selectionState_off                        0
#define selectionState_on                         1

#define selectionMode_none                        0
#define selectionMode_stream                      1
#define selectionMode_column                      2
#define selectionMode_line                        3

#define selectionDisplay_none                     0
#define selectionDisplay_qt                       1
#define selectionDisplay_ide                      2

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
   selectionState           = selectionState_off;
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

   #if 0
   QTextFrameFormat format( this->document()->rootFrame()->frameFormat() );
   format.setMargin( 0 );
   format.setPadding( 0 );
   this->rootFrame().setFrameFormat( format );
   #endif

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

   ttFrame = new QFrame( this );
   ttFrame->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Expanding );
   ttLayout = new QVBoxLayout( ttFrame );
   ttFrame->setLayout( ttLayout );
   ttLabel = new QLabel( ttFrame );
   ttLabel->setWordWrap( true );
   ttLabel->setText( "" );
   hbSetProtoStyle();
   ttLayout->addWidget( ttLabel );

   ttTextEdit = new QTextEdit( ttFrame );
   ttTextEdit->setStyleSheet( "background-color: rgb(255,255,174); border: 1px solid black;" );// padding: 3px;" );
   ttTextEdit->setFocusPolicy( Qt::NoFocus );
   ttTextEdit->setReadOnly( true );
   ttTextEdit->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Expanding );
   ttTextEdit->setFont( QFont( "Courier New", 10 ) );
   ttLayout->addWidget( ttTextEdit );

   ttFrame->setFocusPolicy( Qt::NoFocus );
   ttFrame->hide();

   #if 0
   timer                    = new QTimer( this );
   connect( timer, SIGNAL( timeout() ), this, SLOT( hbUpdateCaret() ) );
   timer->start( 500 );
   #endif

   QTextDocument * doc = document();
   doc->setDocumentMargin( 0 );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbShowPrototype( const QString & tip, int rows, int cols )
{
   if( ! isCompletionTipsActive ){
      ttTextEdit->hide();
      ttLabel->hide();
      return;
   }

   if( rows <= 1 )
   {
      ttLabel->setText( tip );
      ttTextEdit->setText( "" );
      ttTextEdit->hide();
      ttLabel->show();
   }
   else
   {
      ttLabel->setText( "" );
      ttTextEdit->setText( tip );
      ttLabel->hide();
      ttTextEdit->show();
   }

   if( tip == ( QString ) "" )
   {
      isTipActive = false;
      ttFrame->hide();
   }
   else
   {
      isTipActive = true;

      if( rows > 1 )
      {
         int h = ( ttTextEdit->fontMetrics().height() * rows ) + 12 + 24;
         int w = ( ttTextEdit->fontMetrics().averageCharWidth() * cols ) + 12 + 24;

         ttFrame->setMinimumHeight( h );
         ttFrame->setMinimumWidth( w );
         ttFrame->setMaximumHeight( h );
         ttFrame->setMaximumWidth( w );
      }

      QRect  r = cursorRect();
      int    w = ttFrame->width();

      int    x = r.x()-r.width();
      int nOff = viewport()->width() - ( x + w );
      if( nOff < 0 )
         x = qMax( 0, x + nOff );
      ttFrame->move( qMax( 0, x ), r.y() + 7 + horzRulerHeight );

      ttFrame->show();
   }
}

/*----------------------------------------------------------------------*/

HBQPlainTextEdit::~HBQPlainTextEdit()
{
   #if 0
   if( timer )
      timer->stop();
   #endif

   disconnect( this, SIGNAL( blockCountChanged( int ) )            );
   disconnect( this, SIGNAL( updateRequest( const QRect &, int ) ) );
   disconnect( this, SIGNAL( cursorPositionChanged() )             );

   delete lineNumberArea;

   if( block )
      hb_itemRelease( block );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetEventBlock( PHB_ITEM pBlock )
{
   if( pBlock )
   {
      block = hb_itemNew( pBlock );
      hb_gcUnlock( block );
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbApplyKey( int key, Qt::KeyboardModifiers modifiers, const QString & txt )
{
   QKeyEvent * ev = new QKeyEvent( QEvent::KeyPress, key, modifiers, txt );
   QPlainTextEdit::keyPressEvent( ev );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbTogglePersistentSelection()
{
   isSelectionPersistent = ! isSelectionPersistent;
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbRefresh()
{
   repaint();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSetProtoStyle( const QString & css )
{
   if( css == ( QString ) "" )
      ttLabel->setStyleSheet( "background-color: rgb(255,255,174); border: 1px solid black; padding: 3px;" );
   else
      ttLabel->setStyleSheet( css );
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::event( QEvent *event )
{
   if( event->type() == QEvent::KeyPress )
   {
      QKeyEvent *keyEvent = ( QKeyEvent * ) event;
      if( ( keyEvent->key() == Qt::Key_Tab ) && ( keyEvent->modifiers() & Qt::ControlModifier ) )
      {
         return false;
      }
      else
      {
         if( ( keyEvent->key() == Qt::Key_Tab ) && !( keyEvent->modifiers() & Qt::ControlModifier & Qt::AltModifier & Qt::ShiftModifier ) )
         {
            this->hbInsertTab( 0 );
            return true;
         }
         else if( ( keyEvent->key() == Qt::Key_Backtab ) && ( keyEvent->modifiers() & Qt::ShiftModifier ) )
         {
            this->hbInsertTab( 1 );
            return true;
         }
      }
   }
   else if( event->type() == QEvent::ToolTip )
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

bool HBQPlainTextEdit::isCursorInSelection()
{
   int cb = columnBegins <= columnEnds ? columnBegins : columnEnds;
   int ce = columnBegins <= columnEnds ? columnEnds   : columnBegins;
   int rb = rowBegins    <= rowEnds    ? rowBegins    : rowEnds;
   int re = rowBegins    <= rowEnds    ? rowEnds      : rowBegins;

   QTextCursor c = textCursor();
   int col = c.columnNumber();
   int row = c.blockNumber();

   return( col >= cb && col <= ce && row >= rb && row <= re );
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbClearSelection()
{
   setCursorWidth( 1 );

   rowBegins    = -1;
   rowEnds      = -1;
   columnBegins = -1;
   columnEnds   = -1;

   emit selectionChanged();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbSelectAll()
{
   setCursorWidth( 1 );

   rowBegins    = 0;
   rowEnds      = document()->blockCount();
   columnBegins = 0;
   columnEnds   = 0;

   emit selectionChanged();
   repaint();
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

void HBQPlainTextEdit::hbSetSelectionInfo( PHB_ITEM selectionInfo )
{
   rowBegins     = hb_arrayGetNI( selectionInfo, 1 );
   columnBegins  = hb_arrayGetNI( selectionInfo, 2 );
   rowEnds       = hb_arrayGetNI( selectionInfo, 3 );
   columnEnds    = hb_arrayGetNI( selectionInfo, 4 );
   selectionMode = hb_arrayGetNI( selectionInfo, 5 );

   PHB_ITEM pSome = hb_arrayGetItemPtr( selectionInfo, 6 );
   if( hb_itemType( pSome ) & HB_IT_LOGICAL ){
      if( hb_itemGetL( pSome ) ){
         QTextCursor c( textCursor() );
         c.clearSelection();
      }
   }
   pSome = hb_arrayGetItemPtr( selectionInfo, 7 );
   if( hb_itemType( pSome ) & HB_IT_LOGICAL ){
      if( hb_itemGetL( pSome ) ){
         emit selectionChanged();
      }
   }
   else {
      emit selectionChanged();
   }
   repaint();
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbGetSelectionInfo()
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21000 );
      PHB_ITEM p2 = hb_itemNew( NULL );

      hb_arrayNew( p2, 6 );

      hb_arraySetNI( p2, 1, rowBegins      );
      hb_arraySetNI( p2, 2, columnBegins   );
      hb_arraySetNI( p2, 3, rowEnds        );
      hb_arraySetNI( p2, 4, columnEnds     );
      hb_arraySetNI( p2, 5, selectionMode  );
      hb_arraySetNI( p2, 6, selectionState );

      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
   }
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
         setCursorWidth( 1 );
      }
      else
      {
         switch( mode )
         {
            case selectionMode_stream:
            {
               setCursorWidth( 1 );
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
               setCursorWidth( 0 );

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
               setCursorWidth( 1 );
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
               setCursorWidth( 1 );
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
               setCursorWidth( 0 );
               selectionMode       = selectionMode_column;
               isColumnSelectionON = true;
               isLineSelectionON   = false;
               break;
            }
         }
      }
   }
   emit selectionChanged();
   repaint();
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

void HBQPlainTextEdit::hbCut( int k )
{
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, 21014 );
      PHB_ITEM p2 = hb_itemNew( NULL );

      hb_arrayNew( p2, 7 );
      hb_arraySetNI( p2, 1, rowBegins      );
      hb_arraySetNI( p2, 2, columnBegins   );
      hb_arraySetNI( p2, 3, rowEnds        );
      hb_arraySetNI( p2, 4, columnEnds     );
      hb_arraySetNI( p2, 5, selectionMode  );
      hb_arraySetNI( p2, 6, selectionState );
      hb_arraySetNI( p2, 7, k              );

      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );

      if( selectionMode == selectionMode_column ) //&& k == 0 )
         columnEnds = columnBegins;
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
      PHB_ITEM p2 = hb_itemNew( NULL );

      hb_arrayNew( p2, 7 );
      hb_arraySetNI( p2, 1, rowBegins      );
      hb_arraySetNI( p2, 2, columnBegins   );
      hb_arraySetNI( p2, 3, rowEnds        );
      hb_arraySetNI( p2, 4, columnEnds     );
      hb_arraySetNI( p2, 5, selectionMode  );
      hb_arraySetNI( p2, 6, selectionState );
      hb_arraySetNI( p2, 7, 0              );

      hb_vmEvalBlockV( block, 2, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );
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
      PHB_ITEM p2 = hb_itemPutNI( NULL, selectionMode );
      hb_vmEvalBlockV( block, 1, p1, p2 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );

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

void HBQPlainTextEdit::mouseDoubleClickEvent( QMouseEvent *event )
{
   QPlainTextEdit::mouseDoubleClickEvent( event );

   QTextCursor c( textCursor() );
   if( c.hasSelection() )
   {
      rowBegins     = c.blockNumber();
      rowEnds       = rowBegins;
      columnEnds    = c.columnNumber();
      columnBegins  = columnEnds - ( c.selectionEnd() - c.selectionStart() );
      selectionMode = selectionMode_stream;
      c.clearSelection();
      setTextCursor( c );
      emit selectionChanged();
      repaint();
   }

   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, QEvent::MouseButtonDblClick );
      hb_vmEvalBlockV( block, 1, p1 );
      hb_itemRelease( p1 );
   }
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

   if( event->modifiers() & Qt::ShiftModifier )
   {
      QTextCursor c( textCursor() );
      rowBegins    = c.blockNumber();
      columnBegins = c.columnNumber();

      QPlainTextEdit::mousePressEvent( event );

      c = textCursor();
      rowEnds    = c.blockNumber();
      columnEnds = c.columnNumber();

      selectionState = 1;
      setCursorWidth( 1 );
      selectionMode = selectionMode_stream;
      emit selectionChanged();

      repaint();
   }
   else
   {
      if( event->buttons() & Qt::LeftButton )
      {
         setCursorWidth( 1 );
         if( ! isSelectionPersistent )
         {
            selectionState = 0;
            hbClearSelection();
         }
         else
         {
            selectionState = 1;
         }
      }
      QPlainTextEdit::mousePressEvent( event );
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
      return;
   }
   selectionState = 1;
   setCursorWidth( 1 );
   emit selectionChanged();
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
      selectionMode = selectionMode_stream;

   if( event->buttons() & Qt::LeftButton )
   {
      if( selectionState == 1 )
      {
         selectionState = 2;
         hbClearSelection();
      }

      if( columnBegins == -1 )
      {
         if( selectionMode == selectionMode_column )
            setCursorWidth( 0 );

         QTextCursor c( textCursor() );

         rowBegins    = c.blockNumber();
         columnBegins = c.columnNumber();
         rowEnds      = rowBegins;
         columnEnds   = columnBegins;

         emit selectionChanged();
         QPlainTextEdit::mouseMoveEvent( event );
      }
      else
      {
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
         repaint();
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::keyReleaseEvent( QKeyEvent * event )
{
   QPlainTextEdit::keyReleaseEvent( event );

   if( ( event->modifiers() & Qt::ControlModifier ) && event->text() == "" )
   {
      if( selectionState == 2 )
      {
         selectionState = 1;
         emit selectionChanged();
      }
   }
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::hbKeyPressSelectionByApplication( QKeyEvent * event )
{
   bool shift = event->modifiers() & Qt::ShiftModifier;
   int      k = event->key();

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
         {
            QPlainTextEdit::keyPressEvent( event );
            columnEnds = textCursor().columnNumber();
            break;
         }
         case Qt::Key_End:
         {
            QTextCursor c( textCursor() );
            c.movePosition( QTextCursor::EndOfLine, QTextCursor::MoveAnchor );
            if( c.columnNumber() <= columnEnds )
            {
               QPlainTextEdit::keyPressEvent( event );
               columnEnds = textCursor().columnNumber();;
            }
            else
            {
               event->ignore();
            }
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
      repaint();
   }
   else
   {
      event->ignore();
   }
   return true;
}

/*----------------------------------------------------------------------*/

bool HBQPlainTextEdit::hbKeyPressSelection( QKeyEvent * event )
{
   int k = event->key();

   bool ctrl  = event->modifiers() & Qt::ControlModifier;
   bool shift = event->modifiers() & Qt::ShiftModifier;
   if( ctrl && shift && ! isNavableKey( k ) ) {
      return false;
   }

   if( ctrl && event->text().isEmpty() && ! isNavableKey( k ) ) {
      return false;
   }

   if( ctrl && ( k == Qt::Key_C || k == Qt::Key_V || k == Qt::Key_X ||
                 k == Qt::Key_A || k == Qt::Key_Z || k == Qt::Key_Y ) ) {
      event->ignore();
      return true;
   }

   if( isSelectionByApplication ) {
      return hbKeyPressSelectionByApplication( event );
   }

   bool bClear = false;

   if( shift && isNavableKey( k ) )
   {
      if( selectionMode == selectionMode_line )
      {
         selectionMode = selectionMode_stream;
         selectionState = 0;
      }

      if( selectionState == 0 )
      {
         hbClearSelection();
      }

      isShiftPressed = true;
      event->accept();
      QTextCursor c( textCursor() );
      c.clearSelection();
      setTextCursor( c );

      if( columnBegins == -1 )
      {
         if( selectionMode == selectionMode_column )
            setCursorWidth( 0 );

         selectionState = 2;
         rowBegins      = c.blockNumber();
         columnBegins   = c.columnNumber();
         rowEnds        = rowBegins;
         columnEnds     = columnBegins;
         emit selectionChanged();
         repaint();
      }

      QKeyEvent * ev = new QKeyEvent( event->type(), event->key(), ctrl ? Qt::ControlModifier : Qt::NoModifier, event->text() );
      keyPressEvent( ev );
      return true;
   }

   if( isShiftPressed && isNavableKey( k ) )
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
         switch( k )
         {
         case Qt::Key_Right:
         {
            QTextCursor c( textCursor() );
            c.movePosition( QTextCursor::EndOfLine );
            if( c.columnNumber() <= columnEnds ){
               setTextCursor( c );
               ensureCursorVisible();
            }
            else
            {
               #if 0    /* Tobe Matured */
               int v = horizontalScrollBar()->value();
               int m = horizontalScrollBar()->maximum();
               int w = fontMetrics().averageCharWidth();
               if( ( ( columnEnds + 1 ) * w ) > m ){
                  if( v == m )
                    horizontalScrollBar()->setMaximum( m + w );
               }
               horizontalScrollBar()->setValue( v + w );
               #endif
            }
            event->ignore();
            columnEnds++;
            break;
         }
         case Qt::Key_Left:
         {
            QTextCursor c( textCursor() );
            int col = c.columnNumber();
            if( col < columnEnds - 1 ){
               c.movePosition( QTextCursor::Left );
               columnEnds--;
            }
            else if( columnEnds - 1 >= 0 ){
               columnEnds--;
            }
            event->ignore();
            break;
         }
         case Qt::Key_Home:
         {
            QPlainTextEdit::keyPressEvent( event );
            columnEnds = textCursor().columnNumber();
            break;
         }
         case Qt::Key_End:
         {
            QTextCursor c( textCursor() );
            c.movePosition( QTextCursor::EndOfLine, QTextCursor::MoveAnchor );
            if( c.columnNumber() <= columnEnds ){
               QPlainTextEdit::keyPressEvent( event );
               columnEnds = textCursor().columnNumber();;
            } else {
               event->ignore();
            }
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
      emit selectionChanged();
      repaint();
      return true;
   }
   else if( ! ctrl && k >= ' ' && k < 127 && columnBegins >= 0 && selectionMode == selectionMode_column )
   {
      if( ( columnBegins == columnEnds && selectionState > 0 ) || isCursorInSelection() )
      {
         if( block )
         {
            PHB_ITEM p1 = hb_itemPutNI( NULL, 21013 );
            PHB_ITEM p2 = hb_itemNew( NULL );
            hb_arrayNew( p2, 7 );
            hb_arraySetNI( p2, 1, rowBegins      );
            hb_arraySetNI( p2, 2, columnBegins   );
            hb_arraySetNI( p2, 3, rowEnds        );
            hb_arraySetNI( p2, 4, columnEnds     );
            hb_arraySetNI( p2, 5, selectionMode  );
            hb_arraySetNI( p2, 6, selectionState );
            hb_arraySet( p2, 7, hbqt_create_objectGC( hbqt_gcAllocate_QKeyEvent( event, false ), "hb_QKeyEvent" ) );
            hb_vmEvalBlockV( block, 2, p1, p2 );
            hb_itemRelease( p1 );
            hb_itemRelease( p2 );

            if( columnBegins == columnEnds ){
               columnBegins++;
               columnEnds++;
            }
            repaint();
            event->accept();
            return true;
         }
      }
      else
         bClear = true;
   }
   else if( ! ctrl && ( k == Qt::Key_Backspace || k == Qt::Key_Delete ) && columnBegins >= 0  && selectionState > 0 && selectionMode == selectionMode_column )
   {
      hbCut( k );
      if( k == Qt::Key_Backspace ){
         columnBegins--;
         columnEnds--;
      }
      else {
         columnEnds = columnBegins;
      }
      repaint();
      event->accept();
      return true;
   }
   else if( ! ctrl && ( k == Qt::Key_Backspace || k == Qt::Key_Delete ) && columnBegins >= 0 && selectionState > 0 && ( selectionMode == selectionMode_stream || selectionMode == selectionMode_line ) )
   {
      hbCut( Qt::Key_Delete );
      repaint();
      selectionState = 0;
      if( k == Qt::Key_Delete ){
         event->accept();
         return true;
      }
   }
   else if( ! ctrl && k >= ' ' && k < 127 && columnBegins >= 0 && selectionMode == selectionMode_stream ) //selectionState > 0
   {
      hbCut( Qt::Key_Delete );
      hbClearSelection();
      repaint();
      selectionState = 0;
   }
   else if( isNavableKey( k ) || ( k >= ' ' && k < 127 ) )
   {
      bClear = true;
   }

   if( bClear )
   {
      if( isSelectionPersistent )
      {
         if( selectionState > 0 )
         {
            emit selectionChanged();
            setCursorWidth( 1 );
            selectionState = 0;
            if( columnEnds == columnBegins ){
               hbClearSelection();
            }
         }
      }
      else
      {
         if( selectionState > 0 )
         {
            emit selectionChanged();
            setCursorWidth( 1 );
            selectionState = 0;
            hbClearSelection();
            repaint();
         }
      }
   }
   return false;

   #if 0
   else if( selectionMode == selectionMode_line )
   {
      if( isLineSelectionON && isNavableKey( k ) )
      {
         QPlainTextEdit::keyPressEvent( event );
         QTextCursor c( textCursor() );
         rowEnds = c.blockNumber();
         repaint();
         return true;
      }
      else if( ! isSelectionPersistent )
      {
         if( selectionState > 0 )
         {
            emit selectionChanged();
            setCursorWidth( 1 );
            selectionState = 0;
            hbClearSelection();
            repaint();
         }
      }
   }
   #endif
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::keyPressEvent( QKeyEvent * event )
{
   if( c && c->popup()->isVisible() )
   {
      // The following keys are forwarded by the completer to the widget
      switch( event->key() )
      {
      case Qt::Key_Enter   :
      case Qt::Key_Return  :
      case Qt::Key_Escape  :
      case Qt::Key_Tab     :
      case Qt::Key_Backtab :
         event->ignore();
         return;                                    /* let the completer do default behavior */
      case Qt::Key_Space:
         if( block ){
            PHB_ITEM p1 = hb_itemPutNI( NULL, 21001 );
            hb_vmEvalBlockV( block, 1, p1 );
            hb_itemRelease( p1 );

            hbRefreshCompleter();
         }
         break;
      case Qt::Key_ParenLeft:
         if( block ){
            PHB_ITEM p1 = hb_itemPutNI( NULL, 21002 );
            hb_vmEvalBlockV( block, 1, p1 );
            hb_itemRelease( p1 );
         }
         break;
      default:
         break;
      }
   }

   if( hbKeyPressSelection( event ) ){
      return;
   }
   QPlainTextEdit::keyPressEvent( event );

   if( ! isCodeCompletionActive ){
      if( c ){
         c->popup()->hide();
      }
      return;
   }

   if( ! c ){
      return;
   }
   if( isTipActive ){
      c->popup()->hide();
      return;
   }

   if( ! isAliasCompleter ){
      hbRefreshCompleter( hbTextAlias() );
   }

   if( ( event->modifiers() & ( Qt::ControlModifier | Qt::AltModifier ) ) ){
      c->popup()->hide();
      return;
   }
   const bool ctrlOrShift = event->modifiers() & ( Qt::ControlModifier | Qt::ShiftModifier );
   if( ctrlOrShift && event->text().isEmpty() ){
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

   if( completionPrefix != c->completionPrefix() ) {
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

void HBQPlainTextEdit::hbRefreshCompleter( const QString & alias )
{
   if( block ){
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

   int curBlock      = textCursor().blockNumber();

   QTextBlock tblock = firstVisibleBlock();
   int blockNumber   = tblock.blockNumber();
   int height        = ( int ) blockBoundingRect( tblock ).height();
   int top           = ( int ) blockBoundingGeometry( tblock ).translated( contentOffset() ).top();
   int bottom        = top + height;

   while( tblock.isValid() && top <= event->rect().bottom() )
   {
      if( tblock.isVisible() && bottom >= event->rect().top() )
      {
         int index = bookMarksGoto.indexOf( blockNumber + 1 );
         if( index != -1 )
         {
            QRect r( 0, top, viewport()->width(), height );
            painter.fillRect( r, brushForBookmark( index ) );
         }
         else if( curBlock == blockNumber && m_currentLineColor.isValid() )
         {
            if( highlightCurLine == true )
            {
               QRect r = HBQPlainTextEdit::cursorRect();
               r.setX( 0 );
               r.setWidth( viewport()->width() );
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

/* this->hbDrawCursor( event ); */

   QPlainTextEdit::paintEvent( event );
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
   int off          = fontMetrics().height() / 4;

   while( block.isValid() && top <= event->rect().bottom() )
   {
      if( block.isVisible() && bottom >= event->rect().top() )
      {
         QString number = QString::number( blockNumber + 1 );
         painter.setPen( (  blockNumber + 1 ) % 10 == 0 ? Qt::red : Qt::black );
         painter.drawText( 0, top, lineNumberArea->width()-2, fontMetrics().height(), Qt::AlignRight, number );

         int index = bookMarksGoto.indexOf( number.toInt() );
         if( index != -1 )
         {
            painter.setBrush( brushForBookmark( index ) );
            painter.drawRect( 5, top + off, off * 2, off * 2 );
         }
      }
      block  = block.next();
      top    = bottom;
      bottom = top +( int ) blockBoundingRect( block ).height();
      ++blockNumber;
   }
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

         //int marginX = ( c > 0 ? 0 : contentsRect().left() ) + 2 ;
         int marginX = ( c > 0 ? 0 : contentsRect().left() ) ;
         int fontWidth = fontMetrics().averageCharWidth();

         int top = ( ( rb <= t ) ? 0 : ( ( rb - t ) * fontHeight ) ) + ttop;
         int btm = ( ( re - t + 1 ) * fontHeight ) - top + ttop;
         btm = btm > viewport()->height() ? viewport()->height() : btm;
         QBrush br( m_selectionColor );

         if( selectionMode == selectionMode_column )
         {
            int x = ( ( cb - c ) * fontWidth ) + marginX;
            int w = ( cb == ce ? 1 : ( ( ce - cb ) * fontWidth ) );

            QRect r( x, top, w, btm );
            p.fillRect( r, br );
         }
         else if( selectionMode == selectionMode_stream )
         {
            int   i;
            int   width  = viewport()->width();
            QRect r;

            for( i = ( rb >= t ? rb : t ); i <= re; i++ )
            {
               if( rowBegins > rowEnds )
               {
                  if( i == rowEnds )
                  {
                     if( rb == re )
                     {
                        int x = ( ( cb - c ) * fontWidth ) + marginX;
                        int w = ( ce - cb ) * fontWidth + marginX ;
                        r = QRect( x, top, w, fontHeight );
                     }
                     else
                     {
                        int x = ( ( columnEnds - c ) * fontWidth ) + marginX;
                        r = QRect( x, top, width + abs( x ), fontHeight );
                     }
                  }
                  else if( i == rowBegins )
                  {
                     int x = ( ( columnBegins - c ) * fontWidth ) + marginX;
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
                        int x = ( ( cb - c ) * fontWidth ) + marginX;
                        int w = ( ce - cb ) * fontWidth + marginX;
                        r = QRect( x, top, w, fontHeight );
                     }
                     else
                     {
                        int x = ( ( columnBegins - c ) * fontWidth ) + marginX;
                        r = QRect( x, top, width + abs( x ), fontHeight );
                     }
                  }
                  else if( i == rowEnds )
                  {
                     int x = ( ( columnEnds - c ) * fontWidth ) + marginX;
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
         else if( selectionMode == selectionMode_line )
         {
            QRect r( 0, top, viewport()->width(), btm );
            p.fillRect( r, QBrush( m_selectionColor ) );
         }

         p.end();
      }
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

   hbUpdateLineNumberAreaWidth( 0 );
   lineNumberArea->repaint();
   update();
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
   int iM     = fontMetrics().height() / 2;
   int iMark  = bookMarksGoto.size() > 0 ? ( 5 + iM + 2 ) : 0;
   int space  = iMark + ( width * digits ) + 2;

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
   else
   {
      if( isTipActive )
      {
         ttFrame->move( ttFrame->x(), ttFrame->y() + dy );
      }
   }
}

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbHighlightPage()
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

/*----------------------------------------------------------------------*/

void HBQPlainTextEdit::hbUpdateLineNumberArea( const QRect &rect, int dy )
{
   if( dy )
   {
      lineNumberArea->scroll( 0, dy );

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
   if( m_currentLineColor.isValid() )
      viewport()->update();

   if( styleHightlighter != "none" )
      hbBraceHighlight();
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
   QColor lineColor = QColor( Qt::yellow ).lighter( 160 );

   QTextDocument *doc = document();

   extraSelections.clear();
   setExtraSelections( extraSelections );
   selection.format.setBackground( lineColor );

   QTextCursor cursor = textCursor();

   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor );
   QString brace = cursor.selectedText();

   if(    ( brace != "{" ) && ( brace != "}" )
       && ( brace != "[" ) && ( brace != "]" )
       && ( brace != "(" ) && ( brace != ")" )
       && ( brace != "<" ) && ( brace != ">" ) )
   {
      return;
   }

   QString openBrace;
   QString closeBrace;

   if( ( brace == "{" ) || ( brace == "}" ) )
   {
      openBrace = "{";
      closeBrace = "}";
   }
   if( ( brace == "[" ) || ( brace == "]" ) )
   {
      openBrace = "[";
      closeBrace = "]";
   }
   if( ( brace == "(" ) || ( brace == ")" ) )
   {
      openBrace = "(";
      closeBrace = ")";
   }
   if( ( brace == "<" ) || ( brace == ">" ) )
   {
      openBrace = "<";
      closeBrace = ">";
   }

   QTextCursor cursor1;
   QTextCursor cursor2;
   QTextCursor matches;

   if( brace == openBrace )
   {
      cursor1 = doc->find( closeBrace, cursor );
      cursor2 = doc->find( openBrace, cursor );
      if( cursor2.isNull() )
      {
         matches = cursor1;
      }
      else
      {
         while( cursor1.position() > cursor2.position() )
         {
            cursor1 = doc->find( closeBrace, cursor1 );
            cursor2 = doc->find( openBrace, cursor2 );
            if( cursor2.isNull() )
                break;
         }
         matches = cursor1;
      }
   }
   else
   {
      if( brace == closeBrace )
      {
         cursor1 = doc->find( openBrace, cursor, QTextDocument::FindBackward );
         cursor2 = doc->find( closeBrace, cursor, QTextDocument::FindBackward );
         if( cursor2.isNull() )
         {
            matches = cursor1;
         }
         else
         {
            while( cursor1.position() < cursor2.position() )
            {
               cursor1 = doc->find( openBrace, cursor1, QTextDocument::FindBackward );
               cursor2 = doc->find( closeBrace, cursor2, QTextDocument::FindBackward );
               if( cursor2.isNull() )
                   break;
            }
            matches = cursor1;
         }
      }
   }
   if( ! matches.isNull() )
   {
      if( m_matchBracesAll )
      {
         selection.cursor = cursor;
         extraSelections.append( selection );
      }
      selection.cursor = cursor1;
      extraSelections.append( selection );
      setExtraSelections( extraSelections );
   }
}

/*----------------------------------------------------------------------*/
#endif
