/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour-Qt wrapper generator.
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
 * Copyright 2009 Gancov Kostya <kossne@mail.ru>
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
#include "hbthread.h"
#include "hbvm.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqplaintextedit.h"

HBQPlainTextEdit::HBQPlainTextEdit( QWidget * parent ) : QPlainTextEdit( parent )
{
   spaces                   = 3;
   spacesTab                = "";
   styleHightlighter        = "prg";
   numberBlock              = true;
   lineNumberArea           = new LineNumberArea( this );
   isTipActive              = false;
   columnBegins             = -1;
   columnEnds               = -1;
   isColumnSelectionEnabled = false;

   connect( this, SIGNAL( blockCountChanged( int ) )           , this, SLOT( hbUpdateLineNumberAreaWidth( int ) ) );
   connect( this, SIGNAL( updateRequest( const QRect &, int ) ), this, SLOT( hbUpdateLineNumberArea( const QRect &, int ) ) );

   hbUpdateLineNumberAreaWidth( 0 );

   connect( this, SIGNAL( cursorPositionChanged() )            , this, SLOT( hbSlotCursorPositionChanged() ) );

   m_currentLineColor.setNamedColor( "#e8e8ff" );
   m_lineAreaBkColor.setNamedColor( "#e4e4e4" );
}

HBQPlainTextEdit::~HBQPlainTextEdit()
{
   if( block )
      hb_itemRelease( block );

   disconnect( this, SIGNAL( blockCountChanged( int ) )            );
   disconnect( this, SIGNAL( updateRequest( const QRect &, int ) ) );
   disconnect( this, SIGNAL( cursorPositionChanged() )             );

   delete lineNumberArea;
}

void HBQPlainTextEdit::hbSetEventBlock( PHB_ITEM pBlock )
{
   if( pBlock )
      block = hb_itemNew( pBlock );
}

void HBQPlainTextEdit::hbRefresh()
{
   update();
}

void HBQPlainTextEdit::hbShowPrototype( const QString & tip )
{
   if( tip == ( QString ) "" )
   {
      QToolTip::hideText();
      isTipActive = false;
   }
   else
   {
      QRect r = HBQPlainTextEdit::cursorRect();
      QToolTip::showText( mapToGlobal( QPoint( r.x(), r.y() ) ), tip );
      isTipActive = true;
   }
}

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

void HBQPlainTextEdit::keyPressEvent( QKeyEvent * event )
{
   if( c && c->popup()->isVisible() )
   {
      // The following keys are forwarded by the completer to the widget
      switch( event->key() )
      {
      case Qt::Key_Enter:
      case Qt::Key_Return:
      case Qt::Key_Escape:
      case Qt::Key_Tab:
      case Qt::Key_Backtab:
         event->ignore();
         return;                                    // let the completer do default behavior
      default:
         break;
      }
   }

   QPlainTextEdit::keyPressEvent( event );

   const bool ctrlOrShift = event->modifiers() & (Qt::ControlModifier | Qt::ShiftModifier);
   if( !c || ( ctrlOrShift && event->text().isEmpty() ) )
       return;

   static  QString            eow( "~!@#$%^&*()+{}|:\"<>?,./;'[]\\-=" );               /* end of word */
   bool    hasModifier      = ( event->modifiers() != Qt::NoModifier ) && !ctrlOrShift;
   QString completionPrefix = hbTextUnderCursor();

   if( ( hasModifier || event->text().isEmpty() || completionPrefix.length() < 3
                                                || eow.contains( event->text().right( 1 ) ) ) )
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
   cr.setWidth( c->popup()->sizeHintForColumn( 0 ) + c->popup()->verticalScrollBar()->sizeHint().width() );
   c->complete( cr ); // popup it up!
}

QString HBQPlainTextEdit::hbTextUnderCursor()
{
   QTextCursor tc = textCursor();
   tc.select( QTextCursor::WordUnderCursor );
   return tc.selectedText();
}

void HBQPlainTextEdit::resizeEvent( QResizeEvent *e )
{
   QPlainTextEdit::resizeEvent( e );

   QRect cr = contentsRect();
   lineNumberArea->setGeometry( QRect( cr.left(), cr.top(), hbLineNumberAreaWidth(), cr.height() ) );
}

void HBQPlainTextEdit::focusInEvent( QFocusEvent * event )
{
   if( c )
      c->setWidget( this );

   QPlainTextEdit::focusInEvent( event );
}

void HBQPlainTextEdit::mouseDoubleClickEvent( QMouseEvent *event )
{
   HB_TRACE( HB_TR_ALWAYS, ( "void HBQPlainTextEdit::mouseDblClickEvent( QMouseEvent * %p )", event ) );
   if( block )
   {
      PHB_ITEM p1 = hb_itemPutNI( NULL, QEvent::MouseButtonDblClick );
      hb_vmEvalBlockV( block, 1, p1 );
      hb_itemRelease( p1 );
   }
   QPlainTextEdit::mouseDoubleClickEvent( event );
}

void HBQPlainTextEdit::paintEvent( QPaintEvent * event )
{
   QPainter painter( viewport() );

   int curBlock     = textCursor().blockNumber();

   QTextBlock block = firstVisibleBlock();
   int blockNumber  = block.blockNumber();
   int height       = ( int ) blockBoundingRect( block ).height();
   int top          = ( int ) blockBoundingGeometry( block ).translated( contentOffset() ).top();
   int bottom       = top + height;

   while( block.isValid() && top <= event->rect().bottom() )
   {
      if( block.isVisible() && bottom >= event->rect().top() )
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
      block  = block.next();
      top    = bottom;
      bottom = top + height;//( int ) blockBoundingRect( block ).height();
      ++blockNumber;
   }
   this->hbPaintColumnSelection( event );
   painter.end();
   QPlainTextEdit::paintEvent( event );
}

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

void HBQPlainTextEdit::lineNumberAreaPaintEvent( QPaintEvent *event )
{
   QPainter painter( lineNumberArea );
   painter.fillRect( event->rect(), m_lineAreaBkColor );

   QTextBlock block = firstVisibleBlock();
   int blockNumber  = block.blockNumber();
   int top          = ( int ) blockBoundingGeometry( block ).translated( contentOffset() ).top();
   int bottom       = top +( int ) blockBoundingRect( block ).height();

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
            //painter.drawText( 0, top, 30, fontMetrics().height(), Qt::AlignCenter, "+" );
            //painter.setBrush( QBrush( Qt::yellow, Qt::SolidPattern ) );
            painter.setBrush( brushForBookmark( index ) );
            #if 0
            painter.drawEllipse( 5, top + ( fontMetrics().height()/4 ),
                                       fontMetrics().height()/2, fontMetrics().height()/2 );
            #endif
            painter.drawRect( 5, top+2, fontMetrics().height()-4, fontMetrics().height()-4 );
         }
      }
      block  = block.next();
      top    = bottom;
      bottom = top +( int ) blockBoundingRect( block ).height();
      ++blockNumber;
   }
}

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
   lineNumberArea->repaint();
   update();
}

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

bool HBQPlainTextEdit::hbNumberBlockVisible()
{
   return numberBlock;
}

int HBQPlainTextEdit::hbLineNumberAreaWidth()
{
   int digits = 1;
   int max = qMax( 1, blockCount() );
   while( max >= 10 )
   {
      max /= 10;
      ++digits;
   }
   int space = ( 18 + fontMetrics().width( QLatin1Char( '9' ) ) * digits ) + 2;
   return space;
}

void HBQPlainTextEdit::hbUpdateLineNumberAreaWidth( int )
{
   if( numberBlock )
   {
      setViewportMargins( hbLineNumberAreaWidth(), 0, 0, 0 );
   }
   else
   {
      setViewportMargins( 0, 0, 0, 0 );
   }
}

void HBQPlainTextEdit::hbUpdateLineNumberArea( const QRect &rect, int dy )
{
   if( dy )
      lineNumberArea->scroll( 0, dy );
   else
      lineNumberArea->update( 0, rect.y(), lineNumberArea->width(), rect.height() );

   if( rect.contains( viewport()->rect() ) )
   {
      hbUpdateLineNumberAreaWidth( 0 );
   }
}

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

int HBQPlainTextEdit::hbGetIndex( const QTextCursor &crQTextCursor )
{
   QTextBlock b;
   int column = 1;
   b = crQTextCursor.block();
   column = crQTextCursor.position() - b.position();
   return column;
}

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

void HBQPlainTextEdit::hbSlotCursorPositionChanged()
{
   if( m_currentLineColor.isValid() )
      viewport()->update();

   if( styleHightlighter != "none" )
      hbBraceHighlight();
}

void HBQPlainTextEdit::hbSetStyleHightlighter( const QString &style )
{
   styleHightlighter = style;
}

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

void HBQPlainTextEdit::hbEscapeQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "'" ), QString( "\\\'" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::hbEscapeDQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "\"" ), QString( "\\\"" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::hbUnescapeQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "\\\'" ), QString( "'" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::hbUnescapeDQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return;

   QString txt = selTxt.replace( QString( "\\\"" ), QString( "\"" ) );
   insertPlainText( txt );
}

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

QString HBQPlainTextEdit::hbGetSelectedText()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
      return "";

   QString txt = selTxt.replace( 0x2029, QString( "\n" ) );
   return txt;
}

void HBQPlainTextEdit::hbPaintColumnSelection( QPaintEvent * event )
{
   QTextCursor c = textCursor();

   HB_SYMBOL_UNUSED( event );

   if( isColumnSelectionEnabled && c.hasSelection() )
   {
      if( columnBegins >= 0 && columnEnds >= 0 )
      {
         QTextBlock b = c.document()->findBlock( c.selectionStart() );
         QTextBlock e = c.document()->findBlock( c.selectionEnd() );

         if( b.isVisible() || e.isVisible() )
         {
            if( b.isVisible() )
            {
               QPainter p( viewport() );

               if( b.isValid() && b.isVisible() )
               {
                  int fontWidth = fontMetrics().averageCharWidth();

                  int x = ( columnBegins + 1 ) * fontWidth;
                  int w = ( c.columnNumber() - columnBegins ) * fontWidth;
                  QRect r( x, 0, w, viewport()->height() );

                  p.fillRect( r, QBrush( QColor( 175, 255, 175 ) ) );
               }
            }
         }
      }
   }
}

void HBQPlainTextEdit::hbHighlightSelectedColumns( bool yes )
{
   if( yes )
   {
      isColumnSelectionEnabled = true;

      QTextCursor c = textCursor();
      if( columnBegins == -1 && c.hasSelection() )
      {
         int b        = c.document()->findBlock( c.selectionStart() ).position();
         columnBegins = c.selectionStart() - b;
         columnEnds   = c.columnNumber();
      }
   }
   else
   {
      columnBegins = -1;
      columnEnds   = -1;
      isColumnSelectionEnabled = false;
   }
   update();
}

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

void HBQPlainTextEdit::hbBraceHighlight()
{
   extraSelections.clear();
   setExtraSelections( extraSelections );
   QColor lineColor = QColor( Qt::yellow ).lighter( 160 );
   selection.format.setBackground( lineColor );

   QTextDocument *doc = document();
   QTextCursor cursor = textCursor();
   QTextCursor beforeCursor = cursor;

   cursor.movePosition( QTextCursor::NextCharacter, QTextCursor::KeepAnchor );
   QString brace = cursor.selectedText();

   beforeCursor.movePosition( QTextCursor::PreviousCharacter, QTextCursor::KeepAnchor );
   QString beforeBrace = beforeCursor.selectedText();

   if(    ( brace != "{" ) && ( brace != "}" )
       && ( brace != "[" ) && ( brace != "]" )
       && ( brace != "(" ) && ( brace != ")" )
       && ( brace != "<" ) && ( brace != ">" ) )
   {
      if(    ( beforeBrace == "{" ) || ( beforeBrace == "}" )
          || ( beforeBrace == "[" ) || ( beforeBrace == "]" )
          || ( beforeBrace == "(" ) || ( beforeBrace == ")" )
          || ( beforeBrace == "<" ) || ( beforeBrace == ">" )  )
      {
         cursor = beforeCursor;
         brace = cursor.selectedText();
      }
      else
         return;
   }

   QTextCharFormat format;
   format.setForeground( Qt::red );
   format.setFontWeight( QFont::Bold );

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

   if( brace == openBrace )
   {
      QTextCursor cursor1 = doc->find( closeBrace, cursor );
      QTextCursor cursor2 = doc->find( openBrace, cursor );
      if( cursor2.isNull() )
      {
         selection.cursor = cursor;
         extraSelections.append( selection );
         selection.cursor = cursor1;
         extraSelections.append( selection );
         setExtraSelections( extraSelections );
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
         selection.cursor = cursor;
         extraSelections.append( selection );
         selection.cursor = cursor1;
         extraSelections.append( selection );
         setExtraSelections( extraSelections );
      }
   }
   else
   {
      if( brace == closeBrace )
      {
         QTextCursor cursor1 = doc->find( openBrace, cursor, QTextDocument::FindBackward );
         QTextCursor cursor2 = doc->find( closeBrace, cursor, QTextDocument::FindBackward );
         if( cursor2.isNull() )
         {
            selection.cursor = cursor;
            extraSelections.append( selection );
            selection.cursor = cursor1;
            extraSelections.append( selection );
            setExtraSelections( extraSelections );
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
            selection.cursor = cursor;
            extraSelections.append( selection );
            selection.cursor = cursor1;
            extraSelections.append( selection );
            setExtraSelections( extraSelections );
         }
      }
   }
}

#endif
