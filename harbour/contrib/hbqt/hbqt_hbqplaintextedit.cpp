/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour-Qt wrapper generator.
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *  Copyright( C ) 2009 by Gancov Kostya < kossne@mail.ru >
 *
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbthread.h"
#include "hbvm.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqplaintextedit.h"


HBQPlainTextEdit::HBQPlainTextEdit( QWidget * parent ) : QPlainTextEdit( parent )
{
   spacesTab         = "";
   styleHightlighter = "prg";
   numberBlock       = true;
   lineNumberArea    = new LineNumberArea( this );

   connect( this, SIGNAL( blockCountChanged( int ) )           , this, SLOT( updateLineNumberAreaWidth( int ) ) );
   connect( this, SIGNAL( updateRequest( const QRect &, int ) ), this, SLOT( updateLineNumberArea( const QRect &, int ) ) );

   updateLineNumberAreaWidth( 0 );

   connect( this, SIGNAL( cursorPositionChanged() )            , this, SLOT( slotCursorPositionChanged() ) );

   m_currentLineColor.setNamedColor( "#e8e8ff" );
}

HBQPlainTextEdit::~HBQPlainTextEdit()
{
   disconnect( this, SIGNAL( blockCountChanged( int ) )            );
   disconnect( this, SIGNAL( updateRequest( const QRect &, int ) ) );
   disconnect( this, SIGNAL( cursorPositionChanged() )             );

   delete lineNumberArea;
}

void HBQPlainTextEdit::paintEvent( QPaintEvent * event )
{
   QPainter painter( viewport() );

   if( m_currentLineColor.isValid() )
   {
      QRect r = HBQPlainTextEdit::cursorRect();
      r.setX( 0 );
      r.setWidth( viewport()->width() );

      int index = bookMark.indexOf( textCursor().blockNumber() + 1 );
      if( index != -1 )
      {
         QBrush br = QBrush( m_currentLineColor );

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

         painter.fillRect( r, br );
      }
      else if( highlightCurLine == true )
      {
         painter.fillRect( r, QBrush( m_currentLineColor ) );
      }
   }
   painter.end();
   QPlainTextEdit::paintEvent( event );
}

void HBQPlainTextEdit::lineNumberAreaPaintEvent( QPaintEvent *event )
{
   QPainter painter( lineNumberArea );
   painter.fillRect( event->rect(), QColor( "#e4e4e4" ) );

   QTextBlock block = firstVisibleBlock();
   int blockNumber = block.blockNumber();
   int top = ( int ) blockBoundingGeometry( block ).translated( contentOffset() ).top();
   int bottom = top +( int ) blockBoundingRect( block ).height();

   while( block.isValid() && top <= event->rect().bottom() )
   {
      if( block.isVisible() && bottom >= event->rect().top() )
      {
         QString number = QString::number( blockNumber + 1 );
         painter.setPen( (  blockNumber + 1 ) % 10 == 0 ? Qt::red : Qt::black );
         painter.drawText( 0, top, lineNumberArea->width()-2, fontMetrics().height(), Qt::AlignRight, number );
         int index = bookMark.indexOf( number.toInt() );
         if( index != -1 )
         {
            //painter.drawText( 0, top, 30, fontMetrics().height(), Qt::AlignCenter, "+" );
            painter.setBrush( QBrush( Qt::yellow, Qt::SolidPattern ) );
            painter.drawEllipse( 5, top +( fontMetrics().height()/4 ),
                                       fontMetrics().height()/2, fontMetrics().height()/2 );
         }
      }
      block = block.next();
      top = bottom;
      bottom = top +( int ) blockBoundingRect( block ).height();
      ++blockNumber;
   }
}
#if 0
void HBQPlainTextEdit::contextMenuEvent( QContextMenuEvent *event )
{
   QWidget::contextMenuEvent( event );
}

void HBQPlainTextEdit::keyPressEvent( QKeyEvent *event )
{
   QPlainTextEdit::keyPressEvent( event );
}
#endif
void HBQPlainTextEdit::resizeEvent( QResizeEvent *e )
{
   QPlainTextEdit::resizeEvent( e );

   QRect cr = contentsRect();
   lineNumberArea->setGeometry( QRect( cr.left(), cr.top(), lineNumberAreaWidth(), cr.height() ) );
}


void HBQPlainTextEdit::bookmarks( int block )
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

void HBQPlainTextEdit::gotoBookmark( int block )
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

void HBQPlainTextEdit::nextBookmark( int block )
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

void HBQPlainTextEdit::prevBookmark( int block )
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

int HBQPlainTextEdit::lineNumberAreaWidth()
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

void HBQPlainTextEdit::updateLineNumberAreaWidth( int )
{
   setViewportMargins( lineNumberAreaWidth(), 0, 0, 0 );
}

void HBQPlainTextEdit::updateLineNumberArea( const QRect &rect, int dy )
{
   if( dy )
      lineNumberArea->scroll( 0, dy );
   else
      lineNumberArea->update( 0, rect.y(), lineNumberArea->width(), rect.height() );

   if( rect.contains( viewport()->rect() ) )
      updateLineNumberAreaWidth( 0 );
}

void HBQPlainTextEdit::setSpaces( int newSpaces )
{
   spaces = newSpaces;
   spacesTab = "";

   if( spaces > 0 )
   {
      for( int i = 0; i < spaces; ++i )
      {
          spacesTab += " ";
      }
   }
   else
   {
      if( spaces == -101 )
         spacesTab = "\t";
   }
}

bool HBQPlainTextEdit::event( QEvent *event )
{
   if( event->type() == QEvent::KeyPress )
   {
      QKeyEvent *keyEvent =( QKeyEvent * )event;
      if( ( keyEvent->key() == Qt::Key_Tab ) && ( keyEvent->modifiers() & Qt::ControlModifier ) )
      {
         return false;
      }
      else
      {
         if( ( keyEvent->key() == Qt::Key_Tab ) && !( keyEvent->modifiers() & Qt::ControlModifier & Qt::AltModifier & Qt::ShiftModifier ) ) {
            this->insertPlainText( spacesTab );
            return true;
         }
      }
   }
   return QPlainTextEdit::event( event );
}

int HBQPlainTextEdit::getIndex( const QTextCursor &crQTextCursor )
{
   QTextBlock b;
   int column = 1;
   b = crQTextCursor.block();
   column = crQTextCursor.position() - b.position();
   return column;
}

int HBQPlainTextEdit::getLine( const QTextCursor &crQTextCursor )
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
   return( line );
}

void HBQPlainTextEdit::slotCursorPositionChanged()
{
   if( m_currentLineColor.isValid() )
      viewport()->update();

   if( styleHightlighter != "none" )
   {
      braceHighlight();
   }
}

void HBQPlainTextEdit::braceHighlight()
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
      {
         return;
      }
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
            {
                break;
            }
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
      if( brace == closeBrace ) {
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
               {
                   break;
               }
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

void HBQPlainTextEdit::setStyleHightlighter( const QString &style )
{
   styleHightlighter = style;
}

void HBQPlainTextEdit::showHighlighter( const QString &style, bool b )
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

void HBQPlainTextEdit::caseUpper()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.toUpper();
   insertPlainText( txt );
}

void HBQPlainTextEdit::caseLower()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.toLower();
   insertPlainText( txt );
}

void HBQPlainTextEdit::replaceSelection( const QString & txt )
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString text = selTxt.toCaseFolded();
   insertPlainText( text );
}

void HBQPlainTextEdit::escapeQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.replace( QString( "'" ), QString( "\\\'" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::escapeDQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.replace( QString( "\"" ), QString( "\\\"" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::unescapeQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.replace( QString( "\\\'" ), QString( "'" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::unescapeDQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.replace( QString( "\\\"" ), QString( "\"" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::convertQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.replace( QString( "\"" ), QString( "\'" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::convertDQuotes()
{
   QTextCursor cursor( textCursor() );
   QString selTxt( cursor.selectedText() );
   if( selTxt.isEmpty() )
   {
      return;
   }
   QString txt = selTxt.replace( QString( "\'" ), QString( "\"" ) );
   insertPlainText( txt );
}

void HBQPlainTextEdit::blockComment()
{
   QTextCursor cursor = textCursor();
   QTextCursor c = cursor;
   cursor.movePosition( QTextCursor::StartOfLine );
   setTextCursor( cursor );
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
      insertPlainText( "//" );
   }
   setTextCursor( c );
}

void HBQPlainTextEdit::streamComment()
{
   QTextCursor cursor = textCursor();
   QString textUnderCursor = cursor.selectedText();
   if( textUnderCursor.isEmpty() )
      return;
   insertPlainText( "/*" + textUnderCursor + "*/" );
}

void HBQPlainTextEdit::duplicateLine()
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

#endif
