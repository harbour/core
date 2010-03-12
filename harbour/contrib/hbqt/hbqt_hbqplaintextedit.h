/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#ifndef HBQT_HBQPLAINTEXTEDIT_H
   #define HBQT_HBQPLAINTEXTEDIT_H

#include "hbapiitm.h"

#include <QPointer>

#include <QtGui/QPlainTextEdit>
#include <QtCore>
#include <QKeyEvent>
#include <QTextBlock>
#include <QPainter>
#include <QMessageBox>
#include <QCompleter>
#include <QAbstractItemView>
#include <QScrollBar>
#include <QToolTip>

#include "hbqt_hbqsyntaxhighlighter.h"

class LineNumberArea;

class HBQPlainTextEdit : public QPlainTextEdit
{
   Q_OBJECT

public:
   HBQPlainTextEdit( QWidget * parent = 0 );
   ~HBQPlainTextEdit();

   PHB_ITEM       block;
   QColor         m_currentLineColor;
   QColor         m_lineAreaBkColor;
   long           m_matchingBegin;
   long           m_matchingEnd;

   void           paintEvent( QPaintEvent * event );
   void           lineNumberAreaPaintEvent( QPaintEvent * event );

   HBQSyntaxHighlighter * highlighter;

   QString        styleHightlighter;
   void           hbSetStyleHightlighter( const QString & style );
   QString        hbGetStyleHightlighter()         { return styleHightlighter; }
   void           hbShowHighlighter( const QString & style, bool b );

   int            hbGetIndex( const QTextCursor &crQTextCursor );
   int            hbGetLine( const QTextCursor &crQTextCursor );
   int            hbLineNumberAreaWidth();
   int            hbGetSpaces()                    { return spaces; }
   void           hbSetSpaces( int newSpaces );
   void           hbBookmarks( int block );
   void           hbNextBookmark( int block );
   void           hbPrevBookmark( int block );
   void           hbGotoBookmark( int block );
   void           hbHighlightCurrentLine( bool b ) { highlightCurLine = b; }
   bool           hbHighlightCurrentLine()         { return highlightCurLine; }
   void           hbSetEventBlock( PHB_ITEM pBlock );

private:
   QVector<int>   bookMark;
   QList<int>     bookMarksGoto;
   QWidget      * lineNumberArea;
   int            spaces;
   bool           numberBlock;
   bool           highlightCurLine;
   QString        spacesTab;
   int            posOpen;
   int            posClose;
   QList<QTextEdit::ExtraSelection> extraSelections;
   QTextEdit::ExtraSelection selection;
   void           hbBraceHighlight();

   int            columnBegins;
   int            columnEnds;
   bool           isColumnSelectionEnabled;
   bool           isTipActive;
   QCompleter   * c;
   QBrush         brushForBookmark( int index );

protected:
   bool           event( QEvent * event );
   void           resizeEvent( QResizeEvent * event );
   void           mouseDoubleClickEvent( QMouseEvent * event );
   void           focusInEvent( QFocusEvent * event );
   void           keyPressEvent( QKeyEvent * event );

public slots:
   void           hbUpdateLineNumberAreaWidth( int newBlockCount );
   void           hbCaseUpper();
   void           hbCaseLower();
   void           hbEscapeQuotes();
   void           hbEscapeDQuotes();
   void           hbUnescapeQuotes();
   void           hbUnescapeDQuotes();
   void           hbConvertQuotes();
   void           hbConvertDQuotes();
   void           hbDeleteLine();
   void           hbMoveLine( int iDirection );
   void           hbBlockIndent( int steps );
   void           hbBlockComment();
   void           hbStreamComment();
   void           hbDuplicateLine();
   void           hbReplaceSelection( const QString & txt );
   void           hbInsertTab( int mode );
   void           hbHighlightSelectedColumns( bool yes );
   QString        hbGetSelectedText();
   QString        hbTextUnderCursor();
   void           hbNumberBlockVisible( bool b );
   bool           hbNumberBlockVisible();
   void           hbShowPrototype( const QString & tip );
   void           hbSetCompleter( QCompleter * completer ) { c = completer; };
   void           hbSetCurrentLineColor( const QColor & color ) { m_currentLineColor = color; };
   void           hbSetLineAreaBkColor( const QColor & color ) { m_lineAreaBkColor = color; };
   void           hbRefresh();
private slots:
   void           hbSlotCursorPositionChanged();
   void           hbUpdateLineNumberArea( const QRect &, int );
   void           hbPaintColumnSelection( QPaintEvent * );
};


class LineNumberArea : public QWidget
{
public:
   LineNumberArea( HBQPlainTextEdit * editor = 0 ) : QWidget( editor )
   {
       codeEditor = editor;
   }

protected:
   QSize sizeHint() const
   {
       return QSize( codeEditor->hbLineNumberAreaWidth(), 0 );
   }

private:
   void paintEvent( QPaintEvent *event )
   {
       codeEditor->lineNumberAreaPaintEvent( event );
   }

   HBQPlainTextEdit *codeEditor;
};

#endif

