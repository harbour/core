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

#include "hbqt_hbqsyntaxhighlighter.h"

class LineNumberArea;

class HBQPlainTextEdit : public QPlainTextEdit
{
   Q_OBJECT

public:
   HBQPlainTextEdit( QWidget * parent = 0 );
   ~HBQPlainTextEdit();

   QColor         m_currentLineColor;
   long           m_matchingBegin;
   long           m_matchingEnd;

   void           paintEvent( QPaintEvent * event );
   void           lineNumberAreaPaintEvent( QPaintEvent * event );

   HBQSyntaxHighlighter * highlighter;

   QString        styleHightlighter;
   void           setStyleHightlighter( const QString & style );
   QString        getStyleHightlighter()         { return styleHightlighter; }
   void           showHighlighter( const QString & style, bool b );

   int            getIndex( const QTextCursor &crQTextCursor );
   int            getLine( const QTextCursor &crQTextCursor );
   int            lineNumberAreaWidth();
   int            getSpaces()                    { return spaces; }
   void           setSpaces( int newSpaces );
   void           bookmarks( int block );
   void           nextBookmark( int block );
   void           prevBookmark( int block );
   void           gotoBookmark( int block );
   void           numberBlockVisible( bool b )   { numberBlock = b; }
   bool           numberBlockVisible()           { return numberBlock; }
   void           highlightCurrentLine( bool b ) { highlightCurLine = b; }
   bool           highlightCurrentLine()         { return highlightCurLine; }

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
   void           braceHighlight();

protected:
   bool           event( QEvent * event );
   void           resizeEvent( QResizeEvent * event );
   #if 0
   void           contextMenuEvent( QContextMenuEvent * event );
   void           keyPressEvent( QKeyEvent * event );
   #endif

public slots:
   void           updateLineNumberAreaWidth( int newBlockCount );
   void           caseUpper();
   void           caseLower();
   void           escapeQuotes();
   void           escapeDQuotes();
   void           unescapeQuotes();
   void           unescapeDQuotes();
   void           convertQuotes();
   void           convertDQuotes();
   void           blockComment();
   void           streamComment();
   void           duplicateLine();
   void           replaceSelection( const QString & txt );

private slots:
   void           slotCursorPositionChanged();
   void           updateLineNumberArea( const QRect &, int );
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
       return QSize( codeEditor->lineNumberAreaWidth(), 0 );
   }

private:
   void paintEvent( QPaintEvent *event )
   {
       codeEditor->lineNumberAreaPaintEvent( event );
   }

   HBQPlainTextEdit *codeEditor;
};

#endif

