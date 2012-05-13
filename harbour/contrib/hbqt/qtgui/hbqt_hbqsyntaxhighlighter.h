/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#ifndef HBQSYNTAXHIGHLIGHTER_H
#define HBQSYNTAXHIGHLIGHTER_H

#include "hbqt.h"

/*----------------------------------------------------------------------*/

#include <QtGui/QSyntaxHighlighter>
#include <QtGui/QTextBlockUserData>
#include <QtGui/QPlainTextEdit>

class QTextDocument;
class HBQPlainTextEdit;

class HBQTextBlockUserData : public QTextBlockUserData
{

public:
   HBQTextBlockUserData();
   ~HBQTextBlockUserData();

   HBQTextBlockUserData * data( const QTextBlock& block );

   int hbSetState( int state );
   int hbState();

   int state;
};

/*----------------------------------------------------------------------*/

class HBQSyntaxHighlighter : public QSyntaxHighlighter
{
   Q_OBJECT

public:
   HBQSyntaxHighlighter( QTextDocument *parent = 0 );

   HBQPlainTextEdit * editor;
   bool initialized;
   int  type;

   void hbSetMultiLineCommentFormat( const QTextCharFormat & format );
   void hbSetSingleLineCommentFormat( const QTextCharFormat & format );
   void hbSetRule( QString name, QString pattern, const QTextCharFormat & format );
   void hbSetFormat( QString name, const QTextCharFormat & format );
   void hbSetFormatColumnSelection( int start, int count, const QColor & color );
   void hbSetRuleWithRegExp( QString name, const QRegExp & reg, const QTextCharFormat & format );
   void hbSetEditor( HBQPlainTextEdit * edit ){ editor = edit; };
   void hbSetEditor( QPlainTextEdit * edit ){ editor = ( HBQPlainTextEdit * ) edit; };
   void hbSetInitialized( bool bInit ){ initialized = bInit; };
   void hbSetType( int ty ){ type = ty; };

protected:
   void highlightBlock( const QString &text );

   struct HighlightingRule
   {
      QRegExp pattern;
      QTextCharFormat format;

      HighlightingRule() {;}

      HighlightingRule( QRegExp _pattern, const QTextCharFormat & _format )
      {
         pattern = _pattern;
         format = _format;
      }
   };
   QMap< QString, HighlightingRule > HighlightingRules;

protected:

   QRegExp commentStartExpression;
   QRegExp commentEndExpression;
   QRegExp commentSingleLine;
   QRegExp patternQuotation;

   QTextCharFormat keywordFormat;
   QTextCharFormat classFormat;
   QTextCharFormat singleLineCommentFormat;
   QTextCharFormat multiLineCommentFormat;
   QTextCharFormat quotationFormat;
   QTextCharFormat functionFormat;
   QTextCharFormat directivesFormat;

   QTextCharFormat entryHeaderFormat;
   QTextCharFormat entryTitleFormat;
   QTextCharFormat entrySourceFormat;
   QTextCharFormat entryFixedFormat;
   QTextCharFormat entryChangedFormat;
   QTextCharFormat entryOptimizedFormat;
   QTextCharFormat entryAddedFormat;
   QTextCharFormat entryRemovedFormat;
   QTextCharFormat entryCommentFormat;
   QTextCharFormat entryTodoFormat;
   QTextCharFormat entryMovedFormat;

   QRegExp entryHeaderRegExp;
   QRegExp entryTitleRegExp;
   QRegExp entrySourceRegExp;
   QRegExp entryFixedRegExp;
   QRegExp entryChangedRegExp;
   QRegExp entryOptimizedRegExp;
   QRegExp entryAddedRegExp;
   QRegExp entryRemovedRegExp;
   QRegExp entryCommentRegExp;
   QRegExp entryTodoRegExp;
   QRegExp entryMovedRegExp;

   QRegExp isEntry;

};

/*----------------------------------------------------------------------*/
/*
class Highlighter : public QSyntaxHighlighter
{    Q_OBJECT

   public:
      Highlighter(QTextDocument *parent = 0);
      void SetRule(QString name,QString pattern,QTextCharFormat format);

   protected:
      void highlightBlock(const QString &text);
      struct HighlightingRule
      {
         HighlightingRule() {}
         HighlightingRule(QRegExp _pattern,QTextCharFormat _format) {pattern = _pattern;format = _format;}
         QRegExp pattern;
         QTextCharFormat format;
      };
      QMap<QString,HighlightingRule> highlightingRules;
};

class MultiLineCommentHighlighter : public Highlighter
{     Q_OBJECT
   public:
      MultiLineCommentHighlighter(QTextDocument *parent = 0);
   protected:
      void highlightBlock(const QString &text);
      QRegExp commentStartExpression;
      QRegExp commentEndExpression;
      QTextCharFormat multiLineCommentFormat;
};

class CppHighlighter : public MultiLineCommentHighlighter
{     Q_OBJECT
   public:
      CppHighlighter(QTextDocument *parent = 0);
};    l
*/
#endif
