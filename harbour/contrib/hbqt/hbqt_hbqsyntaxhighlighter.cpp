/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_slots.h"

HBQSyntaxHighlighter::HBQSyntaxHighlighter( QTextDocument *parent )
   : QSyntaxHighlighter( parent )
{
   HighlightingRule rule;

   keywordFormat.setForeground( Qt::darkBlue );
   keywordFormat.setFontWeight( QFont::Bold );
   QStringList keywordPatterns;
   keywordPatterns << "\\bchar\\b" << "\\bclass\\b" << "\\bconst\\b"
                   << "\\bdouble\\b" << "\\benum\\b" << "\\bexplicit\\b"
                   << "\\bfriend\\b" << "\\binline\\b" << "\\bint\\b"
                   << "\\blong\\b" << "\\bnamespace\\b" << "\\boperator\\b"
                   << "\\bprivate\\b" << "\\bprotected\\b" << "\\bpublic\\b"
                   << "\\bshort\\b" << "\\bsignals\\b" << "\\bsigned\\b"
                   << "\\bslots\\b" << "\\bstatic\\b" << "\\bstruct\\b"
                   << "\\btemplate\\b" << "\\btypedef\\b" << "\\btypename\\b"
                   << "\\bunion\\b" << "\\bunsigned\\b" << "\\bvirtual\\b"
                   << "\\bvoid\\b" << "\\bvolatile\\b";
   foreach ( const QString &pattern, keywordPatterns ) {
       rule.pattern = QRegExp( pattern );
       rule.format = keywordFormat;
       highlightingRules.append( rule );
   }

   classFormat.setFontWeight( QFont::Bold );
   classFormat.setForeground( Qt::darkMagenta );
   rule.pattern = QRegExp( "\\bQ[A-Za-z]+\\b" );
   rule.format = classFormat;
   highlightingRules.append( rule );

   singleLineCommentFormat.setForeground( Qt::red );
   rule.pattern = QRegExp( "//[^\n]*" );
   rule.format = singleLineCommentFormat;
   highlightingRules.append( rule );

   multiLineCommentFormat.setForeground( Qt::red );

   quotationFormat.setForeground( Qt::darkGreen );
   rule.pattern = QRegExp( "\".*\"" );
   rule.format = quotationFormat;
   highlightingRules.append( rule );

   functionFormat.setFontItalic( true );
   functionFormat.setForeground( Qt::blue );
   rule.pattern = QRegExp( "\\b[A-Za-z0-9_]+(?=\\()" );
   rule.format = functionFormat;
   highlightingRules.append( rule );

   commentStartExpression = QRegExp("/\\*");
   commentEndExpression = QRegExp("\\*/");
}

void HBQSyntaxHighlighter::highlightBlock( const QString &text )
{
   foreach ( const HighlightingRule &rule, highlightingRules ) {
      QRegExp expression( rule.pattern );
      int index = expression.indexIn( text );
      while ( index >= 0 ) {
         int length = expression.matchedLength();
         setFormat( index, length, rule.format );
         index = expression.indexIn( text, index + length );
      }
   }
   setCurrentBlockState( 0 );

   int startIndex = 0;
   if ( previousBlockState() != 1 )
      startIndex = commentStartExpression.indexIn( text );

   while ( startIndex >= 0 ) {
      int endIndex = commentEndExpression.indexIn( text, startIndex );
      int commentLength;
      if ( endIndex == -1 ) {
          setCurrentBlockState( 1 );
          commentLength = text.length() - startIndex;
      } else {
          commentLength = endIndex - startIndex
                          + commentEndExpression.matchedLength();
      }
      setFormat( startIndex, commentLength, multiLineCommentFormat );
      startIndex = commentStartExpression.indexIn( text, startIndex + commentLength );
   }
}

#endif
