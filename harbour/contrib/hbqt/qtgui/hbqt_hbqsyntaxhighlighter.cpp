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

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqsyntaxhighlighter.h"

#include <QtCore/QPointer>
#include <QtCore/QHash>
#include <QtGui/QTextCharFormat>

HBQTextBlockUserData::HBQTextBlockUserData() : QTextBlockUserData()
{
   state = -1;
}
HBQTextBlockUserData::~HBQTextBlockUserData()
{
}
int HBQTextBlockUserData::hbSetState( int istate )
{
   int iCurState = state;
   state = istate;
   return iCurState;
}
int HBQTextBlockUserData::hbState()
{
   return state;
}
HBQTextBlockUserData * HBQTextBlockUserData::data( const QTextBlock& block )
{
   return static_cast<HBQTextBlockUserData *>( block.userData() );
}

/*----------------------------------------------------------------------*/

HBQSyntaxHighlighter::HBQSyntaxHighlighter( QTextDocument * parent )
   : QSyntaxHighlighter( parent )
{
   HighlightingRule rule;
   multiLineCommentFormat.setForeground( Qt::red );

   commentStartExpression = QRegExp( "/\\*" );
   commentEndExpression = QRegExp( "\\*/" );

   commentSingleLine = QRegExp( "//[^\n]*" );

   patternQuotation = QRegExp( "\"[^\"]+\"|\'[^\']+\'" );
}

void HBQSyntaxHighlighter::hbSetRule( QString name, QString pattern, const QTextCharFormat & format )
{
   if( pattern != "" )
      HighlightingRules.insert( name, HighlightingRule( QRegExp( pattern ), format ) );
   else
      HighlightingRules.remove( name );
}
void HBQSyntaxHighlighter::hbSetRuleWithRegExp( QString name, const QRegExp & reg, const QTextCharFormat & format )
{
   HighlightingRules.insert( name, HighlightingRule( reg, format ) );
}
void HBQSyntaxHighlighter::hbSetFormat( QString name, const QTextCharFormat & format )
{
   if( ( QString ) "TerminatedStrings" == name )
   {
      quotationFormat = format;
   }
   else
   {
      if( HighlightingRules.contains( name ) )
      {
         HighlightingRule rule = HighlightingRules.value( name );
         QRegExp reg = rule.pattern;
         HighlightingRules.insert( name, HighlightingRule( reg, format ) );
      }
      else
      {
         HighlightingRules.remove( name );
      }
   }
}
void HBQSyntaxHighlighter::hbSetMultiLineCommentFormat( const QTextCharFormat & format )
{
   multiLineCommentFormat = format;
}
void HBQSyntaxHighlighter::hbSetSingleLineCommentFormat( const QTextCharFormat & format )
{
   singleLineCommentFormat = format;
}

void HBQSyntaxHighlighter::hbSetFormatColumnSelection( int start, int count, const QColor & color )
{
   setFormat( start, count, color );
}

void HBQSyntaxHighlighter::highlightBlock( const QString &text )
{
   #if 0
   QTextBlock curBlock( currentBlock() );
   int iState = -1;
   HBQTextBlockUserData * data = ( HBQTextBlockUserData * ) curBlock.userData();

   QTextBlockFormat fmt( curBlock.blockFormat() );
   if( data )
   {
      iState = data->state;
      HB_TRACE( HB_TR_DEBUG, ( "iState = %i", iState ) );

      switch( iState )
      {
      case 99:
         fmt.setBackground( QColor( 255,255,0 ) );
         break;
      }
   }
   #endif

   int index = 0;

   foreach( const HighlightingRule &rule, HighlightingRules )
   {
      #if 0
      QRegExp expression( rule.pattern );
      index = expression.indexIn( text );
      while( index >= 0 )
      {
         int length = expression.matchedLength();
         setFormat( index, length, rule.format );
         index = expression.indexIn( text, index + length );
      }
      #endif
      index = rule.pattern.indexIn( text );
      while( index >= 0 )
      {
         int length = rule.pattern.matchedLength();
         setFormat( index, length, rule.format );
         index = rule.pattern.indexIn( text, index + length );
      }
   }

   /* Quoted text */
   index = patternQuotation.indexIn( text );
   while( index >= 0 )
   {
      int length = patternQuotation.matchedLength();
      setFormat( index, length, quotationFormat );
      index = patternQuotation.indexIn( text, index + length );
   }

   /* Single Line Comments */
   index = commentSingleLine.indexIn( text );
   while( index >= 0 )
   {
      int length = commentSingleLine.matchedLength();
      setFormat( index, length, singleLineCommentFormat );
      index = commentSingleLine.indexIn( text, index + length );
   }

   setCurrentBlockState( 0 );

   /* Multi Line Comments */
   int startIndex = 0;
   if( previousBlockState() != 1 )
   {
      startIndex = commentStartExpression.indexIn( text );
   }
   while( startIndex >= 0 )
   {
      int endIndex = commentEndExpression.indexIn( text, startIndex );
      int commentLength;
      if( endIndex == -1 )
      {
         setCurrentBlockState( 1 );
         commentLength = text.length() - startIndex;
      }
      else
      {
         commentLength = endIndex - startIndex + commentEndExpression.matchedLength();
      }
      setFormat( startIndex, commentLength, multiLineCommentFormat );
      startIndex = commentStartExpression.indexIn( text, startIndex + commentLength );
   }
}

#endif
