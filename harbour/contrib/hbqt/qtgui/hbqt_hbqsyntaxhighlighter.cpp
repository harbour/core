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
#include "hbqt_hbqplaintextedit.h"

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
   commentEndExpression   = QRegExp( "\\*/" );
   commentSingleLine      = QRegExp( "//[^\n]*|^[ ]*\\*[^\n]*" );
   patternQuotation       = QRegExp( "\"[^\"]*\"|\'[^\']*\'" );
// definedConstants       = QRegExp( "\b(__[A-Za-z0-9_]+__)\b" );
   definedConstants       = QRegExp( "__[A-Za-z0-9_]+__" );

   initialized = false;
   type = 0;
   editor = NULL;

   constantsFormat.setForeground( QColor( 255, 153, 51 ) );
   constantsFormat.setFontWeight( 1000 );

   //entryHeaderFormat.setForeground( Qt::red );
   entryHeaderFormat.setForeground( QColor( 255, 153, 51 ) );
   entryHeaderFormat.setFontWeight( 1000 );
   //entryHeaderFormat.setBackground( Qt::gray );

   //entryTitleFormat.setForeground( Qt::darkBlue );
   entryTitleFormat.setForeground( QColor( 45, 187, 255 ) );
   entryTitleFormat.setFontItalic( true );
   //entryTitleFormat.setFontWeight( 1000 );

   entrySourceFormat.setForeground( Qt::darkGreen );
   entrySourceFormat.setFontWeight( 1000 );

   entryFixedFormat.setForeground( Qt::blue );
   entryFixedFormat.setFontItalic( true );

   entryChangedFormat.setForeground( Qt::darkGray );
   entryChangedFormat.setFontItalic( true );

   entryOptimizedFormat.setForeground( Qt::magenta );
   entryOptimizedFormat.setFontItalic( true );

   entryAddedFormat.setForeground( Qt::green );
   entryAddedFormat.setFontItalic( true );

   entryRemovedFormat.setForeground( Qt::red );
   entryRemovedFormat.setFontItalic( true );

   entryCommentFormat.setForeground( Qt::green );
   entryCommentFormat.setFontItalic( true );

   entryTodoFormat.setForeground( Qt::blue );
   entryTodoFormat.setFontItalic( true );

   entryMovedFormat.setForeground( Qt::magenta );
   entryMovedFormat.setFontItalic( true );

   entryHeaderRegExp     =  QRegExp( "^\\$\\<[0-9]*\\>[^\n]*" );
   entryTitleRegExp      =  QRegExp( "^[ ]*\\#[^\n]*" );
   entrySourceRegExp     =  QRegExp( "^[ ]*\\*[^\n]*" );
   entryFixedRegExp      =  QRegExp( "^[ ]*\\! Fixed  " );
   entryChangedRegExp    =  QRegExp( "^[ ]*\\* Changed" );
   entryOptimizedRegExp  =  QRegExp( "^[ ]*\\% Optimzd" );
   entryAddedRegExp      =  QRegExp( "^[ ]*\\+ Added  " );
   entryRemovedRegExp    =  QRegExp( "^[ ]*\\- Removed" );
   entryCommentRegExp    =  QRegExp( "^[ ]*\\; Comment" );
   entryTodoRegExp       =  QRegExp( "^[ ]*\\@ TODO   " );
   entryMovedRegExp      =  QRegExp( "^[ ]*\\| Moved  " );

   isEntry               =  QRegExp( "^[ ]*\\||^[ ]*\\@|^[ ]*\\;|^[ ]*\\-|^[ ]*\\+|^[ ]*\\%|^[ ]*\\&|^[ ]*\\!|^[ ]*\\*|^[ ]*\\#|^\\$" );
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
   if( type == 0 )   /* PRG C C++ Sources */
   {
      if( ! initialized )
         return;

      if( editor )
      {
         int iFirstBlock = editor->firstVisibleBlockNumber();
         int iLastBlock = editor->lastVisibleBlockNumber();
         int iBlock = currentBlock().blockNumber();

         if( iBlock < iFirstBlock || iBlock > iLastBlock )
         {
            return;
         }
      }

      int index = 0;
      int length = 0;

      foreach( const HighlightingRule &rule, HighlightingRules )
      {
         index = rule.pattern.indexIn( text );
         while( index >= 0 )
         {
            length = rule.pattern.matchedLength();
            setFormat( index, length, rule.format );
            index = rule.pattern.indexIn( text, index + length );
         }
      }

      /* Defined constants */
      index = definedConstants.indexIn( text );
      while( index >= 0 )
      {
         length = definedConstants.matchedLength();
         setFormat( index, length, constantsFormat );
         index = definedConstants.indexIn( text, index + length );
      }

      /* Multi Line Comments - to ascertain if it is embedded in quotes */
      int startIndex = 0;
      int startSglLine = 0;
      if( previousBlockState() != 1 )
      {
         startIndex = commentStartExpression.indexIn( text );
         startSglLine = commentSingleLine.indexIn( text );
      }

      /* Quoted text */
      index = patternQuotation.indexIn( text );
      while( index >= 0 )
      {
         length = patternQuotation.matchedLength();
         setFormat( index, length, quotationFormat );
         if( startIndex > index && startIndex < index + length )
         {
            startIndex = -1;
         }
         if( startSglLine > index && startSglLine < index + length )
         {
            startSglLine = -1;
         }
         index = patternQuotation.indexIn( text, index + length );
      }

      /* Single Line Comments */
      if( startSglLine >= 0 )
      {
         index = commentSingleLine.indexIn( text );
         while( index >= 0 )
         {
            length = commentSingleLine.matchedLength();
            setFormat( index, length, singleLineCommentFormat );
            index = commentSingleLine.indexIn( text, index + length );
         }
      }
      /* Multi Line Comments - continued */
      setCurrentBlockState( 0 );

      while( startIndex >= 0 )
      {
         int commentLength;
         int endIndex = commentEndExpression.indexIn( text, startIndex );
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
   else if( type == 1 )  /* ChangeLog */
   {
      int index, length;

      index = isEntry.indexIn( text );
      if( index >= 0 )
      {
         /* Single Line Comments */
         index = entryHeaderRegExp.indexIn( text );
         if( index >= 0 )
         {
            length = entryHeaderRegExp.matchedLength();
            setFormat( index, length, entryHeaderFormat );
         }
         else
         {
            index = entryTitleRegExp.indexIn( text );
            if( index >= 0 )
            {
               length = entryTitleRegExp.matchedLength();
               setFormat( index, length, entryTitleFormat );
            }
            else
            {
               index = entryChangedRegExp.indexIn( text );
               if( index >= 0 )
               {
                  length = entryChangedRegExp.matchedLength();
                  setFormat( index, length, entryChangedFormat );
               }
               else
               {
                  index = entryFixedRegExp.indexIn( text );
                  if( index >= 0 )
                  {
                     length = entryFixedRegExp.matchedLength();
                     setFormat( index, length, entryFixedFormat );
                  }
                  else
                  {
                     index = entrySourceRegExp.indexIn( text );
                     if( index >= 0 )
                     {
                        length = entrySourceRegExp.matchedLength();
                        setFormat( index, length, entrySourceFormat );
                     }
                     else
                     {
                        index = entryOptimizedRegExp.indexIn( text );
                        if( index >= 0 )
                        {
                           length = entryOptimizedRegExp.matchedLength();
                           setFormat( index, length, entryOptimizedFormat );
                        }
                        else
                        {
                           index = entryAddedRegExp.indexIn( text );
                           if( index >= 0 )
                           {
                              length = entryAddedRegExp.matchedLength();
                              setFormat( index, length, entryAddedFormat );
                           }
                           else
                           {
                              index = entryRemovedRegExp.indexIn( text );
                              if( index >= 0 )
                              {
                                 length = entryRemovedRegExp.matchedLength();
                                 setFormat( index, length, entryRemovedFormat );
                              }
                              else
                              {
                                 index = entryCommentRegExp.indexIn( text );
                                 if( index >= 0 )
                                 {
                                    length = entryCommentRegExp.matchedLength();
                                    setFormat( index, length, entryCommentFormat );
                                 }
                                 else
                                 {
                                    index = entryTodoRegExp.indexIn( text );
                                    if( index >= 0 )
                                    {
                                       length = entryTodoRegExp.matchedLength();
                                       setFormat( index, length, entryTodoFormat );
                                    }
                                    else
                                    {
                                       index = entryMovedRegExp.indexIn( text );
                                       if( index >= 0 )
                                       {
                                          length = entryMovedRegExp.matchedLength();
                                          setFormat( index, length, entryMovedFormat );
                                       }
                                       else
                                       {
                                       }
                                    }
                                 }
                              }
                           }
                        }
                     }
                  }
               }
            }
         }
      }
#if 0
      /* Multi Line Comments */
      int startIndex = 0;
      if( previousBlockState() != 1 )
      {
         startIndex = commentStartExpression.indexIn( text );
      }
      /* Multi Line Comments - continued */
      setCurrentBlockState( 0 );

      while( startIndex >= 0 )
      {
         int commentLength;
         int endIndex = commentEndExpression.indexIn( text, startIndex );
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
#endif
   }
}

#endif

