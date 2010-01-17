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
/*----------------------------------------------------------------------*/

#include "hbapi.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqsyntaxhighlighter.h"

#include <QtCore/QPointer>
#include <QHash>
#include <QTextCharFormat>

HBQTextBlockUserData::HBQTextBlockUserData() : QTextBlockUserData()
{
   state = -1;
}
HBQTextBlockUserData::~HBQTextBlockUserData()
{
}
void HBQTextBlockUserData::setData( int iState )
{
   state = iState;
}
HBQTextBlockUserData * HBQTextBlockUserData::data( const QTextBlock& block )
{
   return static_cast<HBQTextBlockUserData *>( block.userData() );
}


HBQSyntaxHighlighter::HBQSyntaxHighlighter( QTextDocument * parent )
   : QSyntaxHighlighter( parent )
{
   HighlightingRule rule;
   multiLineCommentFormat.setForeground( Qt::red );

   commentStartExpression = QRegExp( "/\\*" );
   commentEndExpression = QRegExp( "\\*/" );
}

void HBQSyntaxHighlighter::setHBRule( QString name, QString pattern, const QTextCharFormat & format )
{
   if( pattern != "" )
      hhighlightingRules.insert( name, hHighlightingRule( QRegExp( pattern ), format ) );
   else
      hhighlightingRules.remove( name );
}

void HBQSyntaxHighlighter::setHBFormat( QString name, const QTextCharFormat & format )
{
   if( hhighlightingRules.contains( name ) )
   {
      hHighlightingRule rule = hhighlightingRules.value( name );
      QRegExp reg = rule.pattern;

      hhighlightingRules.insert( name, hHighlightingRule( reg, format ) );
   }
   else
   {
      hhighlightingRules.remove( name );
   }
}

void HBQSyntaxHighlighter::setHBCompilerDirectives( const QStringList & directives, const QTextCharFormat & format )
{
   HighlightingRule rule;

   directivesFormat = format;
   foreach( const QString &pattern, directives )
   {
      rule.pattern = QRegExp( pattern );
      rule.format = directivesFormat;
      highlightingRules.append( rule );
   }
}

void HBQSyntaxHighlighter::setHBMultiLineCommentFormat( const QTextCharFormat & format )
{
   multiLineCommentFormat = format;
}

void HBQSyntaxHighlighter::highlightBlock( const QString &text )
{
   //HB_TRACE( HB_TR_ALWAYS, ( "text = %s", ( char * ) &text ) );
   #if 0
   return ;
   #endif

   QRegExp expression;
   QTextBlock curBlock( currentBlock() );
   bool bMerge = false;
   //HBQTextBlockUserData * data = HBQTextBlockUserData::data( curBlock );
   HBQTextBlockUserData * data = ( HBQTextBlockUserData * ) curBlock.userData();

   if( data )
      bMerge = ( data->state == 99 );

   foreach( const hHighlightingRule &rule, hhighlightingRules )
   {
      QRegExp expression( rule.pattern );
      int index = expression.indexIn( text );
      while( index >= 0 )
      {
         int length = expression.matchedLength();
         QTextBlockFormat tBlockFormat( curBlock.blockFormat() );
         QBrush brush( tBlockFormat.background() );

         if( bMerge )
         {
            HB_TRACE( HB_TR_ALWAYS, ( "text = %s", ( char * ) &text ) );
            setFormat( index, length, rule.format );
         }
         else
            setFormat( index, length, rule.format );

         index = expression.indexIn( text, index + length );
      }
   }
   setCurrentBlockState( 0 );

   int startIndex = 0;
   if( previousBlockState() != 1 )
      startIndex = commentStartExpression.indexIn( text );

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
         commentLength = endIndex - startIndex + commentEndExpression.matchedLength();

      setFormat( startIndex, commentLength, multiLineCommentFormat );
      startIndex = commentStartExpression.indexIn( text, startIndex + commentLength );
   }
}

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< HBQSyntaxHighlighter > pq;
} QGC_POINTER_HBQSyntaxHighlighter;

static QT_G_FUNC( release_HBQSyntaxHighlighter )
{
   QGC_POINTER_HBQSyntaxHighlighter * p = ( QGC_POINTER_HBQSyntaxHighlighter * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_HBQSyntaxHighlighter           p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_HBQSyntaxHighlighter          ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( HBQSyntaxHighlighter * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( HBQSyntaxHighlighter * ) p->ph )->~HBQSyntaxHighlighter();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( HBQSyntaxHighlighter * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_HBQSyntaxHighlighter          Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_HBQSyntaxHighlighter          Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_HBQSyntaxHighlighter          Object Already deleted!" ) );
   }
}

static void * hbqt_gcAllocate_HBQSyntaxHighlighter( void * pObj )
{
   QGC_POINTER_HBQSyntaxHighlighter * p = ( QGC_POINTER_HBQSyntaxHighlighter * ) hb_gcAllocate( sizeof( QGC_POINTER_HBQSyntaxHighlighter ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = release_HBQSyntaxHighlighter;
   new( & p->pq ) QPointer< HBQSyntaxHighlighter >( ( HBQSyntaxHighlighter * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_HBQSyntaxHighlighter          %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
      pObj = new HBQSyntaxHighlighter( hbqt_par_QTextDocument( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_HBQSyntaxHighlighter( pObj ) );
}
/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_DOCUMENT )
{
   hb_retptr( ( QTextDocument* ) hbqt_par_HBQSyntaxHighlighter( 1 )->document() );
}

/*
 * void setDocument ( QTextDocument * doc )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_SETDOCUMENT )
{
   hbqt_par_HBQSyntaxHighlighter( 1 )->setDocument( hbqt_par_QTextDocument( 2 ) );
}

/*
 * void rehighlight ()
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_REHIGHLIGHT )
{
   hbqt_par_HBQSyntaxHighlighter( 1 )->rehighlight();
}

/*
 * void setHBCompilerDirectives( const QStringList & directives, const QTextCharFormat & format )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_SETHBCOMPILERDIRECTIVES )
{
   hbqt_par_HBQSyntaxHighlighter( 1 )->setHBCompilerDirectives( *hbqt_par_QStringList( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
}

/*
 * void setHBMultiLineCommentFormat( const QTextCharFormat & format )
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_SETHBMULTILINECOMMENTFORMAT )
{
   hbqt_par_HBQSyntaxHighlighter( 1 )->setHBMultiLineCommentFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/*
 * void setRule( QString name, QString pattern, QTextCharFormat format );
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_SETHBRULE )
{
   hbqt_par_HBQSyntaxHighlighter( 1 )->setHBRule( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ), *hbqt_par_QTextCharFormat( 4 ) );
}

/*
 * void setFormat( QString name, const QTextCharFormat & format );
 */
HB_FUNC( QT_HBQSYNTAXHIGHLIGHTER_SETHBFORMAT )
{
   hbqt_par_HBQSyntaxHighlighter( 1 )->setHBFormat( hbqt_par_QString( 2 ), *hbqt_par_QTextCharFormat( 3 ) );
}

#endif
