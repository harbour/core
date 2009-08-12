/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtGui/QTextBlock>


/*
 * QTextBlock ( const QTextBlock & other )
 */
HB_FUNC( QT_QTEXTBLOCK )
{
   hb_retptr( ( QTextBlock* ) new QTextBlock( *hbqt_par_QTextBlock( 1 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QTEXTBLOCK_DESTROY )
{

}

/*
 * QTextBlockFormat blockFormat () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKFORMAT )
{
   hb_retptr( new QTextBlockFormat( hbqt_par_QTextBlock( 1 )->blockFormat() ) );
}

/*
 * int blockFormatIndex () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKFORMATINDEX )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->blockFormatIndex() );
}

/*
 * int blockNumber () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKNUMBER )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->blockNumber() );
}

/*
 * QTextCharFormat charFormat () const
 */
HB_FUNC( QT_QTEXTBLOCK_CHARFORMAT )
{
   hb_retptr( new QTextCharFormat( hbqt_par_QTextBlock( 1 )->charFormat() ) );
}

/*
 * int charFormatIndex () const
 */
HB_FUNC( QT_QTEXTBLOCK_CHARFORMATINDEX )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->charFormatIndex() );
}

/*
 * void clearLayout ()
 */
HB_FUNC( QT_QTEXTBLOCK_CLEARLAYOUT )
{
   hbqt_par_QTextBlock( 1 )->clearLayout();
}

/*
 * bool contains ( int position ) const
 */
HB_FUNC( QT_QTEXTBLOCK_CONTAINS )
{
   hb_retl( hbqt_par_QTextBlock( 1 )->contains( hb_parni( 2 ) ) );
}

/*
 * const QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTBLOCK_DOCUMENT )
{
   hb_retptr( ( QTextDocument* ) hbqt_par_QTextBlock( 1 )->document() );
}

/*
 * int firstLineNumber () const
 */
HB_FUNC( QT_QTEXTBLOCK_FIRSTLINENUMBER )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->firstLineNumber() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBLOCK_ISVALID )
{
   hb_retl( hbqt_par_QTextBlock( 1 )->isValid() );
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QTEXTBLOCK_ISVISIBLE )
{
   hb_retl( hbqt_par_QTextBlock( 1 )->isVisible() );
}

/*
 * QTextLayout * layout () const
 */
HB_FUNC( QT_QTEXTBLOCK_LAYOUT )
{
   hb_retptr( ( QTextLayout* ) hbqt_par_QTextBlock( 1 )->layout() );
}

/*
 * int length () const
 */
HB_FUNC( QT_QTEXTBLOCK_LENGTH )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->length() );
}

/*
 * int lineCount () const
 */
HB_FUNC( QT_QTEXTBLOCK_LINECOUNT )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->lineCount() );
}

/*
 * QTextBlock next () const
 */
HB_FUNC( QT_QTEXTBLOCK_NEXT )
{
   hb_retptr( new QTextBlock( hbqt_par_QTextBlock( 1 )->next() ) );
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTBLOCK_POSITION )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->position() );
}

/*
 * QTextBlock previous () const
 */
HB_FUNC( QT_QTEXTBLOCK_PREVIOUS )
{
   hb_retptr( new QTextBlock( hbqt_par_QTextBlock( 1 )->previous() ) );
}

/*
 * int revision () const
 */
HB_FUNC( QT_QTEXTBLOCK_REVISION )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->revision() );
}

/*
 * void setLineCount ( int count )
 */
HB_FUNC( QT_QTEXTBLOCK_SETLINECOUNT )
{
   hbqt_par_QTextBlock( 1 )->setLineCount( hb_parni( 2 ) );
}

/*
 * void setRevision ( int rev )
 */
HB_FUNC( QT_QTEXTBLOCK_SETREVISION )
{
   hbqt_par_QTextBlock( 1 )->setRevision( hb_parni( 2 ) );
}

/*
 * void setUserData ( QTextBlockUserData * data )
 */
HB_FUNC( QT_QTEXTBLOCK_SETUSERDATA )
{
   hbqt_par_QTextBlock( 1 )->setUserData( hbqt_par_QTextBlockUserData( 2 ) );
}

/*
 * void setUserState ( int state )
 */
HB_FUNC( QT_QTEXTBLOCK_SETUSERSTATE )
{
   hbqt_par_QTextBlock( 1 )->setUserState( hb_parni( 2 ) );
}

/*
 * void setVisible ( bool visible )
 */
HB_FUNC( QT_QTEXTBLOCK_SETVISIBLE )
{
   hbqt_par_QTextBlock( 1 )->setVisible( hb_parl( 2 ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTEXTBLOCK_TEXT )
{
   hb_retc( hbqt_par_QTextBlock( 1 )->text().toLatin1().data() );
}

/*
 * QTextList * textList () const
 */
HB_FUNC( QT_QTEXTBLOCK_TEXTLIST )
{
   hb_retptr( ( QTextList* ) hbqt_par_QTextBlock( 1 )->textList() );
}

/*
 * QTextBlockUserData * userData () const
 */
HB_FUNC( QT_QTEXTBLOCK_USERDATA )
{
   hb_retptr( ( QTextBlockUserData* ) hbqt_par_QTextBlock( 1 )->userData() );
}

/*
 * int userState () const
 */
HB_FUNC( QT_QTEXTBLOCK_USERSTATE )
{
   hb_retni( hbqt_par_QTextBlock( 1 )->userState() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
