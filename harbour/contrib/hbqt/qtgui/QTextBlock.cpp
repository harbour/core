/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QTextBlock>
#include <QtGui/QTextDocument>
#include "../hbqt_hbqsyntaxhighlighter.h"

/*
 * QTextBlock ( const QTextBlock & other )
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
} QGC_POINTER_QTextBlock;

QT_G_FUNC( hbqt_gcRelease_QTextBlock )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextBlock   /.\\    ph=%p", p->ph ) );
         delete ( ( QTextBlock * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextBlock   \\./    ph=%p", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextBlock    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextBlock    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBlock( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBlock;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextBlock                 ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTBLOCK )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QTextBlock( *hbqt_par_QTextBlock( 1 ) ) ;
   }
   else
   {
      pObj = new QTextBlock() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTextBlock( pObj, true ) );
}

/*
 * QTextBlockFormat blockFormat () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( hbqt_par_QTextBlock( 1 )->blockFormat() ), true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( hbqt_par_QTextBlock( 1 )->charFormat() ), true ) );
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
 * virtual const QTextDocument * document () const
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
   hb_retptrGC( hbqt_gcAllocate_QTextLayout( hbqt_par_QTextBlock( 1 )->layout(), false ) );
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
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextBlock( 1 )->next() ), true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( hbqt_par_QTextBlock( 1 )->previous() ), true ) );
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
 * void setUserData ( HBQTextBlockUserData * data )
 */
HB_FUNC( QT_QTEXTBLOCK_SETUSERDATA )
{
   hbqt_par_QTextBlock( 1 )->setUserData( hbqt_par_HBQTextBlockUserData( 2 ) );
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
   hb_retc( hbqt_par_QTextBlock( 1 )->text().toAscii().data() );
}

/*
 * QTextList * textList () const
 */
HB_FUNC( QT_QTEXTBLOCK_TEXTLIST )
{
   hb_retptrGC( hbqt_gcAllocate_QTextList( hbqt_par_QTextBlock( 1 )->textList(), false ) );
}

/*
 * HBQTextBlockUserData * userData () const
 */
HB_FUNC( QT_QTEXTBLOCK_USERDATA )
{
   hb_retptr( ( HBQTextBlockUserData* ) hbqt_par_QTextBlock( 1 )->userData() );
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
