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
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QTextBlock>
#include <QtGui/QTextDocument>
#include "hbqt_hbqsyntaxhighlighter.h"

/*
 * QTextBlock ( const QTextBlock & other )
 */

typedef struct
{
   QTextBlock * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextBlock;

HBQT_GC_FUNC( hbqt_gcRelease_QTextBlock )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextBlock   /.\\", p->ph ) );
         delete ( ( QTextBlock * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextBlock   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextBlock    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextBlock    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBlock( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextBlock * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBlock;
   p->type = HBQT_TYPE_QTextBlock;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextBlock", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextBlock", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTBLOCK )
{
   QTextBlock * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QTextBlock( *hbqt_par_QTextBlock( 1 ) ) ;
   }
   else
   {
      pObj = new QTextBlock() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QTextBlock( ( void * ) pObj, true ) );
}

/*
 * QTextBlockFormat blockFormat () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKFORMAT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->blockFormat() ), true ) );
   }
}

/*
 * int blockFormatIndex () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKFORMATINDEX )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->blockFormatIndex() );
   }
}

/*
 * int blockNumber () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKNUMBER )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->blockNumber() );
   }
}

/*
 * QTextCharFormat charFormat () const
 */
HB_FUNC( QT_QTEXTBLOCK_CHARFORMAT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) );
   }
}

/*
 * int charFormatIndex () const
 */
HB_FUNC( QT_QTEXTBLOCK_CHARFORMATINDEX )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->charFormatIndex() );
   }
}

/*
 * void clearLayout ()
 */
HB_FUNC( QT_QTEXTBLOCK_CLEARLAYOUT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      ( p )->clearLayout();
   }
}

/*
 * bool contains ( int position ) const
 */
HB_FUNC( QT_QTEXTBLOCK_CONTAINS )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retl( ( p )->contains( hb_parni( 2 ) ) );
   }
}

/*
 * virtual const QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTBLOCK_DOCUMENT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( void * ) ( p )->document(), false ) );
   }
}

/*
 * int firstLineNumber () const
 */
HB_FUNC( QT_QTEXTBLOCK_FIRSTLINENUMBER )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->firstLineNumber() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBLOCK_ISVALID )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QTEXTBLOCK_ISVISIBLE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retl( ( p )->isVisible() );
   }
}

/*
 * QTextLayout * layout () const
 */
HB_FUNC( QT_QTEXTBLOCK_LAYOUT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextLayout( ( p )->layout(), false ) );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QTEXTBLOCK_LENGTH )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->length() );
   }
}

/*
 * int lineCount () const
 */
HB_FUNC( QT_QTEXTBLOCK_LINECOUNT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->lineCount() );
   }
}

/*
 * QTextBlock next () const
 */
HB_FUNC( QT_QTEXTBLOCK_NEXT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->next() ), true ) );
   }
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTBLOCK_POSITION )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->position() );
   }
}

/*
 * QTextBlock previous () const
 */
HB_FUNC( QT_QTEXTBLOCK_PREVIOUS )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->previous() ), true ) );
   }
}

/*
 * int revision () const
 */
HB_FUNC( QT_QTEXTBLOCK_REVISION )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->revision() );
   }
}

/*
 * void setLineCount ( int count )
 */
HB_FUNC( QT_QTEXTBLOCK_SETLINECOUNT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      ( p )->setLineCount( hb_parni( 2 ) );
   }
}

/*
 * void setRevision ( int rev )
 */
HB_FUNC( QT_QTEXTBLOCK_SETREVISION )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      ( p )->setRevision( hb_parni( 2 ) );
   }
}

/*
 * void setUserData ( HBQTextBlockUserData * data )
 */
HB_FUNC( QT_QTEXTBLOCK_SETUSERDATA )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      ( p )->setUserData( hbqt_par_HBQTextBlockUserData( 2 ) );
   }
}

/*
 * void setUserState ( int state )
 */
HB_FUNC( QT_QTEXTBLOCK_SETUSERSTATE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      ( p )->setUserState( hb_parni( 2 ) );
   }
}

/*
 * void setVisible ( bool visible )
 */
HB_FUNC( QT_QTEXTBLOCK_SETVISIBLE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      ( p )->setVisible( hb_parl( 2 ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTEXTBLOCK_TEXT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
   }
}

/*
 * QTextList * textList () const
 */
HB_FUNC( QT_QTEXTBLOCK_TEXTLIST )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->textList(), false ) );
   }
}

/*
 * HBQTextBlockUserData * userData () const
 */
HB_FUNC( QT_QTEXTBLOCK_USERDATA )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_HBQTextBlockUserData( ( p )->userData(), false ) );
   }
}

/*
 * int userState () const
 */
HB_FUNC( QT_QTEXTBLOCK_USERSTATE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
   {
      hb_retni( ( p )->userState() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
