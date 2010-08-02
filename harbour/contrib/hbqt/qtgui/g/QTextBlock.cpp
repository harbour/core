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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextBlock;

QT_G_FUNC( hbqt_gcRelease_QTextBlock )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

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
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

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
      hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->blockFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_BLOCKFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->blockFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * int blockFormatIndex () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKFORMATINDEX )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->blockFormatIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_BLOCKFORMATINDEX FP=hb_retni( ( p )->blockFormatIndex() ); p is NULL" ) );
   }
}

/*
 * int blockNumber () const
 */
HB_FUNC( QT_QTEXTBLOCK_BLOCKNUMBER )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->blockNumber() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_BLOCKNUMBER FP=hb_retni( ( p )->blockNumber() ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat charFormat () const
 */
HB_FUNC( QT_QTEXTBLOCK_CHARFORMAT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_CHARFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * int charFormatIndex () const
 */
HB_FUNC( QT_QTEXTBLOCK_CHARFORMATINDEX )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->charFormatIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_CHARFORMATINDEX FP=hb_retni( ( p )->charFormatIndex() ); p is NULL" ) );
   }
}

/*
 * void clearLayout ()
 */
HB_FUNC( QT_QTEXTBLOCK_CLEARLAYOUT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      ( p )->clearLayout();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_CLEARLAYOUT FP=( p )->clearLayout(); p is NULL" ) );
   }
}

/*
 * bool contains ( int position ) const
 */
HB_FUNC( QT_QTEXTBLOCK_CONTAINS )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retl( ( p )->contains( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_CONTAINS FP=hb_retl( ( p )->contains( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual const QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTBLOCK_DOCUMENT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( void * ) ( p )->document(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_DOCUMENT FP=hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( void * ) ( p )->document(), false ) ); p is NULL" ) );
   }
}

/*
 * int firstLineNumber () const
 */
HB_FUNC( QT_QTEXTBLOCK_FIRSTLINENUMBER )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->firstLineNumber() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_FIRSTLINENUMBER FP=hb_retni( ( p )->firstLineNumber() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBLOCK_ISVALID )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QTEXTBLOCK_ISVISIBLE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_ISVISIBLE FP=hb_retl( ( p )->isVisible() ); p is NULL" ) );
   }
}

/*
 * QTextLayout * layout () const
 */
HB_FUNC( QT_QTEXTBLOCK_LAYOUT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLayout( ( p )->layout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_LAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QTextLayout( ( p )->layout(), false ) ); p is NULL" ) );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QTEXTBLOCK_LENGTH )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->length() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_LENGTH FP=hb_retni( ( p )->length() ); p is NULL" ) );
   }
}

/*
 * int lineCount () const
 */
HB_FUNC( QT_QTEXTBLOCK_LINECOUNT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->lineCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_LINECOUNT FP=hb_retni( ( p )->lineCount() ); p is NULL" ) );
   }
}

/*
 * QTextBlock next () const
 */
HB_FUNC( QT_QTEXTBLOCK_NEXT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->next() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_NEXT FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->next() ), true ) ); p is NULL" ) );
   }
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTBLOCK_POSITION )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->position() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_POSITION FP=hb_retni( ( p )->position() ); p is NULL" ) );
   }
}

/*
 * QTextBlock previous () const
 */
HB_FUNC( QT_QTEXTBLOCK_PREVIOUS )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->previous() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_PREVIOUS FP=hb_retptrGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( p )->previous() ), true ) ); p is NULL" ) );
   }
}

/*
 * int revision () const
 */
HB_FUNC( QT_QTEXTBLOCK_REVISION )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->revision() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_REVISION FP=hb_retni( ( p )->revision() ); p is NULL" ) );
   }
}

/*
 * void setLineCount ( int count )
 */
HB_FUNC( QT_QTEXTBLOCK_SETLINECOUNT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      ( p )->setLineCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_SETLINECOUNT FP=( p )->setLineCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRevision ( int rev )
 */
HB_FUNC( QT_QTEXTBLOCK_SETREVISION )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      ( p )->setRevision( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_SETREVISION FP=( p )->setRevision( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUserData ( HBQTextBlockUserData * data )
 */
HB_FUNC( QT_QTEXTBLOCK_SETUSERDATA )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      ( p )->setUserData( hbqt_par_HBQTextBlockUserData( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_SETUSERDATA FP=( p )->setUserData( hbqt_par_HBQTextBlockUserData( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUserState ( int state )
 */
HB_FUNC( QT_QTEXTBLOCK_SETUSERSTATE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      ( p )->setUserState( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_SETUSERSTATE FP=( p )->setUserState( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setVisible ( bool visible )
 */
HB_FUNC( QT_QTEXTBLOCK_SETVISIBLE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTEXTBLOCK_TEXT )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextList * textList () const
 */
HB_FUNC( QT_QTEXTBLOCK_TEXTLIST )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->textList(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_TEXTLIST FP=hb_retptrGC( hbqt_gcAllocate_QTextList( ( p )->textList(), false ) ); p is NULL" ) );
   }
}

/*
 * HBQTextBlockUserData * userData () const
 */
HB_FUNC( QT_QTEXTBLOCK_USERDATA )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_HBQTextBlockUserData( ( p )->userData(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_USERDATA FP=hb_retptrGC( hbqt_gcAllocate_HBQTextBlockUserData( ( p )->userData(), false ) ); p is NULL" ) );
   }
}

/*
 * int userState () const
 */
HB_FUNC( QT_QTEXTBLOCK_USERSTATE )
{
   QTextBlock * p = hbqt_par_QTextBlock( 1 );
   if( p )
      hb_retni( ( p )->userState() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBLOCK_USERSTATE FP=hb_retni( ( p )->userState() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
