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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QPlainTextEdit>

#include "../hbqt_hbqplaintextedit.h"

/*
 * HBQPlainTextEdit ( QWidget * parent = 0 )
 * HBQPlainTextEdit ( const QString & text, QWidget * parent = 0 )
 * virtual ~HBQPlainTextEdit ()
 */

typedef struct
{
   QPointer< HBQPlainTextEdit > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_HBQPlainTextEdit;

QT_G_FUNC( hbqt_gcRelease_HBQPlainTextEdit )
{
   HBQPlainTextEdit  * ph = NULL ;
   QGC_POINTER_HBQPlainTextEdit * p = ( QGC_POINTER_HBQPlainTextEdit * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQPlainTextEdit   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_HBQPlainTextEdit   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_HBQPlainTextEdit          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_HBQPlainTextEdit    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_HBQPlainTextEdit    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQPlainTextEdit( void * pObj, bool bNew )
{
   QGC_POINTER_HBQPlainTextEdit * p = ( QGC_POINTER_HBQPlainTextEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_HBQPlainTextEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< HBQPlainTextEdit >( ( HBQPlainTextEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQPlainTextEdit;
   p->type = HBQT_TYPE_HBQPlainTextEdit;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_HBQPlainTextEdit  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_HBQPlainTextEdit", pObj ) );
   }
   return p;
}

HB_FUNC( QT_HBQPLAINTEXTEDIT )
{
   HBQPlainTextEdit * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new HBQPlainTextEdit() ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new HBQPlainTextEdit( hbqt_par_QWidget( 1 ) ) ;
   }
   else
   {
      pObj = new HBQPlainTextEdit() ;
   }

   hb_retptrGC( hbqt_gcAllocate_HBQPlainTextEdit( ( void * ) pObj, true ) );
}

/*
 * int            hbGetIndex( const QTextCursor & crQTextCursor)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETINDEX )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->hbGetIndex( *hbqt_par_QTextCursor( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBGETINDEX FP=hb_retni( ( p )->hbGetIndex( *hbqt_par_QTextCursor( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int            hbGetLine( const QTextCursor & crQTextCursor)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETLINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->hbGetLine( *hbqt_par_QTextCursor( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBGETLINE FP=hb_retni( ( p )->hbGetLine( *hbqt_par_QTextCursor( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int            hbLineNumberAreaWidth()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBLINENUMBERAREAWIDTH )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->hbLineNumberAreaWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBLINENUMBERAREAWIDTH FP=hb_retni( ( p )->hbLineNumberAreaWidth() ); p is NULL" ) );
   }
}

/*
 * int            hbGetSpaces()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSPACES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->hbGetSpaces() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBGETSPACES FP=hb_retni( ( p )->hbGetSpaces() ); p is NULL" ) );
   }
}

/*
 * void           hbSetSpaces(int newSpaces)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSPACES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetSpaces( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETSPACES FP=( p )->hbSetSpaces( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbBookmarks(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBOOKMARKS )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbBookmarks( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBBOOKMARKS FP=( p )->hbBookmarks( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbNextBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNEXTBOOKMARK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbNextBookmark( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBNEXTBOOKMARK FP=( p )->hbNextBookmark( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbPrevBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBPREVBOOKMARK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbPrevBookmark( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBPREVBOOKMARK FP=( p )->hbPrevBookmark( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbGotoBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGOTOBOOKMARK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbGotoBookmark( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBGOTOBOOKMARK FP=( p )->hbGotoBookmark( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbNumberBlockVisible(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbNumberBlockVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE FP=( p )->hbNumberBlockVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool           hbNumberBlockVisible()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE_1 )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->hbNumberBlockVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE_1 FP=hb_retl( ( p )->hbNumberBlockVisible() ); p is NULL" ) );
   }
}

/*
 * void           hbHighlightCurrentLine(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbHighlightCurrentLine( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE FP=( p )->hbHighlightCurrentLine( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool           hbHighlightCurrentLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE_1 )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->hbHighlightCurrentLine() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE_1 FP=hb_retl( ( p )->hbHighlightCurrentLine() ); p is NULL" ) );
   }
}

/*
 * void           hbSetEventBlock( PHB_ITEM block )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETEVENTBLOCK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetEventBlock( hb_param( 2, HB_IT_ANY ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETEVENTBLOCK FP=( p )->hbSetEventBlock( hb_param( 2, HB_IT_ANY ) ); p is NULL" ) );
   }
}

/*
 * void           hbUpdateLineNumberAreaWidth( int newBlockCount )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUPDATELINENUMBERAREAWIDTH )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbUpdateLineNumberAreaWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBUPDATELINENUMBERAREAWIDTH FP=( p )->hbUpdateLineNumberAreaWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbCaseUpper()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCASEUPPER )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbCaseUpper();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBCASEUPPER FP=( p )->hbCaseUpper(); p is NULL" ) );
   }
}

/*
 * void           hbCaseLower()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCASELOWER )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbCaseLower();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBCASELOWER FP=( p )->hbCaseLower(); p is NULL" ) );
   }
}

/*
 * void           hbEscapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBESCAPEQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbEscapeQuotes();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBESCAPEQUOTES FP=( p )->hbEscapeQuotes(); p is NULL" ) );
   }
}

/*
 * void           hbEscapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBESCAPEDQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbEscapeDQuotes();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBESCAPEDQUOTES FP=( p )->hbEscapeDQuotes(); p is NULL" ) );
   }
}

/*
 * void           hbUnescapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUNESCAPEQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbUnescapeQuotes();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBUNESCAPEQUOTES FP=( p )->hbUnescapeQuotes(); p is NULL" ) );
   }
}

/*
 * void           hbUnescapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUNESCAPEDQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbUnescapeDQuotes();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBUNESCAPEDQUOTES FP=( p )->hbUnescapeDQuotes(); p is NULL" ) );
   }
}

/*
 * void           hbConvertQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCONVERTQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbConvertQuotes();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBCONVERTQUOTES FP=( p )->hbConvertQuotes(); p is NULL" ) );
   }
}

/*
 * void           hbConvertDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCONVERTDQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbConvertDQuotes();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBCONVERTDQUOTES FP=( p )->hbConvertDQuotes(); p is NULL" ) );
   }
}

/*
 * void           hbBlockComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBLOCKCOMMENT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbBlockComment();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBBLOCKCOMMENT FP=( p )->hbBlockComment(); p is NULL" ) );
   }
}

/*
 * void           hbStreamComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSTREAMCOMMENT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbStreamComment();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSTREAMCOMMENT FP=( p )->hbStreamComment(); p is NULL" ) );
   }
}

/*
 * void           hbDuplicateLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBDUPLICATELINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbDuplicateLine();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBDUPLICATELINE FP=( p )->hbDuplicateLine(); p is NULL" ) );
   }
}

/*
 * void           hbReplaceSelection( const QString & txt )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBREPLACESELECTION )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbReplaceSelection( HBQPlainTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBREPLACESELECTION FP=( p )->hbReplaceSelection( HBQPlainTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void           hbBlockIndent( int steps )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBLOCKINDENT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbBlockIndent( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBBLOCKINDENT FP=( p )->hbBlockIndent( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbDeleteLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBDELETELINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbDeleteLine();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBDELETELINE FP=( p )->hbDeleteLine(); p is NULL" ) );
   }
}

/*
 * void           hbMoveLine( int iDirection )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBMOVELINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbMoveLine( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBMOVELINE FP=( p )->hbMoveLine( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString        hbGetSelectedText()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSELECTEDTEXT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retc( ( p )->hbGetSelectedText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBGETSELECTEDTEXT FP=hb_retc( ( p )->hbGetSelectedText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString        hbTextUnderCursor( bool bCodeComplete )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBTEXTUNDERCURSOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      hb_retc( ( p )->hbTextUnderCursor( hb_parl( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBTEXTUNDERCURSOR FP=hb_retc( ( p )->hbTextUnderCursor( hb_parl( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void           hbShowPrototype( const QString & tip, int rows, int cols )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSHOWPROTOTYPE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbShowPrototype( HBQPlainTextEdit::tr( hb_parc( 2 ) ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSHOWPROTOTYPE FP=( p )->hbShowPrototype( HBQPlainTextEdit::tr( hb_parc( 2 ) ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void           hbSetCompleter( QCompleter * completer )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETCOMPLETER )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetCompleter( hbqt_par_QCompleter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETCOMPLETER FP=( p )->hbSetCompleter( hbqt_par_QCompleter( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbSetCurrentLineColor( const QColor & color )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETCURRENTLINECOLOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetCurrentLineColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETCURRENTLINECOLOR FP=( p )->hbSetCurrentLineColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbSetLineAreaBkColor( const QColor & color )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETLINEAREABKCOLOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetLineAreaBkColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETLINEAREABKCOLOR FP=( p )->hbSetLineAreaBkColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbRefresh()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBREFRESH )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbRefresh();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBREFRESH FP=( p )->hbRefresh(); p is NULL" ) );
   }
}

/*
 * void           hbCut( int key )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCUT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbCut( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBCUT FP=( p )->hbCut( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbCopy()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCOPY )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbCopy();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBCOPY FP=( p )->hbCopy(); p is NULL" ) );
   }
}

/*
 * void           hbPaste()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBPASTE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbPaste();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBPASTE FP=( p )->hbPaste(); p is NULL" ) );
   }
}

/*
 * void           hbSetSelectionMode( int mode, bool on )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSELECTIONMODE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetSelectionMode( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETSELECTIONMODE FP=( p )->hbSetSelectionMode( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void           hbGetSelectionInfo()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSELECTIONINFO )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbGetSelectionInfo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBGETSELECTIONINFO FP=( p )->hbGetSelectionInfo(); p is NULL" ) );
   }
}

/*
 * void           hbSetSelectionInfo( PHB_ITEM selectionInfo )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSELECTIONINFO )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetSelectionInfo( hb_param( 2, HB_IT_ANY ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETSELECTIONINFO FP=( p )->hbSetSelectionInfo( hb_param( 2, HB_IT_ANY ) ); p is NULL" ) );
   }
}

/*
 * void           hbSetSelectionColor( const QColor & color )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSELECTIONCOLOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetSelectionColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETSELECTIONCOLOR FP=( p )->hbSetSelectionColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbSetMatchBraces( bool all )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETMATCHBRACES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetMatchBraces( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETMATCHBRACES FP=( p )->hbSetMatchBraces( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbGetViewportInfo()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETVIEWPORTINFO )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbGetViewportInfo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBGETVIEWPORTINFO FP=( p )->hbGetViewportInfo(); p is NULL" ) );
   }
}

/*
 * void           hbApplyKey( int key, Qt::KeyboardModifiers modifiers = 0, const QString & txt )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBAPPLYKEY )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbApplyKey( hb_parni( 2 ), ( Qt::KeyboardModifiers ) hb_parni( 3 ), HBQPlainTextEdit::tr( hb_parc( 4 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBAPPLYKEY FP=( p )->hbApplyKey( hb_parni( 2 ), ( Qt::KeyboardModifiers ) hb_parni( 3 ), HBQPlainTextEdit::tr( hb_parc( 4 ) ) ); p is NULL" ) );
   }
}

/*
 * void           hbHighlightArea( int top, int left, int bottom, int right, int mode )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTAREA )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbHighlightArea( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTAREA FP=( p )->hbHighlightArea( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ) ); p is NULL" ) );
   }
}

/*
 * void           hbTogglePersistentSelection()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBTOGGLEPERSISTENTSELECTION )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbTogglePersistentSelection();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBTOGGLEPERSISTENTSELECTION FP=( p )->hbTogglePersistentSelection(); p is NULL" ) );
   }
}

/*
 * void           hbHorzRulerVisible( bool visible )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHORZRULERVISIBLE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbHorzRulerVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBHORZRULERVISIBLE FP=( p )->hbHorzRulerVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void           hbSetProtoStyle( const QString & css )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETPROTOSTYLE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSetProtoStyle( HBQPlainTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSETPROTOSTYLE FP=( p )->hbSetProtoStyle( HBQPlainTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void           hbSelectAll()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSELECTALL )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
      ( p )->hbSelectAll();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_HBQPLAINTEXTEDIT_HBSELECTALL FP=( p )->hbSelectAll(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
