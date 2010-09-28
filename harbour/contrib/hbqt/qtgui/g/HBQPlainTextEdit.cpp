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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 56/56 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPlainTextEdit>

#include "hbqt_hbqplaintextedit.h"

/*
 * HBQPlainTextEdit ( QWidget * parent = 0 )
 * HBQPlainTextEdit ( const QString & text, QWidget * parent = 0 )
 * virtual ~HBQPlainTextEdit ()
 */

typedef struct
{
   QPointer< HBQPlainTextEdit > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQPlainTextEdit;

HBQT_GC_FUNC( hbqt_gcRelease_HBQPlainTextEdit )
{
   HBQPlainTextEdit  * ph = NULL ;
   HBQT_GC_T_HBQPlainTextEdit * p = ( HBQT_GC_T_HBQPlainTextEdit * ) Cargo;

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
   HBQT_GC_T_HBQPlainTextEdit * p = ( HBQT_GC_T_HBQPlainTextEdit * ) hb_gcAllocate( sizeof( HBQT_GC_T_HBQPlainTextEdit ), hbqt_gcFuncs() );

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
   {
      hb_retni( ( p )->hbGetIndex( *hbqt_par_QTextCursor( 2 ) ) );
   }
}

/*
 * int            hbGetLine( const QTextCursor & crQTextCursor)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETLINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->hbGetLine( *hbqt_par_QTextCursor( 2 ) ) );
   }
}

/*
 * int            hbLineNumberAreaWidth()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBLINENUMBERAREAWIDTH )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->hbLineNumberAreaWidth() );
   }
}

/*
 * int            hbGetSpaces()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSPACES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->hbGetSpaces() );
   }
}

/*
 * void           hbSetSpaces(int newSpaces)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSPACES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetSpaces( hb_parni( 2 ) );
   }
}

/*
 * void           hbBookmarks(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBOOKMARKS )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbBookmarks( hb_parni( 2 ) );
   }
}

/*
 * void           hbNextBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNEXTBOOKMARK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbNextBookmark( hb_parni( 2 ) );
   }
}

/*
 * void           hbPrevBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBPREVBOOKMARK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbPrevBookmark( hb_parni( 2 ) );
   }
}

/*
 * void           hbGotoBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGOTOBOOKMARK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbGotoBookmark( hb_parni( 2 ) );
   }
}

/*
 * void           hbNumberBlockVisible(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbNumberBlockVisible( hb_parl( 2 ) );
   }
}

/*
 * bool           hbNumberBlockVisible()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE_1 )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->hbNumberBlockVisible() );
   }
}

/*
 * void           hbHighlightCurrentLine(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbHighlightCurrentLine( hb_parl( 2 ) );
   }
}

/*
 * bool           hbHighlightCurrentLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE_1 )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->hbHighlightCurrentLine() );
   }
}

/*
 * void           hbSetEventBlock( PHB_ITEM block )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETEVENTBLOCK )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetEventBlock( hb_param( 2, HB_IT_ANY ) );
   }
}

/*
 * void           hbUpdateLineNumberAreaWidth( int newBlockCount )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUPDATELINENUMBERAREAWIDTH )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbUpdateLineNumberAreaWidth( hb_parni( 2 ) );
   }
}

/*
 * void           hbCaseUpper()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCASEUPPER )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbCaseUpper();
   }
}

/*
 * void           hbCaseLower()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCASELOWER )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbCaseLower();
   }
}

/*
 * void           hbEscapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBESCAPEQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbEscapeQuotes();
   }
}

/*
 * void           hbEscapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBESCAPEDQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbEscapeDQuotes();
   }
}

/*
 * void           hbUnescapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUNESCAPEQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbUnescapeQuotes();
   }
}

/*
 * void           hbUnescapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUNESCAPEDQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbUnescapeDQuotes();
   }
}

/*
 * void           hbConvertQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCONVERTQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbConvertQuotes();
   }
}

/*
 * void           hbConvertDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCONVERTDQUOTES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbConvertDQuotes();
   }
}

/*
 * void           hbBlockComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBLOCKCOMMENT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbBlockComment();
   }
}

/*
 * void           hbStreamComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSTREAMCOMMENT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbStreamComment();
   }
}

/*
 * void           hbDuplicateLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBDUPLICATELINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbDuplicateLine();
   }
}

/*
 * void           hbReplaceSelection( const QString & txt )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBREPLACESELECTION )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbReplaceSelection( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void           hbBlockIndent( int steps )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBLOCKINDENT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbBlockIndent( hb_parni( 2 ) );
   }
}

/*
 * void           hbDeleteLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBDELETELINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbDeleteLine();
   }
}

/*
 * void           hbMoveLine( int iDirection )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBMOVELINE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbMoveLine( hb_parni( 2 ) );
   }
}

/*
 * QString        hbGetSelectedText()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSELECTEDTEXT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->hbGetSelectedText().toUtf8().data() );
   }
}

/*
 * QString        hbTextUnderCursor( bool bCodeComplete )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBTEXTUNDERCURSOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->hbTextUnderCursor( hb_parl( 2 ) ).toUtf8().data() );
   }
}

/*
 * void           hbShowPrototype( const QString & tip, int rows, int cols )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSHOWPROTOTYPE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbShowPrototype( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), hb_parni( 4 ) );
      hb_strfree( pText );
   }
}

/*
 * void           hbSetCompleter( QCompleter * completer )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETCOMPLETER )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetCompleter( hbqt_par_QCompleter( 2 ) );
   }
}

/*
 * void           hbSetFldsCompleter( QCompleter * completer )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETFLDSCOMPLETER )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetFldsCompleter( hbqt_par_QCompleter( 2 ) );
   }
}

/*
 * void           hbSetCurrentLineColor( const QColor & color )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETCURRENTLINECOLOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetCurrentLineColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void           hbSetLineAreaBkColor( const QColor & color )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETLINEAREABKCOLOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetLineAreaBkColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void           hbRefresh()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBREFRESH )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbRefresh();
   }
}

/*
 * void           hbCut( int key )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCUT )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbCut( hb_parni( 2 ) );
   }
}

/*
 * void           hbCopy()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCOPY )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbCopy();
   }
}

/*
 * void           hbPaste()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBPASTE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbPaste();
   }
}

/*
 * void           hbSetSelectionMode( int mode, bool on )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSELECTIONMODE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetSelectionMode( hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void           hbGetSelectionInfo()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSELECTIONINFO )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbGetSelectionInfo();
   }
}

/*
 * void           hbSetSelectionInfo( PHB_ITEM selectionInfo )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSELECTIONINFO )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetSelectionInfo( hb_param( 2, HB_IT_ANY ) );
   }
}

/*
 * void           hbSetSelectionColor( const QColor & color )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSELECTIONCOLOR )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetSelectionColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void           hbSetMatchBraces( bool all )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETMATCHBRACES )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetMatchBraces( hb_parl( 2 ) );
   }
}

/*
 * void           hbGetViewportInfo()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETVIEWPORTINFO )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbGetViewportInfo();
   }
}

/*
 * void           hbApplyKey( int key, Qt::KeyboardModifiers modifiers = 0, const QString & txt )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBAPPLYKEY )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbApplyKey( hb_parni( 2 ), ( Qt::KeyboardModifiers ) hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void           hbHighlightArea( int top, int left, int bottom, int right, int mode )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTAREA )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbHighlightArea( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ) );
   }
}

/*
 * void           hbTogglePersistentSelection()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBTOGGLEPERSISTENTSELECTION )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbTogglePersistentSelection();
   }
}

/*
 * void           hbHorzRulerVisible( bool visible )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHORZRULERVISIBLE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbHorzRulerVisible( hb_parl( 2 ) );
   }
}

/*
 * void           hbSetProtoStyle( const QString & css )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETPROTOSTYLE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->hbSetProtoStyle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void           hbSelectAll()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSELECTALL )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSelectAll();
   }
}

/*
 * void           hbSetFieldsListActive( bool active )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETFIELDSLISTACTIVE )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbSetFieldsListActive( hb_parl( 2 ) );
   }
}

/*
 * void           hbToggleCodeCompetion()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBTOGGLECODECOMPETION )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbToggleCodeCompetion();
   }
}

/*
 * void           hbToggleCompetionTips()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBTOGGLECOMPETIONTIPS )
{
   HBQPlainTextEdit * p = hbqt_par_HBQPlainTextEdit( 1 );
   if( p )
   {
      ( p )->hbToggleCompetionTips();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
