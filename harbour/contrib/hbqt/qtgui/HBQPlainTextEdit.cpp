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

#include <QtGui/QPlainTextEdit>

#include "../hbqt_hbqplaintextedit.h"

/*
 * HBQPlainTextEdit ( QWidget * parent = 0 )
 * HBQPlainTextEdit ( const QString & text, QWidget * parent = 0 )
 * virtual ~HBQPlainTextEdit ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< HBQPlainTextEdit > pq;
} QGC_POINTER_HBQPlainTextEdit;

QT_G_FUNC( hbqt_gcRelease_HBQPlainTextEdit )
{
   QGC_POINTER_HBQPlainTextEdit * p = ( QGC_POINTER_HBQPlainTextEdit * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_HBQPlainTextEdit   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( HBQPlainTextEdit * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_HBQPlainTextEdit   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_HBQPlainTextEditph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_HBQPlainTextEdit    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_HBQPlainTextEdit    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQPlainTextEdit( void * pObj, bool bNew )
{
   QGC_POINTER_HBQPlainTextEdit * p = ( QGC_POINTER_HBQPlainTextEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_HBQPlainTextEdit ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQPlainTextEdit;

   if( bNew )
   {
      new( & p->pq ) QPointer< HBQPlainTextEdit >( ( HBQPlainTextEdit * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_HBQPlainTextEdit           ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_HBQPLAINTEXTEDIT )
{
   void * pObj = NULL;

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

   hb_retptrGC( hbqt_gcAllocate_HBQPlainTextEdit( pObj, true ) );
}

/*
 * int            hbGetIndex( const QTextCursor & crQTextCursor)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETINDEX )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->hbGetIndex( *hbqt_par_QTextCursor( 2 ) ) );
}

/*
 * int            hbGetLine( const QTextCursor & crQTextCursor)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETLINE )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->hbGetLine( *hbqt_par_QTextCursor( 2 ) ) );
}

/*
 * int            hbLineNumberAreaWidth()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBLINENUMBERAREAWIDTH )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->hbLineNumberAreaWidth() );
}

/*
 * int            hbGetSpaces()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSPACES )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->hbGetSpaces() );
}

/*
 * void           hbSetSpaces(int newSpaces)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETSPACES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbSetSpaces( hb_parni( 2 ) );
}

/*
 * void           hbBookmarks(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBOOKMARKS )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbBookmarks( hb_parni( 2 ) );
}

/*
 * void           hbNextBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNEXTBOOKMARK )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbNextBookmark( hb_parni( 2 ) );
}

/*
 * void           hbPrevBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBPREVBOOKMARK )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbPrevBookmark( hb_parni( 2 ) );
}

/*
 * void           hbGotoBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGOTOBOOKMARK )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbGotoBookmark( hb_parni( 2 ) );
}

/*
 * void           hbNumberBlockVisible(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbNumberBlockVisible( hb_parl( 2 ) );
}

/*
 * bool           hbNumberBlockVisible()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBNUMBERBLOCKVISIBLE_1 )
{
   hb_retl( hbqt_par_HBQPlainTextEdit( 1 )->hbNumberBlockVisible() );
}

/*
 * void           hbHighlightCurrentLine(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbHighlightCurrentLine( hb_parl( 2 ) );
}

/*
 * bool           hbHighlightCurrentLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTCURRENTLINE_1 )
{
   hb_retl( hbqt_par_HBQPlainTextEdit( 1 )->hbHighlightCurrentLine() );
}

/*
 * void           hbSetEventBlock( PHB_ITEM block )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSETEVENTBLOCK )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbSetEventBlock( hb_param( 2, HB_IT_ANY ) );
}

/*
 * void           hbUpdateLineNumberAreaWidth( int newBlockCount )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUPDATELINENUMBERAREAWIDTH )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbUpdateLineNumberAreaWidth( hb_parni( 2 ) );
}

/*
 * void           hbCaseUpper()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCASEUPPER )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbCaseUpper();
}

/*
 * void           hbCaseLower()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCASELOWER )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbCaseLower();
}

/*
 * void           hbEscapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBESCAPEQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbEscapeQuotes();
}

/*
 * void           hbEscapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBESCAPEDQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbEscapeDQuotes();
}

/*
 * void           hbUnescapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUNESCAPEQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbUnescapeQuotes();
}

/*
 * void           hbUnescapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBUNESCAPEDQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbUnescapeDQuotes();
}

/*
 * void           hbConvertQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCONVERTQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbConvertQuotes();
}

/*
 * void           hbConvertDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBCONVERTDQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbConvertDQuotes();
}

/*
 * void           hbBlockComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBLOCKCOMMENT )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbBlockComment();
}

/*
 * void           hbStreamComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSTREAMCOMMENT )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbStreamComment();
}

/*
 * void           hbDuplicateLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBDUPLICATELINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbDuplicateLine();
}

/*
 * void           hbReplaceSelection( const QString & txt )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBREPLACESELECTION )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbReplaceSelection( HBQPlainTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void           hbBlockIndent( int steps )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBBLOCKINDENT )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbBlockIndent( hb_parni( 2 ) );
}

/*
 * void           hbDeleteLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBDELETELINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbDeleteLine();
}

/*
 * void           hbMoveLine( int iDirection )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBMOVELINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbMoveLine( hb_parni( 2 ) );
}

/*
 * void           hbHighlightSelectedColumns( bool yes )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBHIGHLIGHTSELECTEDCOLUMNS )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbHighlightSelectedColumns( hb_parl( 2 ) );
}

/*
 * QString        hbGetSelectedText()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBGETSELECTEDTEXT )
{
   hb_retc( hbqt_par_HBQPlainTextEdit( 1 )->hbGetSelectedText().toAscii().data() );
}

/*
 * void           hbShowPrototype( const QString & tip )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HBSHOWPROTOTYPE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->hbShowPrototype( HBQPlainTextEdit::tr( hb_parc( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
