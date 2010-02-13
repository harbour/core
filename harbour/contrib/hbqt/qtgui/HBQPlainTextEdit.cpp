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

#include "hbapi.h"
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
 * int            getIndex( const QTextCursor & crQTextCursor)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_GETINDEX )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->getIndex( *hbqt_par_QTextCursor( 2 ) ) );
}

/*
 * int            getLine( const QTextCursor & crQTextCursor)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_GETLINE )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->getLine( *hbqt_par_QTextCursor( 2 ) ) );
}

/*
 * int            lineNumberAreaWidth()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_LINENUMBERAREAWIDTH )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->lineNumberAreaWidth() );
}

/*
 * int            getSpaces()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_GETSPACES )
{
   hb_retni( hbqt_par_HBQPlainTextEdit( 1 )->getSpaces() );
}

/*
 * void           setSpaces(int newSpaces)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_SETSPACES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->setSpaces( hb_parni( 2 ) );
}

/*
 * void           bookmarks(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_BOOKMARKS )
{
   hbqt_par_HBQPlainTextEdit( 1 )->bookmarks( hb_parni( 2 ) );
}

/*
 * void           nextBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_NEXTBOOKMARK )
{
   hbqt_par_HBQPlainTextEdit( 1 )->nextBookmark( hb_parni( 2 ) );
}

/*
 * void           prevBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_PREVBOOKMARK )
{
   hbqt_par_HBQPlainTextEdit( 1 )->prevBookmark( hb_parni( 2 ) );
}

/*
 * void           gotoBookmark(int block)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_GOTOBOOKMARK )
{
   hbqt_par_HBQPlainTextEdit( 1 )->gotoBookmark( hb_parni( 2 ) );
}

/*
 * void           numberBlockVisible(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_NUMBERBLOCKVISIBLE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->numberBlockVisible( hb_parl( 2 ) );
}

/*
 * bool           numberBlockVisible()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_NUMBERBLOCKVISIBLE_1 )
{
   hb_retl( hbqt_par_HBQPlainTextEdit( 1 )->numberBlockVisible() );
}

/*
 * void           highlightCurrentLine(bool b)
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HIGHLIGHTCURRENTLINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->highlightCurrentLine( hb_parl( 2 ) );
}

/*
 * bool           highlightCurrentLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HIGHLIGHTCURRENTLINE_1 )
{
   hb_retl( hbqt_par_HBQPlainTextEdit( 1 )->highlightCurrentLine() );
}

/*
 * void           updateLineNumberAreaWidth( int newBlockCount )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_UPDATELINENUMBERAREAWIDTH )
{
   hbqt_par_HBQPlainTextEdit( 1 )->updateLineNumberAreaWidth( hb_parni( 2 ) );
}

/*
 * void           caseUpper()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_CASEUPPER )
{
   hbqt_par_HBQPlainTextEdit( 1 )->caseUpper();
}

/*
 * void           caseLower()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_CASELOWER )
{
   hbqt_par_HBQPlainTextEdit( 1 )->caseLower();
}

/*
 * void           escapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_ESCAPEQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->escapeQuotes();
}

/*
 * void           escapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_ESCAPEDQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->escapeDQuotes();
}

/*
 * void           unescapeQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_UNESCAPEQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->unescapeQuotes();
}

/*
 * void           unescapeDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_UNESCAPEDQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->unescapeDQuotes();
}

/*
 * void           convertQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_CONVERTQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->convertQuotes();
}

/*
 * void           convertDQuotes()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_CONVERTDQUOTES )
{
   hbqt_par_HBQPlainTextEdit( 1 )->convertDQuotes();
}

/*
 * void           blockComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_BLOCKCOMMENT )
{
   hbqt_par_HBQPlainTextEdit( 1 )->blockComment();
}

/*
 * void           streamComment()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_STREAMCOMMENT )
{
   hbqt_par_HBQPlainTextEdit( 1 )->streamComment();
}

/*
 * void           duplicateLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_DUPLICATELINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->duplicateLine();
}

/*
 * void           replaceSelection( const QString & txt )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_REPLACESELECTION )
{
   hbqt_par_HBQPlainTextEdit( 1 )->replaceSelection( HBQPlainTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void           blockIndent( int steps )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_BLOCKINDENT )
{
   hbqt_par_HBQPlainTextEdit( 1 )->blockIndent( hb_parni( 2 ) );
}

/*
 * void           deleteLine()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_DELETELINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->deleteLine();
}

/*
 * void           moveLine( int iDirection )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_MOVELINE )
{
   hbqt_par_HBQPlainTextEdit( 1 )->moveLine( hb_parni( 2 ) );
}

/*
 * void           highlightSelectedColumns( bool yes )
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_HIGHLIGHTSELECTEDCOLUMNS )
{
   hbqt_par_HBQPlainTextEdit( 1 )->highlightSelectedColumns( hb_parl( 2 ) );
}

/*
 * QString        getSelectedText()
 */
HB_FUNC( QT_HBQPLAINTEXTEDIT_GETSELECTEDTEXT )
{
   hb_retc( hbqt_par_HBQPlainTextEdit( 1 )->getSelectedText().toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
