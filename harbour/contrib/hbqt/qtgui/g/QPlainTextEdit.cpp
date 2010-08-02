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

/*
 *  enum LineWrapMode { NoWrap, WidgetWidth }
 */

/*
 *  Constructed[ 57/59 [ 96.61% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QTextEdit::ExtraSelection> extraSelections () const
 *  void setExtraSelections ( const QList<QTextEdit::ExtraSelection> & selections )
 */

#include <QtCore/QPointer>

#include <QtGui/QPlainTextEdit>


/*
 * QPlainTextEdit ( QWidget * parent = 0 )
 * QPlainTextEdit ( const QString & text, QWidget * parent = 0 )
 * virtual ~QPlainTextEdit ()
 */

typedef struct
{
   QPointer< QPlainTextEdit > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPlainTextEdit;

HBQT_GC_FUNC( hbqt_gcRelease_QPlainTextEdit )
{
   QPlainTextEdit  * ph = NULL ;
   HBQT_GC_T_QPlainTextEdit * p = ( HBQT_GC_T_QPlainTextEdit * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPlainTextEdit   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPlainTextEdit   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QPlainTextEdit          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPlainTextEdit    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPlainTextEdit    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPlainTextEdit( void * pObj, bool bNew )
{
   HBQT_GC_T_QPlainTextEdit * p = ( HBQT_GC_T_QPlainTextEdit * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPlainTextEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPlainTextEdit >( ( QPlainTextEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPlainTextEdit;
   p->type = HBQT_TYPE_QPlainTextEdit;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPlainTextEdit  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPlainTextEdit", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPLAINTEXTEDIT )
{
   QPlainTextEdit * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QPlainTextEdit() ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPlainTextEdit( hbqt_par_QWidget( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QPlainTextEdit( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   }
   else
   {
      pObj = new QPlainTextEdit() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPlainTextEdit( ( void * ) pObj, true ) );
}

/*
 * bool backgroundVisible () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_BACKGROUNDVISIBLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->backgroundVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_BACKGROUNDVISIBLE FP=hb_retl( ( p )->backgroundVisible() ); p is NULL" ) );
   }
}

/*
 * int blockCount () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_BLOCKCOUNT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->blockCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_BLOCKCOUNT FP=hb_retni( ( p )->blockCount() ); p is NULL" ) );
   }
}

/*
 * bool canPaste () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CANPASTE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->canPaste() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CANPASTE FP=hb_retl( ( p )->canPaste() ); p is NULL" ) );
   }
}

/*
 * bool centerOnScroll () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CENTERONSCROLL )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->centerOnScroll() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CENTERONSCROLL FP=hb_retl( ( p )->centerOnScroll() ); p is NULL" ) );
   }
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CREATESTANDARDCONTEXTMENU )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CREATESTANDARDCONTEXTMENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat currentCharFormat () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURRENTCHARFORMAT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->currentCharFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CURRENTCHARFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->currentCharFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor cursorForPosition ( const QPoint & pos ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORFORPOSITION )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->cursorForPosition( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CURSORFORPOSITION FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->cursorForPosition( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect cursorRect ( const QTextCursor & cursor ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORRECT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect( *hbqt_par_QTextCursor( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CURSORRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect( *hbqt_par_QTextCursor( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect cursorRect () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORRECT_1 )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CURSORRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * int cursorWidth () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->cursorWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CURSORWIDTH FP=hb_retni( ( p )->cursorWidth() ); p is NULL" ) );
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_DOCUMENT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_DOCUMENT FP=hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) ); p is NULL" ) );
   }
}

/*
 * QString documentTitle () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_DOCUMENTTITLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retc( ( p )->documentTitle().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_DOCUMENTTITLE FP=hb_retc( ( p )->documentTitle().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void ensureCursorVisible ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ENSURECURSORVISIBLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->ensureCursorVisible();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_ENSURECURSORVISIBLE FP=( p )->ensureCursorVisible(); p is NULL" ) );
   }
}

/*
 * bool find ( const QString & exp, QTextDocument::FindFlags options = 0 )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_FIND )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->find( QPlainTextEdit::tr( hb_parc( 2 ) ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_FIND FP=hb_retl( ( p )->find( QPlainTextEdit::tr( hb_parc( 2 ) ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ISREADONLY )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_ISREADONLY FP=hb_retl( ( p )->isReadOnly() ); p is NULL" ) );
   }
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ISUNDOREDOENABLED )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->isUndoRedoEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_ISUNDOREDOENABLED FP=hb_retl( ( p )->isUndoRedoEnabled() ); p is NULL" ) );
   }
}

/*
 * LineWrapMode lineWrapMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_LINEWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retni( ( QPlainTextEdit::LineWrapMode ) ( p )->lineWrapMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_LINEWRAPMODE FP=hb_retni( ( QPlainTextEdit::LineWrapMode ) ( p )->lineWrapMode() ); p is NULL" ) );
   }
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_LOADRESOURCE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_LOADRESOURCE FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int maximumBlockCount () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MAXIMUMBLOCKCOUNT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->maximumBlockCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_MAXIMUMBLOCKCOUNT FP=hb_retni( ( p )->maximumBlockCount() ); p is NULL" ) );
   }
}

/*
 * void mergeCurrentCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MERGECURRENTCHARFORMAT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->mergeCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_MERGECURRENTCHARFORMAT FP=( p )->mergeCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void moveCursor ( QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode = QTextCursor::MoveAnchor )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MOVECURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_MOVECURSOR FP=( p )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) ); p is NULL" ) );
   }
}

/*
 * bool overwriteMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_OVERWRITEMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->overwriteMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_OVERWRITEMODE FP=hb_retl( ( p )->overwriteMode() ); p is NULL" ) );
   }
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_PRINT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->print( hbqt_par_QPrinter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_PRINT FP=( p )->print( hbqt_par_QPrinter( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackgroundVisible ( bool visible )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETBACKGROUNDVISIBLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setBackgroundVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETBACKGROUNDVISIBLE FP=( p )->setBackgroundVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCenterOnScroll ( bool enabled )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCENTERONSCROLL )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setCenterOnScroll( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETCENTERONSCROLL FP=( p )->setCenterOnScroll( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCURRENTCHARFORMAT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETCURRENTCHARFORMAT FP=( p )->setCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCursorWidth ( int width )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCURSORWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setCursorWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETCURSORWIDTH FP=( p )->setCursorWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocument ( QTextDocument * document )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETDOCUMENT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setDocument( hbqt_par_QTextDocument( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETDOCUMENT FP=( p )->setDocument( hbqt_par_QTextDocument( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocumentTitle ( const QString & title )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETDOCUMENTTITLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setDocumentTitle( QPlainTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETDOCUMENTTITLE FP=( p )->setDocumentTitle( QPlainTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setLineWrapMode ( LineWrapMode mode )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETLINEWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setLineWrapMode( ( QPlainTextEdit::LineWrapMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETLINEWRAPMODE FP=( p )->setLineWrapMode( ( QPlainTextEdit::LineWrapMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumBlockCount ( int maximum )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETMAXIMUMBLOCKCOUNT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setMaximumBlockCount( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETMAXIMUMBLOCKCOUNT FP=( p )->setMaximumBlockCount( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETOVERWRITEMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setOverwriteMode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETOVERWRITEMODE FP=( p )->setOverwriteMode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setReadOnly ( bool ro )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETREADONLY )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETREADONLY FP=( p )->setReadOnly( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabChangesFocus ( bool b )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTABCHANGESFOCUS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setTabChangesFocus( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETTABCHANGESFOCUS FP=( p )->setTabChangesFocus( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabStopWidth ( int width )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTABSTOPWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setTabStopWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETTABSTOPWIDTH FP=( p )->setTabStopWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextCursor ( const QTextCursor & cursor )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTEXTCURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setTextCursor( *hbqt_par_QTextCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETTEXTCURSOR FP=( p )->setTextCursor( *hbqt_par_QTextCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTEXTINTERACTIONFLAGS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETTEXTINTERACTIONFLAGS FP=( p )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETUNDOREDOENABLED )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setUndoRedoEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETUNDOREDOENABLED FP=( p )->setUndoRedoEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWordWrapMode ( QTextOption::WrapMode policy )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETWORDWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setWordWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETWORDWRAPMODE FP=( p )->setWordWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool tabChangesFocus () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TABCHANGESFOCUS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retl( ( p )->tabChangesFocus() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_TABCHANGESFOCUS FP=hb_retl( ( p )->tabChangesFocus() ); p is NULL" ) );
   }
}

/*
 * int tabStopWidth () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TABSTOPWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retni( ( p )->tabStopWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_TABSTOPWIDTH FP=hb_retni( ( p )->tabStopWidth() ); p is NULL" ) );
   }
}

/*
 * QTextCursor textCursor () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TEXTCURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->textCursor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_TEXTCURSOR FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->textCursor() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TEXTINTERACTIONFLAGS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retni( ( Qt::TextInteractionFlags ) ( p )->textInteractionFlags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_TEXTINTERACTIONFLAGS FP=hb_retni( ( Qt::TextInteractionFlags ) ( p )->textInteractionFlags() ); p is NULL" ) );
   }
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TOPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retc( ( p )->toPlainText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_TOPLAINTEXT FP=hb_retc( ( p )->toPlainText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextOption::WrapMode wordWrapMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_WORDWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      hb_retni( ( QTextOption::WrapMode ) ( p )->wordWrapMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_WORDWRAPMODE FP=hb_retni( ( QTextOption::WrapMode ) ( p )->wordWrapMode() ); p is NULL" ) );
   }
}

/*
 * void appendHtml ( const QString & html )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_APPENDHTML )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->appendHtml( QPlainTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_APPENDHTML FP=( p )->appendHtml( QPlainTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void appendPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_APPENDPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->appendPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_APPENDPLAINTEXT FP=( p )->appendPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void centerCursor ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CENTERCURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->centerCursor();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CENTERCURSOR FP=( p )->centerCursor(); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CLEAR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * void copy ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_COPY )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->copy();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_COPY FP=( p )->copy(); p is NULL" ) );
   }
}

/*
 * void cut ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CUT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->cut();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_CUT FP=( p )->cut(); p is NULL" ) );
   }
}

/*
 * void insertPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_INSERTPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->insertPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_INSERTPLAINTEXT FP=( p )->insertPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void paste ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_PASTE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->paste();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_PASTE FP=( p )->paste(); p is NULL" ) );
   }
}

/*
 * void redo ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_REDO )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->redo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_REDO FP=( p )->redo(); p is NULL" ) );
   }
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SELECTALL )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->selectAll();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SELECTALL FP=( p )->selectAll(); p is NULL" ) );
   }
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->setPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_SETPLAINTEXT FP=( p )->setPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void undo ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_UNDO )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
      ( p )->undo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPLAINTEXTEDIT_UNDO FP=( p )->undo(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
