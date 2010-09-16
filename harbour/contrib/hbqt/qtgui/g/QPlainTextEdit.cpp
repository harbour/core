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
   {
      hb_retl( ( p )->backgroundVisible() );
   }
}

/*
 * int blockCount () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_BLOCKCOUNT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->blockCount() );
   }
}

/*
 * bool canPaste () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CANPASTE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->canPaste() );
   }
}

/*
 * bool centerOnScroll () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CENTERONSCROLL )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->centerOnScroll() );
   }
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CREATESTANDARDCONTEXTMENU )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
   }
}

/*
 * QTextCharFormat currentCharFormat () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURRENTCHARFORMAT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->currentCharFormat() ), true ) );
   }
}

/*
 * QTextCursor cursorForPosition ( const QPoint & pos ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORFORPOSITION )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->cursorForPosition( *hbqt_par_QPoint( 2 ) ) ), true ) );
   }
}

/*
 * QRect cursorRect ( const QTextCursor & cursor ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORRECT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect( *hbqt_par_QTextCursor( 2 ) ) ), true ) );
   }
}

/*
 * QRect cursorRect () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORRECT_1 )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect() ), true ) );
   }
}

/*
 * int cursorWidth () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->cursorWidth() );
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_DOCUMENT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   }
}

/*
 * QString documentTitle () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_DOCUMENTTITLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->documentTitle().toUtf8().data() );
   }
}

/*
 * void ensureCursorVisible ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ENSURECURSORVISIBLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->ensureCursorVisible();
   }
}

/*
 * bool find ( const QString & exp, QTextDocument::FindFlags options = 0 )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_FIND )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->find( hb_parstr_utf8( 2, &pText, NULL ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ISREADONLY )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->isReadOnly() );
   }
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ISUNDOREDOENABLED )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->isUndoRedoEnabled() );
   }
}

/*
 * LineWrapMode lineWrapMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_LINEWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( QPlainTextEdit::LineWrapMode ) ( p )->lineWrapMode() );
   }
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_LOADRESOURCE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
   }
}

/*
 * int maximumBlockCount () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MAXIMUMBLOCKCOUNT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->maximumBlockCount() );
   }
}

/*
 * void mergeCurrentCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MERGECURRENTCHARFORMAT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->mergeCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * void moveCursor ( QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode = QTextCursor::MoveAnchor )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MOVECURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) );
   }
}

/*
 * bool overwriteMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_OVERWRITEMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->overwriteMode() );
   }
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_PRINT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->print( hbqt_par_QPrinter( 2 ) );
   }
}

/*
 * void setBackgroundVisible ( bool visible )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETBACKGROUNDVISIBLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setBackgroundVisible( hb_parl( 2 ) );
   }
}

/*
 * void setCenterOnScroll ( bool enabled )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCENTERONSCROLL )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setCenterOnScroll( hb_parl( 2 ) );
   }
}

/*
 * void setCurrentCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCURRENTCHARFORMAT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   }
}

/*
 * void setCursorWidth ( int width )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCURSORWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setCursorWidth( hb_parni( 2 ) );
   }
}

/*
 * void setDocument ( QTextDocument * document )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETDOCUMENT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setDocument( hbqt_par_QTextDocument( 2 ) );
   }
}

/*
 * void setDocumentTitle ( const QString & title )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETDOCUMENTTITLE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->setDocumentTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setLineWrapMode ( LineWrapMode mode )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETLINEWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setLineWrapMode( ( QPlainTextEdit::LineWrapMode ) hb_parni( 2 ) );
   }
}

/*
 * void setMaximumBlockCount ( int maximum )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETMAXIMUMBLOCKCOUNT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setMaximumBlockCount( hb_parni( 2 ) );
   }
}

/*
 * void setOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETOVERWRITEMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setOverwriteMode( hb_parl( 2 ) );
   }
}

/*
 * void setReadOnly ( bool ro )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETREADONLY )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setReadOnly( hb_parl( 2 ) );
   }
}

/*
 * void setTabChangesFocus ( bool b )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTABCHANGESFOCUS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setTabChangesFocus( hb_parl( 2 ) );
   }
}

/*
 * void setTabStopWidth ( int width )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTABSTOPWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setTabStopWidth( hb_parni( 2 ) );
   }
}

/*
 * void setTextCursor ( const QTextCursor & cursor )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTEXTCURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setTextCursor( *hbqt_par_QTextCursor( 2 ) );
   }
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTEXTINTERACTIONFLAGS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
   }
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETUNDOREDOENABLED )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setUndoRedoEnabled( hb_parl( 2 ) );
   }
}

/*
 * void setWordWrapMode ( QTextOption::WrapMode policy )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETWORDWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->setWordWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) );
   }
}

/*
 * bool tabChangesFocus () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TABCHANGESFOCUS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retl( ( p )->tabChangesFocus() );
   }
}

/*
 * int tabStopWidth () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TABSTOPWIDTH )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( p )->tabStopWidth() );
   }
}

/*
 * QTextCursor textCursor () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TEXTCURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->textCursor() ), true ) );
   }
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TEXTINTERACTIONFLAGS )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( Qt::TextInteractionFlags ) ( p )->textInteractionFlags() );
   }
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TOPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toPlainText().toUtf8().data() );
   }
}

/*
 * QTextOption::WrapMode wordWrapMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_WORDWRAPMODE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      hb_retni( ( QTextOption::WrapMode ) ( p )->wordWrapMode() );
   }
}

/*
 * void appendHtml ( const QString & html )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_APPENDHTML )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->appendHtml( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void appendPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_APPENDPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->appendPlainText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void centerCursor ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CENTERCURSOR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->centerCursor();
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CLEAR )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * void copy ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_COPY )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->copy();
   }
}

/*
 * void cut ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CUT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->cut();
   }
}

/*
 * void insertPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_INSERTPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertPlainText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void paste ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_PASTE )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->paste();
   }
}

/*
 * void redo ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_REDO )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->redo();
   }
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SELECTALL )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->selectAll();
   }
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETPLAINTEXT )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPlainText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void undo ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_UNDO )
{
   QPlainTextEdit * p = hbqt_par_QPlainTextEdit( 1 );
   if( p )
   {
      ( p )->undo();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
