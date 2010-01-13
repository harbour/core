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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum LineWrapMode { NoWrap, WidgetWidth }
 */

/*
 *  Constructed[ 57/64 [ 89.06% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QTextEdit::ExtraSelection> extraSelections () const
 *  void setExtraSelections ( const QList<QTextEdit::ExtraSelection> & selections )
 *  15 public functions inherited from QAbstractScrollArea
 *  13 public functions inherited from QFrame
 *  207 public functions inherited from QWidget
 *  29 public functions inherited from QObject
 *  12 public functions inherited from QPaintDevice
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
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QPlainTextEdit > pq;
} QGC_POINTER_QPlainTextEdit;

QT_G_FUNC( hbqt_gcRelease_QPlainTextEdit )
{
   QGC_POINTER_QPlainTextEdit * p = ( QGC_POINTER_QPlainTextEdit * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QPlainTextEdit               p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QPlainTextEdit              ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QPlainTextEdit * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QPlainTextEdit * ) p->ph )->~QPlainTextEdit();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QPlainTextEdit * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QPlainTextEdit              Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QPlainTextEdit              Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QPlainTextEdit              Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QPlainTextEdit( void * pObj )
{
   QGC_POINTER_QPlainTextEdit * p = ( QGC_POINTER_QPlainTextEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_QPlainTextEdit ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QPlainTextEdit;
   new( & p->pq ) QPointer< QPlainTextEdit >( ( QPlainTextEdit * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QPlainTextEdit              %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QPLAINTEXTEDIT )
{
   void * pObj = NULL;

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

   hb_retptrGC( hbqt_gcAllocate_QPlainTextEdit( pObj ) );
}
/*
 * bool backgroundVisible () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_BACKGROUNDVISIBLE )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->backgroundVisible() );
}

/*
 * int blockCount () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_BLOCKCOUNT )
{
   hb_retni( hbqt_par_QPlainTextEdit( 1 )->blockCount() );
}

/*
 * bool canPaste () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CANPASTE )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->canPaste() );
}

/*
 * bool centerOnScroll () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CENTERONSCROLL )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->centerOnScroll() );
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CREATESTANDARDCONTEXTMENU )
{
   hb_retptr( ( QMenu* ) hbqt_par_QPlainTextEdit( 1 )->createStandardContextMenu() );
}

/*
 * QTextCharFormat currentCharFormat () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURRENTCHARFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( hbqt_par_QPlainTextEdit( 1 )->currentCharFormat() ) ) );
}

/*
 * QTextCursor cursorForPosition ( const QPoint & pos ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORFORPOSITION )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QPlainTextEdit( 1 )->cursorForPosition( *hbqt_par_QPoint( 2 ) ) ) ) );
}

/*
 * QRect cursorRect ( const QTextCursor & cursor ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QPlainTextEdit( 1 )->cursorRect( *hbqt_par_QTextCursor( 2 ) ) ) ) );
}

/*
 * QRect cursorRect () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORRECT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QPlainTextEdit( 1 )->cursorRect() ) ) );
}

/*
 * int cursorWidth () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CURSORWIDTH )
{
   hb_retni( hbqt_par_QPlainTextEdit( 1 )->cursorWidth() );
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_DOCUMENT )
{
   hb_retptr( ( QTextDocument* ) hbqt_par_QPlainTextEdit( 1 )->document() );
}

/*
 * QString documentTitle () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_DOCUMENTTITLE )
{
   hb_retc( hbqt_par_QPlainTextEdit( 1 )->documentTitle().toAscii().data() );
}

/*
 * void ensureCursorVisible ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ENSURECURSORVISIBLE )
{
   hbqt_par_QPlainTextEdit( 1 )->ensureCursorVisible();
}

/*
 * bool find ( const QString & exp, QTextDocument::FindFlags options = 0 )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_FIND )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->find( QPlainTextEdit::tr( hb_parc( 2 ) ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ISREADONLY )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->isReadOnly() );
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_ISUNDOREDOENABLED )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->isUndoRedoEnabled() );
}

/*
 * LineWrapMode lineWrapMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_LINEWRAPMODE )
{
   hb_retni( ( QPlainTextEdit::LineWrapMode ) hbqt_par_QPlainTextEdit( 1 )->lineWrapMode() );
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_LOADRESOURCE )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QPlainTextEdit( 1 )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ) ) );
}

/*
 * int maximumBlockCount () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MAXIMUMBLOCKCOUNT )
{
   hb_retni( hbqt_par_QPlainTextEdit( 1 )->maximumBlockCount() );
}

/*
 * void mergeCurrentCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MERGECURRENTCHARFORMAT )
{
   hbqt_par_QPlainTextEdit( 1 )->mergeCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/*
 * void moveCursor ( QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode = QTextCursor::MoveAnchor )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_MOVECURSOR )
{
   hbqt_par_QPlainTextEdit( 1 )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) );
}

/*
 * bool overwriteMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_OVERWRITEMODE )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->overwriteMode() );
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_PRINT )
{
   hbqt_par_QPlainTextEdit( 1 )->print( hbqt_par_QPrinter( 2 ) );
}

/*
 * void setBackgroundVisible ( bool visible )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETBACKGROUNDVISIBLE )
{
   hbqt_par_QPlainTextEdit( 1 )->setBackgroundVisible( hb_parl( 2 ) );
}

/*
 * void setCenterOnScroll ( bool enabled )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCENTERONSCROLL )
{
   hbqt_par_QPlainTextEdit( 1 )->setCenterOnScroll( hb_parl( 2 ) );
}

/*
 * void setCurrentCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCURRENTCHARFORMAT )
{
   hbqt_par_QPlainTextEdit( 1 )->setCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/*
 * void setCursorWidth ( int width )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETCURSORWIDTH )
{
   hbqt_par_QPlainTextEdit( 1 )->setCursorWidth( hb_parni( 2 ) );
}

/*
 * void setDocument ( QTextDocument * document )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETDOCUMENT )
{
   hbqt_par_QPlainTextEdit( 1 )->setDocument( hbqt_par_QTextDocument( 2 ) );
}

/*
 * void setDocumentTitle ( const QString & title )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETDOCUMENTTITLE )
{
   hbqt_par_QPlainTextEdit( 1 )->setDocumentTitle( QPlainTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void setLineWrapMode ( LineWrapMode mode )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETLINEWRAPMODE )
{
   hbqt_par_QPlainTextEdit( 1 )->setLineWrapMode( ( QPlainTextEdit::LineWrapMode ) hb_parni( 2 ) );
}

/*
 * void setMaximumBlockCount ( int maximum )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETMAXIMUMBLOCKCOUNT )
{
   hbqt_par_QPlainTextEdit( 1 )->setMaximumBlockCount( hb_parni( 2 ) );
}

/*
 * void setOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETOVERWRITEMODE )
{
   hbqt_par_QPlainTextEdit( 1 )->setOverwriteMode( hb_parl( 2 ) );
}

/*
 * void setReadOnly ( bool ro )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETREADONLY )
{
   hbqt_par_QPlainTextEdit( 1 )->setReadOnly( hb_parl( 2 ) );
}

/*
 * void setTabChangesFocus ( bool b )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTABCHANGESFOCUS )
{
   hbqt_par_QPlainTextEdit( 1 )->setTabChangesFocus( hb_parl( 2 ) );
}

/*
 * void setTabStopWidth ( int width )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTABSTOPWIDTH )
{
   hbqt_par_QPlainTextEdit( 1 )->setTabStopWidth( hb_parni( 2 ) );
}

/*
 * void setTextCursor ( const QTextCursor & cursor )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTEXTCURSOR )
{
   hbqt_par_QPlainTextEdit( 1 )->setTextCursor( *hbqt_par_QTextCursor( 2 ) );
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETTEXTINTERACTIONFLAGS )
{
   hbqt_par_QPlainTextEdit( 1 )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETUNDOREDOENABLED )
{
   hbqt_par_QPlainTextEdit( 1 )->setUndoRedoEnabled( hb_parl( 2 ) );
}

/*
 * void setWordWrapMode ( QTextOption::WrapMode policy )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETWORDWRAPMODE )
{
   hbqt_par_QPlainTextEdit( 1 )->setWordWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) );
}

/*
 * bool tabChangesFocus () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TABCHANGESFOCUS )
{
   hb_retl( hbqt_par_QPlainTextEdit( 1 )->tabChangesFocus() );
}

/*
 * int tabStopWidth () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TABSTOPWIDTH )
{
   hb_retni( hbqt_par_QPlainTextEdit( 1 )->tabStopWidth() );
}

/*
 * QTextCursor textCursor () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TEXTCURSOR )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QPlainTextEdit( 1 )->textCursor() ) ) );
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TEXTINTERACTIONFLAGS )
{
   hb_retni( ( Qt::TextInteractionFlags ) hbqt_par_QPlainTextEdit( 1 )->textInteractionFlags() );
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_TOPLAINTEXT )
{
   hb_retc( hbqt_par_QPlainTextEdit( 1 )->toPlainText().toAscii().data() );
}

/*
 * QTextOption::WrapMode wordWrapMode () const
 */
HB_FUNC( QT_QPLAINTEXTEDIT_WORDWRAPMODE )
{
   hb_retni( ( QTextOption::WrapMode ) hbqt_par_QPlainTextEdit( 1 )->wordWrapMode() );
}

/*
 * void appendHtml ( const QString & html )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_APPENDHTML )
{
   hbqt_par_QPlainTextEdit( 1 )->appendHtml( QPlainTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void appendPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_APPENDPLAINTEXT )
{
   hbqt_par_QPlainTextEdit( 1 )->appendPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void centerCursor ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CENTERCURSOR )
{
   hbqt_par_QPlainTextEdit( 1 )->centerCursor();
}

/*
 * void clear ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CLEAR )
{
   hbqt_par_QPlainTextEdit( 1 )->clear();
}

/*
 * void copy ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_COPY )
{
   hbqt_par_QPlainTextEdit( 1 )->copy();
}

/*
 * void cut ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_CUT )
{
   hbqt_par_QPlainTextEdit( 1 )->cut();
}

/*
 * void insertPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_INSERTPLAINTEXT )
{
   hbqt_par_QPlainTextEdit( 1 )->insertPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void paste ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_PASTE )
{
   hbqt_par_QPlainTextEdit( 1 )->paste();
}

/*
 * void redo ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_REDO )
{
   hbqt_par_QPlainTextEdit( 1 )->redo();
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SELECTALL )
{
   hbqt_par_QPlainTextEdit( 1 )->selectAll();
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QPLAINTEXTEDIT_SETPLAINTEXT )
{
   hbqt_par_QPlainTextEdit( 1 )->setPlainText( QPlainTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void undo ()
 */
HB_FUNC( QT_QPLAINTEXTEDIT_UNDO )
{
   hbqt_par_QPlainTextEdit( 1 )->undo();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
