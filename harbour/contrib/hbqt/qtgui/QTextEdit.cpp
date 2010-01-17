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

/*
 *  flags AutoFormatting
 *  enum AutoFormattingFlag { AutoNone, AutoBulletList, AutoAll }
 *  enum LineWrapMode { NoWrap, WidgetWidth, FixedPixelWidth, FixedColumnWidth }
 */

/*
 *  Constructed[ 81/83 [ 97.59% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<ExtraSelection> extraSelections () const
 *  void setExtraSelections ( const QList<ExtraSelection> & selections )
 */

#include <QtCore/QPointer>

#include <QtGui/QTextEdit>


/* QTextEdit ( QWidget * parent = 0 )
 * QTextEdit ( const QString & text, QWidget * parent = 0 )
 * virtual ~QTextEdit ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QTextEdit > pq;
} QGC_POINTER_QTextEdit;

QT_G_FUNC( hbqt_gcRelease_QTextEdit )
{
   QGC_POINTER_QTextEdit * p = ( QGC_POINTER_QTextEdit * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QTextEdit * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextEdit                  ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QTextEdit                  ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextEdit                   Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextEdit                   Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextEdit( void * pObj, bool bNew )
{
   QGC_POINTER_QTextEdit * p = ( QGC_POINTER_QTextEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextEdit ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextEdit;

   if( bNew )
   {
      new( & p->pq ) QPointer< QTextEdit >( ( QTextEdit * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextEdit                  ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTEDIT )
{
   void * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISCHAR( 1 ) )
      pObj = new QTextEdit( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = new QTextEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextEdit( pObj, true ) );
}
/*
 * bool acceptRichText () const
 */
HB_FUNC( QT_QTEXTEDIT_ACCEPTRICHTEXT )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->acceptRichText() );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QTEXTEDIT_ALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QTextEdit( 1 )->alignment() );
}

/*
 * QString anchorAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTEXTEDIT_ANCHORAT )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->anchorAt( *hbqt_par_QPoint( 2 ) ).toAscii().data() );
}

/*
 * AutoFormatting autoFormatting () const
 */
HB_FUNC( QT_QTEXTEDIT_AUTOFORMATTING )
{
   hb_retni( ( QTextEdit::AutoFormatting ) hbqt_par_QTextEdit( 1 )->autoFormatting() );
}

/*
 * bool canPaste () const
 */
HB_FUNC( QT_QTEXTEDIT_CANPASTE )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->canPaste() );
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QTextEdit( 1 )->createStandardContextMenu(), false ) );
}

/*
 * QMenu * createStandardContextMenu ( const QPoint & position )
 */
HB_FUNC( QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QTextEdit( 1 )->createStandardContextMenu( *hbqt_par_QPoint( 2 ) ), false ) );
}

/*
 * QTextCharFormat currentCharFormat () const
 */
HB_FUNC( QT_QTEXTEDIT_CURRENTCHARFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( hbqt_par_QTextEdit( 1 )->currentCharFormat() ), true ) );
}

/*
 * QFont currentFont () const
 */
HB_FUNC( QT_QTEXTEDIT_CURRENTFONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QTextEdit( 1 )->currentFont() ), true ) );
}

/*
 * QTextCursor cursorForPosition ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORFORPOSITION )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextEdit( 1 )->cursorForPosition( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/*
 * QRect cursorRect ( const QTextCursor & cursor ) const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QTextEdit( 1 )->cursorRect( *hbqt_par_QTextCursor( 2 ) ) ), true ) );
}

/*
 * QRect cursorRect () const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORRECT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QTextEdit( 1 )->cursorRect() ), true ) );
}

/*
 * int cursorWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORWIDTH )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->cursorWidth() );
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTEDIT_DOCUMENT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextDocument( hbqt_par_QTextEdit( 1 )->document(), false ) );
}

/*
 * QString documentTitle () const
 */
HB_FUNC( QT_QTEXTEDIT_DOCUMENTTITLE )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->documentTitle().toAscii().data() );
}

/*
 * void ensureCursorVisible ()
 */
HB_FUNC( QT_QTEXTEDIT_ENSURECURSORVISIBLE )
{
   hbqt_par_QTextEdit( 1 )->ensureCursorVisible();
}

/*
 * bool find ( const QString & exp, QTextDocument::FindFlags options = 0 )
 */
HB_FUNC( QT_QTEXTEDIT_FIND )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->find( QTextEdit::tr( hb_parc( 2 ) ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) );
}

/*
 * QString fontFamily () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTFAMILY )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->fontFamily().toAscii().data() );
}

/*
 * bool fontItalic () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTITALIC )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->fontItalic() );
}

/*
 * qreal fontPointSize () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTPOINTSIZE )
{
   hb_retnd( hbqt_par_QTextEdit( 1 )->fontPointSize() );
}

/*
 * bool fontUnderline () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTUNDERLINE )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->fontUnderline() );
}

/*
 * int fontWeight () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTWEIGHT )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->fontWeight() );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QTEXTEDIT_ISREADONLY )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->isReadOnly() );
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QTEXTEDIT_ISUNDOREDOENABLED )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->isUndoRedoEnabled() );
}

/*
 * int lineWrapColumnOrWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_LINEWRAPCOLUMNORWIDTH )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->lineWrapColumnOrWidth() );
}

/*
 * LineWrapMode lineWrapMode () const
 */
HB_FUNC( QT_QTEXTEDIT_LINEWRAPMODE )
{
   hb_retni( ( QTextEdit::LineWrapMode ) hbqt_par_QTextEdit( 1 )->lineWrapMode() );
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QTEXTEDIT_LOADRESOURCE )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QTextEdit( 1 )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
}

/*
 * void mergeCurrentCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QTEXTEDIT_MERGECURRENTCHARFORMAT )
{
   hbqt_par_QTextEdit( 1 )->mergeCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/*
 * void moveCursor ( QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode = QTextCursor::MoveAnchor )
 */
HB_FUNC( QT_QTEXTEDIT_MOVECURSOR )
{
   hbqt_par_QTextEdit( 1 )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) );
}

/*
 * bool overwriteMode () const
 */
HB_FUNC( QT_QTEXTEDIT_OVERWRITEMODE )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->overwriteMode() );
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QTEXTEDIT_PRINT )
{
   hbqt_par_QTextEdit( 1 )->print( hbqt_par_QPrinter( 2 ) );
}

/*
 * void setAcceptRichText ( bool accept )
 */
HB_FUNC( QT_QTEXTEDIT_SETACCEPTRICHTEXT )
{
   hbqt_par_QTextEdit( 1 )->setAcceptRichText( hb_parl( 2 ) );
}

/*
 * void setAutoFormatting ( AutoFormatting features )
 */
HB_FUNC( QT_QTEXTEDIT_SETAUTOFORMATTING )
{
   hbqt_par_QTextEdit( 1 )->setAutoFormatting( ( QTextEdit::AutoFormatting ) hb_parni( 2 ) );
}

/*
 * void setCurrentCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTEDIT_SETCURRENTCHARFORMAT )
{
   hbqt_par_QTextEdit( 1 )->setCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
}

/*
 * void setCursorWidth ( int width )
 */
HB_FUNC( QT_QTEXTEDIT_SETCURSORWIDTH )
{
   hbqt_par_QTextEdit( 1 )->setCursorWidth( hb_parni( 2 ) );
}

/*
 * void setDocument ( QTextDocument * document )
 */
HB_FUNC( QT_QTEXTEDIT_SETDOCUMENT )
{
   hbqt_par_QTextEdit( 1 )->setDocument( hbqt_par_QTextDocument( 2 ) );
}

/*
 * void setDocumentTitle ( const QString & title )
 */
HB_FUNC( QT_QTEXTEDIT_SETDOCUMENTTITLE )
{
   hbqt_par_QTextEdit( 1 )->setDocumentTitle( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void setLineWrapColumnOrWidth ( int w )
 */
HB_FUNC( QT_QTEXTEDIT_SETLINEWRAPCOLUMNORWIDTH )
{
   hbqt_par_QTextEdit( 1 )->setLineWrapColumnOrWidth( hb_parni( 2 ) );
}

/*
 * void setLineWrapMode ( LineWrapMode mode )
 */
HB_FUNC( QT_QTEXTEDIT_SETLINEWRAPMODE )
{
   hbqt_par_QTextEdit( 1 )->setLineWrapMode( ( QTextEdit::LineWrapMode ) hb_parni( 2 ) );
}

/*
 * void setOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QTEXTEDIT_SETOVERWRITEMODE )
{
   hbqt_par_QTextEdit( 1 )->setOverwriteMode( hb_parl( 2 ) );
}

/*
 * void setReadOnly ( bool ro )
 */
HB_FUNC( QT_QTEXTEDIT_SETREADONLY )
{
   hbqt_par_QTextEdit( 1 )->setReadOnly( hb_parl( 2 ) );
}

/*
 * void setTabChangesFocus ( bool b )
 */
HB_FUNC( QT_QTEXTEDIT_SETTABCHANGESFOCUS )
{
   hbqt_par_QTextEdit( 1 )->setTabChangesFocus( hb_parl( 2 ) );
}

/*
 * void setTabStopWidth ( int width )
 */
HB_FUNC( QT_QTEXTEDIT_SETTABSTOPWIDTH )
{
   hbqt_par_QTextEdit( 1 )->setTabStopWidth( hb_parni( 2 ) );
}

/*
 * void setTextCursor ( const QTextCursor & cursor )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTCURSOR )
{
   hbqt_par_QTextEdit( 1 )->setTextCursor( *hbqt_par_QTextCursor( 2 ) );
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTINTERACTIONFLAGS )
{
   hbqt_par_QTextEdit( 1 )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QTEXTEDIT_SETUNDOREDOENABLED )
{
   hbqt_par_QTextEdit( 1 )->setUndoRedoEnabled( hb_parl( 2 ) );
}

/*
 * void setWordWrapMode ( QTextOption::WrapMode policy )
 */
HB_FUNC( QT_QTEXTEDIT_SETWORDWRAPMODE )
{
   hbqt_par_QTextEdit( 1 )->setWordWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) );
}

/*
 * bool tabChangesFocus () const
 */
HB_FUNC( QT_QTEXTEDIT_TABCHANGESFOCUS )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->tabChangesFocus() );
}

/*
 * int tabStopWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_TABSTOPWIDTH )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->tabStopWidth() );
}

/*
 * QColor textBackgroundColor () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTBACKGROUNDCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QTextEdit( 1 )->textBackgroundColor() ), true ) );
}

/*
 * QColor textColor () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QTextEdit( 1 )->textColor() ), true ) );
}

/*
 * QTextCursor textCursor () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTCURSOR )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextEdit( 1 )->textCursor() ), true ) );
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTINTERACTIONFLAGS )
{
   hb_retni( ( Qt::TextInteractionFlags ) hbqt_par_QTextEdit( 1 )->textInteractionFlags() );
}

/*
 * QString toHtml () const
 */
HB_FUNC( QT_QTEXTEDIT_TOHTML )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->toHtml().toAscii().data() );
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QTEXTEDIT_TOPLAINTEXT )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->toPlainText().toAscii().data() );
}

/*
 * QTextOption::WrapMode wordWrapMode () const
 */
HB_FUNC( QT_QTEXTEDIT_WORDWRAPMODE )
{
   hb_retni( ( QTextOption::WrapMode ) hbqt_par_QTextEdit( 1 )->wordWrapMode() );
}

/*
 * void append ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_APPEND )
{
   hbqt_par_QTextEdit( 1 )->append( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTEXTEDIT_CLEAR )
{
   hbqt_par_QTextEdit( 1 )->clear();
}

/*
 * void copy ()
 */
HB_FUNC( QT_QTEXTEDIT_COPY )
{
   hbqt_par_QTextEdit( 1 )->copy();
}

/*
 * void cut ()
 */
HB_FUNC( QT_QTEXTEDIT_CUT )
{
   hbqt_par_QTextEdit( 1 )->cut();
}

/*
 * void insertHtml ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_INSERTHTML )
{
   hbqt_par_QTextEdit( 1 )->insertHtml( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void insertPlainText ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_INSERTPLAINTEXT )
{
   hbqt_par_QTextEdit( 1 )->insertPlainText( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void paste ()
 */
HB_FUNC( QT_QTEXTEDIT_PASTE )
{
   hbqt_par_QTextEdit( 1 )->paste();
}

/*
 * void redo ()
 */
HB_FUNC( QT_QTEXTEDIT_REDO )
{
   hbqt_par_QTextEdit( 1 )->redo();
}

/*
 * void scrollToAnchor ( const QString & name )
 */
HB_FUNC( QT_QTEXTEDIT_SCROLLTOANCHOR )
{
   hbqt_par_QTextEdit( 1 )->scrollToAnchor( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QTEXTEDIT_SELECTALL )
{
   hbqt_par_QTextEdit( 1 )->selectAll();
}

/*
 * void setAlignment ( Qt::Alignment a )
 */
HB_FUNC( QT_QTEXTEDIT_SETALIGNMENT )
{
   hbqt_par_QTextEdit( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setCurrentFont ( const QFont & f )
 */
HB_FUNC( QT_QTEXTEDIT_SETCURRENTFONT )
{
   hbqt_par_QTextEdit( 1 )->setCurrentFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setFontFamily ( const QString & fontFamily )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTFAMILY )
{
   hbqt_par_QTextEdit( 1 )->setFontFamily( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void setFontItalic ( bool italic )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTITALIC )
{
   hbqt_par_QTextEdit( 1 )->setFontItalic( hb_parl( 2 ) );
}

/*
 * void setFontPointSize ( qreal s )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTPOINTSIZE )
{
   hbqt_par_QTextEdit( 1 )->setFontPointSize( hb_parnd( 2 ) );
}

/*
 * void setFontUnderline ( bool underline )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTUNDERLINE )
{
   hbqt_par_QTextEdit( 1 )->setFontUnderline( hb_parl( 2 ) );
}

/*
 * void setFontWeight ( int weight )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTWEIGHT )
{
   hbqt_par_QTextEdit( 1 )->setFontWeight( hb_parni( 2 ) );
}

/*
 * void setHtml ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_SETHTML )
{
   hbqt_par_QTextEdit( 1 )->setHtml( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_SETPLAINTEXT )
{
   hbqt_par_QTextEdit( 1 )->setPlainText( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXT )
{
   hbqt_par_QTextEdit( 1 )->setText( QTextEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void setTextBackgroundColor ( const QColor & c )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTBACKGROUNDCOLOR )
{
   hbqt_par_QTextEdit( 1 )->setTextBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void setTextColor ( const QColor & c )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTCOLOR )
{
   hbqt_par_QTextEdit( 1 )->setTextColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void undo ()
 */
HB_FUNC( QT_QTEXTEDIT_UNDO )
{
   hbqt_par_QTextEdit( 1 )->undo();
}

/*
 * void zoomIn ( int range = 1 )
 */
HB_FUNC( QT_QTEXTEDIT_ZOOMIN )
{
   hbqt_par_QTextEdit( 1 )->zoomIn( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 1 ) );
}

/*
 * void zoomOut ( int range = 1 )
 */
HB_FUNC( QT_QTEXTEDIT_ZOOMOUT )
{
   hbqt_par_QTextEdit( 1 )->zoomOut( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 1 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
