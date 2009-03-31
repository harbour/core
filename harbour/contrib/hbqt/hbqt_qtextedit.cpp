/*
 * $Id$
 */
   
/* 
 * Harbour Project source code:
 * QT wrapper main header
 * 
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


/*
 *  Constructed[ 44/60 [ 73.33% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  QTextCharFormat currentCharFormat () const
 *  QFont currentFont () const
 *  QTextCursor cursorForPosition ( const QPoint & pos ) const
 *  QRect cursorRect ( const QTextCursor & cursor ) const
 *  QList<ExtraSelection> extraSelections () const
 *  virtual QVariant loadResource ( int type, const QUrl & name )
 *  void mergeCurrentCharFormat ( const QTextCharFormat & modifier )
 *  void setCurrentCharFormat ( const QTextCharFormat & format )
 *  void setExtraSelections ( const QList<ExtraSelection> & selections )
 *  void setTextCursor ( const QTextCursor & cursor )
 *  QColor textBackgroundColor () const
 *  QColor textColor () const
 *  QTextCursor textCursor () const
 */ 


#include <QtGui/QTextEdit>


/*

 */
HB_FUNC( QT_QTEXTEDIT )
{
   if( hb_pcount() >= 1 && HB_ISCHAR( 1 ) )
      hb_retptr( ( QTextEdit* ) new QTextEdit( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) );
   else
      hb_retptr( ( QTextEdit* ) new QTextEdit( hbqt_par_QWidget( 1 ) ) );
}

/*
 * bool acceptRichText () const
 */
HB_FUNC( QT_QTEXTEDIT_ACCEPTRICHTEXT )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->acceptRichText(  ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QTEXTEDIT_ALIGNMENT )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->alignment(  ) );
}

/*
 * QString anchorAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTEXTEDIT_ANCHORAT )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->anchorAt( hbqt_const_QPoint( 2 )).toLatin1().data() );
}

/*
 * AutoFormatting autoFormatting () const
 */
HB_FUNC( QT_QTEXTEDIT_AUTOFORMATTING )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->autoFormatting(  ) );
}

/*
 * bool canPaste () const
 */
HB_FUNC( QT_QTEXTEDIT_CANPASTE )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->canPaste(  ) );
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU )
{
   hb_retptr( ( QMenu* ) hbqt_par_QTextEdit( 1 )->createStandardContextMenu(  ) );
}

/*
 * QMenu * createStandardContextMenu ( const QPoint & position )
 */
HB_FUNC( QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU_1 )
{
   hb_retptr( ( QMenu* ) hbqt_par_QTextEdit( 1 )->createStandardContextMenu( hbqt_const_QPoint( 2 ) ) );
}

/*
 * QRect cursorRect () const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORRECT )
{
   hbqt_ret_QRect( hbqt_par_QTextEdit( 1 )->cursorRect(  ) );
}

/*
 * int cursorWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORWIDTH )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->cursorWidth(  ) );
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTEDIT_DOCUMENT )
{
   hb_retptr( ( QTextDocument* ) hbqt_par_QTextEdit( 1 )->document(  ) );
}

/*
 * QString documentTitle () const
 */
HB_FUNC( QT_QTEXTEDIT_DOCUMENTTITLE )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->documentTitle( ).toLatin1().data() );
}

/*
 * void ensureCursorVisible ()
 */
HB_FUNC( QT_QTEXTEDIT_ENSURECURSORVISIBLE )
{
   hbqt_par_QTextEdit( 1 )->ensureCursorVisible(  );
}

/*
 * bool find ( const QString & exp, QTextDocument::FindFlags options = 0 )
 */
HB_FUNC( QT_QTEXTEDIT_FIND )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->find( hbqt_par_QString( 2 ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) );
}

/*
 * QString fontFamily () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTFAMILY )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->fontFamily( ).toLatin1().data() );
}

/*
 * bool fontItalic () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTITALIC )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->fontItalic(  ) );
}

/*
 * qreal fontPointSize () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTPOINTSIZE )
{
   hb_retnd( hbqt_par_QTextEdit( 1 )->fontPointSize(  ) );
}

/*
 * bool fontUnderline () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTUNDERLINE )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->fontUnderline(  ) );
}

/*
 * int fontWeight () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTWEIGHT )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->fontWeight(  ) );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QTEXTEDIT_ISREADONLY )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->isReadOnly(  ) );
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QTEXTEDIT_ISUNDOREDOENABLED )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->isUndoRedoEnabled(  ) );
}

/*
 * int lineWrapColumnOrWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_LINEWRAPCOLUMNORWIDTH )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->lineWrapColumnOrWidth(  ) );
}

/*
 * LineWrapMode lineWrapMode () const
 */
HB_FUNC( QT_QTEXTEDIT_LINEWRAPMODE )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->lineWrapMode(  ) );
}

/*
 * void moveCursor ( QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode = QTextCursor::MoveAnchor )
 */
HB_FUNC( QT_QTEXTEDIT_MOVECURSOR )
{
   hbqt_par_QTextEdit( 1 )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( QTextCursor::MoveMode ) hb_parni( 3 ) );
}

/*
 * bool overwriteMode () const
 */
HB_FUNC( QT_QTEXTEDIT_OVERWRITEMODE )
{
   hb_retl( hbqt_par_QTextEdit( 1 )->overwriteMode(  ) );
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
   hbqt_par_QTextEdit( 1 )->setDocumentTitle( hbqt_par_QString( 2 ) );
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
   hb_retl( hbqt_par_QTextEdit( 1 )->tabChangesFocus(  ) );
}

/*
 * int tabStopWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_TABSTOPWIDTH )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->tabStopWidth(  ) );
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTINTERACTIONFLAGS )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->textInteractionFlags(  ) );
}

/*
 * QString toHtml () const
 */
HB_FUNC( QT_QTEXTEDIT_TOHTML )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->toHtml( ).toLatin1().data() );
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QTEXTEDIT_TOPLAINTEXT )
{
   hb_retc( hbqt_par_QTextEdit( 1 )->toPlainText( ).toLatin1().data() );
}

/*
 * QTextOption::WrapMode wordWrapMode () const
 */
HB_FUNC( QT_QTEXTEDIT_WORDWRAPMODE )
{
   hb_retni( hbqt_par_QTextEdit( 1 )->wordWrapMode(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

