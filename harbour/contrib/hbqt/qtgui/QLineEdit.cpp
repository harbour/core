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
 *  enum EchoMode { Normal, NoEcho, Password, PasswordEchoOnEdit }
 */

#include <QtCore/QPointer>

#include <QtGui/QLineEdit>
#include <QtGui/QValidator>


/*
 * QLineEdit ( QWidget * parent = 0 )
 * QLineEdit ( const QString & contents, QWidget * parent = 0 )
 * ~QLineEdit ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QLineEdit > pq;
} QGC_POINTER_QLineEdit;

QT_G_FUNC( hbqt_gcRelease_QLineEdit )
{
   QGC_POINTER_QLineEdit * p = ( QGC_POINTER_QLineEdit * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QLineEdit * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QLineEdit                  ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QLineEdit                  ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QLineEdit                   Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QLineEdit                   Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLineEdit( void * pObj, bool bNew )
{
   QGC_POINTER_QLineEdit * p = ( QGC_POINTER_QLineEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_QLineEdit ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLineEdit;

   if( bNew )
   {
      new( & p->pq ) QPointer< QLineEdit >( ( QLineEdit * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QLineEdit                  ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QLINEEDIT )
{
   void * pObj = NULL;

   pObj = ( QLineEdit* ) new QLineEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QLineEdit( pObj, true ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QLINEEDIT_ALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QLineEdit( 1 )->alignment() );
}

/*
 * void backspace ()
 */
HB_FUNC( QT_QLINEEDIT_BACKSPACE )
{
   hbqt_par_QLineEdit( 1 )->backspace();
}

/*
 * QCompleter * completer () const
 */
HB_FUNC( QT_QLINEEDIT_COMPLETER )
{
   hb_retptrGC( hbqt_gcAllocate_QCompleter( hbqt_par_QLineEdit( 1 )->completer(), false ) );
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QLINEEDIT_CREATESTANDARDCONTEXTMENU )
{
   hb_retptrGC( hbqt_gcAllocate_QMenu( hbqt_par_QLineEdit( 1 )->createStandardContextMenu(), false ) );
}

/*
 * void cursorBackward ( bool mark, int steps = 1 )
 */
HB_FUNC( QT_QLINEEDIT_CURSORBACKWARD )
{
   hbqt_par_QLineEdit( 1 )->cursorBackward( hb_parl( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 1 ) );
}

/*
 * void cursorForward ( bool mark, int steps = 1 )
 */
HB_FUNC( QT_QLINEEDIT_CURSORFORWARD )
{
   hbqt_par_QLineEdit( 1 )->cursorForward( hb_parl( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 1 ) );
}

/*
 * int cursorPosition () const
 */
HB_FUNC( QT_QLINEEDIT_CURSORPOSITION )
{
   hb_retni( hbqt_par_QLineEdit( 1 )->cursorPosition() );
}

/*
 * int cursorPositionAt ( const QPoint & pos )
 */
HB_FUNC( QT_QLINEEDIT_CURSORPOSITIONAT )
{
   hb_retni( hbqt_par_QLineEdit( 1 )->cursorPositionAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * void cursorWordBackward ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_CURSORWORDBACKWARD )
{
   hbqt_par_QLineEdit( 1 )->cursorWordBackward( hb_parl( 2 ) );
}

/*
 * void cursorWordForward ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_CURSORWORDFORWARD )
{
   hbqt_par_QLineEdit( 1 )->cursorWordForward( hb_parl( 2 ) );
}

/*
 * void del ()
 */
HB_FUNC( QT_QLINEEDIT_DEL )
{
   hbqt_par_QLineEdit( 1 )->del();
}

/*
 * void deselect ()
 */
HB_FUNC( QT_QLINEEDIT_DESELECT )
{
   hbqt_par_QLineEdit( 1 )->deselect();
}

/*
 * QString displayText () const
 */
HB_FUNC( QT_QLINEEDIT_DISPLAYTEXT )
{
   hb_retc( hbqt_par_QLineEdit( 1 )->displayText().toAscii().data() );
}

/*
 * bool dragEnabled () const
 */
HB_FUNC( QT_QLINEEDIT_DRAGENABLED )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->dragEnabled() );
}

/*
 * EchoMode echoMode () const
 */
HB_FUNC( QT_QLINEEDIT_ECHOMODE )
{
   hb_retni( ( QLineEdit::EchoMode ) hbqt_par_QLineEdit( 1 )->echoMode() );
}

/*
 * void end ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_END )
{
   hbqt_par_QLineEdit( 1 )->end( hb_parl( 2 ) );
}

/*
 * void getTextMargins ( int * left, int * top, int * right, int * bottom ) const
 */
HB_FUNC( QT_QLINEEDIT_GETTEXTMARGINS )
{
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   hbqt_par_QLineEdit( 1 )->getTextMargins( &iLeft, &iTop, &iRight, &iBottom );

   hb_storni( iLeft, 2 );
   hb_storni( iTop, 3 );
   hb_storni( iRight, 4 );
   hb_storni( iBottom, 5 );
}

/*
 * bool hasAcceptableInput () const
 */
HB_FUNC( QT_QLINEEDIT_HASACCEPTABLEINPUT )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->hasAcceptableInput() );
}

/*
 * bool hasFrame () const
 */
HB_FUNC( QT_QLINEEDIT_HASFRAME )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->hasFrame() );
}

/*
 * bool hasSelectedText () const
 */
HB_FUNC( QT_QLINEEDIT_HASSELECTEDTEXT )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->hasSelectedText() );
}

/*
 * void home ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_HOME )
{
   hbqt_par_QLineEdit( 1 )->home( hb_parl( 2 ) );
}

/*
 * QString inputMask () const
 */
HB_FUNC( QT_QLINEEDIT_INPUTMASK )
{
   hb_retc( hbqt_par_QLineEdit( 1 )->inputMask().toAscii().data() );
}

/*
 * void insert ( const QString & newText )
 */
HB_FUNC( QT_QLINEEDIT_INSERT )
{
   hbqt_par_QLineEdit( 1 )->insert( QLineEdit::tr( hb_parc( 2 ) ) );
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QLINEEDIT_ISMODIFIED )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->isModified() );
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QLINEEDIT_ISREADONLY )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->isReadOnly() );
}

/*
 * bool isRedoAvailable () const
 */
HB_FUNC( QT_QLINEEDIT_ISREDOAVAILABLE )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->isRedoAvailable() );
}

/*
 * bool isUndoAvailable () const
 */
HB_FUNC( QT_QLINEEDIT_ISUNDOAVAILABLE )
{
   hb_retl( hbqt_par_QLineEdit( 1 )->isUndoAvailable() );
}

/*
 * int maxLength () const
 */
HB_FUNC( QT_QLINEEDIT_MAXLENGTH )
{
   hb_retni( hbqt_par_QLineEdit( 1 )->maxLength() );
}

/*
 * virtual QSize minimumSizeHint () const
 */
HB_FUNC( QT_QLINEEDIT_MINIMUMSIZEHINT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QLineEdit( 1 )->minimumSizeHint() ), true ) );
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QLINEEDIT_SELECTEDTEXT )
{
   hb_retc( hbqt_par_QLineEdit( 1 )->selectedText().toAscii().data() );
}

/*
 * int selectionStart () const
 */
HB_FUNC( QT_QLINEEDIT_SELECTIONSTART )
{
   hb_retni( hbqt_par_QLineEdit( 1 )->selectionStart() );
}

/*
 * void setAlignment ( Qt::Alignment flag )
 */
HB_FUNC( QT_QLINEEDIT_SETALIGNMENT )
{
   hbqt_par_QLineEdit( 1 )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/*
 * void setCompleter ( QCompleter * c )
 */
HB_FUNC( QT_QLINEEDIT_SETCOMPLETER )
{
   hbqt_par_QLineEdit( 1 )->setCompleter( hbqt_par_QCompleter( 2 ) );
}

/*
 * void setCursorPosition ( int )
 */
HB_FUNC( QT_QLINEEDIT_SETCURSORPOSITION )
{
   hbqt_par_QLineEdit( 1 )->setCursorPosition( hb_parni( 2 ) );
}

/*
 * void setDragEnabled ( bool b )
 */
HB_FUNC( QT_QLINEEDIT_SETDRAGENABLED )
{
   hbqt_par_QLineEdit( 1 )->setDragEnabled( hb_parl( 2 ) );
}

/*
 * void setEchoMode ( EchoMode )
 */
HB_FUNC( QT_QLINEEDIT_SETECHOMODE )
{
   hbqt_par_QLineEdit( 1 )->setEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) );
}

/*
 * void setFrame ( bool )
 */
HB_FUNC( QT_QLINEEDIT_SETFRAME )
{
   hbqt_par_QLineEdit( 1 )->setFrame( hb_parl( 2 ) );
}

/*
 * void setInputMask ( const QString & inputMask )
 */
HB_FUNC( QT_QLINEEDIT_SETINPUTMASK )
{
   hbqt_par_QLineEdit( 1 )->setInputMask( QLineEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void setMaxLength ( int )
 */
HB_FUNC( QT_QLINEEDIT_SETMAXLENGTH )
{
   hbqt_par_QLineEdit( 1 )->setMaxLength( hb_parni( 2 ) );
}

/*
 * void setModified ( bool )
 */
HB_FUNC( QT_QLINEEDIT_SETMODIFIED )
{
   hbqt_par_QLineEdit( 1 )->setModified( hb_parl( 2 ) );
}

/*
 * void setReadOnly ( bool )
 */
HB_FUNC( QT_QLINEEDIT_SETREADONLY )
{
   hbqt_par_QLineEdit( 1 )->setReadOnly( hb_parl( 2 ) );
}

/*
 * void setSelection ( int start, int length )
 */
HB_FUNC( QT_QLINEEDIT_SETSELECTION )
{
   hbqt_par_QLineEdit( 1 )->setSelection( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setTextMargins ( int left, int top, int right, int bottom )
 */
HB_FUNC( QT_QLINEEDIT_SETTEXTMARGINS )
{
   hbqt_par_QLineEdit( 1 )->setTextMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setValidator ( const QValidator * v )
 */
HB_FUNC( QT_QLINEEDIT_SETVALIDATOR )
{
   hbqt_par_QLineEdit( 1 )->setValidator( hbqt_par_QValidator( 2 ) );
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QLINEEDIT_SIZEHINT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QLineEdit( 1 )->sizeHint() ), true ) );
}

/*
 * QString text () const
 */
HB_FUNC( QT_QLINEEDIT_TEXT )
{
   hb_retc( hbqt_par_QLineEdit( 1 )->text().toAscii().data() );
}

/*
 * virtual const QValidator * validator () const
 */
HB_FUNC( QT_QLINEEDIT_VALIDATOR )
{
   hb_retptr( ( QValidator* ) hbqt_par_QLineEdit( 1 )->validator() );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QLINEEDIT_CLEAR )
{
   hbqt_par_QLineEdit( 1 )->clear();
}

/*
 * void copy () const
 */
HB_FUNC( QT_QLINEEDIT_COPY )
{
   hbqt_par_QLineEdit( 1 )->copy();
}

/*
 * void cut ()
 */
HB_FUNC( QT_QLINEEDIT_CUT )
{
   hbqt_par_QLineEdit( 1 )->cut();
}

/*
 * void paste ()
 */
HB_FUNC( QT_QLINEEDIT_PASTE )
{
   hbqt_par_QLineEdit( 1 )->paste();
}

/*
 * void redo ()
 */
HB_FUNC( QT_QLINEEDIT_REDO )
{
   hbqt_par_QLineEdit( 1 )->redo();
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QLINEEDIT_SELECTALL )
{
   hbqt_par_QLineEdit( 1 )->selectAll();
}

/*
 * void setText ( const QString & )
 */
HB_FUNC( QT_QLINEEDIT_SETTEXT )
{
   hbqt_par_QLineEdit( 1 )->setText( QLineEdit::tr( hb_parc( 2 ) ) );
}

/*
 * void undo ()
 */
HB_FUNC( QT_QLINEEDIT_UNDO )
{
   hbqt_par_QLineEdit( 1 )->undo();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
