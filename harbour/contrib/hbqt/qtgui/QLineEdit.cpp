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
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

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
   QPointer< QLineEdit > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QLineEdit;

QT_G_FUNC( hbqt_gcRelease_QLineEdit )
{
   QLineEdit  * ph = NULL ;
   QGC_POINTER_QLineEdit * p = ( QGC_POINTER_QLineEdit * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QLineEdit   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QLineEdit   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QLineEdit          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QLineEdit    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QLineEdit    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLineEdit( void * pObj, bool bNew )
{
   QGC_POINTER_QLineEdit * p = ( QGC_POINTER_QLineEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_QLineEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QLineEdit >( ( QLineEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLineEdit;
   p->type = HBQT_TYPE_QLineEdit;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLineEdit  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLineEdit", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLINEEDIT )
{
   QLineEdit * pObj = NULL;

   pObj =  new QLineEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QLineEdit( ( void * ) pObj, true ) );
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QLINEEDIT_ALIGNMENT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_ALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->alignment() ); p is NULL" ) );
   }
}

/*
 * void backspace ()
 */
HB_FUNC( QT_QLINEEDIT_BACKSPACE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->backspace();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_BACKSPACE FP=( p )->backspace(); p is NULL" ) );
   }
}

/*
 * QCompleter * completer () const
 */
HB_FUNC( QT_QLINEEDIT_COMPLETER )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCompleter( ( p )->completer(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_COMPLETER FP=hb_retptrGC( hbqt_gcAllocate_QCompleter( ( p )->completer(), false ) ); p is NULL" ) );
   }
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QLINEEDIT_CREATESTANDARDCONTEXTMENU )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CREATESTANDARDCONTEXTMENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) ); p is NULL" ) );
   }
}

/*
 * void cursorBackward ( bool mark, int steps = 1 )
 */
HB_FUNC( QT_QLINEEDIT_CURSORBACKWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorBackward( hb_parl( 2 ), hb_parnidef( 3, 1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CURSORBACKWARD FP=( p )->cursorBackward( hb_parl( 2 ), hb_parnidef( 3, 1 ) ); p is NULL" ) );
   }
}

/*
 * void cursorForward ( bool mark, int steps = 1 )
 */
HB_FUNC( QT_QLINEEDIT_CURSORFORWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorForward( hb_parl( 2 ), hb_parnidef( 3, 1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CURSORFORWARD FP=( p )->cursorForward( hb_parl( 2 ), hb_parnidef( 3, 1 ) ); p is NULL" ) );
   }
}

/*
 * int cursorPosition () const
 */
HB_FUNC( QT_QLINEEDIT_CURSORPOSITION )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->cursorPosition() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CURSORPOSITION FP=hb_retni( ( p )->cursorPosition() ); p is NULL" ) );
   }
}

/*
 * int cursorPositionAt ( const QPoint & pos )
 */
HB_FUNC( QT_QLINEEDIT_CURSORPOSITIONAT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->cursorPositionAt( *hbqt_par_QPoint( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CURSORPOSITIONAT FP=hb_retni( ( p )->cursorPositionAt( *hbqt_par_QPoint( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void cursorWordBackward ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_CURSORWORDBACKWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorWordBackward( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CURSORWORDBACKWARD FP=( p )->cursorWordBackward( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void cursorWordForward ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_CURSORWORDFORWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorWordForward( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CURSORWORDFORWARD FP=( p )->cursorWordForward( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void del ()
 */
HB_FUNC( QT_QLINEEDIT_DEL )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->del();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_DEL FP=( p )->del(); p is NULL" ) );
   }
}

/*
 * void deselect ()
 */
HB_FUNC( QT_QLINEEDIT_DESELECT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->deselect();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_DESELECT FP=( p )->deselect(); p is NULL" ) );
   }
}

/*
 * QString displayText () const
 */
HB_FUNC( QT_QLINEEDIT_DISPLAYTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retc( ( p )->displayText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_DISPLAYTEXT FP=hb_retc( ( p )->displayText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool dragEnabled () const
 */
HB_FUNC( QT_QLINEEDIT_DRAGENABLED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->dragEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_DRAGENABLED FP=hb_retl( ( p )->dragEnabled() ); p is NULL" ) );
   }
}

/*
 * EchoMode echoMode () const
 */
HB_FUNC( QT_QLINEEDIT_ECHOMODE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( QLineEdit::EchoMode ) ( p )->echoMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_ECHOMODE FP=hb_retni( ( QLineEdit::EchoMode ) ( p )->echoMode() ); p is NULL" ) );
   }
}

/*
 * void end ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_END )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->end( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_END FP=( p )->end( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void getTextMargins ( int * left, int * top, int * right, int * bottom ) const
 */
HB_FUNC( QT_QLINEEDIT_GETTEXTMARGINS )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   if( p )
      ( p )->getTextMargins( &iLeft, &iTop, &iRight, &iBottom );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_GETTEXTMARGINS FP=( p )->getTextMargins( &iLeft, &iTop, &iRight, &iBottom ); p is NULL" ) );
   }

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
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->hasAcceptableInput() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_HASACCEPTABLEINPUT FP=hb_retl( ( p )->hasAcceptableInput() ); p is NULL" ) );
   }
}

/*
 * bool hasFrame () const
 */
HB_FUNC( QT_QLINEEDIT_HASFRAME )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->hasFrame() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_HASFRAME FP=hb_retl( ( p )->hasFrame() ); p is NULL" ) );
   }
}

/*
 * bool hasSelectedText () const
 */
HB_FUNC( QT_QLINEEDIT_HASSELECTEDTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->hasSelectedText() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_HASSELECTEDTEXT FP=hb_retl( ( p )->hasSelectedText() ); p is NULL" ) );
   }
}

/*
 * void home ( bool mark )
 */
HB_FUNC( QT_QLINEEDIT_HOME )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->home( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_HOME FP=( p )->home( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString inputMask () const
 */
HB_FUNC( QT_QLINEEDIT_INPUTMASK )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retc( ( p )->inputMask().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_INPUTMASK FP=hb_retc( ( p )->inputMask().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void insert ( const QString & newText )
 */
HB_FUNC( QT_QLINEEDIT_INSERT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->insert( QLineEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_INSERT FP=( p )->insert( QLineEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QLINEEDIT_ISMODIFIED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_ISMODIFIED FP=hb_retl( ( p )->isModified() ); p is NULL" ) );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QLINEEDIT_ISREADONLY )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_ISREADONLY FP=hb_retl( ( p )->isReadOnly() ); p is NULL" ) );
   }
}

/*
 * bool isRedoAvailable () const
 */
HB_FUNC( QT_QLINEEDIT_ISREDOAVAILABLE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isRedoAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_ISREDOAVAILABLE FP=hb_retl( ( p )->isRedoAvailable() ); p is NULL" ) );
   }
}

/*
 * bool isUndoAvailable () const
 */
HB_FUNC( QT_QLINEEDIT_ISUNDOAVAILABLE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isUndoAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_ISUNDOAVAILABLE FP=hb_retl( ( p )->isUndoAvailable() ); p is NULL" ) );
   }
}

/*
 * int maxLength () const
 */
HB_FUNC( QT_QLINEEDIT_MAXLENGTH )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->maxLength() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_MAXLENGTH FP=hb_retni( ( p )->maxLength() ); p is NULL" ) );
   }
}

/*
 * virtual QSize minimumSizeHint () const
 */
HB_FUNC( QT_QLINEEDIT_MINIMUMSIZEHINT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_MINIMUMSIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QLINEEDIT_SELECTEDTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retc( ( p )->selectedText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SELECTEDTEXT FP=hb_retc( ( p )->selectedText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int selectionStart () const
 */
HB_FUNC( QT_QLINEEDIT_SELECTIONSTART )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->selectionStart() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SELECTIONSTART FP=hb_retni( ( p )->selectionStart() ); p is NULL" ) );
   }
}

/*
 * void setAlignment ( Qt::Alignment flag )
 */
HB_FUNC( QT_QLINEEDIT_SETALIGNMENT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETALIGNMENT FP=( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCompleter ( QCompleter * c )
 */
HB_FUNC( QT_QLINEEDIT_SETCOMPLETER )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setCompleter( hbqt_par_QCompleter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETCOMPLETER FP=( p )->setCompleter( hbqt_par_QCompleter( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCursorPosition ( int )
 */
HB_FUNC( QT_QLINEEDIT_SETCURSORPOSITION )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setCursorPosition( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETCURSORPOSITION FP=( p )->setCursorPosition( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDragEnabled ( bool b )
 */
HB_FUNC( QT_QLINEEDIT_SETDRAGENABLED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setDragEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETDRAGENABLED FP=( p )->setDragEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEchoMode ( EchoMode )
 */
HB_FUNC( QT_QLINEEDIT_SETECHOMODE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETECHOMODE FP=( p )->setEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFrame ( bool )
 */
HB_FUNC( QT_QLINEEDIT_SETFRAME )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setFrame( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETFRAME FP=( p )->setFrame( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setInputMask ( const QString & inputMask )
 */
HB_FUNC( QT_QLINEEDIT_SETINPUTMASK )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setInputMask( QLineEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETINPUTMASK FP=( p )->setInputMask( QLineEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setMaxLength ( int )
 */
HB_FUNC( QT_QLINEEDIT_SETMAXLENGTH )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setMaxLength( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETMAXLENGTH FP=( p )->setMaxLength( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setModified ( bool )
 */
HB_FUNC( QT_QLINEEDIT_SETMODIFIED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setModified( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETMODIFIED FP=( p )->setModified( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setReadOnly ( bool )
 */
HB_FUNC( QT_QLINEEDIT_SETREADONLY )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETREADONLY FP=( p )->setReadOnly( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSelection ( int start, int length )
 */
HB_FUNC( QT_QLINEEDIT_SETSELECTION )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setSelection( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETSELECTION FP=( p )->setSelection( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTextMargins ( int left, int top, int right, int bottom )
 */
HB_FUNC( QT_QLINEEDIT_SETTEXTMARGINS )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setTextMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETTEXTMARGINS FP=( p )->setTextMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setValidator ( const QValidator * v )
 */
HB_FUNC( QT_QLINEEDIT_SETVALIDATOR )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setValidator( hbqt_par_QValidator( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETVALIDATOR FP=( p )->setValidator( hbqt_par_QValidator( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QLINEEDIT_SIZEHINT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QLINEEDIT_TEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * virtual const QValidator * validator () const
 */
HB_FUNC( QT_QLINEEDIT_VALIDATOR )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QValidator( ( void * ) ( p )->validator(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_VALIDATOR FP=hb_retptrGC( hbqt_gcAllocate_QValidator( ( void * ) ( p )->validator(), false ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QLINEEDIT_CLEAR )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * void copy () const
 */
HB_FUNC( QT_QLINEEDIT_COPY )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->copy();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_COPY FP=( p )->copy(); p is NULL" ) );
   }
}

/*
 * void cut ()
 */
HB_FUNC( QT_QLINEEDIT_CUT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cut();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_CUT FP=( p )->cut(); p is NULL" ) );
   }
}

/*
 * void paste ()
 */
HB_FUNC( QT_QLINEEDIT_PASTE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->paste();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_PASTE FP=( p )->paste(); p is NULL" ) );
   }
}

/*
 * void redo ()
 */
HB_FUNC( QT_QLINEEDIT_REDO )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->redo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_REDO FP=( p )->redo(); p is NULL" ) );
   }
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QLINEEDIT_SELECTALL )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->selectAll();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SELECTALL FP=( p )->selectAll(); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & )
 */
HB_FUNC( QT_QLINEEDIT_SETTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setText( QLineEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_SETTEXT FP=( p )->setText( QLineEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void undo ()
 */
HB_FUNC( QT_QLINEEDIT_UNDO )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->undo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QLINEEDIT_UNDO FP=( p )->undo(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
