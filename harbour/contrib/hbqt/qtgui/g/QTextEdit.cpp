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
 *  void setExtraSelections ( const QList<ExtraSelection> & selections )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // QList<ExtraSelection> extraSelections () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextEdit>


/* QTextEdit ( QWidget * parent = 0 )
 * QTextEdit ( const QString & text, QWidget * parent = 0 )
 * virtual ~QTextEdit ()
 */

typedef struct
{
   QPointer< QTextEdit > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextEdit;

QT_G_FUNC( hbqt_gcRelease_QTextEdit )
{
   QTextEdit  * ph = NULL ;
   QGC_POINTER_QTextEdit * p = ( QGC_POINTER_QTextEdit * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextEdit   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextEdit   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTextEdit          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextEdit    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextEdit    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextEdit( void * pObj, bool bNew )
{
   QGC_POINTER_QTextEdit * p = ( QGC_POINTER_QTextEdit * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextEdit >( ( QTextEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextEdit;
   p->type = HBQT_TYPE_QTextEdit;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextEdit  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextEdit", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTEDIT )
{
   QTextEdit * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISCHAR( 1 ) )
      pObj = new QTextEdit( hbqt_par_QString( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = new QTextEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextEdit( ( void * ) pObj, true ) );
}

/*
 * bool acceptRichText () const
 */
HB_FUNC( QT_QTEXTEDIT_ACCEPTRICHTEXT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->acceptRichText() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ACCEPTRICHTEXT FP=hb_retl( ( p )->acceptRichText() ); p is NULL" ) );
   }
}

/*
 * Qt::Alignment alignment () const
 */
HB_FUNC( QT_QTEXTEDIT_ALIGNMENT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ALIGNMENT FP=hb_retni( ( Qt::Alignment ) ( p )->alignment() ); p is NULL" ) );
   }
}

/*
 * QString anchorAt ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTEXTEDIT_ANCHORAT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retc( ( p )->anchorAt( *hbqt_par_QPoint( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ANCHORAT FP=hb_retc( ( p )->anchorAt( *hbqt_par_QPoint( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * AutoFormatting autoFormatting () const
 */
HB_FUNC( QT_QTEXTEDIT_AUTOFORMATTING )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( QTextEdit::AutoFormatting ) ( p )->autoFormatting() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_AUTOFORMATTING FP=hb_retni( ( QTextEdit::AutoFormatting ) ( p )->autoFormatting() ); p is NULL" ) );
   }
}

/*
 * bool canPaste () const
 */
HB_FUNC( QT_QTEXTEDIT_CANPASTE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->canPaste() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CANPASTE FP=hb_retl( ( p )->canPaste() ); p is NULL" ) );
   }
}

/*
 * QMenu * createStandardContextMenu ()
 */
HB_FUNC( QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) ); p is NULL" ) );
   }
}

/*
 * QMenu * createStandardContextMenu ( const QPoint & position )
 */
HB_FUNC( QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU_1 )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CREATESTANDARDCONTEXTMENU_1 FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat currentCharFormat () const
 */
HB_FUNC( QT_QTEXTEDIT_CURRENTCHARFORMAT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->currentCharFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CURRENTCHARFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->currentCharFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFont currentFont () const
 */
HB_FUNC( QT_QTEXTEDIT_CURRENTFONT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CURRENTFONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->currentFont() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor cursorForPosition ( const QPoint & pos ) const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORFORPOSITION )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->cursorForPosition( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CURSORFORPOSITION FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->cursorForPosition( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect cursorRect ( const QTextCursor & cursor ) const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORRECT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect( *hbqt_par_QTextCursor( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CURSORRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect( *hbqt_par_QTextCursor( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect cursorRect () const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORRECT_1 )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CURSORRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->cursorRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * int cursorWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_CURSORWIDTH )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( p )->cursorWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CURSORWIDTH FP=hb_retni( ( p )->cursorWidth() ); p is NULL" ) );
   }
}

/*
 * QTextDocument * document () const
 */
HB_FUNC( QT_QTEXTEDIT_DOCUMENT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_DOCUMENT FP=hb_retptrGC( hbqt_gcAllocate_QTextDocument( ( p )->document(), false ) ); p is NULL" ) );
   }
}

/*
 * QString documentTitle () const
 */
HB_FUNC( QT_QTEXTEDIT_DOCUMENTTITLE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retc( ( p )->documentTitle().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_DOCUMENTTITLE FP=hb_retc( ( p )->documentTitle().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void ensureCursorVisible ()
 */
HB_FUNC( QT_QTEXTEDIT_ENSURECURSORVISIBLE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->ensureCursorVisible();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ENSURECURSORVISIBLE FP=( p )->ensureCursorVisible(); p is NULL" ) );
   }
}

/*
 * bool find ( const QString & exp, QTextDocument::FindFlags options = 0 )
 */
HB_FUNC( QT_QTEXTEDIT_FIND )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->find( QTextEdit::tr( hb_parc( 2 ) ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_FIND FP=hb_retl( ( p )->find( QTextEdit::tr( hb_parc( 2 ) ), ( QTextDocument::FindFlags ) hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QString fontFamily () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTFAMILY )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retc( ( p )->fontFamily().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_FONTFAMILY FP=hb_retc( ( p )->fontFamily().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool fontItalic () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTITALIC )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->fontItalic() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_FONTITALIC FP=hb_retl( ( p )->fontItalic() ); p is NULL" ) );
   }
}

/*
 * qreal fontPointSize () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTPOINTSIZE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retnd( ( p )->fontPointSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_FONTPOINTSIZE FP=hb_retnd( ( p )->fontPointSize() ); p is NULL" ) );
   }
}

/*
 * bool fontUnderline () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTUNDERLINE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->fontUnderline() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_FONTUNDERLINE FP=hb_retl( ( p )->fontUnderline() ); p is NULL" ) );
   }
}

/*
 * int fontWeight () const
 */
HB_FUNC( QT_QTEXTEDIT_FONTWEIGHT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( p )->fontWeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_FONTWEIGHT FP=hb_retni( ( p )->fontWeight() ); p is NULL" ) );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QTEXTEDIT_ISREADONLY )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ISREADONLY FP=hb_retl( ( p )->isReadOnly() ); p is NULL" ) );
   }
}

/*
 * bool isUndoRedoEnabled () const
 */
HB_FUNC( QT_QTEXTEDIT_ISUNDOREDOENABLED )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->isUndoRedoEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ISUNDOREDOENABLED FP=hb_retl( ( p )->isUndoRedoEnabled() ); p is NULL" ) );
   }
}

/*
 * int lineWrapColumnOrWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_LINEWRAPCOLUMNORWIDTH )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( p )->lineWrapColumnOrWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_LINEWRAPCOLUMNORWIDTH FP=hb_retni( ( p )->lineWrapColumnOrWidth() ); p is NULL" ) );
   }
}

/*
 * LineWrapMode lineWrapMode () const
 */
HB_FUNC( QT_QTEXTEDIT_LINEWRAPMODE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( QTextEdit::LineWrapMode ) ( p )->lineWrapMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_LINEWRAPMODE FP=hb_retni( ( QTextEdit::LineWrapMode ) ( p )->lineWrapMode() ); p is NULL" ) );
   }
}

/*
 * virtual QVariant loadResource ( int type, const QUrl & name )
 */
HB_FUNC( QT_QTEXTEDIT_LOADRESOURCE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_LOADRESOURCE FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->loadResource( hb_parni( 2 ), *hbqt_par_QUrl( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void mergeCurrentCharFormat ( const QTextCharFormat & modifier )
 */
HB_FUNC( QT_QTEXTEDIT_MERGECURRENTCHARFORMAT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->mergeCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_MERGECURRENTCHARFORMAT FP=( p )->mergeCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void moveCursor ( QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode = QTextCursor::MoveAnchor )
 */
HB_FUNC( QT_QTEXTEDIT_MOVECURSOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_MOVECURSOR FP=( p )->moveCursor( ( QTextCursor::MoveOperation ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QTextCursor::MoveMode ) hb_parni( 3 ) : ( QTextCursor::MoveMode ) QTextCursor::MoveAnchor ) ); p is NULL" ) );
   }
}

/*
 * bool overwriteMode () const
 */
HB_FUNC( QT_QTEXTEDIT_OVERWRITEMODE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->overwriteMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_OVERWRITEMODE FP=hb_retl( ( p )->overwriteMode() ); p is NULL" ) );
   }
}

/*
 * void print ( QPrinter * printer ) const
 */
HB_FUNC( QT_QTEXTEDIT_PRINT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->print( hbqt_par_QPrinter( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_PRINT FP=( p )->print( hbqt_par_QPrinter( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAcceptRichText ( bool accept )
 */
HB_FUNC( QT_QTEXTEDIT_SETACCEPTRICHTEXT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setAcceptRichText( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETACCEPTRICHTEXT FP=( p )->setAcceptRichText( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoFormatting ( AutoFormatting features )
 */
HB_FUNC( QT_QTEXTEDIT_SETAUTOFORMATTING )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setAutoFormatting( ( QTextEdit::AutoFormatting ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETAUTOFORMATTING FP=( p )->setAutoFormatting( ( QTextEdit::AutoFormatting ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentCharFormat ( const QTextCharFormat & format )
 */
HB_FUNC( QT_QTEXTEDIT_SETCURRENTCHARFORMAT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETCURRENTCHARFORMAT FP=( p )->setCurrentCharFormat( *hbqt_par_QTextCharFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCursorWidth ( int width )
 */
HB_FUNC( QT_QTEXTEDIT_SETCURSORWIDTH )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setCursorWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETCURSORWIDTH FP=( p )->setCursorWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocument ( QTextDocument * document )
 */
HB_FUNC( QT_QTEXTEDIT_SETDOCUMENT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setDocument( hbqt_par_QTextDocument( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETDOCUMENT FP=( p )->setDocument( hbqt_par_QTextDocument( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocumentTitle ( const QString & title )
 */
HB_FUNC( QT_QTEXTEDIT_SETDOCUMENTTITLE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setDocumentTitle( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETDOCUMENTTITLE FP=( p )->setDocumentTitle( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setLineWrapColumnOrWidth ( int w )
 */
HB_FUNC( QT_QTEXTEDIT_SETLINEWRAPCOLUMNORWIDTH )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setLineWrapColumnOrWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETLINEWRAPCOLUMNORWIDTH FP=( p )->setLineWrapColumnOrWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLineWrapMode ( LineWrapMode mode )
 */
HB_FUNC( QT_QTEXTEDIT_SETLINEWRAPMODE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setLineWrapMode( ( QTextEdit::LineWrapMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETLINEWRAPMODE FP=( p )->setLineWrapMode( ( QTextEdit::LineWrapMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOverwriteMode ( bool overwrite )
 */
HB_FUNC( QT_QTEXTEDIT_SETOVERWRITEMODE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setOverwriteMode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETOVERWRITEMODE FP=( p )->setOverwriteMode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setReadOnly ( bool ro )
 */
HB_FUNC( QT_QTEXTEDIT_SETREADONLY )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETREADONLY FP=( p )->setReadOnly( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabChangesFocus ( bool b )
 */
HB_FUNC( QT_QTEXTEDIT_SETTABCHANGESFOCUS )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setTabChangesFocus( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETTABCHANGESFOCUS FP=( p )->setTabChangesFocus( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabStopWidth ( int width )
 */
HB_FUNC( QT_QTEXTEDIT_SETTABSTOPWIDTH )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setTabStopWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETTABSTOPWIDTH FP=( p )->setTabStopWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextCursor ( const QTextCursor & cursor )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTCURSOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setTextCursor( *hbqt_par_QTextCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETTEXTCURSOR FP=( p )->setTextCursor( *hbqt_par_QTextCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextInteractionFlags ( Qt::TextInteractionFlags flags )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTINTERACTIONFLAGS )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETTEXTINTERACTIONFLAGS FP=( p )->setTextInteractionFlags( ( Qt::TextInteractionFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUndoRedoEnabled ( bool enable )
 */
HB_FUNC( QT_QTEXTEDIT_SETUNDOREDOENABLED )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setUndoRedoEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETUNDOREDOENABLED FP=( p )->setUndoRedoEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWordWrapMode ( QTextOption::WrapMode policy )
 */
HB_FUNC( QT_QTEXTEDIT_SETWORDWRAPMODE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setWordWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETWORDWRAPMODE FP=( p )->setWordWrapMode( ( QTextOption::WrapMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool tabChangesFocus () const
 */
HB_FUNC( QT_QTEXTEDIT_TABCHANGESFOCUS )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retl( ( p )->tabChangesFocus() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TABCHANGESFOCUS FP=hb_retl( ( p )->tabChangesFocus() ); p is NULL" ) );
   }
}

/*
 * int tabStopWidth () const
 */
HB_FUNC( QT_QTEXTEDIT_TABSTOPWIDTH )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( p )->tabStopWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TABSTOPWIDTH FP=hb_retni( ( p )->tabStopWidth() ); p is NULL" ) );
   }
}

/*
 * QColor textBackgroundColor () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTBACKGROUNDCOLOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textBackgroundColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TEXTBACKGROUNDCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textBackgroundColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * QColor textColor () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTCOLOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TEXTCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor textCursor () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTCURSOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->textCursor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TEXTCURSOR FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->textCursor() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::TextInteractionFlags textInteractionFlags () const
 */
HB_FUNC( QT_QTEXTEDIT_TEXTINTERACTIONFLAGS )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( Qt::TextInteractionFlags ) ( p )->textInteractionFlags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TEXTINTERACTIONFLAGS FP=hb_retni( ( Qt::TextInteractionFlags ) ( p )->textInteractionFlags() ); p is NULL" ) );
   }
}

/*
 * QString toHtml () const
 */
HB_FUNC( QT_QTEXTEDIT_TOHTML )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retc( ( p )->toHtml().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TOHTML FP=hb_retc( ( p )->toHtml().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QTEXTEDIT_TOPLAINTEXT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retc( ( p )->toPlainText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_TOPLAINTEXT FP=hb_retc( ( p )->toPlainText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextOption::WrapMode wordWrapMode () const
 */
HB_FUNC( QT_QTEXTEDIT_WORDWRAPMODE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      hb_retni( ( QTextOption::WrapMode ) ( p )->wordWrapMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_WORDWRAPMODE FP=hb_retni( ( QTextOption::WrapMode ) ( p )->wordWrapMode() ); p is NULL" ) );
   }
}

/*
 * void append ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_APPEND )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->append( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_APPEND FP=( p )->append( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QTEXTEDIT_CLEAR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * void copy ()
 */
HB_FUNC( QT_QTEXTEDIT_COPY )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->copy();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_COPY FP=( p )->copy(); p is NULL" ) );
   }
}

/*
 * void cut ()
 */
HB_FUNC( QT_QTEXTEDIT_CUT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->cut();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_CUT FP=( p )->cut(); p is NULL" ) );
   }
}

/*
 * void insertHtml ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_INSERTHTML )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->insertHtml( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_INSERTHTML FP=( p )->insertHtml( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void insertPlainText ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_INSERTPLAINTEXT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->insertPlainText( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_INSERTPLAINTEXT FP=( p )->insertPlainText( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void paste ()
 */
HB_FUNC( QT_QTEXTEDIT_PASTE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->paste();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_PASTE FP=( p )->paste(); p is NULL" ) );
   }
}

/*
 * void redo ()
 */
HB_FUNC( QT_QTEXTEDIT_REDO )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->redo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_REDO FP=( p )->redo(); p is NULL" ) );
   }
}

/*
 * void scrollToAnchor ( const QString & name )
 */
HB_FUNC( QT_QTEXTEDIT_SCROLLTOANCHOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->scrollToAnchor( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SCROLLTOANCHOR FP=( p )->scrollToAnchor( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void selectAll ()
 */
HB_FUNC( QT_QTEXTEDIT_SELECTALL )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->selectAll();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SELECTALL FP=( p )->selectAll(); p is NULL" ) );
   }
}

/*
 * void setAlignment ( Qt::Alignment a )
 */
HB_FUNC( QT_QTEXTEDIT_SETALIGNMENT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETALIGNMENT FP=( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentFont ( const QFont & f )
 */
HB_FUNC( QT_QTEXTEDIT_SETCURRENTFONT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setCurrentFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETCURRENTFONT FP=( p )->setCurrentFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFontFamily ( const QString & fontFamily )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTFAMILY )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setFontFamily( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETFONTFAMILY FP=( p )->setFontFamily( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setFontItalic ( bool italic )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTITALIC )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setFontItalic( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETFONTITALIC FP=( p )->setFontItalic( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFontPointSize ( qreal s )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTPOINTSIZE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setFontPointSize( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETFONTPOINTSIZE FP=( p )->setFontPointSize( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFontUnderline ( bool underline )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTUNDERLINE )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setFontUnderline( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETFONTUNDERLINE FP=( p )->setFontUnderline( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFontWeight ( int weight )
 */
HB_FUNC( QT_QTEXTEDIT_SETFONTWEIGHT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setFontWeight( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETFONTWEIGHT FP=( p )->setFontWeight( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHtml ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_SETHTML )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setHtml( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETHTML FP=( p )->setHtml( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setPlainText ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_SETPLAINTEXT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setPlainText( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETPLAINTEXT FP=( p )->setPlainText( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setText ( const QString & text )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setText( QTextEdit::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETTEXT FP=( p )->setText( QTextEdit::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setTextBackgroundColor ( const QColor & c )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTBACKGROUNDCOLOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setTextBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETTEXTBACKGROUNDCOLOR FP=( p )->setTextBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTextColor ( const QColor & c )
 */
HB_FUNC( QT_QTEXTEDIT_SETTEXTCOLOR )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->setTextColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_SETTEXTCOLOR FP=( p )->setTextColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void undo ()
 */
HB_FUNC( QT_QTEXTEDIT_UNDO )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->undo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_UNDO FP=( p )->undo(); p is NULL" ) );
   }
}

/*
 * void zoomIn ( int range = 1 )
 */
HB_FUNC( QT_QTEXTEDIT_ZOOMIN )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->zoomIn( hb_parnidef( 2, 1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ZOOMIN FP=( p )->zoomIn( hb_parnidef( 2, 1 ) ); p is NULL" ) );
   }
}

/*
 * void zoomOut ( int range = 1 )
 */
HB_FUNC( QT_QTEXTEDIT_ZOOMOUT )
{
   QTextEdit * p = hbqt_par_QTextEdit( 1 );
   if( p )
      ( p )->zoomOut( hb_parnidef( 2, 1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTEDIT_ZOOMOUT FP=( p )->zoomOut( hb_parnidef( 2, 1 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
