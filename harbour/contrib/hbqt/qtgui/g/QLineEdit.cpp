/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum EchoMode { Normal, NoEcho, Password, PasswordEchoOnEdit }
 */

/*
 *  Constructed[ 55/55 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLineEdit;

HBQT_GC_FUNC( hbqt_gcRelease_QLineEdit )
{
   HBQT_GC_T_QLineEdit * p = ( HBQT_GC_T_QLineEdit * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QLineEdit * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLineEdit( void * pObj, bool bNew )
{
   HBQT_GC_T_QLineEdit * p = ( HBQT_GC_T_QLineEdit * ) hb_gcAllocate( sizeof( HBQT_GC_T_QLineEdit ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QLineEdit >( ( QLineEdit * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLineEdit;
   p->type = HBQT_TYPE_QLineEdit;

   return p;
}

HB_FUNC( QT_QLINEEDIT )
{
   QLineEdit * pObj = NULL;

   pObj = new QLineEdit( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QLineEdit( ( void * ) pObj, true ) );
}

/* Qt::Alignment alignment () const */
HB_FUNC( QT_QLINEEDIT_ALIGNMENT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( Qt::Alignment ) ( p )->alignment() );
}

/* void backspace () */
HB_FUNC( QT_QLINEEDIT_BACKSPACE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->backspace();
}

/* QCompleter * completer () const */
HB_FUNC( QT_QLINEEDIT_COMPLETER )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCompleter( ( p )->completer(), false ) );
}

/* QMenu * createStandardContextMenu () */
HB_FUNC( QT_QLINEEDIT_CREATESTANDARDCONTEXTMENU )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createStandardContextMenu(), false ) );
}

/* void cursorBackward ( bool mark, int steps = 1 ) */
HB_FUNC( QT_QLINEEDIT_CURSORBACKWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorBackward( hb_parl( 2 ), hb_parnidef( 3, 1 ) );
}

/* void cursorForward ( bool mark, int steps = 1 ) */
HB_FUNC( QT_QLINEEDIT_CURSORFORWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorForward( hb_parl( 2 ), hb_parnidef( 3, 1 ) );
}

/* int cursorPosition () const */
HB_FUNC( QT_QLINEEDIT_CURSORPOSITION )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->cursorPosition() );
}

/* int cursorPositionAt ( const QPoint & pos ) */
HB_FUNC( QT_QLINEEDIT_CURSORPOSITIONAT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->cursorPositionAt( *hbqt_par_QPoint( 2 ) ) );
}

/* void cursorWordBackward ( bool mark ) */
HB_FUNC( QT_QLINEEDIT_CURSORWORDBACKWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorWordBackward( hb_parl( 2 ) );
}

/* void cursorWordForward ( bool mark ) */
HB_FUNC( QT_QLINEEDIT_CURSORWORDFORWARD )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cursorWordForward( hb_parl( 2 ) );
}

/* void del () */
HB_FUNC( QT_QLINEEDIT_DEL )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->del();
}

/* void deselect () */
HB_FUNC( QT_QLINEEDIT_DESELECT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->deselect();
}

/* QString displayText () const */
HB_FUNC( QT_QLINEEDIT_DISPLAYTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retstr_utf8( ( p )->displayText().toUtf8().data() );
}

/* bool dragEnabled () const */
HB_FUNC( QT_QLINEEDIT_DRAGENABLED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->dragEnabled() );
}

/* EchoMode echoMode () const */
HB_FUNC( QT_QLINEEDIT_ECHOMODE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( QLineEdit::EchoMode ) ( p )->echoMode() );
}

/* void end ( bool mark ) */
HB_FUNC( QT_QLINEEDIT_END )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->end( hb_parl( 2 ) );
}

/* void getTextMargins ( int * left, int * top, int * right, int * bottom ) const */
HB_FUNC( QT_QLINEEDIT_GETTEXTMARGINS )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   if( p )
      ( p )->getTextMargins( &iLeft, &iTop, &iRight, &iBottom );

   hb_storni( iLeft, 2 );
   hb_storni( iTop, 3 );
   hb_storni( iRight, 4 );
   hb_storni( iBottom, 5 );
}

/* bool hasAcceptableInput () const */
HB_FUNC( QT_QLINEEDIT_HASACCEPTABLEINPUT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->hasAcceptableInput() );
}

/* bool hasFrame () const */
HB_FUNC( QT_QLINEEDIT_HASFRAME )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->hasFrame() );
}

/* bool hasSelectedText () const */
HB_FUNC( QT_QLINEEDIT_HASSELECTEDTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->hasSelectedText() );
}

/* void home ( bool mark ) */
HB_FUNC( QT_QLINEEDIT_HOME )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->home( hb_parl( 2 ) );
}

/* QString inputMask () const */
HB_FUNC( QT_QLINEEDIT_INPUTMASK )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retstr_utf8( ( p )->inputMask().toUtf8().data() );
}

/* void insert ( const QString & newText ) */
HB_FUNC( QT_QLINEEDIT_INSERT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->insert( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* bool isModified () const */
HB_FUNC( QT_QLINEEDIT_ISMODIFIED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
}

/* bool isReadOnly () const */
HB_FUNC( QT_QLINEEDIT_ISREADONLY )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
}

/* bool isRedoAvailable () const */
HB_FUNC( QT_QLINEEDIT_ISREDOAVAILABLE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isRedoAvailable() );
}

/* bool isUndoAvailable () const */
HB_FUNC( QT_QLINEEDIT_ISUNDOAVAILABLE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retl( ( p )->isUndoAvailable() );
}

/* int maxLength () const */
HB_FUNC( QT_QLINEEDIT_MAXLENGTH )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->maxLength() );
}

/* virtual QSize minimumSizeHint () const */
HB_FUNC( QT_QLINEEDIT_MINIMUMSIZEHINT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSizeHint() ), true ) );
}

/* QString selectedText () const */
HB_FUNC( QT_QLINEEDIT_SELECTEDTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retstr_utf8( ( p )->selectedText().toUtf8().data() );
}

/* int selectionStart () const */
HB_FUNC( QT_QLINEEDIT_SELECTIONSTART )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retni( ( p )->selectionStart() );
}

/* void setAlignment ( Qt::Alignment flag ) */
HB_FUNC( QT_QLINEEDIT_SETALIGNMENT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setAlignment( ( Qt::Alignment ) hb_parni( 2 ) );
}

/* void setCompleter ( QCompleter * c ) */
HB_FUNC( QT_QLINEEDIT_SETCOMPLETER )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setCompleter( hbqt_par_QCompleter( 2 ) );
}

/* void setCursorPosition ( int ) */
HB_FUNC( QT_QLINEEDIT_SETCURSORPOSITION )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setCursorPosition( hb_parni( 2 ) );
}

/* void setDragEnabled ( bool b ) */
HB_FUNC( QT_QLINEEDIT_SETDRAGENABLED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setDragEnabled( hb_parl( 2 ) );
}

/* void setEchoMode ( EchoMode ) */
HB_FUNC( QT_QLINEEDIT_SETECHOMODE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setEchoMode( ( QLineEdit::EchoMode ) hb_parni( 2 ) );
}

/* void setFrame ( bool ) */
HB_FUNC( QT_QLINEEDIT_SETFRAME )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setFrame( hb_parl( 2 ) );
}

/* void setInputMask ( const QString & inputMask ) */
HB_FUNC( QT_QLINEEDIT_SETINPUTMASK )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->setInputMask( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setMaxLength ( int ) */
HB_FUNC( QT_QLINEEDIT_SETMAXLENGTH )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setMaxLength( hb_parni( 2 ) );
}

/* void setModified ( bool ) */
HB_FUNC( QT_QLINEEDIT_SETMODIFIED )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setModified( hb_parl( 2 ) );
}

/* void setReadOnly ( bool ) */
HB_FUNC( QT_QLINEEDIT_SETREADONLY )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
}

/* void setSelection ( int start, int length ) */
HB_FUNC( QT_QLINEEDIT_SETSELECTION )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setSelection( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setTextMargins ( int left, int top, int right, int bottom ) */
HB_FUNC( QT_QLINEEDIT_SETTEXTMARGINS )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setTextMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setValidator ( const QValidator * v ) */
HB_FUNC( QT_QLINEEDIT_SETVALIDATOR )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->setValidator( hbqt_par_QValidator( 2 ) );
}

/* virtual QSize sizeHint () const */
HB_FUNC( QT_QLINEEDIT_SIZEHINT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
}

/* QString text () const */
HB_FUNC( QT_QLINEEDIT_TEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* virtual const QValidator * validator () const */
HB_FUNC( QT_QLINEEDIT_VALIDATOR )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QValidator( ( void * ) ( p )->validator(), false ) );
}

/* void clear () */
HB_FUNC( QT_QLINEEDIT_CLEAR )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->clear();
}

/* void copy () const */
HB_FUNC( QT_QLINEEDIT_COPY )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->copy();
}

/* void cut () */
HB_FUNC( QT_QLINEEDIT_CUT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->cut();
}

/* void paste () */
HB_FUNC( QT_QLINEEDIT_PASTE )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->paste();
}

/* void redo () */
HB_FUNC( QT_QLINEEDIT_REDO )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->redo();
}

/* void selectAll () */
HB_FUNC( QT_QLINEEDIT_SELECTALL )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->selectAll();
}

/* void setText ( const QString & ) */
HB_FUNC( QT_QLINEEDIT_SETTEXT )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void undo () */
HB_FUNC( QT_QLINEEDIT_UNDO )
{
   QLineEdit * p = hbqt_par_QLineEdit( 1 );
   if( p )
      ( p )->undo();
}


#endif /* #if QT_VERSION >= 0x040500 */
