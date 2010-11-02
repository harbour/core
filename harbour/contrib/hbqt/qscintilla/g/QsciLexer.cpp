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
#include "hbqscintilla.h"

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 44/44 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscilexer.h>


/*
 * QsciLexer (QObject *parent=0)
 * virtual ~QsciLexer ()
 *
 */

typedef struct
{
   QPointer< QsciLexer > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciLexer;

HBQT_GC_FUNC( hbqt_gcRelease_QsciLexer )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QsciLexer( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciLexer * p = ( HBQT_GC_T_QsciLexer * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciLexer ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciLexer >( ( QsciLexer * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciLexer;
   p->type = HBQT_TYPE_QsciLexer;

   return p;
}

HB_FUNC( QT_QSCILEXER )
{

}

/* virtual const char * language () const =0 */
HB_FUNC( QT_QSCILEXER_LANGUAGE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retc( ( p )->language() );
}

/* virtual const char * lexer () const */
HB_FUNC( QT_QSCILEXER_LEXER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retc( ( p )->lexer() );
}

/* virtual int lexerId () const */
HB_FUNC( QT_QSCILEXER_LEXERID )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retni( ( p )->lexerId() );
}

/* QsciAbstractAPIs * apis () const */
HB_FUNC( QT_QSCILEXER_APIS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciAbstractAPIs( ( p )->apis(), false ) );
}

/* virtual const char * autoCompletionFillups () const */
HB_FUNC( QT_QSCILEXER_AUTOCOMPLETIONFILLUPS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retc( ( p )->autoCompletionFillups() );
}

/* virtual QStringList autoCompletionWordSeparators () const */
HB_FUNC( QT_QSCILEXER_AUTOCOMPLETIONWORDSEPARATORS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->autoCompletionWordSeparators() ), true ) );
}

/* int autoIndentStyle () */
HB_FUNC( QT_QSCILEXER_AUTOINDENTSTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retni( ( p )->autoIndentStyle() );
}

/* virtual const char * blockEnd (int *style=0) const */
HB_FUNC( QT_QSCILEXER_BLOCKEND )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockEnd( &iStyle ) );

   hb_storni( iStyle, 2 );
}

/* virtual int blockLookback () const */
HB_FUNC( QT_QSCILEXER_BLOCKLOOKBACK )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retni( ( p )->blockLookback() );
}

/* virtual const char * blockStart (int *style=0) const */
HB_FUNC( QT_QSCILEXER_BLOCKSTART )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockStart( &iStyle ) );

   hb_storni( iStyle, 2 );
}

/* virtual const char * blockStartKeyword (int *style=0) const */
HB_FUNC( QT_QSCILEXER_BLOCKSTARTKEYWORD )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockStartKeyword( &iStyle ) );

   hb_storni( iStyle, 2 );
}

/* virtual int braceStyle () const */
HB_FUNC( QT_QSCILEXER_BRACESTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retni( ( p )->braceStyle() );
}

/* virtual bool caseSensitive () const */
HB_FUNC( QT_QSCILEXER_CASESENSITIVE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retl( ( p )->caseSensitive() );
}

/* virtual QColor color (int style) const */
HB_FUNC( QT_QSCILEXER_COLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color( hb_parni( 2 ) ) ), true ) );
}

/* virtual bool eolFill (int style) const */
HB_FUNC( QT_QSCILEXER_EOLFILL )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retl( ( p )->eolFill( hb_parni( 2 ) ) );
}

/* virtual QFont font (int style) const */
HB_FUNC( QT_QSCILEXER_FONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hb_parni( 2 ) ) ), true ) );
}

/* virtual int indentationGuideView () const */
HB_FUNC( QT_QSCILEXER_INDENTATIONGUIDEVIEW )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retni( ( p )->indentationGuideView() );
}

/* virtual const char * keywords (int set) const */
HB_FUNC( QT_QSCILEXER_KEYWORDS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retc( ( p )->keywords( hb_parni( 2 ) ) );
}

/* virtual int defaultStyle () const */
HB_FUNC( QT_QSCILEXER_DEFAULTSTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retni( ( p )->defaultStyle() );
}

/* virtual QString description (int style) const =0 */
HB_FUNC( QT_QSCILEXER_DESCRIPTION )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retstr_utf8( ( p )->description( hb_parni( 2 ) ).toUtf8().data() );
}

/* virtual QColor paper (int style) const */
HB_FUNC( QT_QSCILEXER_PAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->paper( hb_parni( 2 ) ) ), true ) );
}

/* QColor defaultColor () const */
HB_FUNC( QT_QSCILEXER_DEFAULTCOLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultColor() ), true ) );
}

/* virtual QColor defaultColor (int style) const */
HB_FUNC( QT_QSCILEXER_DEFAULTCOLOR_1 )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultColor( hb_parni( 2 ) ) ), true ) );
}

/* virtual bool defaultEolFill (int style) const */
HB_FUNC( QT_QSCILEXER_DEFAULTEOLFILL )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retl( ( p )->defaultEolFill( hb_parni( 2 ) ) );
}

/* QFont defaultFont () const */
HB_FUNC( QT_QSCILEXER_DEFAULTFONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont() ), true ) );
}

/* virtual QFont defaultFont (int style) const */
HB_FUNC( QT_QSCILEXER_DEFAULTFONT_1 )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont( hb_parni( 2 ) ) ), true ) );
}

/* QColor defaultPaper () const */
HB_FUNC( QT_QSCILEXER_DEFAULTPAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultPaper() ), true ) );
}

/* virtual QColor defaultPaper (int style) const */
HB_FUNC( QT_QSCILEXER_DEFAULTPAPER_1 )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultPaper( hb_parni( 2 ) ) ), true ) );
}

/* QsciScintilla * editor () const */
HB_FUNC( QT_QSCILEXER_EDITOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciScintilla( ( p )->editor(), false ) );
}

/* virtual void setEditor (QsciScintilla *editor) */
HB_FUNC( QT_QSCILEXER_SETEDITOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setEditor( hbqt_par_QsciScintilla( 2 ) );
}

/* bool readSettings (QSettings &qs, const char *prefix="/Scintilla") */
HB_FUNC( QT_QSCILEXER_READSETTINGS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retl( ( p )->readSettings( *hbqt_par_QSettings( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* virtual void refreshProperties () */
HB_FUNC( QT_QSCILEXER_REFRESHPROPERTIES )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->refreshProperties();
}

/* virtual int styleBitsNeeded () const */
HB_FUNC( QT_QSCILEXER_STYLEBITSNEEDED )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retni( ( p )->styleBitsNeeded() );
}

/* virtual const char * wordCharacters () const */
HB_FUNC( QT_QSCILEXER_WORDCHARACTERS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retc( ( p )->wordCharacters() );
}

/* bool writeSettings (QSettings &qs, const char *prefix="/Scintilla") const */
HB_FUNC( QT_QSCILEXER_WRITESETTINGS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      hb_retl( ( p )->writeSettings( *hbqt_par_QSettings( 2 ), ( const char * ) hb_parc( 3 ) ) );
}

/* void setAPIs (QsciAbstractAPIs *apis) */
HB_FUNC( QT_QSCILEXER_SETAPIS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setAPIs( hbqt_par_QsciAbstractAPIs( 2 ) );
}

/* void setDefaultColor (const QColor &c) */
HB_FUNC( QT_QSCILEXER_SETDEFAULTCOLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setDefaultColor( *hbqt_par_QColor( 2 ) );
}

/* void setDefaultFont (const QFont &f) */
HB_FUNC( QT_QSCILEXER_SETDEFAULTFONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setDefaultFont( *hbqt_par_QFont( 2 ) );
}

/* void setDefaultPaper (const QColor &c) */
HB_FUNC( QT_QSCILEXER_SETDEFAULTPAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setDefaultPaper( *hbqt_par_QColor( 2 ) );
}

/* virtual void setAutoIndentStyle (int autoindentstyle) */
HB_FUNC( QT_QSCILEXER_SETAUTOINDENTSTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setAutoIndentStyle( hb_parni( 2 ) );
}

/* virtual void setColor (const QColor &c, int style=-1) */
HB_FUNC( QT_QSCILEXER_SETCOLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
}

/* virtual void setEolFill (bool eoffill, int style=-1) */
HB_FUNC( QT_QSCILEXER_SETEOLFILL )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setEolFill( hb_parl( 2 ), hb_parnidef( 3, -1 ) );
}

/* virtual void setFont (const QFont &f, int style=-1) */
HB_FUNC( QT_QSCILEXER_SETFONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ), hb_parnidef( 3, -1 ) );
}

/* virtual void setPaper (const QColor &c, int style=-1) */
HB_FUNC( QT_QSCILEXER_SETPAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
      ( p )->setPaper( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
