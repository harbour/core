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
 *  enum {
 *    Default = 0, Comment = 1, CommentLine = 2,
 *    CommentDoc = 3, Number = 4, Keyword = 5,
 *    DoubleQuotedString = 6, SingleQuotedString = 7, UUID = 8,
 *    PreProcessor = 9, Operator = 10, Identifier = 11,
 *    UnclosedString = 12, VerbatimString = 13, Regex = 14,
 *    CommentLineDoc = 15, KeywordSet2 = 16, CommentDocKeyword = 17,
 *    CommentDocKeywordError = 18, GlobalClass = 19
 *  }
 */

/*
 *  Constructed[ 27/27 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <qscilexerflagship.h>


/*
 * QsciLexerFlagship (QObject *parent=0, bool caseInsensitiveKeywords=false)
 * virtual ~QsciLexerFlagship ()
 *
 */

typedef struct
{
   QPointer< QsciLexerFlagship > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciLexerFlagship;

HBQT_GC_FUNC( hbqt_gcRelease_QsciLexerFlagship )
{
   HBQT_GC_T_QsciLexerFlagship * p = ( HBQT_GC_T_QsciLexerFlagship * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QsciLexerFlagship * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciLexerFlagship( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciLexerFlagship * p = ( HBQT_GC_T_QsciLexerFlagship * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciLexerFlagship ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciLexerFlagship >( ( QsciLexerFlagship * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciLexerFlagship;
   p->type = HBQT_TYPE_QsciLexerFlagship;

   return p;
}

HB_FUNC( QT_QSCILEXERFLAGSHIP )
{
   QsciLexerFlagship * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QsciLexerFlagship( hbqt_par_QObject( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISLOG( 1 ) )
   {
      pObj = new QsciLexerFlagship( 0, hb_parl( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISLOG( 2 ) )
   {
      pObj = new QsciLexerFlagship( hbqt_par_QObject( 1 ), hb_parl( 2 ) ) ;
   }
   else
   {
      pObj = new QsciLexerFlagship() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QsciLexerFlagship( ( void * ) pObj, true ) );
}

/* const char * language () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_LANGUAGE )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retc( ( p )->language() );
}

/* const char * lexer () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_LEXER )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retc( ( p )->lexer() );
}

/* QStringList autoCompletionWordSeparators () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_AUTOCOMPLETIONWORDSEPARATORS )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->autoCompletionWordSeparators() ), true ) );
}

/* const char * blockEnd (int *style=0) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_BLOCKEND )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockEnd( &iStyle ) );

   hb_storni( iStyle, 2 );
}

/* const char * blockStart (int *style=0) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_BLOCKSTART )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockStart( &iStyle ) );

   hb_storni( iStyle, 2 );
}

/* const char * blockStartKeyword (int *style=0) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_BLOCKSTARTKEYWORD )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockStartKeyword( &iStyle ) );

   hb_storni( iStyle, 2 );
}

/* int braceStyle () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_BRACESTYLE )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retni( ( p )->braceStyle() );
}

/* const char * wordCharacters () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_WORDCHARACTERS )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retc( ( p )->wordCharacters() );
}

/* QColor defaultColor (int style) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_DEFAULTCOLOR )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultColor( hb_parni( 2 ) ) ), true ) );
}

/* bool defaultEolFill (int style) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_DEFAULTEOLFILL )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retl( ( p )->defaultEolFill( hb_parni( 2 ) ) );
}

/* QFont defaultFont (int style) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_DEFAULTFONT )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont( hb_parni( 2 ) ) ), true ) );
}

/* QColor defaultPaper (int style) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_DEFAULTPAPER )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultPaper( hb_parni( 2 ) ) ), true ) );
}

/* const char * keywords (int set) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_KEYWORDS )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retc( ( p )->keywords( hb_parni( 2 ) ) );
}

/* QString description (int style) const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_DESCRIPTION )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retstr_utf8( ( p )->description( hb_parni( 2 ) ).toUtf8().data() );
}

/* void refreshProperties () */
HB_FUNC( QT_QSCILEXERFLAGSHIP_REFRESHPROPERTIES )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      ( p )->refreshProperties();
}

/* bool foldAtElse () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_FOLDATELSE )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retl( ( p )->foldAtElse() );
}

/* bool foldComments () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_FOLDCOMMENTS )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retl( ( p )->foldComments() );
}

/* bool foldCompact () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_FOLDCOMPACT )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retl( ( p )->foldCompact() );
}

/* bool foldPreprocessor () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_FOLDPREPROCESSOR )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retl( ( p )->foldPreprocessor() );
}

/* bool stylePreprocessor () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_STYLEPREPROCESSOR )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retl( ( p )->stylePreprocessor() );
}

/* void setDollarsAllowed (bool allowed) */
HB_FUNC( QT_QSCILEXERFLAGSHIP_SETDOLLARSALLOWED )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      ( p )->setDollarsAllowed( hb_parl( 2 ) );
}

/* bool dollarsAllowed () const */
HB_FUNC( QT_QSCILEXERFLAGSHIP_DOLLARSALLOWED )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      hb_retl( ( p )->dollarsAllowed() );
}

/* virtual void setFoldAtElse (bool fold) */
HB_FUNC( QT_QSCILEXERFLAGSHIP_SETFOLDATELSE )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      ( p )->setFoldAtElse( hb_parl( 2 ) );
}

/* virtual void setFoldComments (bool fold) */
HB_FUNC( QT_QSCILEXERFLAGSHIP_SETFOLDCOMMENTS )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      ( p )->setFoldComments( hb_parl( 2 ) );
}

/* virtual void setFoldCompact (bool fold) */
HB_FUNC( QT_QSCILEXERFLAGSHIP_SETFOLDCOMPACT )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      ( p )->setFoldCompact( hb_parl( 2 ) );
}

/* virtual void setFoldPreprocessor (bool fold) */
HB_FUNC( QT_QSCILEXERFLAGSHIP_SETFOLDPREPROCESSOR )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      ( p )->setFoldPreprocessor( hb_parl( 2 ) );
}

/* virtual void setStylePreprocessor (bool style) */
HB_FUNC( QT_QSCILEXERFLAGSHIP_SETSTYLEPREPROCESSOR )
{
   QsciLexerFlagship * p = hbqt_par_QsciLexerFlagship( 1 );
   if( p )
      ( p )->setStylePreprocessor( hb_parl( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
