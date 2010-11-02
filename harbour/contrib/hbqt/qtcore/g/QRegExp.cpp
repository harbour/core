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

#if QT_VERSION >= 0x040500

/*
 *  enum CaretMode { CaretAtZero, CaretAtOffset, CaretWontMatch }
 *  enum PatternSyntax { RegExp, RegExp2, Wildcard, FixedString }
 */

/*
 *  Constructed[ 19/19 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QRegExp>
#include <QtCore/QStringList>

/* QRegExp ()
 * QRegExp ( const QString & pattern, Qt::CaseSensitivity cs = Qt::CaseSensitive, PatternSyntax syntax = RegExp )
 * QRegExp ( const QRegExp & rx )
 * ~QRegExp ()
 */

typedef struct
{
   QRegExp * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRegExp;

HBQT_GC_FUNC( hbqt_gcRelease_QRegExp )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QRegExp * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QRegExp( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QRegExp * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRegExp;
   p->type = HBQT_TYPE_QRegExp;

   return p;
}

HB_FUNC( QT_QREGEXP )
{
   QRegExp * pObj = NULL;

   pObj = new QRegExp() ;

   hb_retptrGC( hbqt_gcAllocate_QRegExp( ( void * ) pObj, true ) );
}

/* QString cap ( int nth = 0 ) const */
HB_FUNC( QT_QREGEXP_CAP )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retstr_utf8( ( p )->cap( hb_parni( 2 ) ).toUtf8().data() );
}

/* QStringList capturedTexts () const */
HB_FUNC( QT_QREGEXP_CAPTUREDTEXTS )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->capturedTexts() ), true ) );
}

/* Qt::CaseSensitivity caseSensitivity () const */
HB_FUNC( QT_QREGEXP_CASESENSITIVITY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( Qt::CaseSensitivity ) ( p )->caseSensitivity() );
}

/* QString errorString () const */
HB_FUNC( QT_QREGEXP_ERRORSTRING )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
}

/* bool exactMatch ( const QString & str ) const */
HB_FUNC( QT_QREGEXP_EXACTMATCH )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->exactMatch( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/* int indexIn ( const QString & str, int offset = 0, CaretMode caretMode = CaretAtZero ) const */
HB_FUNC( QT_QREGEXP_INDEXIN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->indexIn( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) );
      hb_strfree( pText );
   }
}

/* bool isEmpty () const */
HB_FUNC( QT_QREGEXP_ISEMPTY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool isMinimal () const */
HB_FUNC( QT_QREGEXP_ISMINIMAL )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retl( ( p )->isMinimal() );
}

/* bool isValid () const */
HB_FUNC( QT_QREGEXP_ISVALID )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* int lastIndexIn ( const QString & str, int offset = -1, CaretMode caretMode = CaretAtZero ) const */
HB_FUNC( QT_QREGEXP_LASTINDEXIN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->lastIndexIn( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, -1 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) );
      hb_strfree( pText );
   }
}

/* int matchedLength () const */
HB_FUNC( QT_QREGEXP_MATCHEDLENGTH )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->matchedLength() );
}

/* int numCaptures () const */
HB_FUNC( QT_QREGEXP_NUMCAPTURES )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->numCaptures() );
}

/* QString pattern () const */
HB_FUNC( QT_QREGEXP_PATTERN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retstr_utf8( ( p )->pattern().toUtf8().data() );
}

/* PatternSyntax patternSyntax () const */
HB_FUNC( QT_QREGEXP_PATTERNSYNTAX )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( QRegExp::PatternSyntax ) ( p )->patternSyntax() );
}

/* int pos ( int nth = 0 ) const */
HB_FUNC( QT_QREGEXP_POS )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->pos( hb_parni( 2 ) ) );
}

/* void setCaseSensitivity ( Qt::CaseSensitivity cs ) */
HB_FUNC( QT_QREGEXP_SETCASESENSITIVITY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      ( p )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
}

/* void setMinimal ( bool minimal ) */
HB_FUNC( QT_QREGEXP_SETMINIMAL )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      ( p )->setMinimal( hb_parl( 2 ) );
}

/* void setPattern ( const QString & pattern ) */
HB_FUNC( QT_QREGEXP_SETPATTERN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      void * pText;
      ( p )->setPattern( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setPatternSyntax ( PatternSyntax syntax ) */
HB_FUNC( QT_QREGEXP_SETPATTERNSYNTAX )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      ( p )->setPatternSyntax( ( QRegExp::PatternSyntax ) hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
