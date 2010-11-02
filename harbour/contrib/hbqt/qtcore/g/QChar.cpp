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
 *  enum Category { Mark_NonSpacing, Mark_SpacingCombining, Mark_Enclosing, Number_DecimalDigit, ..., NoCategory }
 *  enum Decomposition { NoDecomposition, Canonical, Circle, Compat, ..., Wide }
 *  enum Direction { DirAL, DirAN, DirB, DirBN, ..., DirWS }
 *  enum Joining { Center, Dual, OtherJoining, Right }
 *  enum SpecialCharacter { Null, Nbsp, ReplacementCharacter, ObjectReplacementCharacter, ..., LineSeparator }
 *  enum UnicodeVersion { Unicode_1_1, Unicode_2_0, Unicode_2_1_2, Unicode_3_0, ..., Unicode_Unassigned }
 */

/*
 *  Constructed[ 32/34 [ 94.12% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  uchar cell () const
 *  uchar row () const
 *
 *  *** Commented out protostypes ***
 *
 *  //ushort unicode () const
 */

#include <QtCore/QPointer>

#include <QtCore/QChar>


/*
 * QChar ()
 * QChar ( char ch )
 * QChar ( uchar ch )
 * QChar ( QLatin1Char ch )
 * QChar ( uchar cell, uchar row )
 * QChar ( ushort code )
 * QChar ( short code )
 * QChar ( uint code )
 * QChar ( int code )
 * QChar ( SpecialCharacter ch )
 */

typedef struct
{
   QChar * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QChar;

HBQT_GC_FUNC( hbqt_gcRelease_QChar )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QChar( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QChar * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QChar;
   p->type = HBQT_TYPE_QChar;

   return p;
}

HB_FUNC( QT_QCHAR )
{
   QChar * pObj = NULL;

   pObj = new QChar() ;

   hb_retptrGC( hbqt_gcAllocate_QChar( ( void * ) pObj, true ) );
}

/* Category category () const */
HB_FUNC( QT_QCHAR_CATEGORY )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( QChar::Category ) ( p )->category() );
}

/* unsigned char combiningClass () const */
HB_FUNC( QT_QCHAR_COMBININGCLASS )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( p )->combiningClass() );
}

/* QString decomposition () const */
HB_FUNC( QT_QCHAR_DECOMPOSITION )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retstr_utf8( ( p )->decomposition().toUtf8().data() );
}

/* Decomposition decompositionTag () const */
HB_FUNC( QT_QCHAR_DECOMPOSITIONTAG )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( QChar::Decomposition ) ( p )->decompositionTag() );
}

/* int digitValue () const */
HB_FUNC( QT_QCHAR_DIGITVALUE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( p )->digitValue() );
}

/* Direction direction () const */
HB_FUNC( QT_QCHAR_DIRECTION )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( QChar::Direction ) ( p )->direction() );
}

/* bool hasMirrored () const */
HB_FUNC( QT_QCHAR_HASMIRRORED )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->hasMirrored() );
}

/* bool isDigit () const */
HB_FUNC( QT_QCHAR_ISDIGIT )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isDigit() );
}

/* bool isHighSurrogate () const */
HB_FUNC( QT_QCHAR_ISHIGHSURROGATE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isHighSurrogate() );
}

/* bool isLetter () const */
HB_FUNC( QT_QCHAR_ISLETTER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isLetter() );
}

/* bool isLetterOrNumber () const */
HB_FUNC( QT_QCHAR_ISLETTERORNUMBER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isLetterOrNumber() );
}

/* bool isLowSurrogate () const */
HB_FUNC( QT_QCHAR_ISLOWSURROGATE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isLowSurrogate() );
}

/* bool isLower () const */
HB_FUNC( QT_QCHAR_ISLOWER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isLower() );
}

/* bool isMark () const */
HB_FUNC( QT_QCHAR_ISMARK )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isMark() );
}

/* bool isNull () const */
HB_FUNC( QT_QCHAR_ISNULL )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* bool isNumber () const */
HB_FUNC( QT_QCHAR_ISNUMBER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isNumber() );
}

/* bool isPrint () const */
HB_FUNC( QT_QCHAR_ISPRINT )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isPrint() );
}

/* bool isPunct () const */
HB_FUNC( QT_QCHAR_ISPUNCT )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isPunct() );
}

/* bool isSpace () const */
HB_FUNC( QT_QCHAR_ISSPACE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isSpace() );
}

/* bool isSymbol () const */
HB_FUNC( QT_QCHAR_ISSYMBOL )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isSymbol() );
}

/* bool isTitleCase () const */
HB_FUNC( QT_QCHAR_ISTITLECASE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isTitleCase() );
}

/* bool isUpper () const */
HB_FUNC( QT_QCHAR_ISUPPER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retl( ( p )->isUpper() );
}

/* Joining joining () const */
HB_FUNC( QT_QCHAR_JOINING )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( QChar::Joining ) ( p )->joining() );
}

/* QChar mirroredChar () const */
HB_FUNC( QT_QCHAR_MIRROREDCHAR )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->mirroredChar() ), true ) );
}

/* char toAscii () const */
HB_FUNC( QT_QCHAR_TOASCII )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( p )->toAscii() );
}

/* QChar toCaseFolded () const */
HB_FUNC( QT_QCHAR_TOCASEFOLDED )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toCaseFolded() ), true ) );
}

/* char toLatin1 () const */
HB_FUNC( QT_QCHAR_TOLATIN1 )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( p )->toLatin1() );
}

/* QChar toLower () const */
HB_FUNC( QT_QCHAR_TOLOWER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toLower() ), true ) );
}

/* QChar toTitleCase () const */
HB_FUNC( QT_QCHAR_TOTITLECASE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toTitleCase() ), true ) );
}

/* QChar toUpper () const */
HB_FUNC( QT_QCHAR_TOUPPER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toUpper() ), true ) );
}

/* ushort & unicode () */
HB_FUNC( QT_QCHAR_UNICODE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( p )->unicode() );
}

/* UnicodeVersion unicodeVersion () const */
HB_FUNC( QT_QCHAR_UNICODEVERSION )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
      hb_retni( ( QChar::UnicodeVersion ) ( p )->unicodeVersion() );
}


#endif /* #if QT_VERSION >= 0x040500 */
