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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Category { Mark_NonSpacing, Mark_SpacingCombining, Mark_Enclosing, Number_DecimalDigit, ..., NoCategory }
 *  enum Decomposition { NoDecomposition, Canonical, Circle, Compat, ..., Wide }
 *  enum Direction { DirAL, DirAN, DirB, DirBN, ..., DirWS }
 *  enum Joining { Center, Dual, OtherJoining, Right }
 *  enum SpecialCharacter { Null, Nbsp, ReplacementCharacter, ObjectReplacementCharacter, ..., LineSeparator }
 *  enum UnicodeVersion { Unicode_1_1, Unicode_2_0, Unicode_2_1_2, Unicode_3_0, ..., Unicode_Unassigned }
 */

/*
 *  Constructed[ 33/35 [ 94.29% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  uchar cell () const
 *  uchar row () const
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
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QChar( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QChar * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QChar;
   p->type = HBQT_TYPE_QChar;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QChar", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QChar", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCHAR )
{
   QChar * pObj = NULL;

   pObj = new QChar() ;

   hb_retptrGC( hbqt_gcAllocate_QChar( ( void * ) pObj, true ) );
}

/*
 * Category category () const
 */
HB_FUNC( QT_QCHAR_CATEGORY )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( QChar::Category ) ( p )->category() );
   }
}

/*
 * unsigned char combiningClass () const
 */
HB_FUNC( QT_QCHAR_COMBININGCLASS )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( p )->combiningClass() );
   }
}

/*
 * QString decomposition () const
 */
HB_FUNC( QT_QCHAR_DECOMPOSITION )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->decomposition().toUtf8().data() );
   }
}

/*
 * Decomposition decompositionTag () const
 */
HB_FUNC( QT_QCHAR_DECOMPOSITIONTAG )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( QChar::Decomposition ) ( p )->decompositionTag() );
   }
}

/*
 * int digitValue () const
 */
HB_FUNC( QT_QCHAR_DIGITVALUE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( p )->digitValue() );
   }
}

/*
 * Direction direction () const
 */
HB_FUNC( QT_QCHAR_DIRECTION )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( QChar::Direction ) ( p )->direction() );
   }
}

/*
 * bool hasMirrored () const
 */
HB_FUNC( QT_QCHAR_HASMIRRORED )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->hasMirrored() );
   }
}

/*
 * bool isDigit () const
 */
HB_FUNC( QT_QCHAR_ISDIGIT )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isDigit() );
   }
}

/*
 * bool isHighSurrogate () const
 */
HB_FUNC( QT_QCHAR_ISHIGHSURROGATE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isHighSurrogate() );
   }
}

/*
 * bool isLetter () const
 */
HB_FUNC( QT_QCHAR_ISLETTER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isLetter() );
   }
}

/*
 * bool isLetterOrNumber () const
 */
HB_FUNC( QT_QCHAR_ISLETTERORNUMBER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isLetterOrNumber() );
   }
}

/*
 * bool isLowSurrogate () const
 */
HB_FUNC( QT_QCHAR_ISLOWSURROGATE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isLowSurrogate() );
   }
}

/*
 * bool isLower () const
 */
HB_FUNC( QT_QCHAR_ISLOWER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isLower() );
   }
}

/*
 * bool isMark () const
 */
HB_FUNC( QT_QCHAR_ISMARK )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isMark() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QCHAR_ISNULL )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * bool isNumber () const
 */
HB_FUNC( QT_QCHAR_ISNUMBER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isNumber() );
   }
}

/*
 * bool isPrint () const
 */
HB_FUNC( QT_QCHAR_ISPRINT )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isPrint() );
   }
}

/*
 * bool isPunct () const
 */
HB_FUNC( QT_QCHAR_ISPUNCT )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isPunct() );
   }
}

/*
 * bool isSpace () const
 */
HB_FUNC( QT_QCHAR_ISSPACE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isSpace() );
   }
}

/*
 * bool isSymbol () const
 */
HB_FUNC( QT_QCHAR_ISSYMBOL )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isSymbol() );
   }
}

/*
 * bool isTitleCase () const
 */
HB_FUNC( QT_QCHAR_ISTITLECASE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isTitleCase() );
   }
}

/*
 * bool isUpper () const
 */
HB_FUNC( QT_QCHAR_ISUPPER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retl( ( p )->isUpper() );
   }
}

/*
 * Joining joining () const
 */
HB_FUNC( QT_QCHAR_JOINING )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( QChar::Joining ) ( p )->joining() );
   }
}

/*
 * QChar mirroredChar () const
 */
HB_FUNC( QT_QCHAR_MIRROREDCHAR )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->mirroredChar() ), true ) );
   }
}

/*
 * char toAscii () const
 */
HB_FUNC( QT_QCHAR_TOASCII )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( p )->toAscii() );
   }
}

/*
 * QChar toCaseFolded () const
 */
HB_FUNC( QT_QCHAR_TOCASEFOLDED )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toCaseFolded() ), true ) );
   }
}

/*
 * char toLatin1 () const
 */
HB_FUNC( QT_QCHAR_TOLATIN1 )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( p )->toLatin1() );
   }
}

/*
 * QChar toLower () const
 */
HB_FUNC( QT_QCHAR_TOLOWER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toLower() ), true ) );
   }
}

/*
 * QChar toTitleCase () const
 */
HB_FUNC( QT_QCHAR_TOTITLECASE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toTitleCase() ), true ) );
   }
}

/*
 * QChar toUpper () const
 */
HB_FUNC( QT_QCHAR_TOUPPER )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toUpper() ), true ) );
   }
}

/*
 * ushort & unicode ()
 */
HB_FUNC( QT_QCHAR_UNICODE )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( p )->unicode() );
   }
}

/*
 * ushort unicode () const
 */
HB_FUNC( QT_QCHAR_UNICODE_1 )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( p )->unicode() );
   }
}

/*
 * UnicodeVersion unicodeVersion () const
 */
HB_FUNC( QT_QCHAR_UNICODEVERSION )
{
   QChar * p = hbqt_par_QChar( 1 );
   if( p )
   {
      hb_retni( ( QChar::UnicodeVersion ) ( p )->unicodeVersion() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
