/*
 * $Id: hbqtgen.prg 14016 2010-02-27 19:53:19Z vouchcac $
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

#include "../hbqt.h"

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
 *  Constructed[ 32/35 [ 91.43% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  uchar cell () const
 *  unsigned char combiningClass () const
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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
} QGC_POINTER_QChar;

QT_G_FUNC( hbqt_gcRelease_QChar )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QChar( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QChar;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QChar                      ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QCHAR )
{
   void * pObj = NULL;

   pObj = new QChar() ;

   hb_retptrGC( hbqt_gcAllocate_QChar( pObj, true ) );
}

/*
 * Category category () const
 */
HB_FUNC( QT_QCHAR_CATEGORY )
{
   hb_retni( ( QChar::Category ) hbqt_par_QChar( 1 )->category() );
}

/*
 * QString decomposition () const
 */
HB_FUNC( QT_QCHAR_DECOMPOSITION )
{
   hb_retc( hbqt_par_QChar( 1 )->decomposition().toAscii().data() );
}

/*
 * Decomposition decompositionTag () const
 */
HB_FUNC( QT_QCHAR_DECOMPOSITIONTAG )
{
   hb_retni( ( QChar::Decomposition ) hbqt_par_QChar( 1 )->decompositionTag() );
}

/*
 * int digitValue () const
 */
HB_FUNC( QT_QCHAR_DIGITVALUE )
{
   hb_retni( hbqt_par_QChar( 1 )->digitValue() );
}

/*
 * Direction direction () const
 */
HB_FUNC( QT_QCHAR_DIRECTION )
{
   hb_retni( ( QChar::Direction ) hbqt_par_QChar( 1 )->direction() );
}

/*
 * bool hasMirrored () const
 */
HB_FUNC( QT_QCHAR_HASMIRRORED )
{
   hb_retl( hbqt_par_QChar( 1 )->hasMirrored() );
}

/*
 * bool isDigit () const
 */
HB_FUNC( QT_QCHAR_ISDIGIT )
{
   hb_retl( hbqt_par_QChar( 1 )->isDigit() );
}

/*
 * bool isHighSurrogate () const
 */
HB_FUNC( QT_QCHAR_ISHIGHSURROGATE )
{
   hb_retl( hbqt_par_QChar( 1 )->isHighSurrogate() );
}

/*
 * bool isLetter () const
 */
HB_FUNC( QT_QCHAR_ISLETTER )
{
   hb_retl( hbqt_par_QChar( 1 )->isLetter() );
}

/*
 * bool isLetterOrNumber () const
 */
HB_FUNC( QT_QCHAR_ISLETTERORNUMBER )
{
   hb_retl( hbqt_par_QChar( 1 )->isLetterOrNumber() );
}

/*
 * bool isLowSurrogate () const
 */
HB_FUNC( QT_QCHAR_ISLOWSURROGATE )
{
   hb_retl( hbqt_par_QChar( 1 )->isLowSurrogate() );
}

/*
 * bool isLower () const
 */
HB_FUNC( QT_QCHAR_ISLOWER )
{
   hb_retl( hbqt_par_QChar( 1 )->isLower() );
}

/*
 * bool isMark () const
 */
HB_FUNC( QT_QCHAR_ISMARK )
{
   hb_retl( hbqt_par_QChar( 1 )->isMark() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QCHAR_ISNULL )
{
   hb_retl( hbqt_par_QChar( 1 )->isNull() );
}

/*
 * bool isNumber () const
 */
HB_FUNC( QT_QCHAR_ISNUMBER )
{
   hb_retl( hbqt_par_QChar( 1 )->isNumber() );
}

/*
 * bool isPrint () const
 */
HB_FUNC( QT_QCHAR_ISPRINT )
{
   hb_retl( hbqt_par_QChar( 1 )->isPrint() );
}

/*
 * bool isPunct () const
 */
HB_FUNC( QT_QCHAR_ISPUNCT )
{
   hb_retl( hbqt_par_QChar( 1 )->isPunct() );
}

/*
 * bool isSpace () const
 */
HB_FUNC( QT_QCHAR_ISSPACE )
{
   hb_retl( hbqt_par_QChar( 1 )->isSpace() );
}

/*
 * bool isSymbol () const
 */
HB_FUNC( QT_QCHAR_ISSYMBOL )
{
   hb_retl( hbqt_par_QChar( 1 )->isSymbol() );
}

/*
 * bool isTitleCase () const
 */
HB_FUNC( QT_QCHAR_ISTITLECASE )
{
   hb_retl( hbqt_par_QChar( 1 )->isTitleCase() );
}

/*
 * bool isUpper () const
 */
HB_FUNC( QT_QCHAR_ISUPPER )
{
   hb_retl( hbqt_par_QChar( 1 )->isUpper() );
}

/*
 * Joining joining () const
 */
HB_FUNC( QT_QCHAR_JOINING )
{
   hb_retni( ( QChar::Joining ) hbqt_par_QChar( 1 )->joining() );
}

/*
 * QChar mirroredChar () const
 */
HB_FUNC( QT_QCHAR_MIRROREDCHAR )
{
   hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( hbqt_par_QChar( 1 )->mirroredChar() ), true ) );
}

/*
 * char toAscii () const
 */
HB_FUNC( QT_QCHAR_TOASCII )
{
   hb_retni( hbqt_par_QChar( 1 )->toAscii() );
}

/*
 * QChar toCaseFolded () const
 */
HB_FUNC( QT_QCHAR_TOCASEFOLDED )
{
   hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( hbqt_par_QChar( 1 )->toCaseFolded() ), true ) );
}

/*
 * char toLatin1 () const
 */
HB_FUNC( QT_QCHAR_TOLATIN1 )
{
   hb_retni( hbqt_par_QChar( 1 )->toLatin1() );
}

/*
 * QChar toLower () const
 */
HB_FUNC( QT_QCHAR_TOLOWER )
{
   hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( hbqt_par_QChar( 1 )->toLower() ), true ) );
}

/*
 * QChar toTitleCase () const
 */
HB_FUNC( QT_QCHAR_TOTITLECASE )
{
   hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( hbqt_par_QChar( 1 )->toTitleCase() ), true ) );
}

/*
 * QChar toUpper () const
 */
HB_FUNC( QT_QCHAR_TOUPPER )
{
   hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( hbqt_par_QChar( 1 )->toUpper() ), true ) );
}

/*
 * ushort & unicode ()
 */
HB_FUNC( QT_QCHAR_UNICODE )
{
   hb_retni( hbqt_par_QChar( 1 )->unicode() );
}

/*
 * ushort unicode () const
 */
HB_FUNC( QT_QCHAR_UNICODE_1 )
{
   hb_retni( hbqt_par_QChar( 1 )->unicode() );
}

/*
 * UnicodeVersion unicodeVersion () const
 */
HB_FUNC( QT_QCHAR_UNICODEVERSION )
{
   hb_retni( ( QChar::UnicodeVersion ) hbqt_par_QChar( 1 )->unicodeVersion() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
