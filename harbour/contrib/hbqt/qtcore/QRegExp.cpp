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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum CaretMode { CaretAtZero, CaretAtOffset, CaretWontMatch }
 *  enum PatternSyntax { RegExp, RegExp2, Wildcard, FixedString }
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QRegExp;

QT_G_FUNC( hbqt_gcRelease_QRegExp )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QRegExp   /.\\", p->ph ) );
         delete ( ( QRegExp * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QRegExp   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QRegExp    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QRegExp    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QRegExp( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QRegExp * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRegExp;
   p->type = QT_TYPE_QRegExp;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QRegExp", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QRegExp", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QREGEXP )
{
   QRegExp * pObj = NULL;

   pObj = new QRegExp() ;

   hb_retptrGC( hbqt_gcAllocate_QRegExp( ( void * ) pObj, true ) );
}

/*
 * QString cap ( int nth = 0 ) const
 */
HB_FUNC( QT_QREGEXP_CAP )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retc( ( p )->cap( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_CAP FP=hb_retc( ( p )->cap( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList capturedTexts () const
 */
HB_FUNC( QT_QREGEXP_CAPTUREDTEXTS )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->capturedTexts() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_CAPTUREDTEXTS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->capturedTexts() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::CaseSensitivity caseSensitivity () const
 */
HB_FUNC( QT_QREGEXP_CASESENSITIVITY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( Qt::CaseSensitivity ) ( p )->caseSensitivity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_CASESENSITIVITY FP=hb_retni( ( Qt::CaseSensitivity ) ( p )->caseSensitivity() ); p is NULL" ) );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QREGEXP_ERRORSTRING )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retc( ( p )->errorString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_ERRORSTRING FP=hb_retc( ( p )->errorString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool exactMatch ( const QString & str ) const
 */
HB_FUNC( QT_QREGEXP_EXACTMATCH )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retl( ( p )->exactMatch( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_EXACTMATCH FP=hb_retl( ( p )->exactMatch( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int indexIn ( const QString & str, int offset = 0, CaretMode caretMode = CaretAtZero ) const
 */
HB_FUNC( QT_QREGEXP_INDEXIN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->indexIn( hbqt_par_QString( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_INDEXIN FP=hb_retni( ( p )->indexIn( hbqt_par_QString( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) ); p is NULL" ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QREGEXP_ISEMPTY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_ISEMPTY FP=hb_retl( ( p )->isEmpty() ); p is NULL" ) );
   }
}

/*
 * bool isMinimal () const
 */
HB_FUNC( QT_QREGEXP_ISMINIMAL )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retl( ( p )->isMinimal() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_ISMINIMAL FP=hb_retl( ( p )->isMinimal() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QREGEXP_ISVALID )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * int lastIndexIn ( const QString & str, int offset = -1, CaretMode caretMode = CaretAtZero ) const
 */
HB_FUNC( QT_QREGEXP_LASTINDEXIN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->lastIndexIn( hbqt_par_QString( 2 ), hb_parnidef( 3, -1 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_LASTINDEXIN FP=hb_retni( ( p )->lastIndexIn( hbqt_par_QString( 2 ), hb_parnidef( 3, -1 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) ); p is NULL" ) );
   }
}

/*
 * int matchedLength () const
 */
HB_FUNC( QT_QREGEXP_MATCHEDLENGTH )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->matchedLength() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_MATCHEDLENGTH FP=hb_retni( ( p )->matchedLength() ); p is NULL" ) );
   }
}

/*
 * int numCaptures () const
 */
HB_FUNC( QT_QREGEXP_NUMCAPTURES )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->numCaptures() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_NUMCAPTURES FP=hb_retni( ( p )->numCaptures() ); p is NULL" ) );
   }
}

/*
 * QString pattern () const
 */
HB_FUNC( QT_QREGEXP_PATTERN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retc( ( p )->pattern().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_PATTERN FP=hb_retc( ( p )->pattern().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * PatternSyntax patternSyntax () const
 */
HB_FUNC( QT_QREGEXP_PATTERNSYNTAX )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( QRegExp::PatternSyntax ) ( p )->patternSyntax() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_PATTERNSYNTAX FP=hb_retni( ( QRegExp::PatternSyntax ) ( p )->patternSyntax() ); p is NULL" ) );
   }
}

/*
 * int pos ( int nth = 0 ) const
 */
HB_FUNC( QT_QREGEXP_POS )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      hb_retni( ( p )->pos( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_POS FP=hb_retni( ( p )->pos( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setCaseSensitivity ( Qt::CaseSensitivity cs )
 */
HB_FUNC( QT_QREGEXP_SETCASESENSITIVITY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      ( p )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_SETCASESENSITIVITY FP=( p )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimal ( bool minimal )
 */
HB_FUNC( QT_QREGEXP_SETMINIMAL )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      ( p )->setMinimal( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_SETMINIMAL FP=( p )->setMinimal( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPattern ( const QString & pattern )
 */
HB_FUNC( QT_QREGEXP_SETPATTERN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      ( p )->setPattern( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_SETPATTERN FP=( p )->setPattern( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPatternSyntax ( PatternSyntax syntax )
 */
HB_FUNC( QT_QREGEXP_SETPATTERNSYNTAX )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
      ( p )->setPatternSyntax( ( QRegExp::PatternSyntax ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QREGEXP_SETPATTERNSYNTAX FP=( p )->setPatternSyntax( ( QRegExp::PatternSyntax ) hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
