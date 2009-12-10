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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#include "hbapi.h"
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

QT_G_FUNC( release_QRegExp )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QRegExp                      p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QRegExp                     ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QRegExp * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QRegExp                     Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QRegExp                     Object Already deleted!" ) );
   }
}

void * gcAllocate_QRegExp( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QRegExp;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QRegExp                     %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QREGEXP )
{
   void * pObj = NULL;

   pObj = new QRegExp() ;

   hb_retptrGC( gcAllocate_QRegExp( pObj ) );
}
/*
 * QString cap ( int nth = 0 ) const
 */
HB_FUNC( QT_QREGEXP_CAP )
{
   hb_retc( hbqt_par_QRegExp( 1 )->cap( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QStringList capturedTexts () const
 */
HB_FUNC( QT_QREGEXP_CAPTUREDTEXTS )
{
   hb_retptrGC( gcAllocate_QStringList( new QStringList( hbqt_par_QRegExp( 1 )->capturedTexts() ) ) );
}

/*
 * Qt::CaseSensitivity caseSensitivity () const
 */
HB_FUNC( QT_QREGEXP_CASESENSITIVITY )
{
   hb_retni( ( Qt::CaseSensitivity ) hbqt_par_QRegExp( 1 )->caseSensitivity() );
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QREGEXP_ERRORSTRING )
{
   hb_retc( hbqt_par_QRegExp( 1 )->errorString().toAscii().data() );
}

/*
 * bool exactMatch ( const QString & str ) const
 */
HB_FUNC( QT_QREGEXP_EXACTMATCH )
{
   hb_retl( hbqt_par_QRegExp( 1 )->exactMatch( hbqt_par_QString( 2 ) ) );
}

/*
 * int indexIn ( const QString & str, int offset = 0, CaretMode caretMode = CaretAtZero ) const
 */
HB_FUNC( QT_QREGEXP_INDEXIN )
{
   hb_retni( hbqt_par_QRegExp( 1 )->indexIn( hbqt_par_QString( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QREGEXP_ISEMPTY )
{
   hb_retl( hbqt_par_QRegExp( 1 )->isEmpty() );
}

/*
 * bool isMinimal () const
 */
HB_FUNC( QT_QREGEXP_ISMINIMAL )
{
   hb_retl( hbqt_par_QRegExp( 1 )->isMinimal() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QREGEXP_ISVALID )
{
   hb_retl( hbqt_par_QRegExp( 1 )->isValid() );
}

/*
 * int lastIndexIn ( const QString & str, int offset = -1, CaretMode caretMode = CaretAtZero ) const
 */
HB_FUNC( QT_QREGEXP_LASTINDEXIN )
{
   hb_retni( hbqt_par_QRegExp( 1 )->lastIndexIn( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ), ( HB_ISNUM( 4 ) ? ( QRegExp::CaretMode ) hb_parni( 4 ) : ( QRegExp::CaretMode ) QRegExp::CaretAtZero ) ) );
}

/*
 * int matchedLength () const
 */
HB_FUNC( QT_QREGEXP_MATCHEDLENGTH )
{
   hb_retni( hbqt_par_QRegExp( 1 )->matchedLength() );
}

/*
 * int numCaptures () const
 */
HB_FUNC( QT_QREGEXP_NUMCAPTURES )
{
   hb_retni( hbqt_par_QRegExp( 1 )->numCaptures() );
}

/*
 * QString pattern () const
 */
HB_FUNC( QT_QREGEXP_PATTERN )
{
   hb_retc( hbqt_par_QRegExp( 1 )->pattern().toAscii().data() );
}

/*
 * PatternSyntax patternSyntax () const
 */
HB_FUNC( QT_QREGEXP_PATTERNSYNTAX )
{
   hb_retni( ( QRegExp::PatternSyntax ) hbqt_par_QRegExp( 1 )->patternSyntax() );
}

/*
 * int pos ( int nth = 0 ) const
 */
HB_FUNC( QT_QREGEXP_POS )
{
   hb_retni( hbqt_par_QRegExp( 1 )->pos( hb_parni( 2 ) ) );
}

/*
 * void setCaseSensitivity ( Qt::CaseSensitivity cs )
 */
HB_FUNC( QT_QREGEXP_SETCASESENSITIVITY )
{
   hbqt_par_QRegExp( 1 )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
}

/*
 * void setMinimal ( bool minimal )
 */
HB_FUNC( QT_QREGEXP_SETMINIMAL )
{
   hbqt_par_QRegExp( 1 )->setMinimal( hb_parl( 2 ) );
}

/*
 * void setPattern ( const QString & pattern )
 */
HB_FUNC( QT_QREGEXP_SETPATTERN )
{
   hbqt_par_QRegExp( 1 )->setPattern( hbqt_par_QString( 2 ) );
}

/*
 * void setPatternSyntax ( PatternSyntax syntax )
 */
HB_FUNC( QT_QREGEXP_SETPATTERNSYNTAX )
{
   hbqt_par_QRegExp( 1 )->setPatternSyntax( ( QRegExp::PatternSyntax ) hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
