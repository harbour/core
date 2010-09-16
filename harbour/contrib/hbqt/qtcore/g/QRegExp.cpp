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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QRegExp * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRegExp;
   p->type = HBQT_TYPE_QRegExp;

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
   {
      hb_retstr_utf8( ( p )->cap( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QStringList capturedTexts () const
 */
HB_FUNC( QT_QREGEXP_CAPTUREDTEXTS )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->capturedTexts() ), true ) );
   }
}

/*
 * Qt::CaseSensitivity caseSensitivity () const
 */
HB_FUNC( QT_QREGEXP_CASESENSITIVITY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retni( ( Qt::CaseSensitivity ) ( p )->caseSensitivity() );
   }
}

/*
 * QString errorString () const
 */
HB_FUNC( QT_QREGEXP_ERRORSTRING )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->errorString().toUtf8().data() );
   }
}

/*
 * bool exactMatch ( const QString & str ) const
 */
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

/*
 * int indexIn ( const QString & str, int offset = 0, CaretMode caretMode = CaretAtZero ) const
 */
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

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QREGEXP_ISEMPTY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isMinimal () const
 */
HB_FUNC( QT_QREGEXP_ISMINIMAL )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retl( ( p )->isMinimal() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QREGEXP_ISVALID )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * int lastIndexIn ( const QString & str, int offset = -1, CaretMode caretMode = CaretAtZero ) const
 */
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

/*
 * int matchedLength () const
 */
HB_FUNC( QT_QREGEXP_MATCHEDLENGTH )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retni( ( p )->matchedLength() );
   }
}

/*
 * int numCaptures () const
 */
HB_FUNC( QT_QREGEXP_NUMCAPTURES )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retni( ( p )->numCaptures() );
   }
}

/*
 * QString pattern () const
 */
HB_FUNC( QT_QREGEXP_PATTERN )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->pattern().toUtf8().data() );
   }
}

/*
 * PatternSyntax patternSyntax () const
 */
HB_FUNC( QT_QREGEXP_PATTERNSYNTAX )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retni( ( QRegExp::PatternSyntax ) ( p )->patternSyntax() );
   }
}

/*
 * int pos ( int nth = 0 ) const
 */
HB_FUNC( QT_QREGEXP_POS )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      hb_retni( ( p )->pos( hb_parni( 2 ) ) );
   }
}

/*
 * void setCaseSensitivity ( Qt::CaseSensitivity cs )
 */
HB_FUNC( QT_QREGEXP_SETCASESENSITIVITY )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      ( p )->setCaseSensitivity( ( Qt::CaseSensitivity ) hb_parni( 2 ) );
   }
}

/*
 * void setMinimal ( bool minimal )
 */
HB_FUNC( QT_QREGEXP_SETMINIMAL )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      ( p )->setMinimal( hb_parl( 2 ) );
   }
}

/*
 * void setPattern ( const QString & pattern )
 */
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

/*
 * void setPatternSyntax ( PatternSyntax syntax )
 */
HB_FUNC( QT_QREGEXP_SETPATTERNSYNTAX )
{
   QRegExp * p = hbqt_par_QRegExp( 1 );
   if( p )
   {
      ( p )->setPatternSyntax( ( QRegExp::PatternSyntax ) hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
