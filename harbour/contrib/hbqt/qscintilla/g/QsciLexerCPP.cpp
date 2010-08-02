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
#include "hbqscintilla.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

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

#include <QtCore/QPointer>

#include <qscilexercpp.h>


/*
 * QsciLexerCPP (QObject *parent=0, bool caseInsensitiveKeywords=false)
 * virtual ~QsciLexerCPP ()
 *
 */

typedef struct
{
   QPointer< QsciLexerCPP > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciLexerCPP;

HBQT_GC_FUNC( hbqt_gcRelease_QsciLexerCPP )
{
   QsciLexerCPP  * ph = NULL ;
   HBQT_GC_T_QsciLexerCPP * p = ( HBQT_GC_T_QsciLexerCPP * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciLexerCPP   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciLexerCPP   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QsciLexerCPP          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QsciLexerCPP    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QsciLexerCPP    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciLexerCPP( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciLexerCPP * p = ( HBQT_GC_T_QsciLexerCPP * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciLexerCPP ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciLexerCPP >( ( QsciLexerCPP * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciLexerCPP;
   p->type = HBQT_TYPE_QsciLexerCPP;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QsciLexerCPP  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QsciLexerCPP", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSCILEXERCPP )
{
   QsciLexerCPP * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QsciLexerCPP( hbqt_par_QObject( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISLOG( 1 ) )
   {
      pObj = new QsciLexerCPP( 0, hb_parl( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISLOG( 2 ) )
   {
      pObj = new QsciLexerCPP( hbqt_par_QObject( 1 ), hb_parl( 2 ) ) ;
   }
   else
   {
      pObj = new QsciLexerCPP() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QsciLexerCPP( ( void * ) pObj, true ) );
}

/*
 * const char * language () const
 */
HB_FUNC( QT_QSCILEXERCPP_LANGUAGE )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retc( ( p )->language() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_LANGUAGE FP=hb_retc( ( p )->language() ); p is NULL" ) );
   }
}

/*
 * const char * lexer () const
 */
HB_FUNC( QT_QSCILEXERCPP_LEXER )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retc( ( p )->lexer() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_LEXER FP=hb_retc( ( p )->lexer() ); p is NULL" ) );
   }
}

/*
 * QStringList autoCompletionWordSeparators () const
 */
HB_FUNC( QT_QSCILEXERCPP_AUTOCOMPLETIONWORDSEPARATORS )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->autoCompletionWordSeparators() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_AUTOCOMPLETIONWORDSEPARATORS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->autoCompletionWordSeparators() ), true ) ); p is NULL" ) );
   }
}

/*
 * const char * blockEnd (int *style=0) const
 */
HB_FUNC( QT_QSCILEXERCPP_BLOCKEND )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockEnd( &iStyle ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_BLOCKEND FP=hb_retc( ( p )->blockEnd( &iStyle ) ); p is NULL" ) );
   }

   hb_storni( iStyle, 2 );
}

/*
 * const char * blockStart (int *style=0) const
 */
HB_FUNC( QT_QSCILEXERCPP_BLOCKSTART )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockStart( &iStyle ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_BLOCKSTART FP=hb_retc( ( p )->blockStart( &iStyle ) ); p is NULL" ) );
   }

   hb_storni( iStyle, 2 );
}

/*
 * const char * blockStartKeyword (int *style=0) const
 */
HB_FUNC( QT_QSCILEXERCPP_BLOCKSTARTKEYWORD )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   int iStyle = 0;

   if( p )
      hb_retc( ( p )->blockStartKeyword( &iStyle ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_BLOCKSTARTKEYWORD FP=hb_retc( ( p )->blockStartKeyword( &iStyle ) ); p is NULL" ) );
   }

   hb_storni( iStyle, 2 );
}

/*
 * int braceStyle () const
 */
HB_FUNC( QT_QSCILEXERCPP_BRACESTYLE )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retni( ( p )->braceStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_BRACESTYLE FP=hb_retni( ( p )->braceStyle() ); p is NULL" ) );
   }
}

/*
 * const char * wordCharacters () const
 */
HB_FUNC( QT_QSCILEXERCPP_WORDCHARACTERS )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retc( ( p )->wordCharacters() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_WORDCHARACTERS FP=hb_retc( ( p )->wordCharacters() ); p is NULL" ) );
   }
}

/*
 * QColor defaultColor (int style) const
 */
HB_FUNC( QT_QSCILEXERCPP_DEFAULTCOLOR )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultColor( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_DEFAULTCOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultColor( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool defaultEolFill (int style) const
 */
HB_FUNC( QT_QSCILEXERCPP_DEFAULTEOLFILL )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retl( ( p )->defaultEolFill( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_DEFAULTEOLFILL FP=hb_retl( ( p )->defaultEolFill( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QFont defaultFont (int style) const
 */
HB_FUNC( QT_QSCILEXERCPP_DEFAULTFONT )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_DEFAULTFONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QColor defaultPaper (int style) const
 */
HB_FUNC( QT_QSCILEXERCPP_DEFAULTPAPER )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultPaper( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_DEFAULTPAPER FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultPaper( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * const char * keywords (int set) const
 */
HB_FUNC( QT_QSCILEXERCPP_KEYWORDS )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retc( ( p )->keywords( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_KEYWORDS FP=hb_retc( ( p )->keywords( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString description (int style) const
 */
HB_FUNC( QT_QSCILEXERCPP_DESCRIPTION )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retc( ( p )->description( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_DESCRIPTION FP=hb_retc( ( p )->description( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void refreshProperties ()
 */
HB_FUNC( QT_QSCILEXERCPP_REFRESHPROPERTIES )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      ( p )->refreshProperties();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_REFRESHPROPERTIES FP=( p )->refreshProperties(); p is NULL" ) );
   }
}

/*
 * bool foldAtElse () const
 */
HB_FUNC( QT_QSCILEXERCPP_FOLDATELSE )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retl( ( p )->foldAtElse() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_FOLDATELSE FP=hb_retl( ( p )->foldAtElse() ); p is NULL" ) );
   }
}

/*
 * bool foldComments () const
 */
HB_FUNC( QT_QSCILEXERCPP_FOLDCOMMENTS )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retl( ( p )->foldComments() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_FOLDCOMMENTS FP=hb_retl( ( p )->foldComments() ); p is NULL" ) );
   }
}

/*
 * bool foldCompact () const
 */
HB_FUNC( QT_QSCILEXERCPP_FOLDCOMPACT )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retl( ( p )->foldCompact() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_FOLDCOMPACT FP=hb_retl( ( p )->foldCompact() ); p is NULL" ) );
   }
}

/*
 * bool foldPreprocessor () const
 */
HB_FUNC( QT_QSCILEXERCPP_FOLDPREPROCESSOR )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retl( ( p )->foldPreprocessor() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_FOLDPREPROCESSOR FP=hb_retl( ( p )->foldPreprocessor() ); p is NULL" ) );
   }
}

/*
 * bool stylePreprocessor () const
 */
HB_FUNC( QT_QSCILEXERCPP_STYLEPREPROCESSOR )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retl( ( p )->stylePreprocessor() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_STYLEPREPROCESSOR FP=hb_retl( ( p )->stylePreprocessor() ); p is NULL" ) );
   }
}

/*
 * void setDollarsAllowed (bool allowed)
 */
HB_FUNC( QT_QSCILEXERCPP_SETDOLLARSALLOWED )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      ( p )->setDollarsAllowed( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_SETDOLLARSALLOWED FP=( p )->setDollarsAllowed( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool dollarsAllowed () const
 */
HB_FUNC( QT_QSCILEXERCPP_DOLLARSALLOWED )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      hb_retl( ( p )->dollarsAllowed() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_DOLLARSALLOWED FP=hb_retl( ( p )->dollarsAllowed() ); p is NULL" ) );
   }
}

/*
 * virtual void setFoldAtElse (bool fold)
 */
HB_FUNC( QT_QSCILEXERCPP_SETFOLDATELSE )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      ( p )->setFoldAtElse( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_SETFOLDATELSE FP=( p )->setFoldAtElse( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setFoldComments (bool fold)
 */
HB_FUNC( QT_QSCILEXERCPP_SETFOLDCOMMENTS )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      ( p )->setFoldComments( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_SETFOLDCOMMENTS FP=( p )->setFoldComments( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setFoldCompact (bool fold)
 */
HB_FUNC( QT_QSCILEXERCPP_SETFOLDCOMPACT )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      ( p )->setFoldCompact( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_SETFOLDCOMPACT FP=( p )->setFoldCompact( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setFoldPreprocessor (bool fold)
 */
HB_FUNC( QT_QSCILEXERCPP_SETFOLDPREPROCESSOR )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      ( p )->setFoldPreprocessor( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_SETFOLDPREPROCESSOR FP=( p )->setFoldPreprocessor( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setStylePreprocessor (bool style)
 */
HB_FUNC( QT_QSCILEXERCPP_SETSTYLEPREPROCESSOR )
{
   QsciLexerCPP * p = hbqt_par_QsciLexerCPP( 1 );
   if( p )
      ( p )->setStylePreprocessor( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCILEXERCPP_SETSTYLEPREPROCESSOR FP=( p )->setStylePreprocessor( hb_parl( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
