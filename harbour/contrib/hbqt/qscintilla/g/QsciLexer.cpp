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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqscintilla.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

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
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciLexer( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciLexer * p = ( HBQT_GC_T_QsciLexer * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciLexer ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciLexer >( ( QsciLexer * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciLexer;
   p->type = HBQT_TYPE_QsciLexer;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QsciLexer  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QsciLexer", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSCILEXER )
{

}

/*
 * virtual const char * language () const =0
 */
HB_FUNC( QT_QSCILEXER_LANGUAGE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retc( ( p )->language() );
   }
}

/*
 * virtual const char * lexer () const
 */
HB_FUNC( QT_QSCILEXER_LEXER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retc( ( p )->lexer() );
   }
}

/*
 * virtual int lexerId () const
 */
HB_FUNC( QT_QSCILEXER_LEXERID )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retni( ( p )->lexerId() );
   }
}

/*
 * QsciAbstractAPIs * apis () const
 */
HB_FUNC( QT_QSCILEXER_APIS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QsciAbstractAPIs( ( p )->apis(), false ) );
   }
}

/*
 * virtual const char * autoCompletionFillups () const
 */
HB_FUNC( QT_QSCILEXER_AUTOCOMPLETIONFILLUPS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retc( ( p )->autoCompletionFillups() );
   }
}

/*
 * virtual QStringList autoCompletionWordSeparators () const
 */
HB_FUNC( QT_QSCILEXER_AUTOCOMPLETIONWORDSEPARATORS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->autoCompletionWordSeparators() ), true ) );
   }
}

/*
 * int autoIndentStyle ()
 */
HB_FUNC( QT_QSCILEXER_AUTOINDENTSTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retni( ( p )->autoIndentStyle() );
   }
}

/*
 * virtual const char * blockEnd (int *style=0) const
 */
HB_FUNC( QT_QSCILEXER_BLOCKEND )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   int iStyle = 0;

   if( p )
   {
      hb_retc( ( p )->blockEnd( &iStyle ) );
   }

   hb_storni( iStyle, 2 );
}

/*
 * virtual int blockLookback () const
 */
HB_FUNC( QT_QSCILEXER_BLOCKLOOKBACK )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retni( ( p )->blockLookback() );
   }
}

/*
 * virtual const char * blockStart (int *style=0) const
 */
HB_FUNC( QT_QSCILEXER_BLOCKSTART )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   int iStyle = 0;

   if( p )
   {
      hb_retc( ( p )->blockStart( &iStyle ) );
   }

   hb_storni( iStyle, 2 );
}

/*
 * virtual const char * blockStartKeyword (int *style=0) const
 */
HB_FUNC( QT_QSCILEXER_BLOCKSTARTKEYWORD )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   int iStyle = 0;

   if( p )
   {
      hb_retc( ( p )->blockStartKeyword( &iStyle ) );
   }

   hb_storni( iStyle, 2 );
}

/*
 * virtual int braceStyle () const
 */
HB_FUNC( QT_QSCILEXER_BRACESTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retni( ( p )->braceStyle() );
   }
}

/*
 * virtual bool caseSensitive () const
 */
HB_FUNC( QT_QSCILEXER_CASESENSITIVE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retl( ( p )->caseSensitive() );
   }
}

/*
 * virtual QColor color (int style) const
 */
HB_FUNC( QT_QSCILEXER_COLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * virtual bool eolFill (int style) const
 */
HB_FUNC( QT_QSCILEXER_EOLFILL )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retl( ( p )->eolFill( hb_parni( 2 ) ) );
   }
}

/*
 * virtual QFont font (int style) const
 */
HB_FUNC( QT_QSCILEXER_FONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * virtual int indentationGuideView () const
 */
HB_FUNC( QT_QSCILEXER_INDENTATIONGUIDEVIEW )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retni( ( p )->indentationGuideView() );
   }
}

/*
 * virtual const char * keywords (int set) const
 */
HB_FUNC( QT_QSCILEXER_KEYWORDS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retc( ( p )->keywords( hb_parni( 2 ) ) );
   }
}

/*
 * virtual int defaultStyle () const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTSTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retni( ( p )->defaultStyle() );
   }
}

/*
 * virtual QString description (int style) const =0
 */
HB_FUNC( QT_QSCILEXER_DESCRIPTION )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->description( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * virtual QColor paper (int style) const
 */
HB_FUNC( QT_QSCILEXER_PAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->paper( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QColor defaultColor () const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTCOLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultColor() ), true ) );
   }
}

/*
 * virtual QColor defaultColor (int style) const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTCOLOR_1 )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultColor( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * virtual bool defaultEolFill (int style) const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTEOLFILL )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retl( ( p )->defaultEolFill( hb_parni( 2 ) ) );
   }
}

/*
 * QFont defaultFont () const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTFONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont() ), true ) );
   }
}

/*
 * virtual QFont defaultFont (int style) const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTFONT_1 )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->defaultFont( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QColor defaultPaper () const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTPAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultPaper() ), true ) );
   }
}

/*
 * virtual QColor defaultPaper (int style) const
 */
HB_FUNC( QT_QSCILEXER_DEFAULTPAPER_1 )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->defaultPaper( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QsciScintilla * editor () const
 */
HB_FUNC( QT_QSCILEXER_EDITOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QsciScintilla( ( p )->editor(), false ) );
   }
}

/*
 * virtual void setEditor (QsciScintilla *editor)
 */
HB_FUNC( QT_QSCILEXER_SETEDITOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setEditor( hbqt_par_QsciScintilla( 2 ) );
   }
}

/*
 * bool readSettings (QSettings &qs, const char *prefix="/Scintilla")
 */
HB_FUNC( QT_QSCILEXER_READSETTINGS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retl( ( p )->readSettings( *hbqt_par_QSettings( 2 ), hbqt_par_char( 3 ) ) );
   }
}

/*
 * virtual void refreshProperties ()
 */
HB_FUNC( QT_QSCILEXER_REFRESHPROPERTIES )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->refreshProperties();
   }
}

/*
 * virtual int styleBitsNeeded () const
 */
HB_FUNC( QT_QSCILEXER_STYLEBITSNEEDED )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retni( ( p )->styleBitsNeeded() );
   }
}

/*
 * virtual const char * wordCharacters () const
 */
HB_FUNC( QT_QSCILEXER_WORDCHARACTERS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retc( ( p )->wordCharacters() );
   }
}

/*
 * bool writeSettings (QSettings &qs, const char *prefix="/Scintilla") const
 */
HB_FUNC( QT_QSCILEXER_WRITESETTINGS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      hb_retl( ( p )->writeSettings( *hbqt_par_QSettings( 2 ), hbqt_par_char( 3 ) ) );
   }
}

/*
 * void setAPIs (QsciAbstractAPIs *apis)
 */
HB_FUNC( QT_QSCILEXER_SETAPIS )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setAPIs( hbqt_par_QsciAbstractAPIs( 2 ) );
   }
}

/*
 * void setDefaultColor (const QColor &c)
 */
HB_FUNC( QT_QSCILEXER_SETDEFAULTCOLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setDefaultColor( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * void setDefaultFont (const QFont &f)
 */
HB_FUNC( QT_QSCILEXER_SETDEFAULTFONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setDefaultFont( *hbqt_par_QFont( 2 ) );
   }
}

/*
 * void setDefaultPaper (const QColor &c)
 */
HB_FUNC( QT_QSCILEXER_SETDEFAULTPAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setDefaultPaper( *hbqt_par_QColor( 2 ) );
   }
}

/*
 * virtual void setAutoIndentStyle (int autoindentstyle)
 */
HB_FUNC( QT_QSCILEXER_SETAUTOINDENTSTYLE )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setAutoIndentStyle( hb_parni( 2 ) );
   }
}

/*
 * virtual void setColor (const QColor &c, int style=-1)
 */
HB_FUNC( QT_QSCILEXER_SETCOLOR )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
   }
}

/*
 * virtual void setEolFill (bool eoffill, int style=-1)
 */
HB_FUNC( QT_QSCILEXER_SETEOLFILL )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setEolFill( hb_parl( 2 ), hb_parnidef( 3, -1 ) );
   }
}

/*
 * virtual void setFont (const QFont &f, int style=-1)
 */
HB_FUNC( QT_QSCILEXER_SETFONT )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setFont( *hbqt_par_QFont( 2 ), hb_parnidef( 3, -1 ) );
   }
}

/*
 * virtual void setPaper (const QColor &c, int style=-1)
 */
HB_FUNC( QT_QSCILEXER_SETPAPER )
{
   QsciLexer * p = hbqt_par_QsciLexer( 1 );
   if( p )
   {
      ( p )->setPaper( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
