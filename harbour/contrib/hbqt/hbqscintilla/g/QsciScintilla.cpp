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
 *  enum { AiMaintain = 0x01, AiOpening = 0x02, AiClosing = 0x04 }
 *  enum AnnotationDisplay { AnnotationHidden = ANNOTATION_HIDDEN, AnnotationStandard = ANNOTATION_STANDARD, AnnotationBoxed = ANNOTATION_BOXED }
 *  enum AutoCompletionSource {
 *    AcsNone, AcsAll, AcsDocument, AcsAPIs
 *  }
 *  enum BraceMatch { NoBraceMatch, StrictBraceMatch, SloppyBraceMatch }
 *  enum CallTipsStyle {
 *    CallTipsNone, CallTipsNoContext, CallTipsNoAutoCompletionContext, CallTipsContext
 *  }
 *  enum EdgeMode { EdgeNone = EDGE_NONE, EdgeLine = EDGE_LINE, EdgeBackground = EDGE_BACKGROUND }
 *  enum EolMode { EolWindows = SC_EOL_CRLF, EolUnix = SC_EOL_LF, EolMac = SC_EOL_CR }
 *  enum FoldStyle {
 *    NoFoldStyle, PlainFoldStyle, CircledFoldStyle,
 *    BoxedFoldStyle, CircledTreeFoldStyle, BoxedTreeFoldStyle
 *  }
 *  enum MarginType {
 *    SymbolMargin = SC_MARGIN_SYMBOL, SymbolMarginDefaultForegroundColor = SC_MARGIN_FORE, SymbolMarginDefaultBackgroundColor = SC_MARGIN_BACK,
 *    NumberMargin = SC_MARGIN_NUMBER, TextMargin = SC_MARGIN_TEXT, TextMarginRightJustified = SC_MARGIN_RTEXT
 *  }
 *  enum MarkerSymbol {
 *    Circle = SC_MARK_CIRCLE, Rectangle = SC_MARK_ROUNDRECT, RightTriangle = SC_MARK_ARROW,
 *    SmallRectangle = SC_MARK_SMALLRECT, RightArrow = SC_MARK_SHORTARROW, Invisible = SC_MARK_EMPTY,
 *    DownTriangle = SC_MARK_ARROWDOWN, Minus = SC_MARK_MINUS, Plus = SC_MARK_PLUS,
 *    VerticalLine = SC_MARK_VLINE, BottomLeftCorner = SC_MARK_LCORNER, LeftSideSplitter = SC_MARK_TCORNER,
 *    BoxedPlus = SC_MARK_BOXPLUS, BoxedPlusConnected = SC_MARK_BOXPLUSCONNECTED, BoxedMinus = SC_MARK_BOXMINUS,
 *    BoxedMinusConnected = SC_MARK_BOXMINUSCONNECTED, RoundedBottomLeftCorner = SC_MARK_LCORNERCURVE, LeftSideRoundedSplitter = SC_MARK_TCORNERCURVE,
 *    CircledPlus = SC_MARK_CIRCLEPLUS, CircledPlusConnected = SC_MARK_CIRCLEPLUSCONNECTED, CircledMinus = SC_MARK_CIRCLEMINUS,
 *    CircledMinusConnected = SC_MARK_CIRCLEMINUSCONNECTED, Background = SC_MARK_BACKGROUND, ThreeDots = SC_MARK_DOTDOTDOT,
 *    ThreeRightArrows = SC_MARK_ARROWS
 *  }
 *  enum WhitespaceVisibility { WsInvisible = SCWS_INVISIBLE, WsVisible = SCWS_VISIBLEALWAYS, WsVisibleAfterIndent = SCWS_VISIBLEAFTERINDENT }
 *  enum WrapMode { WrapNone = SC_WRAP_NONE, WrapWord = SC_WRAP_WORD, WrapCharacter = SC_WRAP_CHAR }
 *  enum WrapVisualFlag { WrapFlagNone, WrapFlagByText, WrapFlagByBorder }
 */

/*
 *  Constructed[ 195/197 [ 98.98% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void annotate (int line, const QList< QsciStyledText > &text)
 *  void setMarginText (int line, const QList< QsciStyledText > &text)
 */

#include <QtCore/QPointer>


#include "qsciscintilla.h"


/*
 * QsciScintilla (QWidget *parent=0)
 * virtual ~QsciScintilla ()
 */

typedef struct
{
   QPointer< QsciScintilla > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QsciScintilla;

QT_G_FUNC( hbqt_gcRelease_QsciScintilla )
{
   QsciScintilla  * ph = NULL ;
   QGC_POINTER_QsciScintilla * p = ( QGC_POINTER_QsciScintilla * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciScintilla   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QsciScintilla   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QsciScintilla          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QsciScintilla    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QsciScintilla    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciScintilla( void * pObj, bool bNew )
{
   QGC_POINTER_QsciScintilla * p = ( QGC_POINTER_QsciScintilla * ) hb_gcAllocate( sizeof( QGC_POINTER_QsciScintilla ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciScintilla >( ( QsciScintilla * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciScintilla;
   p->type = HBQT_TYPE_QsciScintilla;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QsciScintilla  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QsciScintilla", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSCISCINTILLA )
{
   QsciScintilla * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
   {
      pObj = new QsciScintilla( hbqt_par_QWidget( 1 ) ) ;
   }
   else
   {
      pObj = new QsciScintilla() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QsciScintilla( ( void * ) pObj, true ) );
}

/*
 * void annotate (int line, const QString &text, int style)
 */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->annotate( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ANNOTATE FP=( p )->annotate( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void annotate (int line, const QString &text, const QsciStyle &style)
 */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATE_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->annotate( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), *hbqt_par_QsciStyle( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ANNOTATE_1 FP=( p )->annotate( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), *hbqt_par_QsciStyle( 4 ) ); p is NULL" ) );
   }
}

/*
 * void annotate (int line, const QsciStyledText &text)
 */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATE_2 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->annotate( hb_parni( 2 ), *hbqt_par_QsciStyledText( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ANNOTATE_2 FP=( p )->annotate( hb_parni( 2 ), *hbqt_par_QsciStyledText( 3 ) ); p is NULL" ) );
   }
}

/*
 * QString annotation (int line) const
 */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retc( ( p )->annotation( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ANNOTATION FP=hb_retc( ( p )->annotation( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * AnnotationDisplay annotationDisplay () const
 */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATIONDISPLAY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::AnnotationDisplay ) ( p )->annotationDisplay() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ANNOTATIONDISPLAY FP=hb_retni( ( QsciScintilla::AnnotationDisplay ) ( p )->annotationDisplay() ); p is NULL" ) );
   }
}

/*
 * void clearAnnotations (int line=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_CLEARANNOTATIONS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearAnnotations( hb_parnidef( 2, -1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CLEARANNOTATIONS FP=( p )->clearAnnotations( hb_parnidef( 2, -1 ) ); p is NULL" ) );
   }
}

/*
 * bool autoCompletionCaseSensitivity () const
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONCASESENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionCaseSensitivity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETIONCASESENSITIVITY FP=hb_retl( ( p )->autoCompletionCaseSensitivity() ); p is NULL" ) );
   }
}

/*
 * bool autoCompletionFillupsEnabled () const
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONFILLUPSENABLED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionFillupsEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETIONFILLUPSENABLED FP=hb_retl( ( p )->autoCompletionFillupsEnabled() ); p is NULL" ) );
   }
}

/*
 * bool autoCompletionReplaceWord () const
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONREPLACEWORD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionReplaceWord() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETIONREPLACEWORD FP=hb_retl( ( p )->autoCompletionReplaceWord() ); p is NULL" ) );
   }
}

/*
 * bool autoCompletionShowSingle () const
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONSHOWSINGLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionShowSingle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETIONSHOWSINGLE FP=hb_retl( ( p )->autoCompletionShowSingle() ); p is NULL" ) );
   }
}

/*
 * AutoCompletionSource autoCompletionSource () const
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONSOURCE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::AutoCompletionSource ) ( p )->autoCompletionSource() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETIONSOURCE FP=hb_retni( ( QsciScintilla::AutoCompletionSource ) ( p )->autoCompletionSource() ); p is NULL" ) );
   }
}

/*
 * int autoCompletionThreshold () const
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONTHRESHOLD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->autoCompletionThreshold() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETIONTHRESHOLD FP=hb_retni( ( p )->autoCompletionThreshold() ); p is NULL" ) );
   }
}

/*
 * bool autoIndent () const
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOINDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoIndent() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOINDENT FP=hb_retl( ( p )->autoIndent() ); p is NULL" ) );
   }
}

/*
 * bool backspaceUnindents () const
 */
HB_FUNC( QT_QSCISCINTILLA_BACKSPACEUNINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->backspaceUnindents() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_BACKSPACEUNINDENTS FP=hb_retl( ( p )->backspaceUnindents() ); p is NULL" ) );
   }
}

/*
 * void beginUndoAction ()
 */
HB_FUNC( QT_QSCISCINTILLA_BEGINUNDOACTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->beginUndoAction();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_BEGINUNDOACTION FP=( p )->beginUndoAction(); p is NULL" ) );
   }
}

/*
 * BraceMatch braceMatching () const
 */
HB_FUNC( QT_QSCISCINTILLA_BRACEMATCHING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::BraceMatch ) ( p )->braceMatching() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_BRACEMATCHING FP=hb_retni( ( QsciScintilla::BraceMatch ) ( p )->braceMatching() ); p is NULL" ) );
   }
}

/*
 * CallTipsStyle callTipsStyle () const
 */
HB_FUNC( QT_QSCISCINTILLA_CALLTIPSSTYLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::CallTipsStyle ) ( p )->callTipsStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CALLTIPSSTYLE FP=hb_retni( ( QsciScintilla::CallTipsStyle ) ( p )->callTipsStyle() ); p is NULL" ) );
   }
}

/*
 * int callTipsVisible () const
 */
HB_FUNC( QT_QSCISCINTILLA_CALLTIPSVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->callTipsVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CALLTIPSVISIBLE FP=hb_retni( ( p )->callTipsVisible() ); p is NULL" ) );
   }
}

/*
 * void cancelList ()
 */
HB_FUNC( QT_QSCISCINTILLA_CANCELLIST )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->cancelList();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CANCELLIST FP=( p )->cancelList(); p is NULL" ) );
   }
}

/*
 * bool caseSensitive () const
 */
HB_FUNC( QT_QSCISCINTILLA_CASESENSITIVE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->caseSensitive() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CASESENSITIVE FP=hb_retl( ( p )->caseSensitive() ); p is NULL" ) );
   }
}

/*
 * void clearFolds ()
 */
HB_FUNC( QT_QSCISCINTILLA_CLEARFOLDS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearFolds();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CLEARFOLDS FP=( p )->clearFolds(); p is NULL" ) );
   }
}

/*
 * void clearRegisteredImages ()
 */
HB_FUNC( QT_QSCISCINTILLA_CLEARREGISTEREDIMAGES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearRegisteredImages();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CLEARREGISTEREDIMAGES FP=( p )->clearRegisteredImages(); p is NULL" ) );
   }
}

/*
 * QColor color () const
 */
HB_FUNC( QT_QSCISCINTILLA_COLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_COLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) ); p is NULL" ) );
   }
}

/*
 * void convertEols (EolMode mode)
 */
HB_FUNC( QT_QSCISCINTILLA_CONVERTEOLS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->convertEols( ( QsciScintilla::EolMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CONVERTEOLS FP=( p )->convertEols( ( QsciScintilla::EolMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QsciDocument document () const
 */
HB_FUNC( QT_QSCISCINTILLA_DOCUMENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciDocument( new QsciDocument( ( p )->document() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_DOCUMENT FP=hb_retptrGC( hbqt_gcAllocate_QsciDocument( new QsciDocument( ( p )->document() ), true ) ); p is NULL" ) );
   }
}

/*
 * void endUndoAction ()
 */
HB_FUNC( QT_QSCISCINTILLA_ENDUNDOACTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->endUndoAction();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ENDUNDOACTION FP=( p )->endUndoAction(); p is NULL" ) );
   }
}

/*
 * QColor edgeColor () const
 */
HB_FUNC( QT_QSCISCINTILLA_EDGECOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->edgeColor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_EDGECOLOR FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->edgeColor() ), true ) ); p is NULL" ) );
   }
}

/*
 * int edgeColumn () const
 */
HB_FUNC( QT_QSCISCINTILLA_EDGECOLUMN )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->edgeColumn() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_EDGECOLUMN FP=hb_retni( ( p )->edgeColumn() ); p is NULL" ) );
   }
}

/*
 * EdgeMode edgeMode () const
 */
HB_FUNC( QT_QSCISCINTILLA_EDGEMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::EdgeMode ) ( p )->edgeMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_EDGEMODE FP=hb_retni( ( QsciScintilla::EdgeMode ) ( p )->edgeMode() ); p is NULL" ) );
   }
}

/*
 * void setFont (const QFont &f)
 */
HB_FUNC( QT_QSCISCINTILLA_SETFONT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * EolMode eolMode () const
 */
HB_FUNC( QT_QSCISCINTILLA_EOLMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::EolMode ) ( p )->eolMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_EOLMODE FP=hb_retni( ( QsciScintilla::EolMode ) ( p )->eolMode() ); p is NULL" ) );
   }
}

/*
 * bool eolVisibility () const
 */
HB_FUNC( QT_QSCISCINTILLA_EOLVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->eolVisibility() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_EOLVISIBILITY FP=hb_retl( ( p )->eolVisibility() ); p is NULL" ) );
   }
}

/*
 * virtual bool findFirst (const QString &expr, bool re, bool cs, bool wo, bool wrap, bool forward=true, int line=-1, int index=-1, bool show=true)
 */
HB_FUNC( QT_QSCISCINTILLA_FINDFIRST )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->findFirst( QsciScintilla::tr( hb_parc( 2 ) ), hb_parl( 3 ), hb_parl( 4 ), hb_parl( 5 ), hb_parl( 6 ), hb_parl( 7 ), hb_parnidef( 8, -1 ), hb_parnidef( 9, -1 ), hb_parl( 10 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_FINDFIRST FP=hb_retl( ( p )->findFirst( QsciScintilla::tr( hb_parc( 2 ) ), hb_parl( 3 ), hb_parl( 4 ), hb_parl( 5 ), hb_parl( 6 ), hb_parl( 7 ), hb_parnidef( 8, -1 ), hb_parnidef( 9, -1 ), hb_parl( 10 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool findNext ()
 */
HB_FUNC( QT_QSCISCINTILLA_FINDNEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->findNext() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_FINDNEXT FP=hb_retl( ( p )->findNext() ); p is NULL" ) );
   }
}

/*
 * int firstVisibleLine () const
 */
HB_FUNC( QT_QSCISCINTILLA_FIRSTVISIBLELINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->firstVisibleLine() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_FIRSTVISIBLELINE FP=hb_retni( ( p )->firstVisibleLine() ); p is NULL" ) );
   }
}

/*
 * FoldStyle folding () const
 */
HB_FUNC( QT_QSCISCINTILLA_FOLDING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::FoldStyle ) ( p )->folding() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_FOLDING FP=hb_retni( ( QsciScintilla::FoldStyle ) ( p )->folding() ); p is NULL" ) );
   }
}

/*
 * void getCursorPosition (int *line, int *index) const
 */
HB_FUNC( QT_QSCISCINTILLA_GETCURSORPOSITION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   int iLine = 0;
   int iIndex = 0;

   if( p )
      ( p )->getCursorPosition( &iLine, &iIndex );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_GETCURSORPOSITION FP=( p )->getCursorPosition( &iLine, &iIndex ); p is NULL" ) );
   }

   hb_storni( iLine, 2 );
   hb_storni( iIndex, 3 );
}

/*
 * void getSelection (int *lineFrom, int *indexFrom, int *lineTo, int *indexTo) const
 */
HB_FUNC( QT_QSCISCINTILLA_GETSELECTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   int iLineFrom = 0;
   int iIndexFrom = 0;
   int iLineTo = 0;
   int iIndexTo = 0;

   if( p )
      ( p )->getSelection( &iLineFrom, &iIndexFrom, &iLineTo, &iIndexTo );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_GETSELECTION FP=( p )->getSelection( &iLineFrom, &iIndexFrom, &iLineTo, &iIndexTo ); p is NULL" ) );
   }

   hb_storni( iLineFrom, 2 );
   hb_storni( iIndexFrom, 3 );
   hb_storni( iLineTo, 4 );
   hb_storni( iIndexTo, 5 );
}

/*
 * bool hasSelectedText () const
 */
HB_FUNC( QT_QSCISCINTILLA_HASSELECTEDTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->hasSelectedText() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_HASSELECTEDTEXT FP=hb_retl( ( p )->hasSelectedText() ); p is NULL" ) );
   }
}

/*
 * int indentation (int line) const
 */
HB_FUNC( QT_QSCISCINTILLA_INDENTATION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->indentation( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_INDENTATION FP=hb_retni( ( p )->indentation( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool indentationGuides () const
 */
HB_FUNC( QT_QSCISCINTILLA_INDENTATIONGUIDES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->indentationGuides() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_INDENTATIONGUIDES FP=hb_retl( ( p )->indentationGuides() ); p is NULL" ) );
   }
}

/*
 * bool indentationsUseTabs () const
 */
HB_FUNC( QT_QSCISCINTILLA_INDENTATIONSUSETABS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->indentationsUseTabs() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_INDENTATIONSUSETABS FP=hb_retl( ( p )->indentationsUseTabs() ); p is NULL" ) );
   }
}

/*
 * int indentationWidth () const
 */
HB_FUNC( QT_QSCISCINTILLA_INDENTATIONWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->indentationWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_INDENTATIONWIDTH FP=hb_retni( ( p )->indentationWidth() ); p is NULL" ) );
   }
}

/*
 * bool isCallTipActive () const
 */
HB_FUNC( QT_QSCISCINTILLA_ISCALLTIPACTIVE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isCallTipActive() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISCALLTIPACTIVE FP=hb_retl( ( p )->isCallTipActive() ); p is NULL" ) );
   }
}

/*
 * bool isListActive () const
 */
HB_FUNC( QT_QSCISCINTILLA_ISLISTACTIVE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isListActive() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISLISTACTIVE FP=hb_retl( ( p )->isListActive() ); p is NULL" ) );
   }
}

/*
 * bool isModified () const
 */
HB_FUNC( QT_QSCISCINTILLA_ISMODIFIED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISMODIFIED FP=hb_retl( ( p )->isModified() ); p is NULL" ) );
   }
}

/*
 * bool isReadOnly () const
 */
HB_FUNC( QT_QSCISCINTILLA_ISREADONLY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISREADONLY FP=hb_retl( ( p )->isReadOnly() ); p is NULL" ) );
   }
}

/*
 * bool isRedoAvailable () const
 */
HB_FUNC( QT_QSCISCINTILLA_ISREDOAVAILABLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isRedoAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISREDOAVAILABLE FP=hb_retl( ( p )->isRedoAvailable() ); p is NULL" ) );
   }
}

/*
 * bool isUndoAvailable () const
 */
HB_FUNC( QT_QSCISCINTILLA_ISUNDOAVAILABLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isUndoAvailable() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISUNDOAVAILABLE FP=hb_retl( ( p )->isUndoAvailable() ); p is NULL" ) );
   }
}

/*
 * bool isUtf8 () const
 */
HB_FUNC( QT_QSCISCINTILLA_ISUTF8 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isUtf8() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISUTF8 FP=hb_retl( ( p )->isUtf8() ); p is NULL" ) );
   }
}

/*
 * bool isWordCharacter (char ch) const
 */
HB_FUNC( QT_QSCISCINTILLA_ISWORDCHARACTER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isWordCharacter( ( char ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ISWORDCHARACTER FP=hb_retl( ( p )->isWordCharacter( ( char ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int lineAt (const QPoint &pos) const
 */
HB_FUNC( QT_QSCISCINTILLA_LINEAT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->lineAt( *hbqt_par_QPoint( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_LINEAT FP=hb_retni( ( p )->lineAt( *hbqt_par_QPoint( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void lineIndexFromPosition (int position, int *line, int *index) const
 */
HB_FUNC( QT_QSCISCINTILLA_LINEINDEXFROMPOSITION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   int iLine = 0;
   int iIndex = 0;

   if( p )
      ( p )->lineIndexFromPosition( hb_parni( 2 ), &iLine, &iIndex );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_LINEINDEXFROMPOSITION FP=( p )->lineIndexFromPosition( hb_parni( 2 ), &iLine, &iIndex ); p is NULL" ) );
   }

   hb_storni( iLine, 3 );
   hb_storni( iIndex, 4 );
}

/*
 * int lineLength (int line) const
 */
HB_FUNC( QT_QSCISCINTILLA_LINELENGTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->lineLength( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_LINELENGTH FP=hb_retni( ( p )->lineLength( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int lines () const
 */
HB_FUNC( QT_QSCISCINTILLA_LINES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->lines() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_LINES FP=hb_retni( ( p )->lines() ); p is NULL" ) );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QSCISCINTILLA_LENGTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->length() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_LENGTH FP=hb_retni( ( p )->length() ); p is NULL" ) );
   }
}

/*
 * QsciLexer * lexer () const
 */
HB_FUNC( QT_QSCISCINTILLA_LEXER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciLexer( ( p )->lexer(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_LEXER FP=hb_retptrGC( hbqt_gcAllocate_QsciLexer( ( p )->lexer(), false ) ); p is NULL" ) );
   }
}

/*
 * bool marginLineNumbers (int margin) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARGINLINENUMBERS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->marginLineNumbers( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARGINLINENUMBERS FP=hb_retl( ( p )->marginLineNumbers( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int marginMarkerMask (int margin) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARGINMARKERMASK )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->marginMarkerMask( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARGINMARKERMASK FP=hb_retni( ( p )->marginMarkerMask( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool marginSensitivity (int margin) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARGINSENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->marginSensitivity( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARGINSENSITIVITY FP=hb_retl( ( p )->marginSensitivity( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * MarginType marginType (int margin) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARGINTYPE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::MarginType ) ( p )->marginType( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARGINTYPE FP=hb_retni( ( QsciScintilla::MarginType ) ( p )->marginType( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int marginWidth (int margin) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARGINWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->marginWidth( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARGINWIDTH FP=hb_retni( ( p )->marginWidth( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int markerDefine (MarkerSymbol sym, int mnr=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERDEFINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerDefine( ( QsciScintilla::MarkerSymbol ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERDEFINE FP=hb_retni( ( p )->markerDefine( ( QsciScintilla::MarkerSymbol ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * int markerDefine (char ch, int mnr=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERDEFINE_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerDefine( ( char ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERDEFINE_1 FP=hb_retni( ( p )->markerDefine( ( char ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * int markerDefine (const QPixmap &pm, int mnr=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERDEFINE_2 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerDefine( *hbqt_par_QPixmap( 2 ), hb_parnidef( 3, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERDEFINE_2 FP=hb_retni( ( p )->markerDefine( *hbqt_par_QPixmap( 2 ), hb_parnidef( 3, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * int markerAdd (int linenr, int mnr)
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERADD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerAdd( hb_parni( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERADD FP=hb_retni( ( p )->markerAdd( hb_parni( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * unsigned markersAtLine (int linenr) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERSATLINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markersAtLine( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERSATLINE FP=hb_retni( ( p )->markersAtLine( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void markerDelete (int linenr, int mnr=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERDELETE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->markerDelete( hb_parni( 2 ), hb_parnidef( 3, -1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERDELETE FP=( p )->markerDelete( hb_parni( 2 ), hb_parnidef( 3, -1 ) ); p is NULL" ) );
   }
}

/*
 * void markerDeleteAll (int mnr=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERDELETEALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->markerDeleteAll( hb_parnidef( 2, -1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERDELETEALL FP=( p )->markerDeleteAll( hb_parnidef( 2, -1 ) ); p is NULL" ) );
   }
}

/*
 * void markerDeleteHandle (int mhandle)
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERDELETEHANDLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->markerDeleteHandle( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERDELETEHANDLE FP=( p )->markerDeleteHandle( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int markerLine (int mhandle) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERLINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerLine( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERLINE FP=hb_retni( ( p )->markerLine( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int markerFindNext (int linenr, unsigned mask) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERFINDNEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerFindNext( hb_parni( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERFINDNEXT FP=hb_retni( ( p )->markerFindNext( hb_parni( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int markerFindPrevious (int linenr, unsigned mask) const
 */
HB_FUNC( QT_QSCISCINTILLA_MARKERFINDPREVIOUS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerFindPrevious( hb_parni( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MARKERFINDPREVIOUS FP=hb_retni( ( p )->markerFindPrevious( hb_parni( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QColor paper () const
 */
HB_FUNC( QT_QSCISCINTILLA_PAPER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->paper() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_PAPER FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->paper() ), true ) ); p is NULL" ) );
   }
}

/*
 * int positionFromLineIndex (int line, int index) const
 */
HB_FUNC( QT_QSCISCINTILLA_POSITIONFROMLINEINDEX )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->positionFromLineIndex( hb_parni( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_POSITIONFROMLINEINDEX FP=hb_retni( ( p )->positionFromLineIndex( hb_parni( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * bool read (QIODevice *io)
 */
HB_FUNC( QT_QSCISCINTILLA_READ )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->read( hbqt_par_QIODevice( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_READ FP=hb_retl( ( p )->read( hbqt_par_QIODevice( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void recolor (int start=0, int end=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_RECOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->recolor( hb_parni( 2 ), hb_parnidef( 3, -1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_RECOLOR FP=( p )->recolor( hb_parni( 2 ), hb_parnidef( 3, -1 ) ); p is NULL" ) );
   }
}

/*
 * void registerImage (int id, const QPixmap &pm)
 */
HB_FUNC( QT_QSCISCINTILLA_REGISTERIMAGE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->registerImage( hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_REGISTERIMAGE FP=( p )->registerImage( hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void replace (const QString &replaceStr)
 */
HB_FUNC( QT_QSCISCINTILLA_REPLACE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->replace( QsciScintilla::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_REPLACE FP=( p )->replace( QsciScintilla::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void resetFoldMarginColors ()
 */
HB_FUNC( QT_QSCISCINTILLA_RESETFOLDMARGINCOLORS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->resetFoldMarginColors();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_RESETFOLDMARGINCOLORS FP=( p )->resetFoldMarginColors(); p is NULL" ) );
   }
}

/*
 * void setFoldMarginColors (const QColor &fore, const QColor &back)
 */
HB_FUNC( QT_QSCISCINTILLA_SETFOLDMARGINCOLORS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setFoldMarginColors( *hbqt_par_QColor( 2 ), *hbqt_par_QColor( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETFOLDMARGINCOLORS FP=( p )->setFoldMarginColors( *hbqt_par_QColor( 2 ), *hbqt_par_QColor( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setAnnotationDisplay (AnnotationDisplay display)
 */
HB_FUNC( QT_QSCISCINTILLA_SETANNOTATIONDISPLAY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAnnotationDisplay( ( QsciScintilla::AnnotationDisplay ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETANNOTATIONDISPLAY FP=( p )->setAnnotationDisplay( ( QsciScintilla::AnnotationDisplay ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoCompletionFillupsEnabled (bool enabled)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONFILLUPSENABLED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionFillupsEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONFILLUPSENABLED FP=( p )->setAutoCompletionFillupsEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoCompletionFillups (const char *fillups)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONFILLUPS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionFillups( hbqt_par_char( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONFILLUPS FP=( p )->setAutoCompletionFillups( hbqt_par_char( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoCompletionWordSeparators (const QStringList &separators)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONWORDSEPARATORS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionWordSeparators( *hbqt_par_QStringList( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONWORDSEPARATORS FP=( p )->setAutoCompletionWordSeparators( *hbqt_par_QStringList( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCallTipsBackgroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCALLTIPSBACKGROUNDCOLOR FP=( p )->setCallTipsBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCallTipsForegroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsForegroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCALLTIPSFOREGROUNDCOLOR FP=( p )->setCallTipsForegroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCallTipsHighlightColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSHIGHLIGHTCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsHighlightColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCALLTIPSHIGHLIGHTCOLOR FP=( p )->setCallTipsHighlightColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCallTipsStyle (CallTipsStyle style)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSSTYLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsStyle( ( QsciScintilla::CallTipsStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCALLTIPSSTYLE FP=( p )->setCallTipsStyle( ( QsciScintilla::CallTipsStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCallTipsVisible (int nr)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsVisible( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCALLTIPSVISIBLE FP=( p )->setCallTipsVisible( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocument (const QsciDocument &document)
 */
HB_FUNC( QT_QSCISCINTILLA_SETDOCUMENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setDocument( *hbqt_par_QsciDocument( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETDOCUMENT FP=( p )->setDocument( *hbqt_par_QsciDocument( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEdgeColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETEDGECOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEdgeColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETEDGECOLOR FP=( p )->setEdgeColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEdgeColumn (int colnr)
 */
HB_FUNC( QT_QSCISCINTILLA_SETEDGECOLUMN )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEdgeColumn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETEDGECOLUMN FP=( p )->setEdgeColumn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEdgeMode (EdgeMode mode)
 */
HB_FUNC( QT_QSCISCINTILLA_SETEDGEMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEdgeMode( ( QsciScintilla::EdgeMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETEDGEMODE FP=( p )->setEdgeMode( ( QsciScintilla::EdgeMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMarginText (int line, const QString &text, int style)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginText( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINTEXT FP=( p )->setMarginText( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setMarginText (int line, const QString &text, const QsciStyle &style)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTEXT_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginText( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), *hbqt_par_QsciStyle( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINTEXT_1 FP=( p )->setMarginText( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ), *hbqt_par_QsciStyle( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setMarginText (int line, const QsciStyledText &text)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTEXT_2 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginText( hb_parni( 2 ), *hbqt_par_QsciStyledText( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINTEXT_2 FP=( p )->setMarginText( hb_parni( 2 ), *hbqt_par_QsciStyledText( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setMarginType (int margin, MarginType type)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTYPE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginType( hb_parni( 2 ), ( QsciScintilla::MarginType ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINTYPE FP=( p )->setMarginType( hb_parni( 2 ), ( QsciScintilla::MarginType ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void clearMarginText (int line=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_CLEARMARGINTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearMarginText( hb_parnidef( 2, -1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CLEARMARGINTEXT FP=( p )->clearMarginText( hb_parnidef( 2, -1 ) ); p is NULL" ) );
   }
}

/*
 * void setMarkerBackgroundColor (const QColor &col, int mnr=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARKERBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarkerBackgroundColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARKERBACKGROUNDCOLOR FP=( p )->setMarkerBackgroundColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) ); p is NULL" ) );
   }
}

/*
 * void setMarkerForegroundColor (const QColor &col, int mnr=-1)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARKERFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarkerForegroundColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARKERFOREGROUNDCOLOR FP=( p )->setMarkerForegroundColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) ); p is NULL" ) );
   }
}

/*
 * void setMatchedBraceBackgroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMATCHEDBRACEBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMatchedBraceBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMATCHEDBRACEBACKGROUNDCOLOR FP=( p )->setMatchedBraceBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMatchedBraceForegroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMATCHEDBRACEFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMatchedBraceForegroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMATCHEDBRACEFOREGROUNDCOLOR FP=( p )->setMatchedBraceForegroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUnmatchedBraceBackgroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETUNMATCHEDBRACEBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setUnmatchedBraceBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETUNMATCHEDBRACEBACKGROUNDCOLOR FP=( p )->setUnmatchedBraceBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUnmatchedBraceForegroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETUNMATCHEDBRACEFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setUnmatchedBraceForegroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETUNMATCHEDBRACEFOREGROUNDCOLOR FP=( p )->setUnmatchedBraceForegroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWrapVisualFlags (WrapVisualFlag eflag, WrapVisualFlag sflag=WrapFlagNone, int sindent=0)
 */
HB_FUNC( QT_QSCISCINTILLA_SETWRAPVISUALFLAGS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setWrapVisualFlags( ( QsciScintilla::WrapVisualFlag ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QsciScintilla::WrapVisualFlag ) hb_parni( 3 ) : ( QsciScintilla::WrapVisualFlag ) QsciScintilla::WrapFlagNone ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETWRAPVISUALFLAGS FP=( p )->setWrapVisualFlags( ( QsciScintilla::WrapVisualFlag ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QsciScintilla::WrapVisualFlag ) hb_parni( 3 ) : ( QsciScintilla::WrapVisualFlag ) QsciScintilla::WrapFlagNone ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * QString selectedText () const
 */
HB_FUNC( QT_QSCISCINTILLA_SELECTEDTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retc( ( p )->selectedText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SELECTEDTEXT FP=hb_retc( ( p )->selectedText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool selectionToEol () const
 */
HB_FUNC( QT_QSCISCINTILLA_SELECTIONTOEOL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->selectionToEol() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SELECTIONTOEOL FP=hb_retl( ( p )->selectionToEol() ); p is NULL" ) );
   }
}

/*
 * void setSelectionToEol (bool filled)
 */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTIONTOEOL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelectionToEol( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETSELECTIONTOEOL FP=( p )->setSelectionToEol( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void showUserList (int id, const QStringList &list)
 */
HB_FUNC( QT_QSCISCINTILLA_SHOWUSERLIST )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->showUserList( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SHOWUSERLIST FP=( p )->showUserList( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) ); p is NULL" ) );
   }
}

/*
 * QsciCommandSet * standardCommands () const
 */
HB_FUNC( QT_QSCISCINTILLA_STANDARDCOMMANDS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciCommandSet( ( p )->standardCommands(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_STANDARDCOMMANDS FP=hb_retptrGC( hbqt_gcAllocate_QsciCommandSet( ( p )->standardCommands(), false ) ); p is NULL" ) );
   }
}

/*
 * bool tabIndents () const
 */
HB_FUNC( QT_QSCISCINTILLA_TABINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->tabIndents() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_TABINDENTS FP=hb_retl( ( p )->tabIndents() ); p is NULL" ) );
   }
}

/*
 * int tabWidth () const
 */
HB_FUNC( QT_QSCISCINTILLA_TABWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->tabWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_TABWIDTH FP=hb_retni( ( p )->tabWidth() ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QSCISCINTILLA_TEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString text (int line) const
 */
HB_FUNC( QT_QSCISCINTILLA_TEXT_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retc( ( p )->text( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_TEXT_1 FP=hb_retc( ( p )->text( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int textHeight (int linenr) const
 */
HB_FUNC( QT_QSCISCINTILLA_TEXTHEIGHT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->textHeight( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_TEXTHEIGHT FP=hb_retni( ( p )->textHeight( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * WhitespaceVisibility whitespaceVisibility () const
 */
HB_FUNC( QT_QSCISCINTILLA_WHITESPACEVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::WhitespaceVisibility ) ( p )->whitespaceVisibility() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_WHITESPACEVISIBILITY FP=hb_retni( ( QsciScintilla::WhitespaceVisibility ) ( p )->whitespaceVisibility() ); p is NULL" ) );
   }
}

/*
 * QString wordAtPoint (const QPoint &point) const
 */
HB_FUNC( QT_QSCISCINTILLA_WORDATPOINT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retc( ( p )->wordAtPoint( *hbqt_par_QPoint( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_WORDATPOINT FP=hb_retc( ( p )->wordAtPoint( *hbqt_par_QPoint( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * const char * wordCharacters () const
 */
HB_FUNC( QT_QSCISCINTILLA_WORDCHARACTERS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retc( ( p )->wordCharacters() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_WORDCHARACTERS FP=hb_retc( ( p )->wordCharacters() ); p is NULL" ) );
   }
}

/*
 * WrapMode wrapMode () const
 */
HB_FUNC( QT_QSCISCINTILLA_WRAPMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::WrapMode ) ( p )->wrapMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_WRAPMODE FP=hb_retni( ( QsciScintilla::WrapMode ) ( p )->wrapMode() ); p is NULL" ) );
   }
}

/*
 * bool write (QIODevice *io) const
 */
HB_FUNC( QT_QSCISCINTILLA_WRITE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->write( hbqt_par_QIODevice( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_WRITE FP=hb_retl( ( p )->write( hbqt_par_QIODevice( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void append (const QString &text)
 */
HB_FUNC( QT_QSCISCINTILLA_APPEND )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->append( QsciScintilla::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_APPEND FP=( p )->append( QsciScintilla::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void autoCompleteFromAll ()
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETEFROMALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->autoCompleteFromAll();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETEFROMALL FP=( p )->autoCompleteFromAll(); p is NULL" ) );
   }
}

/*
 * virtual void autoCompleteFromAPIs ()
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETEFROMAPIS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->autoCompleteFromAPIs();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETEFROMAPIS FP=( p )->autoCompleteFromAPIs(); p is NULL" ) );
   }
}

/*
 * virtual void autoCompleteFromDocument ()
 */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETEFROMDOCUMENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->autoCompleteFromDocument();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_AUTOCOMPLETEFROMDOCUMENT FP=( p )->autoCompleteFromDocument(); p is NULL" ) );
   }
}

/*
 * virtual void callTip ()
 */
HB_FUNC( QT_QSCISCINTILLA_CALLTIP )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->callTip();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CALLTIP FP=( p )->callTip(); p is NULL" ) );
   }
}

/*
 * virtual void clear ()
 */
HB_FUNC( QT_QSCISCINTILLA_CLEAR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * virtual void copy ()
 */
HB_FUNC( QT_QSCISCINTILLA_COPY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->copy();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_COPY FP=( p )->copy(); p is NULL" ) );
   }
}

/*
 * virtual void cut ()
 */
HB_FUNC( QT_QSCISCINTILLA_CUT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->cut();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_CUT FP=( p )->cut(); p is NULL" ) );
   }
}

/*
 * virtual void ensureCursorVisible ()
 */
HB_FUNC( QT_QSCISCINTILLA_ENSURECURSORVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->ensureCursorVisible();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ENSURECURSORVISIBLE FP=( p )->ensureCursorVisible(); p is NULL" ) );
   }
}

/*
 * virtual void ensureLineVisible (int line)
 */
HB_FUNC( QT_QSCISCINTILLA_ENSURELINEVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->ensureLineVisible( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ENSURELINEVISIBLE FP=( p )->ensureLineVisible( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void foldAll (bool children=false)
 */
HB_FUNC( QT_QSCISCINTILLA_FOLDALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->foldAll( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_FOLDALL FP=( p )->foldAll( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void foldLine (int line)
 */
HB_FUNC( QT_QSCISCINTILLA_FOLDLINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->foldLine( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_FOLDLINE FP=( p )->foldLine( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void indent (int line)
 */
HB_FUNC( QT_QSCISCINTILLA_INDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->indent( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_INDENT FP=( p )->indent( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void insert (const QString &text)
 */
HB_FUNC( QT_QSCISCINTILLA_INSERT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->insert( QsciScintilla::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_INSERT FP=( p )->insert( QsciScintilla::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void insertAt (const QString &text, int line, int index)
 */
HB_FUNC( QT_QSCISCINTILLA_INSERTAT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->insertAt( QsciScintilla::tr( hb_parc( 2 ) ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_INSERTAT FP=( p )->insertAt( QsciScintilla::tr( hb_parc( 2 ) ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * virtual void moveToMatchingBrace ()
 */
HB_FUNC( QT_QSCISCINTILLA_MOVETOMATCHINGBRACE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->moveToMatchingBrace();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_MOVETOMATCHINGBRACE FP=( p )->moveToMatchingBrace(); p is NULL" ) );
   }
}

/*
 * virtual void paste ()
 */
HB_FUNC( QT_QSCISCINTILLA_PASTE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->paste();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_PASTE FP=( p )->paste(); p is NULL" ) );
   }
}

/*
 * virtual void redo ()
 */
HB_FUNC( QT_QSCISCINTILLA_REDO )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->redo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_REDO FP=( p )->redo(); p is NULL" ) );
   }
}

/*
 * virtual void removeSelectedText ()
 */
HB_FUNC( QT_QSCISCINTILLA_REMOVESELECTEDTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->removeSelectedText();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_REMOVESELECTEDTEXT FP=( p )->removeSelectedText(); p is NULL" ) );
   }
}

/*
 * virtual void resetSelectionBackgroundColor ()
 */
HB_FUNC( QT_QSCISCINTILLA_RESETSELECTIONBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->resetSelectionBackgroundColor();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_RESETSELECTIONBACKGROUNDCOLOR FP=( p )->resetSelectionBackgroundColor(); p is NULL" ) );
   }
}

/*
 * virtual void resetSelectionForegroundColor ()
 */
HB_FUNC( QT_QSCISCINTILLA_RESETSELECTIONFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->resetSelectionForegroundColor();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_RESETSELECTIONFOREGROUNDCOLOR FP=( p )->resetSelectionForegroundColor(); p is NULL" ) );
   }
}

/*
 * virtual void selectAll (bool select=true)
 */
HB_FUNC( QT_QSCISCINTILLA_SELECTALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->selectAll( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SELECTALL FP=( p )->selectAll( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void selectToMatchingBrace ()
 */
HB_FUNC( QT_QSCISCINTILLA_SELECTTOMATCHINGBRACE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->selectToMatchingBrace();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SELECTTOMATCHINGBRACE FP=( p )->selectToMatchingBrace(); p is NULL" ) );
   }
}

/*
 * virtual void setAutoCompletionCaseSensitivity (bool cs)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONCASESENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionCaseSensitivity( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONCASESENSITIVITY FP=( p )->setAutoCompletionCaseSensitivity( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setAutoCompletionReplaceWord (bool replace)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONREPLACEWORD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionReplaceWord( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONREPLACEWORD FP=( p )->setAutoCompletionReplaceWord( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setAutoCompletionShowSingle (bool single)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONSHOWSINGLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionShowSingle( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONSHOWSINGLE FP=( p )->setAutoCompletionShowSingle( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setAutoCompletionSource (AutoCompletionSource source)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONSOURCE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionSource( ( QsciScintilla::AutoCompletionSource ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONSOURCE FP=( p )->setAutoCompletionSource( ( QsciScintilla::AutoCompletionSource ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setAutoCompletionThreshold (int thresh)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONTHRESHOLD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionThreshold( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOCOMPLETIONTHRESHOLD FP=( p )->setAutoCompletionThreshold( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setAutoIndent (bool autoindent)
 */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOINDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoIndent( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETAUTOINDENT FP=( p )->setAutoIndent( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setBraceMatching (BraceMatch bm)
 */
HB_FUNC( QT_QSCISCINTILLA_SETBRACEMATCHING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setBraceMatching( ( QsciScintilla::BraceMatch ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETBRACEMATCHING FP=( p )->setBraceMatching( ( QsciScintilla::BraceMatch ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setBackspaceUnindents (bool unindent)
 */
HB_FUNC( QT_QSCISCINTILLA_SETBACKSPACEUNINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setBackspaceUnindents( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETBACKSPACEUNINDENTS FP=( p )->setBackspaceUnindents( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setCaretForegroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCARETFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretForegroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCARETFOREGROUNDCOLOR FP=( p )->setCaretForegroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setCaretLineBackgroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCARETLINEBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretLineBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCARETLINEBACKGROUNDCOLOR FP=( p )->setCaretLineBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setCaretLineVisible (bool enable)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCARETLINEVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretLineVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCARETLINEVISIBLE FP=( p )->setCaretLineVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setCaretWidth (int width)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCARETWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCARETWIDTH FP=( p )->setCaretWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setColor (const QColor &c)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCOLOR FP=( p )->setColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setCursorPosition (int line, int index)
 */
HB_FUNC( QT_QSCISCINTILLA_SETCURSORPOSITION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCursorPosition( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETCURSORPOSITION FP=( p )->setCursorPosition( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setEolMode (EolMode mode)
 */
HB_FUNC( QT_QSCISCINTILLA_SETEOLMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEolMode( ( QsciScintilla::EolMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETEOLMODE FP=( p )->setEolMode( ( QsciScintilla::EolMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setEolVisibility (bool visible)
 */
HB_FUNC( QT_QSCISCINTILLA_SETEOLVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEolVisibility( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETEOLVISIBILITY FP=( p )->setEolVisibility( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setFolding (FoldStyle fold, int margin=2)
 */
HB_FUNC( QT_QSCISCINTILLA_SETFOLDING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setFolding( ( QsciScintilla::FoldStyle ) hb_parni( 2 ), hb_parnidef( 3, 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETFOLDING FP=( p )->setFolding( ( QsciScintilla::FoldStyle ) hb_parni( 2 ), hb_parnidef( 3, 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setIndentation (int line, int indentation)
 */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentation( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETINDENTATION FP=( p )->setIndentation( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setIndentationGuides (bool enable)
 */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONGUIDES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationGuides( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETINDENTATIONGUIDES FP=( p )->setIndentationGuides( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setIndentationGuidesBackgroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONGUIDESBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationGuidesBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETINDENTATIONGUIDESBACKGROUNDCOLOR FP=( p )->setIndentationGuidesBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setIndentationGuidesForegroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONGUIDESFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationGuidesForegroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETINDENTATIONGUIDESFOREGROUNDCOLOR FP=( p )->setIndentationGuidesForegroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setIndentationsUseTabs (bool tabs)
 */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONSUSETABS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationsUseTabs( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETINDENTATIONSUSETABS FP=( p )->setIndentationsUseTabs( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setIndentationWidth (int width)
 */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETINDENTATIONWIDTH FP=( p )->setIndentationWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setLexer (QsciLexer *lexer=0)
 */
HB_FUNC( QT_QSCISCINTILLA_SETLEXER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setLexer( hbqt_par_QsciLexer( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETLEXER FP=( p )->setLexer( hbqt_par_QsciLexer( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginsBackgroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginsBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINSBACKGROUNDCOLOR FP=( p )->setMarginsBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginsFont (const QFont &f)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSFONT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginsFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINSFONT FP=( p )->setMarginsFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginsForegroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginsForegroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINSFOREGROUNDCOLOR FP=( p )->setMarginsForegroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginLineNumbers (int margin, bool lnrs)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINLINENUMBERS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginLineNumbers( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINLINENUMBERS FP=( p )->setMarginLineNumbers( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginMarkerMask (int margin, int mask)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINMARKERMASK )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginMarkerMask( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINMARKERMASK FP=( p )->setMarginMarkerMask( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginSensitivity (int margin, bool sens)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginSensitivity( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINSENSITIVITY FP=( p )->setMarginSensitivity( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginWidth (int margin, int width)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginWidth( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINWIDTH FP=( p )->setMarginWidth( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setMarginWidth (int margin, const QString &s)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINWIDTH_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginWidth( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMARGINWIDTH_1 FP=( p )->setMarginWidth( hb_parni( 2 ), QsciScintilla::tr( hb_parc( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setModified (bool m)
 */
HB_FUNC( QT_QSCISCINTILLA_SETMODIFIED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setModified( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETMODIFIED FP=( p )->setModified( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setPaper (const QColor &c)
 */
HB_FUNC( QT_QSCISCINTILLA_SETPAPER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setPaper( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETPAPER FP=( p )->setPaper( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setReadOnly (bool ro)
 */
HB_FUNC( QT_QSCISCINTILLA_SETREADONLY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETREADONLY FP=( p )->setReadOnly( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setSelection (int lineFrom, int indexFrom, int lineTo, int indexTo)
 */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelection( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETSELECTION FP=( p )->setSelection( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setSelectionBackgroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTIONBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelectionBackgroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETSELECTIONBACKGROUNDCOLOR FP=( p )->setSelectionBackgroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setSelectionForegroundColor (const QColor &col)
 */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTIONFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelectionForegroundColor( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETSELECTIONFOREGROUNDCOLOR FP=( p )->setSelectionForegroundColor( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setTabIndents (bool indent)
 */
HB_FUNC( QT_QSCISCINTILLA_SETTABINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setTabIndents( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETTABINDENTS FP=( p )->setTabIndents( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setTabWidth (int width)
 */
HB_FUNC( QT_QSCISCINTILLA_SETTABWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setTabWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETTABWIDTH FP=( p )->setTabWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setText (const QString &text)
 */
HB_FUNC( QT_QSCISCINTILLA_SETTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setText( QsciScintilla::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETTEXT FP=( p )->setText( QsciScintilla::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setUtf8 (bool cp)
 */
HB_FUNC( QT_QSCISCINTILLA_SETUTF8 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setUtf8( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETUTF8 FP=( p )->setUtf8( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setWhitespaceVisibility (WhitespaceVisibility mode)
 */
HB_FUNC( QT_QSCISCINTILLA_SETWHITESPACEVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setWhitespaceVisibility( ( QsciScintilla::WhitespaceVisibility ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETWHITESPACEVISIBILITY FP=( p )->setWhitespaceVisibility( ( QsciScintilla::WhitespaceVisibility ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void setWrapMode (WrapMode mode)
 */
HB_FUNC( QT_QSCISCINTILLA_SETWRAPMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setWrapMode( ( QsciScintilla::WrapMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_SETWRAPMODE FP=( p )->setWrapMode( ( QsciScintilla::WrapMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void undo ()
 */
HB_FUNC( QT_QSCISCINTILLA_UNDO )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->undo();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_UNDO FP=( p )->undo(); p is NULL" ) );
   }
}

/*
 * virtual void unindent (int line)
 */
HB_FUNC( QT_QSCISCINTILLA_UNINDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->unindent( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_UNINDENT FP=( p )->unindent( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void zoomIn (int range)
 */
HB_FUNC( QT_QSCISCINTILLA_ZOOMIN )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomIn( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ZOOMIN FP=( p )->zoomIn( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void zoomIn ()
 */
HB_FUNC( QT_QSCISCINTILLA_ZOOMIN_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomIn();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ZOOMIN_1 FP=( p )->zoomIn(); p is NULL" ) );
   }
}

/*
 * virtual void zoomOut (int range)
 */
HB_FUNC( QT_QSCISCINTILLA_ZOOMOUT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomOut( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ZOOMOUT FP=( p )->zoomOut( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual void zoomOut ()
 */
HB_FUNC( QT_QSCISCINTILLA_ZOOMOUT_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomOut();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ZOOMOUT_1 FP=( p )->zoomOut(); p is NULL" ) );
   }
}

/*
 * virtual void zoomTo (int size)
 */
HB_FUNC( QT_QSCISCINTILLA_ZOOMTO )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomTo( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSCISCINTILLA_ZOOMTO FP=( p )->zoomTo( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
