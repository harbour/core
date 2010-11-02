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
#include "hbqscintilla.h"

#if QT_VERSION >= 0x040500

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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QsciScintilla;

HBQT_GC_FUNC( hbqt_gcRelease_QsciScintilla )
{
   HBQT_GC_T_QsciScintilla * p = ( HBQT_GC_T_QsciScintilla * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QsciScintilla * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QsciScintilla( void * pObj, bool bNew )
{
   HBQT_GC_T_QsciScintilla * p = ( HBQT_GC_T_QsciScintilla * ) hb_gcAllocate( sizeof( HBQT_GC_T_QsciScintilla ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QsciScintilla >( ( QsciScintilla * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QsciScintilla;
   p->type = HBQT_TYPE_QsciScintilla;

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

/* void annotate (int line, const QString &text, int style) */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->annotate( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parni( 4 ) );
      hb_strfree( pText );
   }
}

/* void annotate (int line, const QString &text, const QsciStyle &style) */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATE_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->annotate( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), *hbqt_par_QsciStyle( 4 ) );
      hb_strfree( pText );
   }
}

/* void annotate (int line, const QsciStyledText &text) */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATE_2 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->annotate( hb_parni( 2 ), *hbqt_par_QsciStyledText( 3 ) );
}

/* QString annotation (int line) const */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retstr_utf8( ( p )->annotation( hb_parni( 2 ) ).toUtf8().data() );
}

/* AnnotationDisplay annotationDisplay () const */
HB_FUNC( QT_QSCISCINTILLA_ANNOTATIONDISPLAY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::AnnotationDisplay ) ( p )->annotationDisplay() );
}

/* void clearAnnotations (int line=-1) */
HB_FUNC( QT_QSCISCINTILLA_CLEARANNOTATIONS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearAnnotations( hb_parnidef( 2, -1 ) );
}

/* bool autoCompletionCaseSensitivity () const */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONCASESENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionCaseSensitivity() );
}

/* bool autoCompletionFillupsEnabled () const */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONFILLUPSENABLED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionFillupsEnabled() );
}

/* bool autoCompletionReplaceWord () const */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONREPLACEWORD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionReplaceWord() );
}

/* bool autoCompletionShowSingle () const */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONSHOWSINGLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoCompletionShowSingle() );
}

/* AutoCompletionSource autoCompletionSource () const */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONSOURCE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::AutoCompletionSource ) ( p )->autoCompletionSource() );
}

/* int autoCompletionThreshold () const */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETIONTHRESHOLD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->autoCompletionThreshold() );
}

/* bool autoIndent () const */
HB_FUNC( QT_QSCISCINTILLA_AUTOINDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->autoIndent() );
}

/* bool backspaceUnindents () const */
HB_FUNC( QT_QSCISCINTILLA_BACKSPACEUNINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->backspaceUnindents() );
}

/* void beginUndoAction () */
HB_FUNC( QT_QSCISCINTILLA_BEGINUNDOACTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->beginUndoAction();
}

/* BraceMatch braceMatching () const */
HB_FUNC( QT_QSCISCINTILLA_BRACEMATCHING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::BraceMatch ) ( p )->braceMatching() );
}

/* CallTipsStyle callTipsStyle () const */
HB_FUNC( QT_QSCISCINTILLA_CALLTIPSSTYLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::CallTipsStyle ) ( p )->callTipsStyle() );
}

/* int callTipsVisible () const */
HB_FUNC( QT_QSCISCINTILLA_CALLTIPSVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->callTipsVisible() );
}

/* void cancelList () */
HB_FUNC( QT_QSCISCINTILLA_CANCELLIST )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->cancelList();
}

/* bool caseSensitive () const */
HB_FUNC( QT_QSCISCINTILLA_CASESENSITIVE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->caseSensitive() );
}

/* void clearFolds () */
HB_FUNC( QT_QSCISCINTILLA_CLEARFOLDS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearFolds();
}

/* void clearRegisteredImages () */
HB_FUNC( QT_QSCISCINTILLA_CLEARREGISTEREDIMAGES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearRegisteredImages();
}

/* QColor color () const */
HB_FUNC( QT_QSCISCINTILLA_COLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) );
}

/* void convertEols (EolMode mode) */
HB_FUNC( QT_QSCISCINTILLA_CONVERTEOLS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->convertEols( ( QsciScintilla::EolMode ) hb_parni( 2 ) );
}

/* QsciDocument document () const */
HB_FUNC( QT_QSCISCINTILLA_DOCUMENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciDocument( new QsciDocument( ( p )->document() ), true ) );
}

/* void endUndoAction () */
HB_FUNC( QT_QSCISCINTILLA_ENDUNDOACTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->endUndoAction();
}

/* QColor edgeColor () const */
HB_FUNC( QT_QSCISCINTILLA_EDGECOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->edgeColor() ), true ) );
}

/* int edgeColumn () const */
HB_FUNC( QT_QSCISCINTILLA_EDGECOLUMN )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->edgeColumn() );
}

/* EdgeMode edgeMode () const */
HB_FUNC( QT_QSCISCINTILLA_EDGEMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::EdgeMode ) ( p )->edgeMode() );
}

/* void setFont (const QFont &f) */
HB_FUNC( QT_QSCISCINTILLA_SETFONT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* EolMode eolMode () const */
HB_FUNC( QT_QSCISCINTILLA_EOLMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::EolMode ) ( p )->eolMode() );
}

/* bool eolVisibility () const */
HB_FUNC( QT_QSCISCINTILLA_EOLVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->eolVisibility() );
}

/* virtual bool findFirst (const QString &expr, bool re, bool cs, bool wo, bool wrap, bool forward=true, int line=-1, int index=-1, bool show=true) */
HB_FUNC( QT_QSCISCINTILLA_FINDFIRST )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->findFirst( hb_parstr_utf8( 2, &pText, NULL ), hb_parl( 3 ), hb_parl( 4 ), hb_parl( 5 ), hb_parl( 6 ), hb_parl( 7 ), hb_parnidef( 8, -1 ), hb_parnidef( 9, -1 ), hb_parl( 10 ) ) );
      hb_strfree( pText );
   }
}

/* virtual bool findNext () */
HB_FUNC( QT_QSCISCINTILLA_FINDNEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->findNext() );
}

/* int firstVisibleLine () const */
HB_FUNC( QT_QSCISCINTILLA_FIRSTVISIBLELINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->firstVisibleLine() );
}

/* FoldStyle folding () const */
HB_FUNC( QT_QSCISCINTILLA_FOLDING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::FoldStyle ) ( p )->folding() );
}

/* void getCursorPosition (int *line, int *index) const */
HB_FUNC( QT_QSCISCINTILLA_GETCURSORPOSITION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   int iLine = 0;
   int iIndex = 0;

   if( p )
      ( p )->getCursorPosition( &iLine, &iIndex );

   hb_storni( iLine, 2 );
   hb_storni( iIndex, 3 );
}

/* void getSelection (int *lineFrom, int *indexFrom, int *lineTo, int *indexTo) const */
HB_FUNC( QT_QSCISCINTILLA_GETSELECTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   int iLineFrom = 0;
   int iIndexFrom = 0;
   int iLineTo = 0;
   int iIndexTo = 0;

   if( p )
      ( p )->getSelection( &iLineFrom, &iIndexFrom, &iLineTo, &iIndexTo );

   hb_storni( iLineFrom, 2 );
   hb_storni( iIndexFrom, 3 );
   hb_storni( iLineTo, 4 );
   hb_storni( iIndexTo, 5 );
}

/* bool hasSelectedText () const */
HB_FUNC( QT_QSCISCINTILLA_HASSELECTEDTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->hasSelectedText() );
}

/* int indentation (int line) const */
HB_FUNC( QT_QSCISCINTILLA_INDENTATION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->indentation( hb_parni( 2 ) ) );
}

/* bool indentationGuides () const */
HB_FUNC( QT_QSCISCINTILLA_INDENTATIONGUIDES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->indentationGuides() );
}

/* bool indentationsUseTabs () const */
HB_FUNC( QT_QSCISCINTILLA_INDENTATIONSUSETABS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->indentationsUseTabs() );
}

/* int indentationWidth () const */
HB_FUNC( QT_QSCISCINTILLA_INDENTATIONWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->indentationWidth() );
}

/* bool isCallTipActive () const */
HB_FUNC( QT_QSCISCINTILLA_ISCALLTIPACTIVE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isCallTipActive() );
}

/* bool isListActive () const */
HB_FUNC( QT_QSCISCINTILLA_ISLISTACTIVE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isListActive() );
}

/* bool isModified () const */
HB_FUNC( QT_QSCISCINTILLA_ISMODIFIED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isModified() );
}

/* bool isReadOnly () const */
HB_FUNC( QT_QSCISCINTILLA_ISREADONLY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isReadOnly() );
}

/* bool isRedoAvailable () const */
HB_FUNC( QT_QSCISCINTILLA_ISREDOAVAILABLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isRedoAvailable() );
}

/* bool isUndoAvailable () const */
HB_FUNC( QT_QSCISCINTILLA_ISUNDOAVAILABLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isUndoAvailable() );
}

/* bool isUtf8 () const */
HB_FUNC( QT_QSCISCINTILLA_ISUTF8 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isUtf8() );
}

/* bool isWordCharacter (char ch) const */
HB_FUNC( QT_QSCISCINTILLA_ISWORDCHARACTER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->isWordCharacter( ( char ) hb_parni( 2 ) ) );
}

/* int lineAt (const QPoint &pos) const */
HB_FUNC( QT_QSCISCINTILLA_LINEAT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->lineAt( *hbqt_par_QPoint( 2 ) ) );
}

/* void lineIndexFromPosition (int position, int *line, int *index) const */
HB_FUNC( QT_QSCISCINTILLA_LINEINDEXFROMPOSITION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   int iLine = 0;
   int iIndex = 0;

   if( p )
      ( p )->lineIndexFromPosition( hb_parni( 2 ), &iLine, &iIndex );

   hb_storni( iLine, 3 );
   hb_storni( iIndex, 4 );
}

/* int lineLength (int line) const */
HB_FUNC( QT_QSCISCINTILLA_LINELENGTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->lineLength( hb_parni( 2 ) ) );
}

/* int lines () const */
HB_FUNC( QT_QSCISCINTILLA_LINES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->lines() );
}

/* int length () const */
HB_FUNC( QT_QSCISCINTILLA_LENGTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->length() );
}

/* QsciLexer * lexer () const */
HB_FUNC( QT_QSCISCINTILLA_LEXER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciLexer( ( p )->lexer(), false ) );
}

/* bool marginLineNumbers (int margin) const */
HB_FUNC( QT_QSCISCINTILLA_MARGINLINENUMBERS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->marginLineNumbers( hb_parni( 2 ) ) );
}

/* int marginMarkerMask (int margin) const */
HB_FUNC( QT_QSCISCINTILLA_MARGINMARKERMASK )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->marginMarkerMask( hb_parni( 2 ) ) );
}

/* bool marginSensitivity (int margin) const */
HB_FUNC( QT_QSCISCINTILLA_MARGINSENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->marginSensitivity( hb_parni( 2 ) ) );
}

/* MarginType marginType (int margin) const */
HB_FUNC( QT_QSCISCINTILLA_MARGINTYPE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::MarginType ) ( p )->marginType( hb_parni( 2 ) ) );
}

/* int marginWidth (int margin) const */
HB_FUNC( QT_QSCISCINTILLA_MARGINWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->marginWidth( hb_parni( 2 ) ) );
}

/* int markerDefine (MarkerSymbol sym, int mnr=-1) */
HB_FUNC( QT_QSCISCINTILLA_MARKERDEFINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerDefine( ( QsciScintilla::MarkerSymbol ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) );
}

/* int markerDefine (char ch, int mnr=-1) */
HB_FUNC( QT_QSCISCINTILLA_MARKERDEFINE_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerDefine( ( char ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) );
}

/* int markerDefine (const QPixmap &pm, int mnr=-1) */
HB_FUNC( QT_QSCISCINTILLA_MARKERDEFINE_2 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerDefine( *hbqt_par_QPixmap( 2 ), hb_parnidef( 3, -1 ) ) );
}

/* int markerAdd (int linenr, int mnr) */
HB_FUNC( QT_QSCISCINTILLA_MARKERADD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerAdd( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* unsigned markersAtLine (int linenr) const */
HB_FUNC( QT_QSCISCINTILLA_MARKERSATLINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markersAtLine( hb_parni( 2 ) ) );
}

/* void markerDelete (int linenr, int mnr=-1) */
HB_FUNC( QT_QSCISCINTILLA_MARKERDELETE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->markerDelete( hb_parni( 2 ), hb_parnidef( 3, -1 ) );
}

/* void markerDeleteAll (int mnr=-1) */
HB_FUNC( QT_QSCISCINTILLA_MARKERDELETEALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->markerDeleteAll( hb_parnidef( 2, -1 ) );
}

/* void markerDeleteHandle (int mhandle) */
HB_FUNC( QT_QSCISCINTILLA_MARKERDELETEHANDLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->markerDeleteHandle( hb_parni( 2 ) );
}

/* int markerLine (int mhandle) const */
HB_FUNC( QT_QSCISCINTILLA_MARKERLINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerLine( hb_parni( 2 ) ) );
}

/* int markerFindNext (int linenr, unsigned mask) const */
HB_FUNC( QT_QSCISCINTILLA_MARKERFINDNEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerFindNext( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* int markerFindPrevious (int linenr, unsigned mask) const */
HB_FUNC( QT_QSCISCINTILLA_MARKERFINDPREVIOUS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->markerFindPrevious( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* QColor paper () const */
HB_FUNC( QT_QSCISCINTILLA_PAPER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->paper() ), true ) );
}

/* int positionFromLineIndex (int line, int index) const */
HB_FUNC( QT_QSCISCINTILLA_POSITIONFROMLINEINDEX )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->positionFromLineIndex( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/* bool read (QIODevice *io) */
HB_FUNC( QT_QSCISCINTILLA_READ )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->read( hbqt_par_QIODevice( 2 ) ) );
}

/* virtual void recolor (int start=0, int end=-1) */
HB_FUNC( QT_QSCISCINTILLA_RECOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->recolor( hb_parni( 2 ), hb_parnidef( 3, -1 ) );
}

/* void registerImage (int id, const QPixmap &pm) */
HB_FUNC( QT_QSCISCINTILLA_REGISTERIMAGE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->registerImage( hb_parni( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/* virtual void replace (const QString &replaceStr) */
HB_FUNC( QT_QSCISCINTILLA_REPLACE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->replace( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void resetFoldMarginColors () */
HB_FUNC( QT_QSCISCINTILLA_RESETFOLDMARGINCOLORS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->resetFoldMarginColors();
}

/* void setFoldMarginColors (const QColor &fore, const QColor &back) */
HB_FUNC( QT_QSCISCINTILLA_SETFOLDMARGINCOLORS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setFoldMarginColors( *hbqt_par_QColor( 2 ), *hbqt_par_QColor( 3 ) );
}

/* void setAnnotationDisplay (AnnotationDisplay display) */
HB_FUNC( QT_QSCISCINTILLA_SETANNOTATIONDISPLAY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAnnotationDisplay( ( QsciScintilla::AnnotationDisplay ) hb_parni( 2 ) );
}

/* void setAutoCompletionFillupsEnabled (bool enabled) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONFILLUPSENABLED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionFillupsEnabled( hb_parl( 2 ) );
}

/* void setAutoCompletionFillups (const char *fillups) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONFILLUPS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionFillups( ( const char * ) hb_parc( 2 ) );
}

/* void setAutoCompletionWordSeparators (const QStringList &separators) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONWORDSEPARATORS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionWordSeparators( *hbqt_par_QStringList( 2 ) );
}

/* void setCallTipsBackgroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* void setCallTipsForegroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsForegroundColor( *hbqt_par_QColor( 2 ) );
}

/* void setCallTipsHighlightColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSHIGHLIGHTCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsHighlightColor( *hbqt_par_QColor( 2 ) );
}

/* void setCallTipsStyle (CallTipsStyle style) */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSSTYLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsStyle( ( QsciScintilla::CallTipsStyle ) hb_parni( 2 ) );
}

/* void setCallTipsVisible (int nr) */
HB_FUNC( QT_QSCISCINTILLA_SETCALLTIPSVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCallTipsVisible( hb_parni( 2 ) );
}

/* void setDocument (const QsciDocument &document) */
HB_FUNC( QT_QSCISCINTILLA_SETDOCUMENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setDocument( *hbqt_par_QsciDocument( 2 ) );
}

/* void setEdgeColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETEDGECOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEdgeColor( *hbqt_par_QColor( 2 ) );
}

/* void setEdgeColumn (int colnr) */
HB_FUNC( QT_QSCISCINTILLA_SETEDGECOLUMN )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEdgeColumn( hb_parni( 2 ) );
}

/* void setEdgeMode (EdgeMode mode) */
HB_FUNC( QT_QSCISCINTILLA_SETEDGEMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEdgeMode( ( QsciScintilla::EdgeMode ) hb_parni( 2 ) );
}

/* void setMarginText (int line, const QString &text, int style) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->setMarginText( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parni( 4 ) );
      hb_strfree( pText );
   }
}

/* void setMarginText (int line, const QString &text, const QsciStyle &style) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTEXT_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->setMarginText( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), *hbqt_par_QsciStyle( 4 ) );
      hb_strfree( pText );
   }
}

/* void setMarginText (int line, const QsciStyledText &text) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTEXT_2 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginText( hb_parni( 2 ), *hbqt_par_QsciStyledText( 3 ) );
}

/* void setMarginType (int margin, MarginType type) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINTYPE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginType( hb_parni( 2 ), ( QsciScintilla::MarginType ) hb_parni( 3 ) );
}

/* void clearMarginText (int line=-1) */
HB_FUNC( QT_QSCISCINTILLA_CLEARMARGINTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clearMarginText( hb_parnidef( 2, -1 ) );
}

/* void setMarkerBackgroundColor (const QColor &col, int mnr=-1) */
HB_FUNC( QT_QSCISCINTILLA_SETMARKERBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarkerBackgroundColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
}

/* void setMarkerForegroundColor (const QColor &col, int mnr=-1) */
HB_FUNC( QT_QSCISCINTILLA_SETMARKERFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarkerForegroundColor( *hbqt_par_QColor( 2 ), hb_parnidef( 3, -1 ) );
}

/* void setMatchedBraceBackgroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETMATCHEDBRACEBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMatchedBraceBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* void setMatchedBraceForegroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETMATCHEDBRACEFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMatchedBraceForegroundColor( *hbqt_par_QColor( 2 ) );
}

/* void setUnmatchedBraceBackgroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETUNMATCHEDBRACEBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setUnmatchedBraceBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* void setUnmatchedBraceForegroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETUNMATCHEDBRACEFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setUnmatchedBraceForegroundColor( *hbqt_par_QColor( 2 ) );
}

/* void setWrapVisualFlags (WrapVisualFlag eflag, WrapVisualFlag sflag=WrapFlagNone, int sindent=0) */
HB_FUNC( QT_QSCISCINTILLA_SETWRAPVISUALFLAGS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setWrapVisualFlags( ( QsciScintilla::WrapVisualFlag ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QsciScintilla::WrapVisualFlag ) hb_parni( 3 ) : ( QsciScintilla::WrapVisualFlag ) QsciScintilla::WrapFlagNone ), hb_parni( 4 ) );
}

/* QString selectedText () const */
HB_FUNC( QT_QSCISCINTILLA_SELECTEDTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retstr_utf8( ( p )->selectedText().toUtf8().data() );
}

/* bool selectionToEol () const */
HB_FUNC( QT_QSCISCINTILLA_SELECTIONTOEOL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->selectionToEol() );
}

/* void setSelectionToEol (bool filled) */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTIONTOEOL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelectionToEol( hb_parl( 2 ) );
}

/* void showUserList (int id, const QStringList &list) */
HB_FUNC( QT_QSCISCINTILLA_SHOWUSERLIST )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->showUserList( hb_parni( 2 ), *hbqt_par_QStringList( 3 ) );
}

/* QsciCommandSet * standardCommands () const */
HB_FUNC( QT_QSCISCINTILLA_STANDARDCOMMANDS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QsciCommandSet( ( p )->standardCommands(), false ) );
}

/* bool tabIndents () const */
HB_FUNC( QT_QSCISCINTILLA_TABINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->tabIndents() );
}

/* int tabWidth () const */
HB_FUNC( QT_QSCISCINTILLA_TABWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->tabWidth() );
}

/* QString text () const */
HB_FUNC( QT_QSCISCINTILLA_TEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* QString text (int line) const */
HB_FUNC( QT_QSCISCINTILLA_TEXT_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text( hb_parni( 2 ) ).toUtf8().data() );
}

/* int textHeight (int linenr) const */
HB_FUNC( QT_QSCISCINTILLA_TEXTHEIGHT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( p )->textHeight( hb_parni( 2 ) ) );
}

/* WhitespaceVisibility whitespaceVisibility () const */
HB_FUNC( QT_QSCISCINTILLA_WHITESPACEVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::WhitespaceVisibility ) ( p )->whitespaceVisibility() );
}

/* QString wordAtPoint (const QPoint &point) const */
HB_FUNC( QT_QSCISCINTILLA_WORDATPOINT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retstr_utf8( ( p )->wordAtPoint( *hbqt_par_QPoint( 2 ) ).toUtf8().data() );
}

/* const char * wordCharacters () const */
HB_FUNC( QT_QSCISCINTILLA_WORDCHARACTERS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retc( ( p )->wordCharacters() );
}

/* WrapMode wrapMode () const */
HB_FUNC( QT_QSCISCINTILLA_WRAPMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retni( ( QsciScintilla::WrapMode ) ( p )->wrapMode() );
}

/* bool write (QIODevice *io) const */
HB_FUNC( QT_QSCISCINTILLA_WRITE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      hb_retl( ( p )->write( hbqt_par_QIODevice( 2 ) ) );
}

/* virtual void append (const QString &text) */
HB_FUNC( QT_QSCISCINTILLA_APPEND )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->append( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual void autoCompleteFromAll () */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETEFROMALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->autoCompleteFromAll();
}

/* virtual void autoCompleteFromAPIs () */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETEFROMAPIS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->autoCompleteFromAPIs();
}

/* virtual void autoCompleteFromDocument () */
HB_FUNC( QT_QSCISCINTILLA_AUTOCOMPLETEFROMDOCUMENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->autoCompleteFromDocument();
}

/* virtual void callTip () */
HB_FUNC( QT_QSCISCINTILLA_CALLTIP )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->callTip();
}

/* virtual void clear () */
HB_FUNC( QT_QSCISCINTILLA_CLEAR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->clear();
}

/* virtual void copy () */
HB_FUNC( QT_QSCISCINTILLA_COPY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->copy();
}

/* virtual void cut () */
HB_FUNC( QT_QSCISCINTILLA_CUT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->cut();
}

/* virtual void ensureCursorVisible () */
HB_FUNC( QT_QSCISCINTILLA_ENSURECURSORVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->ensureCursorVisible();
}

/* virtual void ensureLineVisible (int line) */
HB_FUNC( QT_QSCISCINTILLA_ENSURELINEVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->ensureLineVisible( hb_parni( 2 ) );
}

/* virtual void foldAll (bool children=false) */
HB_FUNC( QT_QSCISCINTILLA_FOLDALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->foldAll( hb_parl( 2 ) );
}

/* virtual void foldLine (int line) */
HB_FUNC( QT_QSCISCINTILLA_FOLDLINE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->foldLine( hb_parni( 2 ) );
}

/* virtual void indent (int line) */
HB_FUNC( QT_QSCISCINTILLA_INDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->indent( hb_parni( 2 ) );
}

/* virtual void insert (const QString &text) */
HB_FUNC( QT_QSCISCINTILLA_INSERT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->insert( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual void insertAt (const QString &text, int line, int index) */
HB_FUNC( QT_QSCISCINTILLA_INSERTAT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertAt( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), hb_parni( 4 ) );
      hb_strfree( pText );
   }
}

/* virtual void moveToMatchingBrace () */
HB_FUNC( QT_QSCISCINTILLA_MOVETOMATCHINGBRACE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->moveToMatchingBrace();
}

/* virtual void paste () */
HB_FUNC( QT_QSCISCINTILLA_PASTE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->paste();
}

/* virtual void redo () */
HB_FUNC( QT_QSCISCINTILLA_REDO )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->redo();
}

/* virtual void removeSelectedText () */
HB_FUNC( QT_QSCISCINTILLA_REMOVESELECTEDTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->removeSelectedText();
}

/* virtual void resetSelectionBackgroundColor () */
HB_FUNC( QT_QSCISCINTILLA_RESETSELECTIONBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->resetSelectionBackgroundColor();
}

/* virtual void resetSelectionForegroundColor () */
HB_FUNC( QT_QSCISCINTILLA_RESETSELECTIONFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->resetSelectionForegroundColor();
}

/* virtual void selectAll (bool select=true) */
HB_FUNC( QT_QSCISCINTILLA_SELECTALL )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->selectAll( hb_parl( 2 ) );
}

/* virtual void selectToMatchingBrace () */
HB_FUNC( QT_QSCISCINTILLA_SELECTTOMATCHINGBRACE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->selectToMatchingBrace();
}

/* virtual void setAutoCompletionCaseSensitivity (bool cs) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONCASESENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionCaseSensitivity( hb_parl( 2 ) );
}

/* virtual void setAutoCompletionReplaceWord (bool replace) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONREPLACEWORD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionReplaceWord( hb_parl( 2 ) );
}

/* virtual void setAutoCompletionShowSingle (bool single) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONSHOWSINGLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionShowSingle( hb_parl( 2 ) );
}

/* virtual void setAutoCompletionSource (AutoCompletionSource source) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONSOURCE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionSource( ( QsciScintilla::AutoCompletionSource ) hb_parni( 2 ) );
}

/* virtual void setAutoCompletionThreshold (int thresh) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOCOMPLETIONTHRESHOLD )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoCompletionThreshold( hb_parni( 2 ) );
}

/* virtual void setAutoIndent (bool autoindent) */
HB_FUNC( QT_QSCISCINTILLA_SETAUTOINDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setAutoIndent( hb_parl( 2 ) );
}

/* virtual void setBraceMatching (BraceMatch bm) */
HB_FUNC( QT_QSCISCINTILLA_SETBRACEMATCHING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setBraceMatching( ( QsciScintilla::BraceMatch ) hb_parni( 2 ) );
}

/* virtual void setBackspaceUnindents (bool unindent) */
HB_FUNC( QT_QSCISCINTILLA_SETBACKSPACEUNINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setBackspaceUnindents( hb_parl( 2 ) );
}

/* virtual void setCaretForegroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETCARETFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretForegroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setCaretLineBackgroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETCARETLINEBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretLineBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setCaretLineVisible (bool enable) */
HB_FUNC( QT_QSCISCINTILLA_SETCARETLINEVISIBLE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretLineVisible( hb_parl( 2 ) );
}

/* virtual void setCaretWidth (int width) */
HB_FUNC( QT_QSCISCINTILLA_SETCARETWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCaretWidth( hb_parni( 2 ) );
}

/* virtual void setColor (const QColor &c) */
HB_FUNC( QT_QSCISCINTILLA_SETCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setCursorPosition (int line, int index) */
HB_FUNC( QT_QSCISCINTILLA_SETCURSORPOSITION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setCursorPosition( hb_parni( 2 ), hb_parni( 3 ) );
}

/* virtual void setEolMode (EolMode mode) */
HB_FUNC( QT_QSCISCINTILLA_SETEOLMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEolMode( ( QsciScintilla::EolMode ) hb_parni( 2 ) );
}

/* virtual void setEolVisibility (bool visible) */
HB_FUNC( QT_QSCISCINTILLA_SETEOLVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setEolVisibility( hb_parl( 2 ) );
}

/* virtual void setFolding (FoldStyle fold, int margin=2) */
HB_FUNC( QT_QSCISCINTILLA_SETFOLDING )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setFolding( ( QsciScintilla::FoldStyle ) hb_parni( 2 ), hb_parnidef( 3, 2 ) );
}

/* virtual void setIndentation (int line, int indentation) */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentation( hb_parni( 2 ), hb_parni( 3 ) );
}

/* virtual void setIndentationGuides (bool enable) */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONGUIDES )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationGuides( hb_parl( 2 ) );
}

/* virtual void setIndentationGuidesBackgroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONGUIDESBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationGuidesBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setIndentationGuidesForegroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONGUIDESFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationGuidesForegroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setIndentationsUseTabs (bool tabs) */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONSUSETABS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationsUseTabs( hb_parl( 2 ) );
}

/* virtual void setIndentationWidth (int width) */
HB_FUNC( QT_QSCISCINTILLA_SETINDENTATIONWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setIndentationWidth( hb_parni( 2 ) );
}

/* virtual void setLexer (QsciLexer *lexer=0) */
HB_FUNC( QT_QSCISCINTILLA_SETLEXER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setLexer( hbqt_par_QsciLexer( 2 ) );
}

/* virtual void setMarginsBackgroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginsBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setMarginsFont (const QFont &f) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSFONT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginsFont( *hbqt_par_QFont( 2 ) );
}

/* virtual void setMarginsForegroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginsForegroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setMarginLineNumbers (int margin, bool lnrs) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINLINENUMBERS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginLineNumbers( hb_parni( 2 ), hb_parl( 3 ) );
}

/* virtual void setMarginMarkerMask (int margin, int mask) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINMARKERMASK )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginMarkerMask( hb_parni( 2 ), hb_parni( 3 ) );
}

/* virtual void setMarginSensitivity (int margin, bool sens) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINSENSITIVITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginSensitivity( hb_parni( 2 ), hb_parl( 3 ) );
}

/* virtual void setMarginWidth (int margin, int width) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setMarginWidth( hb_parni( 2 ), hb_parni( 3 ) );
}

/* virtual void setMarginWidth (int margin, const QString &s) */
HB_FUNC( QT_QSCISCINTILLA_SETMARGINWIDTH_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->setMarginWidth( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual void setModified (bool m) */
HB_FUNC( QT_QSCISCINTILLA_SETMODIFIED )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setModified( hb_parl( 2 ) );
}

/* virtual void setPaper (const QColor &c) */
HB_FUNC( QT_QSCISCINTILLA_SETPAPER )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setPaper( *hbqt_par_QColor( 2 ) );
}

/* virtual void setReadOnly (bool ro) */
HB_FUNC( QT_QSCISCINTILLA_SETREADONLY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setReadOnly( hb_parl( 2 ) );
}

/* virtual void setSelection (int lineFrom, int indexFrom, int lineTo, int indexTo) */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTION )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelection( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* virtual void setSelectionBackgroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTIONBACKGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelectionBackgroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setSelectionForegroundColor (const QColor &col) */
HB_FUNC( QT_QSCISCINTILLA_SETSELECTIONFOREGROUNDCOLOR )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setSelectionForegroundColor( *hbqt_par_QColor( 2 ) );
}

/* virtual void setTabIndents (bool indent) */
HB_FUNC( QT_QSCISCINTILLA_SETTABINDENTS )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setTabIndents( hb_parl( 2 ) );
}

/* virtual void setTabWidth (int width) */
HB_FUNC( QT_QSCISCINTILLA_SETTABWIDTH )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setTabWidth( hb_parni( 2 ) );
}

/* virtual void setText (const QString &text) */
HB_FUNC( QT_QSCISCINTILLA_SETTEXT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual void setUtf8 (bool cp) */
HB_FUNC( QT_QSCISCINTILLA_SETUTF8 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setUtf8( hb_parl( 2 ) );
}

/* virtual void setWhitespaceVisibility (WhitespaceVisibility mode) */
HB_FUNC( QT_QSCISCINTILLA_SETWHITESPACEVISIBILITY )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setWhitespaceVisibility( ( QsciScintilla::WhitespaceVisibility ) hb_parni( 2 ) );
}

/* virtual void setWrapMode (WrapMode mode) */
HB_FUNC( QT_QSCISCINTILLA_SETWRAPMODE )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->setWrapMode( ( QsciScintilla::WrapMode ) hb_parni( 2 ) );
}

/* virtual void undo () */
HB_FUNC( QT_QSCISCINTILLA_UNDO )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->undo();
}

/* virtual void unindent (int line) */
HB_FUNC( QT_QSCISCINTILLA_UNINDENT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->unindent( hb_parni( 2 ) );
}

/* virtual void zoomIn (int range) */
HB_FUNC( QT_QSCISCINTILLA_ZOOMIN )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomIn( hb_parni( 2 ) );
}

/* virtual void zoomIn () */
HB_FUNC( QT_QSCISCINTILLA_ZOOMIN_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomIn();
}

/* virtual void zoomOut (int range) */
HB_FUNC( QT_QSCISCINTILLA_ZOOMOUT )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomOut( hb_parni( 2 ) );
}

/* virtual void zoomOut () */
HB_FUNC( QT_QSCISCINTILLA_ZOOMOUT_1 )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomOut();
}

/* virtual void zoomTo (int size) */
HB_FUNC( QT_QSCISCINTILLA_ZOOMTO )
{
   QsciScintilla * p = hbqt_par_QsciScintilla( 1 );
   if( p )
      ( p )->zoomTo( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
