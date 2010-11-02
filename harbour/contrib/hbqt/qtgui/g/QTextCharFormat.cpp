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
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum UnderlineStyle { NoUnderline, SingleUnderline, DashUnderline, DotLine, ..., SpellCheckUnderline }
 *  enum VerticalAlignment { AlignNormal, AlignSuperScript, AlignSubScript, AlignMiddle, AlignBottom, AlignTop }
 */

/*
 *  Constructed[ 47/47 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QTextCharFormat>


/*
 * QTextCharFormat ()
 *
 */

typedef struct
{
   QTextCharFormat * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextCharFormat;

HBQT_GC_FUNC( hbqt_gcRelease_QTextCharFormat )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextCharFormat * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QTextCharFormat( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextCharFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextCharFormat;
   p->type = HBQT_TYPE_QTextCharFormat;

   return p;
}

HB_FUNC( QT_QTEXTCHARFORMAT )
{
   QTextCharFormat * pObj = NULL;

   pObj = new QTextCharFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( ( void * ) pObj, true ) );
}

/* QString anchorHref () const */
HB_FUNC( QT_QTEXTCHARFORMAT_ANCHORHREF )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retstr_utf8( ( p )->anchorHref().toUtf8().data() );
}

/* QStringList anchorNames () const */
HB_FUNC( QT_QTEXTCHARFORMAT_ANCHORNAMES )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->anchorNames() ), true ) );
}

/* QFont font () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* QFont::Capitalization fontCapitalization () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTCAPITALIZATION )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retni( ( QFont::Capitalization ) ( p )->fontCapitalization() );
}

/* QString fontFamily () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTFAMILY )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fontFamily().toUtf8().data() );
}

/* bool fontFixedPitch () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTFIXEDPITCH )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->fontFixedPitch() );
}

/* bool fontItalic () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTITALIC )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->fontItalic() );
}

/* bool fontKerning () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTKERNING )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->fontKerning() );
}

/* qreal fontLetterSpacing () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTLETTERSPACING )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retnd( ( p )->fontLetterSpacing() );
}

/* bool fontOverline () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTOVERLINE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->fontOverline() );
}

/* qreal fontPointSize () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTPOINTSIZE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retnd( ( p )->fontPointSize() );
}

/* bool fontStrikeOut () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTSTRIKEOUT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->fontStrikeOut() );
}

/* QFont::StyleHint fontStyleHint () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTSTYLEHINT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retni( ( QFont::StyleHint ) ( p )->fontStyleHint() );
}

/* QFont::StyleStrategy fontStyleStrategy () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTSTYLESTRATEGY )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retni( ( QFont::StyleStrategy ) ( p )->fontStyleStrategy() );
}

/* bool fontUnderline () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTUNDERLINE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->fontUnderline() );
}

/* int fontWeight () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTWEIGHT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retni( ( p )->fontWeight() );
}

/* qreal fontWordSpacing () const */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTWORDSPACING )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retnd( ( p )->fontWordSpacing() );
}

/* bool isAnchor () const */
HB_FUNC( QT_QTEXTCHARFORMAT_ISANCHOR )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->isAnchor() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTCHARFORMAT_ISVALID )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* void setAnchor ( bool anchor ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETANCHOR )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setAnchor( hb_parl( 2 ) );
}

/* void setAnchorHref ( const QString & value ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETANCHORHREF )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
   {
      void * pText;
      ( p )->setAnchorHref( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setAnchorNames ( const QStringList & names ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETANCHORNAMES )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setAnchorNames( *hbqt_par_QStringList( 2 ) );
}

/* void setFont ( const QFont & font ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setFontCapitalization ( QFont::Capitalization capitalization ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTCAPITALIZATION )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontCapitalization( ( QFont::Capitalization ) hb_parni( 2 ) );
}

/* void setFontFamily ( const QString & family ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTFAMILY )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFontFamily( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setFontFixedPitch ( bool fixedPitch ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTFIXEDPITCH )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontFixedPitch( hb_parl( 2 ) );
}

/* void setFontItalic ( bool italic ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTITALIC )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontItalic( hb_parl( 2 ) );
}

/* void setFontKerning ( bool enable ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTKERNING )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontKerning( hb_parl( 2 ) );
}

/* void setFontLetterSpacing ( qreal spacing ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTLETTERSPACING )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontLetterSpacing( hb_parnd( 2 ) );
}

/* void setFontOverline ( bool overline ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTOVERLINE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontOverline( hb_parl( 2 ) );
}

/* void setFontPointSize ( qreal size ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTPOINTSIZE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontPointSize( hb_parnd( 2 ) );
}

/* void setFontStrikeOut ( bool strikeOut ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTSTRIKEOUT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontStrikeOut( hb_parl( 2 ) );
}

/* void setFontStyleHint ( QFont::StyleHint hint, QFont::StyleStrategy strategy = QFont::PreferDefault ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTSTYLEHINT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontStyleHint( ( QFont::StyleHint ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QFont::StyleStrategy ) hb_parni( 3 ) : ( QFont::StyleStrategy ) QFont::PreferDefault ) );
}

/* void setFontStyleStrategy ( QFont::StyleStrategy strategy ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTSTYLESTRATEGY )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontStyleStrategy( ( QFont::StyleStrategy ) hb_parni( 2 ) );
}

/* void setFontUnderline ( bool underline ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTUNDERLINE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontUnderline( hb_parl( 2 ) );
}

/* void setFontWeight ( int weight ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTWEIGHT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontWeight( hb_parni( 2 ) );
}

/* void setFontWordSpacing ( qreal spacing ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTWORDSPACING )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setFontWordSpacing( hb_parnd( 2 ) );
}

/* void setTextOutline ( const QPen & pen ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETTEXTOUTLINE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setTextOutline( *hbqt_par_QPen( 2 ) );
}

/* void setToolTip ( const QString & text ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETTOOLTIP )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setUnderlineColor ( const QColor & color ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETUNDERLINECOLOR )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setUnderlineColor( *hbqt_par_QColor( 2 ) );
}

/* void setUnderlineStyle ( UnderlineStyle style ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETUNDERLINESTYLE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setUnderlineStyle( ( QTextCharFormat::UnderlineStyle ) hb_parni( 2 ) );
}

/* void setVerticalAlignment ( VerticalAlignment alignment ) */
HB_FUNC( QT_QTEXTCHARFORMAT_SETVERTICALALIGNMENT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      ( p )->setVerticalAlignment( ( QTextCharFormat::VerticalAlignment ) hb_parni( 2 ) );
}

/* QPen textOutline () const */
HB_FUNC( QT_QTEXTCHARFORMAT_TEXTOUTLINE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->textOutline() ), true ) );
}

/* QString toolTip () const */
HB_FUNC( QT_QTEXTCHARFORMAT_TOOLTIP )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
}

/* QColor underlineColor () const */
HB_FUNC( QT_QTEXTCHARFORMAT_UNDERLINECOLOR )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->underlineColor() ), true ) );
}

/* UnderlineStyle underlineStyle () const */
HB_FUNC( QT_QTEXTCHARFORMAT_UNDERLINESTYLE )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retni( ( QTextCharFormat::UnderlineStyle ) ( p )->underlineStyle() );
}

/* VerticalAlignment verticalAlignment () const */
HB_FUNC( QT_QTEXTCHARFORMAT_VERTICALALIGNMENT )
{
   QTextCharFormat * p = hbqt_par_QTextCharFormat( 1 );
   if( p )
      hb_retni( ( QTextCharFormat::VerticalAlignment ) ( p )->verticalAlignment() );
}


#endif /* #if QT_VERSION >= 0x040500 */
