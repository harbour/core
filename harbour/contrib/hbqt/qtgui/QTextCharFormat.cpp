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
 *  enum UnderlineStyle { NoUnderline, SingleUnderline, DashUnderline, DotLine, ..., SpellCheckUnderline }
 *  enum VerticalAlignment { AlignNormal, AlignSuperScript, AlignSubScript, AlignMiddle, AlignBottom, AlignTop }
 */

#include <QtCore/QPointer>

#include <QtGui/QTextCharFormat>


/*
 * QTextCharFormat ()
 *
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QTextCharFormat;

QT_G_FUNC( hbqt_gcRelease_QTextCharFormat )
{
      QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextCharFormat * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextCharFormat            ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextCharFormat             Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextCharFormat             Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextCharFormat( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextCharFormat;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextCharFormat            ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTCHARFORMAT )
{
   void * pObj = NULL;

   pObj = ( QTextCharFormat* ) new QTextCharFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( pObj, true ) );
}
/*
 * QString anchorHref () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_ANCHORHREF )
{
   hb_retc( hbqt_par_QTextCharFormat( 1 )->anchorHref().toAscii().data() );
}

/*
 * QStringList anchorNames () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_ANCHORNAMES )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QTextCharFormat( 1 )->anchorNames() ), true ) );
}

/*
 * QFont font () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QTextCharFormat( 1 )->font() ), true ) );
}

/*
 * QFont::Capitalization fontCapitalization () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTCAPITALIZATION )
{
   hb_retni( ( QFont::Capitalization ) hbqt_par_QTextCharFormat( 1 )->fontCapitalization() );
}

/*
 * QString fontFamily () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTFAMILY )
{
   hb_retc( hbqt_par_QTextCharFormat( 1 )->fontFamily().toAscii().data() );
}

/*
 * bool fontFixedPitch () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTFIXEDPITCH )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->fontFixedPitch() );
}

/*
 * bool fontItalic () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTITALIC )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->fontItalic() );
}

/*
 * bool fontKerning () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTKERNING )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->fontKerning() );
}

/*
 * qreal fontLetterSpacing () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTLETTERSPACING )
{
   hb_retnd( hbqt_par_QTextCharFormat( 1 )->fontLetterSpacing() );
}

/*
 * bool fontOverline () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTOVERLINE )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->fontOverline() );
}

/*
 * qreal fontPointSize () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTPOINTSIZE )
{
   hb_retnd( hbqt_par_QTextCharFormat( 1 )->fontPointSize() );
}

/*
 * bool fontStrikeOut () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTSTRIKEOUT )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->fontStrikeOut() );
}

/*
 * QFont::StyleHint fontStyleHint () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTSTYLEHINT )
{
   hb_retni( ( QFont::StyleHint ) hbqt_par_QTextCharFormat( 1 )->fontStyleHint() );
}

/*
 * QFont::StyleStrategy fontStyleStrategy () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTSTYLESTRATEGY )
{
   hb_retni( ( QFont::StyleStrategy ) hbqt_par_QTextCharFormat( 1 )->fontStyleStrategy() );
}

/*
 * bool fontUnderline () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTUNDERLINE )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->fontUnderline() );
}

/*
 * int fontWeight () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTWEIGHT )
{
   hb_retni( hbqt_par_QTextCharFormat( 1 )->fontWeight() );
}

/*
 * qreal fontWordSpacing () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_FONTWORDSPACING )
{
   hb_retnd( hbqt_par_QTextCharFormat( 1 )->fontWordSpacing() );
}

/*
 * bool isAnchor () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_ISANCHOR )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->isAnchor() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_ISVALID )
{
   hb_retl( hbqt_par_QTextCharFormat( 1 )->isValid() );
}

/*
 * void setAnchor ( bool anchor )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETANCHOR )
{
   hbqt_par_QTextCharFormat( 1 )->setAnchor( hb_parl( 2 ) );
}

/*
 * void setAnchorHref ( const QString & value )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETANCHORHREF )
{
   hbqt_par_QTextCharFormat( 1 )->setAnchorHref( hbqt_par_QString( 2 ) );
}

/*
 * void setAnchorNames ( const QStringList & names )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETANCHORNAMES )
{
   hbqt_par_QTextCharFormat( 1 )->setAnchorNames( *hbqt_par_QStringList( 2 ) );
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONT )
{
   hbqt_par_QTextCharFormat( 1 )->setFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setFontCapitalization ( QFont::Capitalization capitalization )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTCAPITALIZATION )
{
   hbqt_par_QTextCharFormat( 1 )->setFontCapitalization( ( QFont::Capitalization ) hb_parni( 2 ) );
}

/*
 * void setFontFamily ( const QString & family )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTFAMILY )
{
   hbqt_par_QTextCharFormat( 1 )->setFontFamily( hbqt_par_QString( 2 ) );
}

/*
 * void setFontFixedPitch ( bool fixedPitch )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTFIXEDPITCH )
{
   hbqt_par_QTextCharFormat( 1 )->setFontFixedPitch( hb_parl( 2 ) );
}

/*
 * void setFontItalic ( bool italic )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTITALIC )
{
   hbqt_par_QTextCharFormat( 1 )->setFontItalic( hb_parl( 2 ) );
}

/*
 * void setFontKerning ( bool enable )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTKERNING )
{
   hbqt_par_QTextCharFormat( 1 )->setFontKerning( hb_parl( 2 ) );
}

/*
 * void setFontLetterSpacing ( qreal spacing )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTLETTERSPACING )
{
   hbqt_par_QTextCharFormat( 1 )->setFontLetterSpacing( hb_parnd( 2 ) );
}

/*
 * void setFontOverline ( bool overline )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTOVERLINE )
{
   hbqt_par_QTextCharFormat( 1 )->setFontOverline( hb_parl( 2 ) );
}

/*
 * void setFontPointSize ( qreal size )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTPOINTSIZE )
{
   hbqt_par_QTextCharFormat( 1 )->setFontPointSize( hb_parnd( 2 ) );
}

/*
 * void setFontStrikeOut ( bool strikeOut )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTSTRIKEOUT )
{
   hbqt_par_QTextCharFormat( 1 )->setFontStrikeOut( hb_parl( 2 ) );
}

/*
 * void setFontStyleHint ( QFont::StyleHint hint, QFont::StyleStrategy strategy = QFont::PreferDefault )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTSTYLEHINT )
{
   hbqt_par_QTextCharFormat( 1 )->setFontStyleHint( ( QFont::StyleHint ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QFont::StyleStrategy ) hb_parni( 3 ) : ( QFont::StyleStrategy ) QFont::PreferDefault ) );
}

/*
 * void setFontStyleStrategy ( QFont::StyleStrategy strategy )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTSTYLESTRATEGY )
{
   hbqt_par_QTextCharFormat( 1 )->setFontStyleStrategy( ( QFont::StyleStrategy ) hb_parni( 2 ) );
}

/*
 * void setFontUnderline ( bool underline )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTUNDERLINE )
{
   hbqt_par_QTextCharFormat( 1 )->setFontUnderline( hb_parl( 2 ) );
}

/*
 * void setFontWeight ( int weight )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTWEIGHT )
{
   hbqt_par_QTextCharFormat( 1 )->setFontWeight( hb_parni( 2 ) );
}

/*
 * void setFontWordSpacing ( qreal spacing )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETFONTWORDSPACING )
{
   hbqt_par_QTextCharFormat( 1 )->setFontWordSpacing( hb_parnd( 2 ) );
}

/*
 * void setTextOutline ( const QPen & pen )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETTEXTOUTLINE )
{
   hbqt_par_QTextCharFormat( 1 )->setTextOutline( *hbqt_par_QPen( 2 ) );
}

/*
 * void setToolTip ( const QString & text )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETTOOLTIP )
{
   hbqt_par_QTextCharFormat( 1 )->setToolTip( hbqt_par_QString( 2 ) );
}

/*
 * void setUnderlineColor ( const QColor & color )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETUNDERLINECOLOR )
{
   hbqt_par_QTextCharFormat( 1 )->setUnderlineColor( *hbqt_par_QColor( 2 ) );
}

/*
 * void setUnderlineStyle ( UnderlineStyle style )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETUNDERLINESTYLE )
{
   hbqt_par_QTextCharFormat( 1 )->setUnderlineStyle( ( QTextCharFormat::UnderlineStyle ) hb_parni( 2 ) );
}

/*
 * void setVerticalAlignment ( VerticalAlignment alignment )
 */
HB_FUNC( QT_QTEXTCHARFORMAT_SETVERTICALALIGNMENT )
{
   hbqt_par_QTextCharFormat( 1 )->setVerticalAlignment( ( QTextCharFormat::VerticalAlignment ) hb_parni( 2 ) );
}

/*
 * QPen textOutline () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_TEXTOUTLINE )
{
   hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( hbqt_par_QTextCharFormat( 1 )->textOutline() ), true ) );
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_TOOLTIP )
{
   hb_retc( hbqt_par_QTextCharFormat( 1 )->toolTip().toAscii().data() );
}

/*
 * QColor underlineColor () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_UNDERLINECOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QTextCharFormat( 1 )->underlineColor() ), true ) );
}

/*
 * UnderlineStyle underlineStyle () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_UNDERLINESTYLE )
{
   hb_retni( ( QTextCharFormat::UnderlineStyle ) hbqt_par_QTextCharFormat( 1 )->underlineStyle() );
}

/*
 * VerticalAlignment verticalAlignment () const
 */
HB_FUNC( QT_QTEXTCHARFORMAT_VERTICALALIGNMENT )
{
   hb_retni( ( QTextCharFormat::VerticalAlignment ) hbqt_par_QTextCharFormat( 1 )->verticalAlignment() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
