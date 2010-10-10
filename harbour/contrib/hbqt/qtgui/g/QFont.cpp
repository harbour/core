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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Capitalization { MixedCase, AllUppercase, AllLowercase, SmallCaps, Capitalize }
 *  enum SpacingType { PercentageSpacing, AbsoluteSpacing }
 *  enum Stretch { UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, ..., UltraExpanded }
 *  enum Style { StyleNormal, StyleItalic, StyleOblique }
 *  enum StyleHint { AnyStyle, SansSerif, Helvetica, Serif, ..., System }
 *  enum StyleStrategy { PreferDefault, PreferBitmap, PreferDevice, PreferOutline, ..., PreferQuality }
 *  enum Weight { Light, Normal, DemiBold, Bold, Black }
 */

/*
 *  Constructed[ 58/58 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // FT_Face freetypeFace () const
 *  // HFONT handle () const
 *  // bool isCopyOf ( const QFont & f ) const
 *  // quint32 macFontID () const
 *  // QFont resolve ( const QFont & other ) const
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>
#include <QtGui/QFont>

/*
 * QFont ()
 * QFont ( const QString & family, int pointSize = -1, int weight = -1, bool italic = false )
 * QFont ( const QFont & font, QPaintDevice * pd )
 * QFont ( const QFont & font )
 * ~QFont ()
 */

typedef struct
{
   QFont * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFont;

HBQT_GC_FUNC( hbqt_gcRelease_QFont )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFont   /.\\", p->ph ) );
         delete ( ( QFont * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFont   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFont    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFont    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFont( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QFont * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFont;
   p->type = HBQT_TYPE_QFont;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFont", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFont", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONT )
{
   QFont * pObj = NULL;

   if(      hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QFont( hbqt_par_QString( 1 ), -1, -1, false ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QFont( hbqt_par_QString( 1 ), hb_parni( 2 ), -1, false ) ;
   }
   else if( hb_pcount() == 3 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 )  && HB_ISNUM( 3 ) )
   {
      pObj = new QFont( hbqt_par_QString( 1 ), hb_parni( 2 ), hb_parni( 3 ), false ) ;
   }
   else if( hb_pcount() == 4 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 )  && HB_ISNUM( 3 ) && HB_ISLOG( 4 ) )
   {
      pObj = new QFont( hbqt_par_QString( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFont( *hbqt_par_QFont( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QFont( *hbqt_par_QFont( 1 ), hbqt_par_QPaintDevice( 2 ) ) ;
   }
   else
   {
      pObj = new QFont() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFont( ( void * ) pObj, true ) );
}

/*
 * bool bold () const
 */
HB_FUNC( QT_QFONT_BOLD )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->bold() );
   }
}

/*
 * Capitalization capitalization () const
 */
HB_FUNC( QT_QFONT_CAPITALIZATION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( QFont::Capitalization ) ( p )->capitalization() );
   }
}

/*
 * QString defaultFamily () const
 */
HB_FUNC( QT_QFONT_DEFAULTFAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->defaultFamily().toUtf8().data() );
   }
}

/*
 * bool exactMatch () const
 */
HB_FUNC( QT_QFONT_EXACTMATCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->exactMatch() );
   }
}

/*
 * QString family () const
 */
HB_FUNC( QT_QFONT_FAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->family().toUtf8().data() );
   }
}

/*
 * bool fixedPitch () const
 */
HB_FUNC( QT_QFONT_FIXEDPITCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->fixedPitch() );
   }
}

/*
 * bool fromString ( const QString & descrip )
 */
HB_FUNC( QT_QFONT_FROMSTRING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->fromString( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool italic () const
 */
HB_FUNC( QT_QFONT_ITALIC )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->italic() );
   }
}

/*
 * bool kerning () const
 */
HB_FUNC( QT_QFONT_KERNING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->kerning() );
   }
}

/*
 * QString key () const
 */
HB_FUNC( QT_QFONT_KEY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->key().toUtf8().data() );
   }
}

/*
 * QString lastResortFamily () const
 */
HB_FUNC( QT_QFONT_LASTRESORTFAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->lastResortFamily().toUtf8().data() );
   }
}

/*
 * QString lastResortFont () const
 */
HB_FUNC( QT_QFONT_LASTRESORTFONT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->lastResortFont().toUtf8().data() );
   }
}

/*
 * qreal letterSpacing () const
 */
HB_FUNC( QT_QFONT_LETTERSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retnd( ( p )->letterSpacing() );
   }
}

/*
 * SpacingType letterSpacingType () const
 */
HB_FUNC( QT_QFONT_LETTERSPACINGTYPE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( QFont::SpacingType ) ( p )->letterSpacingType() );
   }
}

/*
 * bool overline () const
 */
HB_FUNC( QT_QFONT_OVERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->overline() );
   }
}

/*
 * int pixelSize () const
 */
HB_FUNC( QT_QFONT_PIXELSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( p )->pixelSize() );
   }
}

/*
 * int pointSize () const
 */
HB_FUNC( QT_QFONT_POINTSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( p )->pointSize() );
   }
}

/*
 * qreal pointSizeF () const
 */
HB_FUNC( QT_QFONT_POINTSIZEF )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retnd( ( p )->pointSizeF() );
   }
}

/*
 * bool rawMode () const
 */
HB_FUNC( QT_QFONT_RAWMODE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->rawMode() );
   }
}

/*
 * QString rawName () const
 */
HB_FUNC( QT_QFONT_RAWNAME )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->rawName().toUtf8().data() );
   }
}

/*
 * void setBold ( bool enable )
 */
HB_FUNC( QT_QFONT_SETBOLD )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setBold( hb_parl( 2 ) );
   }
}

/*
 * void setCapitalization ( Capitalization caps )
 */
HB_FUNC( QT_QFONT_SETCAPITALIZATION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setCapitalization( ( QFont::Capitalization ) hb_parni( 2 ) );
   }
}

/*
 * void setFamily ( const QString & family )
 */
HB_FUNC( QT_QFONT_SETFAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      ( p )->setFamily( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setFixedPitch ( bool enable )
 */
HB_FUNC( QT_QFONT_SETFIXEDPITCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setFixedPitch( hb_parl( 2 ) );
   }
}

/*
 * void setItalic ( bool enable )
 */
HB_FUNC( QT_QFONT_SETITALIC )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setItalic( hb_parl( 2 ) );
   }
}

/*
 * void setKerning ( bool enable )
 */
HB_FUNC( QT_QFONT_SETKERNING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setKerning( hb_parl( 2 ) );
   }
}

/*
 * void setLetterSpacing ( SpacingType type, qreal spacing )
 */
HB_FUNC( QT_QFONT_SETLETTERSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setLetterSpacing( ( QFont::SpacingType ) hb_parni( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setOverline ( bool enable )
 */
HB_FUNC( QT_QFONT_SETOVERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setOverline( hb_parl( 2 ) );
   }
}

/*
 * void setPixelSize ( int pixelSize )
 */
HB_FUNC( QT_QFONT_SETPIXELSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setPixelSize( hb_parni( 2 ) );
   }
}

/*
 * void setPointSize ( int pointSize )
 */
HB_FUNC( QT_QFONT_SETPOINTSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setPointSize( hb_parni( 2 ) );
   }
}

/*
 * void setPointSizeF ( qreal pointSize )
 */
HB_FUNC( QT_QFONT_SETPOINTSIZEF )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setPointSizeF( hb_parnd( 2 ) );
   }
}

/*
 * void setRawMode ( bool enable )
 */
HB_FUNC( QT_QFONT_SETRAWMODE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setRawMode( hb_parl( 2 ) );
   }
}

/*
 * void setRawName ( const QString & name )
 */
HB_FUNC( QT_QFONT_SETRAWNAME )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      ( p )->setRawName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setStretch ( int factor )
 */
HB_FUNC( QT_QFONT_SETSTRETCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setStretch( hb_parni( 2 ) );
   }
}

/*
 * void setStrikeOut ( bool enable )
 */
HB_FUNC( QT_QFONT_SETSTRIKEOUT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setStrikeOut( hb_parl( 2 ) );
   }
}

/*
 * void setStyle ( Style style )
 */
HB_FUNC( QT_QFONT_SETSTYLE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setStyle( ( QFont::Style ) hb_parni( 2 ) );
   }
}

/*
 * void setStyleHint ( StyleHint hint, StyleStrategy strategy = PreferDefault )
 */
HB_FUNC( QT_QFONT_SETSTYLEHINT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setStyleHint( ( QFont::StyleHint ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QFont::StyleStrategy ) hb_parni( 3 ) : ( QFont::StyleStrategy ) QFont::PreferDefault ) );
   }
}

/*
 * void setStyleStrategy ( StyleStrategy s )
 */
HB_FUNC( QT_QFONT_SETSTYLESTRATEGY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setStyleStrategy( ( QFont::StyleStrategy ) hb_parni( 2 ) );
   }
}

/*
 * void setUnderline ( bool enable )
 */
HB_FUNC( QT_QFONT_SETUNDERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setUnderline( hb_parl( 2 ) );
   }
}

/*
 * void setWeight ( int weight )
 */
HB_FUNC( QT_QFONT_SETWEIGHT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setWeight( hb_parni( 2 ) );
   }
}

/*
 * void setWordSpacing ( qreal spacing )
 */
HB_FUNC( QT_QFONT_SETWORDSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->setWordSpacing( hb_parnd( 2 ) );
   }
}

/*
 * int stretch () const
 */
HB_FUNC( QT_QFONT_STRETCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( p )->stretch() );
   }
}

/*
 * bool strikeOut () const
 */
HB_FUNC( QT_QFONT_STRIKEOUT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->strikeOut() );
   }
}

/*
 * Style style () const
 */
HB_FUNC( QT_QFONT_STYLE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( QFont::Style ) ( p )->style() );
   }
}

/*
 * StyleHint styleHint () const
 */
HB_FUNC( QT_QFONT_STYLEHINT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( QFont::StyleHint ) ( p )->styleHint() );
   }
}

/*
 * StyleStrategy styleStrategy () const
 */
HB_FUNC( QT_QFONT_STYLESTRATEGY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( QFont::StyleStrategy ) ( p )->styleStrategy() );
   }
}

/*
 * QString toString () const
 */
HB_FUNC( QT_QFONT_TOSTRING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toString().toUtf8().data() );
   }
}

/*
 * bool underline () const
 */
HB_FUNC( QT_QFONT_UNDERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retl( ( p )->underline() );
   }
}

/*
 * int weight () const
 */
HB_FUNC( QT_QFONT_WEIGHT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retni( ( p )->weight() );
   }
}

/*
 * qreal wordSpacing () const
 */
HB_FUNC( QT_QFONT_WORDSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retnd( ( p )->wordSpacing() );
   }
}

/*
 * void cleanup ()
 */
HB_FUNC( QT_QFONT_CLEANUP )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->cleanup();
   }
}

/*
 * void initialize ()
 */
HB_FUNC( QT_QFONT_INITIALIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      ( p )->initialize();
   }
}

/*
 * void insertSubstitution ( const QString & familyName, const QString & substituteName )
 */
HB_FUNC( QT_QFONT_INSERTSUBSTITUTION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertSubstitution( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void insertSubstitutions ( const QString & familyName, const QStringList & substituteNames )
 */
HB_FUNC( QT_QFONT_INSERTSUBSTITUTIONS )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      ( p )->insertSubstitutions( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringList( 3 ) );
      hb_strfree( pText );
   }
}

/*
 * void removeSubstitution ( const QString & familyName )
 */
HB_FUNC( QT_QFONT_REMOVESUBSTITUTION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      ( p )->removeSubstitution( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QString substitute ( const QString & familyName )
 */
HB_FUNC( QT_QFONT_SUBSTITUTE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->substitute( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QStringList substitutes ( const QString & familyName )
 */
HB_FUNC( QT_QFONT_SUBSTITUTES )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->substitutes( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList substitutions ()
 */
HB_FUNC( QT_QFONT_SUBSTITUTIONS )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->substitutions() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
