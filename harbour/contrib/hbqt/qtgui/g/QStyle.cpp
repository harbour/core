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
 *  flags State
 *  flags SubControls
 *  enum ComplexControl { CC_SpinBox, CC_ComboBox, CC_ScrollBar, CC_Slider, ..., CC_CustomBase }
 *  enum ContentsType { CT_CheckBox, CT_ComboBox, CT_Q3DockWindow, CT_HeaderSection, ..., CT_MdiControls }
 *  enum ControlElement { CE_PushButton, CE_PushButtonBevel, CE_PushButtonLabel, CE_DockWidgetTitle, ..., CE_ShapedFrame }
 *  enum PixelMetric { PM_ButtonMargin, PM_DockWidgetTitleBarButtonMargin, PM_ButtonDefaultIndicator, PM_MenuButtonIndicator, ..., PM_SubMenuOverlap }
 *  enum PrimitiveElement { PE_FrameStatusBar, PE_PanelButtonCommand, PE_FrameDefaultButton, PE_PanelButtonBevel, ..., PE_PanelMenu }
 *  enum StandardPixmap { SP_TitleBarMinButton, SP_TitleBarMenuButton, SP_TitleBarMaxButton, SP_TitleBarCloseButton, ..., SP_CustomBase }
 *  enum StateFlag { State_None, State_Active, State_AutoRaise, State_Children, ..., State_Small }
 *  enum StyleHint { SH_EtchDisabledText, SH_DitherDisabledText, SH_GUIStyle, SH_ScrollBar_ContextMenu, ..., SH_DockWidget_ButtonsHaveFrame }
 *  enum SubControl { SC_None, SC_ScrollBarAddLine, SC_ScrollBarSubLine, SC_ScrollBarAddPage, ..., SC_All }
 *  enum SubElement { SE_PushButtonContents, SE_PushButtonFocusRect, SE_PushButtonLayoutItem, SE_CheckBoxIndicator, ..., SE_TabBarTabText }
 */

/*
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyle>


/*
 * QStyle ()
 * virtual ~QStyle ()
 */

typedef struct
{
   QPointer< QStyle > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyle;

HBQT_GC_FUNC( hbqt_gcRelease_QStyle )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyle( void * pObj, bool bNew )
{
   HBQT_GC_T_QStyle * p = ( HBQT_GC_T_QStyle * ) hb_gcAllocate( sizeof( HBQT_GC_T_QStyle ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStyle >( ( QStyle * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyle;
   p->type = HBQT_TYPE_QStyle;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStyle  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStyle", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTYLE )
{

}

/*
 * int combinedLayoutSpacing ( QSizePolicy::ControlTypes controls1, QSizePolicy::ControlTypes controls2, Qt::Orientation orientation, QStyleOption * option = 0, QWidget * widget = 0 ) const
 */
HB_FUNC( QT_QSTYLE_COMBINEDLAYOUTSPACING )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( p )->combinedLayoutSpacing( ( QSizePolicy::ControlTypes ) hb_parni( 2 ), ( QSizePolicy::ControlTypes ) hb_parni( 3 ), ( Qt::Orientation ) hb_parni( 4 ), hbqt_par_QStyleOption( 5 ), hbqt_par_QWidget( 6 ) ) );
   }
}

/*
 * virtual void drawComplexControl ( ComplexControl control, const QStyleOptionComplex * option, QPainter * painter, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_DRAWCOMPLEXCONTROL )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->drawComplexControl( ( QStyle::ComplexControl ) hb_parni( 2 ), hbqt_par_QStyleOptionComplex( 3 ), hbqt_par_QPainter( 4 ), hbqt_par_QWidget( 5 ) );
   }
}

/*
 * virtual void drawControl ( ControlElement element, const QStyleOption * option, QPainter * painter, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_DRAWCONTROL )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->drawControl( ( QStyle::ControlElement ) hb_parni( 2 ), hbqt_par_QStyleOption( 3 ), hbqt_par_QPainter( 4 ), hbqt_par_QWidget( 5 ) );
   }
}

/*
 * virtual void drawItemPixmap ( QPainter * painter, const QRect & rectangle, int alignment, const QPixmap & pixmap ) const
 */
HB_FUNC( QT_QSTYLE_DRAWITEMPIXMAP )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->drawItemPixmap( hbqt_par_QPainter( 2 ), *hbqt_par_QRect( 3 ), hb_parni( 4 ), *hbqt_par_QPixmap( 5 ) );
   }
}

/*
 * virtual void drawItemText ( QPainter * painter, const QRect & rectangle, int alignment, const QPalette & palette, bool enabled, const QString & text, QPalette::ColorRole textRole = QPalette::NoRole ) const
 */
HB_FUNC( QT_QSTYLE_DRAWITEMTEXT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawItemText( hbqt_par_QPainter( 2 ), *hbqt_par_QRect( 3 ), hb_parni( 4 ), *hbqt_par_QPalette( 5 ), hb_parl( 6 ), hb_parstr_utf8( 7, &pText, NULL ), ( HB_ISNUM( 8 ) ? ( QPalette::ColorRole ) hb_parni( 8 ) : ( QPalette::ColorRole ) QPalette::NoRole ) );
      hb_strfree( pText );
   }
}

/*
 * virtual void drawPrimitive ( PrimitiveElement element, const QStyleOption * option, QPainter * painter, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_DRAWPRIMITIVE )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->drawPrimitive( ( QStyle::PrimitiveElement ) hb_parni( 2 ), hbqt_par_QStyleOption( 3 ), hbqt_par_QPainter( 4 ), hbqt_par_QWidget( 5 ) );
   }
}

/*
 * virtual QPixmap generatedIconPixmap ( QIcon::Mode iconMode, const QPixmap & pixmap, const QStyleOption * option ) const = 0
 */
HB_FUNC( QT_QSTYLE_GENERATEDICONPIXMAP )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->generatedIconPixmap( ( QIcon::Mode ) hb_parni( 2 ), *hbqt_par_QPixmap( 3 ), hbqt_par_QStyleOption( 4 ) ) ), true ) );
   }
}

/*
 * virtual SubControl hitTestComplexControl ( ComplexControl control, const QStyleOptionComplex * option, const QPoint & position, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_HITTESTCOMPLEXCONTROL )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( QStyle::SubControl ) ( p )->hitTestComplexControl( ( QStyle::ComplexControl ) hb_parni( 2 ), hbqt_par_QStyleOptionComplex( 3 ), *hbqt_par_QPoint( 4 ), hbqt_par_QWidget( 5 ) ) );
   }
}

/*
 * virtual QRect itemPixmapRect ( const QRect & rectangle, int alignment, const QPixmap & pixmap ) const
 */
HB_FUNC( QT_QSTYLE_ITEMPIXMAPRECT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->itemPixmapRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ) ) ), true ) );
   }
}

/*
 * virtual QRect itemTextRect ( const QFontMetrics & metrics, const QRect & rectangle, int alignment, bool enabled, const QString & text ) const
 */
HB_FUNC( QT_QSTYLE_ITEMTEXTRECT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->itemTextRect( *hbqt_par_QFontMetrics( 2 ), *hbqt_par_QRect( 3 ), hb_parni( 4 ), hb_parl( 5 ), hb_parstr_utf8( 6, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * int layoutSpacing ( QSizePolicy::ControlType control1, QSizePolicy::ControlType control2, Qt::Orientation orientation, const QStyleOption * option = 0, const QWidget * widget = 0 ) const
 */
HB_FUNC( QT_QSTYLE_LAYOUTSPACING )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( p )->layoutSpacing( ( QSizePolicy::ControlType ) hb_parni( 2 ), ( QSizePolicy::ControlType ) hb_parni( 3 ), ( Qt::Orientation ) hb_parni( 4 ), hbqt_par_QStyleOption( 5 ), hbqt_par_QWidget( 6 ) ) );
   }
}

/*
 * virtual int pixelMetric ( PixelMetric metric, const QStyleOption * option = 0, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_PIXELMETRIC )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( p )->pixelMetric( ( QStyle::PixelMetric ) hb_parni( 2 ), hbqt_par_QStyleOption( 3 ), hbqt_par_QWidget( 4 ) ) );
   }
}

/*
 * virtual void polish ( QWidget * widget )
 */
HB_FUNC( QT_QSTYLE_POLISH )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->polish( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * virtual void polish ( QApplication * application )
 */
HB_FUNC( QT_QSTYLE_POLISH_1 )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->polish( hbqt_par_QApplication( 2 ) );
   }
}

/*
 * virtual void polish ( QPalette & palette )
 */
HB_FUNC( QT_QSTYLE_POLISH_2 )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->polish( *hbqt_par_QPalette( 2 ) );
   }
}

/*
 * virtual QSize sizeFromContents ( ContentsType type, const QStyleOption * option, const QSize & contentsSize, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_SIZEFROMCONTENTS )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeFromContents( ( QStyle::ContentsType ) hb_parni( 2 ), hbqt_par_QStyleOption( 3 ), *hbqt_par_QSize( 4 ), hbqt_par_QWidget( 5 ) ) ), true ) );
   }
}

/*
 * QIcon standardIcon ( StandardPixmap standardIcon, const QStyleOption * option = 0, const QWidget * widget = 0 ) const
 */
HB_FUNC( QT_QSTYLE_STANDARDICON )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->standardIcon( ( QStyle::StandardPixmap ) hb_parni( 2 ), hbqt_par_QStyleOption( 3 ), hbqt_par_QWidget( 4 ) ) ), true ) );
   }
}

/*
 * virtual QPalette standardPalette () const
 */
HB_FUNC( QT_QSTYLE_STANDARDPALETTE )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->standardPalette() ), true ) );
   }
}

/*
 * virtual int styleHint ( StyleHint hint, const QStyleOption * option = 0, const QWidget * widget = 0, QStyleHintReturn * returnData = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_STYLEHINT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( p )->styleHint( ( QStyle::StyleHint ) hb_parni( 2 ), hbqt_par_QStyleOption( 3 ), hbqt_par_QWidget( 4 ), hbqt_par_QStyleHintReturn( 5 ) ) );
   }
}

/*
 * virtual QRect subControlRect ( ComplexControl control, const QStyleOptionComplex * option, SubControl subControl, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_SUBCONTROLRECT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->subControlRect( ( QStyle::ComplexControl ) hb_parni( 2 ), hbqt_par_QStyleOptionComplex( 3 ), ( QStyle::SubControl ) hb_parni( 4 ), hbqt_par_QWidget( 5 ) ) ), true ) );
   }
}

/*
 * virtual QRect subElementRect ( SubElement element, const QStyleOption * option, const QWidget * widget = 0 ) const = 0
 */
HB_FUNC( QT_QSTYLE_SUBELEMENTRECT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->subElementRect( ( QStyle::SubElement ) hb_parni( 2 ), hbqt_par_QStyleOption( 3 ), hbqt_par_QWidget( 4 ) ) ), true ) );
   }
}

/*
 * virtual void unpolish ( QWidget * widget )
 */
HB_FUNC( QT_QSTYLE_UNPOLISH )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->unpolish( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * virtual void unpolish ( QApplication * application )
 */
HB_FUNC( QT_QSTYLE_UNPOLISH_1 )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      ( p )->unpolish( hbqt_par_QApplication( 2 ) );
   }
}

/*
 * QRect alignedRect ( Qt::LayoutDirection direction, Qt::Alignment alignment, const QSize & size, const QRect & rectangle )
 */
HB_FUNC( QT_QSTYLE_ALIGNEDRECT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->alignedRect( ( Qt::LayoutDirection ) hb_parni( 2 ), ( Qt::Alignment ) hb_parni( 3 ), *hbqt_par_QSize( 4 ), *hbqt_par_QRect( 5 ) ) ), true ) );
   }
}

/*
 * int sliderPositionFromValue ( int min, int max, int logicalValue, int span, bool upsideDown = false )
 */
HB_FUNC( QT_QSTYLE_SLIDERPOSITIONFROMVALUE )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( p )->sliderPositionFromValue( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parl( 6 ) ) );
   }
}

/*
 * int sliderValueFromPosition ( int min, int max, int position, int span, bool upsideDown = false )
 */
HB_FUNC( QT_QSTYLE_SLIDERVALUEFROMPOSITION )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( p )->sliderValueFromPosition( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parl( 6 ) ) );
   }
}

/*
 * Qt::Alignment visualAlignment ( Qt::LayoutDirection direction, Qt::Alignment alignment )
 */
HB_FUNC( QT_QSTYLE_VISUALALIGNMENT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retni( ( Qt::Alignment ) ( p )->visualAlignment( ( Qt::LayoutDirection ) hb_parni( 2 ), ( Qt::Alignment ) hb_parni( 3 ) ) );
   }
}

/*
 * QPoint visualPos ( Qt::LayoutDirection direction, const QRect & boundingRectangle, const QPoint & logicalPosition )
 */
HB_FUNC( QT_QSTYLE_VISUALPOS )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->visualPos( ( Qt::LayoutDirection ) hb_parni( 2 ), *hbqt_par_QRect( 3 ), *hbqt_par_QPoint( 4 ) ) ), true ) );
   }
}

/*
 * QRect visualRect ( Qt::LayoutDirection direction, const QRect & boundingRectangle, const QRect & logicalRectangle )
 */
HB_FUNC( QT_QSTYLE_VISUALRECT )
{
   QStyle * p = hbqt_par_QStyle( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->visualRect( ( Qt::LayoutDirection ) hb_parni( 2 ), *hbqt_par_QRect( 3 ), *hbqt_par_QRect( 4 ) ) ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
