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
 *  enum Spec { Rgb, Hsv, Cmyk, Invalid }
 */

/*
 *  Constructed[ 71/71 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QColor ( const char * name )
 *  //bool allowX11ColorNames ()
 *  //void setAllowX11ColorNames ( bool enabled )
 */

#include <QtCore/QPointer>

#include <QtGui/QColor>

/*
QColor ()
QColor ( int r, int g, int b, int a = 255 )
QColor ( QRgb color )
QColor ( Qt::GlobalColor color )
QColor ( const QString & name )
QColor ( const char * name )
QColor ( const QColor & color )
~QColor ()
 */

typedef struct
{
   QColor * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QColor;

HBQT_GC_FUNC( hbqt_gcRelease_QColor )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QColor   /.\\", p->ph ) );
         delete ( ( QColor * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QColor   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QColor    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QColor    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QColor( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QColor * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QColor;
   p->type = HBQT_TYPE_QColor;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QColor", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QColor", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QCOLOR )
{
   QColor * pObj = NULL;

   if( hb_pcount() >= 1 )
   {
      if( hb_pcount() == 1 && HB_ISNUM( 1 ) && hb_parni( 1 ) < 25 )
      {
         pObj = new QColor( ( Qt::GlobalColor ) hb_parni( 1 ) ) ;
      }
      else if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
      {
         pObj = new QColor( ( QRgb ) hb_parni( 1 ) ) ;
      }
      else if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
      {
         pObj = new QColor( hbqt_par_QString( 1 ) ) ;
      }
      else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
      {
         pObj = new QColor( *hbqt_par_QColor( 1 ) ) ;
      }
      else if( hb_pcount() == 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      {
         pObj = new QColor( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), ( int ) 255 ) ;
      }
      else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
      {
         pObj = new QColor( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ;
      }
      else
      {
         pObj = new QColor() ;
      }
   }
   else
   {
      pObj = new QColor() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QColor( ( void * ) pObj, true ) );
}

/*
 * QColor ()
 */
HB_FUNC( QT_QCOLOR_QCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor(), true ) );
}

/*
 * QColor ( int r, int g, int b, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_QCOLOR_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) ), true ) );
}

/*
 * QColor ( QRgb color )
 */
HB_FUNC( QT_QCOLOR_QCOLOR_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hb_parnl( 2 ) ), true ) );
}

/*
 * QColor ( Qt::GlobalColor color )
 */
HB_FUNC( QT_QCOLOR_QCOLOR_3 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( Qt::GlobalColor ) hb_parni( 2 ) ), true ) );
}

/*
 * QColor ( const QString & name )
 */
HB_FUNC( QT_QCOLOR_QCOLOR_4 )
{
      void * pText;
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hb_parstr_utf8( 2, &pText, NULL ) ), true ) );
      hb_strfree( pText );
}

/*
 * QColor ( const QColor & color )
 */
HB_FUNC( QT_QCOLOR_QCOLOR_5 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( *hbqt_par_QColor( 2 ) ), true ) );
}

/*
 * int alpha () const
 */
HB_FUNC( QT_QCOLOR_ALPHA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->alpha() );
   }
}

/*
 * qreal alphaF () const
 */
HB_FUNC( QT_QCOLOR_ALPHAF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->alphaF() );
   }
}

/*
 * int black () const
 */
HB_FUNC( QT_QCOLOR_BLACK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->black() );
   }
}

/*
 * qreal blackF () const
 */
HB_FUNC( QT_QCOLOR_BLACKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->blackF() );
   }
}

/*
 * int blue () const
 */
HB_FUNC( QT_QCOLOR_BLUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->blue() );
   }
}

/*
 * qreal blueF () const
 */
HB_FUNC( QT_QCOLOR_BLUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->blueF() );
   }
}

/*
 * QColor convertTo ( Spec colorSpec ) const
 */
HB_FUNC( QT_QCOLOR_CONVERTTO )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->convertTo( ( QColor::Spec ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * int cyan () const
 */
HB_FUNC( QT_QCOLOR_CYAN )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->cyan() );
   }
}

/*
 * qreal cyanF () const
 */
HB_FUNC( QT_QCOLOR_CYANF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->cyanF() );
   }
}

/*
 * QColor darker ( int factor = 200 ) const
 */
HB_FUNC( QT_QCOLOR_DARKER )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->darker( hb_parnidef( 2, 200 ) ) ), true ) );
   }
}

/*
 * void getCmyk ( int * c, int * m, int * y, int * k, int * a = 0 )
 */
HB_FUNC( QT_QCOLOR_GETCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   int iC = 0;
   int iM = 0;
   int iY = 0;
   int iK = 0;
   int iA = 0;

   if( p )
   {
      ( p )->getCmyk( &iC, &iM, &iY, &iK, &iA );
   }

   hb_storni( iC, 2 );
   hb_storni( iM, 3 );
   hb_storni( iY, 4 );
   hb_storni( iK, 5 );
   hb_storni( iA, 6 );
}

/*
 * void getCmykF ( qreal * c, qreal * m, qreal * y, qreal * k, qreal * a = 0 )
 */
HB_FUNC( QT_QCOLOR_GETCMYKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   qreal qrC = 0;
   qreal qrM = 0;
   qreal qrY = 0;
   qreal qrK = 0;
   qreal qrA = 0;

   if( p )
   {
      ( p )->getCmykF( &qrC, &qrM, &qrY, &qrK, &qrA );
   }

   hb_stornd( qrC, 2 );
   hb_stornd( qrM, 3 );
   hb_stornd( qrY, 4 );
   hb_stornd( qrK, 5 );
   hb_stornd( qrA, 6 );
}

/*
 * void getHsv ( int * h, int * s, int * v, int * a = 0 ) const
 */
HB_FUNC( QT_QCOLOR_GETHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   int iH = 0;
   int iS = 0;
   int iV = 0;
   int iA = 0;

   if( p )
   {
      ( p )->getHsv( &iH, &iS, &iV, &iA );
   }

   hb_storni( iH, 2 );
   hb_storni( iS, 3 );
   hb_storni( iV, 4 );
   hb_storni( iA, 5 );
}

/*
 * void getHsvF ( qreal * h, qreal * s, qreal * v, qreal * a = 0 ) const
 */
HB_FUNC( QT_QCOLOR_GETHSVF )
{
   QColor * p = hbqt_par_QColor( 1 );
   qreal qrH = 0;
   qreal qrS = 0;
   qreal qrV = 0;
   qreal qrA = 0;

   if( p )
   {
      ( p )->getHsvF( &qrH, &qrS, &qrV, &qrA );
   }

   hb_stornd( qrH, 2 );
   hb_stornd( qrS, 3 );
   hb_stornd( qrV, 4 );
   hb_stornd( qrA, 5 );
}

/*
 * void getRgb ( int * r, int * g, int * b, int * a = 0 ) const
 */
HB_FUNC( QT_QCOLOR_GETRGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   int iR = 0;
   int iG = 0;
   int iB = 0;
   int iA = 0;

   if( p )
   {
      ( p )->getRgb( &iR, &iG, &iB, &iA );
   }

   hb_storni( iR, 2 );
   hb_storni( iG, 3 );
   hb_storni( iB, 4 );
   hb_storni( iA, 5 );
}

/*
 * void getRgbF ( qreal * r, qreal * g, qreal * b, qreal * a = 0 ) const
 */
HB_FUNC( QT_QCOLOR_GETRGBF )
{
   QColor * p = hbqt_par_QColor( 1 );
   qreal qrR = 0;
   qreal qrG = 0;
   qreal qrB = 0;
   qreal qrA = 0;

   if( p )
   {
      ( p )->getRgbF( &qrR, &qrG, &qrB, &qrA );
   }

   hb_stornd( qrR, 2 );
   hb_stornd( qrG, 3 );
   hb_stornd( qrB, 4 );
   hb_stornd( qrA, 5 );
}

/*
 * int green () const
 */
HB_FUNC( QT_QCOLOR_GREEN )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->green() );
   }
}

/*
 * qreal greenF () const
 */
HB_FUNC( QT_QCOLOR_GREENF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->greenF() );
   }
}

/*
 * int hue () const
 */
HB_FUNC( QT_QCOLOR_HUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->hue() );
   }
}

/*
 * qreal hueF () const
 */
HB_FUNC( QT_QCOLOR_HUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->hueF() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QCOLOR_ISVALID )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * QColor lighter ( int factor = 150 ) const
 */
HB_FUNC( QT_QCOLOR_LIGHTER )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->lighter( hb_parnidef( 2, 150 ) ) ), true ) );
   }
}

/*
 * int magenta () const
 */
HB_FUNC( QT_QCOLOR_MAGENTA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->magenta() );
   }
}

/*
 * qreal magentaF () const
 */
HB_FUNC( QT_QCOLOR_MAGENTAF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->magentaF() );
   }
}

/*
 * QString name () const
 */
HB_FUNC( QT_QCOLOR_NAME )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->name().toUtf8().data() );
   }
}

/*
 * int red () const
 */
HB_FUNC( QT_QCOLOR_RED )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->red() );
   }
}

/*
 * qreal redF () const
 */
HB_FUNC( QT_QCOLOR_REDF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->redF() );
   }
}

/*
 * QRgb rgb () const
 */
HB_FUNC( QT_QCOLOR_RGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnl( ( p )->rgb() );
   }
}

/*
 * QRgb rgba () const
 */
HB_FUNC( QT_QCOLOR_RGBA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnl( ( p )->rgba() );
   }
}

/*
 * int saturation () const
 */
HB_FUNC( QT_QCOLOR_SATURATION )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->saturation() );
   }
}

/*
 * qreal saturationF () const
 */
HB_FUNC( QT_QCOLOR_SATURATIONF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->saturationF() );
   }
}

/*
 * void setAlpha ( int alpha )
 */
HB_FUNC( QT_QCOLOR_SETALPHA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setAlpha( hb_parni( 2 ) );
   }
}

/*
 * void setAlphaF ( qreal alpha )
 */
HB_FUNC( QT_QCOLOR_SETALPHAF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setAlphaF( hb_parnd( 2 ) );
   }
}

/*
 * void setBlue ( int blue )
 */
HB_FUNC( QT_QCOLOR_SETBLUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setBlue( hb_parni( 2 ) );
   }
}

/*
 * void setBlueF ( qreal blue )
 */
HB_FUNC( QT_QCOLOR_SETBLUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setBlueF( hb_parnd( 2 ) );
   }
}

/*
 * void setCmyk ( int c, int m, int y, int k, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_SETCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setCmyk( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnidef( 6, 255 ) );
   }
}

/*
 * void setCmykF ( qreal c, qreal m, qreal y, qreal k, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_SETCMYKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setCmykF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
   }
}

/*
 * void setGreen ( int green )
 */
HB_FUNC( QT_QCOLOR_SETGREEN )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setGreen( hb_parni( 2 ) );
   }
}

/*
 * void setGreenF ( qreal green )
 */
HB_FUNC( QT_QCOLOR_SETGREENF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setGreenF( hb_parnd( 2 ) );
   }
}

/*
 * void setHsv ( int h, int s, int v, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_SETHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setHsv( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) );
   }
}

/*
 * void setHsvF ( qreal h, qreal s, qreal v, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_SETHSVF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setHsvF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   }
}

/*
 * void setNamedColor ( const QString & name )
 */
HB_FUNC( QT_QCOLOR_SETNAMEDCOLOR )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      void * pText;
      ( p )->setNamedColor( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setRed ( int red )
 */
HB_FUNC( QT_QCOLOR_SETRED )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setRed( hb_parni( 2 ) );
   }
}

/*
 * void setRedF ( qreal red )
 */
HB_FUNC( QT_QCOLOR_SETREDF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setRedF( hb_parnd( 2 ) );
   }
}

/*
 * void setRgb ( QRgb rgb )
 */
HB_FUNC( QT_QCOLOR_SETRGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setRgb( hb_parnl( 2 ) );
   }
}

/*
 * void setRgb ( int r, int g, int b, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_SETRGB_1 )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setRgb( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) );
   }
}

/*
 * void setRgba ( QRgb rgba )
 */
HB_FUNC( QT_QCOLOR_SETRGBA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setRgba( hb_parnl( 2 ) );
   }
}

/*
 * void setRgbF ( qreal r, qreal g, qreal b, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_SETRGBF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      ( p )->setRgbF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   }
}

/*
 * Spec spec () const
 */
HB_FUNC( QT_QCOLOR_SPEC )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( QColor::Spec ) ( p )->spec() );
   }
}

/*
 * QColor toCmyk () const
 */
HB_FUNC( QT_QCOLOR_TOCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->toCmyk() ), true ) );
   }
}

/*
 * QColor toHsv () const
 */
HB_FUNC( QT_QCOLOR_TOHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->toHsv() ), true ) );
   }
}

/*
 * QColor toRgb () const
 */
HB_FUNC( QT_QCOLOR_TORGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->toRgb() ), true ) );
   }
}

/*
 * int value () const
 */
HB_FUNC( QT_QCOLOR_VALUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->value() );
   }
}

/*
 * qreal valueF () const
 */
HB_FUNC( QT_QCOLOR_VALUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->valueF() );
   }
}

/*
 * int yellow () const
 */
HB_FUNC( QT_QCOLOR_YELLOW )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retni( ( p )->yellow() );
   }
}

/*
 * qreal yellowF () const
 */
HB_FUNC( QT_QCOLOR_YELLOWF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retnd( ( p )->yellowF() );
   }
}

/*
 * QStringList colorNames ()
 */
HB_FUNC( QT_QCOLOR_COLORNAMES )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->colorNames() ), true ) );
   }
}

/*
 * QColor fromCmyk ( int c, int m, int y, int k, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_FROMCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromCmyk( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnidef( 6, 255 ) ) ), true ) );
   }
}

/*
 * QColor fromCmykF ( qreal c, qreal m, qreal y, qreal k, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_FROMCMYKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromCmykF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
   }
}

/*
 * QColor fromHsv ( int h, int s, int v, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_FROMHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromHsv( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) ) ), true ) );
   }
}

/*
 * QColor fromHsvF ( qreal h, qreal s, qreal v, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_FROMHSVF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromHsvF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   }
}

/*
 * QColor fromRgb ( QRgb rgb )
 */
HB_FUNC( QT_QCOLOR_FROMRGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgb( hb_parnl( 2 ) ) ), true ) );
   }
}

/*
 * QColor fromRgb ( int r, int g, int b, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_FROMRGB_1 )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgb( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) ) ), true ) );
   }
}

/*
 * QColor fromRgbF ( qreal r, qreal g, qreal b, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_FROMRGBF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgbF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   }
}

/*
 * QColor fromRgba ( QRgb rgba )
 */
HB_FUNC( QT_QCOLOR_FROMRGBA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgba( hb_parnl( 2 ) ) ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
