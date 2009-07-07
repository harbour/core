/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


#include <QtGui/QColor>

/*
QColor ()
QColor ( int r, int g, int b, int a = 255 )
QColor ( QRgb color )
QColor ( Qt::GlobalColor color )
QColor ( const QString & name )
QColor ( const char * name )
QColor ( const QColor & color )
 */
HB_FUNC( QT_QCOLOR )
{
   if( hb_pcount() >= 1 )
   {
      if( hb_pcount() == 1 && HB_ISNUM( 1 ) && hb_parni( 1 ) < 25 )
      {
         hb_retptr( ( QColor* ) new QColor( ( Qt::GlobalColor ) hb_parni( 1 ) ) );
      }
      else if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
      {
         hb_retptr( ( QColor* ) new QColor( ( QRgb ) hb_parni( 1 ) ) );
      }
      else if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
      {
         hb_retptr( ( QColor* ) new QColor( hbqt_par_QString( 1 ) ) );
      }
      else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
      {
         hb_retptr( ( QColor* ) new QColor( *hbqt_par_QColor( 1 ) ) );
      }
      else if( hb_pcount() == 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      {
         hb_retptr( ( QColor* ) new QColor( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), ( int ) 255 ) );
      }
      else if( hb_pcount() == 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
      {
         hb_retptr( ( QColor* ) new QColor( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
      }
      else
      {
         hb_retptr( ( QColor* ) new QColor() );
      }
   }
   else
   {
      hb_retptr( ( QColor* ) new QColor() );
   }
}

/*
 * int alpha () const
 */
HB_FUNC( QT_QCOLOR_ALPHA )
{
   hb_retni( hbqt_par_QColor( 1 )->alpha() );
}

/*
 * qreal alphaF () const
 */
HB_FUNC( QT_QCOLOR_ALPHAF )
{
   hb_retnd( hbqt_par_QColor( 1 )->alphaF() );
}

/*
 * int black () const
 */
HB_FUNC( QT_QCOLOR_BLACK )
{
   hb_retni( hbqt_par_QColor( 1 )->black() );
}

/*
 * qreal blackF () const
 */
HB_FUNC( QT_QCOLOR_BLACKF )
{
   hb_retnd( hbqt_par_QColor( 1 )->blackF() );
}

/*
 * int blue () const
 */
HB_FUNC( QT_QCOLOR_BLUE )
{
   hb_retni( hbqt_par_QColor( 1 )->blue() );
}

/*
 * qreal blueF () const
 */
HB_FUNC( QT_QCOLOR_BLUEF )
{
   hb_retnd( hbqt_par_QColor( 1 )->blueF() );
}

/*
 * QColor convertTo ( Spec colorSpec ) const
 */
HB_FUNC( QT_QCOLOR_CONVERTTO )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->convertTo( ( QColor::Spec ) hb_parni( 2 ) ) ) );
}

/*
 * int cyan () const
 */
HB_FUNC( QT_QCOLOR_CYAN )
{
   hb_retni( hbqt_par_QColor( 1 )->cyan() );
}

/*
 * qreal cyanF () const
 */
HB_FUNC( QT_QCOLOR_CYANF )
{
   hb_retnd( hbqt_par_QColor( 1 )->cyanF() );
}

/*
 * QColor darker ( int factor = 200 ) const
 */
HB_FUNC( QT_QCOLOR_DARKER )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->darker( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 200 ) ) ) );
}

/*
 * void getCmyk ( int * c, int * m, int * y, int * k, int * a = 0 )
 */
HB_FUNC( QT_QCOLOR_GETCMYK )
{
   int iC = 0;
   int iM = 0;
   int iY = 0;
   int iK = 0;
   int iA = 0;

   hbqt_par_QColor( 1 )->getCmyk( &iC, &iM, &iY, &iK, &iA );

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
   qreal qrC = 0;
   qreal qrM = 0;
   qreal qrY = 0;
   qreal qrK = 0;
   qreal qrA = 0;

   hbqt_par_QColor( 1 )->getCmykF( &qrC, &qrM, &qrY, &qrK, &qrA );

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
   int iH = 0;
   int iS = 0;
   int iV = 0;
   int iA = 0;

   hbqt_par_QColor( 1 )->getHsv( &iH, &iS, &iV, &iA );

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
   qreal qrH = 0;
   qreal qrS = 0;
   qreal qrV = 0;
   qreal qrA = 0;

   hbqt_par_QColor( 1 )->getHsvF( &qrH, &qrS, &qrV, &qrA );

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
   int iR = 0;
   int iG = 0;
   int iB = 0;
   int iA = 0;

   hbqt_par_QColor( 1 )->getRgb( &iR, &iG, &iB, &iA );

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
   qreal qrR = 0;
   qreal qrG = 0;
   qreal qrB = 0;
   qreal qrA = 0;

   hbqt_par_QColor( 1 )->getRgbF( &qrR, &qrG, &qrB, &qrA );

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
   hb_retni( hbqt_par_QColor( 1 )->green() );
}

/*
 * qreal greenF () const
 */
HB_FUNC( QT_QCOLOR_GREENF )
{
   hb_retnd( hbqt_par_QColor( 1 )->greenF() );
}

/*
 * int hue () const
 */
HB_FUNC( QT_QCOLOR_HUE )
{
   hb_retni( hbqt_par_QColor( 1 )->hue() );
}

/*
 * qreal hueF () const
 */
HB_FUNC( QT_QCOLOR_HUEF )
{
   hb_retnd( hbqt_par_QColor( 1 )->hueF() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QCOLOR_ISVALID )
{
   hb_retl( hbqt_par_QColor( 1 )->isValid() );
}

/*
 * QColor lighter ( int factor = 150 ) const
 */
HB_FUNC( QT_QCOLOR_LIGHTER )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->lighter( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 150 ) ) ) );
}

/*
 * int magenta () const
 */
HB_FUNC( QT_QCOLOR_MAGENTA )
{
   hb_retni( hbqt_par_QColor( 1 )->magenta() );
}

/*
 * qreal magentaF () const
 */
HB_FUNC( QT_QCOLOR_MAGENTAF )
{
   hb_retnd( hbqt_par_QColor( 1 )->magentaF() );
}

/*
 * QString name () const
 */
HB_FUNC( QT_QCOLOR_NAME )
{
   hb_retc( hbqt_par_QColor( 1 )->name().toLatin1().data() );
}

/*
 * int red () const
 */
HB_FUNC( QT_QCOLOR_RED )
{
   hb_retni( hbqt_par_QColor( 1 )->red() );
}

/*
 * qreal redF () const
 */
HB_FUNC( QT_QCOLOR_REDF )
{
   hb_retnd( hbqt_par_QColor( 1 )->redF() );
}

/*
 * QRgb rgb () const
 */
HB_FUNC( QT_QCOLOR_RGB )
{
   hb_retnl( hbqt_par_QColor( 1 )->rgb() );
}

/*
 * QRgb rgba () const
 */
HB_FUNC( QT_QCOLOR_RGBA )
{
   hb_retnl( hbqt_par_QColor( 1 )->rgba() );
}

/*
 * int saturation () const
 */
HB_FUNC( QT_QCOLOR_SATURATION )
{
   hb_retni( hbqt_par_QColor( 1 )->saturation() );
}

/*
 * qreal saturationF () const
 */
HB_FUNC( QT_QCOLOR_SATURATIONF )
{
   hb_retnd( hbqt_par_QColor( 1 )->saturationF() );
}

/*
 * void setAlpha ( int alpha )
 */
HB_FUNC( QT_QCOLOR_SETALPHA )
{
   hbqt_par_QColor( 1 )->setAlpha( hb_parni( 2 ) );
}

/*
 * void setAlphaF ( qreal alpha )
 */
HB_FUNC( QT_QCOLOR_SETALPHAF )
{
   hbqt_par_QColor( 1 )->setAlphaF( hb_parnd( 2 ) );
}

/*
 * void setBlue ( int blue )
 */
HB_FUNC( QT_QCOLOR_SETBLUE )
{
   hbqt_par_QColor( 1 )->setBlue( hb_parni( 2 ) );
}

/*
 * void setBlueF ( qreal blue )
 */
HB_FUNC( QT_QCOLOR_SETBLUEF )
{
   hbqt_par_QColor( 1 )->setBlueF( hb_parnd( 2 ) );
}

/*
 * void setCmyk ( int c, int m, int y, int k, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_SETCMYK )
{
   hbqt_par_QColor( 1 )->setCmyk( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? hb_parni( 6 ) : 255 ) );
}

/*
 * void setCmykF ( qreal c, qreal m, qreal y, qreal k, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_SETCMYKF )
{
   hbqt_par_QColor( 1 )->setCmykF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
}

/*
 * void setGreen ( int green )
 */
HB_FUNC( QT_QCOLOR_SETGREEN )
{
   hbqt_par_QColor( 1 )->setGreen( hb_parni( 2 ) );
}

/*
 * void setGreenF ( qreal green )
 */
HB_FUNC( QT_QCOLOR_SETGREENF )
{
   hbqt_par_QColor( 1 )->setGreenF( hb_parnd( 2 ) );
}

/*
 * void setHsv ( int h, int s, int v, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_SETHSV )
{
   hbqt_par_QColor( 1 )->setHsv( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HB_ISNUM( 5 ) ? hb_parni( 5 ) : 255 ) );
}

/*
 * void setHsvF ( qreal h, qreal s, qreal v, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_SETHSVF )
{
   hbqt_par_QColor( 1 )->setHsvF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * void setNamedColor ( const QString & name )
 */
HB_FUNC( QT_QCOLOR_SETNAMEDCOLOR )
{
   hbqt_par_QColor( 1 )->setNamedColor( hbqt_par_QString( 2 ) );
}

/*
 * void setRed ( int red )
 */
HB_FUNC( QT_QCOLOR_SETRED )
{
   hbqt_par_QColor( 1 )->setRed( hb_parni( 2 ) );
}

/*
 * void setRedF ( qreal red )
 */
HB_FUNC( QT_QCOLOR_SETREDF )
{
   hbqt_par_QColor( 1 )->setRedF( hb_parnd( 2 ) );
}

/*
 * void setRgb ( QRgb rgb )
 */
HB_FUNC( QT_QCOLOR_SETRGB )
{
   hbqt_par_QColor( 1 )->setRgb( hb_parnl( 2 ) );
}

/*
 * void setRgb ( int r, int g, int b, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_SETRGB_1 )
{
   hbqt_par_QColor( 1 )->setRgb( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HB_ISNUM( 5 ) ? hb_parni( 5 ) : 255 ) );
}

/*
 * void setRgba ( QRgb rgba )
 */
HB_FUNC( QT_QCOLOR_SETRGBA )
{
   hbqt_par_QColor( 1 )->setRgba( hb_parnl( 2 ) );
}

/*
 * void setRgbF ( qreal r, qreal g, qreal b, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_SETRGBF )
{
   hbqt_par_QColor( 1 )->setRgbF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * Spec spec () const
 */
HB_FUNC( QT_QCOLOR_SPEC )
{
   hb_retni( ( QColor::Spec ) hbqt_par_QColor( 1 )->spec() );
}

/*
 * QColor toCmyk () const
 */
HB_FUNC( QT_QCOLOR_TOCMYK )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->toCmyk() ) );
}

/*
 * QColor toHsv () const
 */
HB_FUNC( QT_QCOLOR_TOHSV )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->toHsv() ) );
}

/*
 * QColor toRgb () const
 */
HB_FUNC( QT_QCOLOR_TORGB )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->toRgb() ) );
}

/*
 * int value () const
 */
HB_FUNC( QT_QCOLOR_VALUE )
{
   hb_retni( hbqt_par_QColor( 1 )->value() );
}

/*
 * qreal valueF () const
 */
HB_FUNC( QT_QCOLOR_VALUEF )
{
   hb_retnd( hbqt_par_QColor( 1 )->valueF() );
}

/*
 * int yellow () const
 */
HB_FUNC( QT_QCOLOR_YELLOW )
{
   hb_retni( hbqt_par_QColor( 1 )->yellow() );
}

/*
 * qreal yellowF () const
 */
HB_FUNC( QT_QCOLOR_YELLOWF )
{
   hb_retnd( hbqt_par_QColor( 1 )->yellowF() );
}

/*
 * QStringList colorNames ()
 */
HB_FUNC( QT_QCOLOR_COLORNAMES )
{
   hb_retptr( new QStringList( hbqt_par_QColor( 1 )->colorNames() ) );
}

/*
 * QColor fromCmyk ( int c, int m, int y, int k, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_FROMCMYK )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromCmyk( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? hb_parni( 6 ) : 255 ) ) ) );
}

/*
 * QColor fromCmykF ( qreal c, qreal m, qreal y, qreal k, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_FROMCMYKF )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromCmykF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ) );
}

/*
 * QColor fromHsv ( int h, int s, int v, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_FROMHSV )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromHsv( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HB_ISNUM( 5 ) ? hb_parni( 5 ) : 255 ) ) ) );
}

/*
 * QColor fromHsvF ( qreal h, qreal s, qreal v, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_FROMHSVF )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromHsvF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ) );
}

/*
 * QColor fromRgb ( QRgb rgb )
 */
HB_FUNC( QT_QCOLOR_FROMRGB )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromRgb( hb_parnl( 2 ) ) ) );
}

/*
 * QColor fromRgb ( int r, int g, int b, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_FROMRGB_1 )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromRgb( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HB_ISNUM( 5 ) ? hb_parni( 5 ) : 255 ) ) ) );
}

/*
 * QColor fromRgbF ( qreal r, qreal g, qreal b, qreal a = 1.0 )
 */
HB_FUNC( QT_QCOLOR_FROMRGBF )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromRgbF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ) );
}

/*
 * QColor fromRgba ( QRgb rgba )
 */
HB_FUNC( QT_QCOLOR_FROMRGBA )
{
   hb_retptr( new QColor( hbqt_par_QColor( 1 )->fromRgba( hb_parnl( 2 ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

