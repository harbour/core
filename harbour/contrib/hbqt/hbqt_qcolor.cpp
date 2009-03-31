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


/*
 *  Constructed[ 40/56 [ 71.43% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  QColor convertTo ( Spec colorSpec ) const
 *  QColor darker ( int factor = 200 ) const
 *  void getCmyk ( int * c, int * m, int * y, int * k, int * a = 0 )
 *  void getCmykF ( qreal * c, qreal * m, qreal * y, qreal * k, qreal * a = 0 )
 *  void getHsv ( int * h, int * s, int * v, int * a = 0 ) const
 *  void getHsvF ( qreal * h, qreal * s, qreal * v, qreal * a = 0 ) const
 *  void getRgb ( int * r, int * g, int * b, int * a = 0 ) const
 *  void getRgbF ( qreal * r, qreal * g, qreal * b, qreal * a = 0 ) const
 *  QColor lighter ( int factor = 150 ) const
 *  QRgb rgb () const
 *  QRgb rgba () const
 *  void setRgb ( QRgb rgb )
 *  void setRgba ( QRgb rgba )
 *  QColor toCmyk () const
 *  QColor toHsv () const
 *  QColor toRgb () const
 */ 


#include <QtGui/QColor>

/*
QColor ()
QColor ( int r, int g, int b, int a = 255 )
QColor ( QRgb color )
QColor ( const QString & name )
QColor ( const char * name )
QColor ( const QColor & color )
QColor ( Qt::GlobalColor color )
 */
HB_FUNC( QT_QCOLOR )
{

}

/*
 * int alpha () const
 */
HB_FUNC( QT_QCOLOR_ALPHA )
{
   hb_retni( hbqt_par_QColor( 1 )->alpha(  ) );
}

/*
 * qreal alphaF () const
 */
HB_FUNC( QT_QCOLOR_ALPHAF )
{
   hb_retnd( hbqt_par_QColor( 1 )->alphaF(  ) );
}

/*
 * int black () const
 */
HB_FUNC( QT_QCOLOR_BLACK )
{
   hb_retni( hbqt_par_QColor( 1 )->black(  ) );
}

/*
 * qreal blackF () const
 */
HB_FUNC( QT_QCOLOR_BLACKF )
{
   hb_retnd( hbqt_par_QColor( 1 )->blackF(  ) );
}

/*
 * int blue () const
 */
HB_FUNC( QT_QCOLOR_BLUE )
{
   hb_retni( hbqt_par_QColor( 1 )->blue(  ) );
}

/*
 * qreal blueF () const
 */
HB_FUNC( QT_QCOLOR_BLUEF )
{
   hb_retnd( hbqt_par_QColor( 1 )->blueF(  ) );
}

/*
 * int cyan () const
 */
HB_FUNC( QT_QCOLOR_CYAN )
{
   hb_retni( hbqt_par_QColor( 1 )->cyan(  ) );
}

/*
 * qreal cyanF () const
 */
HB_FUNC( QT_QCOLOR_CYANF )
{
   hb_retnd( hbqt_par_QColor( 1 )->cyanF(  ) );
}

/*
 * int green () const
 */
HB_FUNC( QT_QCOLOR_GREEN )
{
   hb_retni( hbqt_par_QColor( 1 )->green(  ) );
}

/*
 * qreal greenF () const
 */
HB_FUNC( QT_QCOLOR_GREENF )
{
   hb_retnd( hbqt_par_QColor( 1 )->greenF(  ) );
}

/*
 * int hue () const
 */
HB_FUNC( QT_QCOLOR_HUE )
{
   hb_retni( hbqt_par_QColor( 1 )->hue(  ) );
}

/*
 * qreal hueF () const
 */
HB_FUNC( QT_QCOLOR_HUEF )
{
   hb_retnd( hbqt_par_QColor( 1 )->hueF(  ) );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QCOLOR_ISVALID )
{
   hb_retl( hbqt_par_QColor( 1 )->isValid(  ) );
}

/*
 * int magenta () const
 */
HB_FUNC( QT_QCOLOR_MAGENTA )
{
   hb_retni( hbqt_par_QColor( 1 )->magenta(  ) );
}

/*
 * qreal magentaF () const
 */
HB_FUNC( QT_QCOLOR_MAGENTAF )
{
   hb_retnd( hbqt_par_QColor( 1 )->magentaF(  ) );
}

/*
 * QString name () const
 */
HB_FUNC( QT_QCOLOR_NAME )
{
   hb_retc( hbqt_par_QColor( 1 )->name( ).toLatin1().data() );
}

/*
 * int red () const
 */
HB_FUNC( QT_QCOLOR_RED )
{
   hb_retni( hbqt_par_QColor( 1 )->red(  ) );
}

/*
 * qreal redF () const
 */
HB_FUNC( QT_QCOLOR_REDF )
{
   hb_retnd( hbqt_par_QColor( 1 )->redF(  ) );
}

/*
 * int saturation () const
 */
HB_FUNC( QT_QCOLOR_SATURATION )
{
   hb_retni( hbqt_par_QColor( 1 )->saturation(  ) );
}

/*
 * qreal saturationF () const
 */
HB_FUNC( QT_QCOLOR_SATURATIONF )
{
   hb_retnd( hbqt_par_QColor( 1 )->saturationF(  ) );
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
   hbqt_par_QColor( 1 )->setCmyk( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ) );
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
   hbqt_par_QColor( 1 )->setHsv( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
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
 * void setRgb ( int r, int g, int b, int a = 255 )
 */
HB_FUNC( QT_QCOLOR_SETRGB )
{
   hbqt_par_QColor( 1 )->setRgb( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
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
   hb_retni( hbqt_par_QColor( 1 )->spec(  ) );
}

/*
 * int value () const
 */
HB_FUNC( QT_QCOLOR_VALUE )
{
   hb_retni( hbqt_par_QColor( 1 )->value(  ) );
}

/*
 * qreal valueF () const
 */
HB_FUNC( QT_QCOLOR_VALUEF )
{
   hb_retnd( hbqt_par_QColor( 1 )->valueF(  ) );
}

/*
 * int yellow () const
 */
HB_FUNC( QT_QCOLOR_YELLOW )
{
   hb_retni( hbqt_par_QColor( 1 )->yellow(  ) );
}

/*
 * qreal yellowF () const
 */
HB_FUNC( QT_QCOLOR_YELLOWF )
{
   hb_retnd( hbqt_par_QColor( 1 )->yellowF(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

