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
         delete ( ( QColor * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QColor( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QColor * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QColor;
   p->type = HBQT_TYPE_QColor;

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

/* QColor () */
HB_FUNC( QT_QCOLOR_QCOLOR )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor(), true ) );
}

/* QColor ( int r, int g, int b, int a = 255 ) */
HB_FUNC( QT_QCOLOR_QCOLOR_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) ), true ) );
}

/* QColor ( QRgb color ) */
HB_FUNC( QT_QCOLOR_QCOLOR_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hb_parnl( 2 ) ), true ) );
}

/* QColor ( Qt::GlobalColor color ) */
HB_FUNC( QT_QCOLOR_QCOLOR_3 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( Qt::GlobalColor ) hb_parni( 2 ) ), true ) );
}

/* QColor ( const QString & name ) */
HB_FUNC( QT_QCOLOR_QCOLOR_4 )
{
      void * pText;
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hb_parstr_utf8( 2, &pText, NULL ) ), true ) );
      hb_strfree( pText );
}

/* QColor ( const QColor & color ) */
HB_FUNC( QT_QCOLOR_QCOLOR_5 )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( *hbqt_par_QColor( 2 ) ), true ) );
}

/* int alpha () const */
HB_FUNC( QT_QCOLOR_ALPHA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->alpha() );
}

/* qreal alphaF () const */
HB_FUNC( QT_QCOLOR_ALPHAF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->alphaF() );
}

/* int black () const */
HB_FUNC( QT_QCOLOR_BLACK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->black() );
}

/* qreal blackF () const */
HB_FUNC( QT_QCOLOR_BLACKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->blackF() );
}

/* int blue () const */
HB_FUNC( QT_QCOLOR_BLUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->blue() );
}

/* qreal blueF () const */
HB_FUNC( QT_QCOLOR_BLUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->blueF() );
}

/* QColor convertTo ( Spec colorSpec ) const */
HB_FUNC( QT_QCOLOR_CONVERTTO )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->convertTo( ( QColor::Spec ) hb_parni( 2 ) ) ), true ) );
}

/* int cyan () const */
HB_FUNC( QT_QCOLOR_CYAN )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->cyan() );
}

/* qreal cyanF () const */
HB_FUNC( QT_QCOLOR_CYANF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->cyanF() );
}

/* QColor darker ( int factor = 200 ) const */
HB_FUNC( QT_QCOLOR_DARKER )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->darker( hb_parnidef( 2, 200 ) ) ), true ) );
}

/* void getCmyk ( int * c, int * m, int * y, int * k, int * a = 0 ) */
HB_FUNC( QT_QCOLOR_GETCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   int iC = 0;
   int iM = 0;
   int iY = 0;
   int iK = 0;
   int iA = 0;

   if( p )
      ( p )->getCmyk( &iC, &iM, &iY, &iK, &iA );

   hb_storni( iC, 2 );
   hb_storni( iM, 3 );
   hb_storni( iY, 4 );
   hb_storni( iK, 5 );
   hb_storni( iA, 6 );
}

/* void getCmykF ( qreal * c, qreal * m, qreal * y, qreal * k, qreal * a = 0 ) */
HB_FUNC( QT_QCOLOR_GETCMYKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   qreal qrC = 0;
   qreal qrM = 0;
   qreal qrY = 0;
   qreal qrK = 0;
   qreal qrA = 0;

   if( p )
      ( p )->getCmykF( &qrC, &qrM, &qrY, &qrK, &qrA );

   hb_stornd( qrC, 2 );
   hb_stornd( qrM, 3 );
   hb_stornd( qrY, 4 );
   hb_stornd( qrK, 5 );
   hb_stornd( qrA, 6 );
}

/* void getHsv ( int * h, int * s, int * v, int * a = 0 ) const */
HB_FUNC( QT_QCOLOR_GETHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   int iH = 0;
   int iS = 0;
   int iV = 0;
   int iA = 0;

   if( p )
      ( p )->getHsv( &iH, &iS, &iV, &iA );

   hb_storni( iH, 2 );
   hb_storni( iS, 3 );
   hb_storni( iV, 4 );
   hb_storni( iA, 5 );
}

/* void getHsvF ( qreal * h, qreal * s, qreal * v, qreal * a = 0 ) const */
HB_FUNC( QT_QCOLOR_GETHSVF )
{
   QColor * p = hbqt_par_QColor( 1 );
   qreal qrH = 0;
   qreal qrS = 0;
   qreal qrV = 0;
   qreal qrA = 0;

   if( p )
      ( p )->getHsvF( &qrH, &qrS, &qrV, &qrA );

   hb_stornd( qrH, 2 );
   hb_stornd( qrS, 3 );
   hb_stornd( qrV, 4 );
   hb_stornd( qrA, 5 );
}

/* void getRgb ( int * r, int * g, int * b, int * a = 0 ) const */
HB_FUNC( QT_QCOLOR_GETRGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   int iR = 0;
   int iG = 0;
   int iB = 0;
   int iA = 0;

   if( p )
      ( p )->getRgb( &iR, &iG, &iB, &iA );

   hb_storni( iR, 2 );
   hb_storni( iG, 3 );
   hb_storni( iB, 4 );
   hb_storni( iA, 5 );
}

/* void getRgbF ( qreal * r, qreal * g, qreal * b, qreal * a = 0 ) const */
HB_FUNC( QT_QCOLOR_GETRGBF )
{
   QColor * p = hbqt_par_QColor( 1 );
   qreal qrR = 0;
   qreal qrG = 0;
   qreal qrB = 0;
   qreal qrA = 0;

   if( p )
      ( p )->getRgbF( &qrR, &qrG, &qrB, &qrA );

   hb_stornd( qrR, 2 );
   hb_stornd( qrG, 3 );
   hb_stornd( qrB, 4 );
   hb_stornd( qrA, 5 );
}

/* int green () const */
HB_FUNC( QT_QCOLOR_GREEN )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->green() );
}

/* qreal greenF () const */
HB_FUNC( QT_QCOLOR_GREENF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->greenF() );
}

/* int hue () const */
HB_FUNC( QT_QCOLOR_HUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->hue() );
}

/* qreal hueF () const */
HB_FUNC( QT_QCOLOR_HUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->hueF() );
}

/* bool isValid () const */
HB_FUNC( QT_QCOLOR_ISVALID )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* QColor lighter ( int factor = 150 ) const */
HB_FUNC( QT_QCOLOR_LIGHTER )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->lighter( hb_parnidef( 2, 150 ) ) ), true ) );
}

/* int magenta () const */
HB_FUNC( QT_QCOLOR_MAGENTA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->magenta() );
}

/* qreal magentaF () const */
HB_FUNC( QT_QCOLOR_MAGENTAF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->magentaF() );
}

/* QString name () const */
HB_FUNC( QT_QCOLOR_NAME )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retstr_utf8( ( p )->name().toUtf8().data() );
}

/* int red () const */
HB_FUNC( QT_QCOLOR_RED )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->red() );
}

/* qreal redF () const */
HB_FUNC( QT_QCOLOR_REDF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->redF() );
}

/* QRgb rgb () const */
HB_FUNC( QT_QCOLOR_RGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnl( ( p )->rgb() );
}

/* QRgb rgba () const */
HB_FUNC( QT_QCOLOR_RGBA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnl( ( p )->rgba() );
}

/* int saturation () const */
HB_FUNC( QT_QCOLOR_SATURATION )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->saturation() );
}

/* qreal saturationF () const */
HB_FUNC( QT_QCOLOR_SATURATIONF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->saturationF() );
}

/* void setAlpha ( int alpha ) */
HB_FUNC( QT_QCOLOR_SETALPHA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setAlpha( hb_parni( 2 ) );
}

/* void setAlphaF ( qreal alpha ) */
HB_FUNC( QT_QCOLOR_SETALPHAF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setAlphaF( hb_parnd( 2 ) );
}

/* void setBlue ( int blue ) */
HB_FUNC( QT_QCOLOR_SETBLUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setBlue( hb_parni( 2 ) );
}

/* void setBlueF ( qreal blue ) */
HB_FUNC( QT_QCOLOR_SETBLUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setBlueF( hb_parnd( 2 ) );
}

/* void setCmyk ( int c, int m, int y, int k, int a = 255 ) */
HB_FUNC( QT_QCOLOR_SETCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setCmyk( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnidef( 6, 255 ) );
}

/* void setCmykF ( qreal c, qreal m, qreal y, qreal k, qreal a = 1.0 ) */
HB_FUNC( QT_QCOLOR_SETCMYKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setCmykF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
}

/* void setGreen ( int green ) */
HB_FUNC( QT_QCOLOR_SETGREEN )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setGreen( hb_parni( 2 ) );
}

/* void setGreenF ( qreal green ) */
HB_FUNC( QT_QCOLOR_SETGREENF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setGreenF( hb_parnd( 2 ) );
}

/* void setHsv ( int h, int s, int v, int a = 255 ) */
HB_FUNC( QT_QCOLOR_SETHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setHsv( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) );
}

/* void setHsvF ( qreal h, qreal s, qreal v, qreal a = 1.0 ) */
HB_FUNC( QT_QCOLOR_SETHSVF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setHsvF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* void setNamedColor ( const QString & name ) */
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

/* void setRed ( int red ) */
HB_FUNC( QT_QCOLOR_SETRED )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setRed( hb_parni( 2 ) );
}

/* void setRedF ( qreal red ) */
HB_FUNC( QT_QCOLOR_SETREDF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setRedF( hb_parnd( 2 ) );
}

/* void setRgb ( QRgb rgb ) */
HB_FUNC( QT_QCOLOR_SETRGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setRgb( hb_parnl( 2 ) );
}

/* void setRgb ( int r, int g, int b, int a = 255 ) */
HB_FUNC( QT_QCOLOR_SETRGB_1 )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setRgb( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) );
}

/* void setRgba ( QRgb rgba ) */
HB_FUNC( QT_QCOLOR_SETRGBA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setRgba( hb_parnl( 2 ) );
}

/* void setRgbF ( qreal r, qreal g, qreal b, qreal a = 1.0 ) */
HB_FUNC( QT_QCOLOR_SETRGBF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      ( p )->setRgbF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* Spec spec () const */
HB_FUNC( QT_QCOLOR_SPEC )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( QColor::Spec ) ( p )->spec() );
}

/* QColor toCmyk () const */
HB_FUNC( QT_QCOLOR_TOCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->toCmyk() ), true ) );
}

/* QColor toHsv () const */
HB_FUNC( QT_QCOLOR_TOHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->toHsv() ), true ) );
}

/* QColor toRgb () const */
HB_FUNC( QT_QCOLOR_TORGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->toRgb() ), true ) );
}

/* int value () const */
HB_FUNC( QT_QCOLOR_VALUE )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->value() );
}

/* qreal valueF () const */
HB_FUNC( QT_QCOLOR_VALUEF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->valueF() );
}

/* int yellow () const */
HB_FUNC( QT_QCOLOR_YELLOW )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retni( ( p )->yellow() );
}

/* qreal yellowF () const */
HB_FUNC( QT_QCOLOR_YELLOWF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retnd( ( p )->yellowF() );
}

/* QStringList colorNames () */
HB_FUNC( QT_QCOLOR_COLORNAMES )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->colorNames() ), true ) );
}

/* QColor fromCmyk ( int c, int m, int y, int k, int a = 255 ) */
HB_FUNC( QT_QCOLOR_FROMCMYK )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromCmyk( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnidef( 6, 255 ) ) ), true ) );
}

/* QColor fromCmykF ( qreal c, qreal m, qreal y, qreal k, qreal a = 1.0 ) */
HB_FUNC( QT_QCOLOR_FROMCMYKF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromCmykF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
}

/* QColor fromHsv ( int h, int s, int v, int a = 255 ) */
HB_FUNC( QT_QCOLOR_FROMHSV )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromHsv( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) ) ), true ) );
}

/* QColor fromHsvF ( qreal h, qreal s, qreal v, qreal a = 1.0 ) */
HB_FUNC( QT_QCOLOR_FROMHSVF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromHsvF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QColor fromRgb ( QRgb rgb ) */
HB_FUNC( QT_QCOLOR_FROMRGB )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgb( hb_parnl( 2 ) ) ), true ) );
}

/* QColor fromRgb ( int r, int g, int b, int a = 255 ) */
HB_FUNC( QT_QCOLOR_FROMRGB_1 )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgb( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parnidef( 5, 255 ) ) ), true ) );
}

/* QColor fromRgbF ( qreal r, qreal g, qreal b, qreal a = 1.0 ) */
HB_FUNC( QT_QCOLOR_FROMRGBF )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgbF( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QColor fromRgba ( QRgb rgba ) */
HB_FUNC( QT_QCOLOR_FROMRGBA )
{
   QColor * p = hbqt_par_QColor( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->fromRgba( hb_parnl( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
