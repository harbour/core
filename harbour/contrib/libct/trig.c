/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 trigonometric functions 
 *     - PI
 *     - SIN
 *     - COS
 *     - TAN
 *     - COT
 *     - ASIN
 *     - ACOS
 *     - ATAN
 *     - SINH
 *     - COSH
 *     - TANH
 *     - ATN2
 *     - RTOD
 *     - DTOR
 *
 * NOTE: All these functions were builded using Borland C++ 5.5 (free version)
 *
 * Copyright 2001  Alejandro de Garate  <alex_degarate@hotmail.com>
 *
 * Documentation and changes concerning error handling Copyright 2001 
 *   IntTec GmbH, Freiburg, Germany, Author: Martin Vogel <vogel@inttec.de>
 *
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


#include "ct.h"


/*  $DOC$
 *  $FUNCNAME$
 *      PI()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Returns Pi, the perimeter-to-diameter-ratio of a circle
 *  $SYNTAX$
 *      PI () -> nPi
 *  $ARGUMENTS$
 *  $RETURNS$
 *      <nPi>      the math constant Pi with maximum precision available
 *  $DESCRIPTION$
 *      The function PI() can be used if the constant Pi is needed
 *      with maximum precision. One of the most known interpretations of this
 *      number is the constant perimeter-to-diameter-ratio of circles.
 *  $EXAMPLES$
 *      // the diameter of a circle-like swimming pool is 3.4 meters, how
 *      // long is the perimeter ?
 *
 *      ? str(PI()*3.4,5,3)+" meters"   --> 10.681 meters
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      PI() is compatible with CT3's PI().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),DTOR()
 *  $END$
 */

HB_FUNC( PI )
{
  hb_retnd (CT_PI);
  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      SIN()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Sine of the argument
 *  $SYNTAX$
 *      SIN (nRadiant) -> nSine
 *  $ARGUMENTS$
 *      <nRadiant>       an angle size given in radiants
 *  $RETURNS$
 *      <nSine>          the sine of <nRadiant>
 *  $DESCRIPTION$
 *      The function SIN() calculates the sine of an angle whose size is
 *      given in radiants (full angle equals 2*Pi - see DTOR() for angle size
 *      given in degress).
 *      A common geometric interpretation of the SIN() function is the
 *      counterkathede-hypotenuse-ratio of a right-angled triangle.
 *  $EXAMPLES$
 *      ? sin (0.0) --> 0.0
 *      ? sin (1.0) --> 0.8414...
 *  $TESTS$
 *      sin (0.0) == 0.0
 *      sin (PI()/4) == sqrt(1/2)
 *      sin (PI()/2) == 1.0
 *      sin (PI()) == 0.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      SIN() is compatible with CT3's SIN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( SIN )
{
  if( ISNUM(1) )
  {
    double dInput  = hb_parnd(1);
    double dResult;

    ct_matherrbegin();
    dResult = sin (dInput);  
    ct_matherrend();

    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_SIN,
                               NULL, "SIN", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      COS()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Cosine of the argument
 *  $SYNTAX$
 *      COS (nRadiant) -> nCosine
 *  $ARGUMENTS$
 *      <nRadiant>       an angle size given in radiants
 *  $RETURNS$
 *      <nCosine>        the cosine of <nRadiant>
 *  $DESCRIPTION$
 *      The function COS() calculates the cosine of an angle whose size is
 *      given in radiants (full angle equals 2*Pi - see DTOR() for angle size
 *      given in degress).
 *      A common geometric interpretation of the COS() function is the
 *      ankathede-hypotenuse-ratio of a right-angled triangle.
 *  $EXAMPLES$
 *      ? cos (0.0) --> 1.0
 *      ? cos (1.0) --> 0.5403...
 *  $TESTS$
 *      cos (0.0) == 1.0
 *      cos (PI()/4) == sqrt(1/2)
 *      cos (PI()/2) == 0.0
 *      cos (PI()) == -1.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      COS() is compatible with CT3's COS().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( COS )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    ct_matherrbegin();
    dResult = cos (dInput);  
    ct_matherrend();

    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_COS,
                               NULL, "COS", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      TAN()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Tangent of the argument
 *  $SYNTAX$
 *      TAN (nRadiant) -> nTangent
 *  $ARGUMENTS$
 *      <nRadiant>       an angle size given in radiants
 *  $RETURNS$
 *      <nTangent>       the tangent of <nRadiant>
 *  $DESCRIPTION$
 *      The function TAN() calculates the tangent of an angle whose size is
 *      given in radiants (full angle equals 2*Pi - see DTOR() for angle size
 *      given in degress).
 *      A common geometric interpretation of the TAN() function is the
 *      counterkathede-ankathede-ratio of a right-angled triangle, or,
 *      tan(x) = sin(x)/cos(x).
 *  $EXAMPLES$
 *      ? tan (0.0) --> 0.0
 *      ? tan (1.0) --> 1.5574...
 *  $TESTS$
 *      tan (0.0) == 0.0
 *      tan (PI()/4) == 1
 *      tan (PI()) == 0.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TAN() is compatible with CT3's TAN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( TAN )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    if (((dInput/CT_PI)-floor(dInput/CT_PI)) == 0.5)
    {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TAN,
                                 NULL, "TAN", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
      }
      if (pSubst != NULL)
      {
        hb_itemReturn (pSubst);
        hb_itemRelease (pSubst);
      }
      else
      {
        hb_retnd (DBL_MAX);
      }
    }
    else
    {
      ct_matherrbegin();
      dResult = tan (dInput);  
      ct_matherrend();
      hb_retnd( dResult );
    }
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TAN,
                               NULL, "TAN", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      COT()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Cotangent of the argument
 *  $SYNTAX$
 *      COT (nRadiant) -> nCotangent
 *  $ARGUMENTS$
 *      <nRadiant>       an angle size given in radiants
 *  $RETURNS$
 *      <nCotangent>     the cotangent of <nRadiant>
 *  $DESCRIPTION$
 *      The function COT() calculates the cotangent of an angle whose size is
 *      given in radiants (full angle equals 2*Pi - see DTOR() for angle size
 *      given in degress).
 *      A common geometric interpretation of the COT() function is the
 *      ankathede-counterkathede-ratio of a right-angled triangle, or,
 *      cot(x) = cos(x)/sin(x)=1/tan(x).
 *  $EXAMPLES$
 *      ? cot (1.0) --> 0.6420...
 *  $TESTS$
 *      cot (PI()/4) == 1
 *      cot (PI()/2) == 0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      COT() is compatible with CT3's COT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( COT )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;
  
    if (((dInput/CT_PI)-floor(dInput/CT_PI)) == 0.0)
    {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_COT,
                                 NULL, "COT", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
      }
      if (pSubst != NULL)
      {
        hb_itemReturn (pSubst);
        hb_itemRelease (pSubst);
      }
      else
      {
        hb_retnd (DBL_MAX);
      }
    }
    else
    {
      ct_matherrbegin();
      dResult = 1/tan (dInput);  
      ct_matherrend();
      hb_retnd( dResult );
    }
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_COT,
                               NULL, "COT", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      ASIN()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Arcus sine of the argument
 *  $SYNTAX$
 *      ASIN (nSine) -> nRadiant
 *  $ARGUMENTS$
 *      <nSine>         the sine of an angle
 *  $RETURNS$
 *      <nRadiant>      the angle whose sine is <nSine>
 *  $DESCRIPTION$
 *      The function ASIN() is the inverse function of SIN(). It takes a
 *      sine value and returns the smallest(!) angle whose sine equals to the argument.
 *      The return value is given in radiants (full angle equals 2*Pi -
 *      see DTOR() if you need to convert it into degress).
 *      Note, that <nSine> must be between -1 and 1 and that <nRadiant>
 *      is always between -PI()/2 and PI()/2. 
 *  $EXAMPLES$
 *      ? asin (0.0) --> 0.0
 *      ? asin (0.5) --> 0.5235...
 *  $TESTS$
 *      asin (0.0) == 0.0
 *      asin (sqrt(1/2)) == PI()/4
 *      asin (1.0) == PI()/2
 *      asin (0.0) == 0.0  // and not PI(), since the smallest angle is returned !
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ASIN() is compatible with CT3's ASIN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( ASIN )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    if (abs(dInput) > 1.0)
    {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ASIN,
                                 NULL, "ASIN", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
      }
      if (pSubst != NULL)
      {
        hb_itemReturn (pSubst);
        hb_itemRelease (pSubst);
      }
      else
      {
        hb_retnd (0.0);
      }
    }
    else
    {
      ct_matherrbegin();
      dResult = asin (dInput);  
      ct_matherrend();
      hb_retnd( dResult );
    }
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ASIN,
                               NULL, "ASIN", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }
  
  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      ACOS()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Arcus cosine of the argument
 *  $SYNTAX$
 *      ACOS (nCosine) -> nRadiant
 *  $ARGUMENTS$
 *      <nCosine>       the cosine of an angle
 *  $RETURNS$
 *      <nRadiant>      the angle whose cosine is <nCosine>
 *  $DESCRIPTION$
 *      The function ACOS() is the inverse function of COS(). It takes a
 *      cosine value and returns the smallest(!) angle whose cosine equals to the argument.
 *      The return value is given in radiants (full angle equals 2*Pi -
 *      see DTOR() if you need to convert it into degress).
 *      Note, that <nCosine> must be between -1 and 1 and that <nRadiant>
 *      is always between 0 and PI(). 
 *  $EXAMPLES$
 *      ? acos (0.0) --> PI()/2
 *      ? acos (0.5) --> 1.04719...
 *  $TESTS$
 *      acos (0.0) == PI()/2
 *      acos (sqrt(1/2)) == PI()/4
 *      acos (1.0) == 0.0
 *      acos (-1.0) == PI()
 *      acos (0.0) == PI()/2  // and not -PI()/2, although cos (-PI()/2) == 0.0 !
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ACOS() is compatible with CT3's ACOS().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( ACOS )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    if (abs(dInput) > 1.0)
    {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ACOS,
                                 NULL, "ACOS", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
      }
      if (pSubst != NULL)
      {
        hb_itemReturn (pSubst);
        hb_itemRelease (pSubst);
      }
      else
      {
        hb_retnd (0.0);
      }
    }
    else
    {
      ct_matherrbegin();
      dResult = acos (dInput);  
      ct_matherrend();
      hb_retnd( dResult );
    }
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ACOS,
                               NULL, "ACOS", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }
  
  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      ATAN()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Arcus tangent of the argument
 *  $SYNTAX$
 *      ACOS (nTangent) -> nRadiant
 *  $ARGUMENTS$
 *      <nTangent>      the tangent of an angle
 *  $RETURNS$
 *      <nRadiant>      the angle whose tangent is <nTangent>
 *  $DESCRIPTION$
 *      The function ATAN() is the inverse function of TAN(). It takes a
 *      tangent value and returns the smallest(!) angle whose tangent equals to the argument.
 *      The return value is given in radiants between -PI()/2 and PI()/2
 *      (full angle equals 2*Pi - see DTOR() if you need to convert it into degress).
 *  $EXAMPLES$
 *      ? atan (0.0) --> 0.0
 *      ? atan (0.5) --> 0.4636...
 *  $TESTS$
 *      atan (0.0) == 0.0
 *      atan (1.0) == PI()/4
 *      atan (0.0) == 0.0 // and not PI(), although tan (PI()) == 0.0 !
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ATAN() is compatible with CT3's ATAN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( ATAN )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    ct_matherrbegin();
    dResult = atan (dInput);  
    ct_matherrend();
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ATAN,
                               NULL, "ATAN", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }
  
  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      ATN2()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Arcus tangent a sine and a cosine argument
 *  $SYNTAX$
 *      ATN2 (nSine, nCosine) -> nRadiant
 *  $ARGUMENTS$
 *      <nSine>         the sine of an angle
 *      <nCosine>       the cosine of an angle
 *  $RETURNS$
 *      <nRadiant>      the angle whose tangent is <nSine>/<nCosine>
 *  $DESCRIPTION$
 *      The function ATN2() is an alternate function for calculating
 *      the arcus tangent, atn2(x,y) = atan(x/y).
 *      It takes two arguments, the sine and the cosine
 *      of the angle that should be calculated. Thus, in contrast to the ATAN()
 *      function, ATN2() can distinguish whether the sine or the cosine has
 *      a negative sign (or both being positive or negative), so that
 *      the return value can be between -PI() and PI() and covers the full
 *      angle.
 *      The return value is given in radiants (full angle equals 2*Pi -
 *      see DTOR() if you need to convert it into degress).
 *  $EXAMPLES$
 *      ? atn2 (0.0, 1.0) --> 0.0
 *      ? atn2 (sqrt(1/2), sqrt(1/2)) --> PI()/4
 *  $TESTS$
 *      atn2 (0.0, 1.0) == 0.0
 *      atn2 (sqrt(1/2),sqrt(1/2)) == PI()/4
 *      atn2 (-sqrt(1/2),-sqrt(1/2)) == -3/4*PI()  // atan() would return PI()/4 !
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ATN2() is compatible with CT3's ATN2().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),SINH(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( ATN2 )
{
  if( ISNUM(1) && ISNUM(2) )
  {
    double dY = hb_parnd(1);
    double dX = hb_parnd(2);
    double dResult;

    ct_matherrbegin();
    dResult = atan2( dY, dX );  /* NOTE: parameters are swapped */
    ct_matherrend();
    hb_retnd( dResult );
 
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ATN2,
                               NULL, "ATN2", 0, EF_CANSUBSTITUTE, 2,
                               hb_paramError (1), hb_paramError (2));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      SINH()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Hyperbolic Sine of the argument
 *  $SYNTAX$
 *      SINH (nArea) -> nHyperbolicSine
 *  $ARGUMENTS$
 *      <nArea>            the size of the area (see below)
 *  $RETURNS$
 *      <nHyperbolicSine>  the hyperbolic sine of <nArea>
 *  $DESCRIPTION$
 *      The function SINH() calculates the hyperbolic sine of the argument.
 *      In analytical mathematics it is defined as 1/2*(exp(nArea)-exp(-nArea)).
 *      A common geometric interpretation of the SINH() function is the
 *      maximum y value of the points in the area with the given size <nArea>,
 *      that is bound by the x axis, a straight line through the point of
 *      origin (this one is fixed by the area) and the hyperbola x^2-y^2=1.
 *  $EXAMPLES$
 *      ? sinh (0.0) --> 0.0
 *      ? sinh (1.0) --> 1.1752...
 *  $TESTS$
 *      sinh (0.0) == 0.0
 *      sinh (-0.5) == -sinh(0.5)
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      SINH() is new in Harbours CT3's library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),COSH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( SINH )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    ct_matherrbegin();
    dResult = sinh (dInput);
    ct_matherrend();
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_SINH,
                               NULL, "SINH", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      COSH()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Hyperbolic Cosine of the argument
 *  $SYNTAX$
 *      COSH (nArea) -> nHyperbolicCosine
 *  $ARGUMENTS$
 *      <nArea>              the size of the area (see below)
 *  $RETURNS$
 *      <nHyperbolicCosine>  the hyperbolic cosine of <nArea>
 *  $DESCRIPTION$
 *      The function COSH() calculates the hyperbolic cosine of the argument.
 *      In analytical mathematics it is defined as 1/2*(exp(nArea)+exp(-nArea)).
 *      A common geometric interpretation of the COSH() function is the
 *      maximum x value of the points in the area with the given size <nArea>,
 *      that is bound by the x axis, a straight line through the point of
 *      origin (this one is fixed by the area) and the hyperbola x^2-y^2=1.
 *  $EXAMPLES$
 *      ? cosh (0.0) --> 1.0
 *      ? cosh (1.0) --> 1.5430...
 *  $TESTS$
 *      cosh (0.0) == 1.0
 *      cosh (-0.5) == cosh(0.5)
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      COSH() is new in Harbours CT3's library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),TANH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( COSH )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    ct_matherrbegin();
    dResult = cosh (dInput);
    ct_matherrend();
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_COSH,
                               NULL, "COSH", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      SINH()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Hyperbolic Tangent of the argument
 *  $SYNTAX$
 *      TANH (nArea) -> nHyperbolicTangent
 *  $ARGUMENTS$
 *      <nArea>               the size of the area (see below)
 *  $RETURNS$
 *      <nHyperbolicTangent>  the hyperbolic tangent of <nArea>
 *  $DESCRIPTION$
 *      The function TANH() calculates the hyperbolic tangent of the argument.
 *      In analytical mathematics it is defined as SINH(x)/COSH(x).
 *  $EXAMPLES$
 *      ? tanh (0.0) --> 0.0
 *      ? tanh (1.0) --> 0.7615...
 *  $TESTS$
 *      tanh (0.0) == 0.0
 *      tanh (-0.5) == -tanh(0.5)
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TANH() is new in Harbours CT3's library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),RTOD(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( TANH )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    ct_matherrbegin();
    dResult = tanh (dInput);
    ct_matherrend();
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TANH,
                               NULL, "TANH", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      RTOD()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Convert radiant to degree
 *  $SYNTAX$
 *      RTOD (nRadiant) -> nDegree
 *  $ARGUMENTS$
 *      <nRadiant>         the size of an angle in radiant
 *  $RETURNS$
 *      <nDegree>          the size of that angle in degree
 *  $DESCRIPTION$
 *      The function RTOD() can be used to convert sizes of angles given
 *      in radiant (like those returned by the asin, acos or atan function)
 *      to degrees that are commonly used geometry and technics.
 *  $EXAMPLES$
 *      ? rtod (PI()) --> 180
 *      ? tanh (PI()/3) --> 60
 *  $TESTS$
 *      rtod (0.0) == 0.0
 *      rtod (PI()) == 180.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      RTOD() is compatible with CT3's RTOD().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),DTOR(),PI()
 *  $END$
 */

HB_FUNC( RTOD )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;
    dResult = ( 180.0 / CT_PI ) * dInput ;    
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_RTOD,
                               NULL, "RTOD", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      DTOR()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Convert degree to radiant
 *  $SYNTAX$
 *      DTOR (nDegree) -> nRadiant
 *  $ARGUMENTS$
 *      <nDegree>          the size of that angle in degree
 *  $RETURNS$
 *      <nRadiant>         the size of an angle in radiant
 *  $DESCRIPTION$
 *      The function DTOR() can be used to convert sizes of angles given
 *      in degrees to radiant (as expected by sin, cos or tan functions).
 *  $EXAMPLES$
 *      ? dtor (180) --> PI()
 *      ? dtor (60) --> PI()/3
 *  $TESTS$
 *      dtor (0.0) == 0.0
 *      dtor (180.0) == PI()
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      DTOR() is compatible with CT3's DTOR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is trig.c, library is libct.
 *  $SEEALSO$
 *      SIN(),COS(),TAN(),COT(),ASIN(),ACOS(),ATAN(),ATN2(),SINH(),COSH(),TANH(),RTOD(),PI()
 *  $END$
 */

HB_FUNC( DTOR )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult = ( CT_PI / 180.0 ) * dInput ;  
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_RTOD,
                               NULL, "RTOD", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


 
