/*
 * Harbour Project source code: 
 *
 *   CT3 mathematical functions
 *     - FLOOR
 *     - CEILING
 *     - SIGN
 *     - LOG10
 *     - FACT
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
 *      FLOOR()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Rounds down a number to the next integer
 *  $SYNTAX$
 *      FLOOR (nNumber) -> nDownRoundedNumber
 *  $ARGUMENTS$      
 *      <nNumber>             number to round down
 *  $RETURNS$
 *      <nDownRoundedNumber>  the rounded number
 *  $DESCRIPTION$
 *      The function FLOOR() determines the biggest integer that is smaller
 *      than <nNumber>. 
 *  $EXAMPLES$
 *      ? floor (1.1)   --> 1.0
 *      ? floor (-1.1)  --> -2.0
 *  $TESTS$
 *      floor (1.1)  == 1.0
 *      floor (-1.1) == -2.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      FLOOR() is compatible with CT3's FLOOR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is math.c, library is libct.
 *  $SEEALSO$
 *      CEILING
 *  $END$
 */

HB_FUNC( FLOOR )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;
    
    hb_mathResetError();
    dResult = floor (dInput);  
      
    hb_retni( (int)dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_FLOOR,
                               NULL, "FLOOR", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
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
 *      CEILING()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Rounds up a number to the next integer
 *  $SYNTAX$
 *      CEILING (nNumber) -> nUpRoundedNumber
 *  $ARGUMENTS$      
 *      <nNumber>             number to round up
 *  $RETURNS$
 *      <nUpRoundedNumber>    the rounded number
 *  $DESCRIPTION$
 *      The function CEILING() determines the smallest integer that is bigger
 *      than <nNumber>. 
 *  $EXAMPLES$
 *      ? ceiling (1.1)   --> 2.0
 *      ? ceiling (-1.1)  --> -1.0
 *  $TESTS$
 *      ceiling (1.1)  == 2.0
 *      ceiling (-1.1) == -1.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CEILING() is compatible with CT3's CEILING().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is math.c, library is libct.
 *  $SEEALSO$
 *      FLOOR
 *  $END$
 */

HB_FUNC( CEILING )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;
    
    hb_mathResetError();
    dResult = ceil (dInput);  
     
    hb_retni( (int)dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CEILING,
                               NULL, "CEILING", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
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
 *      SIGN()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Sign of a number
 *  $SYNTAX$
 *      SIGN (nNumber) -> nSign
 *  $ARGUMENTS$      
 *      <nNumber>             a number 
 *  $RETURNS$
 *      <nSign>               sign of <nNumber>
 *  $DESCRIPTION$
 *      The function SIGN() determines the sign of <nNumber>.
 *      If <nNumber> is > 0, then SIGN(<nNumber>) returns 1
 *      If <nNumber> is < 0, then SIGN(<nNumber>) returns -1
 *      If <nNumber> is == 0, then SIGN(<nNumber>) returns 0
 *  $EXAMPLES$
 *      ? sign (1.1)   --> 1
 *      ? sign (-1.1)  --> -1
 *      ? sign (0.0)  --> 0
 *  $TESTS$         
 *      sign (1.1)  == 1
 *      sign (-1.1) == -1
 *      sign (0.0)  == 0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      SIGN() is compatible with CT3's SIGN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is math.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( SIGN )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);   
    int iResult ;

    if( dInput == 0.00 )
        iResult = 0;
    else 
    {
        if( dInput > 0.00 )
            iResult = 1;
        else
            iResult = -1;
    }
    hb_retni( iResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_SIGN,
                               NULL, "SIGN", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retni (0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      LOG10()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Decadic logarithm of a number 
 *  $SYNTAX$
 *      LOG10 (nNumber) -> nLogarithm
 *  $ARGUMENTS$      
 *      <nNumber>             number to logarithm
 *  $RETURNS$
 *      <nLogarithm>          decadic logarithm of <nNumber>
 *  $DESCRIPTION$
 *      The function LOG10() calculates the decadic logarithm of <nNumber>,
 *      i.e. 10^<nLogarithm> == <nNumber>.
 *  $EXAMPLES$
 *      ? log10 (10.0)       --> 1.0
 *      ? log10 (sqrt(10.0)) --> 0.5
 *  $TESTS$
 *      log10 (10.0)       == 1.0
 *      log10 (sqrt(10.0)) == 0.5
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      LOG10() is compatible with CT3's LOG10().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is math.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( LOG10 )
{
  if( ISNUM(1) )
  { 
    double dInput = hb_parnd(1);
    double dResult;

    hb_mathResetError();
    dResult = log10 (dInput);  

    if (hb_mathIsMathErr())
    {
      /* the C-RTL provides a kind of matherr() mechanism */ 
      HB_MATH_EXCEPTION hb_exc;
      int iLastError = hb_mathGetLastError (&hb_exc);
      if (iLastError != HB_MATH_ERR_NONE)
      {
	if (hb_exc.handled)
	{
	  hb_retndlen (hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec);
	}
	else
	{
	  /* math exception is up to the Harbour function, so do this as CTIII compatible as possible */
	  switch (iLastError)
	  {
	  case HB_MATH_ERR_SING:     /* argument to log10 was 0.0 */
	  case HB_MATH_ERR_DOMAIN:   /* argument to log10 was < 0.0 */
	    {
	      hb_retndlen (-HUGE_VAL, -1, -1); /* return -infinity */
	    }; break;
	  default:
	    {
	      hb_retnd (0.0);
	    }
	  }
	}
	return;
      }
    }
    hb_retnd (dResult);
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_LOG10,
                               NULL, "LOG10", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retni (0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      FACT()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Calculates faculty 
 *  $SYNTAX$
 *      FACT (nNumber) -> nFaculty
 *  $ARGUMENTS$      
 *      <nNumber>          number between 0 and 21
 *  $RETURNS$
 *      <nFaculty>         the faculty of <nNumber>
 *  $DESCRIPTION$
 *      The function FACT() calculates the faculty to the integer given in
 *      <nNumber>. The faculty is defined as n! = 1*2*...*n and is often
 *      used in statistics. Note, that faculties above 21 are too big
 *      so that the function must return a -1.
 *  $EXAMPLES$
 *      ? fact (0)  --> 1
 *      ? fact (1)  --> 1
 *      ? fact (4)  --> 24
 *  $TESTS$
 *      fact (0) == 1
 *      fact (1) == 1
 *      fact (4) == 24
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      FACT() is compatible with CT3's FACT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is math.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( FACT )
{
  if( ISNUM(1) )
  {
    int iInput = hb_parni(1); 
    int i;
    double dResult = 1.0;

    if ((iInput >= 0) && (iInput < 22))
    {
      for (i = 1; i <= iInput; i++)
      {
        dResult *= (double)i;
      }
      hb_retnd( dResult );
    }
    else
    {
      hb_retnd (-1.0);
    }
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_FACT,
                               NULL, "FACT", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
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

