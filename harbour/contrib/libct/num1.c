/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *
 *   CT3 Numeric functions - PART 1
 *     - CELSIUS
 *     - FAHRENHEIT
 *     - INFINITY
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
 *      CELSIUS()
 *  $CATEGORY$
 *      CT3 numeric functions
 *  $ONELINER$
 *      Temperature conversion Fahrenheit to Celsius
 *  $SYNTAX$
 *      CELSIUS (nDegreeFahrenheit) --> nDegreeCelsius
 *  $ARGUMENTS$
 *      <nDegreeFahrenheit>     temperature in degree Fahrenheit
 *  $RETURNS$
 *      <nDegreeCelsius>        temperate in degree Celsius
 *  $DESCRIPTION$
 *      CELSIUS() converts temperature values measured in the Fahrenheit scale
 *      to the Celsius scale.
 *  $EXAMPLES$
 *      // melting point of water in standard conditions
 *      ? celsius (32.0)       --> 0.0
 *      // boiling point of water in standard conditions
 *      ? celsius (212.0)      --> 100.0
 *  $TESTS$
 *      celsius (32.0)  == 0.0
 *      celsius (212.0) == 100.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CELSIUS() is compatible with CT3's CELSIUS().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is num1.c, library is libct.
 *  $SEEALSO$
 *      FAHRENHEIT()
 *  $END$
 */

HB_FUNC( CELSIUS )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;
    
    ct_matherrbegin();
    dResult = (5.0 / 9.0) * ( dInput - 32.0 );  
    ct_matherrend();
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CELSIUS,
                               NULL, "CELSIUS", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
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
 *      FAHRENHEIT()
 *  $CATEGORY$
 *      CT3 numeric functions
 *  $ONELINER$
 *      Temperature conversion Celsius to Fahrenheit
 *  $SYNTAX$
 *      FAHRENHEIT (nDegreeCelsius) --> nDegreeFahrenheit
 *  $ARGUMENTS$
 *      <nDegreeCelsius>        temperate in degree Celsius
 *  $RETURNS$
 *      <nDegreeFahrenheit>     temperature in degree Fahrenheit
 *  $DESCRIPTION$
 *      FAHRENHEIT() converts temperature values measured in the Celsius scale
 *      to the Fahrenheit scale.
 *  $EXAMPLES$
 *      // melting point of water in standard conditions
 *      ? fahrenheit (0.0)       --> 32.0
 *      // boiling point of water in standard conditions
 *      ? fahrenheit (100.0)      --> 212.0
 *  $TESTS$
 *      fahrenheit (0.0) == 32.0
 *      celsius (100.0) == 212.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      FAHRENHEIT() is compatible with CT3's FAHRENHEIT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is num1.c, library is libct.
 *  $SEEALSO$
 *      CELSIUS()
 *  $END$
 */

HB_FUNC( FAHRENHEIT )
{
  if( ISNUM(1) )
  {
    double dInput = hb_parnd(1);
    double dResult;

    ct_matherrbegin();
    dResult = (( 9.0 / 5.0) * dInput ) + 32.0 ;  
    ct_matherrend();
    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_FAHRENHEIT,
                               NULL, "FAHRENHEIT", 0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
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
 *      INFINITY()
 *  $CATEGORY$
 *      CT3 numeric functions
 *  $ONELINER$
 *      Returns the largest floating point number available in the system
 *  $SYNTAX$
 *      INFINITY ([<lPlatformIndependant>]) --> nLargestNumber
 *  $ARGUMENTS$
 *      [<lPlatformIndependant>]   .T., if the function should return
 *                                 the maximum floating point value
 *                                 available (DBL_MAX)
 *                                 .F., function should try to return
 *                                 the same value as the original CT3 lib did
 *                                 Default: .F.
 *  $RETURNS$
 *      <nLargestNumber>     the largest floating point number available in the system
 *  $DESCRIPTION$
 *      INFINITY() returns the largest floating point number available 
 *      in the system. For platform independance, this is set to DBL_MAX.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      INFINITY() must not necessarily return the same number as CT3's INFINITY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is num1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( INFINITY ) 
{
  
  if (ISLOG (1) && hb_parl(1))
  {
    hb_retnd (DBL_MAX);
  }
  else
  {
    hb_retnd (93786976294838206460.00);
  }
  return;

}
 

