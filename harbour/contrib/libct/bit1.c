/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - NUMAND()
 *                                              - NUMOR()
 *                                              - NUMXOR()
 *                                              - NUMNOT()
 *                                              - NUMROL()
 *                                              - NUMMIRR()
 *
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
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
#include "clipdefs.h"

WORD static __hex2int( char *cNum1, int iLenHex );
WORD static __getparam( int iParam );
WORD static __numand( WORD wNum1, WORD wNum2 );
WORD static __numor ( WORD wNum1, WORD wNum2 );
WORD static __numxor( WORD wNum1, WORD wNum2 );
WORD static __numnot( WORD wNum1, WORD wNum2 );
long static __numfun( int iPCount, WORD (*operation)(WORD wNum1, WORD wNum2));


/*  $DOC$
 *  $FUNCNAME$
 *      NUMAND()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *      Bitwise logical AND operation on 16-bit numbers
 *  $SYNTAX$
 *      NUMAND( <nWORD1|cHexWORD1>, <nWORD2|cHexWORD2>[, ..<nWORDn|cHexWORDn>) 
 *            -> <nWordAND>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMAND() is compatible with CT3's NUMAND().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit1.c, library is libct.
 *  $SEEALSO$
 *      NUMOR(), NUMXOR(), NUMNOT(), NUMROL(), NUMMIRR()
 *  $END$
 */

HB_FUNC( NUMAND )
{
  int iPCount;

  iPCount = hb_pcount();

  hb_retnl( __numfun( iPCount, (WORD (*)(WORD wNum1, WORD wNum2))(__numand) ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMOR()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *      Bitwise logical OR operation on 16-bit numbers
 *  $SYNTAX$
 *      NUMOR( <nWORD1|cHexWORD>1, <nWORD2|cHexWORD2>[, ..<nWORDn|cHexWORDn>) 
 *           -> <nWordOR>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMOR() is compatible with CT3's NUMOR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit1.c, library is libct.
 *  $SEEALSO$
 *      NUMAND(), NUMXOR(), NUMNOT(), NUMROL(), NUMMIRR()
 *  $END$
 */

HB_FUNC( NUMOR )
{
  int  iPCount;

  iPCount = hb_pcount();

  hb_retnl( __numfun( iPCount, (WORD (*)(WORD wNum1, WORD wNum2))(__numor) ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMXOR()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *      Bitwise logical XOR operation on 16-bit numbers
 *  $SYNTAX$
 *      NUMXOR( <nWORD1|cHexWORD1>, <nWORD2|cHexWORD2> ) -> <nWordXOR>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMXOR() is compatible with CT3's NUMXOR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit1.c, library is libct.
 *  $SEEALSO$
 *      NUMAND(), NUMOR(), NUMNOT(), NUMROL(), NUMMIRR()
 *  $END$
 */

HB_FUNC( NUMXOR )
{
  int  iPCount;

/*  iPCount = hb_pcount(); */

  iPCount = 2;

  hb_retnl( __numfun( iPCount, (WORD (*)(WORD wNum1, WORD wNum2))(__numxor) ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMNOT()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *      Bitwise logical NOT operation on 16-bit numbers
 *  $SYNTAX$
 *      NUMNOT( <nWORD|cHexWORD> ) -> <nWordNOT>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMNOT() is compatible with CT3's NUMNOT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit1.c, library is libct.
 *  $SEEALSO$
 *      NUMAND(), NUMOR(), NUMXOR(), NUMROL(), NUMMIRR()
 *  $END$
 */

HB_FUNC( NUMNOT )
{
  int  iPCount;

/*  iPCount = hb_pcount(); */

  iPCount = 1;

  hb_retnl( __numfun( iPCount, (WORD (*)(WORD wNum1, WORD wNum2))(__numnot) ) );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMROL()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *      Bitwise ROL (rotate left) operation on 16-bit numbers
 *  $SYNTAX$
 *      NUMROL( <nWORD1|cHexWORD1>, <nWORD1|cHexWORD1>[, <lLowByte>] ) 
 *            -> <nWordROL>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMROL() is compatible with CT3's NUMROL().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit1.c, library is libct.
 *  $SEEALSO$
 *      NUMAND(), NUMOR(), NUMXOR(), NUMNOT(), NUMMIRR()
 *  $END$
 */

HB_FUNC( NUMROL )
{
  USHORT   usNum1, usNum2, usNumBak, usPattern, usTestRol;
  USHORT   usBytes, usFor;

  usNum1 = (USHORT) __getparam( 1 );  /* Number to do ROL */
  usNum2 = (USHORT) __getparam( 2 );  /* Iterations       */

  if ( ISLOG( 3 ) )                   /* if 3th parameter is LOGICAL */
  {
    if ( hb_parl( 3 ) )
       usBytes = 8;
    else
       usBytes = 16;
  }
  else
    usBytes = 16;
  
  usNum2 = usNum2 % usBytes;          /* Set usNum2 < usBytes  */

  usPattern = (-1) << usBytes;

  usTestRol = 1 << ( usBytes - 1 );   /* Pattern to test the MSB */

  usNumBak = usNum1 & usPattern;      /* usNumBak contain the section 
                                         to doesn't ROL               */

  for (usFor = 1; usFor <= usNum2; usFor++)
  {
     if ( usNum1 & usTestRol )  /* Test if MSB is ON */
     {
        usNum1 = usNum1 << 1;
        usNum1 = usNum1 | 1;    /* Simulate that the MSB move to LSB */
     }
     else
        usNum1 = usNum1 << 1;
   }
                              /* Set the section not ROLed */
  usNum1 = ( usNum1 & (~usPattern) ) | usNumBak;

  hb_retnl( usNum1 );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMMIRR()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *      Bitwise mirror operation on 8-bit and 16-bit numbers
 *  $SYNTAX$
 *      NUMMIRR( <nNumber|cHexNum>[, <l8/16bit>] ) -> <nResult>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMMIRR() is compatible with CT3's NUMMIRR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit1.c, library is libct.
 *  $SEEALSO$
 *      NUMAND(), NUMOR(), NUMXOR(), NUMNOT(), NUMROL()
 *  $END$
 */

HB_FUNC ( NUMMIRR )
{
  USHORT  usNum1, usBytes, usFor, usPattern, usNumBak, usMirror = 0;

  usNum1 = (USHORT) __getparam( 1 );
  
  if ( ISLOG( 2 ) )                   /* if 3th parameter is LOGICAL */
  {
    if ( hb_parl( 2 ) )
    {
       usBytes = 8;
       usPattern = 0xFF00;
    }
    else
    {
       usBytes = 16;
       usPattern = 0;
    }
  }
  else
    usBytes = 16;

  usNumBak = usNum1 & usPattern;

  for ( usFor = 1; usFor <= usBytes; usFor++ )
  {
    if ( usNum1 & 1 )
    {
                                 
       usMirror = usMirror << 1;  /* if the LSB of usNum1 == 1 then */
       usMirror = usMirror | 1;   /* set the LSB of usMirror = 1    */
    }
    else
       usMirror = usMirror << 1;

    usNum1 = usNum1 >> 1;

  }
  usMirror = ( usMirror & (~usPattern) ) | usNumBak;

  hb_retnl( usMirror );

}

/*
 * Function to test, transform Hexadecimal numbers to decimals numbers
 * It's more quicker than CTON() function

HB_FUNC ( HEX2NUM )
{
  hb_retnl( __hex2int( hb_parc( 1 ), hb_parclen( 1 ) ) );
}
*/

WORD static __hex2int( char *cNum1, int iLenHex )
{
  int  i;
  int  iNum;
  WORD uiHexNum = 0;


  i = ( iLenHex - 1 );
  while (( i >= 0 ) && ( iLenHex-i <= 4 ))
  {
     iNum = ((int) cNum1[i]) - 0x30;

     if (iNum > 9)
       iNum -= 7;

     if ((iNum < 0) || (iNum > 0x0F))
       break;
     
     uiHexNum += (WORD) iNum * (1 << (4 * ( iLenHex - i - 1 )));
     i--;
  }
  return uiHexNum;
}


WORD static __getparam( int iParam )
{

  if ( ISCHAR( iParam ) )
     return  __hex2int( hb_parc( iParam ), hb_parclen( iParam ) );
  else
     return (WORD) hb_parnl( iParam );

}


WORD static __numand( WORD uiNum1, WORD uiNum2 )
{
    return uiNum1 & uiNum2;
}


WORD static __numor( WORD uiNum1, WORD uiNum2 )
{
    return uiNum1 | uiNum2;
}


WORD static __numxor( WORD uiNum1, WORD uiNum2 )
{
    return uiNum1 ^ uiNum2;
}


WORD static __numnot( WORD uiNum1, WORD uiNum2 )
{
    HB_SYMBOL_UNUSED (uiNum2);
    return ~uiNum1;
}


long static __numfun( int iPCount, WORD (*operation)(WORD wNum1, WORD wNum2))
{
  WORD uiNumOp;
  WORD uiNum1, uiNum2;
  int  iFor;

  if ( ISNUM(1) || ISCHAR(1) )
  {
     uiNum1 = __getparam( 1 );

     if ( iPCount == 1 )

  /*  If unary operation: NOT                           */
        uiNumOp = (*operation)( uiNum1, 0 );

     else
     {

        for ( iFor=2; iFor <= iPCount; iFor++)
        {
           if ( ISNUM( iFor ) || ISCHAR( iFor ) )
           {
              uiNum2 = __getparam( iFor );


  /*  Call to operation: AND, OR, XOR                   */
              uiNumOp = (*operation)( uiNum1, uiNum2 );

           }
           else
  /*  If error in parameter then return -1              */
              return (-1);

  /*  Copy result to first parameter if multi operation */
           uiNum1 = uiNumOp;
        }

     }                 

  }
  else

  /*  If error in parameter then return -1              */
     return (-1);


  /*  Return result of operation */
  return uiNumOp;

}

