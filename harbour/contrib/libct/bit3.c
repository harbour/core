/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - NUMANDX()
 *                                              - NUMORX()
 *                                              - NUMXORX()
 *                                              - NUMNOTX()
 *                                              - NUMROLX()
 *                                              - NUMMIRRX()
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

long static __hex2long( char *cNum1, int iLenHex );
long static __getparam( int iParam );
long static __numand( long wNum1, long wNum2 );
long static __numor ( long wNum1, long wNum2 );
long static __numxor( long wNum1, long wNum2 );
long static __numnot( long wNum1, long wNum2 );
long static __numfun( int iPCount, long (*operation)(long wNum1, long wNum2), BOOLP pbOk );
void static sizeofbits( USHORTP pusBytes, LONGP plPattern, LONGP plTestMSB );

/*  $DOC$
 *  $FUNCNAME$
 *      NUMANDX()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NUMANDX( <nSignificativeBits>, <nLONG1|cHexLONG1>, <nLONG2|cHexLONG2>
 *              [, ..<nLONGn|cHexLONGn>) -> <nLONGAND>
 *  $ARGUMENTS$
 *      <SignificativeBits> Designate a number in the range of 0 to 32, 
 *           indicating the LSB of nLONGx|cHexLONGx that will be used.
 *
 *      <nLONG | cHexLONG>  Designate either decimal or hexadecimal
 *           number string.
 *
 *  $RETURNS$
 *      NUMANDX() join all designated parameters with the logical "AND" and
 *           return the result.
 *
 *  $DESCRIPTION$
 *      This function is similar to NUMAND() function with a significative
 *      change. The first parameter indicate the quantity of lower bits of
 *      nLONG are used. If MSB of the result is ON the number is considerate
 *      a negative number.
 *      In other words, if <nSignificativeBits> = 16, nResult return a number
 *      between -32768 and 32767; if <nSignificativeBits> = 8, nResult return
 *      a number between -128 and 127.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMANDX() is a new function in the CT3-library for Harbour.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit3.c, library is libct.
 *  $SEEALSO$
 *      NUMAND(), NUMORX(), NUMXORX(), NUMNOTX(), NUMROLX(), NUMMIRRX()
 *  $END$
 */

HB_FUNC( NUMANDX )
{
  int   iPCount;
  long  lNumOp;
  BOOL  bOk;

  iPCount = hb_pcount();

  lNumOp = __numfun( iPCount, (long (*)(long wNum1, long wNum2))(__numand), &bOk );

  if ( bOk )
     hb_retnl( lNumOp );
  else
     hb_ret( );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMORX()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NUMORX( <nSignificativeBits>, <nLONG1|cHexLONG>1, <nLONG2|cHexLONG2>
 *             [, ..<nLONGn|cHexLONGn>) -> <nLONGOR>
 *  $ARGUMENTS$
 *      <SignificativeBits> Designate a number in the range of 0 to 32, 
 *           indicating the LSB of nLONGx|cHexLONGx that will be used.
 *
 *      <nLONG | cHexLONG>  Designate either decimal or hexadecimal
 *           number string.
 *
 *  $RETURNS$
 *      NUMORX() join all designated parameters with the logical "OR" and
 *           return the result.
 *
 *  $DESCRIPTION$
 *      This function is similar to NUMOR() function with a significative
 *      change. The first parameter indicate the quantity of lower bits of
 *      nLONG are used. If MSB of the result is ON the number is considerate
 *      a negative number.
 *      In other words, if <nSignificativeBits> = 16, nResult return a number
 *      between -32768 and 32767; if <nSignificativeBits> = 8, nResult return
 *      a number between -128 and 127.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMORX() is a new function in the CT3-library for Harbour.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit3.c, library is libct.
 *  $SEEALSO$
 *      NUMOR(), NUMANDX(), NUMXORX(), NUMNOTX(), NUMROLX(), NUMMIRRX()
 *  $END$
 */

HB_FUNC( NUMORX )
{
  int   iPCount;
  long  lNumOp;
  BOOL  bOk;

  iPCount = hb_pcount();

  lNumOp = __numfun( iPCount, (long (*)(long wNum1, long wNum2))(__numor), &bOk );

  if ( bOk )
     hb_retnl( lNumOp );
  else
     hb_ret( );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMXORX()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NUMXORX( <nSignificativeBits>, <nLONG1|cHexLONG1>, <nLONG2|cHexLONG2> )
 *              -> <nLONGXOR>
 *  $ARGUMENTS$
 *      <SignificativeBits> Designate a number in the range of 0 to 32, 
 *           indicating the LSB of nLONGx|cHexLONGx that will be used.
 *
 *      <nLONG | cHexLONG>  Designate either decimal or hexadecimal
 *           number string.
 *
 *  $RETURNS$
 *      NUMXORX() join all designated parameters with the logical "XOR" and
 *           return the result.
 *
 *  $DESCRIPTION$
 *      This function is similar to NUMXOR() function with a significative
 *      change. The first parameter indicate the quantity of lower bits of
 *      nLONG are used. If MSB of the result is ON the number is considerate
 *      a negative number.
 *      In other words, if <nSignificativeBits> = 16, nResult return a number
 *      between -32768 and 32767; if <nSignificativeBits> = 8, nResult return
 *      a number between -128 and 127.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMXORX() is a new function in the CT3-library for Harbour.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit3.c, library is libct.
 *  $SEEALSO$
 *      NUMXOR(), NUMANDX(), NUMORX(), NUMNOTX(), NUMROLX(), NUMMIRRX()
 *  $END$
 */

HB_FUNC( NUMXORX )
{
  int   iPCount;
  long  lNumOp;
  BOOL  bOk;

/*  iPCount = hb_pcount(); */

  iPCount = 3;

  lNumOp = __numfun( iPCount, (long (*)(long wNum1, long wNum2))(__numxor), &bOk );

  if ( bOk )
     hb_retnl( lNumOp );
  else
     hb_ret( );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMNOTX()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NUMNOTX( <nSignificativeBits>, <nLONG|cHexLONG> ) -> <nLONGNOT>
 *  $ARGUMENTS$
 *      <SignificativeBits> Designate a number in the range of 0 to 32, 
 *           indicating the LSB of nLONGx|cHexLONGx that will be used.
 *
 *      <nLONG | cHexLONG>  Designate either decimal or hexadecimal
 *           number string.
 *
 *  $RETURNS$
 *      NUMNOTX() return the negated binary value of the nLONG parameter.
 *           The 0 bits become 1, and 1 bits become 0.
 *
 *  $DESCRIPTION$
 *      This function is similar to NUMNOT() function with a significative
 *      change. The first parameter indicate the quantity of lower bits of
 *      nLONG are used. If MSB of the result is ON the number is considerate
 *      a negative number.
 *      In other words, if <nSignificativeBits> = 16, nResult return a number
 *      between -32768 and 32767; if <nSignificativeBits> = 8, nResult return
 *      a number between -128 and 127.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMNOTX() is a new function in the CT3-library for Harbour.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit3.c, library is libct.
 *  $SEEALSO$
 *      NUMNOT(), NUMANDX(), NUMORX(), NUMXORX(), NUMROLX(), NUMMIRRX()
 *  $END$
 */

HB_FUNC( NUMNOTX )
{
  int   iPCount;
  long  lNumOp;
  BOOL  bOk;

/*  iPCount = hb_pcount(); */

  iPCount = 2;

  lNumOp = __numfun( iPCount, (long (*)(long wNum1, long wNum2))(__numnot), &bOk );

  if ( bOk )
     hb_retnl( lNumOp );
  else
     hb_ret( );
}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMROLX()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NUMROLX( <nSignificativeBits>, <nLONG|cHexLONG>, <nWORD|cHexWORD> ) 
 *            -> <nLONGROL>
 *  $ARGUMENTS$
 *      <SignificativeBits> Designate a number in the range of 0 to 32, 
 *           indicating the LSB of nLONGx|cHexLONGx that will be used.
 *
 *      <nLONG | cHexLONG>  Designate either decimal or hexadecimal
 *           number string.
 *
 *      <nWORD | cHexWORD>  Designate a number of rotations in the range of
 *           1 to <nSignificativeBits>; as either numeric or hexadecimal.
 *
 *  $RETURNS$
 *      NUMROLX() return the rotation result.
 *
 *  $DESCRIPTION$
 *      This function is similar to NUMROL() function with a significative
 *      change. The first parameter indicate the quantity of lower bits of
 *      nLONG are used. When the high bit rotates it is not just moved out to
 *      the left, it is also moved in on the right.
 *      The not rotated bits is not moved.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMROLX() is a new function in the CT3-library for Harbour.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit3.c, library is libct.
 *  $SEEALSO$
 *      NUMROL(), NUMANDX(), NUMORX(), NUMXORX(), NUMNOTX(), NUMMIRRX()
 *  $END$
 */

HB_FUNC( NUMROLX )
{
  long    lNum1, lNumBak, lPattern, lTestRol;
  USHORT  usBytes, usFor, usNum2;

  if ( ISNUM(2) || ISCHAR(2) )
  {
     lNum1  = __getparam( 2 );           /* Number to do ROL */
     usNum2 = (USHORT) __getparam( 3 );  /* Iterations       */

     sizeofbits( &usBytes, &lPattern, &lTestRol );

     usNum2 = usNum2 % usBytes;          /* Set usNum2 < usBytes  */

     lNumBak = lNum1 & lPattern;         /* lNumBak contain the section 
                                            to doesn't ROL               */

     for (usFor = 1; usFor <= usNum2; usFor++)
     {
        if ( lNum1 & lTestRol )  /* Test if MSB is ON */
        {
           lNum1 = lNum1 << 1;
           lNum1 = lNum1 | 1;    /* Simulate that the MSB move to LSB */
        }
        else
           lNum1 = lNum1 << 1;
     }
                                 /* Set the section not ROLed */
     lNum1 = ( lNum1 & (~lPattern) ) | lNumBak;

     hb_retnl( lNum1 );
  }
  else
     hb_ret( );

}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMMIRRX()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      NUMMIRRX( <nSignificativeBits>, <nNumber|cHexNum> ) -> <nResult>
 *  $ARGUMENTS$
 *      <SignificativeBits> Designate a number in the range of 0 to 32, 
 *           indicating the LSB of nLONGx|cHexLONGx that will be used.
 *
 *      <nLONG | cHexLONG>  Designate either decimal or hexadecimal
 *           number string.
 *
 *  $RETURNS$
 *      NUMMIRR() returns a value by which the bit opposite the first 
 *           parameter is mirrored.
 *
 *  $DESCRIPTION$
 *      This function is similar to NUMMIRR() function with a significative
 *      change. The first parameter indicate the quantity of lower bits of
 *      nLONG are used.
 *      When you mirror bit, bit 1 interchanges with bit <nSignificativeBits>,
 *      bit 2 with bit <nSignificativeBits> - 1, etc..
 *      The not mirrored bits is not moved.
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      NUMMIRRX() is a new function in the CT3-library for Harbour.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit3.c, library is libct.
 *  $SEEALSO$
 *      NUMMIRR(), NUMANDX(), NUMORX(), NUMXORX(), NUMNOTX(), NUMROLX()
 *  $END$
 */

HB_FUNC ( NUMMIRRX )
{
  long    lNum1, lPattern, lTestMSB, lNumBak, lMirror = 0;
  USHORT  usBytes, usFor;

  if ( ISNUM(2) || ISCHAR(2) )
  {
     lNum1 = __getparam( 2 );
  
     sizeofbits( &usBytes, &lPattern, &lTestMSB );

     lNumBak = lNum1 & lPattern;

     for ( usFor = 1; usFor <= usBytes; usFor++ )
     {
       if ( lNum1 & 1 )
       {
                                 
          lMirror = lMirror << 1;  /* if the LSB of lNum1 == 1 then */
          lMirror = lMirror | 1;   /* set the LSB of lMirror = 1    */
       }
       else
          lMirror = lMirror << 1;

       lNum1 = lNum1 >> 1;

     }
     lMirror = ( lMirror & (~lPattern) ) | lNumBak;

     hb_retnl( lMirror );
  }
  else
     hb_ret( );

}


long static __hex2long( char *cNum1, int iLenHex )
{
  int  i;
  int  iNum;
  unsigned long lHexNum = 0;


  i = ( iLenHex - 1 );
  while (( i >= 0 ) && ( iLenHex-i <= 8 ))
  {
     iNum = ((int) cNum1[i]) - 0x30;

     if (iNum > 9)
       iNum -= 7;

     if ((iNum < 0) || (iNum > 0x0F))
       break;
     
     lHexNum += (unsigned long) iNum * (1 << (4 * ( iLenHex - i - 1 )));
     i--;
  }
  return lHexNum;
}


long static __getparam( int iParam )
{

  if ( ISCHAR( iParam ) )
     return  __hex2long( hb_parc( iParam ), hb_parclen( iParam ) );
  else
     return  hb_parnl( iParam );

}


long static __numand( long lNum1, long lNum2 )
{
    return lNum1 & lNum2;
}


long static __numor( long lNum1, long lNum2 )
{
    return lNum1 | lNum2;
}


long static __numxor( long lNum1, long lNum2 )
{
    return lNum1 ^ lNum2;
}


long static __numnot( long lNum1, long lNum2 )
{
    HB_SYMBOL_UNUSED (lNum2);
    return ~lNum1;
}


long static __numfun( int iPCount, long (*operation)(long wNum1, long wNum2), BOOLP pbOk )
{
  long   lNumOp;
  long   lNum1, lNum2;
  long   lPattern, lTestMSB;
  USHORT usBytes;
  int    iFor;

  if ( ISNUM(1) || ISNIL(1) )
  {
     sizeofbits( &usBytes, &lPattern, &lTestMSB );

     if ( ISNUM(2) || ISCHAR(2) )
     {
        lNum1 = __getparam( 2 );

        if ( iPCount == 2 )

  /*  If unary operation: NOT                           */
           lNumOp = (*operation)( lNum1, 0 );

        else
        {

           for ( iFor=3; iFor <= iPCount; iFor++)
           {
              if ( ISNUM( iFor ) || ISCHAR( iFor ) )
              {
                 lNum2 = __getparam( iFor );


  /*  Call to operation: AND, OR, XOR                   */
                 lNumOp = (*operation)( lNum1, lNum2 );

              }
              else
              {
  /*  If error in parameter then return -1              */
                 *pbOk = FALSE;
                 return (-1);
              }

  /*  Copy result to first parameter if multi operation */
              lNum1 = lNumOp;
           }

        }                 

     }
     else
     {

  /*  If error in parameter then return -1              */
        *pbOk = FALSE;
        return (-1);
     }

  /*  Return result of operation */
     lNumOp = (lNumOp & lTestMSB) ? lNumOp | lPattern : lNumOp & (~lPattern);

     *pbOk = TRUE;

     return lNumOp;
  }
  else
  {
     *pbOk = FALSE;
     return (-1);
  }

}

void static sizeofbits( USHORTP pusBytes, long *plPattern, long *plTestMSB )
{

  *pusBytes = ((ISNIL(1) || hb_parni(1) == 0) ? sizeof( int ) * 8 
                                             : (USHORT) hb_parni( 1 ) );

  if ( *pusBytes > sizeof( long ) * 8 )
     *pusBytes = *pusBytes % (sizeof( long ) * 8);

  *plPattern = *pusBytes == ( sizeof( long ) * 8) ? 0 : (-1) << *pusBytes;
  *plTestMSB = *pusBytes == 0 ? 0 : 1 << (*pusBytes - 1);
}


