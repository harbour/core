/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Number and bit manipulation functions: - CLEARBIT()
 *                                              - SETBIT()
 *                                              - ISBIT()
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


long int static __hex2long( char *cNum1, int iLenHex );
long int static __getparam( int iParam );


/*  $DOC$
 *  $FUNCNAME$
 *      CLEARBIT()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      CLEARBIT( <nLONG|cHexLONG>, <nBitPos1>[,...<nBitPos32>] ) -> <nNewValue>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit2.c, library is libct.
 *  $SEEALSO$
 *      SETBIT(), ISBIT()
 *  $END$
 */

HB_FUNC( CLEARBIT )
{
  long int lNumClearBit = -1;
  long int lNum1;
  int      iNum2;
  int iPCount, iFor;

  iPCount = hb_pcount();

  lNum1 = __getparam( 1 );  /* Obtain the parameter and converting from HEXA
                               if necessary */

  if ( ISNUM(1) || ISCHAR(1) )

    for ( iFor=2; iFor <= iPCount; iFor++)
    {
      if ( ISNUM( iFor ) )
      {
         iNum2 = hb_parni( iFor );

         if ( (iNum2 >= 1) && (iNum2 <= 32) )
           /* if bit to clear this between 1 and 32 */
              
           lNumClearBit = lNum1 & ~( 1L << (iNum2 - 1) );

         else
         {
           lNumClearBit = -1L;
           break;
         }
      }
      else
      {
        lNumClearBit = -1L;
        break;
      }
    }
  else
    lNumClearBit = -1L;

  hb_retnl( lNumClearBit );

}


/*  $DOC$
 *  $FUNCNAME$
 *      SETBIT()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      SETBIT( <nLONG|cHexLONG>, <nBitPos1>[,...<nBitPos32>] ) -> <nNewValue>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit2.c, library is libct.
 *  $SEEALSO$
 *      CLEARBIT(), ISBIT()
 *  $END$
 */

HB_FUNC( SETBIT )
{
  long int lNumSetBit = -1L;
  long int lNum1;
  int      iNum2;
  int iPCount, iFor;

  iPCount = hb_pcount();

  lNum1 = __getparam( 1 );  /* Obtain the parameter and converting from HEXA
                               if necessary */

  if ( ISNUM(1) || ISCHAR(1) )

    for ( iFor=2; iFor <= iPCount; iFor++)
    {
      if ( ISNUM( iFor ) )
      {
         iNum2 = hb_parni( iFor );

         if ( (iNum2 >= 1) && (iNum2 <= 32) )
           /* if bit to clear this between 1 and 32 */

           lNumSetBit = lNum1 | ( 1L << (iNum2 - 1) );

         else
         {
           lNumSetBit = -1L;
           break;
         }
      }
      else
      {
        lNumSetBit = -1L;
        break;
      }
    }
  else
    lNumSetBit = -1L;

  hb_retnl( lNumSetBit );

}

/*  $DOC$
 *  $FUNCNAME$
 *      ISBIT()
 *  $CATEGORY$
 *      CT3 number and bit manipulation functions
 *  $ONELINER$
 *  $SYNTAX$
 *      ISBIT( <nLONG|cHexLONG>, <nBitPos> ) -> <lSet>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is bit2.c, library is libct.
 *  $SEEALSO$
 *      CLEARBIT(), SETBIT()
 *  $END$
 */

HB_FUNC ( ISBIT )
{
  long int lNum1;
  int      iTestBit;

  if (( ISNUM( 1 ) || ISCHAR( 1 ) ) && ( ISNUM( 2 ) ) )
  {
     lNum1    = __getparam( 1 );
     iTestBit = hb_parni( 2 );

     hb_retl( lNum1 & ( 1L << (iTestBit - 1) ) );
  }
  else
     hb_retl( FALSE );

}


long int static __hex2long( char *cNum1, int iLenHex )
{
  register int i;
  register int iNum;
  long int     iHexNum = 0;


  i = ( iLenHex - 1 );
  while (( i >= 0 ) && ( iLenHex-i <= 4 ))
  {
     iNum = ((int) cNum1[i]) - 0x30;

     if (iNum > 9)
       iNum -= 7;

     if ((iNum < 0) || (iNum > 0x0F))
       break;
     
     iHexNum += (long int) iNum * (1 << (4 * ( iLenHex - i - 1 )));
     i--;
  }
  return iHexNum;
}


long int static __getparam( int iParam )
{

  if ( ISCHAR( iParam ) )
     return  __hex2long( hb_parc( iParam ), hb_parclen( iParam ) );
  else
     return hb_parnl( iParam );

}


