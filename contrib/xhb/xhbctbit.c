/*
 * Number and bit manipulation functions: Num*X()
 *
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbapi.h"

static HB_ULONG hb_hextonum( const char * cHex )
{
   HB_ULONG ulNum = 0;
   char     c;

   while( *cHex == ' ' )
      cHex++;

   while( ( c = *cHex++ ) != 0 )
   {
      ulNum <<= 4;

      if( c >= '0' && c <= '9' )
         ulNum += c - '0';
      else if( c >= 'A' && c <= 'F' )
         ulNum += c - ( 'A' + 10 );
      else if( c >= 'a' && c <= 'f' )
         ulNum += c - ( 'a' + 10 );
      else
      {
         ulNum = 0;
         break;
      }
   }

   return ulNum;
}

static HB_LONG __getparam( int iParam )
{
   const char * szHexNum = hb_parc( iParam );

   if( szHexNum )
      return ( HB_LONG ) hb_hextonum( szHexNum );
   else
      return hb_parnl( iParam );
}

static HB_LONG __numand( HB_LONG lNum1, HB_LONG lNum2 )
{
   return lNum1 & lNum2;
}

static HB_LONG __numor( HB_LONG lNum1, HB_LONG lNum2 )
{
   return lNum1 | lNum2;
}

static HB_LONG __numxor( HB_LONG lNum1, HB_LONG lNum2 )
{
   return lNum1 ^ lNum2;
}

static HB_LONG __numnot( HB_LONG lNum1, HB_LONG lNum2 )
{
   HB_SYMBOL_UNUSED( lNum2 );

   return ~lNum1;
}

static void sizeofbits( HB_USHORT * pusBytes, HB_LONG * plPattern, HB_LONG * plTestMSB )
{
   *pusBytes = ( HB_ISNIL( 1 ) || hb_parni( 1 ) == 0 ) ?
               sizeof( int ) * 8 : ( HB_USHORT ) hb_parni( 1 );

   if( *pusBytes > sizeof( HB_LONG ) * 8 )
      *pusBytes = *pusBytes % ( sizeof( HB_LONG ) * 8 );

   *plPattern = ( *pusBytes == ( sizeof( HB_LONG ) * 8 ) ) ? 0 : ( HB_LONG ) ( ULONG_MAX << *pusBytes );

   *plTestMSB = ( *pusBytes == 0 ) ? 0 : ( 1 << ( *pusBytes - 1 ) );
}

static HB_LONG __numfun( int iPCount, HB_LONG ( * operation )( HB_LONG wNum1, HB_LONG wNum2 ), HB_BOOL * pbOk )
{
   if( ( HB_ISNUM( 1 ) || HB_ISNIL( 1 ) ) &&
       ( HB_ISNUM( 2 ) || HB_ISCHAR( 2 ) ) )
   {
      HB_LONG   lNum1 = __getparam( 2 );
      HB_LONG   lNumOp = 0;
      HB_LONG   lPattern, lTestMSB;
      HB_USHORT usBytes;

      sizeofbits( &usBytes, &lPattern, &lTestMSB );

      if( iPCount == 2 )
         lNumOp = ( *operation )( lNum1, 0 );  /* If unary operation: NOT */
      else
      {
         int iFor;

         for( iFor = 3; iFor <= iPCount; iFor++ )
         {
            if( HB_ISNUM( iFor ) || HB_ISCHAR( iFor ) )
               lNumOp = ( *operation )( lNum1, __getparam( iFor ) );  /* Call to operation: AND, OR, XOR */
            else
            {
               *pbOk = HB_FALSE;
               return 0;
            }

            lNum1 = lNumOp;  /* Copy result to first parameter if multi operation */
         }
      }

      *pbOk = HB_TRUE;
      return ( lNumOp & lTestMSB ) ? lNumOp | lPattern : lNumOp & ~lPattern;
   }

   *pbOk = HB_FALSE;
   return 0;
}

HB_FUNC( NUMANDX )
{
   HB_BOOL bOk;
   HB_LONG lNumOp = __numfun( hb_pcount(), __numand, &bOk );

   if( bOk )
      hb_retnl( lNumOp );
}

HB_FUNC( NUMORX )
{
   HB_BOOL bOk;
   HB_LONG lNumOp = __numfun( hb_pcount(), __numor, &bOk );

   if( bOk )
      hb_retnl( lNumOp );
}

HB_FUNC( NUMXORX )
{
   HB_BOOL bOk;
   HB_LONG lNumOp = __numfun( 3, __numxor, &bOk );

   if( bOk )
      hb_retnl( lNumOp );
}

HB_FUNC( NUMNOTX )
{
   HB_BOOL bOk;
   HB_LONG lNumOp = __numfun( 2, __numnot, &bOk );

   if( bOk )
      hb_retnl( lNumOp );
}

HB_FUNC( NUMROLX )
{
   if( HB_ISNUM( 2 ) || HB_ISCHAR( 2 ) )
   {
      HB_LONG   lNum1, lNumBak, lPattern, lTestRol;
      HB_USHORT usBytes, usFor, usNum2;

      lNum1  = __getparam( 2 );                /* Number to do ROL */
      usNum2 = ( HB_USHORT ) __getparam( 3 );  /* Iterations */

      sizeofbits( &usBytes, &lPattern, &lTestRol );

      usNum2 %= usBytes;   /* Set usNum2 < usBytes */

      lNumBak = lNum1 & lPattern;  /* lNumBak contain the section to doesn't ROL */

      for( usFor = 1; usFor <= usNum2; usFor++ )
      {
         if( lNum1 & lTestRol )  /* Test if MSB is ON */
         {
            lNum1 <<= 1;
            lNum1  |= 1;  /* Simulate that the MSB move to LSB */
         }
         else
            lNum1 <<= 1;
      }

      hb_retnl( ( lNum1 & ~lPattern ) | lNumBak );  /* Set the section not ROLed */
   }
}

HB_FUNC( NUMMIRRX )
{
   if( HB_ISNUM( 2 ) || HB_ISCHAR( 2 ) )
   {
      HB_LONG   lNum1, lPattern, lTestMSB, lNumBak, lMirror = 0;
      HB_USHORT usBytes, usFor;

      lNum1 = __getparam( 2 );

      sizeofbits( &usBytes, &lPattern, &lTestMSB );

      lNumBak = lNum1 & lPattern;

      for( usFor = 1; usFor <= usBytes; usFor++ )
      {
         if( lNum1 & 1 )
         {
            lMirror <<= 1;  /* if the LSB of lNum1 == 1 then */
            lMirror  |= 1;  /* set the LSB of lMirror = 1 */
         }
         else
            lMirror <<= 1;

         lNum1 >>= 1;
      }

      hb_retnl( ( lMirror & ~lPattern ) | lNumBak );
   }
}
