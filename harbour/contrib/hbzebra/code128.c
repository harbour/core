/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
 * www - http://harbour-project.org
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

#include "hbzebra.h"
#include "hbapiitm.h"
#include "hbapierr.h"


static const unsigned short s_code[] = {
   00633,   /*            00  */
   00663,   /*  !    !    01  */
   01463,   /*  "    "    02  */
   00311,   /*  #    #    03  */
   00611,   /*  $    $    04  */
   00621,   /*  %    %    05  */
   00231,   /*  &    &    06  */
   00431,   /*  '    '    07  */
   00461,   /*  (    (    08  */
   00223,   /*  )    )    09  */
   00423,   /*  *    *    10  */
   00443,   /*  +    +    11  */
   00715,   /*  ,    ,    12  */
   00731,   /*  -    -    13  */
   01631,   /*  .    .    14  */
   00635,   /*  /    /    15  */
   00671,   /*  0    0    16  */
   01471,   /*  1    1    17  */
   01163,   /*  2    2    18  */
   00723,   /*  3    3    19  */
   01623,   /*  4    4    20  */
   00473,   /*  5    5    21  */
   00563,   /*  6    6    22  */
   01667,   /*  7    7    23  */
   00627,   /*  8    8    24  */
   00647,   /*  9    9    25  */
   01447,   /*  :    :    26  */
   00467,   /*  ;    ;    27  */
   00547,   /*  <    <    28  */
   01147,   /*  =    =    29  */
   00333,   /*  >    >    30  */
   01433,   /*  ?    ?    31  */
   01543,   /*  @    @    32  */
   00305,   /*  A    A    33  */
   00321,   /*  B    B    34  */
   01421,   /*  C    C    35  */
   00215,   /*  D    D    36  */
   00261,   /*  E    E    37  */
   01061,   /*  F    F    38  */
   00213,   /*  G    G    39  */
   00243,   /*  H    H    40  */
   01043,   /*  I    I    41  */
   00355,   /*  J    J    42  */
   01615,   /*  K    K    43  */
   01661,   /*  L    L    44  */
   00335,   /*  M    M    45  */
   01435,   /*  N    N    46  */
   01561,   /*  O    O    47  */
   01567,   /*  P    P    48  */
   01613,   /*  Q    Q    49  */
   01643,   /*  R    R    50  */
   00273,   /*  S    S    51  */
   01073,   /*  T    T    52  */
   01673,   /*  U    U    53  */
   00327,   /*  V    V    54  */
   01427,   /*  W    W    55  */
   01507,   /*  X    X    56  */
   00267,   /*  Y    Y    57  */
   01067,   /*  Z    Z    58  */
   01307,   /*  [    [    59  */
   01367,   /*  \    \    60  */
   01023,   /*  ]    ]    61  */
   01217,   /*  ^    ^    62  */
   00145,   /*  _    _    63  */
   00605,   /*  NUL   `   64  */
   00151,   /*  SOH   a   65  */
   01411,   /*  STX   b   66  */
   00641,   /*  ETX   c   67  */
   01441,   /*  EOT   d   68  */
   00115,   /*  ENQ   e   69  */
   00415,   /*  ACK   f   70  */
   00131,   /*  BEL   g   71  */
   01031,   /*  BS    h   72  */
   00541,   /*  HT    i   73  */
   01141,   /*  LF    j   74  */
   01103,   /*  VT    k   75  */
   00123,   /*  FF    l   76  */
   01357,   /*  CR    m   77  */
   00503,   /*  SO    n   78  */
   01361,   /*  SI    o   79  */
   00745,   /*  DLE   p   80  */
   00751,   /*  DC1   q   81  */
   01711,   /*  DC2   r   82  */
   00475,   /*  DC3   s   83  */
   00571,   /*  DC4   t   84  */
   01171,   /*  NAK   u   85  */
   00457,   /*  SYN   v   86  */
   00517,   /*  ETB   w   87  */
   01117,   /*  CAN   x   88  */
   01733,   /*  EM    y   89  */
   01573,   /*  SUB   z   90  */
   01557,   /*  ESC   {   91  */
   00365,   /*  FS    |   92  */
   01705,   /*  GS    }   93  */
   01721,   /*  RS    ~   94  */
   00275,   /*  US   DEL  95  */
   01075,   /*  FNC3 FNC3 96  */
   00257,   /*  FNC2 FNC2 97  */
   01057,   /*  ShiB ShiA 98  */
   01735,   /*  CodC CodC 99  */
   01675,   /*  CodB FNC4 CodB 100 */
   01727,   /*  FNC4 CodA CodA 101 */
   01657,   /*  FNC1 FNC1 FNC1 102 */
   00413,   /*  Start Code A   103 */
   00113,   /*  Start Code B   104 */
   00713};  /*  Start Code C   105 */

#define CODESET_A       0
#define CODESET_B       1

#define START_A       103
#define START_B       104
#define START_C       105

#define SELECT_A      101
#define SELECT_B      100
#define SELECT_C       99

#define SHIFT_AB       98


static int _code128_charno( char ch, int iCodeSet )
{
   if( iCodeSet == CODESET_A )
   {
      if( ch >= ' ' && ch <= '_' )
         return ch - ' ';
      else if( ( unsigned char ) ch <= 31 )
         return ch + 64;
      else
         return -1;
   }
   else if( iCodeSet == CODESET_B )
   {
      if( ch >= ' ' && ( unsigned char ) ch <= 127 )
         return ch - ' ';
      else
         return -1;
   }
   return -1;
}

PHB_ZEBRA hb_zebra_create_code128( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA  pZebra;
   int        i, j, k, csum, iCode, iCodeSet, iCodeLen, iLen = ( int ) nLen;
   int *      pCode;

   HB_SYMBOL_UNUSED( iFlags );

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_CODE128;

   j = 0;
   for( i = 0; i < iLen; i++ )
   {
      if( ( unsigned char ) szCode[ i ] >= 128 )
      {
         pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
         return pZebra;
      }
      if( szCode[ i ] >= ' ' && szCode[ i ] <= 126 )
         j++;
   }

   /* make print string */
   pZebra->szCode = ( char * ) hb_xgrab( j + 1 );
   j = 0;
   for( i = 0; i < iLen; i++ )
   {
      if( szCode[ i ] >= 32 && szCode[ i ] <= 126 )
         pZebra->szCode[ j++ ] = szCode[ i ];
   }
   pZebra->szCode[ j ] = '\0';

   /* generate code set switch characters */
   pCode = ( int * ) hb_xgrab( sizeof( int ) * iLen * 2 );
   iCodeSet = CODESET_B; /* to pacify MSVC warning only. It will be assigned later */
   iCodeLen = 0;
   /* determine the first optimal codeset */
   for( i = 0; i < iLen; i++ )
   {
      if( _code128_charno( szCode[ i ], CODESET_A ) == -1 )
      {
         iCodeSet = CODESET_B;
         pCode[ iCodeLen++ ] = START_B;
         break;
      }
      else if( _code128_charno( szCode[ i ], CODESET_B ) == -1 )
      {
         iCodeSet = CODESET_A;
         pCode[ iCodeLen++ ] = START_A;
         break;
      }
   }
   if( iCodeLen == 0 )
   {
      iCodeSet = CODESET_B;
      pCode[ iCodeLen++ ] = START_B;
   }

   /* encode source data */
   /* Warning: digit optimizer works in optimal way with this encoder code. Be careful
      if you'll change encoder code, digit optimizer canm require adjustment also [Mindaugas] */
   for( i = 0; i < iLen; i++ )
   {
      iCode = _code128_charno( szCode[ i ], iCodeSet );
      if( iCode != -1 )
         pCode[ iCodeLen++ ] = iCode;
      else
      {
         /* We should generate codeset switch instead of shift for the last character.
            This will help digit optimizer to do its job [Mindaugas] */
         if( i + 1 < iLen &&
             _code128_charno( szCode[ i + 1 ], iCodeSet == CODESET_A ? CODESET_B : CODESET_A ) == -1 )
         {
            pCode[ iCodeLen++ ] = SHIFT_AB;
            pCode[ iCodeLen++ ] = _code128_charno( szCode[ i ], iCodeSet == CODESET_A ? CODESET_B : CODESET_A );
         }
         else
         {
            if( iCodeSet == CODESET_A )
            {
               iCodeSet = CODESET_B;
               pCode[ iCodeLen++ ] = SELECT_B;
            }
            else
            {
               iCodeSet = CODESET_A;
               pCode[ iCodeLen++ ] = SELECT_A;
            }
            pCode[ iCodeLen++ ] = _code128_charno( szCode[ i ], iCodeSet );
         }
      }
   }

   /* optimize digits */
   iCodeSet = pCode[ 0 ] == START_A ? CODESET_A : CODESET_B;
   for( i = 1; i < iCodeLen; i++ )
   {
      if( iCodeSet == CODESET_A && pCode[ i ] == SELECT_B )
         iCodeSet = CODESET_B;
      else if( iCodeSet == CODESET_B && pCode[ i ] == SELECT_A )
         iCodeSet = CODESET_A;

      if( 16 <= pCode[ i ] && pCode[ i ] <= 25 )
      {
         for( j = i + 1; j < iCodeLen && 16 <= pCode[ j ] && pCode[ j ] <= 25; j++ );
         if( j - i == 2 && i == 1 && j == iCodeLen )
         {
            /* [StartB] 1 2  -->  [StartC] [12] */
            pCode[ 0 ] = START_C;
            pCode[ 1 ] = ( pCode[ 1 ] - 16 ) * 10 + pCode[ 2 ] - 16;
            iCodeLen = 2;
            break;
         }
         else if( ( j - i >= 4 && ( i == 1 || j == iCodeLen || pCode[ j ] == SELECT_A || pCode[ j ] == SELECT_B ) ) ||
                  j - i >= 6 )
         {
            if( i == 1 )
            {
               /* [StartN] 1 2 3 4  -->  [StartC] [12] [34] */
               /* [StartN] 1 2 3 4 5  -->  [StartC] [12] [34] [CodeN] 5 */
               /* [StartN] 1 2 3 4 X ... -->  [StartC] [12] [34] [CodeN] X ... */
               /* [StartN] 1 2 3 4 5 X ... -->  [StartC] [12] [34] [CodeN] 5 X ... */
               pCode[ 0 ] = START_C;
               for( k = 1; k < j - 1; k += 2 )
                  pCode[ i++ ] = ( pCode[ k ] - 16 ) * 10 + pCode[ k + 1 ] - 16;

               if( k < iCodeLen )
               {
                  j = i;
                  pCode[ i++ ] = iCodeSet == CODESET_A ? SELECT_A : SELECT_B;
                  for( ; k < iCodeLen; k++ )
                     pCode[ i++ ] = pCode[ k ];
               }
               iCodeLen = i;
            }
            else
            {
               /* ... X 1 2 3 4  -->  ... X [CodeC] [12] [34] */
               /* ... X 1 2 3 4 [CodeN] ... -->  ... X [CodeC] [12] [34] [CodeN] ... */
               /* ... X 1 2 3 4 5  -->  ... X 1 [CodeC] [23] [45] */
               /* ... X 1 2 3 4 5 [CodeN] ...  -->  ... X 1 [CodeC] [23] [45] [CodeN] ... */
               /* ... X 1 2 3 4 5 6 Y ...  -->  ... X [CodeC] [12] [34] [56] [CodeN] Y ... */
               /* ... X 1 2 3 4 5 6 7 Y ...  -->  ... X 1 [CodeC] [23] [45] [67] [CodeN] Y ... */

               if( ( j - i ) & 1 )
               {
                  /* digit count is odd */
                  i++;
               }

               pCode[ i + 1 ] = ( pCode[ i ] - 16 ) * 10 + pCode[ i + 1 ] - 16;
               pCode[ i ] = SELECT_C;
               i += 2;
               for( k = i; k < j; k += 2 )
                  pCode[ i++ ] = ( pCode[ k ] - 16 ) * 10 + pCode[ k + 1 ] - 16;
               j = i;
               if( k < iCodeLen )
               {
                  if( pCode[ k ] != SELECT_A && pCode[ k ] != SELECT_B )
                     pCode[ i++ ] = iCodeSet == CODESET_A ? SELECT_A : SELECT_B;

                  for( ; k < iCodeLen; k++ )
                     pCode[ i++ ] = pCode[ k ];
               }
               iCodeLen = i;
            }
         }
         i = j - 1;
      }
   }

   pZebra->pBits = hb_bitbuffer_create();
   csum = pCode[ 0 ];
   for( i = 0; i < iCodeLen; i++ )
   {
      hb_bitbuffer_cat_int( pZebra->pBits, s_code[ pCode[ i ] ], 11 );
      csum += i * pCode[ i ];
   }

   hb_xfree( pCode );
   /* checksum */
   hb_bitbuffer_cat_int( pZebra->pBits, s_code[ csum % 103 ], 11 );

   hb_bitbuffer_cat_int( pZebra->pBits, 0x1AE3, 13 );
   return pZebra;
}


HB_FUNC( HB_ZEBRA_CREATE_CODE128 )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );
   if( pItem )
   {
      hb_zebra_ret( hb_zebra_create_code128( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
