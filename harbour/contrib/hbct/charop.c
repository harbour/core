/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - CHARADD()
 *     - CHARAND()
 *     - CHARNOT()
 *     - CHAROR()
 *     - CHARXOR()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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

#include "ct.h"

/* helper function */
void ct_charop( int iMode )
{
   /* suppressing return value ? */
   int iNoRet = ct_getref() && HB_ISBYREF( 1 );

   if( HB_ISCHAR( 1 ) )
   {
      HB_SIZE sStrLen = hb_parclen( 1 );
      HB_SIZE sPos;
      unsigned char * pucString = ( unsigned char * ) hb_parc( 1 );
      unsigned char * pucResult;

      if( sStrLen == 0 )
      {
         if( iNoRet )
            hb_ret();
         else
            hb_retc_null();
         return;
      }

      pucResult = ( unsigned char * ) hb_xgrab( sStrLen + 1 );

      switch ( iMode )
      {
         /* NOT */
         case CT_CHAROP_CHARNOT:
            for( sPos = 0; sPos < sStrLen; ++sPos )
               pucResult[ sPos ] = ~pucString[ sPos ];
            break;

         /* SHL */
         case CT_CHAROP_CHARSHL:
         {
            int iSHL = hb_parni( 2 ) % 8;   /* defaults to 0 */

            if( iSHL == 0 )
               hb_xmemcpy( pucResult, pucString, sStrLen );
            else
               for( sPos = 0; sPos < sStrLen; ++sPos )
                  pucResult[ sPos ] = pucString[ sPos ] << iSHL;
            break;
         }

         /* SHR */
         case CT_CHAROP_CHARSHR:
         {
            int iSHR = hb_parni( 2 ) % 8;   /* defaults to 0 */

            if( iSHR == 0 )
               hb_xmemcpy( pucResult, pucString, sStrLen );
            else
               for( sPos = 0; sPos < sStrLen; ++sPos )
                  pucResult[ sPos ] = pucString[ sPos ] >> iSHR;
            break;
         }

         /* RLL */
         case CT_CHAROP_CHARRLL:
         {
            int iRLL = hb_parni( 2 ) % 8;   /* defaults to 0 */

            hb_xmemcpy( pucResult, pucString, sStrLen );

            if( iRLL != 0 )
               for( sPos = 0; sPos < sStrLen; ++sPos )
               {
                  int iRLLCnt;

                  for( iRLLCnt = 0; iRLLCnt < iRLL; iRLLCnt++ )
                     if( pucResult[ sPos ] & 0x80 )  /* most left bit set -> roll over */
                     {
                        pucResult[ sPos ] <<= 1;
                        pucResult[ sPos ] |= 0x01;
                     }
                     else
                     {
                        pucResult[ sPos ] <<= 1;
                     }
               }
            break;
         }

         /* RLR */
         case CT_CHAROP_CHARRLR:
         {
            int iRLR = hb_parni( 2 ) % 8;   /* defaults to 0 */

            hb_xmemcpy( pucResult, pucString, sStrLen );

            if( iRLR != 0 )
            {
               for( sPos = 0; sPos < sStrLen; ++sPos )
               {
                  int iRLRCnt;

                  for( iRLRCnt = 0; iRLRCnt < iRLR; iRLRCnt++ )
                  {
                     if( pucResult[ sPos ] & 0x01 )  /* most right bit set -> roll over */
                     {
                        pucResult[ sPos ] >>= 1;
                        pucResult[ sPos ] |= 0x80;
                     }
                     else
                        pucResult[ sPos ] >>= 1;
                  }
               }
            }
            break;
         }

         /* ADD */
         case CT_CHAROP_CHARADD:
         {
            if( HB_ISCHAR( 2 ) )
            {
               const char * pucString2 = hb_parc( 2 );
               HB_SIZE sStrLen2 = hb_parclen( 2 );

               for( sPos = 0; sPos < sStrLen; ++sPos )
                  pucResult[ sPos ] = ( char ) ( pucString[ sPos ] + pucString2[ sStrLen2 ? ( sPos % sStrLen2 ) : 0 ] );
            }
            else
            {
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARADD,
                            NULL, HB_ERR_FUNCNAME, 0,
                            EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_xmemcpy( pucResult, pucString, sStrLen );
            }
            break;
         }

         /* SUB */
         case CT_CHAROP_CHARSUB:
         {
            if( HB_ISCHAR( 2 ) )
            {
               const char * pucString2 = hb_parc( 2 );
               HB_SIZE sStrLen2 = hb_parclen( 2 );

               for( sPos = 0; sPos < sStrLen; ++sPos )
                  pucResult[ sPos ] = ( char ) ( pucString[ sPos ] - pucString2[ sStrLen2 ? ( sPos % sStrLen2 ) : 0 ] );
            }
            else
            {
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARSUB,
                            NULL, HB_ERR_FUNCNAME, 0,
                            EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_xmemcpy( pucResult, pucString, sStrLen );
            }
            break;
         }

         /* AND */
         case CT_CHAROP_CHARAND:
         {
            if( HB_ISCHAR( 2 ) )
            {
               const char * pucString2 = hb_parc( 2 );
               HB_SIZE sStrLen2 = hb_parclen( 2 );

               for( sPos = 0; sPos < sStrLen; ++sPos )
                  pucResult[ sPos ] = ( char ) ( pucString[ sPos ] & pucString2[ sStrLen2 ? ( sPos % sStrLen2 ) : 0 ] );
            }
            else
            {
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARAND, NULL, HB_ERR_FUNCNAME, 0,
                            EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_xmemcpy( pucResult, pucString, sStrLen );
            }
            break;
         }

         /* OR */
         case CT_CHAROP_CHAROR:
         {
            if( HB_ISCHAR( 2 ) )
            {
               const char * pucString2 = hb_parc( 2 );
               HB_SIZE sStrLen2 = hb_parclen( 2 );

               for( sPos = 0; sPos < sStrLen; ++sPos )
                  pucResult[ sPos ] = ( char ) ( pucString[ sPos ] | pucString2[ sStrLen2 ? ( sPos % sStrLen2 ) : 0 ] );
            }
            else
            {
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHAROR, NULL, HB_ERR_FUNCNAME, 0,
                            EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_xmemcpy( pucResult, pucString, sStrLen );
            }
            break;
         }

         /* XOR */
         case CT_CHAROP_CHARXOR:
         {
            if( HB_ISCHAR( 2 ) )
            {
               const char * pucString2 = hb_parc( 2 );
               HB_SIZE sStrLen2 = hb_parclen( 2 );

               for( sPos = 0; sPos < sStrLen; ++sPos )
                  pucResult[ sPos ] = ( char ) ( pucString[ sPos ] ^ pucString2[ sStrLen2 ? ( sPos % sStrLen2 ) : 0 ] );
            }
            else
            {
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARXOR, NULL, HB_ERR_FUNCNAME, 0,
                            EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_xmemcpy( pucResult, pucString, sStrLen );
            }
            break;
         }
      }  /* endswitch( iMode ) */

      if( HB_ISBYREF( 1 ) )
         hb_storclen( ( char * ) pucResult, sStrLen, 1 );

      if( iNoRet )
         hb_xfree( pucResult );
      else
         hb_retclen_buffer( ( char * ) pucResult, sStrLen );
   }
   else  /* if( HB_ISCHAR( 1 ) ) */
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      int iError = 0;

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         switch ( iMode )
         {
            case CT_CHAROP_CHARADD:
               iError = CT_ERROR_CHARADD;
               break;

            case CT_CHAROP_CHARSUB:
               iError = CT_ERROR_CHARSUB;
               break;

            case CT_CHAROP_CHARAND:
               iError = CT_ERROR_CHARAND;
               break;

            case CT_CHAROP_CHARNOT:
               iError = CT_ERROR_CHARNOT;
               break;

            case CT_CHAROP_CHAROR:
               iError = CT_ERROR_CHAROR;
               break;

            case CT_CHAROP_CHARXOR:
               iError = CT_ERROR_CHARXOR;
               break;

            case CT_CHAROP_CHARSHL:
               iError = CT_ERROR_CHARSHL;
               break;

            case CT_CHAROP_CHARSHR:
               iError = CT_ERROR_CHARSHR;
               break;

            case CT_CHAROP_CHARRLL:
               iError = CT_ERROR_CHARRLL;
               break;

            case CT_CHAROP_CHARRLR:
               iError = CT_ERROR_CHARRLR;
               break;
         }
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG, iError,
                                  NULL, HB_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                  HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_ret();
   }
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARADD()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Adds corresponding ASCII value of two strings
 *  $SYNTAX$
 *      CHARADD (<[@]cString1>, <cString2>) --> cAddString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cAddString>    string with added ASCII values
 *  $DESCRIPTION$
 *      The CHARADD() function constructs a new string from the two strings
 *      passed as parameters. To do this, it adds the ASCII values of the
 *      corresponding characters of both strings and places a character in
 *      the resulting string whose ASCII value equals to that sum (modulo 256).
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      ? charadd ("012345678", chr(1)) --> "123456789"
 *      ? charadd ("123456789", chr(255)) --> "012345678"
 *      ? charadd ("0000", chr(0)+chr(1)+chr(2)+chr(3)) --> "0123"
 *  $TESTS$
 *      charadd ("012345678", chr(1)) == "123456789"
 *      charadd ("012345678", chr(1)+chr(2)) == "133557799"
 *      charadd ("123456789", chr(255)) == "012345678"
 *      charadd ("123456789", chr(255)+chr(254)) == "002244668"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARADD() is compatible with CT3's CHARADD().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARSUB()   CHARAND()   CHARNOT()
 *      CHAROR()    CHARXOR()   CHARSHL()
 *      CHARSHR()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARADD )
{
   ct_charop( CT_CHAROP_CHARADD );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARAND()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Combine corresponding ASCII value of two strings with bitwise AND
 *  $SYNTAX$
 *      CHARAND (<[@]cString1>, <cString2>) --> cAndString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cAndString>    string with bitwise AND combined ASCII values
 *  $DESCRIPTION$
 *      The CHARAND() function constructs a new string from the two strings
 *      passed as parameters. To do this, it combines the ASCII values of the
 *      corresponding characters of both strings with a bitwise AND-operation
 *      and places a character in the resulting string whose ASCII value
 *      equals to the result of that operation.
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      // clear the LSB
 *      ? charand ("012345678", chr(254)) --> "002244668"
 *      ? charand ("012345678", chr(254)+chr(252)) --> "002044648"
 *  $TESTS$
 *      charand ("012345678", chr(254)) == "002244668"
 *      charand ("012345678", chr(254)+chr(252)) == "002044648"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARAND() is compatible with CT3's CHARAND().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARNOT()
 *      CHAROR()    CHARXOR()   CHARSHL()
 *      CHARSHR()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARAND )
{
   ct_charop( CT_CHAROP_CHARAND );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARNOT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Process each character in a string with bitwise NOT operation
 *  $SYNTAX$
 *      CHARNOT (<[@]cString>) --> cNotString
 *  $ARGUMENTS$
 *      <[@]cString>    string to be processed
 *  $RETURNS$
 *      <cNotString>    string with bitwise negated characters
 *  $DESCRIPTION$
 *      The CHARNOT() function constructs a new string from the string
 *      passed as parameter. To do this, it performs a bitwise NOT operation
 *      to the characters of the string and places a character in
 *      the resulting string whose ASCII value equals to the result of that
 *      operation. It can be easily seen that the resulting ASCII-value equals
 *      255 minus input ASCII value.
 *      If the string is passed by reference, the resulting string is
 *      stored in <cString>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *  $EXAMPLES$
 *      ? charnot (chr(85)+chr(128)+chr(170)+chr(1)) --> chr(170)+chr(127)+chr(85)+chr(254)
 *      ? charnot (charnot ("This is a test!")) --> "This is a test!"
 *  $TESTS$
 *      charnot (chr(85)+chr(128)+chr(170)+chr(1)) == chr(170)+chr(127)+chr(85)+chr(254)
 *      charnot (charnot ("This is a test!")) == "This is a test!"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARNOT() is compatible with CT3's CHARNOT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARAND()
 *      CHAROR()    CHARXOR()   CHARSHL()
 *      CHARSHR()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARNOT )
{
   ct_charop( CT_CHAROP_CHARNOT );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHAROR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Combine corresponding ASCII value of two strings with bitwise OR
 *  $SYNTAX$
 *      CHAROR (<[@]cString1>, <cString2>) --> cOrString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cOrString>     string with bitwise OR combined ASCII values
 *  $DESCRIPTION$
 *      The CHAROR() function constructs a new string from the two strings
 *      passed as parameters. To do this, it combines the ASCII values of the
 *      corresponding characters of both strings with a bitwise OR-operation
 *      and places a character in the resulting string whose ASCII value
 *      equals to the result of that operation.
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      // set the LSB
 *      ? charor ("012345678", chr(1)) --> "113355779"
 *      ? charor ("012345678", chr(1)+chr(3)) --> "133357779"
 *  $TESTS$
 *      charor ("012345678", chr(1)) == "113355779"
 *      charor ("012345678", chr(1)+chr(3)) == "133357779"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHAROR() is compatible with CT3's CHAROR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARNOT()
 *      CHARAND()   CHARXOR()   CHARSHL()
 *      CHARSHR()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHAROR )
{
   ct_charop( CT_CHAROP_CHAROR );
}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARXOR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Combine corresponding ASCII value of two strings with bitwise XOR
 *  $SYNTAX$
 *      CHARXOR (<[@]cString1>, <cString2>) --> cXOrString
 *  $ARGUMENTS$
 *      <[@]cString1>   first string
 *      <cString2>      second string
 *  $RETURNS$
 *      <cXOrString>    string with bitwise XOR combined ASCII values
 *  $DESCRIPTION$
 *      The CHARXOR() function constructs a new string from the two strings
 *      passed as parameters. To do this, it combines the ASCII values of the
 *      corresponding characters of both strings with a bitwise XOR-operation
 *      and places a character in the resulting string whose ASCII value
 *      equals to the result of that operation.
 *      If the first string is passed by reference, the resulting string is
 *      stored in <cString1>, too. By setting the CSETREF()-switch to .T.,
 *      the return value can be omitted.
 *      If <cString2> is shorter than <cString1> and the last character of
 *      <cString2> has been processed, the function restarts with the first
 *      character of <cString2>.
 *  $EXAMPLES$
 *      // easy encryption
 *      ? charxor ("This is top secret !", "My Password") --> <encrypted sentence>
 *  $TESTS$
 *      charxor (charxor ("This is top secret !", "My Password"), "My Password") == "This is top secret !"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARXOR() is compatible with CT3's CHARXOR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charop.c, library is ct3.
 *  $SEEALSO$
 *      CHARADD()   CHARSUB()   CHARNOT()
 *      CHARAND()   CHAROR()    CHARSHL()
 *      CHARSHR()   CHARRLL()   CHARRLR()
 *      CSETREF()
 *  $END$
 */

HB_FUNC( CHARXOR )
{
   ct_charop( CT_CHAROP_CHARXOR );
}
