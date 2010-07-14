/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *
 *   - AFTERATNUM()
 *   - BEFORATNUM()
 *   - ATNUM()
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

#define DO_ATNUM_AFTERATNUM      0
#define DO_ATNUM_BEFORATNUM      1
#define DO_ATNUM_ATNUM           2

/* helper function */
static void do_atnum( int iSwitch )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char *pcStringToMatch = hb_parc( 1 );
      HB_SIZE sStrToMatchLen = hb_parclen( 1 );
      const char *pcString = hb_parc( 2 );
      HB_SIZE sStrLen = hb_parclen( 2 );
      int iMultiPass = ct_getatmupa();
      int iAtLike = ct_getatlike();
      char cAtLike = ct_getatlikechar();
      HB_SIZE sIgnore, sMatchStrLen = 0;
      HB_SIZE nCounter;
      const char *pc = NULL;

      /* eventually ignore some characters */
      if( HB_ISNUM( 4 ) )
         sIgnore = hb_parns( 4 );
      else
         sIgnore = 0;

      if( sIgnore >= sStrLen )
      {
         switch ( iSwitch )
         {
            case DO_ATNUM_AFTERATNUM:
            {
               /* AFTERATNUM */
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_AFTERATNUM, NULL,
                            HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_retc_null();
               break;
            }
            case DO_ATNUM_BEFORATNUM:
            {
               /* BEFORATNUM */
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_BEFORATNUM, NULL,
                            HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_retc_null();
               break;
            }
            case DO_ATNUM_ATNUM:
            {
               /* ATNUM */
               int iArgErrorMode = ct_getargerrormode();

               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATNUM, NULL, HB_ERR_FUNCNAME, 0,
                            EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
               }
               hb_retns( 0 );
               break;
            }
         }
         return;
      }
      else
      {
         pcString += sIgnore;
         sStrLen -= sIgnore;
      }

      /* nth match or last match ? */
      if( HB_ISNUM( 3 ) && ( nCounter = hb_parns( 3 ) ) != 0 )
      {
         /* find the <nCounter>th match */
         const char *pcSubStr;
         HB_SIZE sSubStrLen;
         HB_SIZE nMatchCounter = 0;

         pcSubStr = pcString;
         sSubStrLen = sStrLen;

         while( nMatchCounter < nCounter )
         {
            switch ( iAtLike )
            {
               case CT_SETATLIKE_EXACT:
                  pc = ct_at_exact_forward( pcSubStr, sSubStrLen, pcStringToMatch,
                                            sStrToMatchLen, &sMatchStrLen );
                  break;

               case CT_SETATLIKE_WILDCARD:
                  pc = ct_at_wildcard_forward( pcSubStr, sSubStrLen, pcStringToMatch,
                                               sStrToMatchLen, cAtLike, &sMatchStrLen );
                  break;

               default:
                  pc = NULL;
            }

            if( pc == NULL )
            {
               /* no match found; if this happens at this point,
                  there are no <nCounter> matches, so return an empty string */
               switch ( iSwitch )
               {
                  case DO_ATNUM_AFTERATNUM:
                  case DO_ATNUM_BEFORATNUM:
                     /* AFTERATNUM */
                     /* BEFORATNUM */
                     hb_retc_null();
                     break;

                  case DO_ATNUM_ATNUM:
                     /* ATNUM */
                     hb_retns( 0 );
                     break;
               }
               return;
            }
            nMatchCounter++;

            if( iMultiPass )
               pcSubStr = pc + 1;
            else
               pcSubStr = pc + sMatchStrLen;
            sSubStrLen = sStrLen - ( pcSubStr - pcString );
         }
      }
      else /* ( HB_ISNUM( 3 ) && ( nCounter = hb_parns( 3 ) ) != 0 ) */
      {
         /* we have to find the last match and return the
            string after that last match */
         switch ( iAtLike )
         {
            case CT_SETATLIKE_EXACT:
               pc = ct_at_exact_backward( pcString, sStrLen, pcStringToMatch,
                                          sStrToMatchLen, &sMatchStrLen );
               break;

            case CT_SETATLIKE_WILDCARD:
               pc = ct_at_wildcard_backward( pcString, sStrLen, pcStringToMatch,
                                             sStrToMatchLen, cAtLike, &sMatchStrLen );
               break;

            default:
               pc = NULL;
         }
         if( pc == NULL )
         {
            /* no matches found */
            switch ( iSwitch )
            {
               case DO_ATNUM_AFTERATNUM:
               case DO_ATNUM_BEFORATNUM:
                  /* AFTERATNUM */
                  /* BEFORATNUM */
                  hb_retc_null();
                  break;

               case DO_ATNUM_ATNUM:
                  /* ATNUM */
                  hb_retns( 0 );
                  break;
            }
            return;
         }
      }

      switch ( iSwitch )
      {
         case DO_ATNUM_AFTERATNUM:
            /* AFTERATNUM */
            if( pc + sMatchStrLen >= pcString + sStrLen )
               hb_retc_null();
            else
               hb_retclen( pc + sMatchStrLen, sStrLen - ( pc + sMatchStrLen - pcString ) );
            break;

         case DO_ATNUM_BEFORATNUM:
            /* BEFORATNUM */
            hb_retclen( pcString - sIgnore, pc - ( pcString - sIgnore ) );
            break;

         case DO_ATNUM_ATNUM:
            /* ATNUM */
#if defined( __POCC__ ) && ( __POCC__ >= 500 ) && defined( HB_OS_WIN_64 )
            /* NOTE: Workaround for Pelles C 5.00.13 AMD64 mode internal error:
                     'fatal error: Internal error: reduce_tree()' [vszakats]. */
            hb_retns( pc - pcString + sIgnore + 1 );
#else
            hb_retns( pc - ( pcString - sIgnore ) + 1 );
#endif
            break;
      }
   }
   else                         /* ((ISCHAR (1)) && (ISCHAR (2))) */
   {
      switch ( iSwitch )
      {
         case DO_ATNUM_AFTERATNUM:
         case DO_ATNUM_BEFORATNUM:
         {
            /* AFTERATNUM */
            PHB_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch ==
                                        DO_ATNUM_AFTERATNUM ? CT_ERROR_AFTERATNUM :
                                        CT_ERROR_BEFORATNUM, NULL, HB_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
            }

            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else
               hb_retc_null();
            break;
         }
         case DO_ATNUM_ATNUM:
         {
            /* ATNUM */
            PHB_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATNUM,
                                        NULL, HB_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE,
                                        HB_ERR_ARGS_BASEPARAMS );
            }

            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else
               hb_retns( 0 );
            break;
         }
      }
   }
}


/*  $DOC$
 *  $FUNCNAME$
 *      AFTERATNUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns string portion after nth occurence of substring
 *  $SYNTAX$
 *      AFTERATNUM (<cStringToMatch>, <cString>, [<nCounter>],
 *                  [<nIgnore>] ) --> cRestString
 *  $ARGUMENTS$
 *      <cStringToMatch>    is the substring scanned for
 *      <cString>           is the scanned string
 *      [<nCounter>]        determines how many occurences are of
 *                          <cStringToMatch> in <cString> are searched
 *                          Default: search last occurence
 *      [<nIgnore>]         determines how many character from the start
 *                          should be ignored in the search
 *                          Default: 0
 *  $RETURNS$
 *      <cRestString>       the portion of <cString> after the <nCounter>th
 *                          occurence of <cStringToMatch> in <cString>
 *                          If such a rest does not exist, an empty string
 *                          is returned.
 *  $DESCRIPTION$
 *      This function scans <cString> for <cStringToMatch>. After the
 *      <nCounter>th match (or the last one, depending on the value of
 *      <nCounter>) has been found, the portion of
 *      <cString> after that match will be returned. If there aren't enough
 *      matches or the last match is identical to the end of <cString>, an
 *      empty string will be returned.
 *      After a match has been found, the function continues to scan after
 *      that match if the CSETATMUPA() switch is turned off, with the
 *      second character of the matched substring otherwise.
 *      The function will also consider the settings of SETATLIKE().
 *  $EXAMPLES$
 *      ? AFTERATNUM ("!", "What is the answer ? 4 ! 5 !") -> ""
 *      ? AFTERATNUM ("!", "What is the answer ? 4 ! 5 ?") -> " 5 ?"
 *      <TODO: add some examples here with csetatmupa() and setatlike()>
 *  $TESTS$
 *      AFTERATNUM ("..", "..This..is..a..test!") == "test!"
 *      AFTERATNUM ("..", "..This..is..a..test!", 2) == "is..a..test!"
 *      AFTERATNUM ("..", "..This..is..a..test!", 2, 2) == "a..test!"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      AFTERATNUM() is compatible with CT3's AFTERATNUM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is atnum.c, library is libct.
 *  $SEEALSO$
 *      ATNUM(),BEFORATNUM(),CSETATMUPA(),SETATLIKE()
 *  $END$
 */

HB_FUNC( AFTERATNUM )
{
   do_atnum( DO_ATNUM_AFTERATNUM );
}


/*  $DOC$
 *  $FUNCNAME$
 *      BEFORATNUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns string portion before nth occurence of substring
 *  $SYNTAX$
 *      BEFORATNUM (<cStringToMatch>, <cString>, [<nCounter>],
 *                  [<nIgnore>] ) --> cRestString
 *  $ARGUMENTS$
 *      <cStringToMatch>    is the substring scanned for
 *      <cString>           is the scanned string
 *      [<nCounter>]        determines how many occurences are of
 *                          <cStringToMatch> in <cString> are searched
 *                          Default: search last occurence
 *      [<nIgnore>]         determines how many character from the start
 *                          should be ignored in the search
 *                          Default: 0
 *  $RETURNS$
 *      <cRestString>       the portion of <cString> before the <nCounter>th
 *                          occurence of <cStringToMatch> in <cString>
 *                          If such a string does not exist, an empty string
 *                          is returned.
 *  $DESCRIPTION$
 *      This function scans <cString> for <cStringToMatch>. After the
 *      <nCounter>th match (or the last one, depending on the value of
 *      <nCounter>) has been found, the portion of
 *      <cString> before that match will be returned. If there aren't enough
 *      matches or the last match is identical to the start of <cString>
 *      (i.e. the last match is the first match), an empty string will be returned.
 *      After a match has been found, the function continues to scan after
 *      that match if the CSETATMUPA() switch is turned off, with the
 *      second character of the matched substring otherwise.
 *      The function will also consider the settings of SETATLIKE().
 *  $EXAMPLES$
 *      ? BEFORATNUM ("!", "What is the answer ? 4 ! 5 !") -> "What is the answer ? 4 ! 5 "
 *      ? BEFORATNUM ("!", "What is the answer ? 4 ! 5 ?") -> "What is the answer ? 4 "
 *      <TODO: add some examples here with csetatmupa() and setatlike()>
 *  $TESTS$
 *      BEFORATNUM ("..", "..This..is..a..test!") == "..This..is..a"
 *      BEFORATNUM ("..", "..This..is..a..test!", 2) == "..This"
 *      BEFORATNUM ("..", "..This..is..a..test!", 2, 2) == "..This..is"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      BEFORATNUM() is compatible with CT3's BEFORATNUM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is atnum.c, library is ct3.
 *  $SEEALSO$
 *      ATNUM()  AFTERATNUM()  CSETATMUPA()  SETATLIKE()
 *  $END$
 */

HB_FUNC( BEFORATNUM )
{
   do_atnum( DO_ATNUM_BEFORATNUM );
}


/*  $DOC$
 *  $FUNCNAME$
 *      ATNUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns the start position of the nth occurence of a substring in a string
 *  $SYNTAX$
 *      ATNUM (<cStringToMatch>, <cString>, [<nCounter>],
 *             [<nIgnore>] ) --> nPosition
 *  $ARGUMENTS$
 *      <cStringToMatch>    is the substring scanned for
 *      <cString>           is the scanned string
 *      [<nCounter>]        determines how many occurences are of
 *                          <cStringToMatch> in <cString> are searched
 *                          Default: search last occurence
 *      [<nIgnore>]         determines how many character from the start
 *                          should be ignored in the search
 *                          Default: 0
 *  $RETURNS$
 *      <nPosition>         the position of the <nCounter>th
 *                          occurence of <cStringToMatch> in <cString>.
 *                          If such an occurence does not exist, 0
 *                          is returned.
 *  $DESCRIPTION$
 *      This function scans <cString> for <cStringToMatch>. After the
 *      <nCounter>th match (or the last one, depending on the value of
 *      <nCounter>) has been found, the position of
 *      that match will be returned. If there aren't enough
 *      matches or there is no last match, 0 will be returned.
 *      After a match has been found, the function continues to scan after
 *      that match if the CSETATMUPA() switch is turned off, with the
 *      second character of the matched substring otherwise.
 *      The function will also consider the settings of SETATLIKE().
 *  $EXAMPLES$
 *      ? ATNUM ("!", "What is the answer ? 4 ! 5 !") -> 28
 *      ? ATNUM ("!", "What is the answer ? 4 ! 5 ?") -> 24
 *      <TODO: add some examples here with csetatmupa() and setatlike()>
 *  $TESTS$
 *      ATNUM ("..", "..This..is..a..test!") == 14
 *      ATNUM ("..", "..This..is..a..test!", 2) == 7
 *      ATNUM ("..", "..This..is..a..test!", 2, 2) == 11
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ATNUM() is compatible with CT3's ATNUM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is atnum.c, library is libct.
 *  $SEEALSO$
 *      ATNUM()  AFTERATNUM()  CSETATMUPA()  SETATLIKE()
 *  $END$
 */

HB_FUNC( ATNUM )
{
   do_atnum( DO_ATNUM_ATNUM );
}
