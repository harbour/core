/*
 * $Id$
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#define abs( a )  ( ( ( a ) > 0 ) ? ( a ) : -( a ) )

static const char * s_cSearch = "INEDTIERESTEON";
static const char * s_cRepl   = "[\\]^_`a";

/*
 *   Function: xForm()
 *    Purpose: Internal function to translate words to dictionary
 *  Arguments: cWord  - upper case word to format
 *    Returns: cXformed - translated word
 *
 *      Notes: I'm assuming that the passed word won't exceed 128 bytes.
 **************************/
HB_FUNC( XFORM )
{
   char cRet[ 128 ];
   char *       cPtr     = cRet;
   HB_ISIZ      iRetLen  = 0;
   const char * cWord    = hb_parc( 1 ) + 2;
   HB_ISIZ      iWordLen = hb_parclen( 1 ) - 2;
   HB_ISIZ      x;
   HB_UINT      iKey;

   while( --iWordLen >= 1 && iRetLen < 128 )
   {
      iKey = *( ( HB_UINT * ) cWord );
      for( x = 0; x < 14; x += 2 )
         if( *( ( HB_UINT * ) ( s_cSearch + x ) ) == iKey )
         {
            if( x == 0 )
            {
               if( *( cWord + 2 ) != 'G' )
               {
                  x = 14;
                  break;
               }
               else
               {
                  iWordLen--;
                  cWord++;
               }
            }
            iWordLen--;
            cWord  += 2;
            *cPtr++ = *( s_cRepl + ( x >> 1 ) );
            break;
         }

      if( x >= 14 )
         *cPtr++ = *cWord++;
      iRetLen++;
   }

   if( iWordLen == 0 && iRetLen < 128 )
   {
      *cPtr++ = *cWord;
      iRetLen++;
   }

   for( x = iRetLen - 2; x >= 0; x-- )
      *( cRet + x ) -= 30;

   *( cRet + iRetLen - 1 ) += 128;
   hb_retclen( cRet, iRetLen );
}


/*
 *   Function: xUnForm()
 *    Purpose: Internal function to translate words from dictionary
 *  Arguments: cWord  - formatted word
 *    Returns: cXformed - unformatted word
 *
 *      Notes: I'm assuming that the returned word won't exceed 128 bytes.
 **************************/
HB_FUNC( XUNFORM )
{
   char cRet[ 128 ];
   char *       cPtr    = cRet;
   HB_ISIZ      iRetLen = 0;
   char         c;
   const char * cWord    = hb_parc( 1 );
   HB_ISIZ      iWordLen = hb_parclen( 1 );
   HB_ISIZ      x;

   while( iWordLen > 0 && iRetLen < 128 )
   {
      iWordLen--;
      iRetLen++;
      c = *cWord++;
      if( iWordLen )
         c += 30;
      else
         c -= 128;

      if( ( x = ( int ) memchr( s_cRepl, c, 7 ) ) <= 6 )
      {
         if( x == 0 )
         {
            if( iRetLen < 127 )
            {
               *cPtr++  = 'I';
               *cPtr++  = 'N';
               *cPtr++  = 'G';
               iRetLen += 2;
            }
         }
         else if( iRetLen < 128 )
         {
            x       = x << 1;
            *cPtr++ = *( s_cSearch + x++ );
            *cPtr++ = *( s_cSearch + x );
            iRetLen++;
         }
      }
      else
         *cPtr++ = c;
   }

   hb_retclen( cRet, iRetLen );
}


/***************************
 *   Function: Sp_Rate()
 *     Syntax: cRating := Sp_Rate( cFound, cWord )
 *    Purpose: Returns a letter code indicating how similar the two
 *             words are.  This is primarily used to sort the list
 *             of suggested words.
 *  Arguments: cFound   - Word from dictionary
 *             cWord    - Word to compare with dictionary word
 *    Returns: cRating  - Letter A-I or Z
 *   Category: INTERNAL
 *  Called by: Sp_Suggest()
 *      Notes: SP_Rate() assigns a rating based upon how likely the
 *             word matches the dictionary word.  It compares the
 *             first 5 letters of the boths word, then the last 5,
 *             down to 2 letters.  This results in a rating from A-H.
 *             If none of these matched, then the function will return
 *             either an I if the words are the same length, or a Z.
 *
 *    C Notes: I'm assuming the words passed are already trimmed.
 **************************/
HB_FUNC( SP_RATE )
{
   const char * cFound  = hb_parc( 1 );
   HB_ISIZ      nFound  = hb_parclen( 1 );
   const char * cWord   = hb_parc( 2 );
   HB_ISIZ      nWord   = hb_parclen( 2 );
   HB_ISIZ      nMinLen = HB_MIN( nFound, nWord );
   HB_ISIZ      x       = abs( nFound - nWord );
   HB_ISIZ      lim     = HB_MIN( nMinLen, 5 );
   char *       cRating = hb_strdup( "nZZ" );

   *cRating = ( char ) ( HB_MIN( x, 9 ) + '0' );

   for( x = 0; x < lim; x++ )
   {
      if( *( cFound + x ) != *( cWord + x ) )
         break;
      *( cRating + 1 ) = ( char ) ( 'A' - 1 + lim - x );
   }

   cFound = cFound + nFound - 1;
   cWord  = cWord + nWord - 1;

   for( x = 0; x < lim; x++ )
   {
      if( *cFound != *cWord )
         break;
      *( cRating + 2 ) = ( char ) ( 'A' - 1 + lim - x );
      cFound--;
      cWord--;
   }

   hb_retclen_buffer( cRating, 3 );
}

/** Start of C_MetaFone() **/

/*
 * Author: Clayton Neff
 * Copyright (c) 1992 by CoN Computer Consultants
 *
 * Revision: 1.0 Last Revised: 8/23/1992 at 16:47
 * Description: Original Creation.
 *---------------------------- ALL RIGHTS RESERVED ----------------------------*/

/*
   This function replaces the ft_Metaph() that Joe Booth used in his
   spelling checker article in the Aquarium.*/

HB_FUNC( C_METAFONE )
{
   char *  sReturn;            /* Pointer to the return string.   */
   char *  sMeta;              /* Pointer to the passed string.   */
   HB_SIZE iRetLen;            /* Length of the return string.    */
   HB_SIZE iStrLen;            /* Length of the passed string.    */
   HB_SIZE iRetPtr;            /* Pointer into the return string. */
   HB_SIZE iStrPtr;            /* Pointer into the passed string. */

   /* If no string was passed then return an empty string. */
   if( hb_pcount() == 0 )
   {
      sMeta      = hb_xgrab( 1 );
      sMeta[ 1 ] = '\0';
      iStrLen    = 0;
      iRetLen    = 4;
   }
   /* If no return lenght was passed, default to 4. */
   else if( hb_pcount() == 1 )
   {
      hb_itemGetWriteCL( hb_param( 1, HB_IT_STRING ), &sMeta, &iStrLen );
      iRetLen = 4;
   }
   else
   {
      hb_itemGetWriteCL( hb_param( 1, HB_IT_STRING ), &sMeta, &iStrLen );
      iRetLen = hb_parns( 2 );
   }

   /* Set up the buffer to hold the return string.
      Be sure to make it 1 character longer than needed
      so there is space for the null terminator. */
   sReturn = hb_xgrab( iRetLen + 1 );
   iRetPtr = 0;
   iStrPtr = 0;

   /* Handle the special prefixes. */
   switch( sMeta[ iStrPtr ] )
   {
      /* Since "KN" and "PN" both translate into 'N', we just stack their
         case statements with no break, and they will all execute the same
         block of code.  We cannot place "GN" here because we must test
         for "GI" as well. */

      /* KN->N */
      case 'K':
      /* PN->N */
      case 'P':
         if( sMeta[ iStrPtr + 1 ] == 'N' )
         {

            /* Put in the first return character and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'N';
            iStrPtr += 2;
         }
         break;

      /* AE->E */
      case 'A':
         if( sMeta[ iStrPtr + 1 ] == 'E' )
         {

            /* Put in the first return character and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'E';
            iStrPtr += 2;
         }
         break;

      /* X->S */
      case 'X':

         /* Put in the first return character and increment the pointer. */
         sReturn[ iRetPtr++ ] = 'S';
         iStrPtr++;
         break;

      /* We have three different possibilities for 'W'... */
      case 'W':

         /* WR->R */
         if( sMeta[ iStrPtr + 1 ] == 'R' )
         {
            /* Put in the first return character and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'R';
            iStrPtr += 2;
         }
         /* WHO->H */
         else if( sMeta[ iStrPtr + 1 ] == 'H' &&
                  sMeta[ iStrPtr + 2 ] == 'O' )
         {
            /* Put in the first return character and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'H';
            iStrPtr += 3;
         }
         /* WH->W */
         else if( sMeta[ iStrPtr + 1 ] == 'H' )
         {
            /* Put in the first return character and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'W';
            iStrPtr += 2;
         }
         break;

      /* Checking for Celtic prefixes... */
      case 'M':

         /* MCG->MK or MC->MK */
         if( sMeta[ iStrPtr + 1 ] == 'C' )
         {
            /* Put in the first return characters and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'M';
            sReturn[ iRetPtr++ ] = 'K';
            iStrPtr += ( sMeta[ iStrPtr + 2 ] == 'G' ) ? 3 : 2;
         }
         /* MACH->MX */
         else if( sMeta[ iStrPtr + 1 ] == 'A' &&
                  sMeta[ iStrPtr + 2 ] == 'C' &&
                  sMeta[ iStrPtr + 3 ] == 'H' )
         {
            /* Put in the first return characters and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'M';
            sReturn[ iRetPtr++ ] = 'X';
            iStrPtr += 4;
         }
         /* MACG->MK or MAC->MK */
         else if( sMeta[ iStrPtr + 1 ] == 'A' &&
                  sMeta[ iStrPtr + 2 ] == 'C' )
         {
            /* Put in the first return characters and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'M';
            sReturn[ iRetPtr++ ] = 'K';
            iStrPtr += ( sMeta[ iStrPtr + 3 ] == 'G' ) ? 4 : 3;
         }
         break;

      case 'G':
         /* GN   -> N */
         if( sMeta[ iStrPtr + 1 ] == 'N' )
         {

            /* Put in the first return character and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'N';
            iStrPtr += 2;
         }
         else if( sMeta[ iStrPtr + 1 ] == 'I' )
         {

            /* Put in the first return character and increment the pointer. */
            sReturn[ iRetPtr++ ] = 'K';
            iStrPtr += 2;
         }
         break;
   }                       /* End of prefix switch...case. */

   /* Now we want to loop until we have stepped through the entire
      passed string or we have filled the return string. */

   while( iStrPtr < iStrLen && iRetPtr < iRetLen )
   {
      /* Use a switch...case statement for each character combination. */
      switch( sMeta[ iStrPtr ] )
      {
         /* If it is a vowel other than 'I', skip it if it isn't the
            first character of the passed string. */
         case 'A':
         case 'E':
         case 'O':
         case 'U':
            if( iStrPtr == 0 )
               sReturn[ iRetPtr++ ] = sMeta[ iStrPtr ];
            iStrPtr++;
            break;

         /* If it is 'I', copy it if it is the first character.
            In addition, check for "ISCH" and replace it with "X". */
         case 'I':
            if( iStrPtr == 0 )
               sReturn[ iRetPtr++ ] = sMeta[ iStrPtr ];
            if( sMeta[ iStrPtr + 1 ] == 'S' &&
                sMeta[ iStrPtr + 2 ] == 'C' &&
                sMeta[ iStrPtr + 3 ] == 'H' )
            {
               sReturn[ iRetPtr++ ] = 'X';
               iStrPtr += 3;
            }
            iStrPtr++;
            break;

         /* Skip B if it is the last character and after an 'M'. */
         case 'B':
            if( ( ( iStrPtr == iStrLen - 1 ) ||
                  sMeta[ iStrPtr + 1 ] == ' ' ) &&
                sMeta[ iStrPtr - 1 ] == 'M' )
            {
               iStrPtr++;
               continue;
            }

         /* B, F, K, L. M. N, R - Skip the second occurance of a double.
            Otherwise, just copy the letter over. */
         case 'F':
         case 'K':
         case 'L':
         case 'M':
         case 'N':
         case 'R':
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;
            sReturn[ iRetPtr++ ] = sMeta[ iStrPtr++ ];
            break;

         /* D - Many differnt sounds possible. */
         case 'D':

            /* If the letter is doubled, skip the first one. */
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;

            /* DGE, DGI, DGY -> J */
            if( sMeta[ iStrPtr + 1 ] == 'G' )
            {
               if( sMeta[ iStrPtr + 2 ] == 'E' ||
                   sMeta[ iStrPtr + 2 ] == 'I' ||
                   sMeta[ iStrPtr + 2 ] == 'Y' )
               {
                  sReturn[ iRetPtr++ ] = 'J';
                  iStrPtr += 3;
               }
               /* If not one of those three, transform to T. */
               else
               {
                  sReturn[ iRetPtr++ ] = 'T';
                  iStrPtr++;
               }
            }
            /* D tranforms to T. */
            else
            {
               sReturn[ iRetPtr++ ] = 'T';
               iStrPtr++;
            }
            break;

         /* G - Many differnt sounds possible. */
         case 'G':

            /* If the letter is doubled, skip the first one. */
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;

            /* GE, GI, GY -> J (if not following a double). */
            if( ( sMeta[ iStrPtr + 1 ] == 'E' ||
                  sMeta[ iStrPtr + 1 ] == 'I' ||
                  sMeta[ iStrPtr + 1 ] == 'Y' ) &&
                sMeta[ iStrPtr - 1 ] != 'G' )
            {
               sReturn[ iRetPtr++ ] = 'J';
               iStrPtr += 2;
            }
            /* vGHT -> T or vGH, vGHTH -> W leaving TH to be processed. */
            else if( sMeta[ iStrPtr + 1 ] == 'H' &&
                     ( sMeta[ iStrPtr - 1 ] == 'A' ||
                       sMeta[ iStrPtr - 1 ] == 'E' ||
                       sMeta[ iStrPtr - 1 ] == 'I' ||
                       sMeta[ iStrPtr - 1 ] == 'O' ||
                       sMeta[ iStrPtr - 1 ] == 'U' ) )
            {
               if( sMeta[ iStrPtr + 2 ] == 'T' &&
                   ! ( sMeta[ iStrPtr + 3 ] == 'H' ) )
               {
                  sReturn[ iRetPtr++ ] = 'T';
                  iStrPtr += 3;
               }
               else
               {
                  /* OUGH -> F */
                  if( sMeta[ iStrPtr - 2 ] == 'O' &&
                      sMeta[ iStrPtr - 1 ] == 'U' )
                  {
                     sReturn[ iRetPtr++ ] = 'F';
                     iStrPtr += 2;
                  }
                  else
                  {
                     sReturn[ iRetPtr++ ] = 'W';
                     iStrPtr += 2;
                  }
               }
            }
            /* GHv -> K */
            else if( sMeta[ iStrPtr + 1 ] == 'H' &&
                     ( sMeta[ iStrPtr + 2 ] == 'A' ||
                       sMeta[ iStrPtr + 2 ] == 'E' ||
                       sMeta[ iStrPtr + 2 ] == 'I' ||
                       sMeta[ iStrPtr + 2 ] == 'O' ||
                       sMeta[ iStrPtr + 2 ] == 'U' ) )
            {
               sReturn[ iRetPtr++ ] = 'K';
               iStrPtr += 2;
            }
            /* GN -> N */
            else if( sMeta[ iStrPtr + 1 ] == 'N' )
            {
               sReturn[ iRetPtr++ ] = 'N';
               iStrPtr += 2;
            }
            /* The suffix NG is skipped. */
            else if( ( ( iStrPtr == iStrLen - 1 ) ||
                       sMeta[ iStrPtr + 1 ] == ' ' ) &&
                     sMeta[ iStrPtr - 1 ] == 'N' )
               iStrPtr++;

            /* G -> K */
            else
            {
               sReturn[ iRetPtr++ ] = 'K';
               iStrPtr++;
            }
            break;

         /* J, W - Direct replacement. */
         case 'J':
         case 'W':
            sReturn[ iRetPtr++ ] = sMeta[ iStrPtr++ ];
            break;

         /* P - Check for PH. */
         case 'P':
            /* If the letter is doubled, skip the first one. */
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;

            if( sMeta[ iStrPtr + 1 ] == 'H' )
            {
               sReturn[ iRetPtr++ ] = 'F';
               iStrPtr += 2;
            }
            else
               sReturn[ iRetPtr++ ] = sMeta[ iStrPtr++ ];
            break;

         /* Q -> K */
         case 'Q':
            sReturn[ iRetPtr++ ] = 'K';
            iStrPtr++;
            break;

         /* S - Many differnt sounds possible. */
         case 'S':

            /* If the letter is doubled, skip the first one. */
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;

            /* SCH -> SK */
            if( sMeta[ iStrPtr + 1 ] == 'C' &&
                sMeta[ iStrPtr + 2 ] == 'H' )
            {
               sReturn[ iRetPtr++ ] = 'S';
               sReturn[ iRetPtr++ ] = 'K';
               iStrPtr += 3;
            }
            /* SIO, SIA, SH -> X */
            else if( ( sMeta[ iStrPtr + 1 ] == 'I' &&
                       ( sMeta[ iStrPtr + 2 ] == 'A' ||
                         sMeta[ iStrPtr + 2 ] == 'O' ) ) ||
                     sMeta[ iStrPtr + 1 ] == 'H' )
            {
               sReturn[ iRetPtr++ ] = 'X';
               iStrPtr += ( sMeta[ iStrPtr + 1 ] == 'H' ) ? 2 : 3;
            }
            /* SCE, SCI, SCY -> S */
            else if( sMeta[ iStrPtr + 1 ] == 'C' )
            {
               if( sMeta[ iStrPtr + 2 ] == 'E' ||
                   sMeta[ iStrPtr + 2 ] == 'I' ||
                   sMeta[ iStrPtr + 2 ] == 'Y' )
               {
                  sReturn[ iRetPtr++ ] = 'S';
                  iStrPtr += 3;
               }
               /* If not one of those three, just copy it over. */
               else
                  sReturn[ iRetPtr++ ] = sMeta[ iStrPtr++ ];
            }
            /* Just copy the letter over. */
            else
               sReturn[ iRetPtr++ ] = sMeta[ iStrPtr++ ];

            break;

         /* T - Many different sounds possible. */
         case 'T':

            /* If the letter is doubled, skip the first one. */
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;

            /* TIA, TIO, TCH -> X */
            if( ( sMeta[ iStrPtr + 1 ] == 'I' &&
                  ( sMeta[ iStrPtr + 2 ] == 'A' ||
                    sMeta[ iStrPtr + 2 ] == 'O' ) ) ||
                ( sMeta[ iStrPtr + 1 ] == 'C' &&
                  sMeta[ iStrPtr + 2 ] == 'H' ) )
            {
               sReturn[ iRetPtr++ ] = 'X';
               iStrPtr += 3;
            }
            /* TH -> 0 */
            else if( sMeta[ iStrPtr + 1 ] == 'H' )
            {
               sReturn[ iRetPtr++ ] = '0';
               iStrPtr += 2;
            }
            /* Copy the letter over. */
            else
               sReturn[ iRetPtr++ ] = sMeta[ iStrPtr++ ];
            break;

         /* V -> F */
         case 'V':
            sReturn[ iRetPtr++ ] = 'F';
            iStrPtr++;
            break;

         /* X -> KS */
         case 'X':

            /* If the letter is doubled, skip the first one. */
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;

            sReturn[ iRetPtr++ ] = 'K';

            /* Now we have to check for all the S possibilities, since
               we want to add one to the string. */

            /* If the followed by an S, skip over it. */
            if( sMeta[ iStrPtr + 1 ] == 'S' )
               iStrPtr++;


            /* SCH -> SK */
            if( sMeta[ iStrPtr + 1 ] == 'C' &&
                sMeta[ iStrPtr + 2 ] == 'H' )
            {
               sReturn[ iRetPtr++ ] = 'S';
               sReturn[ iRetPtr++ ] = 'K';
               iStrPtr += 3;
            }
            /* SIO, SIA, SH -> X */
            else if( ( sMeta[ iStrPtr + 1 ] == 'I' &&
                       ( sMeta[ iStrPtr + 2 ] == 'A' ||
                         sMeta[ iStrPtr + 2 ] == 'O' ) ) ||
                     sMeta[ iStrPtr + 1 ] == 'H' )
            {
               sReturn[ iRetPtr++ ] = 'X';
               iStrPtr += ( sMeta[ iStrPtr + 1 ] == 'H' ) ? 2 : 3;
            }
            /* SCE, SCI, SCY -> S */
            else if( sMeta[ iStrPtr + 1 ] == 'C' )
            {
               if( sMeta[ iStrPtr + 2 ] == 'E' ||
                   sMeta[ iStrPtr + 2 ] == 'I' ||
                   sMeta[ iStrPtr + 2 ] == 'Y' )
               {
                  sReturn[ iRetPtr++ ] = 'S';
                  iStrPtr += 3;
               }
               /* If not one of those three, just copy it over. */
               else
               {
                  sReturn[ iRetPtr++ ] = 'S';
                  iStrPtr++;
               }
            }
            /* Just copy an 'S' over. */
            else
            {
               sReturn[ iRetPtr++ ] = 'S';
               iStrPtr++;
            }
            break;

         /* Z -> S */
         case 'Z':

            /* If the letter is doubled, skip the first one. */
            if( sMeta[ iStrPtr + 1 ] == sMeta[ iStrPtr ] )
               iStrPtr++;
            sReturn[ iRetPtr++ ] = 'S';
            iStrPtr++;
            break;

         /* C - More sounds than any one letter should be allowed! */
         case 'C':

            /* CC, CH, CIA -> X */
            if( sMeta[ iStrPtr + 1 ] == 'C' ||
                sMeta[ iStrPtr + 1 ] == 'H' ||
                ( sMeta[ iStrPtr + 1 ] == 'I' &&
                  sMeta[ iStrPtr + 2 ] == 'A' ) )
            {
               sReturn[ iRetPtr++ ] = 'X';
               iStrPtr += ( sMeta[ iStrPtr + 1 ] == 'I' ) ? 3 : 2;
            }
            /* CE, CI, CY -> S */
            else if( sMeta[ iStrPtr + 1 ] == 'E' ||
                     sMeta[ iStrPtr + 1 ] == 'I' ||
                     sMeta[ iStrPtr + 1 ] == 'Y' )
            {
               sReturn[ iRetPtr++ ] = 'S';
               iStrPtr += 2;
            }
            /* CK - Skip. */
            else if( sMeta[ iStrPtr + 1 ] == 'K' )
               iStrPtr++;

            /* C -> K */
            else
            {
               sReturn[ iRetPtr++ ] = 'K';
               iStrPtr++;
            }
            break;

         /* Y - If not followed by a vowel, skip it. */
         case 'Y':
            if( sMeta[ iStrPtr + 1 ] == 'A' ||
                sMeta[ iStrPtr + 1 ] == 'E' ||
                sMeta[ iStrPtr + 1 ] == 'I' ||
                sMeta[ iStrPtr + 1 ] == 'O' ||
                sMeta[ iStrPtr + 1 ] == 'U' )
               sReturn[ iRetPtr++ ] = 'Y';
            iStrPtr++;
            break;

         /* H - Skip if preceeded by and not followed by a vowel. */
         case 'H':
            if( ( sMeta[ iStrPtr - 1 ] == 'A' ||
                  sMeta[ iStrPtr - 1 ] == 'E' ||
                  sMeta[ iStrPtr - 1 ] == 'I' ||
                  sMeta[ iStrPtr - 1 ] == 'O' ||
                  sMeta[ iStrPtr - 1 ] == 'U' ) && !
                ( sMeta[ iStrPtr + 1 ] == 'A' ||
                  sMeta[ iStrPtr + 1 ] == 'E' ||
                  sMeta[ iStrPtr + 1 ] == 'I' ||
                  sMeta[ iStrPtr + 1 ] == 'O' ||
                  sMeta[ iStrPtr + 1 ] == 'U' ) )
               iStrPtr++;
            else
            {
               sReturn[ iRetPtr++ ] = 'H';
               iStrPtr++;
            }
            break;

         /* Default action, skip it. */
         default:
            iStrPtr++;
      }
   }

   /* Return the prepared string.  Return only the lenght that we know
      is good so we don't return any uninitialized part of memory. */
   hb_retclen( sReturn, iRetPtr );

   /* Release the memory that we allocated for the return string. */
   hb_xfree( sReturn );
}

/** Start of bit() **/

/*
 *  Purpose: Sets the given bit in a passed bit string.  Returns the previous
 *           value.  Be sure to pass the string by reference.  NOTE.  In order
 *           to stay as fast as possible, minimal parameter checking is
 *           performed.  It is up to the user to not be too stupid.
 *
 *   Syntax: bit( @<OptC String>, <OptN (1...n) Offset> [, <OptL Set/Clear>] )
 **************************/
HB_FUNC( BIT )
{
   HB_UCHAR mask;
   char *   ptr;
   HB_SIZE  loc;
   HB_SIZE  offset = hb_parns( 2 ) - 1;
   HB_SIZE  res    = 0;

   loc = offset / 8;
   if( loc < hb_parclen( 1 ) )
   {
      HB_SIZE nLen;
      hb_itemGetWriteCL( hb_param( 1, HB_IT_STRING ), &ptr, &nLen );
      ptr += loc;
      loc  = offset % 8;
      res  = ( HB_UCHAR ) *ptr << loc & 0x80;

      if( hb_pcount() > 2 )
      {
         mask = ( HB_UCHAR ) 0x80 >> loc;
         if( hb_parl( 3 ) )
            *ptr = ( HB_UCHAR ) *ptr | mask;
         else
            *ptr = ( HB_UCHAR ) *ptr & ~mask;
      }
   }
   hb_retl( res );
}

/** Start of Sp_Line() **/

static HB_BOOL WordSep( HB_UCHAR c )
{
   return c <= ' ' ||
          ( c != 39 && ( c > ' ' && c < '0' ) ) ||
          ( c > '9' && c < 'A' ) ||
          ( c > 'Z' && c < 'a' ) ||
          ( c > 'z' && c < 128 ); /* Support international characters, too. */
}

/*
 *  Author: John F. Kaster
 *   Notes: Copyright (c) 1994 by John F. Kaster and Joseph D. Booth
 *          Written for Grumpfish Speller to make it way faster than Prolixity.
 *
 *  Syntax: Sp_Line( <cText>, @<nOffset>, <nLineLen> ) -> cLine
 *
 *  Arguments:
 *  <cText>     Text from which to retrieve a wrapped line
 *  <nOffset>   Offset (usually passed by reference) for start of wrapped line.
 *              Will be set to 0 when the end of the string is encountered.
 *              Defaults to start of string.
 *  <nLineLen>  Maximum wrap length for line.  Defaults to 75.
 **************************/
HB_FUNC( SP_LINE )
{
   int          nArgs      = hb_pcount();
   HB_BOOL      bLineBreak = HB_FALSE;
   HB_ISIZ      nCount     = 0;
   HB_ISIZ      nWrap      = 0;
   HB_SIZE      nOffset    = 0;
   const char * cIn;
   const char * p;
   HB_BYTE      cTest;
   HB_ISIZ      nLineLen;
   HB_SIZE      nStop;

   if( nArgs > 0 && HB_ISCHAR( 1 ) )
   {
      cIn   = hb_parc( 1 );
      nStop = hb_parclen( 1 );
      if( nArgs > 1 && HB_ISNUM( 2 ) )
      {
         nOffset = hb_parns( 2 );
         if( nOffset > 0 )
            nOffset--;
      }

      if( nOffset < nStop )                           /* In string somewhere */
      {
         /* Default line len to 75 */
         nLineLen = nArgs > 2 && HB_ISNUM( 3 ) ? hb_parns( 3 ) - 1 : 75;
         p        = &cIn[ nOffset ];                  /* Starting pointer */

         if( nOffset + nLineLen > nStop )             /* Past end of string? */
            nLineLen = nStop - nOffset;               /* Limit to end of string */

         while( ! bLineBreak && nCount++ < nLineLen )
         {
            cTest = *p++;
            if( cTest == 13 || cTest == 141 )         /* Hard or soft return? */
               bLineBreak = HB_TRUE;
            else if( WordSep( cTest ) )               /* Wrappable character? */
               nWrap = nCount - 1;
         }

         if( ! bLineBreak && nWrap > 0 )              /* Back up to wrap pos */
            nCount = nWrap;

         hb_retclen( &( cIn[ nOffset ] ), nCount + 1 - ( bLineBreak ? 2 : 0 ) );
         nOffset += nCount + 1;
         if( ! bLineBreak )
         {
            while( cIn[ nOffset ] == 32 )             /* Remove leading spaces */
               nOffset++;
         }
         nOffset++;                                   /* +1 for Harbour string */
      }
      else
      {
         nOffset = 0;
         hb_retc_null();
      }
   }
   else
      hb_retc_null();

   if( HB_ISBYREF( 2 ) )                                 /* Change reference val */
      hb_stornl( nOffset, 2 );
}
