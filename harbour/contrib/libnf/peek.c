/*
 * File......: PEEK.C
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   07 Feb 1994 20:11:50   GLENN
 * Ted re-wrote to make it CPMI compliant.
 *
 *    Rev 1.2   15 Aug 1991 23:08:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:52   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_PEEK()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Retrieve a byte from a specified memory location.
 *  $SYNTAX$
 *     FT_PEEK( <nSegment>, <nOffset> ) -> nValue
 *  $ARGUMENTS$
 *     <nSegment> is the segment of the desired memory address.
 *
 *     <nOffset>  is the offset of the desired memory address.
 *  $RETURNS$
 *     <nValue> will be a value from 0 to 255 if all parameters were valid and
 *              the function was able to retrieve the desired byte.
 *     <nValue> will be -1 if invalid parameters were passed.
 *  $DESCRIPTION$
 *     Use this function if you have a need to examine a specific memory
 *     location.  The function will return the byte at the specified
 *     address as a numeric value.  If you need this value as a character,
 *     use the Chr() function to convert it.
 *
 *     This function was written for version 5.1 of MicroSoft C.  You may
 *     have to modify the source code to use another compiler.
 *  $EXAMPLES$
 *     local nVMode := FT_PEEK( 0, 1097 )  // Get the current video mode
 *  $END$
 */

#include <extend.api>
#include <cpmi.h>

#define FP_SEG( fp ) ( *( ( unsigned int * ) &( fp ) + 1 ) )
#define FP_OFF( fp ) ( *( ( unsigned int * ) &( fp ) ) )

HB_FUNC(FT_PEEK)
{
   auto unsigned int ProtMode = cpmiIsProtected();
   auto unsigned char * bytePtr;

   if ( ( PCOUNT >= 2 ) && ( ISNUM( 1 ) ) && ( ISNUM( 2 ) ) )
   {
      FP_SEG( bytePtr ) = _parni( 1 );
      FP_OFF( bytePtr ) = _parni( 2 );

      if ( ProtMode )
      {
         FP_SEG( bytePtr ) = hb_cpmiProtectedPtr( bytePtr, 1 );
         FP_OFF( bytePtr ) = 0;

         if ( FP_SEG( bytePtr ) == 0 ) goto Bogus;
      }

      _retni( ( int ) *bytePtr );

      if ( ProtMode ) hb_cpmiFreeSelector( FP_SEG( bytePtr ) );
   }
   else
      Bogus: _retni( -1 );

   return;
}
