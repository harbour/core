/*
 * File......: POKE.C
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   07 Feb 1994 20:13:22   GLENN
 * Ted re-wrote to make it CPMI compliant.
 *
 *    Rev 1.2   15 Aug 1991 23:08:20   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:48   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:54   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_POKE()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Write a byte to a specified memory location
 *  $SYNTAX$
 *     FT_POKE( <nSegment>, <nOffset>, <nValue> ) -> lResult
 *  $ARGUMENTS$
 *     <nSegment> is the segment of the desired memory address.
 *
 *     <nOffset>  is the offset of the desired memory address.
 *
 *     <nValue>   is the value to write to the desired memory address.
 *  $RETURNS$
 *     <lResult> will be .T. if all parameters were valid and the function was
 *               able to write the desired byte.
 *     <lResult> will be .F. if invalid parameters were passed.
 *  $DESCRIPTION$
 *     Use this function if you have a need to change the value at a specific
 *     memory location.  The function will write the specified byte to the
 *     specified address.  The value must be passed as a numeric; if the byte
 *     you wish to use is stored as a character, use the Asc() function
 *     to convert it.
 *
 *     This function was written for version 5.1 of MicroSoft C.  You may
 *     have to modify the source code to use another compiler.
 *  $EXAMPLES$
 *     FT_POKE( 0, 1047, 64)  // Turn CapsLock on
 *  $END$
 */

#include <extend.api>
#include <cpmi.h>

#define FP_SEG( fp ) ( *( ( unsigned int * ) &( fp ) + 1 ) )
#define FP_OFF( fp ) ( *( ( unsigned int * ) &( fp ) ) )

HB_FUNC( FT_POKE )
{
   auto unsigned int ProtMode = hb_cpmiIsProtected();
   auto unsigned char * bytePtr;

   if ( ( PCOUNT >= 3 ) && ( ISNUM( 1 ) ) && ( ISNUM( 2 ) ) && ( ISNUM( 3 ) ) )
   {
      FP_SEG( bytePtr ) = _parni( 1 );
      FP_OFF( bytePtr ) = _parni( 2 );

      if ( ProtMode )
      {
         FP_SEG( bytePtr ) = hb_cpmiProtectedPtr( bytePtr, 1 );
         FP_OFF( bytePtr ) = 0;

         if ( FP_SEG( bytePtr ) == 0 ) goto Bogus;
      }

      *bytePtr = ( unsigned char ) _parni( 3 );

      if ( ProtMode ) hb_cpmiFreeSelector( FP_SEG( bytePtr ) );

      _retl( TRUE );
   }
   else
      Bogus: _retl( FALSE );

   return;
}
