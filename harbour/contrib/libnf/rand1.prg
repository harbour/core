/*
 * File......: RAND1.PRG
 * Author....: Gary Baren
 * CIS ID....: 75470,1027
 *
 * This is an original work by Gary Baren and is hereby placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:30   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:03:38   GLENN
 * Initial revision.
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_RAND1()
 *  $CATEGORY$
 *     Math
 *  $ONELINER$
 *     Generate a random number
 *  $SYNTAX$
 *     FT_RAND1( <nMax> ) -> nRand
 *  $ARGUMENTS$
 *     <nMax>  Maximum limit of value to be produced.
 *  $RETURNS$
 *     nRand is a random number between 0 (inclusive) and <nMax> (exclusive).
 *  $DESCRIPTION$
 *     Generates a non-integer random number based on the Linear
 *     Congruential Method.
 *
 *     If you need a random number between 1 and <nMax> inclusive, INT()
 *     the result and add 1.
 *
 *     If you need a random number between 0 and <nMax> inclusive,
 *     then you should ROUND() the result.
 *  $EXAMPLES$
 *      nResult := INT( FT_RAND1(100) ) + 1    // 1 <= nResult <= 100
 *      nResult := ROUND( FT_RAND1(100), 0 )   // 0 <= nResult <= 100
 *      nResult := FT_RAND1( 1 )               // 0 <= nResult < 1
 *  $END$
 */

#ifdef FT_TEST

  // Write 100 random numbers from 1 to 100 to stdout.
  // Run it multiple times and redirect output to a file
  // to check it

  function main()
     local x

     for x := 1 to 100
        outstd( int( ft_rand1(100) ) )
        outstd( chr(13) + chr(10) )
     next
     return nil

#endif


function ft_rand1(nMax)
  static nSeed
  local m := 100000000, b := 31415621

  nSeed := iif( nSeed == NIL, seconds(), nSeed )   // init_seed()

  return( nMax * ( ( nSeed := mod( nSeed*b+1, m ) ) / m ) )
