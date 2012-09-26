/*
 * $Id$
 */

/*
 * File......: rand1.prg
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

#ifdef FT_TEST

// Write 100 random numbers from 1 to 100 to stdout.
// Run it multiple times and redirect output to a file
// to check it

PROCEDURE Main()

   LOCAL x

   FOR x := 1 TO 100
      OutStd( Int( ft_rand1(100 ) ) )
      OutStd( hb_eol() )
   NEXT

   RETURN

#endif

FUNCTION ft_rand1( nMax )

   THREAD STATIC t_nSeed
   LOCAL m := 100000000, b := 31415621

   t_nSeed := iif( t_nSeed == NIL, Seconds(), t_nSeed )   // init_seed()

   RETURN nMax * ( ( t_nSeed := Mod( t_nSeed * b + 1, m ) ) / m )
