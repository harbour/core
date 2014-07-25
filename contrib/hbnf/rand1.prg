/* This is an original work by Gary Baren and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:04:30   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:52:46   GLENN
   Minor edit to file header

      Rev 1.0   07 Jun 1991 23:03:38   GLENN
   Initial revision.
 */

#define _B_  31415621
#define _M_  100000000

FUNCTION ft_Rand1( nMax )

   THREAD STATIC t_nSeed

   t_nSeed := iif( t_nSeed == NIL, Seconds(), t_nSeed )

   RETURN nMax * ( ( t_nSeed := ( ( t_nSeed * _B_ + 1 ) % _M_ ) ) / _M_ )
