/*
 * This is an original work by Mike Schinkel and is placed in the
 * public domain.  The toolkit's original ft_LDay() function was
 * submitted by Jeff Bryant.  Mike saw it and optimized it.  Thanks
 * to you both for your great code!
 *
 * Modification history:
 *
 *    Rev 1.1   15 Aug 1991 23:02:32   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   14 Jun 1991 04:24:04   GLENN
 * Initial revision.
 *
 */

FUNCTION ft_LDay( dDate )

   __defaultNIL( @dDate, Date() )

   RETURN ( dDate += 45 - Day( dDate ) ) - Day( dDate )
