/*
 * $Id$
 */

/*
 * File......: wda.prg
 * Author....: Eric Splaver
 * CIS ID....: ?
 *
 * This is an original work by Eric Splaver and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:04:34   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   14 Jun 1991 04:25:46   GLENN
 * Initial revision.
 *
 */

#ifdef FT_TEST
  PROCEDURE Main( cDate, cDays )
     local nDays := ft_addWkDy( ctod(cDate), val(cDays) )
     qout( "Num days to add: " + str( nDays ) )
     qout( "New date:        " + dtoc( ctod( cDate ) + nDays ) )
     return
#endif

FUNCTION ft_addWkDy( dStart, nDys )
    LOCAL nDc  := dow( dStart )
    RETURN iif( nDc == 7,                                                        ;
            (nDys-1)      % 5 + 7 * int( (nDys-1)      / 5 ) + 2,         ;
            (nDys+nDc-2)  % 5 + 7 * int( (nDys+nDc-2)  / 5 ) + 2  - nDc   ;
                )                                                                   ;
