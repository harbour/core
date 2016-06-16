/* This is an original work of Jo W. French (dba Practical Computing)
   and is placed in the public domain.

      Rev 1.3   28 Sep 1992 00:24:54   GLENN
   Jo French clean up.

      Rev 1.2   15 Aug 1991 23:02:30   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:50:42   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:00:24   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_AcctMonth( dGivenDate, nMonthNum )

   LOCAL nYTemp, nMTemp, aRetVal

   IF HB_ISNUMERIC( dGivenDate )
      nMonthNum := dGivenDate
   ENDIF

   hb_default( @dGivenDate, Date() )

   aRetVal := ft_Month( dGivenDate )
   nYTemp := Val( SubStr( aRetVal[ 1 ], 1, 4 ) )
   nMTemp := Val( SubStr( aRetVal[ 1 ], 5, 2 ) )
   aRetVal[ 2 ] := ft_AcctAdj( aRetVal[ 2 ] )
   aRetVal[ 3 ] := ft_AcctAdj( aRetVal[ 3 ], .T. )

   DO CASE
   CASE dGivenDate < aRetVal[ 2 ]

      dGivenDate := ft_MAdd( dGivenDate, -1 )
      aRetVal    := ft_Month( dGivenDate )
      nMTemp     -= 1
      IF nMTemp  == 0
         nYTemp -= 1
         nMTemp := 12
      ENDIF
      aRetVal[ 2 ] := ft_AcctAdj( aRetVal[ 2 ] )
      aRetVal[ 3 ] := ft_AcctAdj( aRetVal[ 3 ], .T. )

   CASE dGivenDate > aRetVal[ 3 ]

      dGivenDate := ft_MAdd( dGivenDate, 1 )
      aRetVal    := ft_Month( dGivenDate )
      nMTemp     += 1
      IF nMTemp == 13
         nYTemp += 1
         nMTemp := 1
      ENDIF
      aRetVal[ 2 ] := ft_AcctAdj( aRetVal[ 2 ] )
      aRetVal[ 3 ] := ft_AcctAdj( aRetVal[ 3 ], .T. )

   ENDCASE

   IF HB_ISNUMERIC( nMonthNum )
      IF nMonthNum < 1 .OR. nMonthNum > 12
         nMonthNum := 12
      ENDIF
      aRetVal      := ft_Month( dGivenDate, nMonthNum )
      nYTemp       := Val( SubStr( aRetVal[ 1 ], 1, 4 ) )
      nMTemp       := Val( SubStr( aRetVal[ 1 ], 5, 2 ) )
      aRetVal[ 2 ] := ft_AcctAdj( aRetVal[ 2 ] )
      aRetVal[ 3 ] := ft_AcctAdj( aRetVal[ 3 ], .T. )
   ENDIF

   aRetVal[ 1 ] := StrZero( nYTemp, 4 ) + StrZero( nMTemp, 2 )

   RETURN aRetVal
