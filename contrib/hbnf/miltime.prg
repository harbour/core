/* This is an original work by Alexander B. Spencer and is placed in the
   public domain.

      Rev 1.2   15 Aug 1991 23:04:02   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:52:22   GLENN
   Minor edit to file header

      Rev 1.0   14 Jun 1991 03:43:52   GLENN
   Initial revision.
 */

FUNCTION ft_Mil2Min( cMILTIME )
   RETURN Int( Val( Left( cMILTIME, 2 ) ) * 60 + Val( Right( cMILTIME, 2 ) ) )

FUNCTION ft_Min2Mil( nMin )

   nMin := nMin % 1440

   RETURN ;
      StrZero( Int( nMin / 60 ), 2 ) + ;
      StrZero( Int( nMin % 60 ), 2 )

FUNCTION ft_Mil2Civ( cMILTIME )

   LOCAL nHRS  := Val( Left( cMILTIME, 2 ) )
   LOCAL cMINS := Right( cMILTIME, 2 )

   DO CASE
   CASE ( nHRS == 24 .OR. nHRS == 0 ) .AND. cMINS == "00"  // Midnight
      RETURN "12:00 m"
   CASE nHRS == 12  // Noon to 12:59pm
      RETURN iif( cMINS == "00", "12:00 n", "12:" + cMINS + " pm" )
   CASE nHRS < 12  // AM
      RETURN iif( nHRS == 0, "12", Str( Int( nHRS ), 2 ) ) + ":" + cMINS + " am"
   ENDCASE

   RETURN Str( Int( nHRS - 12 ), 2 ) + ":" + cMINS + " pm"  // PM

FUNCTION ft_Civ2Mil( cTIME )

   // Ensure leading 0's
   cTIME := Replicate( "0", 3 - At( ":", LTrim( cTIME ) ) ) + LTrim( cTIME )

   // Adjust for popular use of '12' for first hour after noon and midnight
   IF hb_LeftEq( LTrim( cTIME ), "12" )
      cTIME := Stuff( cTIME, 1, 2, "00" )
   ENDIF

   SWITCH Upper( SubStr( LTrim( cTIME ), 7, 1 ) )
   CASE "N" ; RETURN iif( Left( cTIME, 2 ) + SubStr( cTIME, 4, 2 ) == "0000", "1200", "    " )  // noon
   CASE "M" ; RETURN iif( Left( cTIME, 2 ) + SubStr( cTIME, 4, 2 ) == "0000", "0000", "    " )  // midnight
   CASE "A" ; RETURN StrZero( Val( Left( cTIME, 2 ) ), 2 ) + SubStr( cTIME, 4, 2 )              // am
   CASE "P" ; RETURN StrZero( Val( Left( cTIME, 2 ) ) + 12, 2 ) + SubStr( cTIME, 4, 2 )         // pm
   ENDSWITCH

   RETURN "    "  // error

FUNCTION ft_Sys2Mil()
   RETURN Left( Stuff( Time(), 3, 1, "" ), 4 )
