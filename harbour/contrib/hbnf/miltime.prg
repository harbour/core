/*
 * $Id$
 */

/*
 * File......: miltime.prg
 * Author....: Alexander B. Spencer
 * CIS ID....: 76276,1012
 *
 * This is an original work by Alexander B. Spencer and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:02   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:22   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   14 Jun 1991 03:43:52   GLENN
 * Initial revision.
 *
 */

#ifdef FT_TEST

PROCEDURE Main()

   cls
   ? "am-pm"
   ? ft_civ2mil( " 5:40 pm" )
   ? ft_civ2mil( "05:40 pm" )
   ? ft_civ2mil( " 5:40 PM" )
   ? ft_civ2mil( " 5:40 am" )
   ? ft_civ2mil( "05:40 am" )
   ? ft_civ2mil( " 5:40 AM" )
   ?
   Inkey( 0 )
   cls
   ? "noon-midnight"
   ? ft_civ2mil( "12:00 m" )
   ? ft_civ2mil( "12:00 M" )
   ? ft_civ2mil( "12:00 m" )
   ? ft_civ2mil( "12:00 n" )
   ? ft_civ2mil( "12:00 N" )
   ? ft_civ2mil( "12:00 n" )
   ?
   Inkey( 0 )
   cls
   ? "errors in noon-midnight"
   ? ft_civ2mil( "12:01 n" )
   ? ft_civ2mil( "22:00 n" )
   ? ft_civ2mil( "12:01 m" )
   ? ft_civ2mil( "22:00 n" )
   ?
   ? "sys to mil"
   ? Time()
   ? ft_sys2mil()

   RETURN

#endif

FUNCTION FT_MIL2MIN( cMILTIME )

   RETURN Int( Val( Left( cMILTIME, 2 ) ) * 60 + Val( Right( cMILTIME, 2 ) ) )

FUNCTION FT_MIN2MIL( nMIN )

   nMIN := nMIN % 1440

   RETURN Right( "00" + hb_ntos( Int( nMIN / 60 ) ), 2 ) + ;
          Right( "00" + hb_ntos( Int( nMIN % 60 ) ), 2 )

FUNCTION FT_MIL2CIV( cMILTIME )

   LOCAL cHRS, cMINS, nHRS, cCIVTIME

   nHRS  := Val( Left( cMILTIME,2 ) )
   cMINS := Right( cMILTIME, 2 )

   DO CASE
   CASE ( nHRS == 24 .OR. nHRS == 0 ) .AND. ( cMINS == "00" )  // Midnight
      cCIVTIME := "12:00 m"
   CASE ( nHRS == 12 )                                       // Noon to 12:59pm
      IF cMINS == "00"
         cCIVTIME := "12:00 n"
      ELSE
         cCIVTIME := "12:" + cMINS + " pm"
      ENDIF
   CASE ( nHRS < 12 )                                    // AM
      IF nHRS == 0
         cHRS := "12"
      ELSE
         cHRS := Right( "  " + hb_ntos( Int( nHRS ) ), 2 )
      ENDIF
      cCIVTIME := cHRS + ":" + cMINS + " am"

   OTHERWISE                                           // PM
      cCIVTIME := Right( "  " + hb_ntos( Int( nHRS - 12 ) ), 2 ) + ;
         ":" + cMINS + " pm"
   ENDCASE

   RETURN cCIVTIME

FUNCTION FT_CIV2MIL( cTIME )

   LOCAL cKEY, cMILTIME

//** Insure leading 0's
   cTIME := Replicate( "0", 3 - At( ":", LTrim( cTIME ) ) ) + LTrim( cTIME )

//** Adjust for popular use of '12' for first hour after noon and midnight
   IF Left( LTrim( cTIME ), 2 ) == "12"
      cTIME := Stuff( cTIME, 1, 2, "00" )
   ENDIF

//** am, pm, noon or midnight
   cKEY := SubStr( LTrim( cTIME ), 7, 1 )

   DO CASE
   CASE Upper( cKEY ) == "N"                           // noon
      IF Left( cTIME, 2 ) + SubStr( cTIME, 4, 2 ) == "0000"
         cMILTIME := "1200"
      ELSE
         cMILTIME := "    "
      ENDIF
   CASE Upper( cKEY ) == "M"                           // midnight
      IF Left( cTIME, 2 ) + SubStr( cTIME, 4, 2 ) == "0000"
         cMILTIME := "0000"
      ELSE
         cMILTIME := "    "
      ENDIF
   CASE Upper( cKEY ) == "A"                           // am
      cMILTIME := Right( "00" + hb_ntos( Val( Left( cTIME, 2 ) ) ), 2 ) + ;
         SubStr( cTIME, 4, 2 )
   CASE Upper( cKEY ) == "P"                           // pm
      cMILTIME := Right( "00" + hb_ntos( Val( Left( cTIME, 2 ) ) + 12 ), 2 ) + ;
         SubStr( cTIME, 4, 2 )
   OTHERWISE
      cMILTIME := "    "                              // error
   ENDCASE

   RETURN cMILTIME

FUNCTION FT_SYS2MIL()

   RETURN Left( Stuff( Time(), 3, 1, "" ), 4 )
