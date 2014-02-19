/*
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:34   GLENN
 * Nanforum Toolkit
 *
 */

#define EARLIEST_DATE  0d01000101

FUNCTION ft_XToY( xValueToConvert, cTypeToConvertTo, lWantYesNo )

   __defaultNIL( @lWantYesNo, .F. )

   DO CASE
   CASE cTypeToConvertTo == "C" .AND. ! HB_ISSTRING( xValueToConvert )

      xValueToConvert := { ;
         "", ;
         xValueToConvert, ;
         iif( HB_ISNUMERIC( xValueToConvert ), hb_ntos( xValueToConvert ), "" ), ;
         iif( HB_ISDATE( xValueToConvert ), DToC( xValueToConvert ), "" ), ;
         iif( HB_ISLOGICAL( xValueToConvert ), iif( xValueToConvert, ".T.", ".F." ), "" ), ;
         xValueToConvert } ;
         [ At( ValType( xValueToConvert ), "CNDLM" ) + 1 ]

   CASE cTypeToConvertTo == "D" .AND. ! HB_ISDATE( xValueToConvert )

      xValueToConvert := iif( HB_ISSTRING( xValueToConvert ), ;
         ; // Convert from a Character
      CToD( xValueToConvert ), ;
         iif( HB_ISNUMERIC( xValueToConvert ), ;
         ; // Convert from a Number
      xValueToConvert + EARLIEST_DATE, ;
         iif( HB_ISLOGICAL( xValueToConvert ), ;
         ; // Convert from a Logical
      iif( xValueToConvert, Date(), 0d0 ), ;
         ; // Unsupported Type
      0d0 ) ) )

   CASE cTypeToConvertTo == "N" .AND. ! HB_ISNUMERIC( xValueToConvert )

      xValueToConvert := iif( HB_ISSTRING( xValueToConvert ), ;
         ; // Convert from a Character
      Val( xValueToConvert ), ;
         iif( HB_ISDATE( xValueToConvert ), ;
         ; // Convert from a Date
      xValueToConvert - EARLIEST_DATE, ;
         iif( HB_ISLOGICAL( xValueToConvert ), ;
         ; // Convert from a Logical
      iif( xValueToConvert, 1, 0 ), ;
         ; // Unsupported Type
      0 ) ) )

   CASE cTypeToConvertTo == "L" .AND. ! HB_ISLOGICAL( xValueToConvert )

      xValueToConvert := iif( HB_ISSTRING( xValueToConvert ), ;
         ; // Convert from a Character
      Upper( xValueToConvert ) == iif( lWantYesNo, "Y", ".T." ), ;
         iif( HB_ISDATE( xValueToConvert ), ;
         ; // Convert from a Date
      ! Empty( xValueToConvert ), ;
         iif( HB_ISNUMERIC( xValueToConvert ), ;
         ; // Convert from a Number
      xValueToConvert != 0, ;
         ; // Unsupported Type
      .F. ) ) )

   CASE cTypeToConvertTo == "A" .AND. ! HB_ISARRAY( xValueToConvert )

      xValueToConvert := { xValueToConvert }

   CASE cTypeToConvertTo == "B" .AND. ! HB_ISBLOCK( xValueToConvert )

      xValueToConvert := {|| xValueToConvert }

   ENDCASE

   RETURN xValueToConvert
