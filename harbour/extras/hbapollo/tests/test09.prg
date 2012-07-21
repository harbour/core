/*
 * $Id$
 */
/*
   testing date settings
   Options:
      "AMERICAN"
      "ANSI"
      "BRITISH"
      "FRENCH"
      "GERMAN"
      "ITALIAN"
      "SPANISH"
      "MM/DD/YY"
      "YY.MM.DD"
      "DD/MM/YY"
      "DD.MM.YY"
      "DD-MM-YY"
      "MM/DD/YYYY"
      "YYYY.MM.DD"
      ANSI
      "DD.MM.YYYY"
      ANSI
*/
#include "sixapi.ch"

#include "simpleio.ch"

PROCEDURE MAIN()

   LOCAL e

   SET CENTURY ON
   ? Date()

   SET DATE AMERICAN
   ? Date()

   SET DATE BRITISH
   ? Date()

   ? "Before :", sx_SetDateFormat( "AMERICAN"   ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "ANSI"       ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "BRITISH"    ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "FRENCH"     ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "GERMAN"     ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "ITALIAN"    ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "SPANISH"    ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "MM/DD/YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "YY.MM.DD"   ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "DD/MM/YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "DD.MM.YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "DD-MM-YY"   ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "MM/DD/YYYY" ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "YYYY.MM.DD" ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( ANSI ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( "DD.MM.YYYY" ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   ? "Before :", sx_SetDateFormat( ANSI ), ", Now :", sx_SetDateFormat(), ", Date() =", Date()
   BEGIN SEQUENCE WITH {| e | Break( e ) }
      ? sx_SetDateFormat( "YYYY-MM-DD" ), Date()
   RECOVER USING e
      ? "This is Invalid =>", e:SubSystem, PadL( e:SubCode, 4 ), e:Operation, e:Description, ValToPrg( e:Args )
   END

   BEGIN SEQUENCE WITH {| e | Break( e ) }
      ? sx_SetDateFormat( "AFRIKAANS" ), Date()
   RECOVER USING e
      ? "This is Invalid =>", e:SubSystem, PadL( e:SubCode, 4 ), e:Operation, e:Description, ValToPrg( e:Args )
   END
