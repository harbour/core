//
// NOTES: 1) The date functions are 'international'; i.e., the
//           system date format is maintained, although ANSI is
//           temporarily used within certain functions.
//
//        2) The date functions fall into two categories:
//
//           a) Calendar or fiscal periods.
//              A calendar or fiscal year is identified by the Year()
//              of the last date in the year.
//
//           b) Accounting Periods. An accounting period has the
//              following characteristics:
//              If the first week of the period contains 4 or
//              more 'work' days, it is included in the period;
//              otherwise, the first week was included in the
//              prior period.
//
//              If the last week of the period contains 4 or more
//              'work' days it is included in the period; otherwise,
//              the last week is included in the next period.
//              This results in 13 week 'quarters' and 4 or 5 week
//              'months'. Every 5 or 6 years, a 'quarter' will contain
//              14 weeks and the year will contain 53 weeks.
//
//        3) The date functions require the presence of two variables:
//
//           a) cFY_Start is a character string used to define the
//              first day of a calendar or fiscal year. It's format
//              is ANSI; e.g., "1980.01.01" defines a calendar year,
//              "1980.10.01" defines a fiscal year, starting October 1.
//
//              The year may be any valid year. It's value has no
//              effect on the date functions. The day is assumed to be
//              less than 29. See function: ft_DateCnfg().
//
//           B) nDow_Start is a number from 1 to 7 which defines the
//              starting day, DoW(), of a work week; e.g., 1 == Sunday.
//
//              See function: ft_DateCnfg()
//

#require "hbnf"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL nNum, dDate, aTestData, aTemp, cFY_Start, nDOW_Start

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )          // User's normal date format
   hb_langSelect( "en" )

   aTemp      := ft_DateCnfg()                   // Get/Set cFY_Start and nDOW_Start.
#if 0
   aTemp      := ft_DateCnfg( "1980-01-03", 1 )  // Date string in user's format.
#endif
   cFY_Start  := aTemp[ 1 ]                      // See ft_DateCnfg()
   nDOW_Start := aTemp[ 2 ]                      // for parameters.
   dDate      := Date() - 500

   CLS
   ? "Given   ", ;
      "     Date:", dDate, ;
      "cFY_Start:", cFY_Start, ;
      "nDOW_Start:", hb_ntos( nDOW_Start )
   ? "---- Fiscal Year Data -----------"

   aTestData := ft_Year( dDate )
   ? "FYYear      ", aTestData[ 1 ] + "  ", aTestData[ 2 ], aTestData[ 3 ]

   aTestData := ft_Qtr( dDate )
   ? "FYQtr       ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   nNum      := Val( SubStr( aTestData[ 1 ], 5, 2 ) )
   aTestData := ft_Qtr( dDate, nNum )
   ? "FYQtr    ", Str( nNum, 2 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   aTestData := ft_Month( dDate )
   ? "FYMonth     ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   nNum := Val( SubStr( aTestData[ 1 ], 5, 2 ) )
   aTestData := ft_Month( dDate, nNum )
   ? "FYMonth  ", Str( nNum, 2 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   aTestData := ft_Week( dDate )
   ? "FYWeek      ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   nNum      := Val( SubStr( aTestData[ 1 ], 5, 2 ) )
   aTestData := ft_Week( dDate, nNum )
   ? "FYWeek   ", Str( nNum, 2 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   aTestData := ft_DayOfYr( dDate )
   ? "FYDay      ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   nNum      := Val( SubStr( aTestData[ 1 ], 5, 3 ) )
   aTestData := ft_DayOfYr( dDate, nNum )
   ? "FYDAY   ", Str( nNum, 3 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   ?
   ? "---- Accounting Year Data -------"

   aTestData := ft_AcctYear( dDate )
   ? "ACCTYear    ", aTestData[ 1 ] + "  ", aTestData[ 2 ], aTestData[ 3 ], ;
      Str( ( aTestData[ 3 ] - aTestData[ 2 ] + 1 ) / 7, 3 ) + " Weeks"

   aTestData := ft_AcctQtr( dDate )
   ? "ACCTQtr     ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ], ;
      Str( ( aTestData[ 3 ] - aTestData[ 2 ] + 1 ) / 7, 3 ) + " Weeks"

   nNum      := Val( SubStr( aTestData[ 1 ], 5, 2 ) )
   aTestData := ft_AcctQtr( dDate, nNum )
   ? "ACCTQtr  ", Str( nNum, 2 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   aTestData := ft_AcctMonth( dDate )
   ? "ACCTMonth   ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ], ;
      Str( ( aTestData[ 3 ] - aTestData[ 2 ] + 1 ) / 7, 3 ) + " Weeks"

   nNum      := Val( SubStr( aTestData[ 1 ], 5, 2 ) )
   aTestData := ft_AcctMonth( dDate, nNum )
   ? "ACCTMonth", Str( nNum, 2 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   aTestData := ft_AcctWeek( dDate )
   ? "ACCTWeek    ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   nNum      := Val( SubStr( aTestData[ 1 ], 5, 2 ) )
   aTestData := ft_AcctWeek( dDate, nNum )
   ? "ACCTWeek ", Str( nNum, 2 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   aTestData := ft_DayOfYr( dDate, , .T. )
   ? "ACCTDay    ", aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   nNum      := Val( SubStr( aTestData[ 1 ], 5, 3 ) )
   aTestData := ft_DayOfYr( dDate, nNum, .T. )
   ? "ACCTDay", Str( nNum, 3 ), aTestData[ 1 ], aTestData[ 2 ], aTestData[ 3 ]

   WAIT

   FT_CAL( dDate )
   FT_CAL( dDate, 1 )

   RETURN

// DEMO Monthly Calendar function.
// nType: 0 -> FT_MONTH, 1 -> FT_ACCTMONTH

STATIC PROCEDURE FT_CAL( dGivenDate, nType )

   LOCAL nTemp, dTemp, aTemp, cFY_Start, dStart, dEnd

   aTemp     := ft_DateCnfg()
   cFY_Start := aTemp[ 1 ]

   IF dGivenDate == NIL .OR. !( ValType( dGivenDate ) $ "NDT" )
      dGivenDate := Date()
   ELSEIF HB_ISNUMERIC( dGivenDate )
      nType := dGivenDate
      dGivenDate := Date()
   ENDIF

   nType := iif( HB_ISNUMERIC( nType ), nType, 0 )

   IF nType == 0
      IF SubStr( cFY_Start, 6, 5 ) == "01.01"
         ? "          Calendar Month Calendar containing", dGivenDate
      ELSE
         ? "            Fiscal Month Calendar containing", dGivenDate
      ENDIF

      aTemp    := ft_Month( dGivenDate )
      dStart   := aTemp[ 2 ]
      dEnd     := aTemp[ 3 ]
      aTemp[ 2 ] -= ft_DayToBoW( aTemp[ 2 ] )
      aTemp[ 3 ] += 6 - ft_DayToBoW( aTemp[ 3 ] )
   ELSE
      ? "            Accounting Month Calendar containing", dGivenDate
      aTemp := ft_AcctMonth( dGivenDate )
   ENDIF

   ?
   dTemp := aTemp[ 2 ]

   FOR nTemp := 0 TO 6
      ?? PadC( CDoW( dTemp + nTemp ), 10 )
   NEXT

   ?
   DO WHILE dTemp <= aTemp[ 3 ]
      FOR nTemp := 1 TO 7
         ?? " "
         IF nType == 0 .AND. ( dTemp < dStart .OR. dTemp > dEnd )
            ?? Space( 8 )
         ELSE
            ?? dTemp
         ENDIF
         ?? " "
         dTemp++
      NEXT
      ?
   ENDDO

   RETURN
