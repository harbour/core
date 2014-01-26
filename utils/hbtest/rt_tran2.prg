/*
 * Harbour Project source code:
 *    test code for Transform() function
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#if defined( __HBSCRIPT__HBSHELL )
#require "hbtest"
#else
#include "rt_main.ch"
#endif

PROCEDURE Main_TRANS2()

   LOCAL dt
   LOCAL df := Set( _SET_DATEFORMAT, "mm/dd/yy" )
   LOCAL sf
   LOCAL sd

   HBTEST Transform( "abcd", "@9!*" )                   IS "ABCD"
   HBTEST Transform( "abcd", "@_9!*" )                  IS "ABCD"
   HBTEST Transform( "abcd", "@_9" + Chr( 9 ) + "9!9" ) IS "aBc"
   HBTEST Transform( "abcd", "@!!!" )                   IS "ABCD"
   HBTEST Transform( "abcd", "@9" )                     IS "abcd"

   HBTEST Transform( 134.24, "99,999.99" )              IS "   134.24"
   HBTEST Transform( 134.24, "@E 99,999.99" )           IS "   134,24"
   HBTEST Transform( -134.24, "@E 99,999.99" )          IS "  -134,24"
   HBTEST Transform( 134.24, "@E99,999.99" )            IS "       134,24"
   HBTEST Transform( -134.24, "@E99,999.99" )           IS "      -134,24"

   HBTEST Transform( -7, "@X 9999" )                    IS "   7 DB"
   HBTEST Transform( hb_SToD( "19920509" ), "@E" )      IS "09/05/92"
   HBTEST Transform( Val( "3.10" ), "@X" )              IS "3.10"
   HBTEST Transform( 0.80, ".9999" )                    IS ".8000"
   HBTEST Transform( -0.80, ".9999" )                   IS ".****"
   HBTEST Transform( 12345.123, "@X99" )                IS "     12345.123"
   HBTEST Transform( -12345.123, "@X99" )               IS "     12345.123 DB"
   HBTEST Transform( 123456.78, "@E" )                  IS "    123456,78"
   HBTEST Transform( 0, "@C 9.99" )                     IS "0.00"

#ifdef __HARBOUR__
   dt := hb_SToD( "19871231" )
   Set( _SET_DATEFORMAT, "MM:DD:YYYY" )
   HBTEST Transform( dt, "@E" )                         IS "31:12:1987"
   Set( _SET_DATEFORMAT, "DD:MM:YYYY" )
   HBTEST Transform( dt, "@E" )                         IS "31:12:1987"
   Set( _SET_DATEFORMAT, "YYYY:MM:DD" )
   HBTEST Transform( dt, "@E" )                         IS "31:12:1987"
   Set( _SET_DATEFORMAT, "YYYY:DD:MM" )
   HBTEST Transform( dt, "@E" )                         IS "31:12:1987"
   Set( _SET_DATEFORMAT, "YY:MM:DD" )
   HBTEST Transform( dt, "@E" )                         IS "31:12:87"
   Set( _SET_DATEFORMAT, "MM:DD:YY" )
   HBTEST Transform( dt, "@E" )                         IS "31:12:87"
   Set( _SET_DATEFORMAT, "DD:MM:YY" )
   HBTEST Transform( dt, "@E" )                         IS "31:12:87"
   Set( _SET_DATEFORMAT, "<YY:DD.MM>" )
   HBTEST Transform( dt, "@E" )                         IS "<31:12.87>"
   Set( _SET_DATEFORMAT, "|YY|MM|DD|" )
   HBTEST Transform( dt, "@E" )                         IS "|31|12|87|"
   Set( _SET_DATEFORMAT, "MM<DD>YY" )
   HBTEST Transform( dt, "@E" )                         IS "31<12>87"

#endif

   HBTEST Transform( -5, "@(Z $###,##9.99" )            IS "(      5.00)"
   HBTEST Transform( -10, "@)Z $###,##9.99" )           IS "$    (10.00)"
   HBTEST Transform( -20, "@Z $###,##9.99" )            IS "$    -20.00"
   HBTEST Transform( 100, "9999." )                     IS " 100."
   HBTEST Transform( 1.1, "@B!99.99" )                  IS "1.1         "
   HBTEST Transform( 12.345, "@R 99/99" )               IS "  /12"
   HBTEST Transform( "1234567890", "@9" )               IS "1234567890"
   HBTEST Transform( 1234567890, "@9" )                 IS " 1234567890"
   HBTEST Transform( 1234, "9 999" )                    IS "1 234"
   HBTEST Transform( 123.123456, "999.99.99.99" )       IS "123.12.45.  "
   HBTEST Transform( 123.123456, "$$$.$$.$$.$$" )       IS "123.12.45.  "
   HBTEST Transform( 123.123456, "***.**.**.**" )       IS "123.12.45.  "
   HBTEST Transform( 99999, "9.999" )                   IS "*.***"
   HBTEST Transform(    99, "*.***" )                   IS "*.***"
   HBTEST Transform( 12345, "9999." )                   IS "****."

   HBTEST Transform( -12345.00, "@(" )                  IS "(    12345.00)"
   HBTEST Transform( -12345.00, "@)" )                  IS "    (12345.00)"
   HBTEST Transform( -123456789.00, "@(" )              IS "(123456789.00)"
   HBTEST Transform( -123456789.00, "@)" )              IS "(123456789.00)"
   HBTEST Transform( -1234567890, "@(" )                IS "(         1234567890)"
   HBTEST Transform( -1234567890, "@)" )                IS "         (1234567890)"
   HBTEST Transform( -12345, "@( [999999]" )            IS "( 12345])"
   HBTEST Transform( -12345, "@) [999999]" )            IS "[(12345])"
   HBTEST Transform( -12345, "@( $999999" )             IS "( 12345)"
   HBTEST Transform( -12345, "@) $999999" )             IS "$(12345)"
   HBTEST Transform( -12345, "@( #999999" )             IS "( 12345)"
   HBTEST Transform( -12345, "@) #999999" )             IS " (12345)"
   HBTEST Transform( -12345, "@( $99999" )              IS "(12345)"
   HBTEST Transform( -12345, "@) $99999" )              IS "(12345)"
   HBTEST Transform( -12345, "@( #99999" )              IS "(12345)"
   HBTEST Transform( -12345, "@) #99999" )              IS "(12345)"
   HBTEST Transform( -12345, "@( 6798^999" )            IS "(7*8^***)"
   HBTEST Transform( -12345, "@( 9798^9999" )           IS "(718^2345)"

   HBTEST Transform( 134.24, "@E99,999.99" )            IS "       134,24"
   HBTEST Transform( -134.24, "@E99,999.99" )           IS "      -134,24"
   HBTEST Transform( 0.80, ".9999" )                    IS ".8000"
   HBTEST Transform( -0.80, ".9999" )                   IS ".****"
   HBTEST Transform( 12345.123, "@X99" )                IS "     12345.123"
   HBTEST Transform( -12345.123, "@X99" )               IS "     12345.123 DB"
   HBTEST Transform( 123456.78, "@E" )                  IS "    123456,78"
   HBTEST Transform( 0, "@C 9.99" )                     IS "0.00"
   HBTEST Transform( 1.1, "@B!99.99" )                  IS "1.1         "
   HBTEST Transform( -12345, "@) [999999]" )            IS "[(12345])"
   HBTEST Transform( -12345, "@) $999999" )             IS "$(12345)"
   HBTEST Transform( -12345, "@) *999999" )             IS "*(12345)"
   HBTEST Transform( -12345, "@) #999999" )             IS " (12345)"
   HBTEST Transform( -12345, "@) *9$9*999]" )           IS "*($12345])"
   HBTEST Transform( -12345, "@) *999*999]" )           IS "* (12345])"
   HBTEST Transform( -12345, "@) 0999*999]" )           IS "0 (12345])"
   HBTEST Transform( -12345, "@) 1999*999]" )           IS "1 (12345])"
   HBTEST Transform( -12345, "@) *[99*999]" )           IS "([ 12345])"
   HBTEST Transform( -12345, "@) *****999]" )           IS "(**12345])"
   HBTEST Transform( -12345, "@) *1***999]" )           IS "(1*12345])"
   HBTEST Transform( -12345, "@) * 999999]" )           IS "* (12345])"
   HBTEST Transform( -5, "@(Z $###,##9.99" )            IS "(      5.00)"
   HBTEST Transform( -10, "@)Z $###,##9.99" )           IS "$    (10.00)"
   HBTEST Transform( -5, "@(Z $999,999.99" )            IS "(      5.00)"
   HBTEST Transform( -10, "@)Z $999,999.99" )           IS "$    (10.00)"
   HBTEST Transform( -5, "@(Z 999,999.99" )             IS "(     5.00)"
   HBTEST Transform( -10, "@)Z 999,999.99" )            IS "    (10.00)"
   HBTEST Transform( -20, "@Z $###,##9.99" )            IS "$    -20.00"
   HBTEST Transform( 0.1, ".9" )                        IS ".1"
   HBTEST Transform( 0.0, ".9" )                        IS ".0"
   HBTEST Transform( 1, ".9" )                          IS ".*"
   HBTEST Transform( .456, ".9" )                       IS ".5"
   HBTEST Transform( 123, "99.-" )                      IS "**.-"

   HBTEST Transform( -123.45, "999,999.99" )                         IS "   -123.45"
   HBTEST Transform( -123456.78, "999,999,999.99" )                  IS "   -123,456.78"
   HBTEST Transform( -123456.78, "$$$,$$$,$$$.$$" )                  IS "$$ -123,456.78"
   HBTEST Transform( -123456.78, "***,***,***.**" )                  IS "***-123,456.78"
   HBTEST Transform( 123456.78, "@E 888,$$$,$$$.$$" )                IS "888.123.456,78"
   HBTEST Transform( 123456.78, "@E 888x,$$$,$$$.$$" )               IS "888xx123.456,78"
   HBTEST Transform( 123456.78, "@E 888x,,$$$,$$$.$$" )              IS "888xxx123.456,78"
   HBTEST Transform( 123456.78, "@E 8,88x,,$$$,$$$.$$" )             IS "8.88xxx123.456,78"
   HBTEST Transform( 123456.78, "@E 8,88x,,$$$,,$$$.$$" )            IS "8.88xxx123..456,78"
   HBTEST Transform( 123456.78, "@E 8,88x,,$$$,,$$$.$$77,7" )        IS "8.88xxx123..456,7877,7"
   HBTEST Transform( 123456, "@E 8,88x,,$$$,,$$$77,7" )              IS "8.88xxx123..45677,7"
   HBTEST Transform( 123456, "@E -,999,999" )                        IS " -123.456"
   HBTEST Transform( 12345, "@E -,999,999" )                         IS " - 12.345"
   HBTEST Transform( 12345, "@E -,|999,999" )                        IS " -| 12.345"
   HBTEST Transform( 12345, "@E ^-,|999,999" )                       IS "^^-| 12.345"
   HBTEST Transform( 12345, "@E 1-,|999,999" )                       IS "11-| 12.345"
   HBTEST Transform( 12345, "@E |--,|999,999" )                      IS "|---| 12.345"

   HBTEST Transform( 12.34, "@E 99'99" )                             IS "  '12"
   HBTEST Transform( 12.34, "99,99,11" )                             IS "   12,11"
   HBTEST Transform( 12.34, "@E 99,99,11" )                          IS "   12,11"
   HBTEST Transform( 12.34, "@E 99," )                               IS "12,"
   HBTEST Transform( 12.34, "@E 9,9" )                               IS "1.2"
   HBTEST Transform( 12.34, "@E ab,cd.ef9,9.99,.--" )                IS "abbcd,ef***,* ,.--"
   HBTEST Transform( 12.34, "@E ab,cd,ef9,9.99,.--" )                IS "abbcddef1.2,34,.--"
   HBTEST Transform( 12.34, "@E ,ab,cd,ef9,9.99,.--" )               IS ",abbcddef1.2,34,.--"
   HBTEST Transform( 12.34, "@E ,,,,99,.99,.--" )                    IS ",,,,12.,34,.--"
   HBTEST Transform( 124.4, "@E ,,,,9,9.99,.--" )                    IS ",,,,***,**,.--"
   HBTEST Transform(  1.2, "@E ,,,,*,*.**,.--" )                     IS ",,,,**1,20,.--"
   HBTEST Transform( 12.34, "@E ,,,,*,*.**,.--" )                    IS ",,,,1.2,34,.--"
   HBTEST Transform( 12.34, "@E ,,,,*,*.**,.--,--" )                 IS ",,,,1.2,34,.--,--"
   HBTEST Transform( 12.34, "@E ,,,,*,*,.,**" )                      IS ",,,,1.2.,,34"
   HBTEST Transform( 12.34, ",,,,*,*,.,**" )                         IS ",,,,1,2,..34"
   HBTEST Transform( 12.34, ",,,,*,*,.,*|,*" )                       IS ",,,,1,2,..3||4"
   HBTEST Transform( 12.34, ",,,,*,*,.,*,*" )                        IS ",,,,1,2,..3,4"
   HBTEST Transform( 123.345678912, "@E 999.99.99,99.99." )          IS "123,34,67.89,  ."
#ifdef __HARBOUR__
   HBTEST Transform(  1234567890123456789, "99999999999999999999" )  IS " 1234567890123456789"
   HBTEST Transform( -1234567890123456789, "99999999999999999999" )  IS "-1234567890123456789"
#else
   HBTEST Transform(  1234567890123456789, "99999999999999999999" )  IS " 1234567890123457000"
   HBTEST Transform( -1234567890123456789, "99999999999999999999" )  IS "-1234567890123457000"
#endif

   Set( _SET_DATEFORMAT, "YYYY/MM/DD" )
   HBTEST Transform( 12345678, "@D" )                   IS "1234/56/78"
   Set( _SET_DATEFORMAT, "YYYY.MM.DD" )
   HBTEST Transform( 1234.56789, "@D" )                 IS "1234.56.9 "
   Set( _SET_DATEFORMAT, "YYYY.MM:DD" )
   HBTEST Transform( 1234.56789, "@D" )                 IS "1234.56:79"
   HBTEST Transform( 123.345678912, "@D " )             IS " 123.34:57"
   Set( _SET_DATEFORMAT, "MM-DD-YYYY" )
   HBTEST Transform( .T., "@RE <|,yY#lL,|>" )           IS "99-99-9999T"
   HBTEST Transform( .F., "@RE <|,yY#lL,|>" )           IS "99-99-9999F"
   HBTEST Transform( .T., "@RD <|,yY#lL,|>" )           IS "99-99-9999T"
   HBTEST Transform( .F., "@RD <|,yY#lL,|>" )           IS "99-99-9999F"
   HBTEST Transform( .F., "@DE <|,yY#lL,|>" )           IS "9"
   HBTEST Transform( "abcdefghij", "@S15! <XXXXXXXX>" ) IS "<BCDEFGHI>"
   HBTEST Transform( "abcdefghij", "@S0! <XXXXXXXX>" )  IS "<BCDEFGHI>"
   HBTEST Transform( "abcdefghij", "@S5! <XXXXXXXX>" )  IS "<BCDE"

   sf := Set( _SET_FIXED, .T. )
   HBTEST Transform( 1234,          )                   IS "            1234"
   HBTEST Transform( 1234,     ""   )                   IS "            1234"
   HBTEST Transform( 1234,     "@"  )                   IS "            1234"
   HBTEST Transform( 1234,     "@!" )                   IS "            1234"
   HBTEST Transform( -1234,         )                   IS "           -1234"
   HBTEST Transform( -1234,    "@"  )                   IS "           -1234"
#ifdef HB_CLP_STRICT
   HBTEST Transform( Round( 123,0 ),       )            IS "       123.00"
   HBTEST Transform( Round( 123,0 ), "@!" )             IS "       123.00"
   HBTEST Transform( Round( 123.0,0 ),     )            IS "       123.00"
   HBTEST Transform( Round( 123.0,0 ), "@!" )           IS "       123.00"
#endif

   HBTEST Transform( 1234.567,      )                   IS "      1234.57"
   HBTEST Transform( 1234.567,   "" )                   IS "      1234.57"
   HBTEST Transform( 1234.567, "@"  )                   IS "      1234.57"
   HBTEST Transform( 1234.567, "@!" )                   IS "      1234.57"
   HBTEST Transform( -1234.567,     )                   IS "     -1234.57"
   HBTEST Transform( -1234.567, "@" )                   IS "     -1234.57"
   HBTEST Transform( Val( "-1.0" ),   )                 IS "-1.00"
   HBTEST Transform( Val( "-1.0" ), "@" )               IS "-1.00"
   HBTEST Transform( Val( "-123" ),   )                 IS "      -123"
   HBTEST Transform( Val( "-123" ), "@" )               IS "      -123"
   HBTEST Transform( 0,             )                   IS "               0"
   HBTEST Transform( 0.0,           )                   IS "         0.00"
   HBTEST Transform( Val( "1" ),      )                 IS "      1"
   HBTEST Transform( Val( "12" ),     )                 IS "      12"
   HBTEST Transform( Val( "123" ),    )                 IS "      123"
   HBTEST Transform( Val( "1234" ),   )                 IS "      1234"

   sd := Set( _SET_DECIMALS, 3 )
   HBTEST Transform( 0.0,           )                   IS "         0.000"
   HBTEST Transform( Val( "1" ),      )                 IS "        1"
   HBTEST Transform( Val( "12" ),     )                 IS "        12"
   HBTEST Transform( Val( "123" ),    )                 IS "        123"
   HBTEST Transform( Val( "1234" ),   )                 IS "        1234"
   Set( _SET_DECIMALS, 4 )
   HBTEST Transform( 0.0,           )                   IS "         0.0000"
   HBTEST Transform( Val( "1" ),      )                 IS "          1"
   HBTEST Transform( Val( "12" ),     )                 IS "          12"
   HBTEST Transform( Val( "123" ),    )                 IS "          123"
   HBTEST Transform( Val( "1234" ),   )                 IS "          1234"
   Set( _SET_FIXED, .F. )

   HBTEST Transform( -1234,  )                          IS "     -1234"
   HBTEST Transform( -1234, "@B" )                      IS "-1234     "
   HBTEST Transform( -1234, "@(" )                      IS "(     1234)"
   HBTEST Transform( -1234, "@)" )                      IS "     (1234)"
   HBTEST Transform( -1234, "@B)" )                     IS "(1234)     "
   HBTEST Transform( -1234, "@B(" )                     IS "(1234)     "
   HBTEST Transform( "(  12)", "@B(" )                  IS "(  12)"
   HBTEST Transform( "(  12)", "@B)" )                  IS "(  12)"
   HBTEST Transform( "   12", "@B(" )                   IS "12   "
   HBTEST Transform( "   12", "@B)" )                   IS "12   "
#ifdef __HARBOUR__
   HBTEST Transform( 1234, "@L" )                       IS "0000001234"
   HBTEST Transform( 1234, "@0" )                       IS "0000001234"
   HBTEST Transform( 1234, "@L(" )                      IS "0000001234"
   HBTEST Transform( 1234, "@0)" )                      IS "0000001234"
   HBTEST Transform( -1234, "@L(" )                     IS "(000001234)"
   HBTEST Transform( -1234, "@0)" )                     IS "(000001234)"
   /* please test it with FoxPro and Xbase++
      to check if they give the same result */
   HBTEST Transform( -1234, "@L" )                      IS "-000001234"
   HBTEST Transform( -1234, "@0" )                      IS "-000001234"
#endif
   /* FlagShip extensions */
   HBTEST Transform( -1234, "@Z" )                      IS "     -1234"
   HBTEST Transform( 1234, "@Z" )                       IS "      1234"
   HBTEST Transform( -1234, "@F" )                      IS "     -1234"
   HBTEST Transform( 1234, "@F" )                       IS "      1234"
   HBTEST Transform( -1234, "@T" )                      IS "     -1234"
   HBTEST Transform( 1234, "@T" )                       IS "      1234"

   HBTEST Transform( 123456789.12, "@,39 999,999,999.99" )    IS "123,456,789.12"
   HBTEST Transform( 123456789.12, "@,39 999,999,999.99" )    IS "123,456,789.12"
   HBTEST Transform( 123.456, "@R 9 9 9.9" )                  IS "1 2 3.5"

   Set( _SET_DATEFORMAT, df )
   Set( _SET_FIXED, sf )
   Set( _SET_DECIMALS, sd )

   RETURN
