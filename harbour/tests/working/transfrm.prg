#include "set.ch"

/*
 * Test of the transform function
 *
 * Copyright (C) 1999  Eddie Runia <eddie@runia.comu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 */

function Main()

   __SetCentury(.T.)
   Set(_SET_DATEFORMAT, "dd/mm/yyyy")
   QOut( "Hallo      ", "!!!!!       ", transform( "Hallo   ", "!!!!!"    ) )
   QOut( "Hallo      ", "!!A!!       ", transform( "Hallo   ", "!!A!!"    ) )
   QOut( "Hallo      ", "!!A9!       ", transform( "Hallo   ", "!!A9!"    ) )
   QOut( "Hallo      ", "!QA9!       ", transform( "Hallo   ", "!QA9!"    ) )
   QOut( "Hallo      ", "ZQA9!       ", transform( "Hallo   ", "ZQA9!"    ) )
   QOut( "Hall       ", "ZQA9!       ", transform( "Hall"    , "ZQA9!"    ) )
   QOut( "Hallo      ", "!AAA        ", transform( "Hallo   ", "!AAA"     ) )
   QOut( "Hallo      ", "@!          ", transform( "Hallo   ", "@!"       ) )
   QOut( "Hallo      ", "@! AA       ", transform( "Hallo   ", "@! AA"    ) )
   QOut( "Hallo      ", "@R          ", transform( "Hallo   ", "@R"       ) )
   QOut( "Hallo      ", "@Z          ", transform( "Hallo   ", "@Z"       ) )
   QOut( "Hallo      ", "@R !!       ", transform( "Hallo   ", "@R !!"    ) )
   QOut( "Hi         ", "@R !!!      ", transform( "Hi"      , "@R !!!"   ) )
   QOut( "Hallo      ", "            ", transform( "Hallo   ", ""         ) )
   Pause()
   QOut( ".T.        ", "            ", transform( .T.       , ""         ) )
   QOut( ".F.        ", "            ", transform( .F.       , ""         ) )
   QOut( ".T.        ", "L           ", transform( .T.       , "L"        ) )
   QOut( ".F.        ", "L           ", transform( .F.       , "L"        ) )
   QOut( ".T.        ", "Y           ", transform( .T.       , "Y"        ) )
   QOut( ".F.        ", "Y           ", transform( .F.       , "Y"        ) )
   QOut( ".T.        ", "X           ", transform( .T.       , "X"        ) )
   QOut( ".F.        ", "#           ", transform( .F.       , "#"        ) )
   QOut( ".T.        ", "X!          ", transform( .T.       , "X!"       ) )
   QOut( ".F.        ", "@R Y        ", transform( .F.       , "@R Y"     ) )
   QOut( ".T.        ", "@R X!       ", transform( .T.       , "@R X!"    ) )
   Pause()
   QOut( "15         ", "9999        ", transform( 15        , "9999"     ) )
   QOut( "1.5        ", "99.99       ", transform( 1.5       , "99.99"    ) )
   QOut( "1.5        ", "9999        ", transform( 1.5       , "9999"     ) )
   QOut( "15         ", "####        ", transform( 15        , "####"     ) )
   QOut( "1.5        ", "##.##       ", transform( 1.5       , "##.##"    ) )
   QOut( "1.5        ", "####        ", transform( 1.5       , "####"     ) )
   QOut( "15         ", " AX##       " ,transform( 15        , " AX##"    ) )
   QOut( "1.5        ", "!9XPA.9     " ,transform( 1.5       , "!9XPA.9"  ) )
   QOut( "-15        ", "9999        ", transform( -15       , "9999"     ) )
   QOut( "-1.5       ", "99.99       ", transform( -1.5      , "99.99"    ) )
   QOut( "-15        ", "$999        ", transform( -15       , "$999"     ) )
   QOut( "-1.5       ", "*9.99       ", transform( -1.5      , "*9.99"    ) )
   QOut( "41         ", "$$$9        ", transform( 41        , "$$$9"     ) )
   QOut( "41         ", "***9        ", transform( 41        , "***9"     ) )
   QOut( "15000      ", "9999        ", transform( 15000     , "9999"     ) )
   QOut( "15000      ", "99,999      ", transform( 15000     , "99,999"   ) )
   Pause()
   QOut( "1500       ", "99,999      ", transform( 1500      , "99,999"   ) )
   QOut( "150        ", "99,999      ", transform( 150       , "99,999"   ) )
   QOut( "150        ", "99,99       ", transform( 150       , "99,99"    ) )
   QOut( "41         ", "@Z 9999     ", transform( 41        , "@Z 9999"  ) )
   QOut( "0          ", "@Z 9999     ", transform( 0         , "@Z 9999"  ) )
   QOut( "41         ", "@0 9999     ", transform( 41        , "@0 9999"  ), "  (Harbour Power !)" )
   QOut( "0          ", "@0 9999     ", transform( 0         , "@0 9999"  ), "  (Harbour Power !)" )
   QOut( "41         ", "@B 9999     ", transform( 41        , "@B 9999"  ) )
   QOut( "41         ", "@B 99.9     ", transform( 41        , "@B 99.9"  ) )
   Pause()
   QOut( "7          ", "@B 99.9     ", transform( 7         , "@B 99.9"  ) )
   QOut( "7          ", "@C 99.9     ", transform( 7         , "@C 99.9"  ) )
   QOut( "-7         ", "@C 99.9     ", transform( -7        , "@C 99.9"  ) )
   QOut( "7          ", "@X 99.9     ", transform( 7         , "@X 99.9"  ) )
   QOut( "-7         ", "@X 99.9     ", transform( -7        , "@X 99.9"  ) )
   QOut( "7          ", "@( 99.9     ", transform( 7         , "@( 99.9"  ) )
   QOut( "-7         ", "@( 99.9     ", transform( -7        , "@( 99.9"  ) )
   QOut( "7          ", "9X9Z5.9     ", transform( 7         , "9X9Z5.9"  ) )
   QOut( "-7         ", "@R 9X9^     ", transform( -7        , "@R 9X9^"  ) )
   QOut( "-7         ", "9X9^        ", transform( -7        , "9X9^"     ) )
   QOut( "1          ", "@R 9HI!     ", transform( 1         , "@R 9HI!"  ) )
   QOut( "1          ", "9HI!        ", transform( 1         , "9HI!"     ) )
   QOut( "-12        ", "@( 99       ", transform( -12       , "@( 99"    ), "  (BUG Fix)" )
   QOut( "12         ", "@( 99       ", transform( 12        , "@( 99"    ) )
   Pause()
   QOut( "1          ", "            ", transform( 1         , ""         ) )
   QOut( "32768      ", "            ", transform( 32768     , ""         ) )
   QOut( "-20        ", "            ", transform( -20       , ""         ) )
   QOut( "1048576    ", "            ", transform( 1048576   , ""         ) )
   QOut( "21.65      ", "            ", transform( 21.65     , ""         ) )
   QOut( "-3.33      ", "            ", transform( -3.33     , ""         ) )
   Pause()
   QOut( "-1234      ", "@( 9999     ", transform( -1234     , "@( 9999"  ), "  (BUG Fix)" )
   QOut( "-1234      ", "@B( 9999    ", transform( -1234     , "@B 9999"  ) )
   QOut( "1234       ", "@E 9,999.99 ", transform( 1234      , "@E 9,999.99" ) )
   QOut( "12.2       ", "@E 9,999.99 ", transform( 12.2      , "@E 9,999.99" ) )
   QOut( "-1234      ", "@X 9999     ", transform( -1234     , "@X 9999"  ) )
   QOut( "-1234      ", "@BX 9999    ", transform( -1234     , "@BX 9999" ) )
   QOut( "1234       ", "@X 9999     ", transform( 1234      , "@B 9999"  ) )
   QOut( "1234       ", "@BX 9999    ", transform( 1234      , "@BX 9999" ) )
   QOut( "1234       ", "@D 9999     ", transform( 1234      , "@D 9999"  ), "  (BUG Fix)" )
   QOut( "1234       ", "@BD 9999    ", transform( 1234      , "@BD 9999" ), "  (BUG Fix)" )
   QOut( "0          ", "@Z 9999     ", transform( 0         , "@Z 9999"  ) )
   QOut( "0          ", "@BZ 9999    ", transform( 0         , "@BZ 9999" ) )
//   QOut( "NIL        ", "9           ", transform( NIL       , "9"        ) )
//   ^ Generates error ^
   Pause()
   QOut( "12/12/1990 ", "99/99/9999  ", transform( ctod("12/12/1990") , "99/99/9999"  ) )
   QOut( "02/12/1990 ", "99.99.9999  ", transform( ctod("02/12/1990") , "99.99.9999"  ) )
   QOut( "  /  /     ", "99/99/9999  ", transform( ctod("  /  /    ") , "99/99/9999"  ) )
   QOut( "02/12/1990 ", "99/99/99    ", transform( ctod("02/12/1990") , "99/99/99"    ) )
   QOut( "12/12/1990 ", "99-99-99    ", transform( ctod("12/12/1990") , "99-99-99"    ) )
   QOut( "30/04/2004 ", "99.99.99    ", transform( ctod("30/04/2004") , "99.99.99"    ) )
   QOut( "  /  /     ", "99/99/99    ", transform( ctod("  /  /    ") , "99/99/99"    ) )
   QOut( "01/01/1992 ", "THISWRNG    ", transform( ctod("01/01/1992") , "THISWRNG"    ) )
   QOut( "05/06/1935 ", "999/99/9    ", transform( ctod("05/06/1935") , "999/99/9"    ) )
   QOut( "12/11/1910 ", "9#-9#/##    ", transform( ctod("12/11/1910") , "9#-9#/##"    ) )
   QOut( "01/01/1992 ", "            ", transform( ctod("01/01/1992") , ""            ) )
   QOut( "01/01/1992 ", "DO THIS     ", transform( ctod("01/01/1992") , "DO THIS "    ) )
   QOut( "02/01/1992 ", "@E          ", transform( ctod("02/01/1992") , "@E"          ) )
   QOut("")
return nil

function Pause()

   QOut("")
   __Accept( "Pause:" )
return nil
