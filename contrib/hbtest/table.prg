/*
 * Generate test .dbf (and generate the generator)
 *
 * Copyright 2015 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* Run this source file as script to regenerate the codes from Windows SDK:
      hbrun <this_source>.prg [<dbf-filename>]
 */

#if defined( __HBSCRIPT__HBSHELL )

#include "dbstruct.ch"

PROCEDURE Main( cInputFile )

   LOCAL cOutput

   LOCAL fld
   LOCAL tmp

   hb_default( @cInputFile, hb_DirBase() + hb_DirSepToOS( "../../tests/test.dbf" ) )

   ? "Input file:", cInputFile

   BEGIN SEQUENCE WITH __BreakBlock()
      USE ( cInputFile ) NEW READONLY SHARED
   END SEQUENCE

   IF Used()

      cOutput := hb_MemoRead( __FILE__ )
      IF ( tmp := RAt( "<<< */", cOutput ) ) > 0
         cOutput := Left( cOutput, tmp - 1 ) + "<<< */" + hb_eol() + hb_eol()
      ENDIF

      cOutput += "FUNCTION hbtest_Table( cAlias, ... )" + hb_eol()

      IF LastRec() > 0
         cOutput += ;
            hb_eol() + ;
            "   LOCAL r, f" + hb_eol()
      ENDIF

      cOutput += hb_eol()
      cOutput += "   IF ! hb_dbCreateTemp( hb_defaultValue( cAlias, " + '"' + "w_TEST" + '"' + " ), { ;" + hb_eol()
      FOR EACH fld IN dbStruct()
         cOutput += ;
            "      " + ;
            "{ " + PadR( '"' + fld[ DBS_NAME ] + '"', 12 ) + ", " + ;
            '"' + fld[ DBS_TYPE ] + '"' + ", " + ;
            Str( fld[ DBS_LEN ], 3 ) + ", " + ;
            Str( fld[ DBS_DEC ], 2 ) + " }" + ;
            iif( fld:__enumIsLast(), " }, ... )", ", ;" ) + hb_eol()
      NEXT
      cOutput += ;
         "      RETURN .F." + hb_eol() + ;
         "   ENDIF" + hb_eol()

      IF LastRec() > 0

         cOutput += ;
            hb_eol() + ;
            "   FOR EACH r IN { ;" + hb_eol()

         dbGoTop()
         DO WHILE ! Eof()
            cOutput += "      { "
            FOR tmp := 1 TO FCount()
               cOutput += ;
                  hb_ValToExp( ;
                     iif( hb_FieldType( tmp ) == "C", RTrim( FieldGet( tmp ) ), FieldGet( tmp ) ) ) + ;
                  iif( tmp == FCount(), ;
                     iif( RecNo() == LastRec(), " } }", " }, ;" ) + hb_eol(), ", " )
            NEXT
            dbSkip()
         ENDDO

         cOutput += ;
            "      dbAppend()" + hb_eol() + ;
            "      FOR EACH f IN r" + hb_eol() + ;
            "         FieldPut( f:__enumIndex(), f )" + hb_eol() + ;
            "      NEXT" + hb_eol() + ;
            "   NEXT" + hb_eol() + ;
            "   dbGoTop()" + hb_eol()
      ENDIF

      cOutput += ;
         hb_eol() + ;
         "   RETURN .T." + hb_eol()

      dbCloseArea()

      ? iif( hb_MemoWrit( __FILE__, cOutput ), "Saved OK:", "Save error:" ), __FILE__
   ELSE
      ? "Input file not found or invalid"
   ENDIF

   RETURN

#endif

/* DO NOT REMOVE OR MODIFY THIS LINE, OR ANY CODE BELOW <<< */

FUNCTION hbtest_Table( cAlias, ... )

   LOCAL r, f

   IF ! hb_dbCreateTemp( hb_defaultValue( cAlias, "w_TEST" ), { ;
      { "FIRST"     , "C",  20,  0 }, ;
      { "LAST"      , "C",  20,  0 }, ;
      { "STREET"    , "C",  30,  0 }, ;
      { "CITY"      , "C",  30,  0 }, ;
      { "STATE"     , "C",   2,  0 }, ;
      { "ZIP"       , "C",  10,  0 }, ;
      { "HIREDATE"  , "D",   8,  0 }, ;
      { "MARRIED"   , "L",   1,  0 }, ;
      { "AGE"       , "N",   2,  0 }, ;
      { "SALARY"    , "N",   6,  0 }, ;
      { "NOTES"     , "C",  70,  0 } }, ... )
      RETURN .F.
   ENDIF

   FOR EACH r IN { ;
      { "Homer", "Simpson", "32179 Maiden Lane", "Springfield", "IL", "20503-8202", 0d19920918, .T., 6, 5900, "Record 1" }, ;
      { "Ceci", "Gibbard", "9540 Raynes Park Road", "Miami", "MA", "55774-2304", 0d19841017, .F., 28, 123700, "Record 2" }, ;
      { "Reg", "Kaczocha", "30522 Park Ten Place", "Scottsdale", "WY", "09226-1483", 0d19890523, .T., 43, 82900, "Record 3" }, ;
      { "Ralph", "Jochum", "8211 Carnegie Center", "Hingham", "SD", "71947-5114", 0d19850509, .F., 34, 138300, "Record 4" }, ;
      { "Simpson", "Jaffee", "32736 Meadowbrook Drive", "Nedlands", "ID", "38179-3789", 0d19901211, .T., 88, 51800, "Record 5" }, ;
      { "Tom", "Logan", "6180 Roselle Street", "West Covina", "CT", "82378-0904", 0d19920224, .F., 90, 20400, "Record 6" }, ;
      { "Gary", "Brock", "3893 Canandaigua Road", "Senford", "WV", "94177-5329", 0d19870912, .T., 58, 145300, "Record 7" }, ;
      { "Frank", "Fonseca", "18712 Sherman Way", "Ashby", "RI", "08218-8409", 0d19880216, .F., 46, 118900, "Record 8" }, ;
      { "Rick", "Sencovici", "13802 South University", "Arcadia", "HI", "82063-8091", 0d19870117, .T., 55, 23700, "Record 9" }, ;
      { "Hugh", "Lupton", "16472 S. LaSalle Street", "Tarzana", "AK", "79021-0643", 0d19890828, .F., 89, 96700, "Record 10" }, ;
      { "Oskar", "Farley", "19123 Washington Street", "Boston", "IN", "25885-0851", 0d19850831, .T., 46, 77300, "Record 11" }, ;
      { "Johnny", "Fischer", "30621 Inridge Drive", "McLean", "WA", "86275-8035", 0d19881112, .F., 37, 2300, "Record 12" }, ;
      { "Corkey", "Young", "9069 Avon Place", "Lund", "NC", "36199-1793", 0d19881224, .T., 54, 30000, "Record 13" }, ;
      { "Phyllis", "Lechuga", "1457 Indianapolis Ave", "Council Bluffs", "AR", "73036-5749", 0d19870129, .F., 94, 84600, "Record 14" }, ;
      { "Chester", "Padilla", "32385 Federal Street", "Ashby", "MS", "82882-2447", 0d19851222, .T., 90, 144000, "Record 15" }, ;
      { "Ali", "Halverson", "10614 No. Sheridan Way", "Fort Collins", "VT", "99748-6645", 0d19910517, .F., 57, 136700, "Record 16" }, ;
      { "Renee", "Chism", "136 West Markham", "Catawba", "NY", "58492-3691", 0d19890225, .T., 78, 9700, "Record 17" }, ;
      { "Vincent", "Woiska", "13093 4th Street", "Jackson", "WY", "28063-0466", 0d19860711, .F., 81, 49600, "Record 18" }, ;
      { "Arnaldo", "Waldrop", "27185 Terri-Lyn", "Devon Park", "KY", "32428-6555", 0d19920513, .T., 74, 106500, "Record 19" }, ;
      { "Herbert", "Fuller", "4057 Parkside Avenue", "Canning Vale", "LA", "03556-9645", 0d19890116, .F., 81, 100500, "Record 20" }, ;
      { "Jimmy", "Coleman", "27233 Amsterdam Ave", "Bayside", "VA", "14986-9684", 0d19880611, .T., 96, 93800, "Record 21" }, ;
      { "Garland", "Humphrey", "31565 Brookwood Drive", "Atlanta", "DE", "18082-8322", 0d19850120, .F., 57, 12200, "Record 22" }, ;
      { "Dave", "Van Allen", "12442 Highbury Avenue", "Camarillo", "HI", "72163-4364", 0d19840112, .T., 86, 133800, "Record 23" }, ;
      { "Homer", "Dodd", "7139 Lawrence Ave West", "Rochester", "WI", "58069-9538", 0d19870208, .F., 21, 17600, "Record 24" }, ;
      { "Shawn", "Lupton", "9119 Northern Parkway", "East Lempster", "FL", "64531-9221", 0d19860426, .T., 84, 84200, "Record 25" }, ;
      { "Andy", "Pels", "3302 Cherokee Blvd", "Stevensville", "TN", "75529-3235", 0d19881210, .F., 76, 116600, "Record 26" }, ;
      { "Alfred", "Reslow", "24662 Telluride Road", "Fullerton", "WA", "01900-1402", 0d19850604, .T., 31, 36100, "Record 27" }, ;
      { "Jamie", "Lyth", "6929 Prince Charles Drive", "Paramus", "NC", "01540-4969", 0d19880715, .F., 80, 30500, "Record 28" }, ;
      { "Armand", "Cowen", "25838 Commonwealth Drive", "Temecula", "KS", "66763-1397", 0d19910107, .T., 49, 91100, "Record 29" }, ;
      { "Barrie", "Thacker", "14160 Kent Road", "Clearbrook", "PA", "54253-1441", 0d19850420, .F., 87, 68100, "Record 30" }, ;
      { "Earl", "Elbaytam", "3138 W. Bullard", "Ft. Lauderdale", "MS", "03367-6761", 0d19920203, .T., 70, 119500, "Record 31" }, ;
      { "Varouj", "Dickens", "25543 Black Avenue", "Murdoch", "VT", "47647-8801", 0d19860105, .F., 89, 57800, "Record 32" }, ;
      { "Matthew", "Saylors", "15742 S. 608 Lilac Lane", "Hauppauge", "IL", "04933-0079", 0d19910907, .T., 29, 81300, "Record 33" }, ;
      { "Lucian", "Stanton", "7953 Waterford Drive", "Athens", "CA", "36127-8609", 0d19891105, .F., 71, 121100, "Record 34" }, ;
      { "Keith", "Lokken", "29698 Seabreeze Avenue", "Stevensville", "IA", "01036-4252", 0d19920829, .T., 96, 107200, "Record 35" }, ;
      { "Cindy", "Voltz", "4556 Pipit Street NE", "Schaumburg", "ID", "62218-9817", 0d19840301, .F., 66, 6500, "Record 36" }, ;
      { "Holger", "Humphrey", "900 Butler Pike", "Lisle", "CT", "08281-0480", 0d19831102, .T., 55, 134600, "Record 37" }, ;
      { "Maritza", "Sencovici", "595 S Dadeland Blvd", "Bethesda", "ME", "62686-3230", 0d19840722, .F., 51, 111800, "Record 38" }, ;
      { "Ceci", "Caster", "15482 South Marine Drive", "Newark", "OH", "30835-9815", 0d19900427, .T., 89, 51700, "Record 39" }, ;
      { "Andreas", "Amaral", "21195 Pheasant", "Nedlands", "NH", "29359-6212", 0d19880814, .F., 48, 3800, "Record 40" }, ;
      { "Bam", "Kaster", "21688 Murietta Ave", "Long Beach", "MI", "38440-5250", 0d19920108, .T., 27, 130900, "Record 41" }, ;
      { "Holger", "Hubbard", "3426 Windermere Ave.", "Mays Landing", "OK", "40816-5287", 0d19830718, .F., 99, 89400, "Record 42" }, ;
      { "Issam", "Gibbons", "14422 South Corning Street", "Langhorne", "FL", "54307-7497", 0d19910710, .T., 60, 133800, "Record 43" }, ;
      { "Fabio", "Quinn", "17108 St George St", "Topeka", "NM", "76510-6571", 0d19831120, .F., 53, 92400, "Record 44" }, ;
      { "Ted", "Bailey", "9941 Cherokee Blvd", "Norwood", "IN", "70255-8644", 0d19910405, .T., 49, 76000, "Record 45" }, ;
      { "Neil", "Valle", "16175 Efna Park Drive", "Brooklyn", "TN", "32923-5070", 0d19880219, .F., 36, 144700, "Record 46" }, ;
      { "Randy", "Scheffler", "16515 Stratford Lane", "Williston", "SC", "16390-5661", 0d19840930, .T., 87, 51200, "Record 47" }, ;
      { "Alon", "Woo", "22745 Plaza Court", "East Lempster", "CO", "44812-7707", 0d19861226, .F., 99, 29700, "Record 48" }, ;
      { "Geoff", "Elsey", "26652 Great North Road", "Oak Park", "AR", "82450-5863", 0d19840107, .T., 73, 145300, "Record 49" }, ;
      { "Paul", "Josefson", "24016 Summer Street", "Lombard", "PA", "36334-0043", 0d19870728, .F., 79, 106500, "Record 50" }, ;
      { "Alan", "Hsu", "7347 Valley Drive", "Glenview", "MT", "33265-9464", 0d19900908, .T., 26, 17200, "Record 51" }, ;
      { "Eric", "VanderHeyden", "25394 Lunt Ave", "San Diego", "AZ", "92980-6593", 0d19900104, .F., 84, 34500, "Record 52" }, ;
      { "Jon", "Locke", "12241 Fox Run Road", "Vienna", "CA", "81658-9266", 0d19860302, .T., 90, 7600, "Record 53" }, ;
      { "Issam", "Meisal", "31486 Luanne Avenue", "White Plaine", "IA", "52282-1840", 0d19831030, .F., 59, 62700, "Record 54" }, ;
      { "Harry", "Neidorff", "31709 New York Avenue N.W.", "La Mesa", "NE", "23851-5264", 0d19840410, .T., 66, 57400, "Record 55" }, ;
      { "Howard", "Duke", "5145 Terri-Lyn", "Tamuning", "DE", "21043-4711", 0d19920624, .F., 48, 69500, "Record 56" }, ;
      { "Michael", "Floriar", "32445 N. Market Street", "Morton Grove", "HI", "92242-4827", 0d19851020, .T., 63, 125200, "Record 57" }, ;
      { "Reg", "Hong", "2197 South 108th Street", "Preston", "OK", "54298-2424", 0d19850528, .F., 89, 79700, "Record 58" }, ;
      { "Cindy", "Lupton", "31931 King Avenue", "Kaneohe", "NM", "61246-8515", 0d19861005, .T., 46, 131500, "Record 59" }, ;
      { "Peter", "Carbello", "7727 Muller Rd", "Kansas City", "TN", "53020-4320", 0d19880429, .F., 47, 28400, "Record 60" }, ;
      { "Gary", "Rotter", "4010 Congress Avenue", "Winnipeg", "NC", "59158-4849", 0d19890714, .T., 88, 73300, "Record 61" }, ;
      { "Patrick", "Kaufmann", "32023 Ash Street", "Flushing", "AR", "05473-7097", 0d19841216, .F., 31, 42700, "Record 62" }, ;
      { "Olaf", "Kaster", "17994 Efna Park Drive", "Macedon", "PA", "93160-0829", 0d19910720, .T., 35, 122300, "Record 63" }, ;
      { "Oved", "Poblete", "24821 Lake Cook Road", "Kowloon Bay", "MT", "44605-3770", 0d19900204, .F., 88, 127400, "Record 64" }, ;
      { "Roger", "Powless", "14314 S. 608 Lilac Lane", "Orland Park", "TX", "65674-9843", 0d19870103, .T., 29, 49700, "Record 65" }, ;
      { "Sandy", "Steinberg", "2089 Chimney Springs Drive", "Missoula", "CA", "20944-9936", 0d19911030, .F., 94, 67100, "Record 66" }, ;
      { "Mark", "Bailey", "12366 Riva Road", "Midland", "LA", "72226-4372", 0d19861227, .T., 38, 3600, "Record 67" }, ;
      { "Laura", "Baldwin", "9555 North Redgum", "Hingham", "IA", "39547-0277", 0d19840711, .F., 63, 90600, "Record 68" }, ;
      { "Francisco", "Weisbeck", "22481 Hui Iwa St", "Anaheim", "VA", "81235-3895", 0d19911009, .T., 50, 23200, "Record 69" }, ;
      { "Dick", "Cooper", "22293 E Lincoln", "Lake Worth", "DE", "53794-3318", 0d19920220, .F., 51, 28300, "Record 70" }, ;
      { "Ralf", "Burke", "31691 Cantrell Road", "Axton", "RI", "39970-1581", 0d19830418, .T., 50, 128500, "Record 71" }, ;
      { "Jamie", "Caster", "27157 Madison Avenue", "Greenwich", "WI", "45955-2340", 0d19880120, .F., 38, 49800, "Record 72" }, ;
      { "James", "Carbello", "23350 East Holland Avenue", "Julian", "AK", "07111-5817", 0d19911220, .T., 27, 78600, "Record 73" }, ;
      { "Martin", "LeMont", "26950 William Penn Hwy", "Rosemont", "AL", "31276-2138", 0d19900703, .F., 85, 145200, "Record 74" }, ;
      { "Homer", "Klein", "7053 Mountainview Parade", "Fountain Valley", "TN", "30799-2210", 0d19830714, .T., 79, 2400, "Record 75" }, ;
      { "Ken", "Dodson", "16489 Sunnybank", "Columbus", "NC", "76618-6257", 0d19890606, .F., 57, 133200, "Record 76" }, ;
      { "Steven", "Ridenour", "18934 Maxanne Drive", "Kaneohe", "KS", "97957-3314", 0d19910123, .T., 47, 61700, "Record 77" }, ;
      { "Sandy", "Scholz", "29903 St George St", "Alberta", "MS", "24580-9849", 0d19910929, .F., 91, 83200, "Record 78" }, ;
      { "Herbert", "Fuller", "3233 South Broadway", "Morton Grove", "TX", "34867-5093", 0d19860209, .T., 85, 70900, "Record 79" }, ;
      { "Jason", "Hong", "14434 Lake Cook Road", "New Brunswick", "IL", "98992-1192", 0d19890622, .F., 99, 107400, "Record 80" }, ;
      { "Olaf", "Moore", "26736 S. LaSalle Street", "Los Angeles", "WY", "21943-8952", 0d19870310, .T., 35, 119500, "Record 81" }, ;
      { "Irene", "Nardamast", "23269 Melrose Avenue", "Van Nuys", "LA", "43570-3647", 0d19840516, .F., 31, 80100, "Record 82" }, ;
      { "Renee", "Muth", "16089 West University", "Little Rock", "ID", "80839-9529", 0d19910521, .T., 91, 145600, "Record 83" }, ;
      { "Ann", "Petersen", "23928 Sixth Street", "Rachno Cucamonga", "NE", "34048-3879", 0d19880211, .F., 48, 108400, "Record 84" }, ;
      { "Geoffrey", "Clark", "26634 Roselle Street", "Greensborough", "OH", "10216-1434", 0d19920407, .T., 85, 115300, "Record 85" }, ;
      { "Bruce", "Howarth", "11360 Guilford Street", "Alphoretta", "RI", "95491-5923", 0d19880404, .F., 70, 60800, "Record 86" }, ;
      { "Luc", "Halverson", "3404 NW 12th Street", "Springfield", "HI", "82639-2466", 0d19910723, .T., 63, 97500, "Record 87" }, ;
      { "Adele", "Schauer", "17158 Harbour View", "Rosemont", "OK", "00307-0199", 0d19841208, .F., 91, 53300, "Record 88" }, ;
      { "Joel", "Nehme", "10155 Luanne Avenue", "Easton", "MN", "16192-2533", 0d19890326, .T., 76, 29900, "Record 89" }, ;
      { "Roger", "Peissner", "15934 4th Street", "Tampa", "NC", "53488-2103", 0d19850118, .F., 91, 82700, "Record 90" }, ;
      { "Sally", "Maldener", "18132 Escalon Drive", "River Edge", "AR", "75628-1936", 0d19910604, .T., 97, 58600, "Record 91" }, ;
      { "Mara", "Jonasson", "8395 West 66th Street", "Woodbridge", "KS", "81280-1450", 0d19900826, .F., 27, 83000, "Record 92" }, ;
      { "David", "Harrell", "1087 Focht Avenue", "Depew", "MS", "43858-2100", 0d19890603, .T., 32, 103700, "Record 93" }, ;
      { "Jobst", "Dysert", "20000 California Ave", "Chesapeake", "MT", "83926-0293", 0d19830705, .F., 66, 130300, "Record 94" }, ;
      { "Orlando", "Dowd", "18063 No. Sheridan Way", "Chattanooga", "NV", "47242-1342", 0d19920129, .T., 66, 30900, "Record 95" }, ;
      { "Jon", "Brokaw", "32617 Buena Vista Ave", "Fountain Valley", "AZ", "53497-2429", 0d19831229, .F., 49, 134000, "Record 96" }, ;
      { "Harold", "Wechter", "14646 East Huntington Dr", "Hermosa Beach", "KY", "70597-2109", 0d19911008, .T., 91, 130500, "Record 97" }, ;
      { "Edward", "Wagner", "14027 West Terrace", "El Paso", "ND", "98794-8396", 0d19880829, .F., 77, 19100, "Record 98" }, ;
      { "Doyle", "Cohen", "7262 Horizon Court", "Allston", "NE", "41824-8321", 0d19920806, .T., 85, 134500, "Record 99" }, ;
      { "Dr. Edwin", "Weicher", "29929 Carmenita Road", "West Hills", "WV", "12340-6979", 0d19870126, .F., 57, 83000, "Record 100" }, ;
      { "Adele", "Cole", "20741 N Los Altos", "Schaumburg", "OH", "54352-9853", 0d19840603, .T., 35, 14700, "Record 101" }, ;
      { "Leighann", "Broyles", "13480 Cumberland Ave", "Rosswell", "RI", "41536-5328", 0d19831221, .F., 32, 135900, "Record 102" }, ;
      { "Orlando", "Rumaner", "17769 Highland Ave", "Worcester", "MD", "80227-2767", 0d19830711, .T., 40, 89100, "Record 103" }, ;
      { "Scott", "Fergusson", "28981 Willow Parkway", "MacLeod", "AK", "73828-9326", 0d19840514, .F., 43, 44600, "Record 104" }, ;
      { "Garland", "Spray", "9636 Galleria Blvd", "Scottsdale", "AL", "35182-5731", 0d19830414, .T., 73, 138000, "Record 105" }, ;
      { "Wayne", "Yusof", "14917 South Belmont Ave", "Hales Corners", "IN", "33481-4631", 0d19841017, .F., 61, 39000, "Record 106" }, ;
      { "Roberto", "Muth", "29848 Pheasant", "Athens", "SC", "53623-1108", 0d19880719, .T., 89, 120300, "Record 107" }, ;
      { "Brian", "Cabon", "6812 Red Oak Blvd", "Basking Ridge", "AR", "04735-1675", 0d19871102, .F., 87, 88400, "Record 108" }, ;
      { "Heinz", "Zeal", "28942 University Avevnue", "Enola", "PA", "11125-0173", 0d19900930, .T., 62, 3900, "Record 109" }, ;
      { "Ginny", "MacKenzie", "21714 14th Avenue", "Universal City", "UT", "28342-8344", 0d19850614, .F., 48, 40800, "Record 110" }, ;
      { "Gilles", "Weinberg", "27419 Witternberg Avenue", "Monrovia", "VT", "71110-5295", 0d19890725, .T., 63, 14300, "Record 111" }, ;
      { "Jason", "Cain", "14930 Square North", "Norwood", "NY", "57322-0959", 0d19900514, .F., 38, 145800, "Record 112" }, ;
      { "Sid", "Schaffner", "28101 Hannahs Road", "Allentown", "CA", "18586-4679", 0d19870202, .T., 96, 3500, "Record 113" }, ;
      { "Orlando", "Flowerman", "8356 East 22nd Street", "Mobile", "KY", "27316-1586", 0d19910628, .F., 79, 73700, "Record 114" }, ;
      { "Arnold", "Jennings", "24804 Vandeven Court", "Pittsburgh", "ND", "57727-4194", 0d19840111, .T., 84, 23400, "Record 115" }, ;
      { "Irshad", "Lesiw", "14573 Marcus Avenue", "New Oxford", "CT", "08884-4638", 0d19830213, .F., 78, 83800, "Record 116" }, ;
      { "Francisco", "LaFarge", "1867 Mission Street", "Santa Ana", "NH", "76798-1884", 0d19900217, .T., 47, 79400, "Record 117" }, ;
      { "Ceci", "Gibbard", "19893 Telluride Road", "Jackson", "HI", "12349-0154", 0d19870528, .F., 39, 118400, "Record 118" }, ;
      { "Diane", "Beauchamp", "32040 Tierra Vista", "San Diego", "NJ", "80893-9076", 0d19920316, .T., 91, 36500, "Record 119" }, ;
      { "Bohdan", "Bryson", "10985 Guilford Street", "Solana Beach", "OK", "70147-5644", 0d19921202, .F., 57, 143000, "Record 120" }, ;
      { "Tina", "Broom", "6825 West Markham", "Farmingham", "IN", "57205-0928", 0d19830524, .T., 41, 11900, "Record 121" }, ;
      { "Solomon", "Fagerback", "13346 Jefferson Road", "Allston", "SC", "34975-6673", 0d19850323, .F., 89, 50100, "Record 122" }, ;
      { "Hans", "Ashworth", "24290 East Holland Avenue", "Alberta", "OR", "22132-5973", 0d19840620, .T., 81, 47000, "Record 123" }, ;
      { "Varouj", "Walden", "27941 Bauxile", "Pacific Grove", "MS", "62938-8047", 0d19850209, .F., 79, 7500, "Record 124" }, ;
      { "Gregory", "Bink", "18156 Summer Street", "Whittier", "MT", "96571-5714", 0d19850807, .T., 46, 116200, "Record 125" }, ;
      { "Miguel", "Popper", "30892 Colonial South Dr", "Omaha", "NY", "43669-0248", 0d19861106, .F., 99, 137500, "Record 126" }, ;
      { "Skip", "Sergent", "13647 East South Temple", "Denver", "MA", "72154-2732", 0d19900218, .T., 26, 25500, "Record 127" }, ;
      { "Jamie", "Caster", "20836 East Capitol Ave", "Lakewood", "WY", "48169-1286", 0d19920929, .F., 25, 119400, "Record 128" }, ;
      { "Sandy", "Carroll", "19875 West 20th Street", "Chandler", "LA", "29953-9558", 0d19840628, .T., 41, 81900, "Record 129" }, ;
      { "Dennis", "Tartaglia", "16695 Prospect St", "Conyers", "ID", "15886-2487", 0d19871127, .F., 94, 22700, "Record 130" }, ;
      { "Mara", "Mathews", "26739 Timber Ridge Drive", "North Andover", "WV", "55981-6195", 0d19871203, .T., 28, 77500, "Record 131" }, ;
      { "Terrie", "Evans", "23245 Wright Avenue", "Clackamas", "NH", "51688-8482", 0d19860207, .F., 96, 114600, "Record 132" }, ;
      { "Pete", "MacPherson", "866 Energy Drive", "Boise", "WI", "92746-6701", 0d19860118, .T., 99, 141100, "Record 133" }, ;
      { "Albert", "Crystal", "31054 Kipling Street", "Julian", "FL", "64270-2578", 0d19840807, .F., 80, 142700, "Record 134" }, ;
      { "Michael", "Payet", "20882 Sokolov Street", "Plano", "IN", "57718-1903", 0d19900731, .T., 49, 85000, "Record 135" }, ;
      { "Hai", "Godby", "2667 Acacia Street", "New Oxford", "WA", "71146-7779", 0d19850625, .F., 42, 125600, "Record 136" }, ;
      { "Reg", "Steele", "20547 Sherlock Street", "San Francisco", "OR", "32473-7313", 0d19880724, .T., 27, 61500, "Record 137" }, ;
      { "Nelson", "Privat", "9886 Wilshire Blvd", "Mammendord", "PA", "72856-2590", 0d19910821, .F., 44, 41100, "Record 138" }, ;
      { "Jimmy", "Coleman", "952 Whipple Ave NW", "Mount Pleasant", "UT", "73594-5609", 0d19860722, .T., 42, 59600, "Record 139" }, ;
      { "Terrie", "Jansen", "15278 Carolina", "Sharon", "NV", "01027-2707", 0d19890222, .F., 83, 59800, "Record 140" }, ;
      { "Dave", "Van Allen", "16479 Carter Drive", "Allston", "IL", "59185-1665", 0d19831029, .T., 90, 26600, "Record 141" }, ;
      { "Frederik", "Fornby", "19830 Dowell Hall", "Dublin", "WY", "66889-2426", 0d19860515, .F., 41, 66400, "Record 142" }, ;
      { "Cheryl", "Carvel", "9266 St Andrews Road", "Lisle", "LA", "25714-3349", 0d19920905, .T., 80, 50600, "Record 143" }, ;
      { "John", "Cusatis", "4328 East Holland Avenue", "Troy", "SD", "62308-1709", 0d19850409, .F., 73, 82500, "Record 144" }, ;
      { "William", "Caton", "9305 South Broadway", "Temecula", "NE", "56350-2234", 0d19880809, .T., 25, 68300, "Record 145" }, ;
      { "Holland", "Beale", "25933 Mt. Ariane Drive", "Austin", "DE", "97354-5007", 0d19830920, .F., 26, 144800, "Record 146" }, ;
      { "Kenny", "Frazier", "21267 Horizon Court", "Toronto", "MD", "93997-1746", 0d19910105, .T., 24, 70500, "Record 147" }, ;
      { "Issam", "Mockenhaupt", "28624 Carmenita Road", "St Paul", "NJ", "62299-5621", 0d19911126, .F., 74, 33900, "Record 148" }, ;
      { "Clyde", "Wechter", "16852 Maxanne Drive", "Modesto", "OK", "15148-9926", 0d19870420, .T., 76, 128100, "Record 149" }, ;
      { "Dean", "Fagerback", "23868 Roberta Road", "La Mesa", "IN", "09874-0499", 0d19911207, .F., 29, 35900, "Record 150" }, ;
      { "Dr. Timothy", "Walker", "9136 Genesee Street", "Bolingbrook", "WA", "98470-9043", 0d19841125, .T., 52, 141700, "Record 151" }, ;
      { "Hans", "Bink", "3587 South University", "Jamesville", "SC", "67537-5387", 0d19851105, .F., 77, 97400, "Record 152" }, ;
      { "Geoffrey", "Donnay", "5112 Earlwood Rd", "Buffalo", "CO", "88660-0526", 0d19891123, .T., 94, 19700, "Record 153" }, ;
      { "Marc", "Hollinger", "12416 E. Hawley Street", "Los Alamitos", "OR", "08101-4151", 0d19880430, .F., 77, 137300, "Record 154" }, ;
      { "Joseph", "Ferrari", "9507 Ohms Lane", "Garland", "KS", "20224-0240", 0d19851001, .T., 23, 14700, "Record 155" }, ;
      { "Abe", "Foster", "3454 Suzanne-Bouvier", "Reston", "GA", "27136-0503", 0d19911125, .F., 91, 19500, "Record 156" }, ;
      { "Allen", "Davenport", "7153 West 11th Ave", "Pocatelo", "UT", "35380-1869", 0d19851130, .T., 86, 136800, "Record 157" }, ;
      { "Ginny", "Zeal", "2809 South Hulen", "Delta", "NY", "81109-1484", 0d19890623, .F., 54, 135800, "Record 158" }, ;
      { "Holger", "Hubbard", "31436 N Brand Blvd", "Alpine", "CA", "37126-8587", 0d19830407, .T., 43, 44700, "Record 159" }, ;
      { "Barry", "Waddle", "10533 Martina Street", "Bethesda", "KY", "44848-6009", 0d19881217, .F., 39, 43900, "Record 160" }, ;
      { "Craig", "Kelly", "16752 N Broadway", "Fitzroy", "LA", "06517-4034", 0d19840401, .T., 37, 77000, "Record 161" }, ;
      { "Ted", "Bailey", "32174 Bauxile", "Montgomery", "ID", "60805-8777", 0d19880912, .F., 48, 8100, "Record 162" }, ;
      { "Asif", "Kelley", "1228 Hedline Place", "Columbus", "CT", "38827-0014", 0d19890415, .T., 54, 68300, "Record 163" }, ;
      { "Lynn", "Dodd", "28991 14th Avenue", "Sidney", "WV", "71065-6958", 0d19880115, .F., 53, 4500, "Record 164" }, ;
      { "Gerard", "Mortimer", "17192 Shasta Dr", "Lisle", "NH", "03907-1942", 0d19861018, .T., 92, 19200, "Record 165" }, ;
      { "Issam", "Norris", "17223 Rainbow Train", "Indianapolis", "HI", "07822-3685", 0d19911005, .F., 90, 91400, "Record 166" }, ;
      { "Raul", "Rosenbaum", "6280 Prospect St", "Paramus", "MI", "20926-3901", 0d19921126, .T., 54, 25800, "Record 167" }, ;
      { "Jackie", "Young", "6136 Martina Street", "Falls Church", "OK", "48637-1918", 0d19860622, .F., 46, 97800, "Record 168" }, ;
      { "Prima", "Azam", "14161 Park Avenue", "Montgomery", "IN", "41014-0961", 0d19900713, .T., 32, 143500, "Record 169" }, ;
      { "Oskar", "Cabel", "5265 North Mozart", "Deerfield Beach", "MO", "37576-7766", 0d19891122, .F., 66, 53100, "Record 170" }, ;
      { "Fulvio", "Vedal", "18732 Research Drive", "Bromma", "AR", "94069-1833", 0d19900803, .T., 98, 107400, "Record 171" }, ;
      { "Mark", "Raffie", "19934 Woodruff Road", "Greensborough", "GA", "68374-8393", 0d19871208, .F., 81, 114800, "Record 172" }, ;
      { "Homer", "Walsh", "7209 Beauregard Street", "Lisle", "MT", "28864-9388", 0d19870306, .T., 84, 127500, "Record 173" }, ;
      { "Michael", "Floriar", "3325 W Third St", "Montecito", "TX", "20548-3122", 0d19830329, .F., 33, 23800, "Record 174" }, ;
      { "Ben", "Benson", "16421 Regent Dr", "Springfield", "IL", "36595-8264", 0d19890327, .T., 65, 141100, "Record 175" }, ;
      { "Kjell", "Bink", "11876 S Dadeland Blvd", "Aurora", "CA", "19153-4532", 0d19880831, .F., 46, 120400, "Record 176" }, ;
      { "Rainer", "Van Allen", "30830 Burroughs Street", "Santa Ana", "IA", "76816-8879", 0d19851210, .T., 69, 32100, "Record 177" }, ;
      { "Elizabeth", "Cromie", "7969 Wirt Road #400", "San Diego", "VA", "31240-6985", 0d19920610, .F., 93, 73400, "Record 178" }, ;
      { "Holger", "Spray", "9638 Dowell Hall", "Tempe", "ME", "82405-4084", 0d19921003, .T., 75, 95300, "Record 179" }, ;
      { "Earl", "Russell", "15458 McDonald's Plaza", "Choctaw", "OH", "86599-7194", 0d19870302, .F., 26, 147200, "Record 180" }, ;
      { "Phyllis", "Lewis", "3045 Pine Tree Lane", "Aurora", "HI", "11584-4030", 0d19911013, .T., 91, 120500, "Record 181" }, ;
      { "Albert", "MacPherson", "6454 Woodruff Road", "Chesterfield", "NJ", "57781-9312", 0d19860221, .F., 69, 121800, "Record 182" }, ;
      { "Nicholas", "Abelson", "6436 Monterey Road", "Pacific Grove", "FL", "62659-5475", 0d19890313, .T., 58, 68600, "Record 183" }, ;
      { "Miguel", "Fox", "28306 14th Avenue", "Sylmar", "IN", "82027-1843", 0d19831221, .F., 57, 90600, "Record 184" }, ;
      { "Vincent", "Hoffman", "4055 Beethoven St", "Kaneohe", "MN", "46648-9916", 0d19840930, .T., 90, 140000, "Record 185" }, ;
      { "Donald", "Rogers", "14744 E Lincoln", "MacLeod", "WA", "24580-1710", 0d19901029, .F., 60, 95700, "Record 186" }, ;
      { "Benoit", "Schauer", "14809 Creek Crossing SE", "Solingen", "OR", "46981-0733", 0d19890313, .T., 42, 41300, "Record 187" }, ;
      { "Norman", "Tullis", "19477 Hansen Way", "Lakewood", "PA", "45397-6049", 0d19910918, .F., 59, 128600, "Record 188" }, ;
      { "Charles", "Jansen", "1934 N Bernard", "Culver City", "VT", "25831-2045", 0d19871125, .T., 78, 99500, "Record 189" }, ;
      { "Grattan", "Hong", "21321 Woodcreek Drive", "Farmingham", "NV", "50293-0452", 0d19850607, .F., 90, 81600, "Record 190" }, ;
      { "Hugh", "Lupton", "16197 Research Drive", "Chesterfield", "IL", "63964-7798", 0d19870212, .T., 23, 142000, "Record 191" }, ;
      { "Dianne", "Karant", "6380 Reyger St", "Oak Brook", "CA", "22474-3108", 0d19840203, .F., 36, 52800, "Record 192" }, ;
      { "Arnold", "Butler", "8413 West Main Street", "Covina", "LA", "20701-0992", 0d19850503, .T., 90, 23900, "Record 193" }, ;
      { "Mario", "Rowe", "623 W Third St", "Murdoch", "ID", "84835-8538", 0d19850205, .F., 90, 44300, "Record 194" }, ;
      { "Jose", "Clark", "8617 Manor Circle", "Renton", "NE", "41581-7101", 0d19840914, .T., 32, 88000, "Record 195" }, ;
      { "Nicholas", "Howarth", "4736 North Redgum", "Kaneohe", "ME", "31402-9279", 0d19911007, .F., 73, 33600, "Record 196" }, ;
      { "Jeffrey", "Steinberg", "9365 Greenwood Road", "Derry", "OH", "42778-9464", 0d19870216, .T., 57, 84600, "Record 197" }, ;
      { "Miguel", "Owensby", "28881 Energy Drive", "Allston", "MD", "10558-1968", 0d19901107, .F., 51, 59600, "Record 198" }, ;
      { "Dominic", "Acker", "21130 Philadelphia Street", "La Porte", "NJ", "81055-0061", 0d19900322, .T., 27, 63200, "Record 199" }, ;
      { "Christian", "Hunt", "15387 Royal Crest Ave", "Frederick", "OK", "59374-5119", 0d19851228, .F., 67, 77600, "Record 200" }, ;
      { "Albert", "Biresch", "30480 Roselle Street", "Van Nuys", "NM", "65530-6224", 0d19901216, .T., 63, 90100, "Record 201" }, ;
      { "Irene", "Nardamast", "10846 Shepherd Cres, S.E.", "Bristol", "IN", "58159-4439", 0d19890107, .F., 29, 115100, "Record 202" }, ;
      { "Asif", "Hussain", "1332 32nd Avenue South", "Reston", "TN", "55693-8426", 0d19841217, .T., 33, 135700, "Record 203" }, ;
      { "Manny", "Bruce", "15212 South Olive Street", "White Plaine", "CO", "72379-6932", 0d19880714, .F., 56, 78500, "Record 204" }, ;
      { "Clesson", "Devic", "10410 Galleria Blvd", "Fort Worth", "OR", "74683-1332", 0d19900112, .T., 22, 30800, "Record 205" }, ;
      { "Lillian", "Paulo", "422 Henry Avenue", "Alberta", "UT", "01828-9533", 0d19921121, .F., 97, 19200, "Record 206" }, ;
      { "Tin", "Agusta", "1844 Hewes Street", "Palo Alto", "VT", "94159-2667", 0d19860101, .T., 31, 108400, "Record 207" }, ;
      { "James", "Havens", "17670 Madison Avenue", "Dracut", "NV", "70354-4131", 0d19890202, .F., 64, 70200, "Record 208" }, ;
      { "Erna", "Irwin", "5699 S Canton Street", "Metairie", "NY", "69058-8265", 0d19891116, .T., 74, 146800, "Record 209" }, ;
      { "Gerard", "Tibbett", "1119 W. 11th Street", "Bonita", "CA", "43831-1442", 0d19830909, .F., 64, 43900, "Record 210" }, ;
      { "Charles", "Cheung", "10461 Windermere Ave.", "Fairfax", "KY", "58096-5040", 0d19880202, .T., 82, 67900, "Record 211" }, ;
      { "Luc", "Halverson", "6369 Powder Mill Road", "Milwaukie", "SD", "21520-1649", 0d19840623, .F., 38, 46800, "Record 212" }, ;
      { "Nick", "Jefferson", "6591 Park Ten Place", "San Diego", "VA", "89686-0735", 0d19890926, .T., 97, 111400, "Record 213" }, ;
      { "Eric", "Hilleary", "32737 14th Avenue", "Hauppauge", "NE", "57997-5333", 0d19850511, .F., 29, 26300, "Record 214" }, ;
      { "Ginny", "Walek", "13764 Rainbow Train", "Riverside", "DE", "53299-2321", 0d19870720, .T., 57, 38000, "Record 215" }, ;
      { "Darren", "Snow", "3800 Forest Drive", "Wausau", "RI", "45523-3810", 0d19861103, .F., 48, 134800, "Record 216" }, ;
      { "Nasar", "Cronin", "19078 Cantillion Blvd", "East Troy", "MI", "04555-9616", 0d19880811, .T., 23, 148100, "Record 217" }, ;
      { "Edward", "Spray", "5546 Allandale Road", "Lisle", "AL", "88678-1822", 0d19830310, .F., 95, 80800, "Record 218" }, ;
      { "Hubert", "Litvak", "32068 Des Vosges", "Charlotte", "NM", "16777-8149", 0d19851224, .T., 66, 68100, "Record 219" }, ;
      { "Reid", "Louis", "13098 Parkview Avenue", "Kempton Park", "WA", "51967-2762", 0d19891010, .F., 90, 32900, "Record 220" }, ;
      { "Simpson", "Blackman", "19972 Mahogony Ct", "Lakewood", "SC", "96166-7962", 0d19911002, .T., 93, 99900, "Record 221" }, ;
      { "George", "Stansberry", "24540 Wood Circle East", "Columbus", "CO", "47638-2757", 0d19841021, .F., 99, 46600, "Record 222" }, ;
      { "Joseph", "Stanton", "27386 S Dadeland Blvd", "Baldwin", "GA", "21907-3952", 0d19830919, .T., 34, 59500, "Record 223" }, ;
      { "Jackie", "Woiska", "29434 North 7th Avenue", "Lombard", "MS", "92188-8248", 0d19890201, .F., 21, 25900, "Record 224" }, ;
      { "Mohinder", "McKenzie", "14378 Paperbirch Drive", "Lamar", "MT", "25120-2272", 0d19830621, .T., 94, 142000, "Record 225" }, ;
      { "Tom", "Booth", "9658 S. El Molilno Ave", "Devon Park", "AZ", "76555-1110", 0d19911220, .F., 62, 38400, "Record 226" }, ;
      { "Arnold", "Knox", "12106 National City Center", "Gaithersburg", "IL", "83296-6708", 0d19851001, .T., 54, 72200, "Record 227" }, ;
      { "Don", "Russel", "22735 Waterford Drive", "Denver", "MA", "56350-4846", 0d19900605, .F., 30, 44100, "Record 228" }, ;
      { "Vosgo", "Garcia", "31723 Acourt Road", "Plymouth", "CA", "16858-6774", 0d19850223, .T., 63, 104200, "Record 229" }, ;
      { "Phyllis", "Lasso", "8238 N Los Altos", "Philadelphia", "LA", "53227-6043", 0d19900916, .F., 97, 90100, "Record 230" }, ;
      { "Sean", "Squires", "30681 Cherokee Blvd", "East Greenwich", "ID", "86581-6981", 0d19910711, .T., 23, 83400, "Record 231" }, ;
      { "Ken", "Kaster", "22804 Bellmore", "Enola", "NE", "19180-7146", 0d19880802, .F., 81, 146400, "Record 232" }, ;
      { "Jamie", "Buck", "27584 Howard Drive", "Langhorne", "DE", "58213-1814", 0d19880822, .T., 28, 92000, "Record 233" }, ;
      { "Thomas", "Breman", "20038 Energy Drive", "Los Angeles", "NH", "07516-1590", 0d19840117, .F., 80, 82200, "Record 234" }, ;
      { "Earl", "Waggener", "14352 North Francisco", "Murdoch", "WI", "56746-1510", 0d19900912, .T., 87, 131700, "Record 235" }, ;
      { "Diane", "Feldman", "11928 Sherlock Street", "Winnipeg", "OK", "05743-1820", 0d19900106, .F., 90, 18100, "Record 236" }, ;
      { "Drew", "Hunt", "18980 Suzanne-Bouvier", "Mobile", "NM", "88462-1865", 0d19890211, .T., 93, 5200, "Record 237" }, ;
      { "Lillian", "Martin", "22962 Grosvenor Hall", "Lombard", "IN", "96130-9916", 0d19830625, .F., 22, 112900, "Record 238" }, ;
      { "Don", "Morgenstern", "416 Malven Ave", "Hartselle", "MN", "46486-5565", 0d19861227, .T., 64, 133600, "Record 239" }, ;
      { "Harold", "Wishengrad", "18191 Whittington Drive", "Adelaide", "NC", "10954-4363", 0d19840326, .F., 80, 134700, "Record 240" }, ;
      { "Raul", "Bruce", "27160 Yonge Street", "Montville", "AR", "69580-4882", 0d19920312, .T., 51, 101200, "Record 241" }, ;
      { "Todd", "Elbaytam", "2887 Skokie Blvd", "Palm Bay", "KS", "02503-5909", 0d19890712, .F., 85, 9600, "Record 242" }, ;
      { "Lance", "Godby", "27896 Greenway Dr", "Mobile", "MT", "03646-3743", 0d19890719, .T., 34, 119700, "Record 243" }, ;
      { "Tom", "Logan", "16271 Devonshira Drive", "Kent", "NY", "49186-9351", 0d19861024, .F., 74, 71100, "Record 244" }, ;
      { "Muriel", "Sweatman", "24481 Beauregard Street", "McAllen", "AZ", "45937-5735", 0d19831125, .T., 77, 63600, "Record 245" }, ;
      { "Nathalie", "McKenzie", "20351 Parsons Blvd", "Lancaster", "KY", "85330-5371", 0d19921129, .F., 50, 34800, "Record 246" }, ;
      { "Suzanne", "Galvez", "4560 Creek Crossing SE", "Cleveland", "SD", "02782-9913", 0d19900828, .T., 80, 28700, "Record 247" }, ;
      { "Frank", "Fonseca", "28948 Carter Drive", "Houston", "ID", "42454-6471", 0d19910412, .F., 53, 2600, "Record 248" }, ;
      { "Frederik", "Snow", "16453 Winmeadow Place", "Greenville", "NE", "83197-8211", 0d19911010, .T., 32, 3800, "Record 249" }, ;
      { "Majola", "Kurtz", "19191 South University", "Conyers", "DE", "57187-8888", 0d19880605, .F., 65, 143900, "Record 250" }, ;
      { "Mike", "Werry", "4002 Riverdale Ave", "Williamstown", "HI", "11782-2946", 0d19850521, .T., 55, 3400, "Record 251" }, ;
      { "Ian", "Norris", "3326 North Francisco", "Hales Corners", "NJ", "23437-0645", 0d19830926, .F., 85, 65100, "Record 252" }, ;
      { "Fulvio", "Botti", "29586 N.E. 15th Court", "Ipswich", "FL", "11926-6643", 0d19900617, .T., 38, 48900, "Record 253" }, ;
      { "Les", "Gibbard", "32496 Kallang Avenue", "Greenwich", "IN", "22114-3764", 0d19890725, .F., 80, 113900, "Record 254" }, ;
      { "Bob", "Fox", "9772 Trotters View Way", "Lancaster", "WA", "17488-1480", 0d19850722, .T., 77, 2700, "Record 255" }, ;
      { "Becky", "Seyffert", "18830 5th Avenue", "Aurora", "NC", "38026-7941", 0d19840303, .F., 64, 85400, "Record 256" }, ;
      { "Ken", "Dodson", "5015 East Capitol Ave", "Chicago", "KS", "45280-6101", 0d19910807, .T., 44, 16300, "Record 257" }, ;
      { "Randy", "Neidorff", "32207 El Chaon Blvd", "Carlsbad", "GA", "77086-7760", 0d19910504, .F., 83, 59200, "Record 258" }, ;
      { "Ulrich", "Lamba", "24048 Montvale Ave", "Decatur", "VT", "15211-8928", 0d19850715, .T., 52, 48500, "Record 259" }, ;
      { "Jerry", "Benko", "7788 S. 608 Lilac Lane", "East Greenwich", "NY", "06832-4987", 0d19900321, .F., 51, 143800, "Record 260" }, ;
      { "Craig", "Lloyd", "23913 Ohms Lane", "Lyndhurst", "WY", "81082-7759", 0d19880730, .T., 75, 86700, "Record 261" }, ;
      { "Dr. Bob", "Spotorno", "12189 Sherlock Street", "Dublin", "SD", "84358-9228", 0d19871022, .F., 58, 64200, "Record 262" }, ;
      { "Ted", "Harrell", "16563 Hampton Lane", "Bonita", "CT", "43615-1506", 0d19911210, .T., 49, 88200, "Record 263" }, ;
      { "Hoechst", "Werry", "4387 Highbury Avenue", "Grand Rapids", "WV", "81136-8606", 0d19850708, .F., 23, 144000, "Record 264" }, ;
      { "Lam", "Roth", "12703 Beauregard Street", "Burbank", "DE", "18721-1737", 0d19880508, .T., 39, 132000, "Record 265" }, ;
      { "Lenore", "Esposito", "9863 Roberta Road", "Martinez", "NH", "26326-6270", 0d19920711, .F., 24, 17600, "Record 266" }, ;
      { "Uzi", "Robertson", "8705 Orcas Ave", "Del Ray Beach", "WI", "48034-2501", 0d19880718, .T., 54, 117500, "Record 267" }, ;
      { "Barry", "Klein", "4515 Golden Springs Rd", "Santa Monica", "AK", "18802-1253", 0d19901002, .F., 95, 12500, "Record 268" }, ;
      { "Martin", "Bickley", "9430 Willow Parkway", "Hales Corners", "NM", "14545-0661", 0d19901231, .T., 57, 53600, "Record 269" }, ;
      { "Max", "Rocquet", "863 North Meeker Lane", "Long Beach", "WA", "26146-8914", 0d19830601, .F., 55, 134200, "Record 270" }, ;
      { "Bob", "Owensby", "30631 East Holland Avenue", "Gardina", "AR", "83998-6812", 0d19880704, .T., 65, 16000, "Record 271" }, ;
      { "Frederik", "Cohen", "26302 Dean Drive", "Charleston", "OR", "88534-9932", 0d19921002, .F., 62, 78400, "Record 272" }, ;
      { "Ken", "Riendeau", "8238 Lincoln Avenue", "Lyndhurst", "UT", "72523-5149", 0d19881118, .T., 67, 149400, "Record 273" }, ;
      { "Hugh", "Phillips", "27161 N.E. 15th Court", "Oak Brook", "TX", "22123-4307", 0d19920615, .F., 34, 131600, "Record 274" }, ;
      { "Bohdan", "Grigsby", "16952 Wright St.", "South Elgin", "IL", "54208-4023", 0d19880520, .T., 93, 24500, "Record 275" }, ;
      { "Clifford", "Johnson", "10923 E. Marina Drive", "Alphoretta", "MA", "24580-9823", 0d19870506, .F., 76, 143300, "Record 276" }, ;
      { "Linda", "Brown", "4555 Summer Street", "Mesa", "WY", "03916-1116", 0d19870205, .T., 32, 124800, "Record 277" }, ;
      { "Adrian", "Tamburrino", "1615 Pipit Street NE", "Irving", "KY", "41707-8165", 0d19830707, .F., 82, 97700, "Record 278" }, ;
      { "Francisco", "Jennings", "1417 Prince Charles Drive", "Laguna Hills", "ND", "80758-7747", 0d19910116, .T., 93, 111600, "Record 279" }, ;
      { "Becky", "Barkalow", "22407 Fox Run Road", "Washington", "ME", "04744-1399", 0d19900805, .F., 27, 23600, "Record 280" }, ;
      { "Rodney", "Caspers", "11605 Center Blvd", "El Paso", "NH", "66151-6219", 0d19920503, .T., 31, 82500, "Record 281" }, ;
      { "Jeff", "Lesiw", "8126 Henry Avenue", "Baldwin", "WI", "44533-3697", 0d19861230, .F., 68, 94000, "Record 282" }, ;
      { "Tin", "Tranum", "20789 Commercial Avenue", "Simi Valley", "AK", "51463-0276", 0d19910606, .T., 51, 82400, "Record 283" }, ;
      { "Lynn", "Klein", "25551 North West 66th St", "Montecito", "OK", "88975-7773", 0d19881102, .F., 45, 73200, "Record 284" }, ;
      { "Gordon", "Dryden", "15480 S. San Fernando Rd", "Kalamazoo", "MN", "96283-8724", 0d19891006, .T., 47, 145600, "Record 285" }, ;
      { "Lou", "Weinberg", "16071 Park Avenue South", "Midland", "WA", "70129-1851", 0d19870314, .F., 22, 55100, "Record 286" }, ;
      { "Clyde", "Fairweather", "22810 Knollway Drive", "Denver", "AR", "25048-1199", 0d19830223, .T., 27, 77100, "Record 287" }, ;
      { "Steve", "Johansson", "28391 N Los Altos", "Tempe", "GA", "76204-5081", 0d19890823, .F., 47, 88000, "Record 288" }, ;
      { "Greg", "Waldrop", "32613 Ash Street", "Columbus", "MS", "38872-8778", 0d19880628, .T., 69, 16300, "Record 289" }, ;
      { "Trevor", "Holzer", "24814 Swain Ave", "Lund", "VT", "90271-5061", 0d19911209, .F., 70, 37100, "Record 290" }, ;
      { "Brian", "Polakoff", "7641 Antlers Dr", "Alta", "AZ", "56341-0243", 0d19880424, .T., 55, 56200, "Record 291" }, ;
      { "Erna", "Ivon", "32151 Exchange Place", "Stoneham", "MA", "96319-0189", 0d19910328, .F., 89, 25000, "Record 292" }, ;
      { "Cary", "Jorgensen", "7503 Walnut Street", "Granada Hills", "WY", "53119-7152", 0d19910411, .T., 84, 75800, "Record 293" }, ;
      { "Homer", "Laszko", "15710 National City Center", "Manassas", "IA", "84691-2914", 0d19861002, .F., 67, 24500, "Record 294" }, ;
      { "Abe", "Novak", "9562 Hampton Lane", "Garland", "NE", "10810-0233", 0d19840507, .T., 31, 114000, "Record 295" }, ;
      { "Sheng", "McSweeney", "18056 Monterey Road", "Hauppauge", "DE", "61057-6490", 0d19880710, .F., 42, 48300, "Record 296" }, ;
      { "Jamie", "Buck", "9455 Mary Baldwin Drive", "Modesto", "NH", "99226-4704", 0d19900623, .T., 85, 77700, "Record 297" }, ;
      { "Solomon", "Fleig", "6499 East 41st Street", "Dedham", "HI", "87508-0230", 0d19840825, .F., 68, 53300, "Record 298" }, ;
      { "Mara", "Leeson", "17222 Herbert Lane", "Stevensville", "NJ", "32914-4747", 0d19860404, .T., 77, 80200, "Record 299" }, ;
      { "Bruce", "Johnson", "3787 Cantillion Blvd", "Simi Valley", "AK", "24616-4272", 0d19870806, .F., 91, 48400, "Record 300" }, ;
      { "Suzanne", "Walker", "7424 Springs Street", "Woodville", "OK", "44740-2089", 0d19830221, .T., 22, 144500, "Record 301" }, ;
      { "Maarhot", "Kulek", "11503 Jefferson Road", "Brick", "IN", "62020-2011", 0d19830522, .F., 46, 97000, "Record 302" }, ;
      { "Ed", "Galvez", "29067 W. Shure Drive", "Houston", "WA", "44587-2647", 0d19900609, .T., 59, 40100, "Record 303" }, ;
      { "Irene", "McGaffin", "5715 Commercial Avenue", "Kaneohe", "CO", "03232-9155", 0d19830808, .F., 85, 2400, "Record 304" }, ;
      { "Jamie", "Short", "9412 Melrose Avenue", "Santa Fe Springs", "AR", "09325-9101", 0d19900315, .T., 36, 130000, "Record 305" }, ;
      { "Jon", "Kurtz", "5910 Stamford Landing", "Columbus", "KS", "80245-9712", 0d19880522, .F., 56, 109500, "Record 306" }, ;
      { "Armand", "Werry", "14556 Jackes Avenue", "Laguna Hills", "GA", "97453-0552", 0d19920221, .T., 27, 110200, "Record 307" }, ;
      { "Arnaldo", "Morgenroth", "5238 West Alameda", "North Canton", "VT", "24751-3431", 0d19900606, .F., 55, 25100, "Record 308" }, ;
      { "Garland", "Stricklen", "9341 South Corning Street", "Macedon", "IL", "44830-2573", 0d19850901, .T., 43, 61300, "Record 309" }, ;
      { "Lou", "Louis", "25234 E Yosemite Ave", "Fort Mill", "CA", "91774-6070", 0d19920202, .F., 31, 50000, "Record 310" }, ;
      { "Frank", "Dearry", "10542 Creek Crossing SE", "Cedarburg", "LA", "29800-3385", 0d19910213, .T., 49, 92200, "Record 311" }, ;
      { "John", "Ivy", "17520 Cantrell Road", "Velden", "IA", "16831-3776", 0d19831015, .F., 53, 77600, "Record 312" }, ;
      { "Tom", "Logan", "3573 East 22nd Street", "Martinez", "SD", "74224-2227", 0d19881219, .T., 96, 20900, "Record 313" }, ;
      { "Armand", "Wegner", "6719 Lack Cook Road", "Bentley", "ND", "92269-7878", 0d19911126, .F., 68, 75300, "Record 314" }, ;
      { "Marguretha", "Hilleary", "25148 N. River Rd.", "Akron", "NE", "12898-1680", 0d19860828, .T., 76, 114000, "Record 315" }, ;
      { "Mark", "Bailey", "479 W. North Avenue", "Kellerberrin", "DE", "20098-1381", 0d19870618, .F., 73, 90100, "Record 316" }, ;
      { "Matthew", "Woods", "11511 Merritt Drive", "Conyers", "NH", "14770-6769", 0d19900219, .T., 63, 47200, "Record 317" }, ;
      { "Bruno", "Wilson", "28064 Sherman Way", "Ipswich", "MD", "50815-7925", 0d19900511, .F., 22, 126600, "Record 318" }, ;
      { "Dickson", "Rinaldi", "11268 New Brooklyn Road", "Flower Mound", "WI", "61282-4769", 0d19830122, .T., 61, 146200, "Record 319" }, ;
      { "Achim", "Biresch", "30024 Manor Circle", "Chulavista", "OK", "75502-8948", 0d19881029, .F., 22, 29000, "Record 320" }, ;
      { "Heinz", "Harrell", "2003 Orcas Ave", "South Norwalk", "IN", "21223-5760", 0d19861221, .T., 87, 26900, "Record 321" }, ;
      { "Phillip", "Voss", "25944 Allendale Road", "Velden", "MO", "47044-4279", 0d19890903, .F., 37, 144600, "Record 322" }, ;
      { "Tina", "Jost", "8008 Davis Blvd", "Victor", "WA", "63028-3515", 0d19870630, .T., 90, 145900, "Record 323" }, ;
      { "Jerry", "DiFrancesco", "20624 Business Park Drive", "Bayside", "SC", "95923-3467", 0d19850203, .F., 49, 141700, "Record 324" }, ;
      { "Al", "Platovsky", "20446 Wakefield Street", "Modesto", "CO", "77725-1614", 0d19890604, .T., 53, 103300, "Record 325" }, ;
      { "Ed", "Vedal", "10162 Horizon Court", "Fort Worth", "AR", "92602-7643", 0d19860323, .F., 34, 25700, "Record 326" }, ;
      { "Sally", "Nehme", "24077 S Mitchell Dr", "Oak Park", "PA", "84115-4461", 0d19871001, .T., 71, 35400, "Record 327" }, ;
      { "Dennis", "Ackerman", "7044 Meadowbrook Drive", "Troy", "NV", "17101-4175", 0d19890204, .F., 57, 79300, "Record 328" }, ;
      { "Simpson", "White", "25071 East South Temple", "Langhorne", "NY", "04483-0267", 0d19880404, .T., 25, 108400, "Record 329" }, ;
      { "Anne", "Wright", "19738 Telluride Road", "Sedalia", "AZ", "31753-0522", 0d19830609, .F., 72, 79400, "Record 330" }, ;
      { "Tony", "Elledge", "23583 Indianapolis Ave", "Berkeley", "KY", "46954-2440", 0d19841017, .T., 28, 18600, "Record 331" }, ;
      { "Harry", "Scheffler", "4178 Commonwealth Drive", "St. Louis", "LA", "68428-0759", 0d19850106, .F., 96, 54600, "Record 332" }, ;
      { "Arthur", "Linden", "22243 Genesee Street", "Beltsville", "IA", "28486-6908", 0d19910124, .T., 48, 27600, "Record 333" }, ;
      { "Diane", "Randolph", "17569 S Canton Street", "Princeton", "SD", "36622-1079", 0d19871012, .F., 68, 26800, "Record 334" }, ;
      { "Andreas", "Dysert", "14219 Escalon Drive", "Waterloo", "NE", "18685-1237", 0d19840902, .T., 59, 52000, "Record 335" }, ;
      { "Jean", "Buttenhoff", "2062 Bambridge Street", "Stevensville", "CT", "97534-2277", 0d19850310, .F., 73, 91900, "Record 336" }, ;
      { "Armand", "Cowen", "9242 N. Hayden Road", "Pittsburgh", "WV", "74107-7614", 0d19910326, .T., 75, 20400, "Record 337" }, ;
      { "Bernie", "Brokaw", "18557 E 104th Street", "Preston", "RI", "91729-0502", 0d19900521, .F., 75, 58100, "Record 338" }, ;
      { "Terrie", "Vidmar", "8484 Windsong Place", "Taylors", "NJ", "68518-6073", 0d19850801, .T., 76, 127300, "Record 339" }, ;
      { "Richard", "Sherwin", "30510 Madeline", "Tucson", "AL", "94519-9694", 0d19921022, .F., 50, 20000, "Record 340" }, ;
      { "Bohdan", "Grigsby", "19448 Cantillion Blvd", "Conyers", "TN", "07552-9415", 0d19910514, .T., 95, 147500, "Record 341" }, ;
      { "Ralf", "Cole", "25222 Allandale Road", "Elmhurst", "SC", "52705-2281", 0d19910225, .F., 63, 142300, "Record 342" }, ;
      { "Lynn", "Ammann", "21759 Plaza Court", "Baldwin", "AR", "24661-6318", 0d19900308, .T., 71, 64500, "Record 343" }, ;
      { "Bernie", "Moon", "31246 W. North Avenue", "Rohnert Park", "MS", "15706-2402", 0d19901016, .F., 68, 48400, "Record 344" }, ;
      { "Loren", "Caravella", "7781 Newman Avenue", "Homewood", "UT", "85888-6750", 0d19910812, .T., 80, 90600, "Record 345" }, ;
      { "Brian", "Kaufmann", "187 Shasta Dr", "Tucson", "NV", "19900-9447", 0d19870526, .F., 40, 2600, "Record 346" }, ;
      { "Asif", "Blumenthal", "8951 Bellmore", "Garland", "IL", "15544-3997", 0d19910224, .T., 77, 140900, "Record 347" }, ;
      { "Jon", "Brokaw", "29037 112th Avenue N.E.", "Reston", "KY", "51049-5086", 0d19871212, .F., 73, 10000, "Record 348" }, ;
      { "Barrie", "Taylor", "2381 McKinley Ave", "Allston", "LA", "90550-1414", 0d19830307, .T., 24, 79800, "Record 349" }, ;
      { "Corkey", "Beckerman", "6587 Wertzville Road", "Palm Desert", "ND", "60589-4822", 0d19860521, .F., 31, 100100, "Record 350" }, ;
      { "Edward", "Wagner", "1834 Kipling Street", "Alpine", "WV", "90352-5780", 0d19910115, .T., 71, 146100, "Record 351" }, ;
      { "Hubert", "Hokanson", "12014 Hampton Lane", "Springfield", "OH", "62299-2439", 0d19881107, .F., 28, 88200, "Record 352" }, ;
      { "Keigh", "Tilles", "31009 Devonshira Drive", "Easton", "HI", "55126-1657", 0d19860727, .T., 68, 94300, "Record 353" }, ;
      { "Jon", "Moon", "24908 Center Avenue", "Tempe", "WI", "51256-4757", 0d19891206, .F., 88, 35200, "Record 354" }, ;
      { "Dorman", "Sprengers", "21114 Square North", "Amerspoort", "NJ", "49213-5225", 0d19900404, .T., 91, 57100, "Record 355" }, ;
      { "Ross", "Morgan", "12696 Howard Drive", "Perth Amboy", "AK", "14023-5307", 0d19841009, .F., 46, 13200, "Record 356" }, ;
      { "Laura", "Jost", "18772 North Maple", "Rockville", "FL", "72577-3106", 0d19900713, .T., 87, 39800, "Record 357" }, ;
      { "Ricardo", "Kaufmann", "3288 Boxwood Lane", "Chesterfield", "AL", "93898-4808", 0d19831103, .F., 31, 110300, "Record 358" }, ;
      { "Ted", "Louman", "25357 Chubb Ave", "Alpine", "TN", "27262-1876", 0d19891209, .T., 94, 15800, "Record 359" }, ;
      { "Rainer", "Yeager", "29270 Stonecutter Lane", "Silverdale", "WA", "54919-8652", 0d19830820, .F., 94, 65000, "Record 360" }, ;
      { "Dennis", "Hays", "17631 Hedline Place", "Chattanooga", "AR", "81586-1592", 0d19900925, .T., 56, 91600, "Record 361" }, ;
      { "Solomon", "Fleig", "10218 Summer Street", "Plettenberg", "PA", "31960-0583", 0d19870730, .F., 49, 147300, "Record 362" }, ;
      { "Bernie", "Seles", "27853 S Mitchell Dr", "Broomall", "UT", "03673-6153", 0d19911217, .T., 68, 116400, "Record 363" }, ;
      { "Aart", "Ashworth", "16504 Sixth Street", "St Paul", "VT", "58204-5297", 0d19841205, .F., 86, 98300, "Record 364" }, ;
      { "Jose", "Johansson", "14014 New Brooklyn Road", "Vienna", "NY", "62686-1177", 0d19871120, .T., 24, 98000, "Record 365" }, ;
      { "Guy", "Acker", "30901 Grosvenor Hall", "Amstelveen", "MA", "20089-7145", 0d19850520, .F., 85, 41500, "Record 366" }, ;
      { "Loren", "Brock", "20968 Knollwood Terrace", "Fallbrook", "KY", "54361-3277", 0d19920508, .T., 72, 36000, "Record 367" }, ;
      { "John", "Linden", "11824 Tobey Road", "Auburn", "IA", "71569-4842", 0d19850519, .F., 69, 25300, "Record 368" }, ;
      { "Roman", "Karant", "2672 Woodcreek Drive", "Summit", "ID", "08488-7952", 0d19911231, .T., 82, 109400, "Record 369" }, ;
      { "Tim", "Larre", "29838 River Drive", "Bethesda", "NE", "23950-2522", 0d19900704, .F., 34, 127100, "Record 370" }, ;
      { "Jay", "Marshall", "2897 Kipling Street", "Wilmington", "CT", "55090-0803", 0d19851026, .T., 71, 103700, "Record 371" }, ;
      { "Rita", "Bruno", "3349 Red Oak Blvd", "Burnaby", "NH", "31258-1751", 0d19830326, .F., 70, 109200, "Record 372" }, ;
      { "Douglas", "Fournier", "31867 Washingtonian Blvd", "McLean", "MD", "73270-4571", 0d19850110, .T., 64, 46200, "Record 373" }, ;
      { "Wayne", "Davenport", "17517 Turner Avenue", "Jersey City", "MI", "84061-1922", 0d19860528, .F., 86, 82900, "Record 374" }, ;
      { "Ricardo", "Cabon", "8509 Park Plaza", "Chimo Hills", "FL", "92620-6883", 0d19910815, .T., 94, 134100, "Record 375" }, ;
      { "Dr. Timothy", "Vedal", "31396 Prospect St", "Stevensville", "IN", "80740-9690", 0d19850420, .F., 31, 109900, "Record 376" }, ;
      { "Pamela", "Bucher", "23269 Prince Charles Drive", "Ottsville", "SC", "27073-1150", 0d19870808, .T., 25, 32100, "Record 377" }, ;
      { "Enoch", "Chism", "22896 Winmeadow Place", "Bonita", "KS", "79129-0035", 0d19850928, .F., 70, 140500, "Record 378" }, ;
      { "Harold", "De Pierola", "13223 St Andrews Road", "South Bend", "MT", "31402-1541", 0d19880423, .T., 92, 92700, "Record 379" }, ;
      { "Irshad", "Kulek", "31930 National City Center", "Charlotte", "VT", "58420-6121", 0d19921231, .F., 42, 119500, "Record 380" }, ;
      { "Bryan", "Rumaner", "21948 Kipling Street", "Austin", "IL", "89164-5222", 0d19891105, .T., 50, 27200, "Record 381" }, ;
      { "Gregory", "Bink", "4440 Lancaster Dr N.E.", "Irving", "CA", "48421-3448", 0d19890310, .F., 86, 35400, "Record 382" }, ;
      { "Jackie", "Tanguay", "4813 W. North Avenue", "Bayside", "LA", "28963-8662", 0d19890802, .T., 43, 107400, "Record 383" }, ;
      { "Cheryl", "Cooman", "11803 Via Leslie", "Calhoun", "SD", "10468-7604", 0d19920226, .F., 56, 64600, "Record 384" }, ;
      { "Lance", "Forsberg", "24464 Barfield Road", "Newton", "NE", "55891-1927", 0d19851029, .T., 87, 126700, "Record 385" }, ;
      { "Lynn", "Laszko", "27402 Center Avenue", "Council Bluffs", "DE", "38944-4993", 0d19890612, .F., 77, 54500, "Record 386" }, ;
      { "Byron", "Dowd", "26544 N River Rd", "Lisle", "RI", "71218-8238", 0d19890216, .T., 85, 117100, "Record 387" }, ;
      { "Gregory", "Brennan", "31470 W Broadway", "Alexandria", "MD", "21718-4267", 0d19890127, .F., 62, 46900, "Record 388" }, ;
      { "Adele", "Budzinski", "7667 National City Center", "Irving", "HI", "87526-1060", 0d19860902, .T., 83, 90400, "Record 389" }, ;
      { "Bohdan", "Blanchard", "19216 N. Kasper Ave", "Martinez", "WI", "32050-7724", 0d19910906, .F., 91, 56100, "Record 390" }, ;
      { "Lance", "Johansson", "27800 Joyce Street", "Snellville", "FL", "41275-2692", 0d19840501, .T., 62, 79900, "Record 391" }, ;
      { "Tom", "Piko", "27967 University Avenue", "Riverside", "IN", "32680-0530", 0d19860301, .F., 37, 121900, "Record 392" }, ;
      { "Marguretha", "Woods", "27136 Southast 127th Avenue", "La Mesa", "SC", "30196-8229", 0d19890318, .T., 81, 10100, "Record 393" }, ;
      { "Les", "Himes", "20501 Business Park", "Ann Arbor", "OR", "23752-5778", 0d19860717, .F., 96, 124000, "Record 394" }, ;
      { "Rob", "Harkin", "13710 Chimney Springs Dr", "Montecito", "MS", "38629-2851", 0d19870617, .T., 53, 49000, "Record 395" }, ;
      { "Achim", "Busey", "2427 Genesee Street", "Burbank", "VT", "39196-0541", 0d19830121, .F., 96, 4800, "Record 396" }, ;
      { "John", "Cusatis", "24785 Grand Avenue", "Manassas", "TX", "86599-0107", 0d19920216, .T., 54, 59000, "Record 397" }, ;
      { "Leighann", "Parnell", "14188 Efna Park Drive", "Kempton Park", "AZ", "35110-9310", 0d19840715, .F., 90, 62800, "Record 398" }, ;
      { "Dr. Timothy", "Botti", "2273 Kildare Crescent", "St. Louis", "IL", "42040-6934", 0d19900404, .T., 95, 138200, "Record 399" }, ;
      { "Melinda", "Mathews", "15096 Boxwood Lane", "Memphis", "WY", "67834-9868", 0d19840928, .F., 72, 118600, "Record 400" }, ;
      { "Lenore", "Esposito", "1031 River Drive", "Vienna", "SD", "91351-6534", 0d19860919, .T., 96, 27600, "Record 401" }, ;
      { "Christer", "Escalante", "20873 Sherlock Street", "Bellmont", "VA", "91774-9692", 0d19841214, .F., 93, 4300, "Record 402" }, ;
      { "Dexter", "Shephard", "18337 Highbury Avenue", "Norman", "ME", "92296-3027", 0d19830814, .T., 33, 113000, "Record 403" }, ;
      { "Graham", "Creagh", "20586 Turner Avenue", "Ann Arbor", "OH", "64549-6350", 0d19891119, .F., 94, 123200, "Record 404" }, ;
      { "Mohinder", "Privat", "27206 Tierra Vista", "Woodville", "HI", "70678-0121", 0d19840422, .T., 77, 135500, "Record 405" }, ;
      { "Mike", "Walden", "1670 Oak St", "Temecula", "WI", "97921-8189", 0d19921007, .F., 64, 122200, "Record 406" }, ;
      { "Timothy", "Broyles", "26151 Canandaigua Road", "Kennesaw", "AK", "13537-9028", 0d19921003, .T., 23, 95100, "Record 407" }, ;
      { "Bernie", "Moon", "2159 Wright Avenue", "Secaucus", "FL", "48907-7210", 0d19860906, .F., 36, 44100, "Record 408" }, ;
      { "Ryan", "Serenska", "32694 Parkside Avenue", "Macedon", "MN", "76231-0398", 0d19870822, .T., 22, 93700, "Record 409" }, ;
      { "Andy", "Olson", "27296 Hoene Place", "Mission Viejo", "NC", "69157-0427", 0d19840421, .F., 55, 131200, "Record 410" }, ;
      { "Orlando", "Dowd", "29835 Huntington Blvd", "Palm City", "OR", "85024-1151", 0d19890601, .T., 67, 83800, "Record 411" }, ;
      { "Ann", "Dobra", "11781 Beauregard Street", "South Bend", "UT", "71380-2562", 0d19901118, .F., 71, 116500, "Record 412" }, ;
      { "Marc", "Crystal", "22147 East Capitol Ave", "Falls Church", "NV", "38863-1603", 0d19840803, .T., 97, 112000, "Record 413" }, ;
      { "Christopher", "Moflett", "25272 Golden Springs Rd", "Playa del Rey", "AZ", "10711-6638", 0d19880426, .F., 60, 74000, "Record 414" }, ;
      { "Laura", "Broom", "14124 El Chaon Blvd", "Atlanta", "KY", "51454-1463", 0d19920814, .T., 43, 141600, "Record 415" }, ;
      { "Sharon", "Scholz", "17678 First Avenue", "Chimo Hills", "LA", "31141-4570", 0d19901129, .F., 67, 11300, "Record 416" }, ;
      { "Theresa", "Greene", "1886 So Sherman", "Derry", "IA", "86320-5637", 0d19850922, .T., 36, 43600, "Record 417" }, ;
      { "Cameron", "Homnick", "22739 Wakefield Street", "Baltimore", "NE", "08839-4326", 0d19850219, .F., 65, 133000, "Record 418" }, ;
      { "Benoit", "Holden", "20267 Via Leslie", "West Patterson", "CT", "42994-9035", 0d19920128, .T., 86, 65200, "Record 419" }, ;
      { "Hans", "Forbes", "18130 West 82nd St", "Paramus", "NH", "32482-5290", 0d19890601, .F., 98, 85500, "Record 420" }, ;
      { "Pat", "Worthen", "8699 Cherokee Blvd", "Salt Lake City", "MD", "35146-0208", 0d19901111, .T., 97, 125500, "Record 421" }, ;
      { "Bryan", "Barnes", "154 Cassandra Ln", "Bloomington", "MI", "87787-5011", 0d19830502, .F., 65, 9300, "Record 422" }, ;
      { "Alan", "Hsu", "29424 Regent Dr", "Carlsbad", "NJ", "74638-0136", 0d19881118, .T., 42, 114100, "Record 423" }, ;
      { "Fred", "Dowd", "28285 Telstar Drive", "Vienna", "OK", "61111-6797", 0d19860113, .F., 87, 77900, "Record 424" }, ;
      { "Hubert", "Beale", "11664 S Broadway", "Conshohoken", "AL", "07579-6119", 0d19870127, .T., 44, 54800, "Record 425" }, ;
      { "Trevor", "Crawford", "11226 Ash Street", "Hermosa Beach", "MO", "80020-9546", 0d19860406, .F., 67, 14700, "Record 426" }, ;
      { "Allen", "Roberts", "13988 South Marine Drive", "Rachno Cucamonga", "SC", "55180-0337", 0d19910523, .T., 64, 127000, "Record 427" }, ;
      { "Jamie", "Buck", "5533 Abbots Place", "Derry", "OR", "24463-2759", 0d19890902, .F., 52, 141200, "Record 428" }, ;
      { "Ian", "Mockenhaupt", "14684 North Meeker Lane", "East Greenwich", "GA", "02206-0843", 0d19880504, .T., 60, 75200, "Record 429" }, ;
      { "Randy", "Hicks", "12037 Maria Lane", "Diamond Bar", "MS", "56926-5832", 0d19830711, .F., 46, 35300, "Record 430" }, ;
      { "Adrian", "Sherwin", "25782 Sokolov Street", "Evanston", "NV", "24697-5677", 0d19920413, .T., 57, 19600, "Record 431" }, ;
      { "Howard", "Duke", "8358 Horizon Court", "Taylors", "NY", "81379-2901", 0d19860708, .F., 99, 44300, "Record 432" }, ;
      { "Becky", "Coleman", "32030 Valley Drive", "MacLeod", "MA", "74449-7852", 0d19870605, .T., 59, 110400, "Record 433" }, ;
      { "Ken", "Thompson", "21935 Mary Baldwin Drive", "Scottsdale", "KY", "72757-6777", 0d19861107, .F., 36, 139600, "Record 434" }, ;
      { "Cameron", "Plossl", "26198 Roberta Road", "Anaheim", "ID", "62731-7419", 0d19841123, .T., 91, 136800, "Record 435" }, ;
      { "Geoffrey", "Forsberg", "31598 Davis Blvd", "Barrington", "CT", "91756-9400", 0d19830525, .F., 96, 19800, "Record 436" }, ;
      { "Raul", "Bruce", "2457 South University", "Olympia", "WV", "80983-1761", 0d19900129, .T., 73, 124500, "Record 437" }, ;
      { "Francisco", "LaFarge", "26386 Pheasant", "Grand Rapids", "ME", "53857-3438", 0d19910206, .F., 89, 95500, "Record 438" }, ;
      { "Sid", "Yeager", "15357 Chimney Springs Drive", "Temecula", "DE", "19783-5860", 0d19871230, .T., 45, 53000, "Record 439" }, ;
      { "Nigel", "Nowakowski", "9649 University Avenue", "Santa Ana", "NH", "04672-0148", 0d19860728, .F., 88, 3900, "Record 440" }, ;
      { "Randall", "Breman", "2327 Decker Lane", "Encinitas", "RI", "54208-8624", 0d19920415, .T., 67, 85400, "Record 441" }, ;
      { "Don", "Feinauer", "14987 Pico Blvd", "Hales Corners", "HI", "56926-5026", 0d19901215, .F., 99, 93300, "Record 442" }, ;
      { "Rupert", "Oliver", "1507 Portage Trail", "Nedlands", "NJ", "48403-7138", 0d19870507, .T., 59, 147400, "Record 443" }, ;
      { "Maurice", "Yellick", "20365 Trice Drive", "Metairie", "AK", "66529-4413", 0d19911121, .F., 93, 80300, "Record 444" }, ;
      { "Gregg", "Dodd", "4067 Harbour View", "Universal City", "FL", "05194-4804", 0d19910228, .T., 29, 118200, "Record 445" }, ;
      { "Ali", "Burns", "23578 Woodcreek Drive", "Bethesda", "MN", "01531-0844", 0d19841126, .F., 50, 102400, "Record 446" }, ;
      { "Dmitry", "Burks", "7303 W. 11th Street", "Miami", "NC", "64342-8610", 0d19841230, .T., 73, 105900, "Record 447" }, ;
      { "Gonzalo", "Rotter", "1543 Plymouth Ave", "Memphis", "GA", "03259-2616", 0d19840614, .F., 34, 27200, "Record 448" }, ;
      { "Vosgo", "Dearry", "31949 N Bristol Court", "Cuyahoga Falls", "UT", "61732-8245", 0d19861214, .T., 86, 6700, "Record 449" }, ;
      { "Sharon", "Blackstein", "16614 Yentchuk Avenue", "Spring Valley", "NV", "02080-1256", 0d19920809, .F., 46, 39300, "Record 450" }, ;
      { "Cris", "Waggener", "14634 North Redgum", "San Marcos", "NY", "20872-7709", 0d19830716, .T., 41, 125500, "Record 451" }, ;
      { "Ben", "Ashworth", "28549 Galleria Blvd", "Highett", "CA", "12331-2478", 0d19900109, .F., 76, 109500, "Record 452" }, ;
      { "Corey", "Feldman", "27050 N Bernard", "Paso Robles", "SD", "75475-1668", 0d19890113, .T., 62, 50400, "Record 453" }, ;
      { "Varouj", "Limones", "2998 N.E. 15th Court", "Greenville", "ID", "99487-7213", 0d19880826, .F., 83, 24600, "Record 454" }, ;
      { "Kirsten", "Weisbeck", "11860 Philadelphia Street", "High Point", "ND", "43642-4421", 0d19861017, .T., 74, 120100, "Record 455" }, ;
      { "Vosgo", "Fonseca", "6226 S. San Fernando Rd", "Lakeland", "WV", "03457-0833", 0d19920103, .F., 31, 3000, "Record 456" }, ;
      { "Chuck", "Gunkel", "11217 Hedline Place", "Chandler", "DE", "13384-2042", 0d19871202, .T., 58, 81500, "Record 457" }, ;
      { "Laura", "Sencovici", "24953 Access Rd", "Solana Beach", "OH", "46549-1709", 0d19840426, .F., 43, 32400, "Record 458" }, ;
      { "Francisco", "Knox", "12366 Washingtonian Blvd", "Winnipeg", "MD", "99064-8344", 0d19831121, .T., 52, 72300, "Record 459" }, ;
      { "Graham", "Goodman", "20253 Highbury Avenue", "Chimo Hills", "NJ", "59239-0147", 0d19880710, .F., 39, 9500, "Record 460" }, ;
      { "Hoechst", "Werry", "25760 Barfield Road", "Seattle", "NM", "48772-8598", 0d19890925, .T., 68, 126800, "Record 461" }, ;
      { "Hugh", "Bassett", "19904 West Davis Street", "Pocatelo", "TN", "13303-8580", 0d19900511, .F., 71, 7700, "Record 462" }, ;
      { "Orlando", "Barnes", "11687 Saffold Way", "Enola", "NC", "87688-2128", 0d19920901, .T., 33, 149600, "Record 463" }, ;
      { "Tony", "LeMont", "4474 Sibley Tower", "Hartselle", "CO", "69949-1413", 0d19920406, .F., 99, 143500, "Record 464" }, ;
      { "Barry", "Klein", "8693 Hannahs Road", "Chattanooga", "GA", "36685-1097", 0d19911108, .T., 27, 138500, "Record 465" }, ;
      { "Matthew", "Saylors", "14143 Chubb Ave", "Welkom", "MT", "76672-0466", 0d19840922, .F., 70, 70700, "Record 466" }, ;
      { "Dr. Hugo", "Schade", "14256 S. Oakley Ave.", "Torrance", "TX", "06616-2266", 0d19900224, .T., 45, 18300, "Record 467" }, ;
      { "Jeff", "Kline", "2144 Vandeven Court", "Athens", "NY", "52624-2111", 0d19900121, .F., 71, 149200, "Record 468" }, ;
      { "Randy", "Ellenwood", "3765 Carmenita Road", "Palo Alto", "WY", "48115-8803", 0d19880512, .T., 99, 68000, "Record 469" }, ;
      { "Daniel", "Maloof", "25680 Newman Avenue", "Baldwin", "KY", "69040-6405", 0d19890318, .F., 81, 79800, "Record 470" }, ;
      { "Ron", "Kelley", "31487 Barfield Road", "Feasterville", "LA", "86113-4099", 0d19861117, .T., 94, 78600, "Record 471" }, ;
      { "Terry", "Srondras", "5289 Atlantis Avenue", "Highett", "IA", "85024-7301", 0d19870509, .F., 62, 18800, "Record 472" }, ;
      { "Arthur", "Vergen", "13128 Country Brook", "New Oxford", "SD", "44533-0804", 0d19861216, .T., 75, 82400, "Record 473" }, ;
      { "Joe", "Posa", "3951 Maria Lane", "Tucson", "ID", "41626-9076", 0d19870325, .F., 36, 113100, "Record 474" }, ;
      { "Ken", "Estrellado", "18415 Jefferson Road", "Encinitas", "NE", "51166-6345", 0d19831211, .T., 41, 98000, "Record 475" }, ;
      { "Allen", "Davenport", "27541 Lack Cook Road", "Hermosa Beach", "WV", "67942-2521", 0d19840211, .F., 60, 135600, "Record 476" }, ;
      { "Leon", "Jacquot", "17947 Business Park", "Palo Alto", "RI", "11314-6549", 0d19891121, .T., 24, 118000, "Record 477" }, ;
      { "Maurice", "Hoffman", "3006 North Sheridan", "West Patterson", "MI", "17272-3356", 0d19880420, .F., 27, 65500, "Record 478" }, ;
      { "Bryan", "Bohne", "11038 Spring Street S.W.", "South Norwalk", "OK", "41986-3696", 0d19900518, .T., 88, 111900, "Record 479" }, ;
      { "Irene", "Elledge", "17223 Carolina Avenue", "Falls Church", "NM", "48142-1867", 0d19830806, .F., 75, 15300, "Record 480" }, ;
      { "Migdalia", "Brownson", "3100 Clarks Hill", "MacLeod", "MN", "44920-1591", 0d19900427, .T., 79, 85400, "Record 481" }, ;
      { "Kandasamy", "Finn", "31107 South Marine Drive", "Santa Fe Springs", "SC", "12322-0665", 0d19860506, .F., 63, 107600, "Record 482" }, ;
      { "Timothy", "Cooper", "5614 Wertzville Road", "Calhoun", "AR", "23878-0740", 0d19841002, .T., 43, 13900, "Record 483" }, ;
      { "Aaron", "Farley", "3910 Bauxile", "East Greenwich", "PA", "74746-5229", 0d19890617, .F., 29, 111400, "Record 484" }, ;
      { "Francisco", "Butler", "18754 West 82nd St", "Pacific Grove", "MT", "10909-5716", 0d19870430, .T., 63, 139000, "Record 485" }, ;
      { "Gregg", "Simpson", "2725 Marcus Avenue", "Bowie", "TX", "52957-8491", 0d19900403, .F., 24, 40900, "Record 486" }, ;
      { "Sheng", "McSweeney", "5893 E Woodward", "Lamar", "NY", "91675-3233", 0d19870408, .T., 83, 7300, "Record 487" }, ;
      { "Iain", "Kramedjian", "19709 Riverside Drive", "Los Alamitos", "CA", "65368-2399", 0d19871123, .F., 88, 48400, "Record 488" }, ;
      { "Luiz", "Kobb", "16643 West 11th Ave", "Victor", "SD", "94717-4704", 0d19861026, .T., 32, 108000, "Record 489" }, ;
      { "Tim", "Ticali", "22323 32nd Avenue South", "Martinez", "NE", "91153-1247", 0d19851010, .F., 71, 9300, "Record 490" }, ;
      { "Don", "Roberge", "8909 Sibley Tower", "Deerfield Beach", "ME", "73621-1906", 0d19910207, .T., 99, 22200, "Record 491" }, ;
      { "Taouk", "Dockett", "24636 Golden Springs Rd", "Princeton", "NH", "51508-6203", 0d19900705, .F., 27, 23300, "Record 492" }, ;
      { "Mario", "French", "18139 Hewes Street", "Williston", "HI", "28288-2203", 0d19830808, .T., 46, 55300, "Record 493" }, ;
      { "Gary", "Beilharz", "16861 Abbots Place", "Amerspoort", "WI", "79471-8583", 0d19910130, .F., 67, 144000, "Record 494" }, ;
      { "Guy", "Acker", "27636 Broadway", "Framingham", "AK", "84142-8069", 0d19890520, .T., 48, 39300, "Record 495" }, ;
      { "Dickson", "Valle", "32693 Stonecutter Lane", "Kowloon Bay", "AL", "18415-9263", 0d19910714, .F., 87, 42200, "Record 496" }, ;
      { "Roger", "Florio", "24268 Pine Tree Lane", "Virginia Beach", "MN", "19648-7987", 0d19840928, .T., 53, 100700, "Record 497" }, ;
      { "Roman", "Karant", "27769 N Los Altos", "Tarzana", "WA", "75979-1063", 0d19901228, .F., 73, 74100, "Record 498" }, ;
      { "Troy", "Barker", "19437 Windsong Place", "Norwood", "AR", "06553-9467", 0d19910924, .T., 26, 119600, "Record 499" }, ;
      { "Kenny", "Dysert", "12671 Pico Blvd", "Santee", "GA", "39439-5930", 0d19840409, .F., 73, 99700, "Record 500" } }
      dbAppend()
      FOR EACH f IN r
         FieldPut( f:__enumIndex(), f )
      NEXT
   NEXT
   dbGoTop()

   RETURN .T.
