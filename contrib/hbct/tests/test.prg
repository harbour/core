#require "hbct"
#require "hbtest"

PROCEDURE Main()

   LOCAL nTotal
   LOCAL a, b

   HBTEST AddAscii( "0000", 1, 1 )         IS "1000"
   HBTEST AddAscii( "0000", 1 )            IS "0001"
   HBTEST AddAscii( "AAAA", -255, 1 )      IS "BAAA"
   HBTEST AddAscii( "AAAA", -255 )         IS "AAAB"
   HBTEST AddAscii( "AAAA", 1, 2, .T. )    IS "ABAA"
   HBTEST AddAscii( "AAAA", 257, 2, .T. )  IS "BBAA"
   HBTEST AddAscii( "AAAA", 257, 2, .F. )  IS "ABAA"
   HBTEST AddAscii( "AAAA", 258,, .T. )    IS "AABC"
   HBTEST AddAscii( "ABBA", -257, 3, .T. ) IS "AAAA"

   HBTEST AsciiSum( Replicate( "A", 10000 ) ) IS 650000
   HBTEST AsciiSum( "0123456789" )            IS 525
   HBTEST AsciiSum( NIL )                     IS 0

   HBTEST AscPos( "0123456789" )     IS 57
   HBTEST AscPos( "0123456789", 1 )  IS 48
   HBTEST AscPos( "0123456789", 11 ) IS 0  // <nPosition> too large !

   HBTEST ValPos( "1234x56789" )     IS 9
   HBTEST ValPos( "1234x56789", 1 )  IS 1
   HBTEST ValPos( "1234x56789", 11 ) IS 0  // <nPosition> too large !
   HBTEST ValPos( "1234x56789", 5 )  IS 0  // "x" is not a digit !

   HBTEST AfterAtNum( "..", "..This..is..a..test!" )       IS "test!"
   HBTEST AfterAtNum( "..", "..This..is..a..test!", 2 )    IS "is..a..test!"
   HBTEST AfterAtNum( "..", "..This..is..a..test!", 2, 2 ) IS "a..test!"
   HBTEST BeforAtNum( "..", "..This..is..a..test!" )       IS "..This..is..a"
   HBTEST BeforAtNum( "..", "..This..is..a..test!", 2 )    IS "..This"
   HBTEST BeforAtNum( "..", "..This..is..a..test!", 2, 2 ) IS "..This..is"

   HBTEST AtNum( "..", "..This..is..a..test!" )       IS 14
   HBTEST AtNum( "..", "..This..is..a..test!", 2 )    IS 7
   HBTEST AtNum( "..", "..This..is..a..test!", 2, 2 ) IS 11

   HBTEST AtRepl( "ABC", "ABCDABCDABC", "xx" )         IS "xxDxxDxx"
   HBTEST AtRepl( "ABC", "ABCDABC", "ZYXW" )           IS "ZYXWDZYXW"
   HBTEST AtRepl( "ABC", "ABCDABCDABC", "xx", 2 )      IS "xxDxxDABC"
   HBTEST AtRepl( "ABC", "ABCDABCDABC", "xx", 2, .T. ) IS "ABCDxxDABC"

   HBTEST CharEven( " 1 2 3 4 5" ) IS "12345"
   HBTEST CharEven( " 1 2 3 4 " )  IS "1234"
   HBTEST CharEven( " " )          IS ""

   HBTEST CharOdd( "1A2B3C4D5E" )  IS "12345"
   HBTEST CharOdd( "1A2B3C4D5" )   IS "12345"

   HBTEST CharSList( "Hello World !" ) IS " !HWdelor"
   HBTEST CharSList( "Hello World !" ) IS CharSort( CharList( "Hello World !" ) )
   HBTEST CharSList( NIL )             IS ""

   HBTEST CharHist( "Hello World !" )[ 109 ] IS 3
   HBTEST ( nTotal := 0, AEval( CharHist( "Hello World !" ), {| x | nTotal += x } ), nTotal ) IS Len( "Hello World !" )

   HBTEST CharList( "Hello World !" ) IS "Helo Wrd!"
   HBTEST CharList( NIL )             IS ""

   HBTEST CharNoList( CharNoList( "Hello World !" ) ) IS CharSList( "Hello World !" )
   HBTEST CharNoList( CharNoList( NIL ) )             IS ""

   HBTEST CharMirr( "racecar" )        IS "racecar"
   HBTEST CharMirr( "racecar  ", .T. ) IS "racecar  "
   HBTEST CharMirr( "racecar  ", .F. ) IS "  racecar"

   HBTEST CharMix( "ABC", "123" )  IS "A1B2C3"
   HBTEST CharMix( "ABCDE", "12" ) IS "A1B2C1D2E1"
   HBTEST CharMix( "AB", "12345" ) IS "A1B2"
   HBTEST CharMix( "HELLO", " " )  IS "H E L L O "
   HBTEST CharMix( "HELLO", "" )   IS "HELLO"

   HBTEST CharOne( "122333a123" )      IS "123a123"
   HBTEST CharOne( "A  B  CCCD" )      IS "A B CD"
   HBTEST CharOne( " ", "A  B  A  B" ) IS "A B A B"
   HBTEST CharOne( "o", "122oooB12o" ) IS "122oB12o"

   HBTEST WordOne( "12ABAB12" )       IS "12AB12"
   HBTEST WordOne( "1AAAA2" )         IS "1AAAA2"
   HBTEST WordOne( "12", "1212ABAB" ) IS "12ABAB"

   HBTEST CharOnly( "0123456789", "0211 - 38 99 77" ) IS "0211389977"
   HBTEST CharOnly( "0123456789", "0211/ 389 977" )   IS "0211389977"

   HBTEST WordOnly( "AABBCCDD", "XXAAYYBBZZ" ) IS "AABB"
   HBTEST WordOnly( "AABBCCDD", "XAAYYYBBZZ" ) IS "BB"

   HBTEST CharRem( " ", " 1  2  " ) IS "12"
   HBTEST CharRem( "3y", "xyz123" ) IS "xz12"

   HBTEST WordRem( "abcd", "0ab1cd" ) IS "0ab1"
   HBTEST WordRem( "abcd", "ab0cd1" ) IS "0cd1"

   HBTEST CharAdd( "012345678", hb_BChar( 1 ) )                     IS "123456789"
   HBTEST CharAdd( "012345678", hb_BChar( 1 ) + hb_BChar( 2 ) )     IS "133557799"
   HBTEST CharAdd( "123456789", hb_BChar( 255 ) )                   IS "012345678"
   HBTEST CharAdd( "123456789", hb_BChar( 255 ) + hb_BChar( 254 ) ) IS "002244668"
   HBTEST CharAnd( "012345678", hb_BChar( 254 ) )                   IS "002244668"
   HBTEST CharAnd( "012345678", hb_BChar( 254 ) + hb_BChar( 252 ) ) IS "002044648"

   HBTEST CharNot( hb_BChar( 85 ) + hb_BChar( 128 ) + hb_BChar( 170 ) + hb_BChar( 1 ) ) IS hb_BChar( 170 ) + hb_BChar( 127 ) + hb_BChar( 85 ) + hb_BChar( 254 )
   HBTEST CharNot( CharNot( "This is a test!" ) ) IS "This is a test!"

   HBTEST CharOr( "012345678", hb_BChar( 1 ) )                 IS "113355779"
   HBTEST CharOr( "012345678", hb_BChar( 1 ) + hb_BChar( 3 ) ) IS "133357779"

   HBTEST CharXor( CharXor( "This is top secret !", "My Password" ), "My Password" ) IS "This is top secret !"

   HBTEST CharSub( "123456789", hb_BChar( 1 ) )                     IS "012345678"
   HBTEST CharSub( "123456789", hb_BChar( 1 ) + hb_BChar( 2 ) )     IS "002244668"
   HBTEST CharSub( "012345678", hb_BChar( 255 ) )                   IS "123456789"
   HBTEST CharSub( "012345678", hb_BChar( 255 ) + hb_BChar( 254 ) ) IS "133557799"

   HBTEST CharShl( hb_BChar( 1 ) + hb_BChar( 2 ) + hb_BChar( 4 ) + hb_BChar( 8 ) + hb_BChar( 16 ) + hb_BChar( 32 ) + hb_BChar( 64 ) + hb_BChar( 128 ), 3 ) IS hb_BChar( 8 ) + hb_BChar( 16 ) + hb_BChar( 32 ) + hb_BChar( 64 ) + hb_BChar( 128 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 )
   HBTEST CharShr( hb_BChar( 1 ) + hb_BChar( 2 ) + hb_BChar( 4 ) + hb_BChar( 8 ) + hb_BChar( 16 ) + hb_BChar( 32 ) + hb_BChar( 64 ) + hb_BChar( 128 ), 3 ) IS hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 1 ) + hb_BChar( 2 ) + hb_BChar( 4 ) + hb_BChar( 8 ) + hb_BChar( 16 )
   HBTEST CharRll( hb_BChar( 1 ) + hb_BChar( 2 ) + hb_BChar( 4 ) + hb_BChar( 8 ) + hb_BChar( 16 ) + hb_BChar( 32 ) + hb_BChar( 64 ) + hb_BChar( 128 ), 3 ) IS hb_BChar( 8 ) + hb_BChar( 16 ) + hb_BChar( 32 ) + hb_BChar( 64 ) + hb_BChar( 128 ) + hb_BChar( 1 ) + hb_BChar( 2 ) + hb_BChar( 4 )
   HBTEST CharRlr( hb_BChar( 1 ) + hb_BChar( 2 ) + hb_BChar( 4 ) + hb_BChar( 8 ) + hb_BChar( 16 ) + hb_BChar( 32 ) + hb_BChar( 64 ) + hb_BChar( 128 ), 3 ) IS hb_BChar( 32 ) + hb_BChar( 64 ) + hb_BChar( 128 ) + hb_BChar( 1 ) + hb_BChar( 2 ) + hb_BChar( 4 ) + hb_BChar( 8 ) + hb_BChar( 16 )

   HBTEST CharRepl( "1234", "1x2y3z", "abcd" )            IS "axbycz"
   HBTEST CharRepl( "abcdefghij", "jhfdb", "1234567890" ) IS "08642"
   HBTEST CharRepl( "abcdefghij", "jhfdb", "12345" )      IS "55542"
   HBTEST CharRepl( "1234", "1234", "234A" )              IS "AAAA"
   HBTEST CharRepl( "1234", "1234", "234A", .T. )         IS "234A"

   HBTEST CharSort( "qwert" )                     IS "eqrtw"
   HBTEST CharSort( "qwert", 2 )                  IS "erqwt"
   HBTEST CharSort( "b1a4a3a2a1", 2, 1 )          IS "a2a1a3a4b1"
   HBTEST CharSort( "XXXqwert", 1, 1, 3 )         IS "XXXeqrtw"
   HBTEST CharSort( "b1a4a3a2a1", 2, 1, 0, 1 )    IS "a1b1a2a3a4"
   HBTEST CharSort( "384172852", 1, 1, 0, 0, 4 )  IS "134872852"
   HBTEST CharSort( "qwert",,,,,, .T. )           IS "wtrqe"

   HBTEST CharSwap( "0123456789" )  IS "1032547698"
   HBTEST CharSwap( "ABCDEFGHIJK" ) IS "BADCFEHGJIK"

   HBTEST WordSwap( "1234567890" )      IS "3412785690"
   HBTEST WordSwap( "1234567890", .T. ) IS "4321876590"

   HBTEST Floor( 1.1 )  IS 1.0
   HBTEST Floor( -1.1 ) IS -2.0
   HBTEST Ceiling( 1.1 )  IS 2.0
   HBTEST Ceiling( -1.1 ) IS -1.0
   HBTEST Sign( 1.1 )  IS 1
   HBTEST Sign( -1.1 ) IS -1
   HBTEST Sign( 0.0 )  IS 0
   HBTEST Log10( 10.0 )         IS 1.0
   HBTEST Log10( Sqrt( 10.0 ) ) IS 0.5
   HBTEST Fact( 0 ) IS 1
   HBTEST Fact( 1 ) IS 1
   HBTEST Fact( 4 ) IS 24
   HBTEST Round( FV( 1000, 0.00, 10 ), 1 ) IS 10000.0
   HBTEST Round( FV( 1000, 0.05, 10 ), 3 ) IS 12577.893
   HBTEST Round( PV( 100, 0.0, 60 ), 1 )   IS 6000.0
   HBTEST Round( PV( 100, 0.005, 60 ), 2 ) IS 5172.56
   HBTEST Round( Payment( 5172.56, 0.0, 60 ), 2 )   IS 86.21
   HBTEST Round( Payment( 5172.56, 0.005, 60 ), 2 ) IS 100.00
   HBTEST Round( Periods( 5172.56, 100, 0.005 ), 1 ) IS 60.0
   HBTEST Round( Periods( 5172.56, 100, 0.0 ), 4 ) IS 51.7256
   HBTEST Round( Rate( 5172.56, 100, 60.0 ), 3 ) IS 0.005
   HBTEST Round( Rate( 6000.0, 100, 60.0 ), 1 ) IS 0.0

   HBTEST Round( Celsius( 32.0 ), 1 )  IS 0.0
   HBTEST Round( Celsius( 212.0 ), 1 ) IS 100.0
   HBTEST Round( Fahrenheit( 0.0 ), 1 ) IS 32.0
   HBTEST Round( Celsius( 100.0 ), 2 ) IS 37.78

   HBTEST RangeRem( "0", "9", "year2002.dbf" ) IS "year.dbf"
   HBTEST RangeRem( "9", "0", "year2002.dbf" ) IS "22"
   HBTEST RangeRem( "0", "9", "yearcurr.dbf" ) IS "yearcurr.dbf"

   HBTEST RangeRepl( "0", "9", "year2002.dbf", "?" ) IS "year????.dbf"
   HBTEST RangeRepl( "9", "0", "year2002.dbf", "?" ) IS "????2??2????"
   HBTEST RangeRepl( "0", "9", "yearcurr.dbf", "?" ) IS "yearcurr.dbf"

   HBTEST StrDiff( "ABC", "ADC" )  IS 3
   HBTEST StrDiff( "ABC", "AEC" )  IS 3
   HBTEST StrDiff( "CBA", "ABC" )  IS 6
   HBTEST StrDiff( "ABC", "AXBC" ) IS 1
   HBTEST StrDiff( "AXBC", "ABC" ) IS 6
   HBTEST StrDiff( "AXBC", "ADC" ) IS 9

   HBTEST TabExpand( "-" + Chr( 9 ) + "!" )            IS "-       !"
   HBTEST TabExpand( "----" + Chr( 9 ) + "!" )         IS "----    !"
   HBTEST TabExpand( "-" + Chr( 9 ) + "!", , "+" )     IS "-+++++++!"
   HBTEST TabExpand( "-" + Chr( 9 ) + "!", 4 )         IS "-   !"
   HBTEST TabExpand( "----" + Chr( 9 ) + "!", 8 )      IS "----    !"
   HBTEST TabExpand( "----" + Chr( 9 ) + "!", 8, "+" ) IS "----++++!"
   HBTEST TabExpand( "-" + Chr( 9 ) + "!" + hb_eol() + "----" + Chr( 9 ) + "!", , "+" ) IS "-+++++++!" + hb_eol() + "----++++!"

   HBTEST AtToken( "Hello, World!" )            IS 8
   HBTEST AtToken( "Hello, World!",, 2 )        IS 8
   HBTEST AtToken( "Hello, World!",, 2, 1 )     IS 7
   HBTEST AtToken( "Hello, World!", " ", 2, 1 ) IS 8

   HBTEST Token( "Hello, World!" )            IS "World"
   HBTEST Token( "Hello, World!",, 2, 1 )     IS ""
   HBTEST Token( "Hello, World!", ",", 2, 1 ) IS " World!"
   HBTEST Token( "Hello, World!", " ", 2, 1 ) IS "World!"

   HBTEST NumToken( "Hello, World!" ) IS  2
   HBTEST NumToken( "This is good. See you! How do you do?", ".!?" ) IS 3
   HBTEST NumToken( "one,,three,four,,six", ",", 1 ) IS  6

   HBTEST TokenLower( "Hello, World, here I am!" )         IS "hello, world, here i am!"
   HBTEST TokenLower( "Hello, World, here I am!",, 3 )     IS "hello, world, here I am!"
   HBTEST TokenLower( "Hello, World, here I am!", ",", 3 ) IS "hello, World, here I am!"
   HBTEST TokenLower( "Hello, World, here I am!", " W" )   IS "hello, World, here i am!"
   HBTEST TokenUpper( "Hello, world, here I am!" )         IS "Hello, World, Here I Am!"
   HBTEST TokenUpper( "Hello, world, here I am!",, 3 )     IS "Hello, World, Here I am!"
   HBTEST TokenUpper( "Hello, world, here I am!", ",", 3 ) IS "Hello, world, here I am!"
   HBTEST TokenUpper( "Hello, world, here I am!", " w" )   IS "Hello, wOrld, Here I Am!"

   HBTEST Sin( 0.0 ) IS 0.0
   HBTEST Round( Sin( Pi() / 4 ), 4 ) IS Round( Sqrt( 1 / 2 ), 4 )
   HBTEST Sin( Pi() / 2 ) IS 1.0
   HBTEST Round( Sin( Pi() ), 1 ) IS 0.0
   HBTEST Cos( 0.0 ) IS 1.0
   HBTEST Round( Cos( Pi() / 4 ), 4 ) IS Round( Sqrt( 1 / 2 ), 4 )
   HBTEST Round( Cos( Pi() / 2 ), 1 ) IS 0.0
   HBTEST Cos( Pi() ) IS -1.0
   HBTEST Tan( 0.0 ) IS 0.0
   HBTEST Round( Tan( Pi() / 4 ), 0 ) IS 1
   HBTEST Round( Tan( Pi() ), 1 ) IS 0.0
   HBTEST Cot( Pi() / 4 ) IS 1
   HBTEST Round( Cot( Pi() / 2 ), 0 ) IS 0
   HBTEST Asin( 0.0 ) IS 0.0
   HBTEST Round( Asin( Sqrt( 1 / 2 ) ), 4 ) IS Round( Pi() / 4, 4 )
   HBTEST Asin( 1.0 ) IS Pi() / 2
   HBTEST Asin( 0.0 ) IS 0.0  // and not Pi(), since the smallest angle is returned !
   HBTEST Acos( 0.0 ) IS Pi() / 2
   HBTEST Acos( Sqrt( 1 / 2 ) ) IS Pi() / 4
   HBTEST Acos( 1.0 ) IS 0.0
   HBTEST Acos( -1.0 ) IS Pi()
   HBTEST Acos( 0.0 ) IS Pi() / 2  // and not -Pi()/2, although cos (-Pi()/2) IS 0.0 !
   HBTEST Atan( 0.0 ) IS 0.0
   HBTEST Atan( 1.0 ) IS Pi() / 4
   HBTEST Atan( 0.0 ) IS 0.0 // and not Pi(), although Tan( Pi() ) IS 0.0 !
   HBTEST Atn2( 0.0, 1.0 ) IS 0.0
   HBTEST Atn2( Sqrt( 1 / 2 ), Sqrt( 1 / 2 ) ) IS Pi() / 4
   HBTEST Atn2( -Sqrt( 1 / 2 ), -Sqrt( 1 / 2 ) ) IS -3 / 4 * Pi()  // Atan() would return Pi() / 4 !
   HBTEST Sinh( 0.0 ) IS 0.0
   HBTEST Sinh( -0.5 ) IS -Sinh( 0.5 )
   HBTEST Cosh( 0.0 ) IS 1.0
   HBTEST Cosh( -0.5 ) IS Cosh( 0.5 )
   HBTEST Tanh( 0.0 ) IS 0.0
   HBTEST Tanh( -0.5 ) IS -Tanh( 0.5 )
   HBTEST RToD( 0.0 ) IS 0.0
   HBTEST RToD( Pi() ) IS 180.0
   HBTEST DToR( 0.0 ) IS 0.0
   HBTEST DToR( 180.0 ) IS Pi()

   HBTEST WordRepl( "CC", "AABBCCDDEE", "XX" ) IS "AABBXXDDEE"
   HBTEST WordRepl( "aa", "1aaaa", "ba" )      IS "1abaa"
   HBTEST WordRepl( "aa", "1aaaa", "ba", .T. ) IS "1baba"

   HBTEST ( CSetAtMupa( .T. ), WordRepl( "aa", "1aaaa", "ba" ) )      IS "1abaa"
   HBTEST ( CSetAtMupa( .T. ), WordRepl( "aa", "1aaaa", "ba", .T. ) ) IS "1bbba"

   hb_langSelect( "en" )

   HBTEST CToDoW()              IS 0
   HBTEST CToDoW( 1 )           IS 0
   HBTEST CToDoW( "" )          IS 0
   HBTEST CToDoW( "Sunday" )    IS 1
   HBTEST CToDoW( "WEDNESDAY" ) IS 4
   HBTEST CToDoW( "Wednesday" ) IS 4
   HBTEST CToDoW( "Wed" )       IS 4
   HBTEST CToDoW( "M" )         IS 2

   HBTEST CToMonth()            IS 0
   HBTEST CToMonth( 1 )         IS 0
   HBTEST CToMonth( "" )        IS 0
   HBTEST CToMonth( "January" ) IS 1
   HBTEST CToMonth( "AUGUST" )  IS 8
   HBTEST CToMonth( "August" )  IS 8
   HBTEST CToMonth( "Au" )      IS 8
   HBTEST CToMonth( "A" )       IS 4

   HBTEST SecToTime( 1000, .F. )     IS "00:16:40"
   HBTEST SecToTime( 1000, .T. )     IS "00:16:40:00"
   HBTEST SecToTime( 3656 - 170 )    IS "00:58:06"
   HBTEST SecToTime( 45873.22, .T. ) IS "12:44:33:22"

   HBTEST ( a := "a", b := "b", StrSwap( @a, @b ) )        IS ""
   HBTEST ( a := "a", b := "b", StrSwap( @a, @b ), a + b ) IS "ba"
   HBTEST ( a := "a", b := "b", StrSwap(  a, @b ), a + b ) IS "aa"
   HBTEST ( a := "a", b := "b", StrSwap( @a,  b ), a + b ) IS "bb"
   HBTEST ( a := "a", b := "b", StrSwap(  a,  b ), a + b ) IS "ab"
   HBTEST ( a := NIL, b := NIL, StrSwap( @a, @b ), a + b ) IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST ( a := 100, b := 200, StrSwap( @a, @b ), a / b ) IS 0.5
   HBTEST StrSwap()                                        IS ""
   HBTEST StrSwap( NIL, NIL )                              IS ""

   RETURN
