/*
 * Harbour Project source code:
 * Regression tests for the runtime library (misc)
 *
 * Copyright 1999-2014 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
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

#include "rt_main.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

#undef HB_CLP_STRICT_OFF
#ifdef __HARBOUR__
   #ifndef HB_CLP_STRICT
      #define HB_CLP_STRICT_OFF
   #endif
#endif

PROCEDURE Main_MISC()

   LOCAL oError
#ifdef __HARBOUR__
   LOCAL cEOL
#endif
   LOCAL o, tmp := 0

#ifdef __HARBOUR__

   /* Hashes */

   Test_Hash()

   /* SHA-1 */

   HBTEST              hb_SHA1( "hello" )                    IS "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
   HBTEST              hb_SHA1( "hello", .F. )               IS "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
   HBTEST hb_StrToHex( hb_SHA1( "hello", .T. ) )             IS "AAF4C61DDCC5E8A2DABEDE0F3B482CD9AEA9434D"

   HBTEST              hb_HMAC_SHA1( "hello", "key" )        IS "b34ceac4516ff23a143e61d79d0fa7a4fbe5f266"
   HBTEST              hb_HMAC_SHA1( "hello", "key", .F. )   IS "b34ceac4516ff23a143e61d79d0fa7a4fbe5f266"
   HBTEST hb_StrToHex( hb_HMAC_SHA1( "hello", "key", .T. ) ) IS "B34CEAC4516FF23A143E61D79D0FA7A4FBE5F266"

   /* https://www.ietf.org/rfc/rfc3174.txt */

   HBTEST Lower( hb_SHA1( "abc"                                                                               ) ) IS "a9993e364706816aba3e25717850c26c9cd0d89d"
   HBTEST Lower( hb_SHA1( "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"                          ) ) IS "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
   HBTEST Lower( hb_SHA1( Replicate( "a", 1000000 )                                                           ) ) IS "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
   HBTEST Lower( hb_SHA1( Replicate( "0123456701234567012345670123456701234567012345670123456701234567", 10 ) ) ) IS "dea356a2cddd90c7a7ecedc5ebb563934f460452"

   /* SHA-2 */

   Test_SHA2()
   Test_SHA2_HMAC()

#endif

   /* Some random error object tests taken from the separate test source */

   oError := ErrorNew()
   HBTEST oError:ClassName()              IS "ERROR"
   oError:Description := "Its description"
   HBTEST oError:Description              IS "Its description"
#ifdef __CLIPPER__
   HBTEST Len( oError )                   IS 7
#endif
#ifdef __HARBOUR__
   HBTEST Len( oError )                   IS 12
#endif

   /* Set()s */

   HBTEST Set( _SET_MARGIN     )       IS 0
   HBTEST Set( _SET_MARGIN    , -1 )   IS "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:25;N:-1 "

#ifdef HB_COMPAT_C53
   HBTEST Set( _SET_EVENTMASK  )       IS 128
   HBTEST Set( _SET_VIDEOMODE  )       IS NIL
   HBTEST Set( _SET_MBLOCKSIZE )       IS 64
   HBTEST Set( _SET_MFILEEXT   )       IS ""
   HBTEST Set( _SET_STRICTREAD )       IS .F.
   HBTEST Set( _SET_OPTIMIZE   )       IS .T.
   HBTEST Set( _SET_AUTOPEN    )       IS .T.
   HBTEST Set( _SET_AUTORDER   )       IS 0
   HBTEST Set( _SET_AUTOSHARE  )       IS 0

   HBTEST Set( _SET_EVENTMASK , -1 )   IS "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:39;N:-1 "
   HBTEST Set( _SET_VIDEOMODE , -1 )   IS NIL
   HBTEST Set( _SET_MBLOCKSIZE, -1 )   IS "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:41;N:-1 "
   HBTEST Set( _SET_MFILEEXT  , {} )   IS ""
   HBTEST Set( _SET_STRICTREAD, {} )   IS .F.
   HBTEST Set( _SET_OPTIMIZE  , {} )   IS .T.
   HBTEST Set( _SET_AUTOPEN   , {} )   IS .T.
   HBTEST Set( _SET_AUTORDER  , -1 )   IS "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:46;N:-1 "
   HBTEST Set( _SET_AUTOSHARE , -1 )   IS "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:47;N:-1 "
#endif

   HBTEST Set( _SET_ALTFILE,   BADFNAME2() ) IS "E 20 TERM 2013 Create error <" + BADFNAME2() + "> OS:2 #:1 F:DR"
   HBTEST Set( _SET_PRINTFILE, BADFNAME2() ) IS "E 20 TERM 2014 Create error <" + BADFNAME2() + "> OS:2 #:1 F:DR"
   HBTEST Set( _SET_EXTRAFILE, BADFNAME2() ) IS "E 20 TERM 2015 Create error <" + BADFNAME2() + "> OS:2 #:1 F:DR"

   /* Some color handling tests */

   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "T"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "A"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "B"                          ), SetColor() ) IS "B/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "C"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "D"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "E"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "F"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "G"                          ), SetColor() ) IS "G/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "H"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "I"                          ), SetColor() ) IS "N/W,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "J"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "K"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "L"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "M"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "N"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "O"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "P"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "Q"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "R"                          ), SetColor() ) IS "R/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "S"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "T"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "U"                          ), SetColor() ) IS "U/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "V"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "W"                          ), SetColor() ) IS "W/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "X"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "Y"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "Z"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "0"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1"                          ), SetColor() ) IS "B/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "2"                          ), SetColor() ) IS "G/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "3"                          ), SetColor() ) IS "BG/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "4"                          ), SetColor() ) IS "R/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "5"                          ), SetColor() ) IS "BR/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "6"                          ), SetColor() ) IS "GR/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "7"                          ), SetColor() ) IS "W/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "8"                          ), SetColor() ) IS "N+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "9"                          ), SetColor() ) IS "B+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "10"                         ), SetColor() ) IS "G+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "11"                         ), SetColor() ) IS "BG+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "12"                         ), SetColor() ) IS "R+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "13"                         ), SetColor() ) IS "BR+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "14"                         ), SetColor() ) IS "GR+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "15"                         ), SetColor() ) IS "W+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "16"                         ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "@"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "!"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "-"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "/"                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "//"                         ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( ","                          ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( ",,"                         ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "+"                          ), SetColor() ) IS "N+/N,N/N,N/N,N/N,N/N"
#ifdef HB_CLP_STRICT_OFF
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "+*"                         ), SetColor() ) IS "N+/N*,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*"                          ), SetColor() ) IS "N/N*,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*+"                         ), SetColor() ) IS "N+/N*,N/N,N/N,N/N,N/N"
#else
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "+*"                         ), SetColor() ) IS "N*+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*"                          ), SetColor() ) IS "N*/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*+"                         ), SetColor() ) IS "N*+/N,N/N,N/N,N/N,N/N"
#endif
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "BR/W+"                      ), SetColor() ) IS "BR+/W,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "BR/W+"                      ), SetColor() ) IS "BR+/W,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "RB/W+"                      ), SetColor() ) IS "BR+/W,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "0123456789"                 ), SetColor() ) IS "BR/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1234567890"                 ), SetColor() ) IS "G/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1"                          ), SetColor() ) IS "B/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "11"                         ), SetColor() ) IS "BG+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1111111111"                 ), SetColor() ) IS "W/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "2"                          ), SetColor() ) IS "G/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "22"                         ), SetColor() ) IS "GR/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "2222222222"                 ), SetColor() ) IS "GR+/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ), SetColor() ) IS "N/N,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "ABCDEFGHIJKLMNOPQRSTUVW"    ), SetColor() ) IS "N/U,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "N/W+"                       ), SetColor() ) IS "N+/W,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( '"W"'+"/"+'"R"'              ), SetColor() ) IS "W/R,N/N,N/N,N/N,N/N"
   HBTEST ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "'W'"+"/"+"'R'"              ), SetColor() ) IS "W/R,N/N,N/N,N/N,N/N"

   SetColor( "" ) /* Reset color to default */

   o := _GET_( tmp, "tmp" )

#ifdef HB_COMPAT_C53
   HBTEST ( o:colorSpec := "T"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "A"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "B"                         , o:colorSpec ) IS "B/N,B/N,B/N,B/N"
   HBTEST ( o:colorSpec := "C"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "D"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "E"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "F"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "G"                         , o:colorSpec ) IS "G/N,G/N,G/N,G/N"
   HBTEST ( o:colorSpec := "H"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "I"                         , o:colorSpec ) IS "N/W,N/W,N/W,N/W"
   HBTEST ( o:colorSpec := "J"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "K"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "L"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "M"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "N"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "O"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "P"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "Q"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "R"                         , o:colorSpec ) IS "R/N,R/N,R/N,R/N"
   HBTEST ( o:colorSpec := "S"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "T"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "U"                         , o:colorSpec ) IS "U/N,U/N,U/N,U/N"
   HBTEST ( o:colorSpec := "V"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "W"                         , o:colorSpec ) IS "W/N,W/N,W/N,W/N"
   HBTEST ( o:colorSpec := "X"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "Y"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "Z"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "0"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "1"                         , o:colorSpec ) IS "B/N,B/N,B/N,B/N"
   HBTEST ( o:colorSpec := "2"                         , o:colorSpec ) IS "G/N,G/N,G/N,G/N"
   HBTEST ( o:colorSpec := "3"                         , o:colorSpec ) IS "BG/N,BG/N,BG/N,BG/N"
   HBTEST ( o:colorSpec := "4"                         , o:colorSpec ) IS "R/N,R/N,R/N,R/N"
   HBTEST ( o:colorSpec := "5"                         , o:colorSpec ) IS "BR/N,BR/N,BR/N,BR/N"
   HBTEST ( o:colorSpec := "6"                         , o:colorSpec ) IS "GR/N,GR/N,GR/N,GR/N"
   HBTEST ( o:colorSpec := "7"                         , o:colorSpec ) IS "W/N,W/N,W/N,W/N"
   HBTEST ( o:colorSpec := "8"                         , o:colorSpec ) IS "N+/N,N+/N,N+/N,N+/N"
   HBTEST ( o:colorSpec := "9"                         , o:colorSpec ) IS "B+/N,B+/N,B+/N,B+/N"
   HBTEST ( o:colorSpec := "10"                        , o:colorSpec ) IS "G+/N,G+/N,G+/N,G+/N"
   HBTEST ( o:colorSpec := "11"                        , o:colorSpec ) IS "BG+/N,BG+/N,BG+/N,BG+/N"
   HBTEST ( o:colorSpec := "12"                        , o:colorSpec ) IS "R+/N,R+/N,R+/N,R+/N"
   HBTEST ( o:colorSpec := "13"                        , o:colorSpec ) IS "BR+/N,BR+/N,BR+/N,BR+/N"
   HBTEST ( o:colorSpec := "14"                        , o:colorSpec ) IS "GR+/N,GR+/N,GR+/N,GR+/N"
   HBTEST ( o:colorSpec := "15"                        , o:colorSpec ) IS "W+/N,W+/N,W+/N,W+/N"
   HBTEST ( o:colorSpec := "16"                        , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "@"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "!"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "-"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "/"                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "//"                        , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := ","                         , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := ",,"                        , o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "+"                         , o:colorSpec ) IS "N+/N,N+/N,N+/N,N+/N"
#ifdef HB_CLP_STRICT_OFF
   HBTEST ( o:colorSpec := "+*"                        , o:colorSpec ) IS "N+/N*,N+/N*,N+/N*,N+/N*"
   HBTEST ( o:colorSpec := "*"                         , o:colorSpec ) IS "N/N*,N/N*,N/N*,N/N*"
   HBTEST ( o:colorSpec := "*+"                        , o:colorSpec ) IS "N+/N*,N+/N*,N+/N*,N+/N*"
#else
   HBTEST ( o:colorSpec := "+*"                        , o:colorSpec ) IS "N*+/N,N*+/N,N*+/N,N*+/N"
   HBTEST ( o:colorSpec := "*"                         , o:colorSpec ) IS "N*/N,N*/N,N*/N,N*/N"
   HBTEST ( o:colorSpec := "*+"                        , o:colorSpec ) IS "N*+/N,N*+/N,N*+/N,N*+/N"
#endif
   HBTEST ( o:colorSpec := "BR/W+"                     , o:colorSpec ) IS "BR+/W,BR+/W,BR+/W,BR+/W"
   HBTEST ( o:colorSpec := "BR/W+"                     , o:colorSpec ) IS "BR+/W,BR+/W,BR+/W,BR+/W"
   HBTEST ( o:colorSpec := "RB/W+"                     , o:colorSpec ) IS "BR+/W,BR+/W,BR+/W,BR+/W"
   HBTEST ( o:colorSpec := "0123456789"                , o:colorSpec ) IS "BR/N,BR/N,BR/N,BR/N"
   HBTEST ( o:colorSpec := "1234567890"                , o:colorSpec ) IS "G/N,G/N,G/N,G/N"
   HBTEST ( o:colorSpec := "1"                         , o:colorSpec ) IS "B/N,B/N,B/N,B/N"
   HBTEST ( o:colorSpec := "11"                        , o:colorSpec ) IS "BG+/N,BG+/N,BG+/N,BG+/N"
   HBTEST ( o:colorSpec := "1111111111"                , o:colorSpec ) IS "W/N,W/N,W/N,W/N"
   HBTEST ( o:colorSpec := "2"                         , o:colorSpec ) IS "G/N,G/N,G/N,G/N"
   HBTEST ( o:colorSpec := "22"                        , o:colorSpec ) IS "GR/N,GR/N,GR/N,GR/N"
   HBTEST ( o:colorSpec := "2222222222"                , o:colorSpec ) IS "GR+/N,GR+/N,GR+/N,GR+/N"
   HBTEST ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVWXYZ", o:colorSpec ) IS "N/N,N/N,N/N,N/N"
   HBTEST ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVW"   , o:colorSpec ) IS "N/U,N/U,N/U,N/U"
   HBTEST ( o:colorSpec := "N/W+"                      , o:colorSpec ) IS "N+/W,N+/W,N+/W,N+/W"
   HBTEST ( o:colorSpec := '"W"'+"/"+'"R"'             , o:colorSpec ) IS "W/R,W/R,W/R,W/R"
   HBTEST ( o:colorSpec := "'W'"+"/"+"'R'"             , o:colorSpec ) IS "W/R,W/R,W/R,W/R"
#else
   HBTEST ( o:colorSpec := "T"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "A"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "B"                         , o:colorSpec ) IS "B/N,B/N"
   HBTEST ( o:colorSpec := "C"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "D"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "E"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "F"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "G"                         , o:colorSpec ) IS "G/N,G/N"
   HBTEST ( o:colorSpec := "H"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "I"                         , o:colorSpec ) IS "N/W,N/W"
   HBTEST ( o:colorSpec := "J"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "K"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "L"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "M"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "N"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "O"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "P"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "Q"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "R"                         , o:colorSpec ) IS "R/N,R/N"
   HBTEST ( o:colorSpec := "S"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "T"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "U"                         , o:colorSpec ) IS "U/N,U/N"
   HBTEST ( o:colorSpec := "V"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "W"                         , o:colorSpec ) IS "W/N,W/N"
   HBTEST ( o:colorSpec := "X"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "Y"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "Z"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "0"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "1"                         , o:colorSpec ) IS "B/N,B/N"
   HBTEST ( o:colorSpec := "2"                         , o:colorSpec ) IS "G/N,G/N"
   HBTEST ( o:colorSpec := "3"                         , o:colorSpec ) IS "BG/N,BG/N"
   HBTEST ( o:colorSpec := "4"                         , o:colorSpec ) IS "R/N,R/N"
   HBTEST ( o:colorSpec := "5"                         , o:colorSpec ) IS "BR/N,BR/N"
   HBTEST ( o:colorSpec := "6"                         , o:colorSpec ) IS "GR/N,GR/N"
   HBTEST ( o:colorSpec := "7"                         , o:colorSpec ) IS "W/N,W/N"
   HBTEST ( o:colorSpec := "8"                         , o:colorSpec ) IS "N+/N,N+/N"
   HBTEST ( o:colorSpec := "9"                         , o:colorSpec ) IS "B+/N,B+/N"
   HBTEST ( o:colorSpec := "10"                        , o:colorSpec ) IS "G+/N,G+/N"
   HBTEST ( o:colorSpec := "11"                        , o:colorSpec ) IS "BG+/N,BG+/N"
   HBTEST ( o:colorSpec := "12"                        , o:colorSpec ) IS "R+/N,R+/N"
   HBTEST ( o:colorSpec := "13"                        , o:colorSpec ) IS "BR+/N,BR+/N"
   HBTEST ( o:colorSpec := "14"                        , o:colorSpec ) IS "GR+/N,GR+/N"
   HBTEST ( o:colorSpec := "15"                        , o:colorSpec ) IS "W+/N,W+/N"
   HBTEST ( o:colorSpec := "16"                        , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "@"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "!"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "-"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "/"                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "//"                        , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := ","                         , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := ",,"                        , o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "+"                         , o:colorSpec ) IS "N+/N,N+/N"
#ifdef HB_CLP_STRICT_OFF
   HBTEST ( o:colorSpec := "+*"                        , o:colorSpec ) IS "N+/N*,N+/N*"
   HBTEST ( o:colorSpec := "*"                         , o:colorSpec ) IS "N/N*,N/N*"
   HBTEST ( o:colorSpec := "*+"                        , o:colorSpec ) IS "N+/N*,N+/N*"
#else
   HBTEST ( o:colorSpec := "+*"                        , o:colorSpec ) IS "N*+/N,N*+/N"
   HBTEST ( o:colorSpec := "*"                         , o:colorSpec ) IS "N*/N,N*/N"
   HBTEST ( o:colorSpec := "*+"                        , o:colorSpec ) IS "N*+/N,N*+/N"
#endif
   HBTEST ( o:colorSpec := "BR/W+"                     , o:colorSpec ) IS "BR+/W,BR+/W"
   HBTEST ( o:colorSpec := "BR/W+"                     , o:colorSpec ) IS "BR+/W,BR+/W"
   HBTEST ( o:colorSpec := "RB/W+"                     , o:colorSpec ) IS "BR+/W,BR+/W"
   HBTEST ( o:colorSpec := "0123456789"                , o:colorSpec ) IS "BR/N,BR/N"
   HBTEST ( o:colorSpec := "1234567890"                , o:colorSpec ) IS "G/N,G/N"
   HBTEST ( o:colorSpec := "1"                         , o:colorSpec ) IS "B/N,B/N"
   HBTEST ( o:colorSpec := "11"                        , o:colorSpec ) IS "BG+/N,BG+/N"
   HBTEST ( o:colorSpec := "1111111111"                , o:colorSpec ) IS "W/N,W/N"
   HBTEST ( o:colorSpec := "2"                         , o:colorSpec ) IS "G/N,G/N"
   HBTEST ( o:colorSpec := "22"                        , o:colorSpec ) IS "GR/N,GR/N"
   HBTEST ( o:colorSpec := "2222222222"                , o:colorSpec ) IS "GR+/N,GR+/N"
   HBTEST ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVWXYZ", o:colorSpec ) IS "N/N,N/N"
   HBTEST ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVW"   , o:colorSpec ) IS "N/U,N/U"
   HBTEST ( o:colorSpec := "N/W+"                      , o:colorSpec ) IS "N+/W,N+/W"
   HBTEST ( o:colorSpec := '"W"'+"/"+'"R"'             , o:colorSpec ) IS "W/R,W/R"
   HBTEST ( o:colorSpec := "'W'"+"/"+"'R'"             , o:colorSpec ) IS "W/R,W/R"
#endif
   /* "Samples" function tests (AMPM(), Days(), ElapTime(), ... ) */

   HBTEST AMPM( "" )                         IS "12 am"
   HBTEST AMPM( "HELLO" )                    IS "12LLO am"
   HBTEST AMPM( " 0:23:45" )                 IS "12:23:45 am"
   HBTEST AMPM( "00:23:45" )                 IS "12:23:45 am"
   HBTEST AMPM( " 5:23:45" )                 IS " 5:23:45 am"
   HBTEST AMPM( "05:23:45" )                 IS "05:23:45 am"
   HBTEST AMPM( "12:23:45" )                 IS "12:23:45 pm"
   HBTEST AMPM( "20:23:45" )                 IS " 8:23:45 pm"
   HBTEST AMPM( "24:23:45" )                 IS "12:23:45 am"
   HBTEST AMPM( "25:23:45" )                 IS "13:23:45 pm"
   HBTEST AMPM( "2" )                        IS "2 am"
   HBTEST AMPM( "02:23" )                    IS "02:23 am"
   HBTEST AMPM( "02:23:45.10" )              IS "02:23:45.10 am"

   HBTEST Days( 100000 )                     IS 1

   HBTEST ElapTime( "23:12:34", "12:34:57" ) IS "13:22:23"
   HBTEST ElapTime( "12:34:57", "23:12:34" ) IS "10:37:37"

   HBTEST LenNum( 10 )                       IS 2
   HBTEST LenNum( 10.9 )                     IS 4
   HBTEST LenNum( 10.90 )                    IS 5

   HBTEST Secs( "23:12:34" )                 IS 83554
   HBTEST Secs( "12:34:57" )                 IS 45297

   HBTEST TString( 1000 )                    IS "00:16:40"

#ifndef __XPP__
   HBTEST SoundEx()                          IS "0000"
   HBTEST SoundEx( 10 )                      IS "0000"
   HBTEST SoundEx( @scString )               IS "H400"
   HBTEST SoundEx( "" )                      IS "0000"
   HBTEST SoundEx( "Hm" )                    IS "H500"
   HBTEST SoundEx( "Smith" )                 IS "S530"
   HBTEST SoundEx( "Harbour" )               IS "H616"
   HBTEST SoundEx( "HARBOUR" )               IS "H616"
   HBTEST SoundEx( "Harpour" )               IS "H616"
   HBTEST SoundEx( "Hello" )                 IS "H400"
   HBTEST SoundEx( "Aardwaark" )             IS "A636"
   HBTEST SoundEx( "Ardwark" )               IS "A636"
   HBTEST SoundEx( "Bold" )                  IS "B430"
   HBTEST SoundEx( "Cold" )                  IS "C430"
   HBTEST SoundEx( "Colt" )                  IS "C430"
   HBTEST SoundEx( "C" + Chr( 0 ) + "olt" )  IS "C430"
   HBTEST SoundEx( "12345" )                 IS "0000"
   HBTEST SoundEx( Chr( 181 ) + Chr( 160 ) + "A" + Chr( 144 ) + Chr( 130 ) ) IS "A000"
#endif

   /* NATION functions (do not exist in 5.2e US) */

#ifdef __HARBOUR__
   #ifndef HB_CLP_UNDOC
      /* NOTE: Use the identical internal versions if Harbour
               was compiled without C5.x undocumented features.
               [vszakats] */
      #xtranslate NationMsg( [<x,...>] )  => __natMsg( <x> )
      #xtranslate IsAffirm( [<x,...>] )   => __natIsAffirm( <x> )
      #xtranslate IsNegative( [<x,...>] ) => __natIsNegative( <x> )
   #endif
#endif

#ifndef __XPP__
   HBTEST NationMsg()                     IS "Invalid argument"
#endif
   HBTEST NationMsg( "A" )                IS ""
   HBTEST NationMsg( -1 )                 IS ""  /* CA-Cl*pper bug: 5.3 may return trash. */
   HBTEST NationMsg( 0 )                  IS ""
   HBTEST NationMsg( 1 )                  IS "Database Files    # Records    Last Update     Size"
   HBTEST NationMsg( 2 )                  IS "Do you want more samples?"
   HBTEST NationMsg( 3 )                  IS "Page No."
   HBTEST NationMsg( 4 )                  IS "** Subtotal **"
   HBTEST NationMsg( 5 )                  IS "* Subsubtotal *"
   HBTEST NationMsg( 6 )                  IS "*** Total ***"
   HBTEST NationMsg( 7 )                  IS "Ins"
   HBTEST NationMsg( 8 )                  IS "   "
   HBTEST NationMsg( 9 )                  IS "Invalid date"
   HBTEST NationMsg( 10 )                 IS "Range: "
   HBTEST NationMsg( 11 )                 IS " - "
   HBTEST NationMsg( 12 )                 IS "Y/N"
   HBTEST NationMsg( 13 )                 IS "INVALID EXPRESSION"
   HBTEST NationMsg( 14 )                 IS ""  /* Bug in CA-Cl*pper 5.3a/b, it will return "ATSORT v1.3i x19 06/Mar/95" */
#ifndef __CLIPPER__ /* Causes GPF in CA-Cl*pper (5.2e International, 5.3b) */
   HBTEST NationMsg( 200 )                IS ""  /* Bug in CA-Cl*pper, it will return "74?" or other trash */
#endif

#ifndef __XPP__

/* These will cause a GPF in CA-Cl*pper (5.2e International, 5.3b) */
#ifndef __CLIPPER__
   HBTEST IsAffirm()                      IS .F.
   HBTEST IsAffirm( .F. )                 IS .F.
   HBTEST IsAffirm( .T. )                 IS .F.
   HBTEST IsAffirm( 0 )                   IS .F.
   HBTEST IsAffirm( 1 )                   IS .F.
#endif
   HBTEST IsAffirm( "" )                  IS .F.
   HBTEST IsAffirm( "I" )                 IS .F.
   HBTEST IsAffirm( "y" )                 IS .T.
   HBTEST IsAffirm( "Y" )                 IS .T.
   HBTEST IsAffirm( "yes" )               IS .T.
   HBTEST IsAffirm( "YES" )               IS .T.
   HBTEST IsAffirm( "n" )                 IS .F.
   HBTEST IsAffirm( "N" )                 IS .F.
   HBTEST IsAffirm( "no" )                IS .F.
   HBTEST IsAffirm( "NO" )                IS .F.

/* These will cause a GPF in CA-Cl*pper (5.2e International, 5.3b) */
#ifndef __CLIPPER__
   HBTEST IsNegative()                    IS .F.
   HBTEST IsNegative( .F. )               IS .F.
   HBTEST IsNegative( .T. )               IS .F.
   HBTEST IsNegative( 0 )                 IS .F.
   HBTEST IsNegative( 1 )                 IS .F.
#endif
   HBTEST IsNegative( "" )                IS .F.
   HBTEST IsNegative( "I" )               IS .F.
   HBTEST IsNegative( "y" )               IS .F.
   HBTEST IsNegative( "Y" )               IS .F.
   HBTEST IsNegative( "yes" )             IS .F.
   HBTEST IsNegative( "YES" )             IS .F.
   HBTEST IsNegative( "n" )               IS .T.
   HBTEST IsNegative( "N" )               IS .T.
   HBTEST IsNegative( "no" )              IS .T.
   HBTEST IsNegative( "NO" )              IS .T.

#endif /* __XPP__ */

   /* FOR/NEXT */

   HBTEST TFORNEXT( .F., .T., NIL )       IS "E 1 BASE 1086 Argument error (++) OS:0 #:0 A:1:L:.F. F:S"
   HBTEST TFORNEXT( .T., .F., NIL )       IS .T.
   HBTEST TFORNEXT( .F., .F., NIL )       IS "E 1 BASE 1086 Argument error (++) OS:0 #:0 A:1:L:.F. F:S"
   HBTEST TFORNEXT( 100, 101, NIL )       IS 102
   HBTEST TFORNEXT( "A", "A", NIL )       IS "E 1 BASE 1086 Argument error (++) OS:0 #:0 A:1:C:A F:S"
   HBTEST TFORNEXT( NIL, NIL, NIL )       IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST TFORNEXT( .F., .T.,   1 )       IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:L:.F.;N:1 F:S"
   HBTEST TFORNEXT( .F., .T.,  -1 )       IS .F.
   HBTEST TFORNEXT( .F., .T., .F. )       IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:L:.F.;N:0 F:S"
   HBTEST TFORNEXT( .T., .F.,   1 )       IS .T.
   HBTEST TFORNEXT( .T., .F.,  -1 )       IS "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:L:.T.;N:-1 F:S"
   HBTEST TFORNEXT( .T., .F., .T. )       IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:L:.T.;N:0 F:S"
   HBTEST TFORNEXT( 100, 101,   1 )       IS 102
   HBTEST TFORNEXT( 101, 100,  -1 )       IS 99
   HBTEST TFORNEXT( "A", "A", "A" )       IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:0 F:S"
   HBTEST TFORNEXT( "A", "B", "A" )       IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:0 F:S"
   HBTEST TFORNEXT( "B", "A", "A" )       IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:0 F:S"
   HBTEST TFORNEXT( NIL, NIL, NIL )       IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:U:NIL;U:NIL F:S"

   HBTEST TFORNEXTX(   1, 10,NIL )        IS "FTTTTTTTTTTT"
   HBTEST TFORNEXTX(  10,  1,NIL )        IS "FT"
   HBTEST TFORNEXTX(   1, 10,  1 )        IS "FTSSTSSTSSTSSTSSTSSTSSTSSTSSTSSTS"
   HBTEST TFORNEXTX(  10,  1, -1 )        IS "FTSSTSSTSSTSSTSSTSSTSSTSSTSSTSSTS"
   HBTEST TFORNEXTX(   1, 10, -1 )        IS "FTS"
   HBTEST TFORNEXTX(  10,  1,  1 )        IS "FTS"
   HBTEST TFORNEXTX(   1, 10,  4 )        IS "FTSSTSSTSSTS"
   HBTEST TFORNEXTX(  10,  1, -4 )        IS "FTSSTSSTSSTS"
   HBTEST TFORNEXTX(   1, 10, -4 )        IS "FTS"
   HBTEST TFORNEXTX(  10,  1,  4 )        IS "FTS"

   HBTEST TFORNEXTXF(   1, 10,NIL )       IS "F-9999T1T2T3T4T5T6T7T8T9T10T11R11"
   HBTEST TFORNEXTXF(  10,  1,NIL )       IS "F-9999T10R10"
   HBTEST TFORNEXTXF(   1, 10,  1 )       IS "F-9999T1S1S1T2S2S2T3S3S3T4S4S4T5S5S5T6S6S6T7S7S7T8S8S8T9S9S9T10S10S10T11S11R11"
   HBTEST TFORNEXTXF(  10,  1, -1 )       IS "F-9999T10S10S10T9S9S9T8S8S8T7S7S7T6S6S6T5S5S5T4S4S4T3S3S3T2S2S2T1S1S1T0S0R0"
   HBTEST TFORNEXTXF(   1, 10, -1 )       IS "F-9999T1S1R1"
   HBTEST TFORNEXTXF(  10,  1,  1 )       IS "F-9999T10S10R10"
   HBTEST TFORNEXTXF(   1, 10,  4 )       IS "F-9999T1S1S1T5S5S5T9S9S9T13S13R13"
   HBTEST TFORNEXTXF(  10,  1, -4 )       IS "F-9999T10S10S10T6S6S6T2S2S2T-2S-2R-2"
   HBTEST TFORNEXTXF(   1, 10, -4 )       IS "F-9999T1S1R1"
   HBTEST TFORNEXTXF(  10,  1,  4 )       IS "F-9999T10S10R10"

   /* Eval(), :Eval(), :EVAL */

   HBTEST Eval( NIL )                     IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Eval( 1 )                       IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:N:1 F:S"
   HBTEST Eval( @sbBlock )                IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:B:{||...} F:S"  /* CA-Cl*pper returns "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:{||...} F:S" */
   HBTEST Eval( {| p1 | p1 }, "A", "B" )  IS "A"
   HBTEST Eval( {| p1, p2 | p1 + p2 }, "A", "B" ) IS "AB"
#ifdef __HARBOUR__
   HBTEST Eval( {| p1, p2, p3 | HB_SYMBOL_UNUSED( p2 ), HB_SYMBOL_UNUSED( p3 ), p1 }, "A", "B" ) IS "A"
#else
   HBTEST Eval( {| p1, p2, p3 | p1 }, "A", "B" ) IS "A"
#endif
   HBTEST suNIL:Eval()                    IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST scString:Eval()                 IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:C:HELLO F:S"
   HBTEST snIntP:Eval()                   IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:N:10 F:S"
   HBTEST sdDateE:Eval()                  IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:D:0d00000000 F:S"
   HBTEST slFalse:Eval()                  IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:L:.F. F:S"
   HBTEST sbBlock:Eval()                  IS NIL
   HBTEST saArray:Eval()                  IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:A:{.[1].} F:S"
   HBTEST soObject:Eval()                 IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:O:ERROR Object F:S"
   HBTEST suNIL:Eval                      IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST scString:Eval                   IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:C:HELLO F:S"
   HBTEST snIntP:Eval                     IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:N:10 F:S"
   HBTEST sdDateE:Eval                    IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:D:0d00000000 F:S"
   HBTEST slFalse:Eval                    IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:L:.F. F:S"
   HBTEST sbBlock:Eval                    IS NIL
   HBTEST saArray:Eval                    IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:A:{.[1].} F:S"
   HBTEST soObject:Eval                   IS "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:O:ERROR Object F:S"

   /* hb_SToD() */

   /* For these tests in CA-Cl*pper 5.2e the following native hb_SToD() has
      been used (not the emulated one written in Clipper):

      CLIPPER hb_SToD( void )
      {
         // The length check is a fix to avoid buggy behaviour of _retds()
         _retds( ( ISCHAR( 1 ) && _parclen( 1 ) == 8 ) ? _parc( 1 ) : "        " );
      }
   */

#ifndef RT_NO_C
#ifndef __XPP__
   HBTEST hb_SToD()                       IS hb_SToD( "        " )
#endif
   HBTEST hb_SToD( 1 )                    IS hb_SToD( "        " )
   HBTEST hb_SToD( NIL )                  IS hb_SToD( "        " )
   HBTEST hb_SToD( "" )                   IS hb_SToD( "        " )
   HBTEST hb_SToD( "        " )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "       " )            IS hb_SToD( "        " )
   HBTEST hb_SToD( "         " )          IS hb_SToD( "        " )
   HBTEST hb_SToD( " 1234567" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "1999    " )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "99999999" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "99990101" )           IS hb_SToD( "99990101" )
   HBTEST hb_SToD( "19991301" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "19991241" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "01000101" )           IS hb_SToD( "01000101" )
   HBTEST hb_SToD( "29991231" )           IS hb_SToD( "29991231" )
   HBTEST hb_SToD( "19990905" )           IS hb_SToD( "19990905" )
   HBTEST hb_SToD( " 9990905" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "  990905" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "   90905" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "    0905" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "     905" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "      05" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "1 990905" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "19 90905" )           IS hb_SToD( "17490905" )
   HBTEST hb_SToD( "199 0905" )           IS hb_SToD( "19740905" )
   HBTEST hb_SToD( "1999 905" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "19990 05" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "199909 5" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "1999090 " )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "1999 9 5" )           IS hb_SToD( "        " )
   HBTEST hb_SToD( "1999090" + Chr( 0 ) ) IS hb_SToD( "        " )
#endif

   /* Descend() */

#ifndef __CLIPPER__ /* Bug in CA-Cl*pper, it returns undefined trash */
#ifndef __XPP__ /* Compiler time error */
   HBTEST Descend()                            IS NIL
#endif
#endif
   HBTEST Descend( NIL )                       IS NIL
   HBTEST Descend( { "A", "B" } )              IS NIL
#ifdef __HARBOUR__
   HBTEST Descend( @scString )                 IS Chr( 184 ) + Chr( 187 ) + Chr( 180 ) + Chr( 180 ) + Chr( 177 )  /* Bug in CA-Cl*pper, it will return NIL */

#endif
   HBTEST Descend( scString )                  IS Chr( 184 ) + Chr( 187 ) + Chr( 180 ) + Chr( 180 ) + Chr( 177 )
   HBTEST Descend( scString )                  IS Chr( 184 ) + Chr( 187 ) + Chr( 180 ) + Chr( 180 ) + Chr( 177 )
   HBTEST Descend( Descend( scString ) )       IS "HELLO"
   HBTEST Descend( .F. )                       IS .T.
   HBTEST Descend( .T. )                       IS .F.
   HBTEST Descend( 0 )                         IS 0.00
   HBTEST Descend( 1 )                         IS -1.00
   HBTEST Descend( -1 )                        IS 1.00
   HBTEST Descend( Descend( 256 ) )            IS 256.00
   HBTEST Descend( 2.0 )                       IS -2.00
   HBTEST Descend( 2.5 )                       IS -2.50
   HBTEST Descend( -100.35 )                   IS 100.35
   HBTEST Str( Descend( -740.354 ) )           IS "       740.35"
   HBTEST Str( Descend( -740.359 ) )           IS "       740.36"
   HBTEST Str( Descend( -740.354 ), 15, 5 )    IS "      740.35400"
   HBTEST Str( Descend( -740.359 ), 15, 5 )    IS "      740.35900"
   HBTEST Descend( 100000 )                    IS -100000.00
   HBTEST Descend( -100000 )                   IS 100000.00
   HBTEST Descend( "" )                        IS ""
   HBTEST Descend( Chr( 0 ) )                  IS Chr( 0 )
   HBTEST Descend( Chr( 0 ) + "Hello" )        IS Chr( 0 ) + Chr( 184 ) + Chr( 155 ) + Chr( 148 ) + Chr( 148 ) + Chr( 145 )
   HBTEST Descend( "Hello" + Chr( 0 ) + "wo" ) IS Chr( 184 ) + Chr( 155 ) + Chr( 148 ) + Chr( 148 ) + Chr( 145 ) + Chr( 0 ) + Chr( 137 ) + Chr( 145 )
   HBTEST Descend( hb_SToD( "" ) )             IS 5231808
   HBTEST Descend( hb_SToD( "01000101" ) )     IS 3474223
   HBTEST Descend( hb_SToD( "19801220" ) )     IS 2787214

#ifdef __HARBOUR__

   /* hb_ColorIndex() */

   HBTEST hb_ColorIndex()                     IS ""
   HBTEST hb_ColorIndex( "", -1 )             IS ""
   HBTEST hb_ColorIndex( "", 0 )              IS ""
   HBTEST hb_ColorIndex( "W/R", -1 )          IS ""
   HBTEST hb_ColorIndex( "W/R", 0 )           IS "W/R"
   HBTEST hb_ColorIndex( "W/R", 1 )           IS ""
   HBTEST hb_ColorIndex( "W/R", 2 )           IS ""
   HBTEST hb_ColorIndex( "W/R,GR/0", 0 )      IS "W/R"
   HBTEST hb_ColorIndex( "W/R,GR/0", 1 )      IS "GR/0"
   HBTEST hb_ColorIndex( "W/R,GR/0", 2 )      IS ""
   HBTEST hb_ColorIndex( "W/R,GR/0", 3 )      IS ""
   HBTEST hb_ColorIndex( "W/R, GR/0", 0 )     IS "W/R"
   HBTEST hb_ColorIndex( "W/R, GR/0", 1 )     IS "GR/0"
   HBTEST hb_ColorIndex( "W/R, GR/0", 2 )     IS ""
   HBTEST hb_ColorIndex( "W/R, GR/0", 3 )     IS ""
   HBTEST hb_ColorIndex( "W/R,GR/0 ", 0 )     IS "W/R"
   HBTEST hb_ColorIndex( "W/R,GR/0 ", 1 )     IS "GR/0"
   HBTEST hb_ColorIndex( "W/R,GR/0 ", 2 )     IS ""
   HBTEST hb_ColorIndex( "W/R, GR/0 ", 0 )    IS "W/R"
   HBTEST hb_ColorIndex( "W/R, GR/0 ", 1 )    IS "GR/0"
   HBTEST hb_ColorIndex( "W/R, GR/0 ", 2 )    IS ""
   HBTEST hb_ColorIndex( "W/R, GR/0 ,", 0 )   IS "W/R"
   HBTEST hb_ColorIndex( "W/R, GR/0 ,", 1 )   IS "GR/0"
   HBTEST hb_ColorIndex( "W/R, GR/0 ,", 2 )   IS ""
   HBTEST hb_ColorIndex( " W/R, GR/0 ,", 0 )  IS "W/R"
   HBTEST hb_ColorIndex( " W/R, GR/0 ,", 1 )  IS "GR/0"
   HBTEST hb_ColorIndex( " W/R, GR/0 ,", 2 )  IS ""
   HBTEST hb_ColorIndex( " W/R , GR/0 ,", 0 ) IS "W/R"
   HBTEST hb_ColorIndex( " W/R , GR/0 ,", 1 ) IS "GR/0"
   HBTEST hb_ColorIndex( " W/R , GR/0 ,", 2 ) IS ""
   HBTEST hb_ColorIndex( " W/R ,   ,", 1 )    IS ""
   HBTEST hb_ColorIndex( " W/R ,,", 1 )       IS ""
   HBTEST hb_ColorIndex( ",,", 0 )            IS ""
   HBTEST hb_ColorIndex( ",,", 1 )            IS ""
   HBTEST hb_ColorIndex( ",,", 2 )            IS ""
   HBTEST hb_ColorIndex( ",  ,", 2 )          IS ""

#endif

#ifndef __XPP__

   /* FKMax(), FKLabel() */

   HBTEST FKMax()                         IS 40
   HBTEST FKMax( 1 )                      IS 40
#ifdef __HARBOUR__
   HBTEST FKLabel()                       IS ""  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:U:NIL;N:40 F:S" */
   HBTEST FKLabel( NIL )                  IS ""  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:U:NIL;N:40 F:S" */
   HBTEST FKLabel( "A" )                  IS ""  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:A;N:40 F:S" */
#endif
   HBTEST FKLabel( -1 )                   IS ""
   HBTEST FKLabel( 0 )                    IS ""
   HBTEST FKLabel( 1 )                    IS "F1"
   HBTEST FKLabel( 25 )                   IS "F25"
   HBTEST FKLabel( 40 )                   IS "F40"
   HBTEST FKLabel( 41 )                   IS ""

#endif /* __XPP__ */

   /* NOTE: BIN2*() functions are quite untable in CA-Cl*pper when the passed
      parameter is smaller than the required length. */

   /* Bin2I() */

#ifndef __CLIPPER__
#ifndef __XPP__
   HBTEST Bin2I()                          IS 0  /* Bug in CA-Cl*pper, this causes a GPF */
#endif
   HBTEST Bin2I( 100 )                     IS 0  /* Bug in CA-Cl*pper, this causes a GPF */
   HBTEST Bin2I( "" )                      IS 0  /* Bug in CA-Cl*pper, it will return trash */
#endif
   HBTEST Bin2I( "AB" )                    IS 16961
   HBTEST Bin2I( "BA" )                    IS 16706
   HBTEST Bin2I( Chr( 255 ) )              IS 255
   HBTEST Bin2I( Chr( 255 ) + Chr( 255 ) ) IS -1
   HBTEST Bin2I( Chr( 0 ) )                IS 0
   HBTEST Bin2I( Chr( 0 ) + Chr( 0 ) )     IS 0
   HBTEST Bin2I( "A" )                     IS 65
   HBTEST Bin2I( "ABC" )                   IS 16961

   /* Bin2W() */

#ifndef __CLIPPER__
#ifndef __XPP__
   HBTEST Bin2W()                          IS 0  /* Bug in CA-Cl*pper, this causes a GPF */
#endif
   HBTEST Bin2W( 100 )                     IS 0  /* Bug in CA-Cl*pper, this causes a GPF */
   HBTEST Bin2W( "" )                      IS 0  /* Bug in CA-Cl*pper, it will return trash */
#endif
   HBTEST Bin2W( "AB" )                    IS 16961
   HBTEST Bin2W( "BA" )                    IS 16706
   HBTEST Bin2W( Chr( 255 ) )              IS 255
   HBTEST Bin2W( Chr( 255 ) + Chr( 255 ) ) IS 65535
   HBTEST Bin2W( Chr( 0 ) )                IS 0
   HBTEST Bin2W( Chr( 0 ) + Chr( 0 ) )     IS 0
   HBTEST Bin2W( "A" )                     IS 65
   HBTEST Bin2W( "ABC" )                   IS 16961

   /* Bin2L() */

#ifndef __CLIPPER__
#ifndef __XPP__
   HBTEST Bin2L()                                                    IS 0  /* Bug in CA-Cl*pper, this causes a GPF */
#endif
   HBTEST Bin2L( 100 )                                               IS 0  /* Bug in CA-Cl*pper, this causes a GPF */
   HBTEST Bin2L( "" )                                                IS 0  /* Bug in CA-Cl*pper, it will return trash */
#endif
   HBTEST Bin2L( "ABCD" )                                            IS 1145258561
   HBTEST Bin2L( "DCBA" )                                            IS 1094861636
#ifndef __CLIPPER__
   HBTEST Bin2L( Chr( 255 ) )                                        IS 255  /* Bug in CA-Cl*pper, it will return trash */
#endif
   HBTEST Bin2L( Chr( 255 ) + Chr( 255 ) + Chr( 255 ) )              IS 16777215
   HBTEST Bin2L( Chr( 255 ) + Chr( 255 ) + Chr( 255 ) + Chr( 255 ) ) IS -1
   HBTEST Bin2L( Chr( 0 ) + Chr( 0 ) + Chr( 0 ) )                    IS 0
   HBTEST Bin2L( Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 ) )         IS 0
   HBTEST Bin2L( "ABC" )                                             IS 4407873
   HBTEST Bin2L( "ABCDE" )                                           IS 1145258561

   /* I2Bin() */

#ifndef __XPP__
   HBTEST I2Bin()                         IS Chr( 0 ) + Chr( 0 )
#endif
   HBTEST I2Bin( ""    )                  IS Chr( 0 ) + Chr( 0 )
   HBTEST I2Bin( 0     )                  IS Chr( 0 ) + Chr( 0 )
   HBTEST I2Bin( 16961 )                  IS "AB"
   HBTEST I2Bin( 16706 )                  IS "BA"
   HBTEST I2Bin( 255   )                  IS Chr( 255 ) + Chr( 0 )
   HBTEST I2Bin( -1    )                  IS Chr( 255 ) + Chr( 255 )
   HBTEST I2Bin( 0     )                  IS Chr( 0 ) + Chr( 0 )
   HBTEST I2Bin( 1     )                  IS Chr( 1 ) + Chr( 0 )
   HBTEST I2Bin( 65    )                  IS "A" + Chr( 0 )
   HBTEST I2Bin( 16961 )                  IS "AB"

   /* L2Bin() */

#ifndef __XPP__
   HBTEST L2Bin()                         IS Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
#endif
   HBTEST L2Bin( ""         )             IS Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   HBTEST L2Bin( 0          )             IS Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   HBTEST L2Bin( 1145258561 )             IS "ABCD"
   HBTEST L2Bin( 1094861636 )             IS "DCBA"
   HBTEST L2Bin( 255        )             IS Chr( 255 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   HBTEST L2Bin( 16777215   )             IS Chr( 255 ) + Chr( 255 ) + Chr( 255 ) + Chr( 0 )
   HBTEST L2Bin( -1         )             IS Chr( 255 ) + Chr( 255 ) + Chr( 255 ) + Chr( 255 )
   HBTEST L2Bin( 0          )             IS Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   HBTEST L2Bin( 1          )             IS Chr( 1 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   HBTEST L2Bin( 4407873    )             IS "ABC" + Chr( 0 )
   HBTEST L2Bin( 1145258561 )             IS "ABCD"

#ifndef __XPP__

   /* __CopyFile() */

   FClose( FCreate( "$$COPYFR.TMP" ) )

   /* NOTE: Cannot yet test the return value of the function on a DEFAULT-ed
            failure. */

   HBTEST __CopyFile( "$$COPYFR.TMP" )                 IS "E 1 BASE 2010 Argument error (__COPYFILE) OS:0 #:0 A:1:C:$$COPYFR.TMP "
   HBTEST __CopyFile( "$$COPYFR.TMP", "$$COPYTO.TMP" ) IS NIL
   HBTEST __CopyFile( "NOT_HERE.$$$", "$$COPYTO.TMP" ) IS "E 21 BASE 2012 Open error <NOT_HERE.$$$> OS:2 #:1 F:DR"
   HBTEST __CopyFile( "$$COPYFR.TMP", BADFNAME() )     IS "E 20 BASE 2012 Create error <" + BADFNAME() + "> OS:2 #:1 F:DR"

   FErase( "$$COPYFR.TMP" )
   FErase( "$$COPYTO.TMP" )

#endif /* __XPP__ */

#ifndef __XPP__

   /* SORT /D */

   HBTEST RDD_SORT_D() IS "7.00 6.00 5.00 -5.00 -5.12"

   /* __Run() */

   /* NOTE: Only error cases are tested. */

   HBTEST __Run()                         IS NIL
   HBTEST __Run( NIL )                    IS NIL
   HBTEST __Run( 10 )                     IS NIL

#endif /* __XPP__ */

   /* MemVarBlock() */

   HBTEST MemVarBlock()                   IS NIL
   HBTEST MemVarBlock( NIL )              IS NIL
   HBTEST MemVarBlock( 100 )              IS NIL
   HBTEST MemVarBlock( "mxNotHere" )      IS NIL
   HBTEST MemVarBlock( "mcString" )       IS "{||...}"

   /* Defines for HardCR() and MemoTran() */

   #define SO Chr( 141 )
   #define NU Chr( 0 )
   #define HT Chr( 9 )
   #define LF Chr( 10 )
   #define CR Chr( 13 )

   /* HardCR() */

#ifndef __XPP__
   HBTEST HardCR()                                              IS ""
#endif
   HBTEST HardCR( NIL )                                         IS ""
   HBTEST HardCR( 100 )                                         IS ""
#ifdef __HARBOUR__
   HBTEST HardCR( @scString )                                   IS "HELLO"  /* Bug in CA-Cl*pper, it will return "" */
#endif
   HBTEST HardCR( "H"+SO+LF+"P"+SO+LF+"W"+SO+"M" )              IS "H" + Chr( 13 ) + Chr( 10 ) + "P" + Chr( 13 ) + Chr( 10 ) + "W" + Chr( 141 ) + "M"
   HBTEST HardCR( "H"+NU+"B"+SO+LF+NU+"P"+SO+LF+"W"+SO+"M"+NU ) IS "H" + Chr( 0 ) + "B" + Chr( 13 ) + Chr( 10 ) + Chr( 0 ) + "P" + Chr( 13 ) + Chr( 10 ) + "W" + Chr( 141 ) + "M" + Chr( 0 )

   /* MemoTran() */

#ifndef __XPP__
   HBTEST MemoTran()                                                         IS ""
#endif
   HBTEST MemoTran( NIL )                                                    IS ""
   HBTEST MemoTran( 100 )                                                    IS ""
   HBTEST MemoTran( 100, "1", "2" )                                          IS ""
#ifdef __HARBOUR__
   HBTEST MemoTran( @scString )                                              IS "HELLO"  /* Bug in CA-Cl*pper, it will return "" */
#endif
   HBTEST MemoTran( "H"+SO+LF+"P"+CR+LF+"M" )                                IS "H P;M"
   HBTEST MemoTran( "H"+NU+"O"+SO+LF+"P"+CR+LF+"M"+NU+"I" )                  IS "H" + Chr( 0 ) + "O P;M" + Chr( 0 ) + "I"
   HBTEST MemoTran( "M"+CR+"s"+CR+LF+"w"+SO+"w"+SO+LF+"h"+CR )               IS "M" + Chr( 13 ) + "s;w" + Chr( 141 ) + "w h" + Chr( 13 )
   HBTEST MemoTran( "M"+CR+"s"+CR+LF+"w"+SO+"w"+SO+LF+"h"+CR, "111", "222" ) IS "M" + Chr( 13 ) + "s1w" + Chr( 141 ) + "w2h" + Chr( 13 )
   HBTEST MemoTran( "M"+CR+"s"+CR+LF+"w"+SO+"w"+SO+LF+"h"+CR, "", "" )       IS "M" + Chr( 13 ) + "s" + Chr( 0 ) + "w" + Chr( 141 ) + "w" + Chr( 0 ) + "h" + Chr( 13 )

   /* MemoWrit()/MemoRead() */

#ifndef __XPP__
   HBTEST MemoWrit()                            IS .F.
   HBTEST MemoWrit( "$$MEMOFI.TMP" )            IS .F.
#endif
   HBTEST MemoWrit( "$$MEMOFI.TMP", "" )        IS .T.
   HBTEST MemoRead( "$$MEMOFI.TMP" )            IS ""
   HBTEST MemoWrit( "$$MEMOFI.TMP", scStringZ ) IS .T.
   HBTEST MemoRead( "$$MEMOFI.TMP" )            IS "A" + Chr( 0 ) + "B"
   HBTEST MemoWrit( "$$MEMOFI.TMP", Chr( 26 ) ) IS .T.
   HBTEST MemoRead( "$$MEMOFI.TMP" )            IS Chr( 26 )
   HBTEST MemoWrit( "$$MEMOFI.TMP", scStringW ) IS .T.
   HBTEST MemoRead( "$$MEMOFI.TMP" )            IS Chr( 13 ) + Chr( 10 ) + Chr( 141 ) + Chr( 10 ) + Chr( 9 )
   HBTEST MemoWrit( BADFNAME2()   , scStringZ ) IS .F.
#ifndef __XPP__
   HBTEST MemoRead()                            IS ""
#endif
   HBTEST MemoRead( BADFNAME2() )               IS ""

   FErase( "$$MEMOFI.TMP" )

#ifdef __HARBOUR__

   /* hb_FNameSplit(), hb_FNameMerge() */

   HBTEST TESTFNAME( ""                            ) IS ";;;;"
   HBTEST TESTFNAME( "                           " ) IS ";;;;"
#ifdef __PLATFORM__UNIX
   HBTEST TESTFNAME( ":                          " ) IS ":;;:;;"
#else
   HBTEST TESTFNAME( ":                          " ) IS ":;:;;;"
#endif
   HBTEST TESTFNAME( "C:/work/hello              " ) IS "C:/work/hello;C:/work/;hello;;"
   HBTEST TESTFNAME( "C:/work/hello              " ) IS "C:/work/hello;C:/work/;hello;;"
   HBTEST TESTFNAME( "C:/work/hello              " ) IS "C:/work/hello;C:/work/;hello;;"
   HBTEST TESTFNAME( "C:/work/hello.             " ) IS "C:/work/hello.;C:/work/;hello;.;"
   HBTEST TESTFNAME( "C:/work/hello.prg          " ) IS "C:/work/hello.prg;C:/work/;hello;.prg;"
   HBTEST TESTFNAME( "C:/work/hello/             " ) IS "C:/work/hello/;C:/work/hello/;;;"
   HBTEST TESTFNAME( "C:/work/hello/.prg         " ) IS "C:/work/hello/.prg;C:/work/hello/;.prg;;"
   HBTEST TESTFNAME( "C:/work/hello/a.prg        " ) IS "C:/work/hello/a.prg;C:/work/hello/;a;.prg;"
   HBTEST TESTFNAME( "C:/work/hello/a.b.prg      " ) IS "C:/work/hello/a.b.prg;C:/work/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "C:work/hello               " ) IS "C:work/hello;C:work/;hello;;"
   HBTEST TESTFNAME( "C:work/hello.              " ) IS "C:work/hello.;C:work/;hello;.;"
   HBTEST TESTFNAME( "C:work/hello.prg           " ) IS "C:work/hello.prg;C:work/;hello;.prg;"
   HBTEST TESTFNAME( "C:work/hello/              " ) IS "C:work/hello/;C:work/hello/;;;"
   HBTEST TESTFNAME( "C:work/hello/.prg          " ) IS "C:work/hello/.prg;C:work/hello/;.prg;;"
   HBTEST TESTFNAME( "C:work/hello/a.prg         " ) IS "C:work/hello/a.prg;C:work/hello/;a;.prg;"
   HBTEST TESTFNAME( "C:work/hello/a.b.prg       " ) IS "C:work/hello/a.b.prg;C:work/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "C:/work.old/hello          " ) IS "C:/work.old/hello;C:/work.old/;hello;;"
   HBTEST TESTFNAME( "C:/work.old/hello.         " ) IS "C:/work.old/hello.;C:/work.old/;hello;.;"
   HBTEST TESTFNAME( "C:/work.old/hello.prg      " ) IS "C:/work.old/hello.prg;C:/work.old/;hello;.prg;"
   HBTEST TESTFNAME( "C:/work.old/hello/         " ) IS "C:/work.old/hello/;C:/work.old/hello/;;;"
   HBTEST TESTFNAME( "C:/work.old/hello/.prg     " ) IS "C:/work.old/hello/.prg;C:/work.old/hello/;.prg;;"
   HBTEST TESTFNAME( "C:/work.old/hello/a.prg    " ) IS "C:/work.old/hello/a.prg;C:/work.old/hello/;a;.prg;"
   HBTEST TESTFNAME( "C:/work.old/hello/a.b.prg  " ) IS "C:/work.old/hello/a.b.prg;C:/work.old/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "C:work.old/hello           " ) IS "C:work.old/hello;C:work.old/;hello;;"
   HBTEST TESTFNAME( "C:work.old/hello.          " ) IS "C:work.old/hello.;C:work.old/;hello;.;"
   HBTEST TESTFNAME( "C:work.old/hello.prg       " ) IS "C:work.old/hello.prg;C:work.old/;hello;.prg;"
   HBTEST TESTFNAME( "C:work.old/hello/          " ) IS "C:work.old/hello/;C:work.old/hello/;;;"
   HBTEST TESTFNAME( "C:work.old/hello/.prg      " ) IS "C:work.old/hello/.prg;C:work.old/hello/;.prg;;"
   HBTEST TESTFNAME( "C:work.old/hello/a.prg     " ) IS "C:work.old/hello/a.prg;C:work.old/hello/;a;.prg;"
   HBTEST TESTFNAME( "C:work.old/hello/a.b.prg   " ) IS "C:work.old/hello/a.b.prg;C:work.old/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "C:.old/hello               " ) IS "C:.old/hello;C:.old/;hello;;"
   HBTEST TESTFNAME( "C:.old/hello.              " ) IS "C:.old/hello.;C:.old/;hello;.;"
   HBTEST TESTFNAME( "C:.old/hello.prg           " ) IS "C:.old/hello.prg;C:.old/;hello;.prg;"
   HBTEST TESTFNAME( "C:.old/hello/              " ) IS "C:.old/hello/;C:.old/hello/;;;"
   HBTEST TESTFNAME( "C:.old/hello/.prg          " ) IS "C:.old/hello/.prg;C:.old/hello/;.prg;;"
   HBTEST TESTFNAME( "C:.old/hello/a.prg         " ) IS "C:.old/hello/a.prg;C:.old/hello/;a;.prg;"
   HBTEST TESTFNAME( "C:.old/hello/a.b.prg       " ) IS "C:.old/hello/a.b.prg;C:.old/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "//server/work/hello        " ) IS "//server/work/hello;//server/work/;hello;;"
   HBTEST TESTFNAME( "//server/work/hello.       " ) IS "//server/work/hello.;//server/work/;hello;.;"
   HBTEST TESTFNAME( "//server/work/hello.prg    " ) IS "//server/work/hello.prg;//server/work/;hello;.prg;"
   HBTEST TESTFNAME( "//server/work/hello/       " ) IS "//server/work/hello/;//server/work/hello/;;;"
   HBTEST TESTFNAME( "//server/work/hello/.prg   " ) IS "//server/work/hello/.prg;//server/work/hello/;.prg;;"
   HBTEST TESTFNAME( "//server/work/hello/a.prg  " ) IS "//server/work/hello/a.prg;//server/work/hello/;a;.prg;"
   HBTEST TESTFNAME( "//server/work/hello/a.b.prg" ) IS "//server/work/hello/a.b.prg;//server/work/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "/server/work/hello         " ) IS "/server/work/hello;/server/work/;hello;;"
   HBTEST TESTFNAME( "/server/work/hello.        " ) IS "/server/work/hello.;/server/work/;hello;.;"
   HBTEST TESTFNAME( "/server/work/hello.prg     " ) IS "/server/work/hello.prg;/server/work/;hello;.prg;"
   HBTEST TESTFNAME( "/server/work/hello/        " ) IS "/server/work/hello/;/server/work/hello/;;;"
   HBTEST TESTFNAME( "/server/work/hello/.prg    " ) IS "/server/work/hello/.prg;/server/work/hello/;.prg;;"
   HBTEST TESTFNAME( "/server/work/hello/a.prg   " ) IS "/server/work/hello/a.prg;/server/work/hello/;a;.prg;"
   HBTEST TESTFNAME( "/server/work/hello/a.b.prg " ) IS "/server/work/hello/a.b.prg;/server/work/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "C:/hello                   " ) IS "C:/hello;C:/;hello;;"
   HBTEST TESTFNAME( "C:/hello.                  " ) IS "C:/hello.;C:/;hello;.;"
   HBTEST TESTFNAME( "C:/hello.prg               " ) IS "C:/hello.prg;C:/;hello;.prg;"
   HBTEST TESTFNAME( "C:/hello/                  " ) IS "C:/hello/;C:/hello/;;;"
   HBTEST TESTFNAME( "C:/hello/.prg              " ) IS "C:/hello/.prg;C:/hello/;.prg;;"
   HBTEST TESTFNAME( "C:/hello/a.prg             " ) IS "C:/hello/a.prg;C:/hello/;a;.prg;"
   HBTEST TESTFNAME( "C:/hello/a.b.prg           " ) IS "C:/hello/a.b.prg;C:/hello/;a.b;.prg;"
#ifdef __PLATFORM__UNIX
   HBTEST TESTFNAME( "C:hello                    " ) IS "C:hello;;C:hello;;"
   HBTEST TESTFNAME( "C:hello.                   " ) IS "C:hello.;;C:hello;.;"
   HBTEST TESTFNAME( "C:hello.prg                " ) IS "C:hello.prg;;C:hello;.prg;"
#else
   HBTEST TESTFNAME( "C:hello                    " ) IS "C:hello;C:;hello;;"
   HBTEST TESTFNAME( "C:hello.                   " ) IS "C:hello.;C:;hello;.;"
   HBTEST TESTFNAME( "C:hello.prg                " ) IS "C:hello.prg;C:;hello;.prg;"
#endif
   HBTEST TESTFNAME( "C:hello/                   " ) IS "C:hello/;C:hello/;;;"
   HBTEST TESTFNAME( "C:hello/.prg               " ) IS "C:hello/.prg;C:hello/;.prg;;"
   HBTEST TESTFNAME( "C:hello/a.prg              " ) IS "C:hello/a.prg;C:hello/;a;.prg;"
   HBTEST TESTFNAME( "C:hello/a.b.prg            " ) IS "C:hello/a.b.prg;C:hello/;a.b;.prg;"
   HBTEST TESTFNAME( "//hello                    " ) IS "//hello;//;hello;;"
   HBTEST TESTFNAME( "//hello.                   " ) IS "//hello.;//;hello;.;"
   HBTEST TESTFNAME( "//hello.prg                " ) IS "//hello.prg;//;hello;.prg;"
   HBTEST TESTFNAME( "//hello/                   " ) IS "//hello/;//hello/;;;"
   HBTEST TESTFNAME( "//.prg                     " ) IS "//.prg;//;.prg;;"
   HBTEST TESTFNAME( "//a.prg                    " ) IS "//a.prg;//;a;.prg;"
   HBTEST TESTFNAME( "//a.b.prg                  " ) IS "//a.b.prg;//;a.b;.prg;"
   HBTEST TESTFNAME( "/hello                     " ) IS "/hello;/;hello;;"
   HBTEST TESTFNAME( "/hello.                    " ) IS "/hello.;/;hello;.;"
   HBTEST TESTFNAME( "/hello.prg                 " ) IS "/hello.prg;/;hello;.prg;"
   HBTEST TESTFNAME( "/hello/                    " ) IS "/hello/;/hello/;;;"
   HBTEST TESTFNAME( "/hello/.prg                " ) IS "/hello/.prg;/hello/;.prg;;"
   HBTEST TESTFNAME( "/hello/a.prg               " ) IS "/hello/a.prg;/hello/;a;.prg;"
   HBTEST TESTFNAME( "/hello/a.b.prg             " ) IS "/hello/a.b.prg;/hello/;a.b;.prg;"
   HBTEST TESTFNAME( "hello                      " ) IS "hello;;hello;;"
   HBTEST TESTFNAME( "hello.                     " ) IS "hello.;;hello;.;"
   HBTEST TESTFNAME( "hello.prg                  " ) IS "hello.prg;;hello;.prg;"
   HBTEST TESTFNAME( "hello/                     " ) IS "hello/;hello/;;;"
   HBTEST TESTFNAME( ".prg                       " ) IS ".prg;;.prg;;"
   HBTEST TESTFNAME( "a.prg                      " ) IS "a.prg;;a;.prg;"
   HBTEST TESTFNAME( "a.b.prg                    " ) IS "a.b.prg;;a.b;.prg;"
   HBTEST TESTFNAME( "                           " ) IS ";;;;"
   HBTEST TESTFNAME( "/                          " ) IS "/;/;;;"
   HBTEST TESTFNAME( "//                         " ) IS "//;//;;;"
   HBTEST TESTFNAME( "C                          " ) IS "C;;C;;"
#ifdef __PLATFORM__UNIX
   HBTEST TESTFNAME( "C:                         " ) IS "C:;;C:;;"
#else
   HBTEST TESTFNAME( "C:                         " ) IS "C:;C:;;;"
#endif
   HBTEST TESTFNAME( "C:/                        " ) IS "C:/;C:/;;;"
   HBTEST TESTFNAME( "C://                       " ) IS "C://;C://;;;"

#endif

   /* MLCToPos() */

#ifdef __HARBOUR__
   cEOL := Set( _SET_EOL, Chr( 13 ) + Chr( 10 ) )
#endif

   HBTEST MLCToPos( 'A message from me', 10, 2, 0 )                                IS 11
   HBTEST MLCToPos( 'A message from me', 5, 2, 0, 4, .F. )                         IS  6
   HBTEST MLCToPos( 'A' + HT + 'message' + HT + 'from' + HT + 'me', 10, 2, 0, 8 )  IS  3
   HBTEST MLCToPos( 'abcd efg hijk lm nopqr stu vwxyz', 5, 3, 0 )                  IS 10
   HBTEST MLCToPos( 'abcd efg hijk lm nopqr stu vwxyz', 8, 2, 0 )                  IS 10
   HBTEST MLCToPos( 'abcd efg hijk lm nopqr stu vwxyz', 8, 2, 0,, .F. )            IS  9
   HBTEST MLCToPos( 'A message from our me', 9, 2, 0 )                             IS 11
   HBTEST MLCToPos( 'A message  from our me', 9, 2, 0 )                            IS 11
   HBTEST MLCToPos( 'A message' + Chr( 9 ) + 'from me', 10, 2, 0 )                 IS 11
   HBTEST MLCToPos( 'A message from me', 9, 2, 0,, .F. )                           IS 10
   HBTEST MLCToPos( 'A message  from me', 9, 2, 0,, .F. )                          IS 10
   HBTEST MLCToPos( 'A message' + Chr( 141 ) + 'from me', 10, 2, 0 )               IS  3
   HBTEST MLCToPos( 'A message' + Chr( 141 ) + 'from me', 9, 2, 0 )                IS  3
   HBTEST MLCToPos( 'A message' + Chr( 141 ) + 'from me', 10, 2, 0,, .F. )         IS 11
   HBTEST MLCToPos( 'A message' + Chr( 141 ) + 'from me', 9, 2, 0,, .F. )          IS 10
   HBTEST MLCToPos( ' message from me', 10, 1, 0 )                                 IS  1
   HBTEST MLCToPos( ' message from me', 10, 1, 8 )                                 IS  9
   HBTEST MLCToPos( ' message from me', 10, 1, 9 )                                 IS 10
   HBTEST MLCToPos( ' message from me', 10, 1, 10 )                                IS 11
   HBTEST MLCToPos( ' message from me', 10, 1, 11 )                                IS 12
   HBTEST MLCToPos( ' message from me', 10, 1, 360 )                               IS 17
   HBTEST MLCToPos( ' message from me', 10, 1, 0,, .F. )                           IS  1
   HBTEST MLCToPos( ' message from me', 10, 1, 8,, .F. )                           IS  9
   HBTEST MLCToPos( ' message from me', 10, 1, 9,, .F. )                           IS 10
   HBTEST MLCToPos( ' message from me', 10, 1, 10,, .F. )                          IS 11
   HBTEST MLCToPos( ' message from me', 10, 1, 11,, .F. )                          IS 12
   HBTEST MLCToPos( ' message from me', 10, 1, 360,, .F. )                         IS 17
   HBTEST MLCToPos( ' message' + Chr( 9 ) + 'from me', 10, 1, 11,, .T. )           IS  9
   HBTEST MLCToPos( ' message' + Chr( 9 ) + 'from me', 10, 1, 11,, .F. )           IS  9
   HBTEST MLCToPos( ' message' + Chr( 9 ) + 'from me', 10, 2, 11 )                 IS 17
   HBTEST MLCToPos( ' message' + Chr( 9 ) + 'from me', 10, 1, 15,, .T. )           IS 13
   HBTEST MLCToPos( ' message' + Chr( 9 ) + 'from me', 10, 1, 15,, .F. )           IS 13
   HBTEST MLCToPos( CR + LF + ' message' + HT + 'from me', 10, 1, 15,, .F. )       IS  1
   HBTEST MLCToPos( CR + LF + ' message' + HT + 'from me', 10, 1, 15,, .T. )       IS  1
   HBTEST MLCToPos( 'A ' + Chr( 13 ) + Chr( 10 ) + 'message from me', 9, 2, 0 )    IS  5
   HBTEST MLCToPos( 'A ' + Chr( 141 ) + Chr( 10 ) + 'message from me', 9, 2, 0 )   IS 13
   HBTEST MLCToPos( 'A' + Chr( 141 ) + Chr( 10 ) + 'message from me', 9, 2, 0 )    IS 12
   HBTEST MLCToPos( 'A' + Chr( 141 ) + 'message from me', 9, 2, 0 )                IS 11
   HBTEST MLCToPos( 'A' + Chr( 13 ) + 'message from me', 9, 2, 0 )                 IS 11
   HBTEST MLCToPos( 'A' + Chr( 10 ) + 'message from me', 9, 2, 0 )                 IS 11
   HBTEST MLCToPos( 'A ' + Chr( 13 ) + 'message from me', 9, 2, 0 )                IS  3
   HBTEST MLCToPos( 'A ' + Chr( 10 ) + 'message from me', 9, 2, 0 )                IS  3
   HBTEST MLCToPos( 'A message from me', 10, 7, 0 )                                IS 18
   HBTEST MLCToPos( , , ,  )                                                       IS  1
   HBTEST MLCToPos( , .T., ,  )                                                    IS  1

#ifdef __HARBOUR__
   Set( _SET_EOL, cEOL )
#endif

   RETURN

#ifdef __HARBOUR__

PROCEDURE Main_OPOVERL()

   LOCAL oString := HB_TString()

   oString:cValue := "Hello"

   HBTEST oString =  "Hello"        IS .T.
   HBTEST oString == "Hello"        IS .T.
   HBTEST oString != "Hello"        IS .F.
   HBTEST oString <> "Hello"        IS .F.
   HBTEST oString #  "Hello"        IS .F.
   HBTEST oString $  "Hello"        IS .T.
   HBTEST oString <  "Hello"        IS .F.
   HBTEST oString <= "Hello"        IS .T.
   HBTEST oString <  "Hello"        IS .F.
   HBTEST oString <= "Hello"        IS .T.
   HBTEST oString +  "Hello"        IS "HelloHello"
   HBTEST oString -  "Hello"        IS "HelloHello"
   HBTEST oString * 3               IS "HelloHelloHello"
   HBTEST oString / 2               IS "He"
   HBTEST oString % "TST"           IS "Hello % TST"
   HBTEST oString ^ "TST"           IS "Hello ^ TST"
   HBTEST oString ** "TST"          IS "Hello ^ TST"
   IF ! TEST_OPT_Z()
   HBTEST oString .AND. "TST"       IS "Hello AND TST"
   HBTEST oString .OR. "TST"        IS "Hello OR TST"
   ENDIF
   HBTEST .NOT. oString             IS Chr( 184 ) + Chr( 155 ) + Chr( 148 ) + Chr( 148 ) + Chr( 145 )
   HBTEST ! oString                 IS Chr( 184 ) + Chr( 155 ) + Chr( 148 ) + Chr( 148 ) + Chr( 145 )
   HBTEST oString++                 IS "HB_TSTRING Object"
   HBTEST oString:cValue            IS "Hello "
   HBTEST oString--                 IS "HB_TSTRING Object"
   HBTEST oString:cValue            IS "Hello"

   RETURN

STATIC FUNCTION HB_TString()

   STATIC oClass

   IF oClass == NIL
      oClass = HBClass():New( "HB_TSTRING" )

      oClass:AddData( "cValue" )

      oClass:AddInline( "="    , {| self, cTest | ::cValue =  cTest } )
      oClass:AddInline( "=="   , {| self, cTest | ::cValue == cTest } )
      oClass:AddInline( "!="   , {| self, cTest | ::cValue != cTest } )
      oClass:AddInline( "<"    , {| self, cTest | ::cValue <  cTest } )
      oClass:AddInline( "<="   , {| self, cTest | ::cValue <= cTest } )
      oClass:AddInline( ">"    , {| self, cTest | ::cValue >  cTest } )
      oClass:AddInline( ">="   , {| self, cTest | ::cValue >= cTest } )
      oClass:AddInline( "+"    , {| self, cTest | ::cValue +  cTest } )
      oClass:AddInline( "-"    , {| self, cTest | ::cValue -  cTest } )
      oClass:AddInline( "++"   , {| self        | ::cValue += " ", self } )
      oClass:AddInline( "--"   , {| self        | iif( Len( ::cValue ) > 0, ::cValue := Left( ::cValue, Len( ::cValue ) - 1 ), ), self } )
      oClass:AddInline( "$"    , {| self, cTest | ::cValue $  cTest } )
      oClass:AddInline( "*"    , {| self, nVal  | Replicate( ::cValue, nVal ) } )
      oClass:AddInline( "/"    , {| self, nVal  | Left( ::cValue, Len( ::cValue ) / nVal ) } )
      oClass:AddInline( "%"    , {| self, cTest | ::cValue + " % " + cTest } )
      oClass:AddInline( "^"    , {| self, cTest | ::cValue + " ^ " + cTest } )
      oClass:AddInline( "**"   , {| self, cTest | ::cValue + " ^ " + cTest } )
      oClass:AddInline( "!"    , {| self        | Descend( ::cValue ) } )
      oClass:AddInline( ".NOT.", {| self        | Descend( ::cValue ) } )
      oClass:AddInline( ".AND.", {| self, cTest | ::cValue + " AND " + cTest } )
      oClass:AddInline( ".OR." , {| self, cTest | ::cValue + " OR " + cTest } )

      oClass:AddInline( "HasMsg", {| self, cMsg | HB_SYMBOL_UNUSED( self ), __objHasMsg( QSelf(), cMsg ) } )

      oClass:Create()
   ENDIF

   RETURN oClass:Instance()

#endif

STATIC FUNCTION TFORNEXT( xFrom, xTo, xStep )

   LOCAL tmp

   IF xStep == NIL
      FOR tmp := xFrom TO xTo
      NEXT
   ELSE
      FOR tmp := xFrom TO xTo STEP xStep
      NEXT
   ENDIF

   RETURN tmp

STATIC FUNCTION TFORNEXTX( xFrom, xTo, xStep )

   LOCAL tmp
   LOCAL cResult := ""
   LOCAL bFrom := {|| cResult += "F", xFrom }
   LOCAL bTo   := {|| cResult += "T", xTo   }
   LOCAL bStep := {|| cResult += "S", xStep }

   IF xStep == NIL
      FOR tmp := Eval( bFrom ) TO Eval( bTo )
      NEXT
   ELSE
      FOR tmp := Eval( bFrom ) TO Eval( bTo ) STEP Eval( bStep )
      NEXT
   ENDIF

   RETURN cResult

STATIC FUNCTION TFORNEXTXF( xFrom, xTo, xStep )

   LOCAL tmp := -9999
   LOCAL cResult := ""
   LOCAL bFrom := {|| cResult += "F" + LTrim( Str( tmp ) ), xFrom }
   LOCAL bTo   := {|| cResult += "T" + LTrim( Str( tmp ) ), xTo   }
   LOCAL bStep := {|| cResult += "S" + LTrim( Str( tmp ) ), xStep }

   IF xStep == NIL
      FOR tmp := Eval( bFrom ) TO Eval( bTo )
      NEXT
   ELSE
      FOR tmp := Eval( bFrom ) TO Eval( bTo ) STEP Eval( bStep )
      NEXT
   ENDIF

   RETURN cResult + "R" + LTrim( Str( tmp ) )

#ifdef __HARBOUR__

/* NOTE: cDrive is not tested because it's platform dependent. */

STATIC FUNCTION TESTFNAME( cFull )

   LOCAL cPath, cName, cExt, cDrive

   hb_FNameSplit( RTrim( cFull ), @cPath, @cName, @cExt, @cDrive )

   RETURN ;
      hb_FNameMerge( cPath, cName, cExt ) + ";" + ;
      cPath + ";" + ;
      cName + ";" + ;
      cExt + ";" + ;
      ""

#endif

STATIC FUNCTION BADFNAME()
   /* NOTE: The dot in the "*INVALID*." filename is intentional and serves
            to hide different path handling, since Harbour is platform
            independent. */
#ifdef __PLATFORM__UNIX
   return "*INVALID/*."
#else
   return "*INVALID*."
#endif

STATIC FUNCTION BADFNAME2()
#ifdef __PLATFORM__UNIX
   return "*INVALI/*.TMP"
#else
   return "*INVALI*.TMP"
#endif

STATIC FUNCTION RDD_SORT_D()

   LOCAL nOldArea := Select()

   LOCAL cSource := "$$SOURCE.DBF"
   LOCAL cSorted := "$$SORTED.DBF"

   LOCAL cResult

   dbCreate( cSource, { { "ITEM", "N", 10, 2 } } )

   USE ( cSource ) ALIAS w_TEMP NEW

   dbAppend(); FIELD->ITEM := 5.00
   dbAppend(); FIELD->ITEM := -5.12
   dbAppend(); FIELD->ITEM := 6.00
   dbAppend(); FIELD->ITEM := 7.00
   dbAppend(); FIELD->ITEM := -5.00

   /* Command to be tested */
   SORT TO ( cSorted ) ON FIELD->ITEM /D

   USE ( cSorted ) ALIAS w_TEMP

   cResult := ""
   dbEval( {|| cResult += LTrim( Str( FIELD->ITEM ) ) + " " } )
   cResult := RTrim( cResult )

   dbCloseArea()

   dbSelectArea( nOldArea )

#ifdef __HARBOUR__
   hb_dbDrop( cSource )
   hb_dbDrop( cSorted )
#else
   FErase( cSource )
   FErase( cSorted )
#endif

   RETURN cResult

#ifdef __HARBOUR__

STATIC PROCEDURE Test_Hash()

   LOCAL h := { "a" => 1, "b" => 2 }
   LOCAL v

   HBTEST ( hb_HGetRef()                           ) IS .F.
   HBTEST ( hb_HGetRef( 1 )                        ) IS .F.
   HBTEST ( hb_HGetRef( , 2 )                      ) IS .F.
   HBTEST ( hb_HGetRef( h )                        ) IS .F.
   HBTEST ( hb_HGetRef( h, h )                     ) IS .F.
   HBTEST ( hb_HGetRef( h, 2 )                     ) IS .F.
   HBTEST ( hb_HGetRef( h, "a" )                   ) IS .T.
   HBTEST ( hb_HGetRef( h, "a", v ), v             ) IS NIL
   HBTEST ( hb_HGetRef( h, "a", @v ), v            ) IS 1
   HBTEST ( hb_HGetRef( h, "b" )                   ) IS .T.
   v := NIL
   HBTEST ( hb_HGetRef( h, "b", v ), v             ) IS NIL
   HBTEST ( hb_HGetRef( h, "b", @v ), v            ) IS 2
   HBTEST ( v := "zz", hb_HGetRef( h, "c" )        ) IS .F.
   HBTEST ( v := "zz", hb_HGetRef( h, "c", v ), v  ) IS "zz"
   HBTEST ( v := "zz", hb_HGetRef( h, "c", @v ), v ) IS NIL

   RETURN

/* SHA-2 FIPS 180-2 Validation tests */
STATIC PROCEDURE Test_SHA2()

   LOCAL cMsg1  := "abc"
   LOCAL cMsg2a := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
   LOCAL cMsg2b := "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
   LOCAL cMsg3  := Replicate( "a", 1000000 )

   HBTEST Lower( hb_SHA224( cMsg1  ) ) IS "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7"
   HBTEST Lower( hb_SHA224( cMsg2a ) ) IS "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525"
   HBTEST Lower( hb_SHA224( cMsg3  ) ) IS "20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67"

   HBTEST Lower( hb_SHA256( cMsg1  ) ) IS "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
   HBTEST Lower( hb_SHA256( cMsg2a ) ) IS "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
   HBTEST Lower( hb_SHA256( cMsg3  ) ) IS "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"

   HBTEST Lower( hb_SHA384( cMsg1  ) ) IS "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"
   HBTEST Lower( hb_SHA384( cMsg2b ) ) IS "09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039"
   HBTEST Lower( hb_SHA384( cMsg3  ) ) IS "9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985"

   HBTEST Lower( hb_SHA512( cMsg1  ) ) IS "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
   HBTEST Lower( hb_SHA512( cMsg2b ) ) IS "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"
   HBTEST Lower( hb_SHA512( cMsg3  ) ) IS "e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"

   RETURN

/* HMAC-SHA-2 IETF Validation tests */
STATIC PROCEDURE Test_SHA2_HMAC()

   LOCAL aMsg := {;
      "Hi There",;
      "what do ya want for nothing?",;
      Replicate( hb_BChar( 0xDD ), 50 ),;
      Replicate( hb_BChar( 0xCD ), 50 ),;
      "Test With Truncation",;
      "Test Using Larger Than Block-Size Key - Hash Key First",;
      "This is a test using a larger than block-size key and a larger than block-size data. The key needs to be hashed before being used by the HMAC algorithm." }

   LOCAL results := {;
      "896fb1128abbdf196832107cd49df33f47b4b1169912ba4f53684b22",; /* HMAC-SHA-224 */
      "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44",;
      "7fb3cb3588c6c1f6ffa9694d7d6ad2649365b0c1f65d69d1ec8333ea",;
      "6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a",;
      "0e2aea68a90c8d37c988bcdb9fca6fa8",;
      "95e9a0db962095adaebe9b2d6f0dbce2d499f112f2d2b7273fa6870e",;
      "3a854166ac5d9f023f54d517d0b39dbd946770db9c2b95c9f6f565d1",;
      "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7",; /* HMAC-SHA-256 */
      "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843",;
      "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe",;
      "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b",;
      "a3b6167473100ee06e0c796c2955552b",;
      "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54",;
      "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2",;
      "afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6",; /* HMAC-SHA-384 */
      "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649",;
      "88062608d3e6ad8a0aa2ace014c8a86f0aa635d947ac9febe83ef4e55966144b2a5ab39dc13814b94e3ab6e101a34f27",;
      "3e8a69b7783c25851933ab6290af6ca77a9981480850009cc5577c6e1f573b4e6801dd23c4a7d679ccf8a386c674cffb",;
      "3abf34c3503b2a23a46efc619baef897",;
      "4ece084485813e9088d2c63a041bc5b44f9ef1012a2b588f3cd11f05033ac4c60c2ef6ab4030fe8296248df163f44952",;
      "6617178e941f020d351e2f254e8fd32c602420feb0b8fb9adccebb82461e99c5a678cc31e799176d3860e6110c46523e",;
      "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854",; /* HMAC-SHA-512 */
      "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737",;
      "fa73b0089d56a284efb0f0756c890be9b1b5dbdd8ee81a3655f83e33b2279d39bf3e848279a722c806b485a47e67c807b946a337bee8942674278859e13292fb",;
      "b0ba465637458c6990e5a8c5f61d4af7e576d97ff94b872de76f8050361ee3dba91ca5c11aa25eb4d679275cc5788063a5f19741120c4f2de2adebeb10a298dd",;
      "415fad6271580a531d4179bc891d87a6",;
      "80b24263c7c1a3ebb71493c1dd7be8b49b46d1f41b4aeec1121b013783f8f3526b56d037e05f2598bd0fd2215d6a1e5295e64f73f63f0aec8b915a985d786598",;
      "e37b6a775dc87dbaa4dfa9f96e5e3ffddebd71f8867289865df5a32d20cdc944b6022cac3c4982b10d5eeb55c3e4de15134676fb6de0446065c97440fa8c6a58"}

   LOCAL keys := {;
      Replicate( hb_BChar( 0x0b ),  20 ),;
      "Jefe",;
      Replicate( hb_BChar( 0xaa ),  20 ),;
      "",;
      Replicate( hb_BChar( 0x0c ),  20 ),;
      Replicate( hb_BChar( 0xaa ), 131 ),;
      Replicate( hb_BChar( 0xaa ), 131 ) }

   LOCAL tmp

   FOR tmp := 1 TO 25
      keys[ 4 ] += hb_BChar( tmp )
   NEXT

   FOR tmp := 1 TO 7
      IF tmp != 5
         HBTEST Lower( hb_HMAC_SHA224( aMsg[ tmp ], keys[ tmp ] ) ) IS results[ tmp ]
         HBTEST Lower( hb_HMAC_SHA256( aMsg[ tmp ], keys[ tmp ] ) ) IS results[ tmp + 7 ]
         HBTEST Lower( hb_HMAC_SHA384( aMsg[ tmp ], keys[ tmp ] ) ) IS results[ tmp + 14 ]
         HBTEST Lower( hb_HMAC_SHA512( aMsg[ tmp ], keys[ tmp ] ) ) IS results[ tmp + 21 ]
/* We don't support these MAC sizes */
#if 0
      ELSE
         mac_224_size := 128 / 8
         mac_256_size := 128 / 8
         mac_384_size := 128 / 8
         mac_512_size := 128 / 8
#endif
      ENDIF
   NEXT

   RETURN

#endif

/* Don't change the position of this #include. */
#include "rt_init.ch"
