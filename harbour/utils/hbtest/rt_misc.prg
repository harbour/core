/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (misc)
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "common.ch"

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

   /* Some random error object tests taken from the separate test source */

   oError := ErrorNew()
   TEST_LINE( oError:ClassName()              , "ERROR"                   )
   oError:Description := "Its description"
   TEST_LINE( oError:Description              , "Its description"         )
#ifdef __CLIPPER__
   TEST_LINE( Len( oError )                   , 7                         )
#endif
#ifdef __HARBOUR__
   TEST_LINE( Len( oError )                   , 12                        )
#endif

   /* SET()s */

   TEST_LINE( Set( _SET_MARGIN     )       , 0 )
   TEST_LINE( Set( _SET_MARGIN    , -1 )   , "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:25;N:-1 " )

#ifdef HB_COMPAT_C53
   TEST_LINE( Set( _SET_EVENTMASK  )       , 128 )
   TEST_LINE( Set( _SET_VIDEOMODE  )       , NIL )
   TEST_LINE( Set( _SET_MBLOCKSIZE )       , 64 )
   TEST_LINE( Set( _SET_MFILEEXT   )       , "" )
   TEST_LINE( Set( _SET_STRICTREAD )       , .F. )
   TEST_LINE( Set( _SET_OPTIMIZE   )       , .T. )
   TEST_LINE( Set( _SET_AUTOPEN    )       , .T. )
   TEST_LINE( Set( _SET_AUTORDER   )       , 0 )
   TEST_LINE( Set( _SET_AUTOSHARE  )       , 0 )

   TEST_LINE( Set( _SET_EVENTMASK , -1 )   , "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:39;N:-1 " )
   TEST_LINE( Set( _SET_VIDEOMODE , -1 )   , NIL )
   TEST_LINE( Set( _SET_MBLOCKSIZE, -1 )   , "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:41;N:-1 " )
   TEST_LINE( Set( _SET_MFILEEXT  , {} )   , "" )
   TEST_LINE( Set( _SET_STRICTREAD, {} )   , .F. )
   TEST_LINE( Set( _SET_OPTIMIZE  , {} )   , .T. )
   TEST_LINE( Set( _SET_AUTOPEN   , {} )   , .T. )
   TEST_LINE( Set( _SET_AUTORDER  , -1 )   , "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:46;N:-1 " )
   TEST_LINE( Set( _SET_AUTOSHARE , -1 )   , "E 1 BASE 2020 Argument error (SET) OS:0 #:0 A:2:N:47;N:-1 " )
#endif

   TEST_LINE( Set( _SET_ALTFILE,   BADFNAME2() ), "E 20 TERM 2013 Create error <" + BADFNAME2() + "> OS:2 #:1 F:DR" )
   TEST_LINE( Set( _SET_PRINTFILE, BADFNAME2() ), "E 20 TERM 2014 Create error <" + BADFNAME2() + "> OS:2 #:1 F:DR" )
   TEST_LINE( Set( _SET_EXTRAFILE, BADFNAME2() ), "E 20 TERM 2015 Create error <" + BADFNAME2() + "> OS:2 #:1 F:DR" )

   /* Some color handling tests */

   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "T"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "A"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "B"                          ), SetColor() ) , "B/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "C"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "D"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "E"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "F"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "G"                          ), SetColor() ) , "G/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "H"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "I"                          ), SetColor() ) , "N/W,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "J"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "K"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "L"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "M"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "N"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "O"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "P"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "Q"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "R"                          ), SetColor() ) , "R/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "S"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "T"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "U"                          ), SetColor() ) , "U/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "V"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "W"                          ), SetColor() ) , "W/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "X"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "Y"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "Z"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "0"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1"                          ), SetColor() ) , "B/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "2"                          ), SetColor() ) , "G/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "3"                          ), SetColor() ) , "BG/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "4"                          ), SetColor() ) , "R/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "5"                          ), SetColor() ) , "BR/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "6"                          ), SetColor() ) , "GR/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "7"                          ), SetColor() ) , "W/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "8"                          ), SetColor() ) , "N+/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "9"                          ), SetColor() ) , "B+/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "10"                         ), SetColor() ) , "G+/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "11"                         ), SetColor() ) , "BG+/N,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "12"                         ), SetColor() ) , "R+/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "13"                         ), SetColor() ) , "BR+/N,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "14"                         ), SetColor() ) , "GR+/N,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "15"                         ), SetColor() ) , "W+/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "16"                         ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "@"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "!"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "-"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "/"                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "//"                         ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( ","                          ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( ",,"                         ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "+"                          ), SetColor() ) , "N+/N,N/N,N/N,N/N,N/N"  )
#ifdef HB_CLP_STRICT_OFF
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "+*"                         ), SetColor() ) , "N+/N*,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*"                          ), SetColor() ) , "N/N*,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*+"                         ), SetColor() ) , "N+/N*,N/N,N/N,N/N,N/N" )
#else
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "+*"                         ), SetColor() ) , "N*+/N,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*"                          ), SetColor() ) , "N*/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "*+"                         ), SetColor() ) , "N*+/N,N/N,N/N,N/N,N/N" )
#endif
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "BR/W+"                      ), SetColor() ) , "BR+/W,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "BR/W+"                      ), SetColor() ) , "BR+/W,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "RB/W+"                      ), SetColor() ) , "BR+/W,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "0123456789"                 ), SetColor() ) , "BR/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1234567890"                 ), SetColor() ) , "G/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1"                          ), SetColor() ) , "B/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "11"                         ), SetColor() ) , "BG+/N,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "1111111111"                 ), SetColor() ) , "W/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "2"                          ), SetColor() ) , "G/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "22"                         ), SetColor() ) , "GR/N,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "2222222222"                 ), SetColor() ) , "GR+/N,N/N,N/N,N/N,N/N" )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ), SetColor() ) , "N/N,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "ABCDEFGHIJKLMNOPQRSTUVW"    ), SetColor() ) , "N/U,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "N/W+"                       ), SetColor() ) , "N+/W,N/N,N/N,N/N,N/N"  )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( '"W"'+"/"+'"R"'              ), SetColor() ) , "W/R,N/N,N/N,N/N,N/N"   )
   TEST_LINE( ( SetColor( "N/N,N/N,N/N,N/N,N/N" ), SetColor( "'W'"+"/"+"'R'"              ), SetColor() ) , "W/R,N/N,N/N,N/N,N/N"   )

   SetColor( "" ) /* Reset color to default */

   o := _GET_( tmp, "tmp" )

#ifdef HB_COMPAT_C53
   TEST_LINE( ( o:colorSpec := "T"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "A"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "B"                         , o:colorSpec ) , "B/N,B/N,B/N,B/N"        )
   TEST_LINE( ( o:colorSpec := "C"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "D"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "E"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "F"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "G"                         , o:colorSpec ) , "G/N,G/N,G/N,G/N"        )
   TEST_LINE( ( o:colorSpec := "H"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "I"                         , o:colorSpec ) , "N/W,N/W,N/W,N/W"        )
   TEST_LINE( ( o:colorSpec := "J"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "K"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "L"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "M"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "N"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "O"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "P"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "Q"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "R"                         , o:colorSpec ) , "R/N,R/N,R/N,R/N"        )
   TEST_LINE( ( o:colorSpec := "S"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "T"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "U"                         , o:colorSpec ) , "U/N,U/N,U/N,U/N"        )
   TEST_LINE( ( o:colorSpec := "V"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "W"                         , o:colorSpec ) , "W/N,W/N,W/N,W/N"        )
   TEST_LINE( ( o:colorSpec := "X"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "Y"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "Z"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "0"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "1"                         , o:colorSpec ) , "B/N,B/N,B/N,B/N"        )
   TEST_LINE( ( o:colorSpec := "2"                         , o:colorSpec ) , "G/N,G/N,G/N,G/N"        )
   TEST_LINE( ( o:colorSpec := "3"                         , o:colorSpec ) , "BG/N,BG/N,BG/N,BG/N"    )
   TEST_LINE( ( o:colorSpec := "4"                         , o:colorSpec ) , "R/N,R/N,R/N,R/N"        )
   TEST_LINE( ( o:colorSpec := "5"                         , o:colorSpec ) , "BR/N,BR/N,BR/N,BR/N"    )
   TEST_LINE( ( o:colorSpec := "6"                         , o:colorSpec ) , "GR/N,GR/N,GR/N,GR/N"    )
   TEST_LINE( ( o:colorSpec := "7"                         , o:colorSpec ) , "W/N,W/N,W/N,W/N"        )
   TEST_LINE( ( o:colorSpec := "8"                         , o:colorSpec ) , "N+/N,N+/N,N+/N,N+/N"    )
   TEST_LINE( ( o:colorSpec := "9"                         , o:colorSpec ) , "B+/N,B+/N,B+/N,B+/N"    )
   TEST_LINE( ( o:colorSpec := "10"                        , o:colorSpec ) , "G+/N,G+/N,G+/N,G+/N"    )
   TEST_LINE( ( o:colorSpec := "11"                        , o:colorSpec ) , "BG+/N,BG+/N,BG+/N,BG+/N")
   TEST_LINE( ( o:colorSpec := "12"                        , o:colorSpec ) , "R+/N,R+/N,R+/N,R+/N"    )
   TEST_LINE( ( o:colorSpec := "13"                        , o:colorSpec ) , "BR+/N,BR+/N,BR+/N,BR+/N")
   TEST_LINE( ( o:colorSpec := "14"                        , o:colorSpec ) , "GR+/N,GR+/N,GR+/N,GR+/N")
   TEST_LINE( ( o:colorSpec := "15"                        , o:colorSpec ) , "W+/N,W+/N,W+/N,W+/N"    )
   TEST_LINE( ( o:colorSpec := "16"                        , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "@"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "!"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "-"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "/"                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "//"                        , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := ","                         , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := ",,"                        , o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "+"                         , o:colorSpec ) , "N+/N,N+/N,N+/N,N+/N"    )
#ifdef HB_CLP_STRICT_OFF
   TEST_LINE( ( o:colorSpec := "+*"                        , o:colorSpec ) , "N+/N*,N+/N*,N+/N*,N+/N*")
   TEST_LINE( ( o:colorSpec := "*"                         , o:colorSpec ) , "N/N*,N/N*,N/N*,N/N*"    )
   TEST_LINE( ( o:colorSpec := "*+"                        , o:colorSpec ) , "N+/N*,N+/N*,N+/N*,N+/N*")
#else
   TEST_LINE( ( o:colorSpec := "+*"                        , o:colorSpec ) , "N*+/N,N*+/N,N*+/N,N*+/N")
   TEST_LINE( ( o:colorSpec := "*"                         , o:colorSpec ) , "N*/N,N*/N,N*/N,N*/N"    )
   TEST_LINE( ( o:colorSpec := "*+"                        , o:colorSpec ) , "N*+/N,N*+/N,N*+/N,N*+/N")
#endif
   TEST_LINE( ( o:colorSpec := "BR/W+"                     , o:colorSpec ) , "BR+/W,BR+/W,BR+/W,BR+/W")
   TEST_LINE( ( o:colorSpec := "BR/W+"                     , o:colorSpec ) , "BR+/W,BR+/W,BR+/W,BR+/W")
   TEST_LINE( ( o:colorSpec := "RB/W+"                     , o:colorSpec ) , "BR+/W,BR+/W,BR+/W,BR+/W")
   TEST_LINE( ( o:colorSpec := "0123456789"                , o:colorSpec ) , "BR/N,BR/N,BR/N,BR/N"    )
   TEST_LINE( ( o:colorSpec := "1234567890"                , o:colorSpec ) , "G/N,G/N,G/N,G/N"        )
   TEST_LINE( ( o:colorSpec := "1"                         , o:colorSpec ) , "B/N,B/N,B/N,B/N"        )
   TEST_LINE( ( o:colorSpec := "11"                        , o:colorSpec ) , "BG+/N,BG+/N,BG+/N,BG+/N")
   TEST_LINE( ( o:colorSpec := "1111111111"                , o:colorSpec ) , "W/N,W/N,W/N,W/N"        )
   TEST_LINE( ( o:colorSpec := "2"                         , o:colorSpec ) , "G/N,G/N,G/N,G/N"        )
   TEST_LINE( ( o:colorSpec := "22"                        , o:colorSpec ) , "GR/N,GR/N,GR/N,GR/N"    )
   TEST_LINE( ( o:colorSpec := "2222222222"                , o:colorSpec ) , "GR+/N,GR+/N,GR+/N,GR+/N")
   TEST_LINE( ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVWXYZ", o:colorSpec ) , "N/N,N/N,N/N,N/N"        )
   TEST_LINE( ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVW"   , o:colorSpec ) , "N/U,N/U,N/U,N/U"        )
   TEST_LINE( ( o:colorSpec := "N/W+"                      , o:colorSpec ) , "N+/W,N+/W,N+/W,N+/W"    )
   TEST_LINE( ( o:colorSpec := '"W"'+"/"+'"R"'             , o:colorSpec ) , "W/R,W/R,W/R,W/R"        )
   TEST_LINE( ( o:colorSpec := "'W'"+"/"+"'R'"             , o:colorSpec ) , "W/R,W/R,W/R,W/R"        )
#else
   TEST_LINE( ( o:colorSpec := "T"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "A"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "B"                         , o:colorSpec ) , "B/N,B/N"                )
   TEST_LINE( ( o:colorSpec := "C"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "D"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "E"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "F"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "G"                         , o:colorSpec ) , "G/N,G/N"                )
   TEST_LINE( ( o:colorSpec := "H"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "I"                         , o:colorSpec ) , "N/W,N/W"                )
   TEST_LINE( ( o:colorSpec := "J"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "K"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "L"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "M"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "N"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "O"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "P"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "Q"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "R"                         , o:colorSpec ) , "R/N,R/N"                )
   TEST_LINE( ( o:colorSpec := "S"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "T"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "U"                         , o:colorSpec ) , "U/N,U/N"                )
   TEST_LINE( ( o:colorSpec := "V"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "W"                         , o:colorSpec ) , "W/N,W/N"                )
   TEST_LINE( ( o:colorSpec := "X"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "Y"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "Z"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "0"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "1"                         , o:colorSpec ) , "B/N,B/N"                )
   TEST_LINE( ( o:colorSpec := "2"                         , o:colorSpec ) , "G/N,G/N"                )
   TEST_LINE( ( o:colorSpec := "3"                         , o:colorSpec ) , "BG/N,BG/N"              )
   TEST_LINE( ( o:colorSpec := "4"                         , o:colorSpec ) , "R/N,R/N"                )
   TEST_LINE( ( o:colorSpec := "5"                         , o:colorSpec ) , "BR/N,BR/N"              )
   TEST_LINE( ( o:colorSpec := "6"                         , o:colorSpec ) , "GR/N,GR/N"              )
   TEST_LINE( ( o:colorSpec := "7"                         , o:colorSpec ) , "W/N,W/N"                )
   TEST_LINE( ( o:colorSpec := "8"                         , o:colorSpec ) , "N+/N,N+/N"              )
   TEST_LINE( ( o:colorSpec := "9"                         , o:colorSpec ) , "B+/N,B+/N"              )
   TEST_LINE( ( o:colorSpec := "10"                        , o:colorSpec ) , "G+/N,G+/N"              )
   TEST_LINE( ( o:colorSpec := "11"                        , o:colorSpec ) , "BG+/N,BG+/N"            )
   TEST_LINE( ( o:colorSpec := "12"                        , o:colorSpec ) , "R+/N,R+/N"              )
   TEST_LINE( ( o:colorSpec := "13"                        , o:colorSpec ) , "BR+/N,BR+/N"            )
   TEST_LINE( ( o:colorSpec := "14"                        , o:colorSpec ) , "GR+/N,GR+/N"            )
   TEST_LINE( ( o:colorSpec := "15"                        , o:colorSpec ) , "W+/N,W+/N"              )
   TEST_LINE( ( o:colorSpec := "16"                        , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "@"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "!"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "-"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "/"                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "//"                        , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := ","                         , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := ",,"                        , o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "+"                         , o:colorSpec ) , "N+/N,N+/N"              )
#ifdef HB_CLP_STRICT_OFF
   TEST_LINE( ( o:colorSpec := "+*"                        , o:colorSpec ) , "N+/N*,N+/N*"            )
   TEST_LINE( ( o:colorSpec := "*"                         , o:colorSpec ) , "N/N*,N/N*"              )
   TEST_LINE( ( o:colorSpec := "*+"                        , o:colorSpec ) , "N+/N*,N+/N*"            )
#else
   TEST_LINE( ( o:colorSpec := "+*"                        , o:colorSpec ) , "N*+/N,N*+/N"            )
   TEST_LINE( ( o:colorSpec := "*"                         , o:colorSpec ) , "N*/N,N*/N"              )
   TEST_LINE( ( o:colorSpec := "*+"                        , o:colorSpec ) , "N*+/N,N*+/N"            )
#endif
   TEST_LINE( ( o:colorSpec := "BR/W+"                     , o:colorSpec ) , "BR+/W,BR+/W"            )
   TEST_LINE( ( o:colorSpec := "BR/W+"                     , o:colorSpec ) , "BR+/W,BR+/W"            )
   TEST_LINE( ( o:colorSpec := "RB/W+"                     , o:colorSpec ) , "BR+/W,BR+/W"            )
   TEST_LINE( ( o:colorSpec := "0123456789"                , o:colorSpec ) , "BR/N,BR/N"              )
   TEST_LINE( ( o:colorSpec := "1234567890"                , o:colorSpec ) , "G/N,G/N"                )
   TEST_LINE( ( o:colorSpec := "1"                         , o:colorSpec ) , "B/N,B/N"                )
   TEST_LINE( ( o:colorSpec := "11"                        , o:colorSpec ) , "BG+/N,BG+/N"            )
   TEST_LINE( ( o:colorSpec := "1111111111"                , o:colorSpec ) , "W/N,W/N"                )
   TEST_LINE( ( o:colorSpec := "2"                         , o:colorSpec ) , "G/N,G/N"                )
   TEST_LINE( ( o:colorSpec := "22"                        , o:colorSpec ) , "GR/N,GR/N"              )
   TEST_LINE( ( o:colorSpec := "2222222222"                , o:colorSpec ) , "GR+/N,GR+/N"            )
   TEST_LINE( ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVWXYZ", o:colorSpec ) , "N/N,N/N"                )
   TEST_LINE( ( o:colorSpec := "ABCDEFGHIJKLMNOPQRSTUVW"   , o:colorSpec ) , "N/U,N/U"                )
   TEST_LINE( ( o:colorSpec := "N/W+"                      , o:colorSpec ) , "N+/W,N+/W"              )
   TEST_LINE( ( o:colorSpec := '"W"'+"/"+'"R"'             , o:colorSpec ) , "W/R,W/R"                )
   TEST_LINE( ( o:colorSpec := "'W'"+"/"+"'R'"             , o:colorSpec ) , "W/R,W/R"                )
#endif
   /* "Samples" function tests (AMPM(), DAYS(), ELAPTIME(), ... ) */

   TEST_LINE( AMPM( "" )                      , "12 am"                                   )
   TEST_LINE( AMPM( "HELLO" )                 , "12LLO am"                                )
   TEST_LINE( AMPM( " 0:23:45" )              , "12:23:45 am"                             )
   TEST_LINE( AMPM( "00:23:45" )              , "12:23:45 am"                             )
   TEST_LINE( AMPM( " 5:23:45" )              , " 5:23:45 am"                             )
   TEST_LINE( AMPM( "05:23:45" )              , "05:23:45 am"                             )
   TEST_LINE( AMPM( "12:23:45" )              , "12:23:45 pm"                             )
   TEST_LINE( AMPM( "20:23:45" )              , " 8:23:45 pm"                             )
   TEST_LINE( AMPM( "24:23:45" )              , "12:23:45 am"                             )
   TEST_LINE( AMPM( "25:23:45" )              , "13:23:45 pm"                             )
   TEST_LINE( AMPM( "2" )                     , "2 am"                                    )
   TEST_LINE( AMPM( "02:23" )                 , "02:23 am"                                )
   TEST_LINE( AMPM( "02:23:45.10" )           , "02:23:45.10 am"                          )

   TEST_LINE( DAYS( 100000 )                  , 1 )

   TEST_LINE( ELAPTIME("23:12:34","12:34:57") , "13:22:23" )
   TEST_LINE( ELAPTIME("12:34:57","23:12:34") , "10:37:37" )

   TEST_LINE( LENNUM( 10 )                    , 2 )
   TEST_LINE( LENNUM( 10.9 )                  , 4 )
   TEST_LINE( LENNUM( 10.90 )                 , 5 )

   TEST_LINE( SECS("23:12:34")                , 83554 )
   TEST_LINE( SECS("12:34:57")                , 45297 )

   TEST_LINE( TSTRING(1000)                   , "00:16:40" )

#ifndef __XPP__
   TEST_LINE( SoundEx()                       , "0000" )
   TEST_LINE( SoundEx( 10 )                   , "0000" )
   TEST_LINE( SoundEx( @scString )            , "H400" )
   TEST_LINE( SoundEx( "" )                   , "0000" )
   TEST_LINE( SoundEx( "Hm" )                 , "H500" )
   TEST_LINE( SoundEx( "Smith" )              , "S530" )
   TEST_LINE( SoundEx( "Harbour" )            , "H616" )
   TEST_LINE( SoundEx( "HARBOUR" )            , "H616" )
   TEST_LINE( SoundEx( "Harpour" )            , "H616" )
   TEST_LINE( SoundEx( "Hello" )              , "H400" )
   TEST_LINE( SoundEx( "Aardwaark" )          , "A636" )
   TEST_LINE( SoundEx( "Ardwark" )            , "A636" )
   TEST_LINE( SoundEx( "Bold" )               , "B430" )
   TEST_LINE( SoundEx( "Cold" )               , "C430" )
   TEST_LINE( SoundEx( "Colt" )               , "C430" )
   TEST_LINE( SoundEx( "C"+Chr(0)+"olt" )     , "C430" )
   TEST_LINE( SoundEx( "µ†AêÇ" )              , "A000" )
   TEST_LINE( SoundEx( "12345" )              , "0000" )
#endif

   /* NATION functions (do not exist in 5.2e US) */

#ifdef __HARBOUR__
   #ifndef HB_CLP_UNDOC
      /* NOTE: Use the identical internal versions if Harbour
               was compiled without C5.x undocumented features.
               [vszakats] */
      #xtranslate NationMsg([<x,...>])  => __NatMsg(<x>)
      #xtranslate IsAffirm([<x,...>])   => __NatIsAffirm(<x>)
      #xtranslate IsNegative([<x,...>]) => __NatIsNegative(<x>)
   #endif
#endif

#ifndef __XPP__
   TEST_LINE( NationMsg()                     , "Invalid argument" )
#endif
   TEST_LINE( NationMsg("A")                  , "" )
   TEST_LINE( NationMsg(-1)                   , "" ) /* CA-Cl*pper bug: 5.3 may return trash. */
   TEST_LINE( NationMsg(0)                    , "" )
   TEST_LINE( NationMsg(1)                    , "Database Files    # Records    Last Update     Size" )
   TEST_LINE( NationMsg(2)                    , "Do you want more samples?"                           )
   TEST_LINE( NationMsg(3)                    , "Page No."                                            )
   TEST_LINE( NationMsg(4)                    , "** Subtotal **"                                      )
   TEST_LINE( NationMsg(5)                    , "* Subsubtotal *"                                     )
   TEST_LINE( NationMsg(6)                    , "*** Total ***"                                       )
   TEST_LINE( NationMsg(7)                    , "Ins"                                                 )
   TEST_LINE( NationMsg(8)                    , "   "                                                 )
   TEST_LINE( NationMsg(9)                    , "Invalid date"                                        )
   TEST_LINE( NationMsg(10)                   , "Range: "                                             )
   TEST_LINE( NationMsg(11)                   , " - "                                                 )
   TEST_LINE( NationMsg(12)                   , "Y/N"                                                 )
   TEST_LINE( NationMsg(13)                   , "INVALID EXPRESSION"                                  )
   TEST_LINE( NationMsg(14)                   , "" ) /* Bug in CA-Cl*pper 5.3a/b, it will return "ATSORT v1.3i x19 06/Mar/95" */
#ifndef __CLIPPER__ /* Causes GPF in CA-Cl*pper (5.2e International, 5.3b) */
   TEST_LINE( NationMsg(200)                  , "" ) /* Bug in CA-Cl*pper, it will return "74?" or other trash */
#endif

#ifndef __XPP__

/* These will cause a GPF in CA-Cl*pper (5.2e International, 5.3b) */
#ifndef __CLIPPER__
   TEST_LINE( IsAffirm()                      , .F.    )
   TEST_LINE( IsAffirm(.F.)                   , .F.    )
   TEST_LINE( IsAffirm(.T.)                   , .F.    )
   TEST_LINE( IsAffirm(0)                     , .F.    )
   TEST_LINE( IsAffirm(1)                     , .F.    )
#endif
   TEST_LINE( IsAffirm("")                    , .F.    )
   TEST_LINE( IsAffirm("I")                   , .F.    )
   TEST_LINE( IsAffirm("y")                   , .T.    )
   TEST_LINE( IsAffirm("Y")                   , .T.    )
   TEST_LINE( IsAffirm("yes")                 , .T.    )
   TEST_LINE( IsAffirm("YES")                 , .T.    )
   TEST_LINE( IsAffirm("n")                   , .F.    )
   TEST_LINE( IsAffirm("N")                   , .F.    )
   TEST_LINE( IsAffirm("no")                  , .F.    )
   TEST_LINE( IsAffirm("NO")                  , .F.    )

/* These will cause a GPF in CA-Cl*pper (5.2e International, 5.3b) */
#ifndef __CLIPPER__
   TEST_LINE( IsNegative()                    , .F.    )
   TEST_LINE( IsNegative(.F.)                 , .F.    )
   TEST_LINE( IsNegative(.T.)                 , .F.    )
   TEST_LINE( IsNegative(0)                   , .F.    )
   TEST_LINE( IsNegative(1)                   , .F.    )
#endif
   TEST_LINE( IsNegative("")                  , .F.    )
   TEST_LINE( IsNegative("I")                 , .F.    )
   TEST_LINE( IsNegative("y")                 , .F.    )
   TEST_LINE( IsNegative("Y")                 , .F.    )
   TEST_LINE( IsNegative("yes")               , .F.    )
   TEST_LINE( IsNegative("YES")               , .F.    )
   TEST_LINE( IsNegative("n")                 , .T.    )
   TEST_LINE( IsNegative("N")                 , .T.    )
   TEST_LINE( IsNegative("no")                , .T.    )
   TEST_LINE( IsNegative("NO")                , .T.    )

#endif /* __XPP__ */

   /* FOR/NEXT */

   TEST_LINE( TFORNEXT( .F., .T., NIL )       , "E 1 BASE 1086 Argument error (++) OS:0 #:0 A:1:L:.F. F:S" )
   TEST_LINE( TFORNEXT( .T., .F., NIL )       , .T.                                       )
   TEST_LINE( TFORNEXT( .F., .F., NIL )       , "E 1 BASE 1086 Argument error (++) OS:0 #:0 A:1:L:.F. F:S" )
   TEST_LINE( TFORNEXT( 100, 101, NIL )       , 102                                       )
   TEST_LINE( TFORNEXT( "A", "A", NIL )       , "E 1 BASE 1086 Argument error (++) OS:0 #:0 A:1:C:A F:S" )
   TEST_LINE( TFORNEXT( NIL, NIL, NIL )       , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )
   TEST_LINE( TFORNEXT( .F., .T.,   1 )       , "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:L:.F.;N:1 F:S" )
   TEST_LINE( TFORNEXT( .F., .T.,  -1 )       , .F.                                       )
   TEST_LINE( TFORNEXT( .F., .T., .F. )       , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:L:.F.;N:0 F:S" )
   TEST_LINE( TFORNEXT( .T., .F.,   1 )       , .T.                                       )
   TEST_LINE( TFORNEXT( .T., .F.,  -1 )       , "E 1 BASE 1081 Argument error (+) OS:0 #:0 A:2:L:.T.;N:-1 F:S" )
   TEST_LINE( TFORNEXT( .T., .F., .T. )       , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:L:.T.;N:0 F:S" )
   TEST_LINE( TFORNEXT( 100, 101,   1 )       , 102                                       )
   TEST_LINE( TFORNEXT( 101, 100,  -1 )       , 99                                        )
   TEST_LINE( TFORNEXT( "A", "A", "A" )       , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:0 F:S" )
   TEST_LINE( TFORNEXT( "A", "B", "A" )       , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:0 F:S" )
   TEST_LINE( TFORNEXT( "B", "A", "A" )       , "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:0 F:S" )
   TEST_LINE( TFORNEXT( NIL, NIL, NIL )       , "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:U:NIL;U:NIL F:S" )

   TEST_LINE( TFORNEXTX(   1, 10,NIL )        , "FTTTTTTTTTTT"                            )
   TEST_LINE( TFORNEXTX(  10,  1,NIL )        , "FT"                                      )
   TEST_LINE( TFORNEXTX(   1, 10,  1 )        , "FTSSTSSTSSTSSTSSTSSTSSTSSTSSTSSTS"       )
   TEST_LINE( TFORNEXTX(  10,  1, -1 )        , "FTSSTSSTSSTSSTSSTSSTSSTSSTSSTSSTS"       )
   TEST_LINE( TFORNEXTX(   1, 10, -1 )        , "FTS"                                     )
   TEST_LINE( TFORNEXTX(  10,  1,  1 )        , "FTS"                                     )
   TEST_LINE( TFORNEXTX(   1, 10,  4 )        , "FTSSTSSTSSTS"                            )
   TEST_LINE( TFORNEXTX(  10,  1, -4 )        , "FTSSTSSTSSTS"                            )
   TEST_LINE( TFORNEXTX(   1, 10, -4 )        , "FTS"                                     )
   TEST_LINE( TFORNEXTX(  10,  1,  4 )        , "FTS"                                     )

   TEST_LINE( TFORNEXTXF(   1, 10,NIL )       , "F-9999T1T2T3T4T5T6T7T8T9T10T11R11"                                              )
   TEST_LINE( TFORNEXTXF(  10,  1,NIL )       , "F-9999T10R10"                                                                   )
   TEST_LINE( TFORNEXTXF(   1, 10,  1 )       , "F-9999T1S1S1T2S2S2T3S3S3T4S4S4T5S5S5T6S6S6T7S7S7T8S8S8T9S9S9T10S10S10T11S11R11" )
   TEST_LINE( TFORNEXTXF(  10,  1, -1 )       , "F-9999T10S10S10T9S9S9T8S8S8T7S7S7T6S6S6T5S5S5T4S4S4T3S3S3T2S2S2T1S1S1T0S0R0"    )
   TEST_LINE( TFORNEXTXF(   1, 10, -1 )       , "F-9999T1S1R1"                                                                   )
   TEST_LINE( TFORNEXTXF(  10,  1,  1 )       , "F-9999T10S10R10"                                                                )
   TEST_LINE( TFORNEXTXF(   1, 10,  4 )       , "F-9999T1S1S1T5S5S5T9S9S9T13S13R13"                                              )
   TEST_LINE( TFORNEXTXF(  10,  1, -4 )       , "F-9999T10S10S10T6S6S6T2S2S2T-2S-2R-2"                                           )
   TEST_LINE( TFORNEXTXF(   1, 10, -4 )       , "F-9999T1S1R1"                                                                   )
   TEST_LINE( TFORNEXTXF(  10,  1,  4 )       , "F-9999T10S10R10"                                                                )

   /* EVAL(), :EVAL(), :EVAL */

   TEST_LINE( Eval( NIL )                     , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( Eval( 1 )                       , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:N:1 F:S" )
   TEST_LINE( Eval( @sbBlock )                , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:B:{||...} F:S" ) /* CA-Cl*pper returns "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:{||...} F:S" */
   TEST_LINE( Eval( {| p1 | p1 },"A","B")     , "A"                                       )
   TEST_LINE( Eval( {| p1, p2 | p1 + p2 },"A","B"), "AB"                                      )
#ifdef __HARBOUR__
   TEST_LINE( Eval( {| p1, p2, p3 | HB_SYMBOL_UNUSED( p2 ), HB_SYMBOL_UNUSED( p3 ), p1 }, "A", "B" ), "A"                                       )
#else
   TEST_LINE( Eval( {| p1, p2, p3 | p1 }, "A", "B" ) , "A"                                       )
#endif
   TEST_LINE( suNIL:Eval()                    , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( scString:Eval()                 , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:C:HELLO F:S" )
   TEST_LINE( snIntP:Eval()                   , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:N:10 F:S" )
   TEST_LINE( sdDateE:Eval()                  , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:D:         F:S" )
   TEST_LINE( slFalse:Eval()                  , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:L:.F. F:S" )
   TEST_LINE( sbBlock:Eval()                  , NIL                                       )
   TEST_LINE( saArray:Eval()                  , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:A:{.[1].} F:S" )
   TEST_LINE( soObject:Eval()                 , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:O:ERROR Object F:S" )
   TEST_LINE( suNIL:Eval                      , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:U:NIL F:S" )
   TEST_LINE( scString:Eval                   , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:C:HELLO F:S" )
   TEST_LINE( snIntP:Eval                     , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:N:10 F:S" )
   TEST_LINE( sdDateE:Eval                    , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:D:         F:S" )
   TEST_LINE( slFalse:Eval                    , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:L:.F. F:S" )
   TEST_LINE( sbBlock:Eval                    , NIL                                       )
   TEST_LINE( saArray:Eval                    , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:A:{.[1].} F:S" )
   TEST_LINE( soObject:Eval                   , "E 13 BASE 1004 No exported method (EVAL) OS:0 #:0 A:1:O:ERROR Object F:S" )

   /* HB_STOD() */

   /* For these tests in CA-Cl*pper 5.2e the following native HB_STOD() has
      been used ( not the emulated one written in Clipper ):

      CLIPPER HB_STOD( void )
      {
         // The length check is a fix to avoid buggy behaviour of _retds()
         _retds( ( ISCHAR( 1 ) && _parclen( 1 ) == 8 ) ? _parc( 1 ) : "        " );
      }
   */

#ifndef RT_NO_C
#ifndef __XPP__
   TEST_LINE( HB_SToD()                       , HB_SToD("        ")          )
#endif
   TEST_LINE( HB_SToD(1)                      , HB_SToD("        ")          )
   TEST_LINE( HB_SToD(NIL)                    , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("")                     , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("        ")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("       ")              , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("         ")            , HB_SToD("        ")          )
   TEST_LINE( HB_SToD(" 1234567")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("1999    ")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("99999999")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("99990101")             , HB_SToD("99990101")          )
   TEST_LINE( HB_SToD("19991301")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("19991241")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("01000101")             , HB_SToD("01000101")          )
   TEST_LINE( HB_SToD("29991231")             , HB_SToD("29991231")          )
   TEST_LINE( HB_SToD("19990905")             , HB_SToD("19990905")          )
   TEST_LINE( HB_SToD(" 9990905")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("  990905")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("   90905")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("    0905")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("     905")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("      05")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("1 990905")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("19 90905")             , HB_SToD("17490905")          )
   TEST_LINE( HB_SToD("199 0905")             , HB_SToD("19740905")          )
   TEST_LINE( HB_SToD("1999 905")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("19990 05")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("199909 5")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("1999090 ")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("1999 9 5")             , HB_SToD("        ")          )
   TEST_LINE( HB_SToD("1999090" + Chr(0))     , HB_SToD("        ")          )
#endif

   /* DESCEND() */

#ifndef __CLIPPER__ /* Bug in CA-Cl*pper, it returns undefined trash */
#ifndef __XPP__ /* Compiler time error */
   TEST_LINE( Descend()                       , NIL                                                 )
#endif
#endif
   TEST_LINE( Descend( NIL )                  , NIL                                                 )
   TEST_LINE( Descend( { "A", "B" } )         , NIL                                                 )
#ifdef __HARBOUR__
   TEST_LINE( Descend( @scString )            , "∏ª¥¥±"                                             ) /* Bug in CA-Cl*pper, it will return NIL */
#endif
   TEST_LINE( Descend( scString )             , "∏ª¥¥±"                                             )
   TEST_LINE( Descend( scString )             , "∏ª¥¥±"                                             )
   TEST_LINE( Descend( Descend( scString ) )  , "HELLO"                                             )
   TEST_LINE( Descend( .F. )                  , .T.                                                 )
   TEST_LINE( Descend( .T. )                  , .F.                                                 )
   TEST_LINE( Descend( 0 )                    , 0.00                                                )
   TEST_LINE( Descend( 1 )                    , -1.00                                               )
   TEST_LINE( Descend( -1 )                   , 1.00                                                )
   TEST_LINE( Descend( Descend( 256 ) )       , 256.00                                              )
   TEST_LINE( Descend( 2.0 )                  , -2.00                                               )
   TEST_LINE( Descend( 2.5 )                  , -2.50                                               )
   TEST_LINE( Descend( -100.35 )              , 100.35                                              )
   TEST_LINE( Str(Descend( -740.354 ))        , "       740.35"                                     )
   TEST_LINE( Str(Descend( -740.359 ))        , "       740.36"                                     )
   TEST_LINE( Str(Descend( -740.354 ), 15, 5) , "      740.35400"                                   )
   TEST_LINE( Str(Descend( -740.359 ), 15, 5) , "      740.35900"                                   )
   TEST_LINE( Descend( 100000 )               , -100000.00                                          )
   TEST_LINE( Descend( -100000 )              , 100000.00                                           )
   TEST_LINE( Descend( "" )                   , ""                                                  )
   TEST_LINE( Descend( Chr(0) )               , ""+Chr(0)+""                                        )
   TEST_LINE( Descend( Chr(0) + "Hello" )     , ""+Chr(0)+"∏õîîë"                                   )
   TEST_LINE( Descend( "Hello"+Chr(0)+"wo" )  , "∏õîîë"+Chr(0)+"âë"                                 )
   TEST_LINE( Descend( HB_SToD( "" ) )        , 5231808                                             )
   TEST_LINE( Descend( HB_SToD( "01000101" ) ), 3474223                                             )
   TEST_LINE( Descend( HB_SToD( "19801220" ) ), 2787214                                             )

#ifdef __HARBOUR__

   /* HB_COLORINDEX() */

   TEST_LINE( hb_ColorIndex()                  , ""               )
   TEST_LINE( hb_ColorIndex("", -1)            , ""               )
   TEST_LINE( hb_ColorIndex("", 0)             , ""               )
   TEST_LINE( hb_ColorIndex("W/R", -1)         , ""               )
   TEST_LINE( hb_ColorIndex("W/R", 0)          , "W/R"            )
   TEST_LINE( hb_ColorIndex("W/R", 1)          , ""               )
   TEST_LINE( hb_ColorIndex("W/R", 2)          , ""               )
   TEST_LINE( hb_ColorIndex("W/R,GR/0", 0)     , "W/R"            )
   TEST_LINE( hb_ColorIndex("W/R,GR/0", 1)     , "GR/0"           )
   TEST_LINE( hb_ColorIndex("W/R,GR/0", 2)     , ""               )
   TEST_LINE( hb_ColorIndex("W/R,GR/0", 3)     , ""               )
   TEST_LINE( hb_ColorIndex("W/R, GR/0", 0)    , "W/R"            )
   TEST_LINE( hb_ColorIndex("W/R, GR/0", 1)    , "GR/0"           )
   TEST_LINE( hb_ColorIndex("W/R, GR/0", 2)    , ""               )
   TEST_LINE( hb_ColorIndex("W/R, GR/0", 3)    , ""               )
   TEST_LINE( hb_ColorIndex("W/R,GR/0 ", 0)    , "W/R"            )
   TEST_LINE( hb_ColorIndex("W/R,GR/0 ", 1)    , "GR/0"           )
   TEST_LINE( hb_ColorIndex("W/R,GR/0 ", 2)    , ""               )
   TEST_LINE( hb_ColorIndex("W/R, GR/0 ", 0)   , "W/R"            )
   TEST_LINE( hb_ColorIndex("W/R, GR/0 ", 1)   , "GR/0"           )
   TEST_LINE( hb_ColorIndex("W/R, GR/0 ", 2)   , ""               )
   TEST_LINE( hb_ColorIndex("W/R, GR/0 ,", 0)  , "W/R"            )
   TEST_LINE( hb_ColorIndex("W/R, GR/0 ,", 1)  , "GR/0"           )
   TEST_LINE( hb_ColorIndex("W/R, GR/0 ,", 2)  , ""               )
   TEST_LINE( hb_ColorIndex(" W/R, GR/0 ,", 0) , "W/R"            )
   TEST_LINE( hb_ColorIndex(" W/R, GR/0 ,", 1) , "GR/0"           )
   TEST_LINE( hb_ColorIndex(" W/R, GR/0 ,", 2) , ""               )
   TEST_LINE( hb_ColorIndex(" W/R , GR/0 ,", 0), "W/R"            )
   TEST_LINE( hb_ColorIndex(" W/R , GR/0 ,", 1), "GR/0"           )
   TEST_LINE( hb_ColorIndex(" W/R , GR/0 ,", 2), ""               )
   TEST_LINE( hb_ColorIndex(" W/R ,   ,", 1)   , ""               )
   TEST_LINE( hb_ColorIndex(" W/R ,,", 1)      , ""               )
   TEST_LINE( hb_ColorIndex(",,", 0)           , ""               )
   TEST_LINE( hb_ColorIndex(",,", 1)           , ""               )
   TEST_LINE( hb_ColorIndex(",,", 2)           , ""               )
   TEST_LINE( hb_ColorIndex(",  ,", 2)         , ""               )

#endif

#ifndef __XPP__

   /* FKMAX(), FKLABEL() */

   TEST_LINE( FKMax()                         , 40               )
   TEST_LINE( FKMax( 1 )                      , 40               )
#ifdef __HARBOUR__
   TEST_LINE( FKLabel()                       , ""               ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:U:NIL;N:40 F:S" */
   TEST_LINE( FKLabel( NIL )                  , ""               ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:U:NIL;N:40 F:S" */
   TEST_LINE( FKLabel( "A" )                  , ""               ) /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:A;N:40 F:S" */
#endif
   TEST_LINE( FKLabel( -1 )                   , ""               )
   TEST_LINE( FKLabel( 0 )                    , ""               )
   TEST_LINE( FKLabel( 1 )                    , "F1"             )
   TEST_LINE( FKLabel( 25 )                   , "F25"            )
   TEST_LINE( FKLabel( 40 )                   , "F40"            )
   TEST_LINE( FKLabel( 41 )                   , ""               )

#endif /* __XPP__ */

   /* NOTE: BIN2*() functions are quite untable in CA-Cl*pper when the passed
      parameter is smaller than the required length. */

   /* BIN2I() */

#ifndef __CLIPPER__
#ifndef __XPP__
   TEST_LINE( BIN2I()                         , 0                ) /* Bug in CA-Cl*pper, this causes a GPF */
#endif
   TEST_LINE( BIN2I(100)                      , 0                ) /* Bug in CA-Cl*pper, this causes a GPF */
   TEST_LINE( BIN2I("")                       , 0                ) /* Bug in CA-Cl*pper, it will return trash */
#endif
   TEST_LINE( BIN2I("AB")                     , 16961            )
   TEST_LINE( BIN2I("BA")                     , 16706            )
   TEST_LINE( BIN2I(Chr(255))                 , 255              )
   TEST_LINE( BIN2I(Chr(255)+Chr(255))        , -1               )
   TEST_LINE( BIN2I(Chr(0))                   , 0                )
   TEST_LINE( BIN2I(Chr(0)+Chr(0))            , 0                )
   TEST_LINE( BIN2I("A")                      , 65               )
   TEST_LINE( BIN2I("ABC")                    , 16961            )

   /* BIN2W() */

#ifndef __CLIPPER__
#ifndef __XPP__
   TEST_LINE( BIN2W()                         , 0                ) /* Bug in CA-Cl*pper, this causes a GPF */
#endif
   TEST_LINE( BIN2W(100)                      , 0                ) /* Bug in CA-Cl*pper, this causes a GPF */
   TEST_LINE( BIN2W("")                       , 0                ) /* Bug in CA-Cl*pper, it will return trash */
#endif
   TEST_LINE( BIN2W("AB")                     , 16961            )
   TEST_LINE( BIN2W("BA")                     , 16706            )
   TEST_LINE( BIN2W(Chr(255))                 , 255              )
   TEST_LINE( BIN2W(Chr(255)+Chr(255))        , 65535            )
   TEST_LINE( BIN2W(Chr(0))                   , 0                )
   TEST_LINE( BIN2W(Chr(0)+Chr(0))            , 0                )
   TEST_LINE( BIN2W("A")                      , 65               )
   TEST_LINE( BIN2W("ABC")                    , 16961            )

   /* BIN2L() */

#ifndef __CLIPPER__
#ifndef __XPP__
   TEST_LINE( BIN2L()                                    , 0                ) /* Bug in CA-Cl*pper, this causes a GPF */
#endif
   TEST_LINE( BIN2L(100)                                 , 0                ) /* Bug in CA-Cl*pper, this causes a GPF */
   TEST_LINE( BIN2L("")                                  , 0                ) /* Bug in CA-Cl*pper, it will return trash */
#endif
   TEST_LINE( BIN2L("ABCD")                              , 1145258561       )
   TEST_LINE( BIN2L("DCBA")                              , 1094861636       )
#ifndef __CLIPPER__
   TEST_LINE( BIN2L(Chr(255))                            , 255              ) /* Bug in CA-Cl*pper, it will return trash */
#endif
   TEST_LINE( BIN2L(Chr(255)+Chr(255)+Chr(255))          , 16777215         )
   TEST_LINE( BIN2L(Chr(255)+Chr(255)+Chr(255)+Chr(255)) , -1               )
   TEST_LINE( BIN2L(Chr(0)+Chr(0)+Chr(0))                , 0                )
   TEST_LINE( BIN2L(Chr(0)+Chr(0)+Chr(0)+Chr(0))         , 0                )
   TEST_LINE( BIN2L("ABC")                               , 4407873          )
   TEST_LINE( BIN2L("ABCDE")                             , 1145258561       )

   /* I2BIN() */

#ifndef __XPP__
   TEST_LINE( I2BIN()                         , ""+Chr(0)+""+Chr(0)+"" )
#endif
   TEST_LINE( I2BIN(""     )                  , ""+Chr(0)+""+Chr(0)+"" )
   TEST_LINE( I2BIN(0      )                  , ""+Chr(0)+""+Chr(0)+"" )
   TEST_LINE( I2BIN(16961  )                  , "AB"                   )
   TEST_LINE( I2BIN(16706  )                  , "BA"                   )
   TEST_LINE( I2BIN(255    )                  , "ˇ"+Chr(0)+""          )
   TEST_LINE( I2BIN(-1     )                  , "ˇˇ"                   )
   TEST_LINE( I2BIN(0      )                  , ""+Chr(0)+""+Chr(0)+"" )
   TEST_LINE( I2BIN(0      )                  , ""+Chr(0)+""+Chr(0)+"" )
   TEST_LINE( I2BIN(65     )                  , "A"+Chr(0)+""          )
   TEST_LINE( I2BIN(16961  )                  , "AB"                   )

   /* L2BIN() */

#ifndef __XPP__
   TEST_LINE( L2BIN()                         , ""+Chr(0)+""+Chr(0)+""+Chr(0)+""+Chr(0)+""  )
#endif
   TEST_LINE( L2BIN("")                       , ""+Chr(0)+""+Chr(0)+""+Chr(0)+""+Chr(0)+""  )
   TEST_LINE( L2BIN(0          )              , ""+Chr(0)+""+Chr(0)+""+Chr(0)+""+Chr(0)+""  )
   TEST_LINE( L2BIN(1145258561 )              , "ABCD"                                      )
   TEST_LINE( L2BIN(1094861636 )              , "DCBA"                                      )
   TEST_LINE( L2BIN(255        )              , "ˇ"+Chr(0)+""+Chr(0)+""+Chr(0)+""           )
   TEST_LINE( L2BIN(16777215   )              , "ˇˇˇ"+Chr(0)+""                             )
   TEST_LINE( L2BIN(-1         )              , Chr(255)+Chr(255)+Chr(255)+Chr(255)         )
   TEST_LINE( L2BIN(0          )              , ""+Chr(0)+""+Chr(0)+""+Chr(0)+""+Chr(0)+""  )
   TEST_LINE( L2BIN(0          )              , Chr(0)+Chr(0)+Chr(0)+Chr(0)                 )
   TEST_LINE( L2BIN(4407873    )              , "ABC"+Chr(0)+""                             )
   TEST_LINE( L2BIN(1145258561 )              , "ABCD"                                      )

#ifndef __XPP__

   /* __COPYFILE() */

   FClose(FCreate("$$COPYFR.TMP"))

   /* NOTE: Cannot yet test the return value of the function on a DEFAULT-ed
            failure. */

   TEST_LINE( __CopyFile("$$COPYFR.TMP")                 , "E 1 BASE 2010 Argument error (__COPYFILE) OS:0 #:0 A:1:C:$$COPYFR.TMP " )
   TEST_LINE( __CopyFile("$$COPYFR.TMP", "$$COPYTO.TMP") , NIL                                        )
   TEST_LINE( __CopyFile("NOT_HERE.$$$", "$$COPYTO.TMP") , "E 21 BASE 2012 Open error <NOT_HERE.$$$> OS:2 #:1 F:DR" )
   TEST_LINE( __CopyFile("$$COPYFR.TMP", BADFNAME())     , "E 20 BASE 2012 Create error <" + BADFNAME() + "> OS:2 #:1 F:DR" )

   FErase("$$COPYFR.TMP")
   FErase("$$COPYTO.TMP")

#endif /* __XPP__ */

#ifndef __XPP__

   /* __RUN() */

   /* NOTE: Only error cases are tested. */

   TEST_LINE( __RUN()                         , NIL              )
   TEST_LINE( __RUN( NIL )                    , NIL              )
   TEST_LINE( __RUN( 10 )                     , NIL              )

#endif /* __XPP__ */

   /* MEMVARBLOCK() */

   TEST_LINE( MEMVARBLOCK()                   , NIL             )
   TEST_LINE( MEMVARBLOCK( NIL )              , NIL             )
   TEST_LINE( MEMVARBLOCK( 100 )              , NIL             )
   TEST_LINE( MEMVARBLOCK( "mxNotHere" )      , NIL             )
   TEST_LINE( MEMVARBLOCK( "mcString" )       , "{||...}"       )

   /* Defines for HARDCR() and MEMOTRAN() */

   #define SO Chr( 141 )
   #define NU Chr( 0 )
   #define LF Chr( 10 )
   #define CR Chr( 13 )

   /* HARDCR() */

#ifndef __XPP__
   TEST_LINE( HardCR()                                                      , ""                                                                                 )
#endif
   TEST_LINE( HardCR(NIL)                                                   , ""                                                                                 )
   TEST_LINE( HardCR(100)                                                   , ""                                                                                 )
#ifdef __HARBOUR__
   TEST_LINE( HardCR(@scString)                                             , "HELLO"                                                                            ) /* Bug in CA-Cl*pper, it will return "" */
#endif
   TEST_LINE( HardCR("H"+SO+LF+"P"+SO+LF+"W"+SO+"M")                        , "H"+Chr(13)+""+Chr(10)+"P"+Chr(13)+""+Chr(10)+"W"+Chr(141)+"M"                                )
   TEST_LINE( HardCR("H"+NU+"B"+SO+LF+NU+"P"+SO+LF+"W"+SO+"M"+NU)           , "H"+Chr(0)+"B"+Chr(13)+""+Chr(10)+""+Chr(0)+"P"+Chr(13)+""+Chr(10)+"W"+Chr(141)+"M"+Chr(0)+"" )

   /* MEMOTRAN() */

#ifndef __XPP__
   TEST_LINE( MemoTran()                                                    , ""                                                 )
#endif
   TEST_LINE( MemoTran(NIL)                                                 , ""                                                 )
   TEST_LINE( MemoTran(100)                                                 , ""                                                 )
   TEST_LINE( MemoTran(100,"1","2")                                         , ""                                                 )
#ifdef __HARBOUR__
   TEST_LINE( MemoTran(@scString)                                           , "HELLO"                                            ) /* Bug in CA-Cl*pper, it will return "" */
#endif
   TEST_LINE( MemoTran("H"+SO+LF+"P"+CR+LF+"M")                             , "H P;M"                                            )
   TEST_LINE( MemoTran("H"+NU+"O"+SO+LF+"P"+CR+LF+"M"+NU+"I")               , "H"+Chr(0)+"O P;M"+Chr(0)+"I"                      )
   TEST_LINE( MemoTran("M"+CR+"s"+CR+LF+"w"+SO+"w"+SO+LF+"h"+CR)            , "M"+Chr(13)+"s;w"+Chr(141)+"w h"+Chr(13)+""        )
   TEST_LINE( MemoTran("M"+CR+"s"+CR+LF+"w"+SO+"w"+SO+LF+"h"+CR,"111","222"), "M"+Chr(13)+"s1w"+Chr(141)+"w2h"+Chr(13)+""        )
   TEST_LINE( MemoTran("M"+CR+"s"+CR+LF+"w"+SO+"w"+SO+LF+"h"+CR,"","")      , "M"+Chr(13)+"s"+Chr(0)+"w"+Chr(141)+"w"+Chr(0)+"h"+Chr(13)+"" )

   /* MEMOWRITE()/MEMOREAD() */

#ifndef __XPP__
   TEST_LINE( MemoWrit()                         , .F.              )
   TEST_LINE( MemoWrit("$$MEMOFI.TMP")           , .F.              )
#endif
   TEST_LINE( MemoWrit("$$MEMOFI.TMP","")        , .T.              )
   TEST_LINE( MemoRead("$$MEMOFI.TMP")           , ""               )
   TEST_LINE( MemoWrit("$$MEMOFI.TMP",scStringZ) , .T.              )
   TEST_LINE( MemoRead("$$MEMOFI.TMP")           , "A"+Chr(0)+"B"   )
   TEST_LINE( MemoWrit("$$MEMOFI.TMP",Chr(26))   , .T.              )
   TEST_LINE( MemoRead("$$MEMOFI.TMP")           , ""+Chr(26)+""    )
   TEST_LINE( MemoWrit("$$MEMOFI.TMP",scStringW) , .T.              )
   TEST_LINE( MemoRead("$$MEMOFI.TMP")           , ""+Chr(13)+""+Chr(10)+Chr(141)+Chr(10)+""+Chr(9)+"" )
   TEST_LINE( MemoWrit(BADFNAME2()   ,scStringZ) , .F.              )
#ifndef __XPP__
   TEST_LINE( MemoRead()                         , ""               )
#endif
   TEST_LINE( MemoRead( BADFNAME2() )            , ""               )

   FErase("$$MEMOFI.TMP")

#ifdef __HARBOUR__

   /* HB_FNAMESPLIT(), HB_FNAMEMERGE() */

   TEST_LINE( TESTFNAME( ""                            ) , ";;;;"                                                                    )
   TEST_LINE( TESTFNAME( "                           " ) , ";;;;"                                                                    )
#ifdef __PLATFORM__UNIX
   TEST_LINE( TESTFNAME( ":                          " ) , ":;;:;;"                                                                  )
#else
   TEST_LINE( TESTFNAME( ":                          " ) , ":;:;;;"                                                                  )
#endif
   TEST_LINE( TESTFNAME( "C:/work/hello              " ) , "C:/work/hello;C:/work/;hello;;"                                          )
   TEST_LINE( TESTFNAME( "C:/work/hello              " ) , "C:/work/hello;C:/work/;hello;;"                                          )
   TEST_LINE( TESTFNAME( "C:/work/hello              " ) , "C:/work/hello;C:/work/;hello;;"                                          )
   TEST_LINE( TESTFNAME( "C:/work/hello.             " ) , "C:/work/hello.;C:/work/;hello;.;"                                        )
   TEST_LINE( TESTFNAME( "C:/work/hello.prg          " ) , "C:/work/hello.prg;C:/work/;hello;.prg;"                                  )
   TEST_LINE( TESTFNAME( "C:/work/hello/             " ) , "C:/work/hello/;C:/work/hello/;;;"                                        )
   TEST_LINE( TESTFNAME( "C:/work/hello/.prg         " ) , "C:/work/hello/.prg;C:/work/hello/;.prg;;"                                )
   TEST_LINE( TESTFNAME( "C:/work/hello/a.prg        " ) , "C:/work/hello/a.prg;C:/work/hello/;a;.prg;"                              )
   TEST_LINE( TESTFNAME( "C:/work/hello/a.b.prg      " ) , "C:/work/hello/a.b.prg;C:/work/hello/;a.b;.prg;"                          )
   TEST_LINE( TESTFNAME( "C:work/hello               " ) , "C:work/hello;C:work/;hello;;"                                            )
   TEST_LINE( TESTFNAME( "C:work/hello.              " ) , "C:work/hello.;C:work/;hello;.;"                                          )
   TEST_LINE( TESTFNAME( "C:work/hello.prg           " ) , "C:work/hello.prg;C:work/;hello;.prg;"                                    )
   TEST_LINE( TESTFNAME( "C:work/hello/              " ) , "C:work/hello/;C:work/hello/;;;"                                          )
   TEST_LINE( TESTFNAME( "C:work/hello/.prg          " ) , "C:work/hello/.prg;C:work/hello/;.prg;;"                                  )
   TEST_LINE( TESTFNAME( "C:work/hello/a.prg         " ) , "C:work/hello/a.prg;C:work/hello/;a;.prg;"                                )
   TEST_LINE( TESTFNAME( "C:work/hello/a.b.prg       " ) , "C:work/hello/a.b.prg;C:work/hello/;a.b;.prg;"                            )
   TEST_LINE( TESTFNAME( "C:/work.old/hello          " ) , "C:/work.old/hello;C:/work.old/;hello;;"                                  )
   TEST_LINE( TESTFNAME( "C:/work.old/hello.         " ) , "C:/work.old/hello.;C:/work.old/;hello;.;"                                )
   TEST_LINE( TESTFNAME( "C:/work.old/hello.prg      " ) , "C:/work.old/hello.prg;C:/work.old/;hello;.prg;"                          )
   TEST_LINE( TESTFNAME( "C:/work.old/hello/         " ) , "C:/work.old/hello/;C:/work.old/hello/;;;"                                )
   TEST_LINE( TESTFNAME( "C:/work.old/hello/.prg     " ) , "C:/work.old/hello/.prg;C:/work.old/hello/;.prg;;"                        )
   TEST_LINE( TESTFNAME( "C:/work.old/hello/a.prg    " ) , "C:/work.old/hello/a.prg;C:/work.old/hello/;a;.prg;"                      )
   TEST_LINE( TESTFNAME( "C:/work.old/hello/a.b.prg  " ) , "C:/work.old/hello/a.b.prg;C:/work.old/hello/;a.b;.prg;"                  )
   TEST_LINE( TESTFNAME( "C:work.old/hello           " ) , "C:work.old/hello;C:work.old/;hello;;"                                    )
   TEST_LINE( TESTFNAME( "C:work.old/hello.          " ) , "C:work.old/hello.;C:work.old/;hello;.;"                                  )
   TEST_LINE( TESTFNAME( "C:work.old/hello.prg       " ) , "C:work.old/hello.prg;C:work.old/;hello;.prg;"                            )
   TEST_LINE( TESTFNAME( "C:work.old/hello/          " ) , "C:work.old/hello/;C:work.old/hello/;;;"                                  )
   TEST_LINE( TESTFNAME( "C:work.old/hello/.prg      " ) , "C:work.old/hello/.prg;C:work.old/hello/;.prg;;"                          )
   TEST_LINE( TESTFNAME( "C:work.old/hello/a.prg     " ) , "C:work.old/hello/a.prg;C:work.old/hello/;a;.prg;"                        )
   TEST_LINE( TESTFNAME( "C:work.old/hello/a.b.prg   " ) , "C:work.old/hello/a.b.prg;C:work.old/hello/;a.b;.prg;"                    )
   TEST_LINE( TESTFNAME( "C:.old/hello               " ) , "C:.old/hello;C:.old/;hello;;"                                            )
   TEST_LINE( TESTFNAME( "C:.old/hello.              " ) , "C:.old/hello.;C:.old/;hello;.;"                                          )
   TEST_LINE( TESTFNAME( "C:.old/hello.prg           " ) , "C:.old/hello.prg;C:.old/;hello;.prg;"                                    )
   TEST_LINE( TESTFNAME( "C:.old/hello/              " ) , "C:.old/hello/;C:.old/hello/;;;"                                          )
   TEST_LINE( TESTFNAME( "C:.old/hello/.prg          " ) , "C:.old/hello/.prg;C:.old/hello/;.prg;;"                                  )
   TEST_LINE( TESTFNAME( "C:.old/hello/a.prg         " ) , "C:.old/hello/a.prg;C:.old/hello/;a;.prg;"                                )
   TEST_LINE( TESTFNAME( "C:.old/hello/a.b.prg       " ) , "C:.old/hello/a.b.prg;C:.old/hello/;a.b;.prg;"                            )
   TEST_LINE( TESTFNAME( "//server/work/hello        " ) , "//server/work/hello;//server/work/;hello;;"                              )
   TEST_LINE( TESTFNAME( "//server/work/hello.       " ) , "//server/work/hello.;//server/work/;hello;.;"                            )
   TEST_LINE( TESTFNAME( "//server/work/hello.prg    " ) , "//server/work/hello.prg;//server/work/;hello;.prg;"                      )
   TEST_LINE( TESTFNAME( "//server/work/hello/       " ) , "//server/work/hello/;//server/work/hello/;;;"                            )
   TEST_LINE( TESTFNAME( "//server/work/hello/.prg   " ) , "//server/work/hello/.prg;//server/work/hello/;.prg;;"                    )
   TEST_LINE( TESTFNAME( "//server/work/hello/a.prg  " ) , "//server/work/hello/a.prg;//server/work/hello/;a;.prg;"                  )
   TEST_LINE( TESTFNAME( "//server/work/hello/a.b.prg" ) , "//server/work/hello/a.b.prg;//server/work/hello/;a.b;.prg;"              )
   TEST_LINE( TESTFNAME( "/server/work/hello         " ) , "/server/work/hello;/server/work/;hello;;"                                )
   TEST_LINE( TESTFNAME( "/server/work/hello.        " ) , "/server/work/hello.;/server/work/;hello;.;"                              )
   TEST_LINE( TESTFNAME( "/server/work/hello.prg     " ) , "/server/work/hello.prg;/server/work/;hello;.prg;"                        )
   TEST_LINE( TESTFNAME( "/server/work/hello/        " ) , "/server/work/hello/;/server/work/hello/;;;"                              )
   TEST_LINE( TESTFNAME( "/server/work/hello/.prg    " ) , "/server/work/hello/.prg;/server/work/hello/;.prg;;"                      )
   TEST_LINE( TESTFNAME( "/server/work/hello/a.prg   " ) , "/server/work/hello/a.prg;/server/work/hello/;a;.prg;"                    )
   TEST_LINE( TESTFNAME( "/server/work/hello/a.b.prg " ) , "/server/work/hello/a.b.prg;/server/work/hello/;a.b;.prg;"                )
   TEST_LINE( TESTFNAME( "C:/hello                   " ) , "C:/hello;C:/;hello;;"                                                    )
   TEST_LINE( TESTFNAME( "C:/hello.                  " ) , "C:/hello.;C:/;hello;.;"                                                  )
   TEST_LINE( TESTFNAME( "C:/hello.prg               " ) , "C:/hello.prg;C:/;hello;.prg;"                                            )
   TEST_LINE( TESTFNAME( "C:/hello/                  " ) , "C:/hello/;C:/hello/;;;"                                                  )
   TEST_LINE( TESTFNAME( "C:/hello/.prg              " ) , "C:/hello/.prg;C:/hello/;.prg;;"                                          )
   TEST_LINE( TESTFNAME( "C:/hello/a.prg             " ) , "C:/hello/a.prg;C:/hello/;a;.prg;"                                        )
   TEST_LINE( TESTFNAME( "C:/hello/a.b.prg           " ) , "C:/hello/a.b.prg;C:/hello/;a.b;.prg;"                                    )
#ifdef __PLATFORM__UNIX
   TEST_LINE( TESTFNAME( "C:hello                    " ) , "C:hello;;C:hello;;"                                                      )
   TEST_LINE( TESTFNAME( "C:hello.                   " ) , "C:hello.;;C:hello;.;"                                                    )
   TEST_LINE( TESTFNAME( "C:hello.prg                " ) , "C:hello.prg;;C:hello;.prg;"                                              )
#else
   TEST_LINE( TESTFNAME( "C:hello                    " ) , "C:hello;C:;hello;;"                                                      )
   TEST_LINE( TESTFNAME( "C:hello.                   " ) , "C:hello.;C:;hello;.;"                                                    )
   TEST_LINE( TESTFNAME( "C:hello.prg                " ) , "C:hello.prg;C:;hello;.prg;"                                              )
#endif
   TEST_LINE( TESTFNAME( "C:hello/                   " ) , "C:hello/;C:hello/;;;"                                                    )
   TEST_LINE( TESTFNAME( "C:hello/.prg               " ) , "C:hello/.prg;C:hello/;.prg;;"                                            )
   TEST_LINE( TESTFNAME( "C:hello/a.prg              " ) , "C:hello/a.prg;C:hello/;a;.prg;"                                          )
   TEST_LINE( TESTFNAME( "C:hello/a.b.prg            " ) , "C:hello/a.b.prg;C:hello/;a.b;.prg;"                                      )
   TEST_LINE( TESTFNAME( "//hello                    " ) , "//hello;//;hello;;"                                                      )
   TEST_LINE( TESTFNAME( "//hello.                   " ) , "//hello.;//;hello;.;"                                                    )
   TEST_LINE( TESTFNAME( "//hello.prg                " ) , "//hello.prg;//;hello;.prg;"                                              )
   TEST_LINE( TESTFNAME( "//hello/                   " ) , "//hello/;//hello/;;;"                                                    )
   TEST_LINE( TESTFNAME( "//.prg                     " ) , "//.prg;//;.prg;;"                                                        )
   TEST_LINE( TESTFNAME( "//a.prg                    " ) , "//a.prg;//;a;.prg;"                                                      )
   TEST_LINE( TESTFNAME( "//a.b.prg                  " ) , "//a.b.prg;//;a.b;.prg;"                                                  )
   TEST_LINE( TESTFNAME( "/hello                     " ) , "/hello;/;hello;;"                                                        )
   TEST_LINE( TESTFNAME( "/hello.                    " ) , "/hello.;/;hello;.;"                                                      )
   TEST_LINE( TESTFNAME( "/hello.prg                 " ) , "/hello.prg;/;hello;.prg;"                                                )
   TEST_LINE( TESTFNAME( "/hello/                    " ) , "/hello/;/hello/;;;"                                                      )
   TEST_LINE( TESTFNAME( "/hello/.prg                " ) , "/hello/.prg;/hello/;.prg;;"                                              )
   TEST_LINE( TESTFNAME( "/hello/a.prg               " ) , "/hello/a.prg;/hello/;a;.prg;"                                            )
   TEST_LINE( TESTFNAME( "/hello/a.b.prg             " ) , "/hello/a.b.prg;/hello/;a.b;.prg;"                                        )
   TEST_LINE( TESTFNAME( "hello                      " ) , "hello;;hello;;"                                                          )
   TEST_LINE( TESTFNAME( "hello.                     " ) , "hello.;;hello;.;"                                                        )
   TEST_LINE( TESTFNAME( "hello.prg                  " ) , "hello.prg;;hello;.prg;"                                                  )
   TEST_LINE( TESTFNAME( "hello/                     " ) , "hello/;hello/;;;"                                                        )
   TEST_LINE( TESTFNAME( ".prg                       " ) , ".prg;;.prg;;"                                                            )
   TEST_LINE( TESTFNAME( "a.prg                      " ) , "a.prg;;a;.prg;"                                                          )
   TEST_LINE( TESTFNAME( "a.b.prg                    " ) , "a.b.prg;;a.b;.prg;"                                                      )
   TEST_LINE( TESTFNAME( "                           " ) , ";;;;"                                                                    )
   TEST_LINE( TESTFNAME( "/                          " ) , "/;/;;;"                                                                  )
   TEST_LINE( TESTFNAME( "//                         " ) , "//;//;;;"                                                                )
   TEST_LINE( TESTFNAME( "C                          " ) , "C;;C;;"                                                                  )
#ifdef __PLATFORM__UNIX
   TEST_LINE( TESTFNAME( "C:                         " ) , "C:;;C:;;"                                                                )
#else
   TEST_LINE( TESTFNAME( "C:                         " ) , "C:;C:;;;"                                                                )
#endif
   TEST_LINE( TESTFNAME( "C:/                        " ) , "C:/;C:/;;;"                                                              )
   TEST_LINE( TESTFNAME( "C://                       " ) , "C://;C://;;;"                                                            )

#endif

   /* MLCTOPOS() */

#ifdef __HARBOUR__
   cEOL := Set( _SET_EOL, CHR(13) + CHR( 10 ) )
#endif

   TEST_LINE( MLCTOPOS( 'A message from me', 10, 2, 0 )                                , 11 )
   TEST_LINE( MLCTOPOS( 'A message from me', 5, 2, 0, 4, .F. )                         ,  6 )
   TEST_LINE( MLCTOPOS( 'A'+CHR(9)+'message'+CHR(9)+'from'+CHR(9)+'me', 10, 2, 0, 8 )  ,  3 )
   TEST_LINE( MLCTOPOS( 'abcd efg hijk lm nopqr stu vwxyz', 5, 3, 0 )                  , 10 )
   TEST_LINE( MLCTOPOS( 'abcd efg hijk lm nopqr stu vwxyz', 8, 2, 0 )                  , 10 )
   TEST_LINE( MLCTOPOS( 'abcd efg hijk lm nopqr stu vwxyz', 8, 2, 0,, .F. )            ,  9 )
   TEST_LINE( MLCTOPOS( 'A message from our me', 9, 2, 0 )                             , 11 )
   TEST_LINE( MLCTOPOS( 'A message  from our me', 9, 2, 0 )                            , 11 )
   TEST_LINE( MLCTOPOS( 'A message'+CHR(9)+'from me', 10, 2, 0 )                       , 11 )
   TEST_LINE( MLCTOPOS( 'A message from me', 9, 2, 0,, .F. )                           , 10 )
   TEST_LINE( MLCTOPOS( 'A message  from me', 9, 2, 0,, .F. )                          , 10 )
   TEST_LINE( MLCTOPOS( 'A message'+CHR(141)+'from me', 10, 2, 0 )                     ,  3 )
   TEST_LINE( MLCTOPOS( 'A message'+CHR(141)+'from me', 9, 2, 0 )                      ,  3 )
   TEST_LINE( MLCTOPOS( 'A message'+CHR(141)+'from me', 10, 2, 0,, .F. )               , 11 )
   TEST_LINE( MLCTOPOS( 'A message'+CHR(141)+'from me', 9, 2, 0,, .F. )                , 10 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 0 )                                 ,  1 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 8 )                                 ,  9 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 9 )                                 , 10 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 10 )                                , 11 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 11 )                                , 12 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 360 )                               , 17 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 0,, .F. )                           ,  1 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 8,, .F. )                           ,  9 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 9,, .F. )                           , 10 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 10,, .F. )                          , 11 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 11,, .F. )                          , 12 )
   TEST_LINE( MLCTOPOS( ' message from me', 10, 1, 360,, .F. )                         , 17 )
   TEST_LINE( MLCTOPOS( ' message'+CHR(9)+'from me', 10, 1, 11,, .T. )                 ,  9 )
   TEST_LINE( MLCTOPOS( ' message'+CHR(9)+'from me', 10, 1, 11,, .F. )                 ,  9 )
   TEST_LINE( MLCTOPOS( ' message'+CHR(9)+'from me', 10, 2, 11 )                       , 17 )
   TEST_LINE( MLCTOPOS( ' message'+CHR(9)+'from me', 10, 1, 15,, .T. )                 , 13 )
   TEST_LINE( MLCTOPOS( ' message'+CHR(9)+'from me', 10, 1, 15,, .F. )                 , 13 )
   TEST_LINE( MLCTOPOS( CHR(13)+CHR(10)+' message'+CHR(9)+'from me', 10, 1, 15,, .F. ) ,  1 )
   TEST_LINE( MLCTOPOS( CHR(13)+CHR(10)+' message'+CHR(9)+'from me', 10, 1, 15,, .T. ) ,  1 )
   TEST_LINE( MLCTOPOS( 'A '+CHR(13)+CHR(10)+'message from me', 9, 2, 0 )              ,  5 )
   TEST_LINE( MLCTOPOS( 'A '+CHR(141)+CHR(10)+'message from me', 9, 2, 0 )             , 13 )
   TEST_LINE( MLCTOPOS( 'A'+CHR(141)+CHR(10)+'message from me', 9, 2, 0 )              , 12 )
   TEST_LINE( MLCTOPOS( 'A'+CHR(141)+'message from me', 9, 2, 0 )                      , 11 )
   TEST_LINE( MLCTOPOS( 'A'+CHR(13)+'message from me', 9, 2, 0 )                       , 11 )
   TEST_LINE( MLCTOPOS( 'A'+CHR(10)+'message from me', 9, 2, 0 )                       , 11 )
   TEST_LINE( MLCTOPOS( 'A '+CHR(13)+'message from me', 9, 2, 0 )                      ,  3 )
   TEST_LINE( MLCTOPOS( 'A '+CHR(10)+'message from me', 9, 2, 0 )                      ,  3 )
   TEST_LINE( MLCTOPOS( 'A message from me', 10, 7, 0 )                                , 18 )
   TEST_LINE( MLCTOPOS( , , ,  )                                                       ,  1 )
   TEST_LINE( MLCTOPOS( , .T., ,  )                                                    ,  1 )

#ifdef __HARBOUR__
   Set( _SET_EOL, cEOL )
#endif

   RETURN

#ifdef __HARBOUR__

PROCEDURE Main_OPOVERL()
   LOCAL oString := HB_TString()

   oString:cValue := "Hello"

   TEST_LINE( oString =  "Hello"        , .T.                 )
   TEST_LINE( oString == "Hello"        , .T.                 )
   TEST_LINE( oString != "Hello"        , .F.                 )
   TEST_LINE( oString <> "Hello"        , .F.                 )
   TEST_LINE( oString #  "Hello"        , .F.                 )
   TEST_LINE( oString $  "Hello"        , .T.                 )
   TEST_LINE( oString <  "Hello"        , .F.                 )
   TEST_LINE( oString <= "Hello"        , .T.                 )
   TEST_LINE( oString <  "Hello"        , .F.                 )
   TEST_LINE( oString <= "Hello"        , .T.                 )
   TEST_LINE( oString +  "Hello"        , "HelloHello"        )
   TEST_LINE( oString -  "Hello"        , "HelloHello"        )
   TEST_LINE( oString * 3               , "HelloHelloHello"   )
   TEST_LINE( oString / 2               , "He"                )
   TEST_LINE( oString % "TST"           , "Hello % TST"       )
   TEST_LINE( oString ^ "TST"           , "Hello ^ TST"       )
   TEST_LINE( oString ** "TST"          , "Hello ^ TST"       )
   IF !TEST_OPT_Z()
   TEST_LINE( oString .AND. "TST"       , "Hello AND TST"     )
   TEST_LINE( oString .OR. "TST"        , "Hello OR TST"      )
   ENDIF
   TEST_LINE( .NOT. oString             , "∏õîîë"             )
   TEST_LINE( !oString                  , "∏õîîë"             )
   TEST_LINE( oString++                 , "HB_TSTRING Object" )
   TEST_LINE( oString:cValue            , "Hello "            )
   TEST_LINE( oString--                 , "HB_TSTRING Object" )
   TEST_LINE( oString:cValue            , "Hello"             )

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

      oClass:AddInline( "HasMsg", {| self, cMsg | HB_SYMBOL_UNUSED( self ), __ObjHasMsg( QSelf(), cMsg ) } )

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

   HB_FNameSplit( RTrim( cFull ), @cPath, @cName, @cExt, @cDrive )

   RETURN HB_FNameMerge( cPath, cName, cExt ) + ";" + ;
          cPath + ";" +;
          cName + ";" +;
          cExt + ";" +;
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

/* Don't change the position of this #include. */
#include "rt_init.ch"
