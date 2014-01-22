/*
 * Harbour Project source code:
 * Regression tests for the runtime library (HVM)
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

PROCEDURE Main_HVM()

   /* ValType() */

   HBTEST ValType(  scString  )           IS "C"
   HBTEST ValType(  scStringE )           IS "C"
   HBTEST ValType(  scStringZ )           IS "C"
   HBTEST ValType(  snIntZ    )           IS "N"
   HBTEST ValType(  snDoubleZ )           IS "N"
   HBTEST ValType(  snIntP    )           IS "N"
   HBTEST ValType(  snLongP   )           IS "N"
   HBTEST ValType(  snDoubleP )           IS "N"
   HBTEST ValType(  snIntN    )           IS "N"
   HBTEST ValType(  snLongN   )           IS "N"
   HBTEST ValType(  snDoubleN )           IS "N"
   HBTEST ValType(  snDoubleI )           IS "N"
   HBTEST ValType(  sdDateE   )           IS "D"
   HBTEST ValType(  slFalse   )           IS "L"
   HBTEST ValType(  slTrue    )           IS "L"
   HBTEST ValType(  soObject  )           IS "O"
   HBTEST ValType(  suNIL     )           IS "U"
   HBTEST ValType(  sbBlock   )           IS "B"
   HBTEST ValType(  saArray   )           IS "A"
   HBTEST ValType( { 1, 2, 3 } )          IS "A"
   IF TEST_DBFAvail()
   HBTEST ValType( w_TEST->TYPE_C )       IS "C"
   HBTEST ValType( w_TEST->TYPE_D )       IS "D"
   HBTEST ValType( w_TEST->TYPE_M )       IS "M"
   HBTEST ValType( w_TEST->TYPE_N_I )     IS "N"
   HBTEST ValType( w_TEST->TYPE_N_D )     IS "N"
   HBTEST ValType( w_TEST->TYPE_L )       IS "L"
   ENDIF
#ifdef __HARBOUR__
   HBTEST ValType( @scString  )           IS "C"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @scStringE )           IS "C"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @scStringZ )           IS "C"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snIntZ    )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snDoubleZ )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snIntP    )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snLongP   )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snDoubleP )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snIntN    )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snLongN   )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snDoubleN )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @snDoubleI )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @sdDateE   )           IS "D"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @slFalse   )           IS "L"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @slTrue    )           IS "L"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @soObject  )           IS "O"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @suNIL     )           IS "U"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @sbBlock   )           IS "B"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @saArray   )           IS "A"  /* Bug in CA-Cl*pper, it will return "U" */
#endif
   HBTEST ValType(  mcString  )           IS "C"
   HBTEST ValType(  mcStringE )           IS "C"
   HBTEST ValType(  mcStringZ )           IS "C"
   HBTEST ValType(  mnIntZ    )           IS "N"
   HBTEST ValType(  mnDoubleZ )           IS "N"
   HBTEST ValType(  mnIntP    )           IS "N"
   HBTEST ValType(  mnLongP   )           IS "N"
   HBTEST ValType(  mnDoubleP )           IS "N"
   HBTEST ValType(  mnIntN    )           IS "N"
   HBTEST ValType(  mnLongN   )           IS "N"
   HBTEST ValType(  mnDoubleN )           IS "N"
   HBTEST ValType(  mnDoubleI )           IS "N"
   HBTEST ValType(  mdDateE   )           IS "D"
   HBTEST ValType(  mlFalse   )           IS "L"
   HBTEST ValType(  mlTrue    )           IS "L"
   HBTEST ValType(  moObject  )           IS "O"
   HBTEST ValType(  muNIL     )           IS "U"
   HBTEST ValType(  mbBlock   )           IS "B"
   HBTEST ValType(  maArray   )           IS "A"
#ifdef __HARBOUR__
   HBTEST ValType( @mcString  )           IS "C"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mcStringE )           IS "C"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mcStringZ )           IS "C"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnIntZ    )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnDoubleZ )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnIntP    )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnLongP   )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnDoubleP )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnIntN    )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnLongN   )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnDoubleN )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mnDoubleI )           IS "N"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mdDateE   )           IS "D"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mlFalse   )           IS "L"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mlTrue    )           IS "L"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @moObject  )           IS "O"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @muNIL     )           IS "U"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @mbBlock   )           IS "B"  /* Bug in CA-Cl*pper, it will return "U" */
   HBTEST ValType( @maArray   )           IS "A"  /* Bug in CA-Cl*pper, it will return "U" */
#endif

   /* Type() */

#ifndef __XPP__
   HBTEST Type( NIL )                     IS "E 1 BASE 1121 Argument error (TYPE) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Type( 100 )                     IS "E 1 BASE 1121 Argument error (TYPE) OS:0 #:0 A:1:N:100 F:S"
   HBTEST Type( {} )                      IS "E 1 BASE 1121 Argument error (TYPE) OS:0 #:0 A:1:A:{.[0].} F:S"
#endif
   IF TEST_DBFAvail()
   HBTEST Type( "w_TEST->TYPE_C" )        IS "C"
   HBTEST Type( "w_TEST->TYPE_D" )        IS "D"
   HBTEST Type( "w_TEST->TYPE_M" )        IS "M"
   HBTEST Type( "w_TEST->TYPE_N_I" )      IS "N"
   HBTEST Type( "w_TEST->TYPE_N_D" )      IS "N"
   HBTEST Type( "w_TEST->TYPE_L" )        IS "L"
   ENDIF
   HBTEST Type( "mxNotHere"  )            IS "U"
   HBTEST Type( "mcString"  )             IS "C"
   HBTEST Type( "mcStringE" )             IS "C"
   HBTEST Type( "mcStringZ" )             IS "C"
   HBTEST Type( "mnIntZ"    )             IS "N"
   HBTEST Type( "mnDoubleZ" )             IS "N"
   HBTEST Type( "mnIntP"    )             IS "N"
   HBTEST Type( "mnLongP"   )             IS "N"
   HBTEST Type( "mnDoubleP" )             IS "N"
   HBTEST Type( "mnIntN"    )             IS "N"
   HBTEST Type( "mnLongN"   )             IS "N"
   HBTEST Type( "mnDoubleN" )             IS "N"
   HBTEST Type( "mnDoubleI" )             IS "N"
   HBTEST Type( "mdDateE"   )             IS "D"
   HBTEST Type( "mlFalse"   )             IS "L"
   HBTEST Type( "mlTrue"    )             IS "L"
   HBTEST Type( "moObject"  )             IS "O"
   HBTEST Type( "muNIL"     )             IS "U"
   HBTEST Type( "mbBlock"   )             IS "B"
   HBTEST Type( "maArray"   )             IS "A"

   /* Special internal messages */

/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   HBTEST NIL:className()                 IS "NIL"
#endif
#endif
#ifndef __XPP__
   HBTEST "":className()                  IS "CHARACTER"
   HBTEST 0:className()                   IS "NUMERIC"
   HBTEST hb_SToD( "" ):className()       IS "DATE"
   HBTEST .F.:className()                 IS "LOGICAL"
   HBTEST {|| NIL }:className()           IS "BLOCK"
   HBTEST {}:className()                  IS "ARRAY"
#endif
   HBTEST ErrorNew():className()          IS "ERROR"
   HBTEST ErrorNew():className            IS "ERROR"
/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   HBTEST NIL:classH()                    IS 0
#endif
#endif
#ifndef __XPP__
   HBTEST "":classH()                     IS 0
   HBTEST 0:classH()                      IS 0
   HBTEST hb_SToD( "" ):classH()          IS 0
   HBTEST .F.:classH()                    IS 0
   HBTEST {|| NIL }:classH()              IS 0
   HBTEST {}:classH()                     IS 0
#endif
   HBTEST ErrorNew():classH() > 0         IS .T.
   HBTEST ErrorNew():classH > 0           IS .T.

/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   HBTEST suNIL:className()               IS "NIL"
#endif
#endif
#ifndef __XPP__
   HBTEST scString:className()            IS "CHARACTER"
   HBTEST snIntP:className()              IS "NUMERIC"
   HBTEST sdDateE:className()             IS "DATE"
   HBTEST slFalse:className()             IS "LOGICAL"
   HBTEST sbBlock:className()             IS "BLOCK"
   HBTEST saArray:className()             IS "ARRAY"
#endif
   HBTEST soObject:className()            IS "ERROR"
   HBTEST soObject:className              IS "ERROR"
/* Harbour compiler not yet handles these */
#ifndef __HARBOUR__
#ifndef __XPP__
   HBTEST suNIL:classH()                  IS 0
#endif
#endif
#ifndef __XPP__
   HBTEST scString:classH()               IS 0
   HBTEST snIntP:classH()                 IS 0
   HBTEST sdDateE:classH()                IS 0
   HBTEST slFalse:classH()                IS 0
   HBTEST sbBlock:classH()                IS 0
   HBTEST saArray:classH()                IS 0
#endif
   HBTEST soObject:classH() > 0           IS .T.
   HBTEST soObject:classH > 0             IS .T.

   /* (operators) */

   /* <= */

   HBTEST 2                     <= 1                     IS .F.
   HBTEST 1                     <= 2                     IS .T.
   HBTEST 2.0                   <= 2                     IS .T.
   HBTEST 2                     <= 2.0                   IS .T.
   HBTEST 2.5                   <= 3.7                   IS .T.
   HBTEST 3.7                   <= 2.5                   IS .F.
   HBTEST 10                    <= 10.50                 IS .T.
   HBTEST .F.                   <= .F.                   IS .T.
   HBTEST .T.                   <= .F.                   IS .F.
   HBTEST .F.                   <= .T.                   IS .T.
   HBTEST hb_SToD( "" )         <= hb_SToD( "" )         IS .T.
   HBTEST hb_SToD( "" )         <= hb_SToD( "19800101" ) IS .T.
   HBTEST hb_SToD( "19800101" ) <= hb_SToD( "" )         IS .F.
   HBTEST ""                    <= "AAA"                 IS .T.
   HBTEST "AAA"                 <= ""                    IS .T.
   HBTEST "AAA"                 <= "AA"                  IS .T.
   HBTEST "AAA"                 <= Chr( 255 )            IS .T.
   HBTEST Chr( 150 )            <= Chr( 255 )            IS .T.
   HBTEST "A"                   <= "a"                   IS .T.
   HBTEST "A"                   <= "Z"                   IS .T.
   HBTEST "Z"                   <= " "                   IS .F.
   HBTEST Chr( 0 )              <= " "                   IS .T.
   HBTEST "Hallo"               <= "Hello"               IS .T.
   HBTEST "Hello"               <= "Hello"               IS .T.
   HBTEST "Hell"                <= "Hello"               IS .T.
   HBTEST "Hellow"              <= "Hello"               IS .T.
   HBTEST "J"                   <= "Hello"               IS .F.
   HBTEST ""                    <= "Hello"               IS .T.
   HBTEST "J"                   <= ""                    IS .T.
   HBTEST ""                    <= ""                    IS .T.

   /* < */

   HBTEST 2                     <  1                     IS .F.
   HBTEST 1                     <  2                     IS .T.
   HBTEST 2.0                   <  2                     IS .F.
   HBTEST 2                     <  2.0                   IS .F.
   HBTEST 2.5                   <  3.7                   IS .T.
   HBTEST 3.7                   <  2.5                   IS .F.
   HBTEST 10.50                 <  10                    IS .F.
   HBTEST .F.                   <  .F.                   IS .F.
   HBTEST .T.                   <  .F.                   IS .F.
   HBTEST .F.                   <  .T.                   IS .T.
   HBTEST hb_SToD( "" )         <  hb_SToD( "" )         IS .F.
   HBTEST hb_SToD( "" )         <  hb_SToD( "19800101" ) IS .T.
   HBTEST hb_SToD( "19800101" ) <  hb_SToD( "" )         IS .F.
   HBTEST ""                    <  "AAA"                 IS .T.
   HBTEST "AAA"                 <  ""                    IS .F.
   HBTEST "AAA"                 <  "AA"                  IS .F.
   HBTEST "AAA"                 <  Chr( 255 )            IS .T.
   HBTEST Chr( 150 )            <  Chr( 255 )            IS .T.
   HBTEST "A"                   <  "a"                   IS .T.
   HBTEST "A"                   <  "Z"                   IS .T.
   HBTEST "Z"                   <  "A"                   IS .F.
   HBTEST Chr( 0 )              <  " "                   IS .T.
   HBTEST "Hallo"               <  "Hello"               IS .T.
   HBTEST "Hello"               <  "Hello"               IS .F.
   HBTEST "Hell"                <  "Hello"               IS .T.
   HBTEST "Hellow"              <  "Hello"               IS .F.
   HBTEST "J"                   <  "Hello"               IS .F.
   HBTEST ""                    <  "Hello"               IS .T.
   HBTEST "J"                   <  ""                    IS .F.
   HBTEST ""                    <  ""                    IS .F.

   /* >= */

   HBTEST 2                     >= 1                     IS .T.
   HBTEST 1                     >= 2                     IS .F.
   HBTEST 2.0                   >= 2                     IS .T.
   HBTEST 2                     >= 2.0                   IS .T.
   HBTEST 2.5                   >= 3.7                   IS .F.
   HBTEST 3.7                   >= 2.5                   IS .T.
   HBTEST 10.50                 >= 10                    IS .T.
   HBTEST .F.                   >= .F.                   IS .T.
   HBTEST .T.                   >= .F.                   IS .T.
   HBTEST .F.                   >= .T.                   IS .F.
   HBTEST hb_SToD( "" )         >= hb_SToD( "" )         IS .T.
   HBTEST hb_SToD( "" )         >= hb_SToD( "19800101" ) IS .F.
   HBTEST hb_SToD( "19800101" ) >= hb_SToD( "" )         IS .T.
   HBTEST ""                    >= "AAA"                 IS .F.
   HBTEST "AAA"                 >= ""                    IS .T.
   HBTEST "AAA"                 >= "AA"                  IS .T.
   HBTEST "AAA"                 >= Chr( 255 )            IS .F.
   HBTEST Chr( 150 )            >= Chr( 255 )            IS .F.
   HBTEST "A"                   >= "a"                   IS .F.
   HBTEST "A"                   >= "Z"                   IS .F.
   HBTEST "Z"                   >= "A"                   IS .T.
   HBTEST Chr( 0 )              >= " "                   IS .F.
   HBTEST "Hallo"               >= "Hello"               IS .F.
   HBTEST "Hello"               >= "Hello"               IS .T.
   HBTEST "Hell"                >= "Hello"               IS .F.
   HBTEST "Hellow"              >= "Hello"               IS .T.
   HBTEST "J"                   >= "Hello"               IS .T.
   HBTEST ""                    >= "Hello"               IS .F.
   HBTEST "J"                   >= ""                    IS .T.
   HBTEST ""                    >= ""                    IS .T.

   /* > */

   HBTEST 2                     >  1                     IS .T.
   HBTEST 1                     >  2                     IS .F.
   HBTEST 2.0                   >  2                     IS .F.
   HBTEST 2                     >  2.0                   IS .F.
   HBTEST 2.5                   >  3.7                   IS .F.
   HBTEST 3.7                   >  2.5                   IS .T.
   HBTEST 10.50                 >  10                    IS .T.
   HBTEST .F.                   >  .F.                   IS .F.
   HBTEST .T.                   >  .F.                   IS .T.
   HBTEST .F.                   >  .T.                   IS .F.
   HBTEST hb_SToD( "" )         >  hb_SToD( "" )         IS .F.
   HBTEST hb_SToD( "" )         >  hb_SToD( "19800101" ) IS .F.
   HBTEST hb_SToD( "19800101" ) >  hb_SToD( "" )         IS .T.
   HBTEST ""                    >  "AAA"                 IS .F.
   HBTEST "AAA"                 >  ""                    IS .F.
   HBTEST "AAA"                 >  "AA"                  IS .F.
   HBTEST "AAA"                 >  Chr( 255 )            IS .F.
   HBTEST Chr( 150 )            >  Chr( 255 )            IS .F.
   HBTEST "A"                   >  "a"                   IS .F.
   HBTEST "A"                   >  "Z"                   IS .F.
   HBTEST "Z"                   >  "A"                   IS .T.
   HBTEST Chr( 0 )              >  " "                   IS .F.
   HBTEST "Hallo"               >  "Hello"               IS .F.
   HBTEST "Hello"               >  "Hello"               IS .F.
   HBTEST "Hell"                >  "Hello"               IS .F.
   HBTEST "Hellow"              >  "Hello"               IS .F.
   HBTEST "J"                   >  "Hello"               IS .T.
   HBTEST ""                    >  "Hello"               IS .F.
   HBTEST "J"                   >  ""                    IS .F.
   HBTEST ""                    >  ""                    IS .F.

   /* =, == */

   SET EXACT ON
   HBTEST "123" = "123  "                 IS .T.
   HBTEST " 123" = "123"                  IS .F.
   HBTEST "123" = "12345"                 IS .F.
   HBTEST "12345" = "123"                 IS .F.
   HBTEST "123" = ""                      IS .F.
   HBTEST "" = "123"                      IS .F.
   HBTEST "A" == "A"                      IS .T.
   HBTEST "Z" == "A"                      IS .F.
   HBTEST "A" == "A "                     IS .F.
   HBTEST "AA" == "A"                     IS .F.
#ifdef __HARBOUR__
   HBTEST hb_LeftIs( "123", "123  " )     IS .F.
   HBTEST hb_LeftIs( " 123", "123" )      IS .F.
   HBTEST hb_LeftIs( "123", "12345" )     IS .F.
   HBTEST hb_LeftIs( "12345", "123" )     IS .T.
   HBTEST hb_LeftIs( "123", "" )          IS .T.
   HBTEST hb_LeftIs( "", "123" )          IS .F.
#endif
   SET EXACT OFF
   HBTEST "123" = "123  "                 IS .F.
   HBTEST " 123" = "123"                  IS .F.
   HBTEST "123" = "12345"                 IS .F.
   HBTEST "12345" = "123"                 IS .T.
   HBTEST "123" = ""                      IS .T.
   HBTEST "" = "123"                      IS .F.
   HBTEST "A" == "A"                      IS .T.
   HBTEST "Z" == "A"                      IS .F.
   HBTEST "A" == "A "                     IS .F.
   HBTEST "AA" == "A"                     IS .F.
#ifdef __HARBOUR__
   HBTEST hb_LeftIs( "123", "123  " )     IS .F.
   HBTEST hb_LeftIs( " 123", "123" )      IS .F.
   HBTEST hb_LeftIs( "123", "12345" )     IS .F.
   HBTEST hb_LeftIs( "12345", "123" )     IS .T.
   HBTEST hb_LeftIs( "123", "" )          IS .T.
   HBTEST hb_LeftIs( "", "123" )          IS .F.
#endif
   HBTEST "Hallo"          == "Hello"     IS .F.
   HBTEST "Hello"          == "Hello"     IS .T.
   HBTEST "Hell"           == "Hello"     IS .F.
   HBTEST "Hellow"         == "Hello"     IS .F.
   HBTEST "J"              == "Hello"     IS .F.
   HBTEST ""               == "Hello"     IS .F.
   HBTEST "J"              == ""          IS .F.
   HBTEST ""               == ""          IS .T.

   HBTEST scString  = scString            IS .T.
   HBTEST scString  = scStringE           IS .T.
   HBTEST scString  = scStringZ           IS .F.
   HBTEST scStringE = scString            IS .F.
   HBTEST scStringE = scStringE           IS .T.
   HBTEST scStringE = scStringZ           IS .F.
   HBTEST scStringZ = scString            IS .F.
   HBTEST scStringZ = scStringE           IS .T.
   HBTEST scStringZ = scStringZ           IS .T.

   /* != */

   SET EXACT ON
   HBTEST "123" != "123  "                IS .F.
   HBTEST " 123" != "123"                 IS .T.
   HBTEST "123" != "12345"                IS .T.
   HBTEST "12345" != "123"                IS .T.
   HBTEST "123" != ""                     IS .T.
   HBTEST "" != "123"                     IS .T.
   HBTEST "A" != "A"                      IS .F.
   HBTEST "Z" != "A"                      IS .T.
   HBTEST "A" != "A "                     IS .F.
   HBTEST "AA" != "A"                     IS .T.
   SET EXACT OFF
   HBTEST "123" != "123  "                IS .T.
   HBTEST " 123" != "123"                 IS .T.
   HBTEST "123" != "12345"                IS .T.
   HBTEST "12345" != "123"                IS .F.
   HBTEST "123" != ""                     IS .F.
   HBTEST "" != "123"                     IS .T.
   HBTEST "A" != "A"                      IS .F.
   HBTEST "Z" != "A"                      IS .T.
   HBTEST "A" != "A "                     IS .T.
   HBTEST "AA" != "A"                     IS .F.
   HBTEST "Hallo"          != "Hello"     IS .T.
   HBTEST "Hello"          != "Hello"     IS .F.
   HBTEST "Hell"           != "Hello"     IS .T.
   HBTEST "Hellow"         != "Hello"     IS .F.
   HBTEST "J"              != "Hello"     IS .T.
   HBTEST ""               != "Hello"     IS .T.
   HBTEST "J"              != ""          IS .F.
   HBTEST ""               != ""          IS .F.

   HBTEST scString  != scString           IS .F.
   HBTEST scString  != scStringE          IS .F.
   HBTEST scString  != scStringZ          IS .T.
   HBTEST scStringE != scString           IS .T.
   HBTEST scStringE != scStringE          IS .F.
   HBTEST scStringE != scStringZ          IS .T.
   HBTEST scStringZ != scString           IS .T.
   HBTEST scStringZ != scStringE          IS .F.
   HBTEST scStringZ != scStringZ          IS .F.

   /* == special */

   HBTEST NIL == NIL                      IS .T.
   HBTEST 1 == NIL                        IS .F.
   HBTEST NIL == 1                        IS .F.
   HBTEST "" == NIL                       IS .F.
   HBTEST NIL == ""                       IS .F.
   HBTEST 1 == suNIL                      IS .F.
   HBTEST suNIL == 1                      IS .F.
   HBTEST "" == suNIL                     IS .F.
   HBTEST suNIL == ""                     IS .F.
   HBTEST scString == NIL                 IS .F.
   HBTEST "A" == 1                        IS "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:C:A;N:1 F:S"
   HBTEST scString == 1                   IS "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST soObject == ""                  IS "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST soObject == soObject            IS .T.
   HBTEST soObject == ErrorNew()          IS .F.
   HBTEST ErrorNew() == ErrorNew()        IS .F.
   HBTEST soObject == TBColumnNew()       IS .F.
   HBTEST saArray == saArray              IS .T.
   HBTEST {} == {}                        IS .F.
   HBTEST {|| NIL } == {|| NIL }          IS "E 1 BASE 1070 Argument error (==) OS:0 #:0 A:2:B:{||...};B:{||...} F:S"

   /* = special */

   HBTEST NIL = NIL                       IS .T.
   HBTEST scString = NIL                  IS .F.
   HBTEST "A" = 1                         IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:C:A;N:1 F:S"
   HBTEST scString = 1                    IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST soObject = ""                   IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST soObject = soObject             IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject = ErrorNew()           IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST ErrorNew() = ErrorNew()         IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject = TBColumnNew()        IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S"
   HBTEST saArray = saArray               IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S"
   HBTEST {} = {}                         IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S"
   HBTEST {|| NIL } = {|| NIL }           IS "E 1 BASE 1071 Argument error (=) OS:0 #:0 A:2:B:{||...};B:{||...} F:S"

   /* != special */

   HBTEST NIL != NIL                      IS .F.
   HBTEST scString != NIL                 IS .T.
   HBTEST "A" != 1                        IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:C:A;N:1 F:S"
   HBTEST scString != 1                   IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST soObject != ""                  IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST soObject != soObject            IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject != ErrorNew()          IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST ErrorNew() != ErrorNew()        IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject != TBColumnNew()       IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S"
   HBTEST saArray != saArray              IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S"
   HBTEST {} != {}                        IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S"
   HBTEST {|| NIL } != {|| NIL }          IS "E 1 BASE 1072 Argument error (<>) OS:0 #:0 A:2:B:{||...};B:{||...} F:S"

   /* < special */

   HBTEST NIL < NIL                       IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST scString < NIL                  IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:HELLO;U:NIL F:S"
   HBTEST "A" < 1                         IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:A;N:1 F:S"
   HBTEST scString < 1                    IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST soObject < ""                   IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST soObject < soObject             IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject < ErrorNew()           IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST ErrorNew() < ErrorNew()         IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject < TBColumnNew()        IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S"
   HBTEST saArray < saArray               IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S"
   HBTEST {} < {}                         IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S"
   HBTEST {|| NIL } < {|| NIL }           IS "E 1 BASE 1073 Argument error (<) OS:0 #:0 A:2:B:{||...};B:{||...} F:S"

   /* <= special */

   HBTEST NIL <= NIL                      IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST scString <= NIL                 IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:HELLO;U:NIL F:S"
   HBTEST "A" <= 1                        IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:A;N:1 F:S"
   HBTEST scString <= 1                   IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST soObject <= ""                  IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST soObject <= soObject            IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject <= ErrorNew()          IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST ErrorNew() <= ErrorNew()        IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject <= TBColumnNew()       IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S"
   HBTEST saArray <= saArray              IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S"
   HBTEST {} <= {}                        IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S"
   HBTEST {|| NIL } <= {|| NIL }          IS "E 1 BASE 1074 Argument error (<=) OS:0 #:0 A:2:B:{||...};B:{||...} F:S"

   /* > special */

   HBTEST NIL > NIL                       IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST scString > NIL                  IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:C:HELLO;U:NIL F:S"
   HBTEST "A" > 1                         IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:C:A;N:1 F:S"
   HBTEST scString > 1                    IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST soObject > ""                   IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST soObject > soObject             IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject > ErrorNew()           IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST ErrorNew() > ErrorNew()         IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject > TBColumnNew()        IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S"
   HBTEST saArray > saArray               IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S"
   HBTEST {} > {}                         IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S"
   HBTEST {|| NIL } > {|| NIL }           IS "E 1 BASE 1075 Argument error (>) OS:0 #:0 A:2:B:{||...};B:{||...} F:S"

   /* >= special */

   HBTEST NIL >= NIL                      IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST scString >= NIL                 IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:C:HELLO;U:NIL F:S"
   HBTEST "A" >= 1                        IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:C:A;N:1 F:S"
   HBTEST scString >= 1                   IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:C:HELLO;N:1 F:S"
   HBTEST soObject >= ""                  IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;C: F:S"
   HBTEST soObject >= soObject            IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject >= ErrorNew()          IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST ErrorNew() >= ErrorNew()        IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:ERROR Object F:S"
   HBTEST soObject >= TBColumnNew()       IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:O:ERROR Object;O:TBCOLUMN Object F:S"
   HBTEST saArray >= saArray              IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:A:{.[1].};A:{.[1].} F:S"
   HBTEST {} >= {}                        IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:A:{.[0].};A:{.[0].} F:S"
   HBTEST {|| NIL } >= {|| NIL }          IS "E 1 BASE 1076 Argument error (>=) OS:0 #:0 A:2:B:{||...};B:{||...} F:S"

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
