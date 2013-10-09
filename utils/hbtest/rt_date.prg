/*
 * Harbour Project source code:
 * Regression tests for the runtime library (date)
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

#include "rt_main.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

PROCEDURE Main_DATE()

   LOCAL cDate := "1999-11-25"

   /* Year() */

   HBTEST Year( NIL )                           IS "E 1 BASE 1112 Argument error (YEAR) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Year( 100 )                           IS "E 1 BASE 1112 Argument error (YEAR) OS:0 #:0 A:1:N:100 F:S"
#ifdef __HARBOUR__
   HBTEST Year( @sdDate )                       IS 1984   /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1112 Argument error (YEAR) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST Year( sdDate)                         IS 1984
   HBTEST Year( sdDateE)                        IS 0
   HBTEST Str( Year( hb_SToD( "19990905" ) ) )  IS " 1999"

   /* Month() */

   HBTEST Month( NIL )                          IS "E 1 BASE 1113 Argument error (MONTH) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Month( 100 )                          IS "E 1 BASE 1113 Argument error (MONTH) OS:0 #:0 A:1:N:100 F:S"
#ifdef __HARBOUR__
   HBTEST Month( @sdDate )                      IS 3      /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1113 Argument error (MONTH) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST Month( sdDate )                       IS 3
   HBTEST Month( sdDateE )                      IS 0
   HBTEST Str( Month( hb_SToD( "19990905" ) ) ) IS "  9"

   /* Day() */

   HBTEST Day( NIL )                            IS "E 1 BASE 1114 Argument error (DAY) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST Day( 100 )                            IS "E 1 BASE 1114 Argument error (DAY) OS:0 #:0 A:1:N:100 F:S"
#ifdef __HARBOUR__
   HBTEST Day( @sdDate )                        IS 25     /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1114 Argument error (DAY) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST Day( sdDate )                         IS 25
   HBTEST Day( sdDateE )                        IS 0
   HBTEST Str( Day( hb_SToD( "19990905" ) ) )   IS "  5"

   /* Time() */

   HBTEST Len( Time() )                   IS 8

   /* DoW() */

   HBTEST DoW( NIL )                      IS "E 1 BASE 1115 Argument error (DOW) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST DoW( 100 )                      IS "E 1 BASE 1115 Argument error (DOW) OS:0 #:0 A:1:N:100 F:S"
#ifdef __HARBOUR__
   HBTEST DoW( @sdDate )                  IS 1  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1115 Argument error (DOW) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST DoW( sdDate )                   IS 1
   HBTEST DoW( sdDateE )                  IS 0
   HBTEST DoW( hb_SToD( "20000222" ) )    IS 3
   HBTEST DoW( hb_SToD( "20000223" ) )    IS 4
   HBTEST DoW( hb_SToD( "20000224" ) )    IS 5
   HBTEST DoW( hb_SToD( "20000225" ) )    IS 6
   HBTEST DoW( hb_SToD( "20000226" ) )    IS 7
   HBTEST DoW( hb_SToD( "20000227" ) )    IS 1
   HBTEST DoW( hb_SToD( "20000228" ) )    IS 2
   HBTEST DoW( hb_SToD( "20000229" ) )    IS 3
   HBTEST DoW( hb_SToD( "20000230" ) )    IS 0
   HBTEST DoW( hb_SToD( "20000231" ) )    IS 0
   HBTEST DoW( hb_SToD( "20000301" ) )    IS 4

   /* CMonth() */

   HBTEST CMonth( NIL )                   IS "E 1 BASE 1116 Argument error (CMONTH) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST CMonth( 100 )                   IS "E 1 BASE 1116 Argument error (CMONTH) OS:0 #:0 A:1:N:100 F:S"
#ifdef __HARBOUR__
   HBTEST CMonth( @sdDate )               IS "March"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1116 Argument error (CMONTH) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST CMonth( sdDate )                IS "March"
   HBTEST CMonth( sdDateE )               IS ""
   HBTEST CMonth( hb_SToD( "19990101" ) ) IS "January"
   HBTEST CMonth( hb_SToD( "19990201" ) ) IS "February"
   HBTEST CMonth( hb_SToD( "19990301" ) ) IS "March"
   HBTEST CMonth( hb_SToD( "19990401" ) ) IS "April"
   HBTEST CMonth( hb_SToD( "19990501" ) ) IS "May"
   HBTEST CMonth( hb_SToD( "19990601" ) ) IS "June"
   HBTEST CMonth( hb_SToD( "19990701" ) ) IS "July"
   HBTEST CMonth( hb_SToD( "19990801" ) ) IS "August"
   HBTEST CMonth( hb_SToD( "19990901" ) ) IS "September"
   HBTEST CMonth( hb_SToD( "19991001" ) ) IS "October"
   HBTEST CMonth( hb_SToD( "19991101" ) ) IS "November"
   HBTEST CMonth( hb_SToD( "19991201" ) ) IS "December"

   /* CDoW() */

   HBTEST CDoW( NIL )                     IS "E 1 BASE 1117 Argument error (CDOW) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST CDoW( 100 )                     IS "E 1 BASE 1117 Argument error (CDOW) OS:0 #:0 A:1:N:100 F:S"
#ifdef __HARBOUR__
   HBTEST CDoW( @sdDate )                 IS "Sunday"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1117 Argument error (CDOW) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST CDoW( sdDate )                  IS "Sunday"
   HBTEST CDoW( sdDateE )                 IS ""
   HBTEST CDoW( hb_SToD( "20000222" ) )   IS "Tuesday"
   HBTEST CDoW( hb_SToD( "20000223" ) )   IS "Wednesday"
   HBTEST CDoW( hb_SToD( "20000224" ) )   IS "Thursday"
   HBTEST CDoW( hb_SToD( "20000225" ) )   IS "Friday"
   HBTEST CDoW( hb_SToD( "20000226" ) )   IS "Saturday"
   HBTEST CDoW( hb_SToD( "20000227" ) )   IS "Sunday"
   HBTEST CDoW( hb_SToD( "20000228" ) )   IS "Monday"
   HBTEST CDoW( hb_SToD( "20000229" ) )   IS "Tuesday"
   HBTEST CDoW( hb_SToD( "20000230" ) )   IS ""
   HBTEST CDoW( hb_SToD( "20000231" ) )   IS ""
   HBTEST CDoW( hb_SToD( "20000301" ) )   IS "Wednesday"

   /* DToC() */

   HBTEST DToC( NIL )                     IS "E 1 BASE 1118 Argument error (DTOC) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST DToC( 100 )                     IS "E 1 BASE 1118 Argument error (DTOC) OS:0 #:0 A:1:N:100 F:S"
   HBTEST DToC( "" )                      IS "E 1 BASE 1118 Argument error (DTOC) OS:0 #:0 A:1:C: F:S"
#ifdef __HARBOUR__
   HBTEST DToC( @sdDate )                 IS "1984-03-25"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1118 Argument error (DTOC) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST DToC( sdDate )                  IS "1984-03-25"
   HBTEST DToC( sdDateE )                 IS "    -  -  "

   /* CToD() */

   HBTEST CToD( NIL )                     IS "E 1 BASE 1119 Argument error (CTOD) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST CToD( 100 )                     IS "E 1 BASE 1119 Argument error (CTOD) OS:0 #:0 A:1:N:100 F:S"
   HBTEST CToD( "" )                      IS hb_SToD("        ")
#ifdef __HARBOUR__
   HBTEST CToD( @cDate )                  IS hb_SToD("19991125")  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1119 Argument error (CTOD) OS:0 #:0 A:1:U:1999-11-25 F:S" */
#endif
   HBTEST CToD( cDate )                   IS hb_SToD("19991125")
   HBTEST CToD( "1999-11-25/10" )         IS hb_SToD("19991125")

   /* DToS() */

   HBTEST DToS( NIL )                     IS "E 1 BASE 1120 Argument error (DTOS) OS:0 #:0 A:1:U:NIL F:S"
   HBTEST DToS( 100 )                     IS "E 1 BASE 1120 Argument error (DTOS) OS:0 #:0 A:1:N:100 F:S"
#ifdef __HARBOUR__
   HBTEST DToS( @sdDate )                 IS "19840325"  /* Bug in CA-Cl*pper, it returns: "E 1 BASE 1120 Argument error (DTOS) OS:0 #:0 A:1:U:19840325 F:S" */
#endif
   HBTEST DToS( sdDate )                  IS "19840325"
   HBTEST DToS( sdDateE )                 IS "        "

   RETURN

/* Don't change the position of this #include. */
#include "rt_init.ch"
