/*
 * Harbour Project source code:
 * Regression tests for the runtime library (array)
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

PROCEDURE Main_ARRAY()

   /* ARRAY function error conditions. */

#ifndef __XPP__
   HBTEST ACopy()                         IS NIL
#endif
   HBTEST ACopy( {}, "C" )                IS NIL
   HBTEST ACopy( "C", {} )                IS NIL
   HBTEST ACopy( {}, {} )                 IS "{.[0].}"
   HBTEST ACopy( {}, ErrorNew() )         IS "ERROR Object"
   HBTEST ACopy( ErrorNew(), {} )         IS "{.[0].}"
#ifndef __XPP__
   HBTEST AClone()                        IS NIL
#endif
   HBTEST AClone( NIL )                   IS NIL
   HBTEST AClone( {} )                    IS "{.[0].}"
   HBTEST AClone( ErrorNew() )            IS NIL
#ifndef __XPP__
   HBTEST AEval()                         IS "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 "
   HBTEST AEval( NIL )                    IS "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:1:U:NIL "
   HBTEST AEval( {} )                     IS "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:1:A:{.[0].} "
#endif
   HBTEST AEval( {}, NIL )                IS "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:2:A:{.[0].};U:NIL "
   HBTEST AEval( {}, {|| NIL } )          IS "{.[0].}"
   HBTEST AEval( ErrorNew(), {|| NIL } )  IS "ERROR Object"
#ifndef __XPP__
   HBTEST AScan()                         IS 0
   HBTEST AScan( NIL )                    IS 0
   HBTEST AScan( "A" )                    IS 0
   HBTEST AScan( {} )                     IS 0
#endif
   HBTEST AScan( {}, "" )                 IS 0
   HBTEST AScan( ErrorNew(), "NOT_FOUND" ) IS 0
#ifndef __XPP__
   HBTEST ASort()                         IS NIL
#endif
   HBTEST ASort( 10 )                     IS NIL
   HBTEST ASort( {} )                     IS "{.[0].}"
   HBTEST ASort( ErrorNew() )             IS NIL
#ifdef HB_CLP_STRICT
#ifndef __XPP__
   HBTEST AFill()                         IS "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:4:U:NIL;B:{||...};U:NIL;U:NIL "
#endif
   HBTEST AFill( NIL )                    IS "E 1 BASE 2017 Argument error (AEVAL) OS:0 #:0 A:4:U:NIL;B:{||...};U:NIL;U:NIL "
#else
#ifndef __XPP__
   HBTEST AFill()                         IS "E 1 BASE 6004 Argument error (AFILL) OS:0 #:0 "
#endif
   HBTEST AFill( NIL )                    IS "E 1 BASE 6004 Argument error (AFILL) OS:0 #:0 A:1:U:NIL "
#endif
   HBTEST AFill( {} )                     IS "{.[0].}"
   HBTEST AFill( {}, 1 )                  IS "{.[0].}"
   HBTEST AFill( ErrorNew() )             IS "ERROR Object"
   HBTEST AFill( ErrorNew(), 1 )          IS "ERROR Object"
#ifndef __XPP__
   HBTEST ADel()                          IS NIL
   HBTEST ADel( NIL )                     IS NIL
   HBTEST ADel( { 1 } )                   IS "{.[1].}"
#endif
   HBTEST ADel( { 1 }, 0 )                IS "{.[1].}"
   HBTEST ADel( { 1 }, 100 )              IS "{.[1].}"
   HBTEST ADel( { 1 }, 1 )                IS "{.[1].}"
   HBTEST ADel( { 1 }, -1 )               IS "{.[1].}"
   HBTEST ADel( { 1 }, 0 )                IS "{.[1].}"
   HBTEST ADel( { 1 }, NIL )              IS "{.[1].}"
#ifndef __XPP__
   HBTEST ADel( ErrorNew() )              IS "ERROR Object"
#endif
   HBTEST ADel( ErrorNew(), 0 )           IS "ERROR Object"
   HBTEST ADel( ErrorNew(), 100 )         IS "ERROR Object"
   HBTEST ADel( ErrorNew(), 1 )           IS "ERROR Object"
   HBTEST ADel( ErrorNew(), -1 )          IS "ERROR Object"
   HBTEST ADel( ErrorNew(), 0 )           IS "ERROR Object"
   HBTEST ADel( ErrorNew(), NIL )         IS "ERROR Object"
#ifndef __XPP__
   HBTEST AIns()                          IS NIL
   HBTEST AIns( NIL )                     IS NIL
   HBTEST AIns( { 1 } )                   IS "{.[1].}"
#endif
   HBTEST AIns( { 1 }, 0 )                IS "{.[1].}"
   HBTEST AIns( { 1 }, 100 )              IS "{.[1].}"
   HBTEST AIns( { 1 }, 1 )                IS "{.[1].}"
   HBTEST AIns( { 1 }, -1 )               IS "{.[1].}"
   HBTEST AIns( { 1 }, 0 )                IS "{.[1].}"
   HBTEST AIns( { 1 }, NIL )              IS "{.[1].}"
#ifndef __XPP__
   HBTEST AIns( ErrorNew() )              IS "ERROR Object"
#endif
   HBTEST AIns( ErrorNew(), 0 )           IS "ERROR Object"
   HBTEST AIns( ErrorNew(), 100 )         IS "ERROR Object"
   HBTEST AIns( ErrorNew(), 1 )           IS "ERROR Object"
   HBTEST AIns( ErrorNew(), -1 )          IS "ERROR Object"
   HBTEST AIns( ErrorNew(), 0 )           IS "ERROR Object"
   HBTEST AIns( ErrorNew(), NIL )         IS "ERROR Object"
#ifndef __XPP__
   HBTEST ATail()                         IS NIL
#endif
   HBTEST ATail( NIL )                    IS NIL
   HBTEST ATail( "" )                     IS NIL
   HBTEST ATail( {} )                     IS NIL
   HBTEST ATail( { 1, 2 } )               IS 2
   HBTEST ATail( ErrorNew() )             IS NIL
#ifndef __XPP__
#ifdef HB_COMPAT_C53
   HBTEST ASize()                         IS "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 "
   HBTEST ASize( NIL )                    IS "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 "
   HBTEST ASize( {} )                     IS "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 "
   HBTEST ASize( ErrorNew() )             IS "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 "
#else
   HBTEST ASize()                         IS NIL
   HBTEST ASize( NIL )                    IS NIL
   HBTEST ASize( {} )                     IS NIL
   HBTEST ASize( ErrorNew() )             IS NIL
#endif
#endif
#ifdef HB_COMPAT_C53
   HBTEST ASize( NIL, 0 )                 IS "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 "
   HBTEST ASize( NIL, 1 )                 IS "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 "
   HBTEST ASize( NIL, -1 )                IS "E 1 BASE 2023 Argument error (ASIZE) OS:0 #:0 "
#else
   HBTEST ASize( NIL, 0 )                 IS NIL
   HBTEST ASize( NIL, 1 )                 IS NIL
   HBTEST ASize( NIL, -1 )                IS NIL
#endif
   HBTEST ASize( {}, 0 )                  IS "{.[0].}"
   HBTEST ASize( ErrorNew(), 0 )          IS "ERROR Object"
   HBTEST ASize( {}, 1 )                  IS "{.[1].}"
   HBTEST ASize( { 1, 2 }, 1 )            IS "{.[1].}"
   HBTEST ASize( { 1, "AAAA" }, 1 )       IS "{.[1].}"
   HBTEST ASize( { "BBB", "AAAA" }, 0 )   IS "{.[0].}"
   HBTEST ASize( ErrorNew(), 1 )          IS "ERROR Object"
   HBTEST ASize( {}, -1 )                 IS "{.[0].}"
   HBTEST ASize( { 1 }, -1 )              IS "{.[0].}"
#ifdef __HARBOUR__
   HBTEST ASize( { 1 }, 5000 )            IS "{.[5000].}"
#else
   HBTEST ASize( { 1 }, 5000 )            IS "{.[1].}"
#endif
   HBTEST ASize( ErrorNew(), -1 )         IS "ERROR Object"
   HBTEST ASize( ErrorNew(), 100 )        IS "ERROR Object"
   HBTEST AAdd( NIL, NIL )                IS "E 1 BASE 1123 Argument error (AADD) OS:0 #:0 A:2:U:NIL;U:NIL F:S"
   HBTEST AAdd( {}, NIL )                 IS NIL
   HBTEST AAdd( {}, "A" )                 IS "A"
   HBTEST AAdd( ErrorNew(), NIL )         IS NIL
   HBTEST AAdd( ErrorNew(), "A" )         IS "A"
#ifndef __XPP__
   HBTEST Array()                         IS NIL
#endif
   HBTEST Array( 0 )                      IS "{.[0].}"
#ifdef __HARBOUR__
   HBTEST Array( 5000 )                   IS "{.[5000].}"
#else
   HBTEST Array( 5000 )                   IS "E 2 BASE 1131 Bound error (array dimension) OS:0 #:0 "
#endif
   HBTEST Array( 1 )                      IS "{.[1].}"
   HBTEST Array( -1 )                     IS "E 2 BASE 1131 Bound error (array dimension) OS:0 #:0 "
   HBTEST Array( 1, 0, -10 )              IS "E 2 BASE 1131 Bound error (array dimension) OS:0 #:0 "
   HBTEST Array( 1, 0, "A" )              IS NIL
   HBTEST Array( 1, 0, 2 )                IS "{.[1].}"
   HBTEST Array( 4, 3, 2 )                IS "{.[4].}"
   HBTEST Array( 0, 3, 2 )                IS "{.[0].}"

   /* AFill() */

   HBTEST TAStr( AFill( TANew(), "X" )          ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X", NIL, -2 ) ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X", NIL,  0 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X", NIL,  3 ) ) IS "XXX......."
   HBTEST TAStr( AFill( TANew(), "X", NIL, 20 ) ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   0 )     ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   0, -2 ) ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   0,  0 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",   0,  3 ) ) IS "XXX......."
   HBTEST TAStr( AFill( TANew(), "X",   0, 20 ) ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   1 )     ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   1, -2 ) ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   1,  0 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",   1,  3 ) ) IS "XXX......."
   HBTEST TAStr( AFill( TANew(), "X",   1, 20 ) ) IS "XXXXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   3 )     ) IS "..XXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",   3, -2 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",   3,  0 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",   3,  3 ) ) IS "..XXX....."
   HBTEST TAStr( AFill( TANew(), "X",   3, 20 ) ) IS "..XXXXXXXX"
   HBTEST TAStr( AFill( TANew(), "X",  -1 )     ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  -1, -2 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  -1,  0 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  -1,  3 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  -1, 20 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  21 )     ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  21, -2 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  21,  0 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  21,  3 ) ) IS ".........."
   HBTEST TAStr( AFill( TANew(), "X",  21, 20 ) ) IS ".........."

   /* ACopy() */

   HBTEST TAStr( ACopy( TARng(), TANew(),  1          ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,   0     ) ) IS ".........."
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,   3     ) ) IS "ABC......."
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,  20     ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ACopy( TARng(), TANew(),  3          ) ) IS "CDEFGHIJ.."
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,   0     ) ) IS ".........."
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,   3     ) ) IS "CDE......."
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,  20     ) ) IS "CDEFGHIJ.."
   HBTEST TAStr( ACopy( TARng(), TANew(), 21          ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,   0     ) ) IS ".........."
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,   3     ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,  20     ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(),  1, NIL,  1 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,   0,  1 ) ) IS ".........."
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,   3,  0 ) ) IS "ABC......."
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,   3,  2 ) ) IS ".ABC......"
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,   3,  8 ) ) IS ".......ABC"
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,   3, 20 ) ) IS ".........A"  /* Strange in CA-Cl*pper, it should return: ".........." */
   HBTEST TAStr( ACopy( TARng(), TANew(),  1,  20,  1 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ACopy( TARng(), TANew(),  3, NIL,  3 ) ) IS "..CDEFGHIJ"
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,   0,  3 ) ) IS ".........."
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,   3,  0 ) ) IS "CDE......."
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,   3,  2 ) ) IS ".CDE......"
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,   3,  8 ) ) IS ".......CDE"
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,   3, 20 ) ) IS ".........C"  /* Strange in CA-Cl*pper, it should return: ".........." */
   HBTEST TAStr( ACopy( TARng(), TANew(),  3,  20,  3 ) ) IS "..CDEFGHIJ"
   HBTEST TAStr( ACopy( TARng(), TANew(), 21, NIL, 21 ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,   0, 21 ) ) IS ".........."
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,   3,  0 ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: "J.........", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,   3,  2 ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: ".J........", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,   3,  8 ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: ".......J..", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,   3, 20 ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */
   HBTEST TAStr( ACopy( TARng(), TANew(), 21,  20, 21 ) ) IS ".........."  /* Bug in CA-Cl*pper, it will return: ".........J", fixed in 5.3a */

   /* ASort() */

   /* Different results in Harbour and CA-Cl*pper due to different
      sorting algorithms. Anyhow the results pattern should match.
      [vszakats] */
#ifdef __HARBOUR__
   HBTEST TAStr( ASort( TARRv(),,, {|| NIL } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| hb_SToD() } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "0" } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "1" } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "2" } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "a" } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "A" } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "" } ) )  IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "z" } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| .T. } ) ) IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| .F. } ) ) IS "FEIDGCHBJA"
   HBTEST TAStr( ASort( TARRv(),,, {|| 2 } ) )   IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| 1 } ) )   IS "DCBAEFIHGJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| 0 } ) )   IS "FEIDGCHBJA"
#else
   HBTEST TAStr( ASort( TARRv(),,, {|| NIL } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| hb_SToD() } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "0" } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "1" } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "2" } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "a" } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "A" } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "" } ) )  IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| "z" } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| .T. } ) ) IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| .F. } ) ) IS "DCEABJIHFG"
   HBTEST TAStr( ASort( TARRv(),,, {|| 2 } ) )   IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| 1 } ) )   IS "IHGFEDCBAJ"
   HBTEST TAStr( ASort( TARRv(),,, {|| 0 } ) )   IS "DCEABJIHFG"
#endif
   HBTEST TAStr( ASort( TARRv() ) )           IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(), NIL, NIL ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(), NIL,  -2 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(), NIL,   0 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(), NIL,   3 ) ) IS "HIJGFEDCBA"
   HBTEST TAStr( ASort( TARRv(), NIL,  20 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(),  -5      ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  -5,  -2 ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  -5,   0 ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  -5,   3 ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  -5,  20 ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),   0      ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(),   0,  -2 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(),   0,   0 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(),   0,   3 ) ) IS "HIJGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),   0,  20 ) ) IS "ABCDEFGHIJ"
   HBTEST TAStr( ASort( TARRv(),   5      ) ) IS "JIHGABCDEF"
#ifdef __HARBOUR__
   HBTEST TAStr( ASort( TARRv(),   5,  -2 ) ) IS "JIHGABCDEF"  /* CA-Cl*pper will crash or GPF on that line. */
#endif
   HBTEST TAStr( ASort( TARRv(),   5,   0 ) ) IS "JIHGABCDEF"
   HBTEST TAStr( ASort( TARRv(),   5,   3 ) ) IS "JIHGDEFCBA"
   HBTEST TAStr( ASort( TARRv(),   5,  20 ) ) IS "JIHGABCDEF"
   HBTEST TAStr( ASort( TARRv(),  20      ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  20,  -2 ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  20,   0 ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  20,   3 ) ) IS "JIHGFEDCBA"
   HBTEST TAStr( ASort( TARRv(),  20,  20 ) ) IS "JIHGFEDCBA"

   /* AScan() */

#ifndef __XPP__
   HBTEST AScan()                         IS 0
   HBTEST AScan( NIL )                    IS 0
   HBTEST AScan( "A" )                    IS 0
#endif
   HBTEST AScan( "A", "A" )               IS 0
   HBTEST AScan( "A", {|| .F. } )         IS 0
   HBTEST AScan( { 1, 2, 3 }, {|| NIL } ) IS 0
   HBTEST AScan( saAllTypes, scString )   IS 1
#ifdef __HARBOUR__
   HBTEST AScan( @saAllTypes, scString )  IS 1  /* Bug in CA-Cl*pper, it will return 0 */
   HBTEST AScan( saAllTypes, @scString )  IS 1  /* Bug in CA-Cl*pper, it will return 0 */
#endif
   HBTEST AScan( saAllTypes, scStringE  ) IS 1
   HBTEST AScan( saAllTypes, scStringZ  ) IS 3
   HBTEST AScan( saAllTypes, snIntZ     ) IS 4
   HBTEST AScan( saAllTypes, snDoubleZ  ) IS 4
   HBTEST AScan( saAllTypes, snIntP     ) IS 6
   HBTEST AScan( saAllTypes, snLongP    ) IS 7
   HBTEST AScan( saAllTypes, snDoubleP  ) IS 8
   HBTEST AScan( saAllTypes, snIntN     ) IS 9
   HBTEST AScan( saAllTypes, snLongN    ) IS 10
   HBTEST AScan( saAllTypes, snDoubleN  ) IS 11
   HBTEST AScan( saAllTypes, snDoubleI  ) IS 4
   HBTEST AScan( saAllTypes, sdDateE    ) IS 13
   HBTEST AScan( saAllTypes, slFalse    ) IS 14
   HBTEST AScan( saAllTypes, slTrue     ) IS 15
   HBTEST AScan( saAllTypes, soObject   ) IS 0
   HBTEST AScan( saAllTypes, suNIL      ) IS 17
   HBTEST AScan( saAllTypes, sbBlock    ) IS 0
   HBTEST AScan( saAllTypes, sbBlockC   ) IS 0
   HBTEST AScan( saAllTypes, saArray    ) IS 0
   SET EXACT ON
   HBTEST AScan( saAllTypes, scString   ) IS 1
   HBTEST AScan( saAllTypes, scStringE  ) IS 2
   HBTEST AScan( saAllTypes, scStringZ  ) IS 3
   SET EXACT OFF

   HBTEST TAEVSM()                        IS "N10N 9N 8N 7N 6N 5N 4N 3N 2N 1         0"  /* Bug in CA-Cl*pper 5.x */
   HBTEST TASOSM1()                       IS "NN 5NN 4NN 3NN 2NN 1NN 0NN 0NN 0NN 0NN 0NN 0NN 0         0{  }"
   HBTEST TASOSM2()                       IS "NN 5NN 4NN 3NN 2NN 1NN 0NN 0NN 0NN 0NN 0         0{  }"

   RETURN

STATIC FUNCTION TAEVSM()

   LOCAL cString := ""
   LOCAL aArray := Array( 10 )

   AFill( aArray, 0 )
   AEval( aArray, {| x | cString += ValType( x ) + Str( Len( aArray ), 2 ), ASize( aArray, Len( aArray ) - 1 ) } )

   RETURN cString + Str( Len( aArray ) )

STATIC FUNCTION TASOSM1()

   LOCAL cString := ""
   LOCAL aArray := { 1, 2, 3, 4, 5 }

   ASort( aArray, NIL, NIL, {| x, y | cString += ValType( x ) + ValType( y ) + Str( Len( aArray ), 2 ), ASize( aArray, Len( aArray ) - 1 ), x > y } )

   RETURN cString + Str( Len( aArray ) ) + XToStrX( aArray )

STATIC FUNCTION TASOSM2()

   LOCAL cString := ""
   LOCAL aArray := { 1, 2, 3, 4, 5 }

   ASort( aArray, NIL, NIL, {| x, y | cString += ValType( x ) + ValType( y ) + Str( Len( aArray ), 2 ), ASize( aArray, Len( aArray ) - 1 ) } )

   RETURN cString + Str( Len( aArray ) ) + XToStrX( aArray )

STATIC FUNCTION TANew( cChar, nLen )

   LOCAL aArray
   LOCAL tmp

   IF nLen == NIL
      nLen := 10
   ENDIF

   IF cChar == NIL
      cChar := "."
   ENDIF

   aArray := Array( nLen )

   /* Intentionally not using AFill() here, since this function is
      involved in testing AFill() itself. */
   FOR tmp := 1 TO nLen
      aArray[ tmp ] := cChar
   NEXT

   RETURN aArray

STATIC FUNCTION TARng( nLen )

   LOCAL aArray
   LOCAL tmp

   IF nLen == NIL
      nLen := 10
   ENDIF

   aArray := Array( nLen )

   FOR tmp := 1 TO nLen
      aArray[ tmp ] := Chr( Asc( "A" ) + tmp - 1 )
   NEXT

   RETURN aArray

STATIC FUNCTION TARRv( nLen )

   LOCAL aArray
   LOCAL tmp

   IF nLen == NIL
      nLen := 10
   ENDIF

   aArray := Array( nLen )

   FOR tmp := 1 TO nLen
      aArray[ tmp ] := Chr( Asc( "A" ) + nLen - tmp )
   NEXT

   RETURN aArray

STATIC FUNCTION TAStr( aArray )

   LOCAL cString := ""
   LOCAL tmp
   LOCAL nLen := Len( aArray )

   FOR tmp := 1 TO nLen
      cString += aArray[ tmp ]
   NEXT

   RETURN cString

/* Don't change the position of this #include. */
#include "rt_init.ch"
