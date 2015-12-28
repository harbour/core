/*
 * Regression tests for the runtime library
 *
 * Copyright 1999-2015 Viktor Szakats (vszakats.net/harbour)
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

#pragma -gc0  /* required to avoid bad -gc3 code to be generated
                 for the large numeric value '1234567654321' below */

FUNCTION hbtest_Object()

   LOCAL o := ErrorNew()

   o:description := "Harbour"

   RETURN o

/* TODO: add 'M' */

FUNCTION hbtest_AllValues()
   RETURN { ;
      NIL, ;
      "HELLO", ;
      "Hello", ;
      "", ;
      "   ", ;
      "A" + hb_BChar( 0 ) + "B", ;
      hb_BChar( 13 ) + hb_BChar( 10 ) + hb_BChar( 141 ) + hb_BChar( 10 ) + hb_BChar( 9 ), ;
      "utf8-űŰőŐ©", ;
      0, ;
      0.0, ;
      10, ;
      65, ;
      100000, ;
      10.567, ;  /* Use different number of decimals than the default */
      -10, ;
      -100000, ;
      -10.567, ;  /* Use different number of decimals than the default */
      1234567654321, ;
      hb_SToD( "19840325" ), ;
      hb_SToD(), ;
      hb_SToT( "19850325123456789" ), ;
      hb_SToT(), ;
      .F., ;
      .T., ;
      @hbtest_AllValues(), ;
      {|| NIL }, ;
      {|| "(string)" }, ;
      hbtest_Object(), ;
      { => }, ;
      { "a" => "b" }, ;
      {}, ;
      { 9898 }, ;
      __hbtest_Pointer( .T. ), ;
      __hbtest_Pointer( .F. ) }

FUNCTION hbtest_AllTypes()
   RETURN { ;
      NIL, ;
      "a", ;
      10, ;
      hb_SToD( "19840325" ), ;
      hb_SToT( "19850325123456789" ), ;
      .T., ;
      @hbtest_AllTypes(), ;
      {|| NIL }, ;
      hbtest_Object(), ;
      { "a" => "b" }, ;
      { 100 }, ;
      __hbtest_Pointer() }
