/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 video function: - INVERTWIN()
 *
 * Copyright 2002 Marek Horodyski <homar@altkom.com.pl>
 * www - http://www.harbour-project.org
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


/*  $DOC$
 *  $FUNCNAME$
 *      INVERTWIN()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *     
 *  $SYNTAX$
 *     
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      INVERTWIN() is compatible with CT3's INVERTWIN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is invertwin.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */

Function INVERTWIN( t, l, b, r)
 Static to_Konv := { ;
    0, 31,112, 32, 95, 66, 40, 97,  0, 73,  3,  0,  2,  0,  0,  2,;
   61,  0, 21, 44,  0,  0,110,111,  0, 75, 40,  0,  4, 48,122, 52,;
   82,  0,162,  0, 32, 32, 44,  9, 83,  9,  0, 82,  3, 23, 14,  0,;
   98,  0,  0,  7,  0, 32, 67, 32, 39,114,  0, 32, 32, 58, 32, 32,;
  111, 32,  2,  0, 34, 98,  0, 41, 55, 48, 13, 52, 31,  0,  4,  0,;
  255, 32, 49,  9, 21,  9,  8, 82, 10, 23, 32,  0, 48,  0, 50,  7,;
   49, 32, 74,  0, 78, 52,114, 41, 32,  0, 32, 32,  2, 32,  9, 32,;
   52, 45, 40, 32, 55,  0, 32, 68, 51, 32, 32,  0, 10, 44,  0, 53,;
   54,110,233, 53,  0,  0, 32, 52, 53,  0,  0, 49,  0,  9, 45,  0,;
   82, 32, 34,  0, 48,  0, 32, 48, 32, 32, 53, 51,  7, 45, 32, 32,;
   32, 32,  2, 51, 98, 32, 32, 13, 52, 19, 32, 45, 32, 53, 32, 49,;
   82, 45, 32, 56, 32,  0, 31, 50, 66, 32, 10, 32, 13, 32, 32, 32,;
   53,112, 66, 32,  0,  0,  0,  0,  0,110, 73,  0, 48, 52, 48,  0,;
   32,  9,  9, 82, 51,  0, 48,  7, 10, 32, 32, 10, 32, 18, 32,  0,;
   28, 13, 32, 32, 32, 53,112, 66, 32,114,  0,  0,  0,  0,  0,110,;
   73,  0, 48, 52, 48,  0, 32,  9,  9, 10, 32, 18,  0, 28, 13, 32}
  Local n AS NUMERIC, c AS CHARACTER, stop AS NUMERIC
  #ifdef HARBOUR
  Local os := ''
  #endif
  t := If( ValType( t) == 'N', t, Row())
  l := If( ValType( l) == 'N', l, Col())
  b := If( ValType( b) == 'N', b, MaxCol())
  r := If( ValType( r) == 'N', r, MaxCol())
  c := SaveScreen( t, l, b, r)
  stop := Len( c)
  For n := 2 TO stop STEP 2
  #ifdef HARBOUR
   os += c[ n - 1] + Chr( TO_Konv[ Asc( c[ n])])
  End
  RestScreen( t, l, b, r, os)
  #else  // in xHarbour
   c[ n] := Chr( TO_Konv[ Asc( c[ n])])
  End
  RestScreen( t, l, b, r, c)
  #endif
 Return ''
