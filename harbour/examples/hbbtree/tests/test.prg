/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hb_btree api test
 *
 * Copyright 2000 April White <april@users.sourceforge.net>
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
 *
 */

#include "simpleio.ch"
#include "hb_btree.ch"
#include "fileio.ch"

Procedure Main()
  local c
  local attr

  // _2 is from ttest.prg; _3 is from ctest.c
  FOR EACH c IN { "test_1.out", "test_1a.out", "test_1b.out", "test_2.out", "test_3.out" }
    hb_FGetAttr( c, @attr )
    if attr == 1 + 32
      hb_FSetAttr( c, 32 )
    endif
    ferase( c )
  NEXT

  testInMemory()
  testInFile()

return

static procedure insertdata( n, s )
  if s == NIL ; s := 1; endif
  hb_btreeinsert( n, "fuweqgsz",  1 / s )
  hb_btreeinsert( n, "sjruexrd",  2 / s )
  hb_btreeinsert( n, "fvveitnz",  3 / s )
  hb_btreeinsert( n, "aqgksjxe",  4 / s )
  hb_btreeinsert( n, "oonrehvj",  5 / s )
  hb_btreeinsert( n, "gvowjwtr",  6 / s )
  hb_btreeinsert( n, "xxidwtvn",  7 / s )
  hb_btreeinsert( n, "rwjbxesd",  8 / s )
  hb_btreeinsert( n, "yaznsaek",  9 / s )
  hb_btreeinsert( n, "wbdhfkfy", 10 / s )
  hb_btreeinsert( n, "lryaezia", 11 / s )
  hb_btreeinsert( n, "tspmnrvk", 12 / s )
  hb_btreeinsert( n, "hpxryhdj", 13 / s )
  hb_btreeinsert( n, "sztcqaby", 14 / s )
  hb_btreeinsert( n, "fcyzsqja", 15 / s )
  hb_btreeinsert( n, "uccxumvg", 16 / s )
  hb_btreeinsert( n, "amwuoout", 17 / s )
  hb_btreeinsert( n, "yaytseln", 18 / s )
  hb_btreeinsert( n, "sfiiozej", 19 / s )
  hb_btreeinsert( n, "xuvsoljy", 20 / s )
  hb_btreeinsert( n, "qmqjbedm", 21 / s )
  hb_btreeinsert( n, "cctzzrkz", 22 / s )
  hb_btreeinsert( n, "ikytgdon", 23 / s )
  hb_btreeinsert( n, "pksobcwu", 24 / s )
  hb_btreeinsert( n, "vmurindj", 25 / s )
  hb_btreeinsert( n, "elvybqwg", 26 / s )
  hb_btreeinsert( n, "ixchaztx", 27 / s )
  hb_btreeinsert( n, "nzpztlhd", 28 / s )
  hb_btreeinsert( n, "aucrchiw", 29 / s )
  hb_btreeinsert( n, "munrytse", 30 / s )
  hb_btreeinsert( n, "kqkhcmls", 31 / s )
  hb_btreeinsert( n, "abqhurbi", 32 / s )
  hb_btreeinsert( n, "ymrldckr", 33 / s )
  hb_btreeinsert( n, "rhsmfflc", 34 / s )
  hb_btreeinsert( n, "apyfkvee", 35 / s )
  hb_btreeinsert( n, "cdntyzrf", 36 / s )
  hb_btreeinsert( n, "iacblqrh", 37 / s )
  hb_btreeinsert( n, "xvewqana", 38 / s )
  hb_btreeinsert( n, "xmybqytj", 39 / s )
  hb_btreeinsert( n, "dnowympf", 40 / s )
  hb_btreeinsert( n, "smloihft", 41 / s )
  hb_btreeinsert( n, "zumppmis", 42 / s )
  hb_btreeinsert( n, "jirucnxu", 43 / s )
  hb_btreeinsert( n, "ecdzikcv", 44 / s )
  hb_btreeinsert( n, "slbwvnpg", 45 / s )
  hb_btreeinsert( n, "yaftlkmz", 46 / s )
  hb_btreeinsert( n, "blcepksd", 47 / s )
  hb_btreeinsert( n, "xufowlpl", 48 / s )
  hb_btreeinsert( n, "xegtjtqc", 49 / s )
  hb_btreeinsert( n, "yplcqumq", 50 / s )
  hb_btreeinsert( n, "vdoycauz", 51 / s )
  hb_btreeinsert( n, "uhqkjuph", 52 / s )
  hb_btreeinsert( n, "prllaeyi", 53 / s )
  hb_btreeinsert( n, "ybzgmwzm", 54 / s )
  hb_btreeinsert( n, "kkvyllnp", 55 / s )
  hb_btreeinsert( n, "nberwsrb", 56 / s )
  hb_btreeinsert( n, "wgetahua", 57 / s )
  hb_btreeinsert( n, "yxcyehcv", 58 / s )
  hb_btreeinsert( n, "oacormks", 59 / s )
  hb_btreeinsert( n, "mcadkdxo", 60 / s )
  hb_btreeinsert( n, "ycsalwqw", 61 / s )
  hb_btreeinsert( n, "qmpysvjl", 62 / s )
  hb_btreeinsert( n, "iqikamew", 63 / s )
  hb_btreeinsert( n, "iaparrva", 64 / s )
  hb_btreeinsert( n, "casbvtay", 65 / s )
  hb_btreeinsert( n, "blaksexr", 66 / s )
  hb_btreeinsert( n, "tbosrbql", 67 / s )
  hb_btreeinsert( n, "ifkywsyt", 68 / s )
  hb_btreeinsert( n, "gvklwevy", 69 / s )
  hb_btreeinsert( n, "krpmpbud", 70 / s )
  hb_btreeinsert( n, "rdvlwbwm", 71 / s )
  hb_btreeinsert( n, "apnvdkww", 72 / s )
  hb_btreeinsert( n, "euqdocvm", 73 / s )
  hb_btreeinsert( n, "ksmkjcwp", 74 / s )
  hb_btreeinsert( n, "bztgclzc", 75 / s )
  hb_btreeinsert( n, "awkdnuxa", 76 / s )
  hb_btreeinsert( n, "abavnpod", 77 / s )
  hb_btreeinsert( n, "dvwvhjmh", 78 / s )
  hb_btreeinsert( n, "dmfmivqb", 79 / s )
  hb_btreeinsert( n, "ewsxanon", 80 / s )
return

STATIC PROCEDURE testInMemory()
  LOCAL n
  LOCAL c

  ? "Harbour API test: in-memory"
  n := hb_btreenew( , 2048, 90, HB_BTREE_READONLY + HB_BTREE_INMEMORY )
  if n > 0
    ? "successfully opened"
    insertdata( n, 100 )
    ? "# keys", hb_btreeinfo( n, HB_BTREEINFO_KEYCOUNT )

    ? "skip to EOF test"
    hb_btreegobottom( n )
    ? hb_btreekey( n ), hb_btreedata( n )
    ? hb_btreeskip( n, 1 )
    ? "skip to EOF test end"

    ? "Forward traversal"
    hb_btreegotop( n )
    c := 0
    while .t.
      ? hb_btreekey( n ), hb_btreedata( n ), ++c
      if 1 != hb_btreeskip( n, 1 )// .or. c == hb_btreeinfo( n, HB_BTREEINFO_KEYCOUNT )-1
        exit
      endif
    end
    ? "Forward traversal end"
    ?

    ? "Reverse traversal"
    hb_btreegobottom( n )
    c := 0
    while .t.
      ? hb_btreekey( n ), hb_btreedata( n ), ++c
      if -1 != hb_btreeskip( n, -1 )// .or. c == hb_btreeinfo( n, HB_BTREEINFO_KEYCOUNT )-1
        exit
      endif
    end
    ? "Reverse traversal end"
    ?

    ? "Test SEEK of 'cdntyzrf'"
    ? hb_btreeseek( n, "cdntyzrf" )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of short key 'cd'"
    ? hb_btreeseek( n, "cd", , .t. )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of an existing key 'cdntyzrf'"
    ? hb_btreeseek( n, "cdntyzrf", , .t. )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of a missing key, that should force EOF ('zzzzzz')"
    ? hb_btreeseek( n, "zzzzzz" )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?
    hb_btreeclose( n )
  else
    ? "error / failure"
    wait
  endif

  ? "Harbour API test: in-memory end"

STATIC PROCEDURE testInFile()
  LOCAL n
  LOCAL c
  LOCAL a

  ? "Harbour API test"
  n := hb_btreenew( "test_1.out", 2048, 90, HB_BTREE_READONLY )
  if n > 0

    ? valtype( a := hb_btreeinfo( n ) )
    ? "File", a[ HB_BTREEINFO_FILENAME ]
    ? "Page", a[ HB_BTREEINFO_PAGESIZE ]
    ? "Key ", a[ HB_BTREEINFO_KEYSIZE  ]
    ? "Max ", a[ HB_BTREEINFO_MAXKEYS  ]
    ? "Min ", a[ HB_BTREEINFO_MINKEYS  ]
    ? "Flag", a[ HB_BTREEINFO_FLAGS    ]
    ? "Keys", a[ HB_BTREEINFO_KEYCOUNT ]
    ?

    insertdata( n )

    ? "Keys", hb_btreeinfo( n, HB_BTREEINFO_KEYCOUNT )

    ?
    ? "Forward traversal"
    hb_btreegotop( n )
    c := 0
    while .t.
      ? hb_btreekey( n ), hb_btreedata( n ), ++c
      if 1 != hb_btreeskip( n, 1 )
        exit
      endif
    end

    ?
    ? "Reverse traversal"
    hb_btreegobottom( n )
    c := 0
    while .t.
      ? hb_btreekey( n ), hb_btreedata( n ), ++c
      if -1 != hb_btreeskip( n, -1 )
        exit
      endif
    end

    ?
    ? "Test SEEK"
    ? hb_btreeseek( n, "cdntyzrf" )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of a short key"
    ? hb_btreeseek( n, "cd", , .t. )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of an existing key"
    ? hb_btreeseek( n, "cdntyzrf", , .t. )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of a missing key, that should force EOF"
    ? hb_btreeseek( n, "zzzzzz" )
    ? hb_btreekey( n ), hb_btreedata( n )
    hb_btreeskip( n, 1 )
    ? hb_btreekey( n ), hb_btreedata( n ), "dmfmivqb  ?"

    ?

    hb_btreeclose( n )

    TTest()
    CTest()

  else
    ? "error / failure"
  endif

  ?
  ? "Harbour caseless sensitivity API test"
  n := hb_btreenew( "test_1a.out", 2048, 90, HB_BTREE_CASELESS )
  if n > 0
    hb_btreeinsert( n, "alpha", 0 )
    if hb_btreeinsert( n, "ALPHA", 0 )
      ? "inserted 'ALPHA', test failed"
    else
      ? "could not insert 'ALPHA', test passed"
    endif
    hb_btreeclose( n )
  endif

  ?
  ? "Harbour case sensitivity API test"
  n := hb_btreenew( "test_1b.out", 2048, 90 )
  if n > 0
    hb_btreeinsert( n, "alpha", 0 )
    if hb_btreeinsert( n, "ALPHA", 0 )
      ? "inserted 'ALPHA', test passed"
    else
      ? "could not insert 'ALPHA', test failed"
    endif
    hb_btreeclose( n )
  endif

  ?
