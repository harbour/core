*
 * $Id$
 */

#include "simpleio.ch"
#include "hb_btree.ch"
#include "fileio.ch"

Procedure Main()
  local n, a

  if fileattr( "test_1.out") = 1 + 32
    setfattr( "test_1.out", 32 )
    ferase( "test_1.out" )
  endif

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
    while .t.
      ? hb_btreekey( n ), hb_btreedata( n )
      if 2 <> hb_btreeskip( n, 2 )
        exit
      endif
    end

    ?
    ? "Reverse traversal"
    hb_btreegobottom( n )
    while .t.
      ? hb_btreekey( n ), hb_btreedata( n )
      if -1 <> hb_btreeskip( n, -1 )
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
    ? hb_btreeseek( n, "cd", ,.t. )
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

return

static procedure insertdata( n )
  hb_btreeinsert( n, "fuweqgsz",  1 )
  hb_btreeinsert( n, "sjruexrd",  2 )
  hb_btreeinsert( n, "fvveitnz",  3 )
  hb_btreeinsert( n, "aqgksjxe",  4 )
  hb_btreeinsert( n, "oonrehvj",  5 )
  hb_btreeinsert( n, "gvowjwtr",  6 )
  hb_btreeinsert( n, "xxidwtvn",  7 )
  hb_btreeinsert( n, "rwjbxesd",  8 )
  hb_btreeinsert( n, "yaznsaek",  9 )
  hb_btreeinsert( n, "wbdhfkfy", 10 )
  hb_btreeinsert( n, "lryaezia", 11 )
  hb_btreeinsert( n, "tspmnrvk", 12 )
  hb_btreeinsert( n, "hpxryhdj", 13 )
  hb_btreeinsert( n, "sztcqaby", 14 )
  hb_btreeinsert( n, "fcyzsqja", 15 )
  hb_btreeinsert( n, "uccxumvg", 16 )
  hb_btreeinsert( n, "amwuoout", 17 )
  hb_btreeinsert( n, "yaytseln", 18 )
  hb_btreeinsert( n, "sfiiozej", 19 )
  hb_btreeinsert( n, "xuvsoljy", 20 )
  hb_btreeinsert( n, "qmqjbedm", 21 )
  hb_btreeinsert( n, "cctzzrkz", 22 )
  hb_btreeinsert( n, "ikytgdon", 23 )
  hb_btreeinsert( n, "pksobcwu", 24 )
  hb_btreeinsert( n, "vmurindj", 25 )
  hb_btreeinsert( n, "elvybqwg", 26 )
  hb_btreeinsert( n, "ixchaztx", 27 )
  hb_btreeinsert( n, "nzpztlhd", 28 )
  hb_btreeinsert( n, "aucrchiw", 29 )
  hb_btreeinsert( n, "munrytse", 30 )
  hb_btreeinsert( n, "kqkhcmls", 31 )
  hb_btreeinsert( n, "abqhurbi", 32 )
  hb_btreeinsert( n, "ymrldckr", 33 )
  hb_btreeinsert( n, "rhsmfflc", 34 )
  hb_btreeinsert( n, "apyfkvee", 35 )
  hb_btreeinsert( n, "cdntyzrf", 36 )
  hb_btreeinsert( n, "iacblqrh", 37 )
  hb_btreeinsert( n, "xvewqana", 38 )
  hb_btreeinsert( n, "xmybqytj", 39 )
  hb_btreeinsert( n, "dnowympf", 40 )
  hb_btreeinsert( n, "smloihft", 41 )
  hb_btreeinsert( n, "zumppmis", 42 )
  hb_btreeinsert( n, "jirucnxu", 43 )
  hb_btreeinsert( n, "ecdzikcv", 44 )
  hb_btreeinsert( n, "slbwvnpg", 45 )
  hb_btreeinsert( n, "yaftlkmz", 46 )
  hb_btreeinsert( n, "blcepksd", 47 )
  hb_btreeinsert( n, "xufowlpl", 48 )
  hb_btreeinsert( n, "xegtjtqc", 49 )
  hb_btreeinsert( n, "yplcqumq", 50 )
  hb_btreeinsert( n, "vdoycauz", 51 )
  hb_btreeinsert( n, "uhqkjuph", 52 )
  hb_btreeinsert( n, "prllaeyi", 53 )
  hb_btreeinsert( n, "ybzgmwzm", 54 )
  hb_btreeinsert( n, "kkvyllnp", 55 )
  hb_btreeinsert( n, "nberwsrb", 56 )
  hb_btreeinsert( n, "wgetahua", 57 )
  hb_btreeinsert( n, "yxcyehcv", 58 )
  hb_btreeinsert( n, "oacormks", 59 )
  hb_btreeinsert( n, "mcadkdxo", 60 )
  hb_btreeinsert( n, "ycsalwqw", 61 )
  hb_btreeinsert( n, "qmpysvjl", 62 )
  hb_btreeinsert( n, "iqikamew", 63 )
  hb_btreeinsert( n, "iaparrva", 64 )
  hb_btreeinsert( n, "casbvtay", 65 )
  hb_btreeinsert( n, "blaksexr", 66 )
  hb_btreeinsert( n, "tbosrbql", 67 )
  hb_btreeinsert( n, "ifkywsyt", 68 )
  hb_btreeinsert( n, "gvklwevy", 69 )
  hb_btreeinsert( n, "krpmpbud", 70 )
  hb_btreeinsert( n, "rdvlwbwm", 71 )
  hb_btreeinsert( n, "apnvdkww", 72 )
  hb_btreeinsert( n, "euqdocvm", 73 )
  hb_btreeinsert( n, "ksmkjcwp", 74 )
  hb_btreeinsert( n, "bztgclzc", 75 )
  hb_btreeinsert( n, "awkdnuxa", 76 )
  hb_btreeinsert( n, "abavnpod", 77 )
  hb_btreeinsert( n, "dvwvhjmh", 78 )
  hb_btreeinsert( n, "dmfmivqb", 79 )
  hb_btreeinsert( n, "ewsxanon", 80 )
return

/*
 * Harbour Project source code:
 * hb_btree api test
 *
 * Copyright 2000 April White <awhite@mail.rosecom.ca>
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
