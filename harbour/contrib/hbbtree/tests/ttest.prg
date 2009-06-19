*
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

Procedure TTest()
  local n, a

  ? "Harbour TBTree API test"
  n := TBTreeNew( "test_2.out", 2048, 90 )
  if n != NIL

    ? valtype( a := n:info() )
    ? "File", a[ HB_BTREEINFO_FILENAME ]
    ? "Page", a[ HB_BTREEINFO_PAGESIZE ]
    ? "Key ", a[ HB_BTREEINFO_KEYSIZE  ]
    ? "Max ", a[ HB_BTREEINFO_MAXKEYS  ]
    ? "Min ", a[ HB_BTREEINFO_MINKEYS  ]
    ? "Flag", a[ HB_BTREEINFO_FLAGS    ]
    ? "Keys", a[ HB_BTREEINFO_KEYCOUNT ]
    ?

    insertdata( n )

    ? "Keys", n:info( HB_BTREEINFO_KEYCOUNT )

    ?
    ? "Forward traversal"
    n:gotop()
    while .t.
      ? n:key(), n:data()
      if 1 != n:skip( 1 )
        exit
      endif
    end

    ?
    ? "Reverse traversal"
    n:gobottom()
    while .t.
      ? n:key(), n:data()
      if -1 != n:skip( -1 )
        exit
      endif
    end

    ?
    ? "Test SEEK"
    ? n:seek( "cdntyzrf" )
    ? n:key(), n:data()
    n:skip( 1 )
    ? n:key(), n:data(), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of a short key"
    ? n:seek( "cd", ,.t. )
    ? n:key(), n:data()
    n:skip( 1 )
    ? n:key(), n:data(), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of an existing key"
    ? n:seek( "cdntyzrf", , .t. )
    ? n:key(), n:data()
    n:skip( 1 )
    ? n:key(), n:data(), "dmfmivqb  ?"

    ?
    ? "Test soft SEEK of a missing key, that should force EOF"
    ? n:seek( "zzzzzz" )
    ? n:key(), n:data()
    n:skip( 1 )
    ? n:key(), n:data(), "dmfmivqb  ?"

    ?

    n:close()
  else
    ? "error"
  endif

return

static procedure insertdata( n )
  n:insert( "fuweqgsz",  1 )
  n:insert( "sjruexrd",  2 )
  n:insert( "fvveitnz",  3 )
  n:insert( "aqgksjxe",  4 )
  n:insert( "oonrehvj",  5 )
  n:insert( "gvowjwtr",  6 )
  n:insert( "xxidwtvn",  7 )
  n:insert( "rwjbxesd",  8 )
  n:insert( "yaznsaek",  9 )
  n:insert( "wbdhfkfy", 10 )
  n:insert( "lryaezia", 11 )
  n:insert( "tspmnrvk", 12 )
  n:insert( "hpxryhdj", 13 )
  n:insert( "sztcqaby", 14 )
  n:insert( "fcyzsqja", 15 )
  n:insert( "uccxumvg", 16 )
  n:insert( "amwuoout", 17 )
  n:insert( "yaytseln", 18 )
  n:insert( "sfiiozej", 19 )
  n:insert( "xuvsoljy", 20 )
  n:insert( "qmqjbedm", 21 )
  n:insert( "cctzzrkz", 22 )
  n:insert( "ikytgdon", 23 )
  n:insert( "pksobcwu", 24 )
  n:insert( "vmurindj", 25 )
  n:insert( "elvybqwg", 26 )
  n:insert( "ixchaztx", 27 )
  n:insert( "nzpztlhd", 28 )
  n:insert( "aucrchiw", 29 )
  n:insert( "munrytse", 30 )
  n:insert( "kqkhcmls", 31 )
  n:insert( "abqhurbi", 32 )
  n:insert( "ymrldckr", 33 )
  n:insert( "rhsmfflc", 34 )
  n:insert( "apyfkvee", 35 )
  n:insert( "cdntyzrf", 36 )
  n:insert( "iacblqrh", 37 )
  n:insert( "xvewqana", 38 )
  n:insert( "xmybqytj", 39 )
  n:insert( "dnowympf", 40 )
  n:insert( "smloihft", 41 )
  n:insert( "zumppmis", 42 )
  n:insert( "jirucnxu", 43 )
  n:insert( "ecdzikcv", 44 )
  n:insert( "slbwvnpg", 45 )
  n:insert( "yaftlkmz", 46 )
  n:insert( "blcepksd", 47 )
  n:insert( "xufowlpl", 48 )
  n:insert( "xegtjtqc", 49 )
  n:insert( "yplcqumq", 50 )
  n:insert( "vdoycauz", 51 )
  n:insert( "uhqkjuph", 52 )
  n:insert( "prllaeyi", 53 )
  n:insert( "ybzgmwzm", 54 )
  n:insert( "kkvyllnp", 55 )
  n:insert( "nberwsrb", 56 )
  n:insert( "wgetahua", 57 )
  n:insert( "yxcyehcv", 58 )
  n:insert( "oacormks", 59 )
  n:insert( "mcadkdxo", 60 )
  n:insert( "ycsalwqw", 61 )
  n:insert( "qmpysvjl", 62 )
  n:insert( "iqikamew", 63 )
  n:insert( "iaparrva", 64 )
  n:insert( "casbvtay", 65 )
  n:insert( "blaksexr", 66 )
  n:insert( "tbosrbql", 67 )
  n:insert( "ifkywsyt", 68 )
  n:insert( "gvklwevy", 69 )
  n:insert( "krpmpbud", 70 )
  n:insert( "rdvlwbwm", 71 )
  n:insert( "apnvdkww", 72 )
  n:insert( "euqdocvm", 73 )
  n:insert( "ksmkjcwp", 74 )
  n:insert( "bztgclzc", 75 )
  n:insert( "awkdnuxa", 76 )
  n:insert( "abavnpod", 77 )
  n:insert( "dvwvhjmh", 78 )
  n:insert( "dmfmivqb", 79 )
  n:insert( "ewsxanon", 80 )
return
