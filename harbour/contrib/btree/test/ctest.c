/*
 * $Id$
 */

#include "hbdefs.h"
#include "hbapi.h"
#include "extend.api"
#include "item.api"

#include "hb_btree.api"

static void insertdata( struct hb_BTree * pBTree );
static void display( const BYTE *cKey, LONG lData );

HB_FUNC( CTEST )
{
  struct hb_BTree * pBTree;

  display( "Harbour API test", 0 );
  hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
  pBTree = hb_BTreeNew( "test_3.out", 2048, 90, HB_BTREE_UNIQUE, 0 );
  if ( pBTree != NULL )
  {
/*
    a := hb_BTreeInfo( pBTree );
    display( "File", a[ hb_BTreeINFO_FILENAME ] );
    display( "Page", a[ hb_BTreeINFO_PAGESIZE ] );
    display( "Key ", a[ hb_BTreeINFO_KEYSIZE  ] );
    display( "Max ", a[ hb_BTreeINFO_MAXKEYS  ] );
    display( "Min ", a[ hb_BTreeINFO_MINKEYS  ] );
    display( "Flag", a[ hb_BTreeINFO_FLAGS    ] );
    display( "Keys", a[ hb_BTreeINFO_KEYCOUNT ] );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
*/
    insertdata( pBTree );
/*
    display( "Keys", hb_BTreeInfo( pBTree, hb_BTreeINFO_KEYCOUNT ) );
*/
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "Forward traversal", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    hb_BTreeGoTop( pBTree );
    while ( TRUE )
    {
      display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) );
      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      if ( 2 != hb_BTreeSkip( pBTree, 2 ) )
      {
        break;
      }
    }

    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "Reverse traversal", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    hb_BTreeGoBottom( pBTree );
    while ( TRUE )
    {
      display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) );
      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      if ( -1 != hb_BTreeSkip( pBTree, -1 ) )
      {
        break;
      }
    }

    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "Test SEEK", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "", hb_BTreeSeek( pBTree, "cdntyzrf", 0, FALSE ) );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    hb_BTreeSkip( pBTree, 1 );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) ); display( " dmfmivqb  ?", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );

    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "Test soft SEEK of a short key", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "", hb_BTreeSeek( pBTree, "cd", 0, TRUE ) );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    hb_BTreeSkip( pBTree, 1 );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) ); display( " dmfmivqb  ?", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );

    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "Test soft SEEK of an existing key", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "", hb_BTreeSeek( pBTree, "cdntyzrf", 0, TRUE ) );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    hb_BTreeSkip( pBTree, 1 );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) ); display( " dmfmivqb  ?", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );

    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "Test soft SEEK of a missing key, that should force EOF", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    display( "", hb_BTreeSeek( pBTree, "zzzzzz", 0, FALSE ) );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    hb_BTreeSkip( pBTree, 1 );
    display( hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ) ); display( " dmfmivqb  ?", 0 );
    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );

    hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
    hb_BTreeClose( pBTree );
  }
  else
  {
    display( "error / failure", 0 );
  }
}

static void display( const BYTE *cKey, LONG lData )
{
  int n;
  BYTE buffer[ 80 ];

  if ( *cKey )
  {
    n = sprintf( buffer, "%s  %ld", cKey, lData );
  }
  else
  {
    n = sprintf( buffer, "%ld", lData );
  }
  hb_conOutStd( buffer, n );
}


static void insertdata( struct hb_BTree * pBTree )
{
  hb_BTreeInsert( pBTree, "fuweqgsz",  1 );
  hb_BTreeInsert( pBTree, "sjruexrd",  2 );
  hb_BTreeInsert( pBTree, "fvveitnz",  3 );
  hb_BTreeInsert( pBTree, "aqgksjxe",  4 );
  hb_BTreeInsert( pBTree, "oonrehvj",  5 );
  hb_BTreeInsert( pBTree, "gvowjwtr",  6 );
  hb_BTreeInsert( pBTree, "xxidwtvn",  7 );
  hb_BTreeInsert( pBTree, "rwjbxesd",  8 );
  hb_BTreeInsert( pBTree, "yaznsaek",  9 );
  hb_BTreeInsert( pBTree, "wbdhfkfy", 10 );
  hb_BTreeInsert( pBTree, "lryaezia", 11 );
  hb_BTreeInsert( pBTree, "tspmnrvk", 12 );
  hb_BTreeInsert( pBTree, "hpxryhdj", 13 );
  hb_BTreeInsert( pBTree, "sztcqaby", 14 );
  hb_BTreeInsert( pBTree, "fcyzsqja", 15 );
  hb_BTreeInsert( pBTree, "uccxumvg", 16 );
  hb_BTreeInsert( pBTree, "amwuoout", 17 );
  hb_BTreeInsert( pBTree, "yaytseln", 18 );
  hb_BTreeInsert( pBTree, "sfiiozej", 19 );
  hb_BTreeInsert( pBTree, "xuvsoljy", 20 );
  hb_BTreeInsert( pBTree, "qmqjbedm", 21 );
  hb_BTreeInsert( pBTree, "cctzzrkz", 22 );
  hb_BTreeInsert( pBTree, "ikytgdon", 23 );
  hb_BTreeInsert( pBTree, "pksobcwu", 24 );
  hb_BTreeInsert( pBTree, "vmurindj", 25 );
  hb_BTreeInsert( pBTree, "elvybqwg", 26 );
  hb_BTreeInsert( pBTree, "ixchaztx", 27 );
  hb_BTreeInsert( pBTree, "nzpztlhd", 28 );
  hb_BTreeInsert( pBTree, "aucrchiw", 29 );
  hb_BTreeInsert( pBTree, "munrytse", 30 );
  hb_BTreeInsert( pBTree, "kqkhcmls", 31 );
  hb_BTreeInsert( pBTree, "abqhurbi", 32 );
  hb_BTreeInsert( pBTree, "ymrldckr", 33 );
  hb_BTreeInsert( pBTree, "rhsmfflc", 34 );
  hb_BTreeInsert( pBTree, "apyfkvee", 35 );
  hb_BTreeInsert( pBTree, "cdntyzrf", 36 );
  hb_BTreeInsert( pBTree, "iacblqrh", 37 );
  hb_BTreeInsert( pBTree, "xvewqana", 38 );
  hb_BTreeInsert( pBTree, "xmybqytj", 39 );
  hb_BTreeInsert( pBTree, "dnowympf", 40 );
  hb_BTreeInsert( pBTree, "smloihft", 41 );
  hb_BTreeInsert( pBTree, "zumppmis", 42 );
  hb_BTreeInsert( pBTree, "jirucnxu", 43 );
  hb_BTreeInsert( pBTree, "ecdzikcv", 44 );
  hb_BTreeInsert( pBTree, "slbwvnpg", 45 );
  hb_BTreeInsert( pBTree, "yaftlkmz", 46 );
  hb_BTreeInsert( pBTree, "blcepksd", 47 );
  hb_BTreeInsert( pBTree, "xufowlpl", 48 );
  hb_BTreeInsert( pBTree, "xegtjtqc", 49 );
  hb_BTreeInsert( pBTree, "yplcqumq", 50 );
  hb_BTreeInsert( pBTree, "vdoycauz", 51 );
  hb_BTreeInsert( pBTree, "uhqkjuph", 52 );
  hb_BTreeInsert( pBTree, "prllaeyi", 53 );
  hb_BTreeInsert( pBTree, "ybzgmwzm", 54 );
  hb_BTreeInsert( pBTree, "kkvyllnp", 55 );
  hb_BTreeInsert( pBTree, "nberwsrb", 56 );
  hb_BTreeInsert( pBTree, "wgetahua", 57 );
  hb_BTreeInsert( pBTree, "yxcyehcv", 58 );
  hb_BTreeInsert( pBTree, "oacormks", 59 );
  hb_BTreeInsert( pBTree, "mcadkdxo", 60 );
  hb_BTreeInsert( pBTree, "ycsalwqw", 61 );
  hb_BTreeInsert( pBTree, "qmpysvjl", 62 );
  hb_BTreeInsert( pBTree, "iqikamew", 63 );
  hb_BTreeInsert( pBTree, "iaparrva", 64 );
  hb_BTreeInsert( pBTree, "casbvtay", 65 );
  hb_BTreeInsert( pBTree, "blaksexr", 66 );
  hb_BTreeInsert( pBTree, "tbosrbql", 67 );
  hb_BTreeInsert( pBTree, "ifkywsyt", 68 );
  hb_BTreeInsert( pBTree, "gvklwevy", 69 );
  hb_BTreeInsert( pBTree, "krpmpbud", 70 );
  hb_BTreeInsert( pBTree, "rdvlwbwm", 71 );
  hb_BTreeInsert( pBTree, "apnvdkww", 72 );
  hb_BTreeInsert( pBTree, "euqdocvm", 73 );
  hb_BTreeInsert( pBTree, "ksmkjcwp", 74 );
  hb_BTreeInsert( pBTree, "bztgclzc", 75 );
  hb_BTreeInsert( pBTree, "awkdnuxa", 76 );
  hb_BTreeInsert( pBTree, "abavnpod", 77 );
  hb_BTreeInsert( pBTree, "dvwvhjmh", 78 );
  hb_BTreeInsert( pBTree, "dmfmivqb", 79 );
  hb_BTreeInsert( pBTree, "ewsxanon", 80 );
}


/*
 * Harbour Project source code:
 * hb_BTree api test
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
