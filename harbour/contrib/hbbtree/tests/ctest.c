/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hb_BTree api test
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

#include "hbapi.h"
#include "hbapiitm.h"

#include "hb_btree.h"

static void display( const char *cKey, LONG lData, BOOL NewLine )
{
   int n;
   char buffer[ 80 ];

   if( *cKey )
      n = sprintf( buffer, "%s  %ld", cKey, lData );
   else
      n = sprintf( buffer, "%ld", lData );

   hb_conOutStd( buffer, n );

   if( NewLine )
      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
}


static void insertdata( struct hb_BTree * pBTree )
{
   PHB_ITEM data = hb_itemNew( NULL );

   hb_BTreeInsert( pBTree, ( BYTE * ) "fuweqgsz", hb_itemPutNL( data,  1 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "sjruexrd", hb_itemPutNL( data,  2 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "fvveitnz", hb_itemPutNL( data,  3 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "aqgksjxe", hb_itemPutNL( data,  4 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "oonrehvj", hb_itemPutNL( data,  5 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "gvowjwtr", hb_itemPutNL( data,  6 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "xxidwtvn", hb_itemPutNL( data,  7 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "rwjbxesd", hb_itemPutNL( data,  8 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "yaznsaek", hb_itemPutNL( data,  9 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "wbdhfkfy", hb_itemPutNL( data, 10 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "lryaezia", hb_itemPutNL( data, 11 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "tspmnrvk", hb_itemPutNL( data, 12 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "hpxryhdj", hb_itemPutNL( data, 13 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "sztcqaby", hb_itemPutNL( data, 14 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "fcyzsqja", hb_itemPutNL( data, 15 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "uccxumvg", hb_itemPutNL( data, 16 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "amwuoout", hb_itemPutNL( data, 17 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "yaytseln", hb_itemPutNL( data, 18 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "sfiiozej", hb_itemPutNL( data, 19 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "xuvsoljy", hb_itemPutNL( data, 20 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "qmqjbedm", hb_itemPutNL( data, 21 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "cctzzrkz", hb_itemPutNL( data, 22 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ikytgdon", hb_itemPutNL( data, 23 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "pksobcwu", hb_itemPutNL( data, 24 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "vmurindj", hb_itemPutNL( data, 25 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "elvybqwg", hb_itemPutNL( data, 26 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ixchaztx", hb_itemPutNL( data, 27 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "nzpztlhd", hb_itemPutNL( data, 28 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "aucrchiw", hb_itemPutNL( data, 29 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "munrytse", hb_itemPutNL( data, 30 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "kqkhcmls", hb_itemPutNL( data, 31 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "abqhurbi", hb_itemPutNL( data, 32 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ymrldckr", hb_itemPutNL( data, 33 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "rhsmfflc", hb_itemPutNL( data, 34 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "apyfkvee", hb_itemPutNL( data, 35 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "cdntyzrf", hb_itemPutNL( data, 36 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "iacblqrh", hb_itemPutNL( data, 37 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "xvewqana", hb_itemPutNL( data, 38 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "xmybqytj", hb_itemPutNL( data, 39 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "dnowympf", hb_itemPutNL( data, 40 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "smloihft", hb_itemPutNL( data, 41 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "zumppmis", hb_itemPutNL( data, 42 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "jirucnxu", hb_itemPutNL( data, 43 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ecdzikcv", hb_itemPutNL( data, 44 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "slbwvnpg", hb_itemPutNL( data, 45 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "yaftlkmz", hb_itemPutNL( data, 46 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "blcepksd", hb_itemPutNL( data, 47 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "xufowlpl", hb_itemPutNL( data, 48 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "xegtjtqc", hb_itemPutNL( data, 49 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "yplcqumq", hb_itemPutNL( data, 50 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "vdoycauz", hb_itemPutNL( data, 51 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "uhqkjuph", hb_itemPutNL( data, 52 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "prllaeyi", hb_itemPutNL( data, 53 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ybzgmwzm", hb_itemPutNL( data, 54 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "kkvyllnp", hb_itemPutNL( data, 55 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "nberwsrb", hb_itemPutNL( data, 56 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "wgetahua", hb_itemPutNL( data, 57 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "yxcyehcv", hb_itemPutNL( data, 58 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "oacormks", hb_itemPutNL( data, 59 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "mcadkdxo", hb_itemPutNL( data, 60 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ycsalwqw", hb_itemPutNL( data, 61 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "qmpysvjl", hb_itemPutNL( data, 62 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "iqikamew", hb_itemPutNL( data, 63 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "iaparrva", hb_itemPutNL( data, 64 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "casbvtay", hb_itemPutNL( data, 65 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "blaksexr", hb_itemPutNL( data, 66 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "tbosrbql", hb_itemPutNL( data, 67 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ifkywsyt", hb_itemPutNL( data, 68 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "gvklwevy", hb_itemPutNL( data, 69 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "krpmpbud", hb_itemPutNL( data, 70 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "rdvlwbwm", hb_itemPutNL( data, 71 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "apnvdkww", hb_itemPutNL( data, 72 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "euqdocvm", hb_itemPutNL( data, 73 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ksmkjcwp", hb_itemPutNL( data, 74 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "bztgclzc", hb_itemPutNL( data, 75 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "awkdnuxa", hb_itemPutNL( data, 76 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "abavnpod", hb_itemPutNL( data, 77 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "dvwvhjmh", hb_itemPutNL( data, 78 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "dmfmivqb", hb_itemPutNL( data, 79 ) );
   hb_BTreeInsert( pBTree, ( BYTE * ) "ewsxanon", hb_itemPutNL( data, 80 ) );
   hb_itemRelease( data );
}

HB_FUNC( CTEST )
{
   struct hb_BTree * pBTree;

   display( "Harbour API test", 0, TRUE );
   pBTree = hb_BTreeNew( ( BYTE * ) "test_3.out", 2048, 90, HB_BTREE_UNIQUE, 0 );
   if ( pBTree != NULL )
   {
/*
      a := hb_BTreeInfo( pBTree );
      display( "File", a[ hb_BTreeINFO_FILENAME ], FALSE );
      display( "Page", a[ hb_BTreeINFO_PAGESIZE ], FALSE );
      display( "Key ", a[ hb_BTreeINFO_KEYSIZE  ], FALSE );
      display( "Max ", a[ hb_BTreeINFO_MAXKEYS  ], FALSE );
      display( "Min ", a[ hb_BTreeINFO_MINKEYS  ], FALSE );
      display( "Flag", a[ hb_BTreeINFO_FLAGS    ], FALSE );
      display( "Keys", a[ hb_BTreeINFO_KEYCOUNT ], TRUE );
*/
      insertdata( pBTree );
/*
      display( "Keys", hb_BTreeInfo( pBTree, hb_BTreeINFO_KEYCOUNT ), TRUE );
*/
      display( "Forward traversal", 0, TRUE );
      hb_BTreeGoTop( pBTree );
      while ( TRUE )
      {
         display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), TRUE );

         if ( 1 != hb_BTreeSkip( pBTree, 1 ) )
            break;
      }

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Reverse traversal", 0, TRUE );
      hb_BTreeGoBottom( pBTree );
      while ( TRUE )
      {
         display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), TRUE );

         if ( -1 != hb_BTreeSkip( pBTree, -1 ) )
            break;
      }

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test SEEK", 0, TRUE );
      display( hb_BTreeSeek( pBTree, ( BYTE * ) "cdntyzrf", 36, FALSE ) == 1 ? ".T." : ".F.", 0, TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), FALSE );
      display( " dmfmivqb  ?", 0, TRUE );
      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test soft SEEK of a short key", 0, TRUE );
      display( hb_BTreeSeek( pBTree, ( BYTE * ) "cd", 0, TRUE ) == 1 ? ".T." : ".F.", 0, TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), FALSE );
      display( " dmfmivqb  ?", 0, TRUE );

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test soft SEEK of an existing key", 0, TRUE );
      display( hb_BTreeSeek( pBTree, ( BYTE * ) "cdntyzrf", 0, TRUE ) == 1 ? ".T." : ".F.", 0, TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), FALSE );
      display( " dmfmivqb  ?", 0, TRUE );

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test soft SEEK of a missing key, that should force EOF", 0, TRUE );
      display( hb_BTreeSeek( pBTree, ( BYTE * ) "zzzzzz", 0, FALSE ) == 1 ? ".T." : ".F.", 0, TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), FALSE );
      display( " dmfmivqb  ?", 0, TRUE );

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      hb_BTreeClose( pBTree );
   }
   else
      display( "error / failure", 0, TRUE );
}
