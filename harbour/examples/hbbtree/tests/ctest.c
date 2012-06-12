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

static void display( const char *cKey, HB_LONG lData, HB_BOOL NewLine )
{
   int n;
   char buffer[ 80 ];

   if( *cKey )
      n = hb_snprintf( buffer, sizeof( buffer ), "%s  %ld", cKey, lData );
   else
      n = hb_snprintf( buffer, sizeof( buffer ), "%ld", lData );

   hb_conOutStd( buffer, n );

   if( NewLine )
      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
}


static void insertdata( struct hb_BTree * pBTree )
{
   PHB_ITEM data = hb_itemNew( NULL );

   hb_BTreeInsert( pBTree, "fuweqgsz", hb_itemPutNL( data,  1 ) );
   hb_BTreeInsert( pBTree, "sjruexrd", hb_itemPutNL( data,  2 ) );
   hb_BTreeInsert( pBTree, "fvveitnz", hb_itemPutNL( data,  3 ) );
   hb_BTreeInsert( pBTree, "aqgksjxe", hb_itemPutNL( data,  4 ) );
   hb_BTreeInsert( pBTree, "oonrehvj", hb_itemPutNL( data,  5 ) );
   hb_BTreeInsert( pBTree, "gvowjwtr", hb_itemPutNL( data,  6 ) );
   hb_BTreeInsert( pBTree, "xxidwtvn", hb_itemPutNL( data,  7 ) );
   hb_BTreeInsert( pBTree, "rwjbxesd", hb_itemPutNL( data,  8 ) );
   hb_BTreeInsert( pBTree, "yaznsaek", hb_itemPutNL( data,  9 ) );
   hb_BTreeInsert( pBTree, "wbdhfkfy", hb_itemPutNL( data, 10 ) );
   hb_BTreeInsert( pBTree, "lryaezia", hb_itemPutNL( data, 11 ) );
   hb_BTreeInsert( pBTree, "tspmnrvk", hb_itemPutNL( data, 12 ) );
   hb_BTreeInsert( pBTree, "hpxryhdj", hb_itemPutNL( data, 13 ) );
   hb_BTreeInsert( pBTree, "sztcqaby", hb_itemPutNL( data, 14 ) );
   hb_BTreeInsert( pBTree, "fcyzsqja", hb_itemPutNL( data, 15 ) );
   hb_BTreeInsert( pBTree, "uccxumvg", hb_itemPutNL( data, 16 ) );
   hb_BTreeInsert( pBTree, "amwuoout", hb_itemPutNL( data, 17 ) );
   hb_BTreeInsert( pBTree, "yaytseln", hb_itemPutNL( data, 18 ) );
   hb_BTreeInsert( pBTree, "sfiiozej", hb_itemPutNL( data, 19 ) );
   hb_BTreeInsert( pBTree, "xuvsoljy", hb_itemPutNL( data, 20 ) );
   hb_BTreeInsert( pBTree, "qmqjbedm", hb_itemPutNL( data, 21 ) );
   hb_BTreeInsert( pBTree, "cctzzrkz", hb_itemPutNL( data, 22 ) );
   hb_BTreeInsert( pBTree, "ikytgdon", hb_itemPutNL( data, 23 ) );
   hb_BTreeInsert( pBTree, "pksobcwu", hb_itemPutNL( data, 24 ) );
   hb_BTreeInsert( pBTree, "vmurindj", hb_itemPutNL( data, 25 ) );
   hb_BTreeInsert( pBTree, "elvybqwg", hb_itemPutNL( data, 26 ) );
   hb_BTreeInsert( pBTree, "ixchaztx", hb_itemPutNL( data, 27 ) );
   hb_BTreeInsert( pBTree, "nzpztlhd", hb_itemPutNL( data, 28 ) );
   hb_BTreeInsert( pBTree, "aucrchiw", hb_itemPutNL( data, 29 ) );
   hb_BTreeInsert( pBTree, "munrytse", hb_itemPutNL( data, 30 ) );
   hb_BTreeInsert( pBTree, "kqkhcmls", hb_itemPutNL( data, 31 ) );
   hb_BTreeInsert( pBTree, "abqhurbi", hb_itemPutNL( data, 32 ) );
   hb_BTreeInsert( pBTree, "ymrldckr", hb_itemPutNL( data, 33 ) );
   hb_BTreeInsert( pBTree, "rhsmfflc", hb_itemPutNL( data, 34 ) );
   hb_BTreeInsert( pBTree, "apyfkvee", hb_itemPutNL( data, 35 ) );
   hb_BTreeInsert( pBTree, "cdntyzrf", hb_itemPutNL( data, 36 ) );
   hb_BTreeInsert( pBTree, "iacblqrh", hb_itemPutNL( data, 37 ) );
   hb_BTreeInsert( pBTree, "xvewqana", hb_itemPutNL( data, 38 ) );
   hb_BTreeInsert( pBTree, "xmybqytj", hb_itemPutNL( data, 39 ) );
   hb_BTreeInsert( pBTree, "dnowympf", hb_itemPutNL( data, 40 ) );
   hb_BTreeInsert( pBTree, "smloihft", hb_itemPutNL( data, 41 ) );
   hb_BTreeInsert( pBTree, "zumppmis", hb_itemPutNL( data, 42 ) );
   hb_BTreeInsert( pBTree, "jirucnxu", hb_itemPutNL( data, 43 ) );
   hb_BTreeInsert( pBTree, "ecdzikcv", hb_itemPutNL( data, 44 ) );
   hb_BTreeInsert( pBTree, "slbwvnpg", hb_itemPutNL( data, 45 ) );
   hb_BTreeInsert( pBTree, "yaftlkmz", hb_itemPutNL( data, 46 ) );
   hb_BTreeInsert( pBTree, "blcepksd", hb_itemPutNL( data, 47 ) );
   hb_BTreeInsert( pBTree, "xufowlpl", hb_itemPutNL( data, 48 ) );
   hb_BTreeInsert( pBTree, "xegtjtqc", hb_itemPutNL( data, 49 ) );
   hb_BTreeInsert( pBTree, "yplcqumq", hb_itemPutNL( data, 50 ) );
   hb_BTreeInsert( pBTree, "vdoycauz", hb_itemPutNL( data, 51 ) );
   hb_BTreeInsert( pBTree, "uhqkjuph", hb_itemPutNL( data, 52 ) );
   hb_BTreeInsert( pBTree, "prllaeyi", hb_itemPutNL( data, 53 ) );
   hb_BTreeInsert( pBTree, "ybzgmwzm", hb_itemPutNL( data, 54 ) );
   hb_BTreeInsert( pBTree, "kkvyllnp", hb_itemPutNL( data, 55 ) );
   hb_BTreeInsert( pBTree, "nberwsrb", hb_itemPutNL( data, 56 ) );
   hb_BTreeInsert( pBTree, "wgetahua", hb_itemPutNL( data, 57 ) );
   hb_BTreeInsert( pBTree, "yxcyehcv", hb_itemPutNL( data, 58 ) );
   hb_BTreeInsert( pBTree, "oacormks", hb_itemPutNL( data, 59 ) );
   hb_BTreeInsert( pBTree, "mcadkdxo", hb_itemPutNL( data, 60 ) );
   hb_BTreeInsert( pBTree, "ycsalwqw", hb_itemPutNL( data, 61 ) );
   hb_BTreeInsert( pBTree, "qmpysvjl", hb_itemPutNL( data, 62 ) );
   hb_BTreeInsert( pBTree, "iqikamew", hb_itemPutNL( data, 63 ) );
   hb_BTreeInsert( pBTree, "iaparrva", hb_itemPutNL( data, 64 ) );
   hb_BTreeInsert( pBTree, "casbvtay", hb_itemPutNL( data, 65 ) );
   hb_BTreeInsert( pBTree, "blaksexr", hb_itemPutNL( data, 66 ) );
   hb_BTreeInsert( pBTree, "tbosrbql", hb_itemPutNL( data, 67 ) );
   hb_BTreeInsert( pBTree, "ifkywsyt", hb_itemPutNL( data, 68 ) );
   hb_BTreeInsert( pBTree, "gvklwevy", hb_itemPutNL( data, 69 ) );
   hb_BTreeInsert( pBTree, "krpmpbud", hb_itemPutNL( data, 70 ) );
   hb_BTreeInsert( pBTree, "rdvlwbwm", hb_itemPutNL( data, 71 ) );
   hb_BTreeInsert( pBTree, "apnvdkww", hb_itemPutNL( data, 72 ) );
   hb_BTreeInsert( pBTree, "euqdocvm", hb_itemPutNL( data, 73 ) );
   hb_BTreeInsert( pBTree, "ksmkjcwp", hb_itemPutNL( data, 74 ) );
   hb_BTreeInsert( pBTree, "bztgclzc", hb_itemPutNL( data, 75 ) );
   hb_BTreeInsert( pBTree, "awkdnuxa", hb_itemPutNL( data, 76 ) );
   hb_BTreeInsert( pBTree, "abavnpod", hb_itemPutNL( data, 77 ) );
   hb_BTreeInsert( pBTree, "dvwvhjmh", hb_itemPutNL( data, 78 ) );
   hb_BTreeInsert( pBTree, "dmfmivqb", hb_itemPutNL( data, 79 ) );
   hb_BTreeInsert( pBTree, "ewsxanon", hb_itemPutNL( data, 80 ) );
   hb_itemRelease( data );
}

HB_FUNC( CTEST )
{
   struct hb_BTree * pBTree;

   display( "Harbour API test", 0, HB_TRUE );
   pBTree = hb_BTreeNew( "test_3.out", 2048, 90, HB_BTREE_UNIQUE, 0 );
   if ( pBTree != NULL )
   {
/*
      a := hb_BTreeInfo( pBTree );
      display( "File", a[ hb_BTreeINFO_FILENAME ], HB_FALSE );
      display( "Page", a[ hb_BTreeINFO_PAGESIZE ], HB_FALSE );
      display( "Key ", a[ hb_BTreeINFO_KEYSIZE  ], HB_FALSE );
      display( "Max ", a[ hb_BTreeINFO_MAXKEYS  ], HB_FALSE );
      display( "Min ", a[ hb_BTreeINFO_MINKEYS  ], HB_FALSE );
      display( "Flag", a[ hb_BTreeINFO_FLAGS    ], HB_FALSE );
      display( "Keys", a[ hb_BTreeINFO_KEYCOUNT ], HB_TRUE );
*/
      insertdata( pBTree );
/*
      display( "Keys", hb_BTreeInfo( pBTree, hb_BTreeINFO_KEYCOUNT ), HB_TRUE );
*/
      display( "Forward traversal", 0, HB_TRUE );
      hb_BTreeGoTop( pBTree );
      while ( HB_TRUE )
      {
         display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_TRUE );

         if ( 1 != hb_BTreeSkip( pBTree, 1 ) )
            break;
      }

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Reverse traversal", 0, HB_TRUE );
      hb_BTreeGoBottom( pBTree );
      while ( HB_TRUE )
      {
         display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_TRUE );

         if ( -1 != hb_BTreeSkip( pBTree, -1 ) )
            break;
      }

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test SEEK", 0, HB_TRUE );
      display( hb_BTreeSeek( pBTree, "cdntyzrf", 36, HB_FALSE ) == 1 ? ".T." : ".F.", 0, HB_TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_FALSE );
      display( " dmfmivqb  ?", 0, HB_TRUE );
      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test soft SEEK of a short key", 0, HB_TRUE );
      display( hb_BTreeSeek( pBTree, "cd", 0, HB_TRUE ) == 1 ? ".T." : ".F.", 0, HB_TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_FALSE );
      display( " dmfmivqb  ?", 0, HB_TRUE );

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test soft SEEK of an existing key", 0, HB_TRUE );
      display( hb_BTreeSeek( pBTree, "cdntyzrf", 0, HB_TRUE ) == 1 ? ".T." : ".F.", 0, HB_TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_FALSE );
      display( " dmfmivqb  ?", 0, HB_TRUE );

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      display( "Test soft SEEK of a missing key, that should force EOF", 0, HB_TRUE );
      display( hb_BTreeSeek( pBTree, "zzzzzz", 0, HB_FALSE ) == 1 ? ".T." : ".F.", 0, HB_TRUE );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_TRUE );
      hb_BTreeSkip( pBTree, 1 );
      display( ( char * ) hb_BTreeKey( pBTree ), hb_BTreeData( pBTree ), HB_FALSE );
      display( " dmfmivqb  ?", 0, HB_TRUE );

      hb_conOutStd( hb_conNewLine(), strlen( hb_conNewLine() ) );
      hb_BTreeClose( pBTree );
   }
   else
      display( "error / failure", 0, HB_TRUE );
}
