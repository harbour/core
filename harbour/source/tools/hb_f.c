/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File handling functions
 *
 * Copyright 1999 Andi Jahja <andij@aonlippo.co.id>
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

/* please run $(HARBOUR)\tests\working\testhbf.prg for testing */

#include "filesys.h"

#define b_size     1024
#define c_size     4096
#define IT_NUMBER  (IT_INTEGER|IT_LONG|IT_DOUBLE)

static long hb_hbfskip( int recs );

static long last_rec[10];
static long recno[10];
static long offset[10];
static int handles[10];
static int area = 0;
static char *b;
static char *c;
static long last_off[10];
static long lastbyte[10];
static int isEof[10];

HARBOUR HB_HB_FUSE( void )

{

   PHB_ITEM arg1_it = hb_param(1,IT_STRING);
   PHB_ITEM arg2_it = hb_param(2,IT_NUMBER);
   int open_flags;

   if ( arg1_it ) {

     if( arg2_it )
         open_flags = hb_parni(2);
     else
         open_flags = 0;

      handles[area]  = hb_fsOpen( ( BYTE * ) hb_parc(1), open_flags );
      offset[area]   = 0;
      recno[area]    = 1;
      b              = ( char * )hb_xgrab( b_size );
      c              = ( char * )hb_xgrab( c_size );
      lastbyte[area] = hb_fsSeek( handles[area], 0L, SEEK_END );
      hb_retni( handles[area] );
   }
   else {
      hb_fsClose( handles[area] );
      hb_xfree( b )         ;
      hb_xfree( c )         ;
      hb_retni( 1 )         ;
      recno[area]    = 0L ;
      offset[area]   = 0L ;
      handles[area]  = 0  ;
      last_rec[area] = 0L ;
      last_off[area] = 0L ;
      lastbyte[area] = 0L ;
      isEof[area]    = 0  ;
   }
}


HARBOUR HB_HB_FRECNO( void )

{
   hb_retnl( recno[area] );
}


HARBOUR HB_HB_FSKIP( void )

{

   PHB_ITEM arg1_it = hb_param(1,IT_NUMBER);
   int nskip;

   if( arg1_it )
       nskip = hb_parni(1);
   else
       nskip = 1;

   hb_hbfskip(nskip);

}

static long hb_hbfskip( int recs )

{

   long read_pos;
   size_t read_len;
   long x, y;


   HB_TRACE(("hb_hbskip(%d)", recs));

   if ( recs > 0 ) {
      for (y = 0; y < recs; y++ ) {
         hb_fsSeek( handles[area], offset[area], SEEK_SET );
         read_len = hb_fsRead( handles[area], ( BYTE * ) b, b_size );
         for (x = 0; x < read_len; x++ ) {
            if ( ((*(b + x) == 13) && (*(b + x + 1) == 10)) ||
                 ((*(b + x) == 10) && (*(b + x + 1) == 13)) ) {
               break;
            }
         }
         if ( (offset[area] + x + 2) < lastbyte[area] ) {
            isEof[area] = FALSE;
            offset[area] += (x + 2);
            recno[area] += 1;
         }
         else
            isEof[area] = TRUE;
      }
   }
   else {
      recs = -recs;
      isEof[area] = FALSE;

      if ( (recno[area] - recs) < 1 )
         return( 1 );

      for (y = recs; y > 0; y-- ) {
         if ( offset[area] - b_size < 0L ) {
            read_pos = 0;
            read_len = (size_t)offset[area];
         }
         else {
            read_pos = (size_t)(offset[area] - b_size);
            read_len = b_size;
         }

         hb_fsSeek( handles[area], read_pos, SEEK_SET );
         read_len = hb_fsRead( handles[area], ( BYTE * ) b, read_len );

         for (x = read_len - 4; x >= 0; x-- ) {
            if ( ((*(b + x) == 13) && (*(b + x + 1) == 10)) ||
                 ((*(b + x) == 10) && (*(b + x + 1) == 13)) ) {
               break;
            }
         }
         if ( x < 0 ) {
            offset[area] = 0;
            recno[area] = 1;
         }
         else {
            offset[area] = read_pos + x + 2;
            recno[area]--;
         }
      }
   }

   return ( recno[area] );
}

HARBOUR HB_HB_FREADLN( void )

{

   int x;
   long read;

   hb_fsSeek( handles[area], offset[area], SEEK_SET );
   read = hb_fsRead( handles[area], ( BYTE * ) b, b_size );

   for ( x = 0; x < b_size; x++ ) {
      if ( ((*(b + x) == 13) && (*(b + x + 1) == 10)) ||
           ((*(b + x) == 10) && (*(b + x + 1) == 13)) ||
           (*(b + x) == 26) || ( x >= (int)read) ) {
         break;
      }
   }
   hb_retclen( b, x );

}

HARBOUR HB_HB_FEOF( void )

{

   hb_retl( isEof[area] );

}

HARBOUR HB_HB_FGOTO( void )

{

   long target;
   long last;

   target = hb_parnl(1);
   last = 0;

   if ( recno[area] > target ) {
      while ( recno[area] != target )   {
         last = recno[area];
         hb_hbfskip(-1);
         if ( recno[area] == last )
            break;
      }
   }
   else {
      while ( recno[area] != target ) {
         last = recno[area];
         hb_hbfskip(1);
         if ( recno[area] == last )
            break;
      }
   }
}

HARBOUR HB_HB_FGOBOTTOM(void)

{

   int x;
   int len;
   long loc;

   if ( last_rec[area] != 0 ) {
      recno[area] = last_rec[area];
      offset[area] = last_off[area];
   }
   else {

      loc = 0L;

      do {

         hb_fsSeek( handles[area], offset[area], SEEK_SET );
         len = hb_fsRead(  handles[area], ( BYTE * ) c, c_size );
         for ( x = 0; x < len; x++ ) {
            if ( ((*(c + x) == 13) && (*(c + x + 1) == 10)) ||
                 ((*(c + x) == 10) && (*(c + x + 1) == 13)) ||
                 ( x - loc > b_size ) ) {
               recno[area]++;
               x++;
               loc = x + 1;
            }
         }
         offset[area] += loc;

      } while ( len == c_size );

      last_rec[area] = --recno[area];
      last_off[area] = offset[area];

   }
}

HARBOUR HB_HB_FGOTOP( void )
{

   offset[area] = 0L;
   recno[area] = 1L;

}

HARBOUR HB_HB_FLASTREC( void )
{

   long old_rec;
   long old_offset;

   old_rec = recno[area];
   old_offset = offset[area];

   HB_HB_FGOBOTTOM();
   hb_retnl( last_rec[area] );

   recno[area] = old_rec;
   offset[area] = old_offset;

}


HARBOUR HB_HB_FSELECT( void )

{

   hb_retni( area + 1 );

   if ( ISNUM(1) )
      area = hb_parni(1) - 1;

}
