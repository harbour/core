/*
 * $Id$

nanforum ft_f*() clones
* hb_fuse
* hb_frecno
* hb_flastrec
* hb_fgotop
* hb_fgobottom
* hb_fgoto
* hb_feof
* hb_freadln
* hb_fskip
*/

#include <filesys.api>

#define b_size     1024
#define c_size     4096
#define IT_NUMBER  (IT_INTEGER|IT_LONG|IT_DOUBLE)

static long _hbfskip( int recs );

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

HARBOUR HB_FUSE( void )

{

   PHB_ITEM arg1_it = _param(1,IT_STRING);
   PHB_ITEM arg2_it = _param(2,IT_NUMBER);
   int open_flags;

   if ( arg1_it ) {

     if( arg2_it )
         open_flags = _parni(2);
     else
         open_flags = 0;

      handles[area]  = _fsOpen( _parc(1), open_flags );
      offset[area]   = 0;
      recno[area]    = 1;
      b              = ( char * )_xgrab( b_size );
      c              = ( char * )_xgrab( c_size );
      lastbyte[area] = _fsSeek( handles[area], 0L, SEEK_END );
      _retni( handles[area] );
   }
   else {
      _fsClose( handles[area] );
      _xfree( b )         ;
      _xfree( c )         ;
      _retni( 1 )         ;
      recno[area]    = 0L ;
      offset[area]   = 0L ;
      handles[area]  = 0  ;
      last_rec[area] = 0L ;
      last_off[area] = 0L ;
      lastbyte[area] = 0L ;
      isEof[area]    = 0  ;
   }
}


HARBOUR HB_FRECNO( void )

{
   _retnl( recno[area] );
}


HARBOUR HB_FSKIP( void )

{

   PHB_ITEM arg1_it = _param(1,IT_NUMBER);
   int nskip;

   if( arg1_it )
       nskip = _parni(1);
   else
       nskip = 1;

   _hbfskip(nskip);

}

long _hbfskip( int recs )

{

   int x;
   long read_pos;
   size_t read_len;
   long y;


   if ( recs > 0 ) {
      for (y = 0; y < recs; y++ ) {
         _fsSeek( handles[area], offset[area], SEEK_SET );
         read_len = _fsRead( handles[area], b, b_size );
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

         _fsSeek( handles[area], read_pos, SEEK_SET );
         read_len = _fsRead( handles[area], b, read_len );

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

HARBOUR HB_FREADLN( void )

{

   int x;
   long read;

   _fsSeek( handles[area], offset[area], SEEK_SET );
   read = _fsRead( handles[area], b, b_size );

   for ( x = 0; x < b_size; x++ ) {
      if ( ((*(b + x) == 13) && (*(b + x + 1) == 10)) ||
           ((*(b + x) == 10) && (*(b + x + 1) == 13)) ||
           (*(b + x) == 26) || ( x >= (int)read) ) {
         break;
      }
   }
   _retclen( b, x );

}

HARBOUR HB_FEOF( void )

{

   _retl( isEof[area] );

}

HARBOUR HB_FGOTO( void )

{

   long target;
   long last;

   target = _parnl(1);
   last = 0;

   if ( recno[area] > target ) {
      while ( recno[area] != target )   {
         last = recno[area];
         _hbfskip(-1);
         if ( recno[area] == last )
            break;
      }
   }
   else {
      while ( recno[area] != target ) {
         last = recno[area];
         _hbfskip(1);
         if ( recno[area] == last )
            break;
      }
   }
}

HARBOUR HB_FGOBOTTOM()

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

         _fsSeek( handles[area], offset[area], SEEK_SET );
         len = _fsRead(  handles[area], c, c_size );
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

HARBOUR HB_FGOTOP( void )
{

   offset[area] = 0L;
   recno[area] = 1L;

}

HARBOUR HB_FLASTREC( void )
{

   long old_rec;
   long old_offset;

   old_rec = recno[area];
   old_offset = offset[area];

   HB_FGOBOTTOM();
   _retnl( last_rec[area] );

   recno[area] = old_rec;
   offset[area] = old_offset;

}


HARBOUR HB_FSELECT( void )

{

   _retni( area + 1 );

   if ( ISNUM(1) )
      area = _parni(1) - 1;

}
