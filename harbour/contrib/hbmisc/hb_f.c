/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File handling functions
 *
 * Copyright 1999 Andi Jahja <andij@aonlippo.co.id>
 * www - http://harbour-project.org
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

/* please run tests\testhbf.prg for testing */

#include "hbapifs.h"

#define _B_SIZE   4096
#define _C_SIZE   4096

static long       last_rec[ 10 ];
static long       recno[ 10 ];
static HB_FOFFSET offset[ 10 ];
static HB_FHANDLE handles[ 10 ];
static int        area = 0;
static char *     b;
static char *     c;
static HB_FOFFSET last_off[ 10 ];
static HB_FOFFSET lastbyte[ 10 ];
static HB_BOOL    isEof[ 10 ];

HB_FUNC( HB_FUSE )
{
   PHB_ITEM arg1_it  = hb_param( 1, HB_IT_STRING );
   PHB_ITEM arg2_it  = hb_param( 2, HB_IT_NUMERIC );
   int      open_flags;

   if( arg1_it )
   {
      if( arg2_it )
         open_flags = hb_parni( 2 );
      else
         open_flags = 0;

      handles[ area ]   = hb_fsOpen( hb_parc( 1 ), ( HB_USHORT ) open_flags );
      offset[ area ]    = 0;
      recno[ area ]     = 1;
      b                 = ( char * ) hb_xgrab( _B_SIZE );
      c                 = ( char * ) hb_xgrab( _C_SIZE );
      lastbyte[ area ]  = hb_fsSeekLarge( handles[ area ], 0, FS_END );
      isEof[ area ]     = ( lastbyte[ area ] == 0 );
      hb_retnint( handles[ area ] );
   }
   else
   {
      hb_fsClose( handles[ area ] );
      hb_xfree( b );
      hb_xfree( c );
      hb_retnint( 1 );
      recno[ area ]     = 0;
      offset[ area ]    = 0;
      handles[ area ]   = 0;
      last_rec[ area ]  = 0;
      last_off[ area ]  = 0;
      lastbyte[ area ]  = 0;
      isEof[ area ]     = HB_FALSE;
   }
}


HB_FUNC( HB_FRECNO )
{
   hb_retnl( recno[ area ] );
}


static long hb_hbfskip( int recs )
{
   HB_FOFFSET  read_pos;
   HB_ISIZ     read_len;
   HB_ISIZ     x;
   int         y;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hbskip(%d)", recs ) );

   if( recs > 0 )
   {
      for( y = 0; y < recs; ++y )
      {
         hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );

         read_len = hb_fsReadLarge( handles[ area ], b, _B_SIZE );

         for( x = 0; x < read_len; ++x )
         {
            if( ( ( *( b + x ) == 13 ) && ( *( b + x + 1 ) == 10 ) ) ||
                ( ( *( b + x ) == 10 ) && ( *( b + x + 1 ) == 13 ) ) )
            {
               break;
            }
         }

         if( ( offset[ area ] + x + 2 ) < lastbyte[ area ] )
         {
            isEof[ area ]  = HB_FALSE;
            offset[ area ] += ( x + 2 );
            recno[ area ]  += 1;
         }
         else
            isEof[ area ] = HB_TRUE;
      }
   }
   else
   {
      recs           = -recs;
      isEof[ area ]  = HB_FALSE;

      if( ( recno[ area ] - recs ) < 1 )
         return 1;

      for( y = recs; y > 0; y-- )
      {
         if( offset[ area ] - _B_SIZE < 0 )
         {
            read_pos = 0;
            read_len = ( size_t ) offset[ area ];
         }
         else
         {
            read_pos = ( size_t ) ( offset[ area ] - _B_SIZE );
            read_len = _B_SIZE;
         }

         hb_fsSeekLarge( handles[ area ], read_pos, FS_SET );
         read_len = hb_fsReadLarge( handles[ area ], b, read_len );

         for( x = read_len - 4; x >= 0; x-- )
         {
            if( ( ( *( b + x ) == 13 ) && ( *( b + x + 1 ) == 10 ) ) ||
                ( ( *( b + x ) == 10 ) && ( *( b + x + 1 ) == 13 ) ) )
            {
               break;
            }
         }
         if( x < 0 )
         {
            offset[ area ] = 0;
            recno[ area ]  = 1;
         }
         else
         {
            offset[ area ] = read_pos + x + 2;
            recno[ area ]--;
         }
      }
   }

   return recno[ area ];
}

HB_FUNC( HB_FSKIP )
{
   hb_hbfskip( hb_parnidef( 1, 1 ) );
}

HB_FUNC( HB_FREADLN )
{
   HB_ISIZ  x;
   HB_ISIZ  read;

   hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );

   read = hb_fsReadLarge( handles[ area ], b, _B_SIZE );

   for( x = 0; x < _B_SIZE; ++x )
   {
      if( ( ( *( b + x ) == 13 ) && ( *( b + x + 1 ) == 10 ) ) ||
          ( ( *( b + x ) == 10 ) && ( *( b + x + 1 ) == 13 ) ) ||
          ( *( b + x ) == 26 ) || ( x >= read ) )
      {
         break;
      }
   }

   hb_retclen( b, x );
}

HB_FUNC( HB_FEOF )
{
   hb_retl( isEof[ area ] );
}

HB_FUNC( HB_FGOTO )
{
   long  target;
   long  last;

   target = hb_parnl( 1 );

   if( recno[ area ] > target )
   {
      while( recno[ area ] != target )
      {
         last = recno[ area ];
         hb_hbfskip( -1 );
         if( recno[ area ] == last )
            break;
      }
   }
   else
   {
      while( recno[ area ] != target )
      {
         last = recno[ area ];
         hb_hbfskip( 1 );
         if( recno[ area ] == last )
            break;
      }
   }
}

HB_FUNC( HB_FGOBOTTOM )
{
   if( last_rec[ area ] != 0 )
   {
      recno[ area ]  = last_rec[ area ];
      offset[ area ] = last_off[ area ];
   }
   else
   {
      HB_ISIZ     loc   = 0;
      HB_ISIZ     len;
      HB_FOFFSET  last  = offset[ area ];

      do
      {
         HB_ISIZ x;

         hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );
         len = hb_fsReadLarge( handles[ area ], c, _C_SIZE );

         for( x = 0; x < len; ++x )
         {
            if( ( ( *( c + x ) == 13 ) && ( *( c + x + 1 ) == 10 ) ) ||
                ( ( *( c + x ) == 10 ) && ( *( c + x + 1 ) == 13 ) ) ||
                ( x - loc > _B_SIZE ) )
            {
               last  = offset[ area ] + loc;
               recno[ area ]++;
               ++x;
               loc   = x + 1;
            }
         }
         offset[ area ] += loc;

      }
      while( len == _C_SIZE );

      last_rec[ area ]  = --recno[ area ];
      last_off[ area ]  = last;
   }
}

HB_FUNC( HB_FGOTOP )
{
   offset[ area ] = 0;
   recno[ area ]  = 1;
   isEof[ area ]  = ( lastbyte[ area ] == 0 );
}

HB_FUNC( HB_FLASTREC )
{
   long        old_rec;
   HB_FOFFSET  old_offset;
   HB_BOOL     bIsEof;

   old_rec     = recno[ area ];
   old_offset  = offset[ area ];
   bIsEof      = isEof[ area ];

   HB_FUNC_EXEC( HB_FGOBOTTOM );
   hb_retnl( last_rec[ area ] );

   recno[ area ]  = old_rec;
   offset[ area ] = old_offset;
   isEof[ area ]  = bIsEof;
}

HB_FUNC( HB_FSELECT )
{
   hb_retni( area + 1 );

   if( HB_ISNUM( 1 ) )
      area = hb_parni( 1 ) - 1;
}

HB_FUNC( HB_FINFO )                     /* used for debugging */
{
   hb_reta( 6 );
   hb_storvni( area + 1, -1, 1 );
   hb_storvni( last_rec[ area ], -1, 2 );
   hb_storvni( recno[ area ], -1, 3 );
   hb_storvnint( offset[ area ], -1, 4 );
   hb_storvnint( lastbyte[ area ], -1, 5 );
   hb_storvl(  isEof[ area ], -1, 6 );
}

HB_FUNC( HB_FREADANDSKIP )
{
/* ------------------------------------------------
   Warning: This is a rogue function! It is a first shot at adding the logic
   to read .csv records that respect CRLF embedded within quotes.
   It is very common, especially with Microsoft products, for
   comma-separated files to allow a field (usually an address field)
   to have hard returns within it. These records appear corrupted to any
   reader that presumes all hard returns are record separators.

   This function is useful right now to loop through a CSV file
   while !hb_feof(), but it does NOT recognize the same record count
   and positioning that the other functions in this file use.
   It does its own skip and read, so an entire file can be read
   sequentially with just this function.
   -BH
   --------------------------------------------------*/
   HB_ISIZ  x        = 0;
   HB_ISIZ  read;
   HB_BOOL  bInField = HB_FALSE, bHasCRLF = HB_FALSE;

   hb_fsSeekLarge( handles[ area ], offset[ area ], FS_SET );
   read = hb_fsReadLarge( handles[ area ], b, _B_SIZE );

   while( x < read )
   {
      if( *( b + x ) == '"' )
      {
         bInField = ! bInField;
         ++x;
         continue;
      }

      if( bInField )
      {
         ++x;
         continue;
      }

      if( ( ( *( b + x ) == 13 ) && x < read - 1 && ( *( b + x + 1 ) == 10 ) ) ||
          ( ( *( b + x ) == 10 ) && x < read - 1 && ( *( b + x + 1 ) == 13 ) ) )
      {
         x        += 2;
         bHasCRLF = HB_TRUE;
         break;
      }
      ++x;
   }

   offset[ area ] = offset[ area ] + x;
   recno[ area ]  += 1;
   /* See if there's more to read */
   if( ! isEof[ area ] )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = ( lastbyte[ area ] <= offset[ area ] + 1 );
      isEof[ area ]  = f;
#else
      isEof[ area ]  = ( lastbyte[ area ] <= offset[ area ] + 1 );
#endif
   }

   hb_retclen( b, x - ( bHasCRLF ? 2 : 0 ) );
}
