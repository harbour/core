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

#include "hbapifs.h"
#include "hbstack.h"

#define _B_SIZE         4096

/* up this number if you need more than 10 text file areas */
#define TEXT_WORKAREAS  10

typedef struct
{
   int        area;
   long       recno[ TEXT_WORKAREAS ];
   HB_FOFFSET offset[ TEXT_WORKAREAS ];
   HB_FHANDLE handles[ TEXT_WORKAREAS ];
   long       last_rec[ TEXT_WORKAREAS ];
   HB_FOFFSET last_off[ TEXT_WORKAREAS ];
   HB_FOFFSET lastbyte[ TEXT_WORKAREAS ];
   HB_BOOL    isEof[ TEXT_WORKAREAS ];
} FT_TEXT, * PFT_TEXT;

static void s_fttext_init_init( void * cargo )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) cargo;

   ft_text->area = 0;
}

static HB_TSD_NEW( s_fttext, sizeof( FT_TEXT ), s_fttext_init_init, NULL );

HB_FUNC( HB_FUSE )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   PHB_ITEM arg1_it = hb_param( 1, HB_IT_STRING );
   PHB_ITEM arg2_it = hb_param( 2, HB_IT_NUMERIC );
   int      open_flags;

   if( arg1_it )
   {
      if( arg2_it )
         open_flags = hb_parni( 2 );
      else
         open_flags = 0;

      ft_text->handles[ ft_text->area ]  = hb_fsOpen( hb_parc( 1 ), ( HB_USHORT ) open_flags );
      ft_text->offset[ ft_text->area ]   = 0;
      ft_text->recno[ ft_text->area ]    = 1;
      ft_text->lastbyte[ ft_text->area ] = hb_fsSeekLarge( ft_text->handles[ ft_text->area ], 0, FS_END );
      ft_text->isEof[ ft_text->area ]    = ( ft_text->lastbyte[ ft_text->area ] == 0 );
      hb_retnint( ft_text->handles[ ft_text->area ] );
   }
   else
   {
      hb_fsClose( ft_text->handles[ ft_text->area ] );
      hb_retnint( 1 );
      ft_text->recno[ ft_text->area ]    = 0;
      ft_text->offset[ ft_text->area ]   = 0;
      ft_text->handles[ ft_text->area ]  = 0;
      ft_text->last_rec[ ft_text->area ] = 0;
      ft_text->last_off[ ft_text->area ] = 0;
      ft_text->lastbyte[ ft_text->area ] = 0;
      ft_text->isEof[ ft_text->area ]    = HB_FALSE;
   }
}


HB_FUNC( HB_FRECNO )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retnl( ft_text->recno[ ft_text->area ] );
}


static long hb_hbfskip( PFT_TEXT ft_text, char * buffer, HB_SIZE bufsize, int recs )
{
   HB_FOFFSET read_pos;
   HB_ISIZ    read_len;
   HB_ISIZ    x;
   int        y;

   HB_TRACE( HB_TR_DEBUG, ( "hb_hbfskip(%p, %p, %d)", ft_text, buffer, recs ) );

   if( recs > 0 )
   {
      for( y = 0; y < recs; ++y )
      {
         hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );

         read_len = hb_fsReadLarge( ft_text->handles[ ft_text->area ], buffer, bufsize );

         for( x = 0; x < read_len; ++x )
         {
            if( ( ( *( buffer + x ) == 13 ) && ( *( buffer + x + 1 ) == 10 ) ) ||
                ( ( *( buffer + x ) == 10 ) && ( *( buffer + x + 1 ) == 13 ) ) )
            {
               break;
            }
         }

         if( ( ft_text->offset[ ft_text->area ] + x + 2 ) < ft_text->lastbyte[ ft_text->area ] )
         {
            ft_text->isEof[ ft_text->area ]   = HB_FALSE;
            ft_text->offset[ ft_text->area ] += ( x + 2 );
            ft_text->recno[ ft_text->area ]  += 1;
         }
         else
            ft_text->isEof[ ft_text->area ] = HB_TRUE;
      }
   }
   else
   {
      recs = -recs;
      ft_text->isEof[ ft_text->area ] = HB_FALSE;

      if( ( ft_text->recno[ ft_text->area ] - recs ) < 1 )
         return 1;

      for( y = recs; y > 0; --y )
      {
         read_pos = ( size_t ) ( ft_text->offset[ ft_text->area ] - bufsize );

         if( read_pos < 0 )
         {
            read_pos = 0;
            read_len = ( size_t ) ft_text->offset[ ft_text->area ];
         }
         else
            read_len = bufsize;

         hb_fsSeekLarge( ft_text->handles[ ft_text->area ], read_pos, FS_SET );
         read_len = hb_fsReadLarge( ft_text->handles[ ft_text->area ], buffer, read_len );

         for( x = read_len - 4; x >= 0; --x )
         {
            if( ( ( *( buffer + x ) == 13 ) && ( *( buffer + x + 1 ) == 10 ) ) ||
                ( ( *( buffer + x ) == 10 ) && ( *( buffer + x + 1 ) == 13 ) ) )
               break;
         }
         if( x < 0 )
         {
            ft_text->offset[ ft_text->area ] = 0;
            ft_text->recno[ ft_text->area ]  = 1;
         }
         else
         {
            ft_text->offset[ ft_text->area ] = read_pos + x + 2;
            ft_text->recno[ ft_text->area ]--;
         }
      }
   }

   return ft_text->recno[ ft_text->area ];
}

HB_FUNC( HB_FSKIP )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   char * buffer = ( char * ) hb_xgrab( _B_SIZE );

   hb_hbfskip( ft_text, buffer, _B_SIZE, hb_parnidef( 1, 1 ) );

   hb_xfree( buffer );
}

HB_FUNC( HB_FREADLN )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   char * buffer = ( char * ) hb_xgrab( _B_SIZE );

   HB_ISIZ x;
   HB_ISIZ read;

   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );

   read = hb_fsReadLarge( ft_text->handles[ ft_text->area ], buffer, _B_SIZE );

   for( x = 0; x < _B_SIZE; ++x )
   {
      if( ( ( *( buffer + x ) == 13 ) && ( *( buffer + x + 1 ) == 10 ) ) ||
          ( ( *( buffer + x ) == 10 ) && ( *( buffer + x + 1 ) == 13 ) ) ||
          ( *( buffer + x ) == 26 ) || x >= read )
         break;
   }

   hb_retclen( buffer, x );

   hb_xfree( buffer );
}

HB_FUNC( HB_FEOF )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retl( ft_text->isEof[ ft_text->area ] );
}

HB_FUNC( HB_FGOTO )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   char * buffer = ( char * ) hb_xgrab( _B_SIZE );

   long target = hb_parnl( 1 );

   if( ft_text->recno[ ft_text->area ] > target )
   {
      while( ft_text->recno[ ft_text->area ] != target )
      {
         long last = ft_text->recno[ ft_text->area ];
         hb_hbfskip( ft_text, buffer, _B_SIZE, -1 );
         if( ft_text->recno[ ft_text->area ] == last )
            break;
      }
   }
   else
   {
      while( ft_text->recno[ ft_text->area ] != target )
      {
         long last = ft_text->recno[ ft_text->area ];
         hb_hbfskip( ft_text, buffer, _B_SIZE, 1 );
         if( ft_text->recno[ ft_text->area ] == last )
            break;
      }
   }

   hb_xfree( buffer );
}

HB_FUNC( HB_FGOBOTTOM )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   if( ft_text->last_rec[ ft_text->area ] != 0 )
   {
      ft_text->recno[ ft_text->area ]  = ft_text->last_rec[ ft_text->area ];
      ft_text->offset[ ft_text->area ] = ft_text->last_off[ ft_text->area ];
   }
   else
   {
      char * buffer = ( char * ) hb_xgrab( _B_SIZE );

      HB_ISIZ    loc = 0;
      HB_ISIZ    len;
      HB_FOFFSET last = ft_text->offset[ ft_text->area ];

      do
      {
         HB_ISIZ x;

         hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );
         len = hb_fsReadLarge( ft_text->handles[ ft_text->area ], buffer, _B_SIZE );

         for( x = 0; x < len; ++x )
         {
            if( ( ( *( buffer + x ) == 13 ) && ( *( buffer + x + 1 ) == 10 ) ) ||
                ( ( *( buffer + x ) == 10 ) && ( *( buffer + x + 1 ) == 13 ) ) ||
                ( x - loc > _B_SIZE ) )
            {
               last = ft_text->offset[ ft_text->area ] + loc;
               ft_text->recno[ ft_text->area ]++;
               ++x;
               loc = x + 1;
            }
         }
         ft_text->offset[ ft_text->area ] += loc;

      }
      while( len == _B_SIZE );

      ft_text->last_rec[ ft_text->area ] = --ft_text->recno[ ft_text->area ];
      ft_text->last_off[ ft_text->area ] = last;

      hb_xfree( buffer );
   }
}

HB_FUNC( HB_FGOTOP )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   ft_text->offset[ ft_text->area ] = 0;
   ft_text->recno[ ft_text->area ]  = 1;
   ft_text->isEof[ ft_text->area ]  = ( ft_text->lastbyte[ ft_text->area ] == 0 );
}

HB_FUNC( HB_FLASTREC )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   long       old_rec;
   HB_FOFFSET old_offset;
   HB_BOOL    bIsEof;

   old_rec    = ft_text->recno[ ft_text->area ];
   old_offset = ft_text->offset[ ft_text->area ];
   bIsEof     = ft_text->isEof[ ft_text->area ];

   HB_FUNC_EXEC( HB_FGOBOTTOM );
   hb_retnl( ft_text->last_rec[ ft_text->area ] );

   ft_text->recno[ ft_text->area ]  = old_rec;
   ft_text->offset[ ft_text->area ] = old_offset;
   ft_text->isEof[ ft_text->area ]  = bIsEof;
}

HB_FUNC( HB_FSELECT )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_retni( ft_text->area + 1 );

   if( HB_ISNUM( 1 ) )
      ft_text->area = hb_parni( 1 ) - 1;
}

HB_FUNC( HB_FINFO )                     /* used for debugging */
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   hb_reta( 6 );
   hb_storvni( ft_text->area + 1, -1, 1 );
   hb_storvni( ft_text->last_rec[ ft_text->area ], -1, 2 );
   hb_storvni( ft_text->recno[ ft_text->area ], -1, 3 );
   hb_storvnint( ft_text->offset[ ft_text->area ], -1, 4 );
   hb_storvnint( ft_text->lastbyte[ ft_text->area ], -1, 5 );
   hb_storvl(  ft_text->isEof[ ft_text->area ], -1, 6 );
}

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
 */
HB_FUNC( HB_FREADANDSKIP )
{
   PFT_TEXT ft_text = ( PFT_TEXT ) hb_stackGetTSD( &s_fttext );

   char * buffer = ( char * ) hb_xgrab( _B_SIZE );

   HB_ISIZ x = 0;
   HB_ISIZ read;
   HB_BOOL bInField = HB_FALSE, bHasCRLF = HB_FALSE;

   hb_fsSeekLarge( ft_text->handles[ ft_text->area ], ft_text->offset[ ft_text->area ], FS_SET );
   read = hb_fsReadLarge( ft_text->handles[ ft_text->area ], buffer, _B_SIZE );

   while( x < read )
   {
      if( *( buffer + x ) == '"' )
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

      if( ( ( *( buffer + x ) == 13 ) && x < read - 1 && ( *( buffer + x + 1 ) == 10 ) ) ||
          ( ( *( buffer + x ) == 10 ) && x < read - 1 && ( *( buffer + x + 1 ) == 13 ) ) )
      {
         x       += 2;
         bHasCRLF = HB_TRUE;
         break;
      }
      ++x;
   }

   ft_text->offset[ ft_text->area ] = ft_text->offset[ ft_text->area ] + x;
   ft_text->recno[ ft_text->area ] += 1;
   /* See if there's more to read */
   if( ! ft_text->isEof[ ft_text->area ] )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = ( ft_text->lastbyte[ ft_text->area ] <= ft_text->offset[ ft_text->area ] + 1 );
      ft_text->isEof[ ft_text->area ] = f;
#else
      ft_text->isEof[ ft_text->area ] = ( ft_text->lastbyte[ ft_text->area ] <= ft_text->offset[ ft_text->area ] + 1 );
#endif
   }

   hb_retclen( buffer, x - ( bHasCRLF ? 2 : 0 ) );

   hb_xfree( buffer );
}
