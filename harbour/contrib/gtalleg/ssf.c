/*
 * $Id$
 */

/*
 *
 * This file was conceived while I was developing an allegro based gt
 * (gtAlleg) for xHarbour, so it is brought under the same license terms.
 *
 */

/*
 * xHarbour Project source code:
 * Simple Scalable Font library, main C module.
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http: (yet to be constructed...)
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

#include <stdio.h>
#include <stdlib.h>

#include "hbapi.h"

#include "ssf.h"
#include "fixedth.sfc"

#define _F_LEFT     0
#define _F_TOP      1
#define _F_RIGHT    2
#define _F_BOTTOM   3
#define _F_LEFT2    4
#define _F_TOP2     5
#define _F_RIGHT2   6
#define _F_BOTTOM2  7

void ssfCreateThinFont( ssfFont * sfont )
{
   sfont->fsize = 16;                     /* default pitch */
   sfont->chars = s_ssfFixedThinChars;    /* chars */
}

void ssfSetFontSize( ssfFont * sfont, unsigned short fsize )
{
   sfont->fsize = fsize;
}

unsigned short ssfDrawChar( AL_BITMAP * dst, ssfFont * sfont, char c, int x, int y, int color )
{
   HB_BYTE  p;
   int      i, j, thick;
   ssfGlyph charGlyph;
   ssfFrame charFrame;
   int      points[ 8 ];
   float    fScale;

   p         = ( HB_BYTE ) c;
   charGlyph = *sfont->chars[ p ];
   fScale    = ( float ) ( ( float ) sfont->fsize / ( float ) 65535 );

   for( i = 0; i < charGlyph.num; i++ )
   {
      charFrame = charGlyph.frames[ i ];
      if( charFrame.ftype == SSF_SPLINE2 )
      {
         points[ _F_LEFT2 ]   = x + ( int ) ( fScale * charFrame.left );
         points[ _F_TOP2 ]    = y + ( int ) ( fScale * charFrame.top );
         points[ _F_RIGHT2 ]  = x + ( int ) ( fScale * charFrame.right );
         points[ _F_BOTTOM2 ] = y + ( int ) ( fScale * charFrame.bottom );
      }
      else
      {
         points[ _F_LEFT ]   = x + ( int ) ( fScale * charFrame.left );
         points[ _F_TOP ]    = y + ( int ) ( fScale * charFrame.top );
         points[ _F_RIGHT ]  = x + ( int ) ( fScale * charFrame.right );
         points[ _F_BOTTOM ] = y + ( int ) ( fScale * charFrame.bottom );
      }

      switch( charFrame.ftype )
      {
         case SSF_SPLINE2:
            thick = ( int ) ( fScale * charFrame.thick );

            if( thick == 0 )
               thick++;

            for( j = 0; j < thick; j++ )
            {
               al_draw_spline( dst, points, color );
               switch( charFrame.thickdir )
               {
                  case THICK_LEFT:
                     points[ _F_LEFT ]--;
                     points[ _F_RIGHT ]--;
                     points[ _F_LEFT2 ]--;
                     points[ _F_RIGHT2 ]--;
                     break;
                  case THICK_UP:
                     points[ _F_TOP ]--;
                     points[ _F_BOTTOM ]--;
                     points[ _F_TOP2 ]--;
                     points[ _F_BOTTOM2 ]--;
                     break;
                  case THICK_RIGHT:
                     points[ _F_LEFT ]++;
                     points[ _F_RIGHT ]++;
                     points[ _F_LEFT2 ]++;
                     points[ _F_RIGHT2 ]++;
                     break;
                  case THICK_DOWN:
                     points[ _F_TOP ]++;
                     points[ _F_BOTTOM ]++;
                     points[ _F_TOP2 ]++;
                     points[ _F_BOTTOM2 ]++;
                     break;
               }
            }
            break;

         case SSF_LINE:
            thick = ( int ) ( fScale * charFrame.thick );

            if( thick == 0 )
               thick++;

            for( j = 0; j < thick; j++ )
            {
               al_draw_line( dst, points[ _F_LEFT ], points[ _F_TOP ], points[ _F_RIGHT ], points[ _F_BOTTOM ], color );
               switch( charFrame.thickdir )
               {
                  case THICK_LEFT:
                     points[ _F_LEFT ]--;
                     points[ _F_RIGHT ]--;
                     break;
                  case THICK_UP:
                     points[ _F_TOP ]--;
                     points[ _F_BOTTOM ]--;
                     break;
                  case THICK_RIGHT:
                     points[ _F_LEFT ]++;
                     points[ _F_RIGHT ]++;
                     break;
                  case THICK_DOWN:
                     points[ _F_TOP ]++;
                     points[ _F_BOTTOM ]++;
                     break;
               }
            }
            break;

         case SSF_BOX:
            al_draw_rect_fill( dst, points[ _F_LEFT ], points[ _F_TOP ], points[ _F_RIGHT ], points[ _F_BOTTOM ], color );
            break;

         case SSF_TRIANGLE:
            thick = x + ( int ) ( fScale * charFrame.thick );
            al_draw_triangle( dst, points[ _F_LEFT ], points[ _F_TOP ], points[ _F_RIGHT ], points[ _F_BOTTOM ], thick, y + ( int ) ( fScale * charFrame.thickdir ), color );
            break;
      }
   }

   return sfont->fsize / 2;
}

int ssfDrawText( AL_BITMAP * dst, ssfFont * sfont, const char * s, int x, int y, int color )
{
   int i = 0;

   while( s[ i ] )
   {
      x += ssfDrawChar( dst, sfont, s[ i ], x, y, color );
      i++;
   }

   return x;
}
