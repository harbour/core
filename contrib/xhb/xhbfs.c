/*
 * Harbour Project source code:
 * xhb compatibility wrappers.
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( ISDIRECTORY )
{
   HB_BOOL bRetVal;

#if defined( HB_OS_WIN ) && 0 /* Document, but don't replicate xhb bug. */
   bRetVal = hb_fsDirExists( hb_parcx( 1 ) );
#else
   {
      PHB_FFIND ffind = hb_fsFindFirst( hb_parcx( 1 ), HB_FA_DIRECTORY );

      if( ffind )
      {
         bRetVal = ( ffind->attr & HB_FA_DIRECTORY );
         hb_fsFindClose( ffind );
      }
      else
         bRetVal = HB_FALSE;
   }
#endif

   hb_fsSetError( 0 );

   hb_retl( bRetVal );
}

HB_FUNC_TRANSLATE( ISDIR, ISDIRECTORY )
HB_FUNC_TRANSLATE( DISABLEWAITLOCKS, HB_DISABLEWAITLOCKS )
