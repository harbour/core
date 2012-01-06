/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File find API
 *
 * Copyright 1991-2009 Viktor Szakats (harbour syenar.net)
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

static HB_GARBAGE_FUNC( PHB_FFIND_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      hb_fsFindClose( ( PHB_FFIND ) * ph );

      /* set pointer to NULL just in case */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcPHB_FFIND_funcs =
{
   PHB_FFIND_release,
   hb_gcDummyMark
};

static PHB_FFIND PHB_FFIND_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcPHB_FFIND_funcs, iParam );

   return ph ? ( PHB_FFIND ) * ph : NULL;
}

HB_FUNC( FILEFINDFIRST )
{
   const char * pszFileName = hb_parc( 1 );

   if( pszFileName && HB_ISBYREF( 2 ) )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( PHB_FFIND ), &s_gcPHB_FFIND_funcs );

      PHB_FFIND ffind = hb_fsFindFirst( pszFileName, hb_parnldef( 3, HB_FA_ALL ) );

      * ph = ( void * ) ffind;

      hb_storptrGC( ph, 2 );

      hb_retl( ffind != NULL );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( FILEFINDNEXT )
{
   PHB_FFIND ffind = PHB_FFIND_par( 1 );

   hb_retl( ffind && hb_fsFindNext( ffind ) );
}

HB_FUNC( FILEFINDNAME )
{
   PHB_FFIND ffind = PHB_FFIND_par( 1 );

   hb_retc( ffind ? ffind->szName : NULL );
}

HB_FUNC( FILEFINDATTR )
{
   PHB_FFIND ffind = PHB_FFIND_par( 1 );

   hb_retnl( ffind ? ffind->attr : 0 );
}

HB_FUNC( FILEFINDSIZE )
{
   PHB_FFIND ffind = PHB_FFIND_par( 1 );

   hb_retnint( ffind ? ffind->size : 0 );
}

HB_FUNC( FILEFINDDATE )
{
   PHB_FFIND ffind = PHB_FFIND_par( 1 );

   hb_retds( ffind ? ffind->szDate : NULL );
}

HB_FUNC( FILEFINDTIME )
{
   PHB_FFIND ffind = PHB_FFIND_par( 1 );

   hb_retc( ffind ? ffind->szTime : NULL );
}
