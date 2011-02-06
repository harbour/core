/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * libmagic wrapper functions
 *
 * Copyright 2010 Tamas TEVESZ <ice@extreme.hu>
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

#include <magic.h>

#include "hbapi.h"
#include "hbapierr.h"

static HB_GARBAGE_FUNC( magic_gc_close )
{
   void ** ph = ( void ** ) Cargo;

   if( ph && * ph )
   {
      magic_close( ( magic_t ) * ph );
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcmagic_gcfuncs = {
   magic_gc_close,
   hb_gcDummyMark
};

static magic_t magic_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcmagic_gcfuncs, iParam );

   return ph ? ( magic_t ) * ph : NULL;
}

HB_FUNC( MAGIC_OPEN )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( magic_t ), &s_gcmagic_gcfuncs );

   * ph = magic_open( hb_parnidef( 1, MAGIC_NONE ) );
   hb_retptrGC( ph );
}

HB_FUNC( MAGIC_ERROR )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retc( magic_error( m ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MAGIC_ERRNO )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retni( magic_errno( m ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MAGIC_FILE )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retc( magic_file( m, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MAGIC_BUFFER )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retc( magic_buffer( m, hb_parc( 2 ), hb_parclen( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MAGIC_SETFLAGS )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retni( magic_setflags( m, hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MAGIC_CHECK )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retni( magic_check( m, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MAGIC_COMPILE )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retni( magic_compile( m, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MAGIC_LOAD )
{
   magic_t m = magic_par( 1 );

   if( m )
      hb_retni( magic_load( m, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
