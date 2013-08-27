/*
 * Harbour Project source code:
 * The Virtual Memory API
 *
 * Copyright 1999-2007 {list of individual authors and e-mail addresses}
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* -------------------------------------------------------- */
/* Warning: VM functionality is not supported by Harbour.   */
/*          All functions will emulate constant failure.    */
/* -------------------------------------------------------- */

#include "hbapi.h"

HB_VMHANDLE hb_xvalloc( HB_SIZE nSize, HB_USHORT nFlags )
{
   /* TODO */
   HB_SYMBOL_UNUSED( nSize );
   HB_SYMBOL_UNUSED( nFlags );
   return 0;
}

void hb_xvfree( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
}

HB_VMHANDLE hb_xvrealloc( HB_VMHANDLE h, HB_SIZE nSize, HB_USHORT nFlags )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( nSize );
   HB_SYMBOL_UNUSED( nFlags );
   return 0;
}

void * hb_xvlock( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   return NULL;
}

void hb_xvunlock( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
}

/* Wire */

void * hb_xvwire( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   return NULL;
}

void hb_xvunwire( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
}


/* State */

HB_SIZE hb_xvlockcount( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   return 0;
}

HB_SIZE hb_xvsize( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   return 0;
}

/* Heap */

HB_VMHANDLE hb_xvheapnew( HB_SIZE nSize )
{
   /* TODO */
   HB_SYMBOL_UNUSED( nSize );
   return 0;
}

void hb_xvheapdestroy( HB_VMHANDLE h )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
}

HB_VMHANDLE hb_xvheapresize( HB_VMHANDLE h, HB_SIZE nSize )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( nSize );
   return 0;
}

HB_SIZE hb_xvheapalloc( HB_VMHANDLE h, HB_SIZE nSize )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( nSize );
   return 0;
}

void hb_xvheapfree( HB_VMHANDLE h, HB_SIZE nOffset )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( nOffset );
}

void * hb_xvheaplock( HB_VMHANDLE h, HB_SIZE nOffset )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( nOffset );
   return NULL;
}

void hb_xvheapunlock( HB_VMHANDLE h, HB_SIZE nOffset )
{
   /* TODO */
   HB_SYMBOL_UNUSED( h );
   HB_SYMBOL_UNUSED( nOffset );
}
