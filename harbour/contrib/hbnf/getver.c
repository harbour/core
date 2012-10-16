/*
 * $Id$
 */

/*
 * Harbour Project source code
 *
 * Getver.c Support functions for Nanfor Library
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
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

#include "hbapi.h"
#include "hbapiitm.h"

#if defined( HB_OS_DOS )
#  include "dos.h"
#endif

HB_FUNC( FT_DOSVER )
{
#if defined( HB_OS_DOS )
   {
      char *      pszPlatform;
      union REGS  regs;
      pszPlatform = ( char * ) hb_xgrab( 256 );

      regs.h.ah   = 0x30;
      HB_DOS_INT86( 0x21, &regs, &regs );

      hb_snprintf( pszPlatform, 256, "%d.%02d", regs.h.al, regs.h.ah );

      hb_retc_buffer( pszPlatform );
   }
#else
   hb_retc_null();
#endif
}

HB_FUNC( FT_ISSHARE )
{
   int iShare;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax  = 0x1000;
      regs.HB_XREGS.cx  = 0;
      HB_DOS_INT86( 0x2F, &regs, &regs );
      iShare            = regs.h.al;
   }
#else
   {
      iShare = 0;
   }
#endif

   hb_retni( iShare );
}

HB_FUNC( _FT_NWKSTAT )
{
   int iConnect;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.HB_XREGS.ax = 0xDC;
      HB_DOS_INT86( 0x2F, &regs, &regs );
      iConnect = regs.h.al;
   }
#else
   {
      iConnect = 0;
   }
#endif

   hb_retni( iConnect );
}

HB_FUNC( FT_SETMODE )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.h.ah = 0;
      regs.h.al = hb_parni( 1 );
      HB_DOS_INT86( 0x10, &regs, &regs );
   }
#endif
}

HB_FUNC( FT_GETMODE )
{
   int iMode;

#if defined( HB_OS_DOS )
   {
      union REGS regs;
      regs.h.ah = 0x0F;
      HB_DOS_INT86( 0x10, &regs, &regs );
      iMode = regs.h.al;
   }
#else
   {
      iMode = 0;
   }
#endif

   hb_retni( iMode );
}

HB_FUNC( _FT_TEMPFIL )
{
   int            nax;
   int            iflags;
   const char *   cPath;

#if defined( HB_OS_DOS ) && ! defined( HB_OS_DOS_32 )
   {
      int            iMode = hb_parni( 2 );
      union REGS     regs;
      struct SREGS   sregs;
      segread( &sregs );
      cPath             = hb_parcx( 1 );
      regs.h.ah         = 0x5A;
      regs.HB_XREGS.cx  = iMode;
      sregs.ds          = FP_SEG( cPath );
      regs.HB_XREGS.dx  = FP_OFF( cPath );
      HB_DOS_INT86X( 0x21, &regs, &regs, &sregs );
      nax               = regs.HB_XREGS.ax;
      iflags            = regs.HB_XREGS.flags;
   }
#else
   {
      nax      = 0;
      iflags   = 0;
      cPath    = hb_parcx( 1 );
   }
#endif
   {
      PHB_ITEM pArray = hb_itemArrayNew( 3 );

      hb_arraySetNI( pArray, 1, nax );
      hb_arraySetC(  pArray, 2, cPath );
      hb_arraySetNI( pArray, 3, iflags );

      hb_itemReturnRelease( pArray );
   }
}
