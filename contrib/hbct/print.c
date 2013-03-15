/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CT3 Printer functions:
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net) (PrintReady())
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru> (PrintSend())
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar> (PrintStat())
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

#if defined( HB_OS_DOS )
#  include <dos.h>
#  if defined( __DJGPP__ )
#     include <dpmi.h>
#  endif
#endif

HB_FUNC( PRINTSTAT )
{
   HB_USHORT uiPort = ( HB_USHORT ) hb_parnidef( 1, 1 );
   int Status = 0;

#if defined( HB_OS_DOS )

   /* NOTE: MS-DOS specific solution, using BIOS interrupt */

   union REGS regs;

   regs.h.ah = 2;
   regs.HB_XREGS.dx = uiPort - 1;

   HB_DOS_INT86( 0x17, &regs, &regs );

   Status = regs.h.ah;

#else
   HB_SYMBOL_UNUSED( uiPort );
#endif

   hb_retni( Status );
}

HB_FUNC( PRINTREADY )
{
   char szLPT[ 8 ];

   hb_snprintf( szLPT, sizeof( szLPT ), "LPT%hu", ( HB_USHORT ) hb_parnidef( 1, 1 ) );

   hb_retl( hb_printerIsReady( szLPT ) );
}

HB_FUNC( PRINTSEND )
{
#ifdef __DJGPP__
   __dpmi_regs r;

   r.x.dx = hb_parni( 2 ) - 1;

   if( HB_ISNUM( 1 ) )
   {
      r.h.al = hb_parni( 1 );
      __dpmi_int( 0x17, &r );
      if( r.h.ah & 1 )
         hb_retni( 1 );
      else
         hb_retni( 0 );
   }
   else if( HB_ISCHAR( 1 ) )
   {
      const char * string = hb_parcx( 1 );
      int i, len = hb_parclen( 1 );

      r.h.ah = 0;
      for( i = 0; i < len && !( r.h.ah & 1 ); i++ )
      {
         r.h.al = string[ i ];
         __dpmi_int( 0x17, &r );
      }
      if( r.h.ah & 1 )
         hb_retni( len - ( i - 1 ) );
      else
         hb_retni( 0 );
   }

#elif defined( HB_OS_WIN )

   char szChr[ 2 ] = { ' ', '\0' };
   char szPort[ 5 ] = { 'l', 'p', 't', '1', '\0' };
   const char * szStr = NULL;
   HB_SIZE nLen = 0, nRet = 0;

   if( HB_ISNUM( 1 ) )
   {
      szChr[ 0 ] = ( char ) hb_parni( 1 );
      szStr = szChr;
      nLen = 1;
   }
   else if( HB_ISCHAR( 1 ) )
   {
      szStr = hb_parc( 1 );
      nLen = hb_parclen( 1 );
   }

   if( HB_ISNUM( 2 ) )
      szPort[ 3 ] = ( char ) hb_parni( 2 ) + '0';

   if( nLen )
   {
      HB_FHANDLE hFile = hb_fsOpen( szPort, FO_WRITE );
      if( hFile != FS_ERROR )
      {
         nRet = hb_fsWriteLarge( hFile, szStr, nLen );
         hb_fsClose( hFile );
      }
   }
   hb_retns( nRet );

#else

   hb_retns( 0 );

#endif
}
