/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Printer functions: - PRINTSTAT() / PRINTREADY()
 *
 * Copyright 2001 Walter Negro - FOEESITRA" <waltern@foeesitra.org.ar>
 * www - http://www.harbour-project.org
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
#include "hbapifs.h"


/*  $DOC$
 *  $FUNCNAME$
 *      PRINTSTAT()
 *  $CATEGORY$
 *      CT3 printer functions
 *  $ONELINER$
 *  $SYNTAX$
 *      PRINTSTAT ([<nPrinter>]) -> nState
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      DOS
 *  $FILES$
 *      Source is print.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( PRINTSTAT )
{
   USHORT uiPort =  ISNUM( 1 ) ? hb_parni( 1 ) : 1;
   int    Status = 0;

#if defined(HB_OS_DOS)

   /* NOTE: DOS specific solution, using BIOS interrupt */

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


/*  $DOC$
 *  $FUNCNAME$
 *      PRINTREADY()
 *  $CATEGORY$
 *      CT3 printer functions
 *  $ONELINER$
 *  $SYNTAX$
 *      PRINTREADY ([<nPrinter>]) -> lPrinterReady
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      DOS
 *  $FILES$
 *      Source is print.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( PRINTREADY )
{
   USHORT uiPort =  ISNUM( 1 ) ? hb_parni( 1 ) : 1;
   int    Status = 0;

#if defined(HB_OS_DOS)

   /* NOTE: DOS specific solution, using BIOS interrupt */

   union REGS regs;

   regs.h.ah = 2;
   regs.HB_XREGS.dx = uiPort - 1;

   HB_DOS_INT86( 0x17, &regs, &regs );

   Status = regs.h.ah;

#else
   HB_SYMBOL_UNUSED( uiPort );
#endif

   hb_retl( (Status == 0x90) );
}


