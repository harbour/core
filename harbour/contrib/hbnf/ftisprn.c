/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FT_ISPRINT()
 *
 * Copyright 1999-2008 Viktor Szakats (harbour.01 syenar.hu)
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

/* File......: isprint.asm
* Author....: Ted Means
* CIS ID....: 73067,3332
*
* This function is an original work by Ted Means and is placed in the
* public domain.  I got the idea from Norm Mongeau, but the code is
* all mine.
*
* Modification history:
* ---------------------
*
*     Rev 1.3   16 Jul 1993 00:00:18   GLENN
*  Modified for compatibility in protected mode under ExoSpace.  Should
*  work in real mode as well.
*
*     Rev 1.2   15 Aug 1991 23:07:56   GLENN
*  Forest Belt proofread/edited/cleaned up doc
*
*     Rev 1.1   14 Jun 1991 19:54:38   GLENN
*  Minor edit to file header
*
*     Rev 1.0   01 Apr 1991 01:03:26   GLENN
*  Nanforum Toolkit
*
*/


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ISPRINT()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Check printer status
 *  $SYNTAX$
 *     FT_ISPRINT( [ <cDevice> ] ) -> lResult
 *  $ARGUMENTS$
 *     <cDevice> is optional and is the device to test (LPT2, COM1, etc.).
 *     If omitted, the function will default to the PRN device.
 *  $RETURNS$
 *     .T.  if device is ready for output.
 *     .F.  if one of the following conditions occurs:
 *          1)  The device is not ready.
 *          2)  The device does not exist.
 *          3)  DOS couldn't open the device for some reason
 *              (such as no file handles available).
 *  $DESCRIPTION$
 *     The Clipper IsPrinter() function is somewhat limited because it only
 *     works with LPT1.  Furthermore, it talks directly to the hardware, so
 *     if you have redirected LPT1 via the DOS MODE command, the IsPrinter()
 *     function will return erroneous results.
 *
 *     This function offers a better alternative.  Instead of talking to the
 *     hardware, it issues a DOS call that checks to see if the device is
 *     ready or not.  That gives DOS an opportunity to deal with any
 *     redirections, and since you pass the device name as a parameter, you
 *     can test any device, not just LPT1 (note that the function defaults
 *     to PRN if you fail to pass a valid parameter).
 *
 *     The function also temporarily traps the DOS critical error handler so
 *     you don't get any nasty error messages if the device isn't ready.  It
 *     restores the old critical error handler before exiting.
 *
 *     Note that although this function is mainly designed for testing
 *     printers, you can also check to see if a drive is ready.  Since DOS
 *     thinks the NUL device exists on every drive, you can pass a drive
 *     letter followed by NUL as a parameter.  If DOS is able to open the
 *     NUL device, then the drive is ready, otherwise the door is open or
 *     something else is wrong.
 *
 *     The source code is written to adhere to Turbo Assembler's IDEAL mode.
 *     To use another assembler, you will need to rearrange the PROC and
 *     SEGMENT directives, and also the ENDP and ENDS directives (a very
 *     minor task).
 *  $EXAMPLES$
 *     IF ! FT_ISPRINT()
 *        Qout( "PRN is not ready!" )
 *     ENDIF
 *
 *     IF ! FT_ISPRINT( "COM2" )
 *        Qout( "Check the device on COM2.  Something is wrong." )
 *     ENDIF
 *
 *     IF ! FT_ISPRINT( "A:\nul" )
 *        Qout( "Oops, better check drive A!" )
 *     ENDIF
 *  $END$
 */

#include "hbapi.h"

/* TOFIX: Has different behaviour depending on platform/parameter. [vszakats] */

HB_FUNC_EXTERN( HB_ISPRINTER );

HB_FUNC( FT_ISPRINT )
{
   HB_FUNC_EXEC( HB_ISPRINTER )
}
