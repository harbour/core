/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Standalone Harbour Portable Object file runner
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * www - http://www.harbour-project.org
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *   added support for dynamic compilation and execution of .prg files
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

/*
 * Please remember that in *nixes if you copy compiled version
 * of this program to /usr/bin directory then you can add to your
 * .prg files as first line:
 *    #!/usr/bin/hbrun
 * and then after setting executable attribute you can directly execute
 * your .prg files
 * If you are using Linux then you can also chose default gt driver by
 * adding to above line: //gt<name>
 * F.e.
 *    #!/usr/bin/hbrun //gtstd
 */

#include "hbextern.ch"

/* NOTE: Undocumented CA-Clipper _APPMAIN is used instead of Main to avoid
         collision with user function in HRB file with that name. [ckedem]
*/
FUNCTION _APPMAIN( cHRBFile, ... )
   LOCAL xRetVal, cPath, cName, cExt, cDrive, aIncDir

   IF Empty( cHRBFile )
      OutStd( "Harbour Runner" + HB_OSNewLine() +;
              "Copyright 1999-2008, http://www.harbour-project.org" + HB_OSNewLine() +;
              HB_OSNewLine() +;
              "Syntax:  hbrun <hrbfile[.hrb|.prg]> [parameters]" + HB_OSNewLine() + ;
              HB_OSNewLine() +;
              "Note:  Linked with " + Version() + HB_OSNewLine() )
      ERRORLEVEL( 1 )
   ELSE
      HB_FNAMESPLIT( cHRBFile, @cPath, @cName, @cExt, @cDrive )

      IF LOWER( cExt ) == ".prg"
         aIncDir := {}
#ifdef _DEFAULT_INC_DIR
         AADD( aIncDir, "-I" + _DEFAULT_INC_DIR )
#endif
         cPath := getenv( "HB_INC_INSTALL" )
         IF !EMPTY( cPath )
            AADD( aIncDir, "-I" + cPath )
         ENDIF
#ifdef __PLATFORM__UNIX
         AADD( aIncDir, "-I/usr/include/harbour" )
         AADD( aIncDir, "-I/usr/local/include/harbour" )
#endif
         cHRBFile := HB_COMPILEBUF( HB_ARGV( 0 ), "-n", "-w", "-es2", "-q0", ;
                                    aIncDir, cHRBFile )
         IF cHRBFile == NIL
            ERRORLEVEL( 1 )
         ELSE
            xRetVal := __hrbRun( cHRBFile, ... )
         ENDIF
      ELSE
         xRetVal := __hrbRun( cHRBFile, ... )
      ENDIF
   ENDIF

   RETURN xRetVal
