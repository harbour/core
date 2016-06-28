/*
 * MyZip utility
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas.at.dbtopas.lt>
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

#require "hbmzip"

#include "directry.ch"
#include "simpleio.ch"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main()

   LOCAL hZip, aFile, aWild, lUnicode, tmp
   LOCAL cZipName, cPath, cFileName, cExt, cWild, cPassword, cComment

   IF lUnicode := ( "--unicode" $ hb_CmdLine() )
      hb_cdpSelect( "UTF8EX" )
      hb_SetTermCP( hb_cdpTerm() )
      Set( _SET_OSCODEPAGE, hb_cdpOS() )
   ENDIF

   aWild := hb_AParams()
   IF Len( aWild ) < 2
      ? "Usage: myzip <ZipName> [ --pass <password> ] [ --unicode ] [ --comment <comment> ] <FilePattern1> [ <FilePattern2> ... ]"
      RETURN
   ENDIF

   cZipName := hb_FNameExtSetDef( aWild[ 1 ], ".zip" )
   hb_ADel( aWild, 1, .T. )

   FOR tmp := Len( aWild ) - 1 TO 1 STEP -1
      SWITCH Lower( aWild[ tmp ] )
      CASE "--pass"
         IF cPassword == NIL
            cPassword := aWild[ tmp + 1 ]
         ENDIF
         aWild[ tmp ] := ""
         aWild[ tmp + 1 ] := ""
         EXIT
      CASE "--comment"
         IF cComment == NIL
            cComment := aWild[ tmp + 1 ]
         ENDIF
         aWild[ tmp ] := ""
         aWild[ tmp + 1 ] := ""
         EXIT
      CASE "--unicode"
         /* skip */
         EXIT
      ENDSWITCH
   NEXT

   IF ! Empty( hZip := hb_zipOpen( cZipName ) )
      ? "Archive file:", cZipName
      FOR EACH cWild IN aWild
         IF ! HB_ISNULL( cWild )
            hb_FNameSplit( cWild, @cPath, @cFileName, @cExt )
            FOR EACH aFile IN hb_DirScan( cPath, cFileName + cExt )
               IF ! cPath + aFile[ F_NAME ] == cZipName
                  ? "Adding", cPath + aFile[ F_NAME ]
                  hb_zipStoreFile( hZip, cPath + aFile[ F_NAME ], cPath + aFile[ F_NAME ], cPassword,, lUnicode )
               ENDIF
            NEXT
         ENDIF
      NEXT
      hb_zipClose( hZip, cComment )
   ENDIF

   RETURN

INIT PROCEDURE ClipInit()

   IF "--unicode" $ hb_CmdLine()
      hb_cdpSelect( "UTF8EX" )
   ENDIF

   RETURN
