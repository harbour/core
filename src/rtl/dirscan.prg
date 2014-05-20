/*
 * Harbour Project source code:
 *    hb_DirScan()
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "directry.ch"

STATIC FUNCTION hb_doScan( cPath, cMask, cAttr, cPathSep )

   LOCAL aFile
   LOCAL lMatch
   LOCAL aResult := {}

   FOR EACH aFile IN hb_Directory( cPath + hb_osFileMask(), cAttr + "D" )
      lMatch := hb_FileMatch( aFile[ F_NAME ], cMask )
      IF "D" $ aFile[ F_ATTR ]
         IF lMatch .AND. "D" $ cAttr
            AAdd( aResult, aFile )
         ENDIF
         IF !( aFile[ F_NAME ] == "." .OR. aFile[ F_NAME ] == ".." .OR. aFile[ F_NAME ] == "" )
            AEval( hb_DoScan( cPath + aFile[ F_NAME ] + cPathSep, cMask, cAttr, cPathSep ), ;
               {| x | x[ F_NAME ] := aFile[ F_NAME ] + cPathSep + x[ F_NAME ], ;
               AAdd( aResult, x ) } )
         ENDIF
      ELSEIF lMatch
         AAdd( aResult, aFile )
      ENDIF
   NEXT

   RETURN aResult

FUNCTION hb_DirScan( cPath, cFileMask, cAttr )

   hb_default( @cPath, "" )

   RETURN HB_DoScan( hb_DirSepAdd( cPath ), ;
      iif( HB_ISSTRING( cFileMask ), cFileMask, hb_osFileMask() ), ;
      iif( HB_ISSTRING( cAttr ), cAttr, "" ), ;
      hb_ps() )
