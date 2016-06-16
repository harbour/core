/*
 * xHarbour compatible WIN32PRN class which inherits from WIN_PRN class
 * hiding some differences between Harbour and xHarbour
 *
 * this wrapper:
 *    Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * original WIN32PRN/WIN_PRN class author:
 *    Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

#ifndef __PLATFORM__WINDOWS

FUNCTION Win32Prn()
   RETURN NIL

FUNCTION Win32Bmp()
   RETURN NIL

#else

#include "hbclass.ch"
#include "hbwin.ch"

CREATE CLASS Win32Prn INHERIT win_Prn

   METHOD New( cPrinter )
   METHOD Create()
   METHOD StartPage()

   METHOD TextOut( cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert )
   METHOD TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert )
   METHOD TextAtFont( nPosX, nPosY, cString, cFont, nPointSize, ;
      nWidth, nBold, lUnderLine, lItalic, lNewLine, ;
      lUpdatePosX, nColor, nAlignHori, nAlignVert )

   VAR SetTextHori      INIT WIN_TA_LEFT      // Default horizontal alignment SetTextAlign() (TextOut())
   VAR SetTextVert      INIT WIN_TA_BOTTOM    // Default vertical alignment for SetTextAlign() (TextOut())

#if 0
   /* not implemented */
   METHOD TextOutW( wString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert )
   METHOD TextOutWAt( nPosX, nPosY, wString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert )
#endif

ENDCLASS

METHOD New( cPrinter ) CLASS WIN32PRN

   ::PrinterName := iif( ! HB_ISSTRING( cPrinter ) .OR. Empty( cPrinter ), win_printerGetDefault(), cPrinter )

   RETURN Self

METHOD Create() CLASS WIN32PRN

   IF ::PaperLength > 0 .AND. ::PaperWidth > 0
      ::FormType := WIN_DMPAPER_USER
   ENDIF

   RETURN ::win_Prn:Create()

METHOD StartPage() CLASS WIN32PRN

   IF ::PaperLength > 0 .AND. ::PaperWidth > 0
      ::FormType := WIN_DMPAPER_USER
   ENDIF

   RETURN ::win_Prn:StartPage()

METHOD TextOut( cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert ) CLASS WIN32PRN

   __defaultNIL( @nAlignHori, ::SetTextHori )
   __defaultNIL( @nAlignVert, ::SetTextVert )

   RETURN ::win_Prn:TextOut( cString, lNewLine, lUpdatePosX, ;
      hb_bitOr( nAlignHori, nAlignVert ) )

METHOD TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, nAlignHori, nAlignVert ) CLASS WIN32PRN

   __defaultNIL( @nAlignHori, ::SetTextHori )
   __defaultNIL( @nAlignVert, ::SetTextVert )

   RETURN ::win_Prn:TextOutAt( nPosX, nPosY, cString, lNewLine, lUpdatePosX, ;
      hb_bitOr( nAlignHori, nAlignVert ) )

METHOD TextAtFont( nPosX, nPosY, cString, cFont, nPointSize, ;
      nWidth, nBold, lUnderLine, lItalic, lNewLine, ;
      lUpdatePosX, nColor, nAlignHori, nAlignVert ) CLASS WIN32PRN

   __defaultNIL( @nAlignHori, ::SetTextHori )
   __defaultNIL( @nAlignVert, ::SetTextVert )

   RETURN ::win_Prn:TextAtFont( nPosX, nPosY, cString, cFont, nPointSize, ;
      nWidth, nBold, lUnderLine, lItalic, lNewLine, ;
      lUpdatePosX, nColor, ;
      hb_bitOr( nAlignHori, nAlignVert ) )

CREATE CLASS Win32Bmp INHERIT win_BMP
ENDCLASS

#endif /* __PLATFORM__WINDOWS */
