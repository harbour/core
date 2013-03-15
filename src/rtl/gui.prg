/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GUI helper functions
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#ifdef HB_COMPAT_C53

#define LLG_VIDEO_TXT   3

FUNCTION _IsGraphic()
   RETURN ;
      Set( _SET_VIDEOMODE ) != NIL .AND. ;
      Set( _SET_VIDEOMODE ) != 0 .AND. ;
      Set( _SET_VIDEOMODE ) != LLG_VIDEO_TXT

FUNCTION _SetVideoMode( nMode )

   HB_SYMBOL_UNUSED( nMode )

   RETURN 0

/* NOTE: Original _GetNumCol() will not recognize colors written
         in lowercase. We're replicating this behaviour in this
         compatibility function. [vszakats] */

FUNCTION _GetNumCol( cColor )

   LOCAL nPos

   IF ( nPos := At( "/", cColor ) ) > 0
      cColor := Left( cColor, nPos - 1 )
   ENDIF
   IF ( nPos := At( ",", cColor ) ) > 0
      cColor := Left( cColor, nPos - 1 )
   ENDIF

   RETURN AScan( { "B", "G", "BG", "R", "RB", "GR", "W", "N+", "B+", "G+", "BG+", "R+", "RB+", "GR+", "W+" }, {| tmp | tmp == cColor } )

FUNCTION __GUIColor( cColor, nPos )
   RETURN hb_ColorIndex( cColor, nPos - 1 )

FUNCTION IsDefColor()
   RETURN SetColor() == "W/N,N/W,N/N,N/N,N/W" /* NOTE: Color must match with the one in set.c */

/* Removes the accelerator marker from a caption string */
FUNCTION __Caption( cCaption )

   LOCAL nPos

   RETURN iif( ( nPos := At( "&", cCaption ) ) > 0, Stuff( cCaption, nPos, 1, "" ), cCaption )

FUNCTION __CapLength( cCaption )

   LOCAL nCaptionLen := Len( cCaption )
   LOCAL nPos

   RETURN iif( ( nPos := At( "&", cCaption ) ) > 0 .AND. nPos < nCaptionLen, nCaptionLen - 1, nCaptionLen )

FUNCTION __CapMetrics( o )
   RETURN __CapLength( o:caption ) + iif( o:isPopup(), 3, 2 )

#endif
