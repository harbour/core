/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                   Xbase++ xbpStatic compatible Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               29Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpStatic  INHERIT  XbpWindow

   DATA     autoSize                              INIT .F.
   DATA     caption                               INIT ""
   DATA     clipParent                            INIT .T.
   DATA     clipSiblings                          INIT .F.
   DATA     options                               INIT 0
   DATA     type                                  INIT -1

   DATA     hBitmap

   METHOD   new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )
   METHOD   configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   destroy()
   METHOD   handleEvent( nEvent, mp1, mp2 )

   METHOD   setCaption( xCaption, cDll )

   ENDCLASS
/*----------------------------------------------------------------------*/

METHOD XbpStatic:new( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatic:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   LOCAL lThick := hb_bitAnd( ::options, XBPSTATIC_FRAMETHICK ) == XBPSTATIC_FRAMETHICK

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   DO CASE
   // OK
   CASE ::type == XBPSTATIC_TYPE_TEXT
      ::oWidget := QLabel( ::pParent )

      IF ( hb_bitAnd( ::options, XBPSTATIC_TEXT_LEFT ) == XBPSTATIC_TEXT_LEFT )
         ::oWidget:setAlignment( Qt_AlignLeft )
      ENDIF
      IF ( hb_bitAnd( ::options, XBPSTATIC_TEXT_RIGHT ) == XBPSTATIC_TEXT_RIGHT )
         ::oWidget:setAlignment( Qt_AlignRight )
      ENDIF
      IF ( hb_bitAnd( ::options, XBPSTATIC_TEXT_CENTER ) == XBPSTATIC_TEXT_CENTER )
         ::oWidget:setAlignment( Qt_AlignCenter )
      ENDIF
      IF ( hb_bitAnd( ::options, XBPSTATIC_TEXT_TOP ) == XBPSTATIC_TEXT_TOP )
         ::oWidget:setAlignment( hb_bitOr( ::oWidget:alignment, Qt_AlignTop ) )
      ENDIF
      IF ( hb_bitAnd( ::options, XBPSTATIC_TEXT_VCENTER ) == XBPSTATIC_TEXT_VCENTER )
         ::oWidget:setAlignment( hb_bitOr( ::oWidget:alignment, Qt_AlignVCenter ) )
      ENDIF
      IF ( hb_bitAnd( ::options, XBPSTATIC_TEXT_BOTTOM ) == XBPSTATIC_TEXT_BOTTOM )
         ::oWidget:setAlignment( hb_bitOr( ::oWidget:alignment, Qt_AlignBottom ) )
      ENDIF
      IF ( hb_bitAnd( ::options, XBPSTATIC_TEXT_WORDBREAK ) == XBPSTATIC_TEXT_WORDBREAK )
         ::oWidget:setWordWrap( .T. )
      ENDIF
   // OK
   CASE ::type == XBPSTATIC_TYPE_GROUPBOX
      ::oWidget := QGroupBox( ::pParent )

   // OK
   CASE ::type == XBPSTATIC_TYPE_RAISEDBOX
      ::oWidget := QFrame( ::pParent )
      ::oWidget:setFrameStyle( QFrame_Panel + QFrame_Raised )
      ::setColorBG( GraMakeRGBColor( { 198, 198, 198 } ) )
      IF lThick
         ::oWidget:setLineWidth( 2 )
      ENDIF
   // OK
   CASE ::type == XBPSTATIC_TYPE_RECESSEDBOX
      ::oWidget := QFrame( ::pParent )
      ::oWidget:setFrameStyle( QFrame_Panel + QFrame_Sunken )
      ::setColorBG( GraMakeRGBColor( { 198, 198, 198 } ) )
      IF lThick
         ::oWidget:setLineWidth( 2 )
      ENDIF
   // OK
   CASE ::type == XBPSTATIC_TYPE_RAISEDRECT
      ::oWidget := QFrame( ::pParent )
      ::oWidget:setFrameStyle( QFrame_Panel + QFrame_Raised )
      IF lThick
         ::oWidget:setLineWidth( 2 )
      ENDIF
   // OK
   CASE ::type == XBPSTATIC_TYPE_RECESSEDRECT
      ::oWidget := QFrame( ::pParent )
      ::oWidget:setFrameStyle( QFrame_Panel + QFrame_Sunken )
      IF lThick
         ::oWidget:setLineWidth( 2 )
      ENDIF
   // OK
   CASE ::type == XBPSTATIC_TYPE_FGNDFRAME     // rectangle in foreground color, not filled
      ::oWidget := QFrame( ::pParent )
      ::oWidget:setFrameStyle( QFrame_Panel + QFrame_Plain )
      ::setColorFG( GraMakeRGBColor( { 0, 0, 0 } ) )
   // OK
   CASE ::type == XBPSTATIC_TYPE_BGNDFRAME
      ::oWidget := QFrame( ::pParent )
      ::oWidget:setFrameStyle( QFrame_Box + QFrame_Plain )
      ::setColorFG( GraMakeRGBColor( { 127, 127, 127 } ) )
   // OK
   CASE ::type == XBPSTATIC_TYPE_FGNDRECT
      ::oWidget := QFrame( ::pParent )
      ::setColorBG( GraMakeRGBColor( { 0, 0, 0 } ) )
   // OK
   CASE ::type == XBPSTATIC_TYPE_BGNDRECT
      ::oWidget := QFrame( ::pParent )
      ::setColorBG( GraMakeRGBColor( { 127, 127, 127 } ) )
   // OK
   CASE ::type == XBPSTATIC_TYPE_HALFTONERECT
      ::oWidget := QFrame( ::pParent )
      ::setColorBG( GraMakeRGBColor( { 255, 255, 255 } ) )
   // OK
   CASE ::type == XBPSTATIC_TYPE_HALFTONEFRAME
      ::oWidget := QFrame( ::pParent )
      ::oWidget:setFrameStyle( QFrame_Box + QFrame_Plain )
      ::setColorFG( GraMakeRGBColor( { 255, 255, 255 } ) )
   // OK
   CASE ::type == XBPSTATIC_TYPE_RAISEDLINE
      ::oWidget := QFrame( ::pParent )
      //IF ::aPos[ 1 ] + ::aSize[ 1 ] >= ::aPos[ 2 ] + ::aSize[ 2 ]
      IF ::aSize[ 1 ] >= ::aSize[ 2 ]
         ::oWidget:setFrameStyle( QFrame_HLine + QFrame_Raised )
      ELSE
         ::oWidget:setFrameStyle( QFrame_VLine + QFrame_Raised )
      ENDIF
      IF lThick
         ::oWidget:setMidLineWidth( 1 )
      ENDIF
   // OK
   CASE ::type == XBPSTATIC_TYPE_RECESSEDLINE
      ::oWidget := QFrame( ::pParent )
      //IF ::aPos[ 1 ] + ::aSize[ 1 ] >= ::aPos[ 2 ] + ::aSize[ 2 ]
      IF ::aSize[ 1 ] >= ::aSize[ 2 ]
         ::oWidget:setFrameStyle( QFrame_HLine + QFrame_Sunken )
      ELSE
         ::oWidget:setFrameStyle( QFrame_VLine + QFrame_Sunken )
      ENDIF
      IF lThick
         ::oWidget:setMidLineWidth( 1 )
      ENDIF

   CASE ::type == XBPSTATIC_TYPE_ICON
      ::oWidget := QLabel( ::pParent )

   CASE ::type == XBPSTATIC_TYPE_SYSICON
      ::oWidget := QLabel( ::pParent )

   CASE ::type == XBPSTATIC_TYPE_BITMAP
      //::oWidget := QFrame( ::pParent )
      ::oWidget := QLabel( ::pParent )

   OTHERWISE
      ::oWidget := QFrame( ::pParent )

   ENDCASE

   ::setCaption( ::caption )

   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:addChild( SELF )
   ::postCreate()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatic:hbCreateFromQtPtr( oParent, oOwner, aPos, aSize, aPresParams, lVisible, pQtObject )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   IF hb_isPointer( pQtObject )
      DO CASE
      CASE ::type == XBPSTATIC_TYPE_TEXT
         ::oWidget := QLabel():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_GROUPBOX
         ::oWidget := QGroupBox():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_RAISEDBOX
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_RECESSEDBOX
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_RAISEDRECT
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_RECESSEDRECT
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_FGNDFRAME     // rectangle in foreground color, not filled
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_BGNDFRAME
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_FGNDRECT
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_BGNDRECT
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_HALFTONERECT
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_HALFTONEFRAME
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_RAISEDLINE
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_RECESSEDLINE
         ::oWidget := QFrame():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_ICON
         ::oWidget := QLabel():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_SYSICON
         ::oWidget := QLabel():from( pQtObject )

      CASE ::type == XBPSTATIC_TYPE_BITMAP
         ::oWidget := QLabel():from( pQtObject )

      OTHERWISE
         ::oWidget := QFrame():from( pQtObject )

      ENDCASE
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatic:handleEvent( nEvent, mp1, mp2 )

   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( mp1    )
   HB_SYMBOL_UNUSED( mp2    )

   RETURN HBXBP_EVENT_UNHANDLED

/*----------------------------------------------------------------------*/

METHOD XbpStatic:destroy()

   ::xbpWindow:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpStatic:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::Initialize( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpStatic:setCaption( xCaption, cDll )
   LOCAL oStyle, oIcon, oPixmap

   HB_SYMBOL_UNUSED( cDll )

   DEFAULT xCaption TO ::caption
   ::caption := xCaption

   IF !empty( ::caption )
      DO CASE
      CASE ::type == XBPSTATIC_TYPE_GROUPBOX
         ::oWidget:setTitle( ::caption )

      CASE ::type == XBPSTATIC_TYPE_TEXT
         ::oWidget:setText( ::caption )

      CASE ::type == XBPSTATIC_TYPE_BITMAP
         IF hb_isObject( ::caption )                 /* XbpBitmap() */
            IF ::options == XBPSTATIC_BITMAP_SCALED
               ::oWidget:setScaledContents( .t. )
            ENDIF
            ::oWidget:setPixmap( QPixmap():fromImage( ::caption:oWidget ) )

         ELSEIF hb_isChar( ::caption )               /* $HARBOUR$ */
            IF ::options == XBPSTATIC_BITMAP_SCALED
               ::oWidget:setStyleSheet( 'border-image: url('+ ::caption +');' )
            ELSE
               ::oWidget:setStyleSheet( 'background: url('+ ::caption +'); repeat-xy;' )
            ENDIF
         ENDIF

      CASE ::type == XBPSTATIC_TYPE_ICON
         ::oWidget:setPixmap( QPixmap( ::caption ):scaled( ::aSize[ 1 ], ::aSize[ 2 ] ) )

      CASE ::type == XBPSTATIC_TYPE_SYSICON
         oStyle      := QApplication():style()

         DO CASE
         CASE ::caption == XBPSTATIC_SYSICON_ICONINFORMATION
            oIcon := oStyle:standardIcon( QStyle_SP_MessageBoxInformation )

         CASE ::caption == XBPSTATIC_SYSICON_ICONQUESTION
            oIcon := oStyle:standardIcon( QStyle_SP_MessageBoxQuestion )

         CASE ::caption == XBPSTATIC_SYSICON_ICONERROR
            oIcon := oStyle:standardIcon( QStyle_SP_MessageBoxCritical )

         CASE ::caption == XBPSTATIC_SYSICON_ICONWARNING
            oIcon := oStyle:standardIcon( QStyle_SP_MessageBoxWarning )

         OTHERWISE
            /* It is a Harbour Extension - you have 60+ icons to display
             * Check hbqtgui.ch : #define QStyle_SP_* constants
             */
            oIcon := oStyle:standardIcon( ::caption )

         ENDCASE

         /* Harbour can also implement if icon be displayed scaled or proportionate
          */
         oPixmap := oIcon:pixmap( ::aSize[ 1 ], ::aSize[ 2 ] ):scaled( ::aSize[ 1 ], ::aSize[ 2 ] )

         ::oWidget:setPixmap( oPixmap )
      ENDCASE
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
