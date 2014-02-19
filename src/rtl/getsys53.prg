/*
 * Harbour Project source code:
 * GET system module (default)
 *
 * Copyright 1999-2001 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik
 *    Support for CA-Cl*pper 5.3 Getsystem
 *
 * See COPYING.txt for licensing terms.
 *
 */

#pragma -gc0

#ifdef HB_COMPAT_C53

PROCEDURE GUIReader( oGet, oGetlist, oMenu, aMsg )

   IF ! HB_ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   IF oGetList != NIL
      oGetlist:GUIReader( oGet, oMenu, aMsg )
   ENDIF

   RETURN

PROCEDURE GUIApplyKey( oGet, oGUI, oGetList, nKey, oMenu, aMsg )

   IF ! HB_ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   IF oGetList != NIL
      oGetList:GUIApplyKey( oGet, oGUI, nKey, oMenu, aMsg )
   ENDIF

   RETURN

FUNCTION GUIPreValidate( oGet, oGUI, aMsg )

   LOCAL oGetList := __GetListActive()

   RETURN iif( oGetList != NIL, oGetList:GUIPreValidate( oGet, oGUI, aMsg ), .F. )

FUNCTION GUIPostValidate( oGet, oGUI, aMsg )

   LOCAL oGetList := __GetListActive()

   RETURN iif( oGetList != NIL, oGetList:GUIPostValidate( oGet, oGUI, aMsg ), .F. )

PROCEDURE TBReader( oGet, oGetList, oMenu, aMsg )

   IF ! HB_ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   IF oGetList != NIL
      oGetlist:TBReader( oGet, oMenu, aMsg )
   ENDIF

   RETURN

PROCEDURE TBApplyKey( oGet, oTB, oGetList, nKey, aMsg )

   IF ! HB_ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   IF oGetList != NIL
      oGetList:TBApplyKey( oGet, oTB, nKey, aMsg )
   ENDIF

   RETURN

FUNCTION Accelerator( oGetList, nKey, aMsg )

   IF ! HB_ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   RETURN iif( oGetList != NIL, oGetlist:Accelerator( nKey, aMsg ), 0 )

FUNCTION HitTest( oGetList, nMRow, nMCol, aMsg )

   IF ! HB_ISOBJECT( oGetList )
      oGetList := __GetListActive()
   ENDIF

   RETURN iif( oGetList != NIL, oGetlist:hitTest( nMRow, nMCol, aMsg ), 0 )

PROCEDURE ShowGetMsg( oGet, aMsg )

   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      oGetList:ShowGetMsg( oGet, aMsg )
   ENDIF

   RETURN

PROCEDURE EraseGetMsg( oGet, aMsg )

   LOCAL oGetList := __GetListActive()

   HB_SYMBOL_UNUSED( oGet )

   IF oGetList != NIL
      oGetList:EraseGetMsg( aMsg )
   ENDIF

   RETURN

FUNCTION ReadStats( nElement, xNewValue )

   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() > 1
         RETURN oGetList:ReadStats( nElement, xNewValue )
      ELSE
         RETURN oGetList:ReadStats( nElement )
      ENDIF
   ENDIF

   RETURN NIL

#endif
