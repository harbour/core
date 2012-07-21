/*
 * $Id: ttag.prg 9576 2012-07-17 16:41:57Z andijahja $
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */

#include "hbclass.ch"

CLASS TTag

   VAR cIndexFile           /* Index File Name */
   VAR aTagName    INIT {}  /* Index Tag Name */
   VAR aExpression INIT {}  /* Index Expression */
   VAR iMode          /* iOption: IDX_NONE=0 IDX_UNIQUE=1 IDX_EMPTY=2  */
   VAR lDescending    /* BOOL lDescending */
   VAR cCondition     /* FOR Condition */
   VAR cAlias

   METHOD New( cIndexFile, atagName, aExpression, iMode, lDescending, cCondition, cAlias )
   METHOD CREATE( lEraseOld )
   METHOD Open()
   METHOD CLOSE()
   METHOD SetOrder( xOrder ) /* nOrder OR cOrder */
   METHOD TagCount()
   METHOD IndexOrd()

ENDCLASS

METHOD CLOSE() CLASS TTag

   RETURN sx_indexClose( ::cAlias )

METHOD IndexOrd() CLASS TTag

   RETURN SX_INDEXORD( ::cAlias )

METHOD SetOrder( xOrder ) CLASS TTag

   RETURN sx_setOrder( xOrder, ::cAlias )

METHOD Open() CLASS TTag

   RETURN Sx_IndexOpen( ::cIndexFile, ::cAlias )

METHOD TagCount() CLASS TTag

   RETURN sx_TagCount( ::cAlias )

METHOD New( cIndexFile, atagName, aExpression, iMode, lDescending, cCondition, cAlias ) CLASS TTag

   ::cIndexFile   := cIndexFile     /* Index File Name */
   ::aTagName     := aTagName       /* Index TagName */
   ::aExpression  := aExpression    /* Index Expression */
   ::iMode        := iMode          /* iOption: IDX_NONE=0 IDX_UNIQUE=1 IDX_EMPTY=2  */
   ::lDescending  := lDescending    /* BOOL lDescending */
   ::cCondition   := cCondition     /* FOR Condition */
   ::cAlias       := cAlias         /* Alias */

   RETURN Self

METHOD CREATE( lEraseOld ) CLASS TTag

   // This is For MULTI TAG Index File : DBFNSX and DBFCDX
   // DBFNTX Should Use TIndex
   LOCAL i, ul, nResult

   IF lEraseOld == NIL
      lEraseOld := .T.
   ENDIF

   IF lEraseOld .AND. File( ::cIndexFile )
      FErase( ::cIndexFile )
   ENDIF

   IF ! Empty( ::aTagName ) .AND. ! Empty( ::aExpression ) .AND. ;
         ( ul := Len( ::aTagName ) ) == Len( ::aExpression )

      FOR i := 1 TO ul
         nResult := sx_IndexTag( ::cIndexFile, ::aTagName[ i ], ::aExpression[ i ], ::iMode, ::lDescending, ::cCondition, ::cAlias )
      NEXT

      RETURN nResult

   ENDIF

   RETURN 0
