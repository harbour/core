/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Dummy functions until they are not implemented
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbsetup.ch"

/* TODO: Dummy functions, should be removed when implemented. */

#ifdef HB_COMPAT_C53
FUNCTION ordCond()         ; RETURN NIL
FUNCTION ordDescend()      ; RETURN .F.
FUNCTION ordIsUnique()     ; RETURN .F.
FUNCTION ordKeyAdd()       ; RETURN .F.
FUNCTION ordKeyCount()     ; RETURN 0
FUNCTION ordKeyDel()       ; RETURN .F.
FUNCTION ordKeyGoto()      ; RETURN .F.
FUNCTION ordKeyNo()        ; RETURN 0
FUNCTION ordKeyVal()       ; RETURN NIL
FUNCTION ordSetRelation()  ; RETURN NIL
FUNCTION ordSkipUnique()   ; RETURN .F.
#endif

#ifdef HB_COMPAT_C53
FUNCTION dbFileGet()       ; RETURN .F.
FUNCTION dbFilePut()       ; RETURN .F.
FUNCTION dbInfo()          ; RETURN NIL
FUNCTION dbOrderInfo()     ; RETURN NIL
FUNCTION dbRecordInfo()    ; RETURN NIL
FUNCTION dbFieldInfo()     ; RETURN NIL
#endif

FUNCTION dbSetRelation()   ; RETURN NIL
FUNCTION dbClearRelation() ; RETURN NIL

FUNCTION MLCToPos()        ; RETURN 0
FUNCTION MPosToLC()        ; RETURN 0

FUNCTION __dbApp()         ; RETURN NIL
FUNCTION __dbCopy()        ; RETURN NIL
FUNCTION __dbDelim()       ; RETURN NIL
FUNCTION __dbJoin()        ; RETURN NIL
FUNCTION __dbList()        ; RETURN NIL
FUNCTION __dbSDF()         ; RETURN NIL
FUNCTION __dbSort()        ; RETURN NIL
FUNCTION __dbTotal()       ; RETURN NIL
FUNCTION __dbUpdate()      ; RETURN NIL

/* NOTE: Internal functions */
FUNCTION __dbArrange()     ; RETURN NIL
FUNCTION __dbFList()       ; RETURN {}
FUNCTION __dbOpenSDF()     ; RETURN NIL
FUNCTION __dbTrans()       ; RETURN NIL
FUNCTION __dbTransRec()    ; RETURN NIL

