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

/* TODO: Dummy functions, should be removed when implemented. */

FUNCTION ordCond()         ; RETURN NIL /* 5.3 */
FUNCTION ordDescend()      ; RETURN .F. /* 5.3 */
FUNCTION ordIsUnique()     ; RETURN .F. /* 5.3 */
FUNCTION ordKeyAdd()       ; RETURN .F. /* 5.3 */
FUNCTION ordKeyCount()     ; RETURN 0   /* 5.3 */
FUNCTION ordKeyDel()       ; RETURN .F. /* 5.3 */
FUNCTION ordKeyGoto()      ; RETURN .F. /* 5.3 */
FUNCTION ordKeyNo()        ; RETURN 0   /* 5.3 */
FUNCTION ordKeyVal()       ; RETURN NIL /* 5.3 */
FUNCTION ordScope()        ; RETURN NIL /* 5.3 */
FUNCTION ordSetRelation()  ; RETURN NIL /* 5.3 */
FUNCTION ordSkipUnique()   ; RETURN .F. /* 5.3 */

FUNCTION dbFileGet()       ; RETURN .F. /* 5.3 */
FUNCTION dbFilePut()       ; RETURN .F. /* 5.3 */
FUNCTION dbInfo()          ; RETURN NIL /* 5.3 */
FUNCTION dbOrderInfo()     ; RETURN NIL /* 5.3 */
FUNCTION dbRecordInfo()    ; RETURN NIL /* 5.3 */
FUNCTION dbFieldInfo()     ; RETURN NIL /* 5.3 */

FUNCTION dbSetRelation()   ; RETURN NIL
FUNCTION dbClearRelation() ; RETURN NIL

FUNCTION MemoEdit( str )   ; RETURN str
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



