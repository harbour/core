/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DBLIST(), __DBUPDATE() functions
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#include "common.ch"
#include "set.ch"

FUNCTION __dbList(lOff,aList,lAll,bFor,bWhile,nNext,nRec,lRest,lPrint,cFile)
Local bBlock,lPrinter,lExtra,cExtraFile,oError

IF lOff
    bBlock:={|| (Qout(if(Deleted(), "*", " ")), aEval(aList, ;
        {|cItem| qqout(eval(cItem),"")}))}
ELSE    
    bBlock:={|| (Qout(STR(Recno(),7), if(Deleted(), "*", " ")), aEval(aList, ;
        {|cItem| qqout(eval(cItem),"")}))}
ENDIF

IF (!EMPTY(lPrint))
    lPrinter := SET(_SET_PRINTER,.T.)
ENDIF

IF (!EMPTY(cFile))
    IF EMPTY(AT(".",cFile))
        cFile += ".txt"
    ENDIF
    lExtra := SET(_SET_EXTRA, .T.)
    cExtraFile := SET(_SET_EXTRAFILE,cFile)
ENDIF
BEGIN SEQUENCE
IF (EMPTY(lAll) .and. EMPTY(bFor) .and. EMPTY(bWhile) .and. EMPTY(nNext) .and. ;
    EMPTY(nRec) .and. EMPTY(lRest))
    EVAL(bBlock)
ELSE
    DBEVAL(bBlock,bFor,bWhile,nNext,nRec,lRest)
ENDIF
RECOVER Using oError
END SEQUENCE
IF (!EMPTY(lPrint))
    set printer to (lPrinter)
ENDIF

IF (!EMPTY(cFile))
     SET(_SET_EXTRA,lExtra)
     SET(_SET_EXTRAFILE,cExtraFile)
ENDIF
IF oError != NIL
    Break(oError)
Endif

RETURN NIL

FUNCTION __dbUpdate(cAlias,bKey,lRand,bFields)

    Local CurArea,oError,bBlock
    Default lRand to .F.
    DBGOTOP()
    CurArea:=Select()
    BEGIN SEQUENCE
        DBSELECTAREA(cAlias)
        DBGOTOP()
        While !EOF()
            bBlock:=EVAL(bKey)
            DBSELECTAREA(CurArea)
            IF lRand
                dbSeek(bBlock, if(.F. ,.T.,NIL))
                IF Found()
                    Eval(bFields)
                Endif
            ELSE
                DO WHILE(Eval(bKey) < bBlock .AND. !EOF())
                    dbSkip()
                ENDDO
                IF (Eval(bKey) == bBlock .AND. !EOF())
                    Eval(bFields)
                ENDIF
            ENDIF
            dbSelectArea(cAlias)
            dbSkip()
         ENDDO
    RECOVER USING oError
    END SEQUENCE
    dbSelectArea(CurArea)
    IF oError != NIL
        Break(oError)
    ENDIF
Return .T.
