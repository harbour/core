/*
 * $Id$
 */

//*******************************************************************
// tokenst.prg: a TOKENST oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "objgen.ch"

#define _TOKENST_PRG_
#define _IMPLEMENT_ONEW_

#include "tokenst.och"
//*******************************************************************
   

//*******************************************************************
implement oinit(id,str,file,line,pos,deep)

   super:oinit(id,str,file,line,pos)
   this:deep:=deep
return this

//*******************************************************************

