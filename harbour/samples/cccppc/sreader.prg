/*
 * $Id$
 */

//*******************************************************************
// sreader.prg: Az SREADER oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "objgen.ch"

#define _SREADER_PRG_
#define _IMPLEMENT_ONEW_

#include "sreader.och"

//*******************************************************************

implement oinit(str,name,errorStream)
   super:oinit(name,errorStream)
   this:str:=str
   this:istr:=1
return this

//*******************************************************************
implement readItem()
local c

   if (this:istr>len(this:str))
      return nil
   endif

   c:=substr(this:str,this:istr,1)
   this:istr:=this:istr+1

return c 
   
//*******************************************************************


