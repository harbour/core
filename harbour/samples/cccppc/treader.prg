/*
 * $Id$
 */

//*******************************************************************
// treader.prg: A TREADER oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "objgen.ch"

#include "lreader.och"

#define _TREADER_PRG_
#define _IMPLEMENT_ONEW_

#include "treader.och"

//*******************************************************************

implement oinit(inputReader,name,errorStream)
   super:oinit(name,errorStream)
   this:inputReader:=inputReader
return this

//*******************************************************************
implement readInput()
   if (this:inputReader==nil)
      return nil
   endif
return READER.(this:inputReader):read()

//*******************************************************************
implement unReadInput(item)
   if (this:inputReader==nil)
      return nil
   endif
return READER.(this:inputReader):unread(item)

//*******************************************************************
implement addInputReader(reader)
local r,w

   r:=this:inputReader
   
   if !(BEHAVIOR.(OBJECT.r:getClass()):isInheritFrom(C.LREADER:self()))
      w:=C.LREADER:onew(nil,nil,READER.r:name)
      LREADER.w:pushReader(r)
      r:=w
      this:inputReader:=r
   endif
   LREADER.r:pushReader(reader)
return nil

//*******************************************************************

