/*
 * $Id$
 */

//*******************************************************************
// parser.prg: A PARSER oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

//*******************************************************************
static BUF_EOF:={"buf_eof"}

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
// #include "ctoken.ch"

//*******************************************************************
#include "tbuffer.och"
// #include "token.och"

//*******************************************************************
#define _PARSER_PRG_
#define _IMPLEMENT_ONEW_

#include "parser.och"

//*******************************************************************
implement oinit(inputReader,name,errorStream)
           
   super:oinit(inputReader,name,errorStream)
   
   this:item:=nil
   this:parserBuffer:=C.TBUFFER:onew()
   
return this
   
//*******************************************************************
implement putParserBuffer(anItem)
// Hozz†ad egy elemet a parserBufferhez. Az elem lehet speci†lis is.
   TBUFFER.(this:parserBuffer):put(anItem)
return nil

//*******************************************************************
implement getParserBuffer()
// Kivesz egy elemet a puffer elejÇrìl.
// Az eof-ot eldobja.
// return TBUFFER.(this:parserBuffer):unget()

local w

   w:=TBUFFER.(this:parserBuffer):get()
   while(valtype(w)=="A" .and. w==BUF_EOF)
      w:=TBUFFER.(this:parserBuffer):get()
   end while
return w

//*******************************************************************
implement unputParserBuffer()
// Kivesz egy elemet a puffer vÇgÇrìl.
// return TBUFFER.(this:parserBuffer):unput()
local w

   w:=TBUFFER.(this:parserBuffer):unput()
   while(valtype(w)=="A" .and. w==BUF_EOF)
      w:=TBUFFER.(this:parserBuffer):unput()
   end while
return w

//*******************************************************************
implement ungetParserBuffer(anItem)
// Betesz egy elemet a puffer elejÇre.
return TBUFFER.(this:parserBuffer):unget(anItem)

//*******************************************************************
implement clearParserBuffer()
return TBUFFER.(this:parserBuffer):clear()

//*******************************************************************
implement strParserBuffer()
// Megadja a parserBuffer tartalm†t stringkÇnt.
// A nem stringeket eldobja.
local i,str:="",w

   for i:=1 to TBUFFER.(this:parserBuffer):bItemNumber
      w:=TBUFFER.(this:parserBuffer):getBItem(i)
      if (valtype(w)=="C")
         str+=w
      endif
   end for
return str

//*******************************************************************
implement arrayParserBuffer()
local i,r:={}

   for i:=1 to TBUFFER.(this:parserBuffer):bItemNumber
      aadd(r,TBUFFER.(this:parserBuffer):getBItem(i))
   end for
return r


//*******************************************************************
implement readItem()
   
   TBUFFER.(this:parserBuffer):clear()
   this:rds()
   
return this:item

//*******************************************************************
implement rds()
// Olvas egy karatert a readInput()-al a parserBuffer-be.
// Az olvasott ÇrtÇket a parserBuffer-be is beleteszi, ha az nem az eof.
// Ha eof, akkor egy spec eof itemet tesz.

   if (nil==(this:item:=this:readInput()))
      this:putParserBuffer(BUF_EOF)
   else
      this:putParserBuffer(this:item)
   endif

return this:item

//*******************************************************************
implement unrds(n)
// A buf utols¢ n elemÇt visszateszi az inputra. Az item-et tîrli.
// Azt adja vissza, hogy h†ny elemet sikerÅlt visszatenni.
// Ha az n nil, akkor egyet tesz vissza.
// Az eof-okat kihagyja.
local w,i

   this:item:=nil
   if (n==nil)
      n:=1
   endif
   i:=0
   while(i<n)
      if (nil==(w:=TBUFFER.(this:parserBuffer):unput()))
         return nil
      endif
      if !(valtype(w)=="A" .and. w==BUF_EOF)
         this:unReadInput(w)
      endif
      i++
   end while
return i
   
//*******************************************************************



