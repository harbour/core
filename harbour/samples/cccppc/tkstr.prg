/*
 * $Id$
 */

//*******************************************************************
// tkstr.prg: a TKSTR oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

#define _TKSTR_PRG_
#define _IMPLEMENT_ONEW_

#include "tkstr.och"
//*******************************************************************
   

//*******************************************************************
implement oinit(id,str,file,line,pos,kezdo,zaro)

   super:oinit(id,str,file,line,pos)
   this:kezdo:=kezdo
   this:zaro :=zaro
return this

//*******************************************************************
implement getStr()
local str,i
    
   if (!this:id==TKID_STRING)
      return super:getStr()
   endif
   // Incompatibility
   // if (empty(this:zaro))
   //    return this:kezdo+this:str+this:kezdo
   // else
   //    return this:kezdo+this:str+this:zaro
   // endif
   
return stringifyStr(this:str)


//*******************************************************************
cimplement copyFromToken(t,id,str,kezdo,zaro)
return class:onew(id,; 
                  if(str==nil,TOKEN.t:str,str),;
                  TOKEN.t:file,TOKEN.t:line,TOKEN.t:pos,kezdo,zaro)
   

//*******************************************************************

