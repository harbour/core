/*
 * $Id$
 */

//*******************************************************************
// prserr.prg: A PRSERR oszt ly implement ci¢ja.
// 1999, Csisz r Levente

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "token.och"

//*******************************************************************
#define _PRSERR_PRG_
#define _IMPLEMENT_ONEW_

#include "prserr.och"

//*******************************************************************
implement oinit(group_and_id,params,file,line,pos)
           
   super:oinit()
   
   this:group :=group_and_id[1]
   this:id    :=group_and_id[2]
   this:params:=params
   this:file  :=file  
   this:line  :=line  
   this:pos   :=pos   
   
return this
   

//*******************************************************************
implement defaultErrFormat(name)
// Egyenl“re a a $0,$1,...,$9-et cser‚li ki.
// $0 az ”sszes param‚tert jel”li, vessz“vel elv lasztva.
// Sajnos az strtran itt nem j¢, mert az az eredm‚nyben is cser‚lne.
local str,i,c,allParam,wStr
           
   if ("$0"$name)
      allParam:=""
      if (!empty(this:params))
         for i:=1 to len(this:params)
            if (i>1)
               allParam+=","
            endif
            allParam+=toStr(this:params[i])
         end for
      endif
   endif

   str:=""   
   while(0!=(i:=at("$",name)))
      str+=substr(name,1,i-1)
      c:=substr(name,i+1,1)
      if (!empty(c) .and. c$"0123456789")
         if (val(c)==0)
            str+=allParam
         elseif (val(c)<=len(this:params))
            str+=toStr(this:params[val(c)])
         else
            str+=substr(name,i,2)
         endif
         name:=substr(name,i+2)
      else
         str+=substr(name,i,1)
         name:=substr(name,i+1)
      endif
   end for
   str+=name
   wStr:=this:file+"("+toStr(this:line)+")"
   if (len(wStr)<20)
      wStr:=padr(wStr,20)
   endif
return wStr+" Error: "+str
// return padr(this:file+"("+toStr(this:line)+")",20)+" Error: "+name

//*******************************************************************
implement getErrStr()
local i, wIdt

   if (0!=(i:=ascan(thisclass:errDict,{|x| x[1]==this:group})))
      wIdt:=thisclass:errDict[i][2]
      if (0!=(i:=ascan(wIdt,{|x| x[1]==this:id})))
         if (wIdt[i][3]==nil)
            return this:defaultErrFormat(wIdt[i][2])
         endif
         return eval(wIdt[i][3],this,wIdt[i][2])
      endif
   endif
return this:defaultErrFormat("Unknown error $0")

//*******************************************************************
cimplement oinitclass()
   superclass:oinitclass()
   class:errDict:={}
return nil

//*******************************************************************
cimplement onewFromToken(group_and_id,params,token)
return class:onew(group_and_id,params,;
                  TOKEN.token:file,;
                  TOKEN.token:line,;
                  TOKEN.token:pos)

//*******************************************************************
cimplement registerError(group_and_id,name,printBlock)
local i, wIdt

   if (0==(i:=ascan(class:errDict,{|x| x[1]==group_and_id[1]})))
      aadd(class:errDict,{group_and_id[1],{}})
      i:=len(class:errDict)
   endif
   
   wIdt:=class:errDict[i][2]
   if (0==(i:=ascan(wIdt,{|x| x[1]==group_and_id[2]})))
      aadd(wIdt,{group_and_id[2],name,printBlock})
      return group_and_id
   endif
return nil

//*******************************************************************
function evalErrorStream(errorStream,bBlock,nStart,nCount)
   if (!empty(errorStream))
      return aeval(errorStream,;
                   {|x| eval(bBlock,PRSERR.x:getErrStr())},;
                   nStart,nCount)
   endif
return nil

//*******************************************************************


