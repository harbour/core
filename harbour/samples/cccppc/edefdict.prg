/*
 * $Id$
 */

//*******************************************************************
// edefdict.prg: #define sz¢t r elem
// 1999, Csisz r Levente

//*******************************************************************
// A helyettes¡t‚s sor n el kell fogadni a #define h(x,x) st¡lus£
// definici¢t, ‚s ilyenkor az els“ 'x' hely‚n  ll¢ ‚rt‚k 
// helyettes¡t“dik be mindenhova.

//*******************************************************************
#include "ctoken.ch"
#include "objgen.ch"

//*******************************************************************
#include "token.och"

//*******************************************************************

#define _EDEFDICT_PRG_
#define _IMPLEMENT_ONEW_

#include "edefdict.och"


//*******************************************************************

implement oinit(name,params,body,deffile,defline,defpos)
local w,i,j,p,pw,l

   super:oinit()
   this:name    :=name
   this:params  :=params
   this:deffilelinepos :={deffile,defline,defpos}

   if (empty(body))
      this:body:={}
   else
      l:=0
      for i:=len(body) to 1 step -1
         if (!TOKEN.body[i]:id==TKID_URES)
            l:=i
            exit
         endif
      end for
      if (empty(params))
         pw:={}
         // aeval(body,{|x| aadd(pw,x)})
         for i:=1 to l
            aadd(pw,body[i])
         end for
         this:body:={pw}
      else
         p:={}
         pw:={}
         // for i:=1 to len(body)
         for i:=1 to l
            w:=body[i]
            if (TOKEN.w:id==TKID_NEV .and.;
                0!=(j:=ascan(params,{|x| TOKEN.w:str==x})))
               
               if (!empty(pw))
                  aadd(p,pw)
               endif
               aadd(p,j)
               pw:={}
            else
               aadd(pw,w)
            endif
         end for
         if (!empty(pw))
            aadd(p,pw)
         endif
         this:body:=p
      endif
   endif
return this   

//*******************************************************************
implement change(paramValues)
local i,pi
local r:={}

   if (empty(this:body))
      return this:body
   elseif (empty(this:params))
      return this:body[1]
   endif
   
   for i:=1 to len(this:body)
      if (valtype(this:body[i])=="N")
         pi:=this:body[i]
         if (len(paramValues)>=pi)
            aeval(paramValues[pi],{|x| aadd(r,x)})
         endif
      else
         aeval(this:body[i],{|x| aadd(r,x)})
      endif
   end for
return r

//*******************************************************************
implement printStr()
local str:="#define "+"'"+this:name+"'"
local w
local wParams:={}
local i
 
     
   if (this:params!=nil)
      str+="'("
      for i:=1 to len(this:params)
         aadd(wParams,{this:params[i]})
         if (i>1)
            str+=","
         endif
         str+=this:params[i]
      end for
      str+=")'"
   endif
   w:=this:change(wParams)
      
   if (len(w)>0)
      str+=" "
   endif
   
   for i:=1 to len(w)
      if (valtype(w[i])=="C")
         str+=w[i]
      else
         str+=TOKEN.(w[i]):getStr()
      endif
   end for

return str   

//*******************************************************************
   



