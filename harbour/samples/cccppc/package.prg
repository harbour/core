/*
 * $Id$
 */

#include "objgen.ch"

#define _PACKAGE_PRG_
#define _IMPLEMENT_ONEW_
#include "package.och"

*********************************************************************
implement oinit(parent,name,nAttribs,nMethods,amBlock,methodsBlock,pkgId)
local t

   this:parentPkg    :=parent
   this:name         :=name
   this:nAttribs     :=nAttribs
   this:amBlock      :=amBlock
   this:methodsBlock :=methodsBlock
   this:pkgId        :=pkgId


   t:=array(nMethods)
   t[1]:=this
   this:pkgMethodsImplement:={||t}

   this:attribs:=array(nAttribs)
   this:methods:=array(nMethods)
   evalMethodsBlocks(this)
   evalAmBlocks(this)
      
return this

*********************************************************************
implement connectTo(obj)

// Ez l‚nyeg‚ben a ocreate()
local t

   // Itt meg kellene n‚zni, erre az objektumra van-e m r
   // install lva valamilyen csomag ebb“l a f b¢l.
   // Ha van, akkor ha az install land¢ “se az install ltnak ==>
   // nem kell semmit sem csin lni, ha nem le kell cser‚lni.
   // Esetleg, ha egyik sem “se a m siknak, akkor hibajelz‚st
   // lehetne adni.

   // Ez az implement ci¢ nem j¢l m–k”dik, ha egyik sem “se a
   // m siknak, de van k”z”s “sk.

   t:=obj[2] // Itt vannak az attrib£tumok.

   if (t==nil)
      obj[2]:=t:=array(this:pkgId)
   elseif (len(t)<this:pkgId)
      asize(t,this:pkgId)
   endif

   t[this:pkgId]:=array(this:nAttribs)
   #ifdef OLD
   // Ez majd k‚s“bb.
   if (t[this:pkgId]==nil)
      t[this:pkgId]:=array(this:nAttribs)
   elseif (len(t[this:pkgId])<this:nAttribs)
      asize(t[this:pkgId],this:nAttribs)
   endif
   #endif

   t:=obj[1][2] // Itt vannak a m–veletek.

   if (t==nil)
      obj[2]:=t:=array(this:pkgId)
   elseif (len(t)<this:pkgId)
      asize(t,this:pkgId)
   endif

   t[this:pkgId]:=eval(PACKAGE.this:pkgMethodsImplement)

return this

*********************************************************************
cimplement nextId()
   if (class:numId==nil)
      class:numId:=0
   endif
   class:numId++
return class:numId

**********************************************************************
static function evalAmBlocks(aClass)
local m,c,i
// local t

   m:={}
   c:=aClass
   while(c!=nil)
      if (nil!=PACKAGE.c:amBlock)
         aadd(m,PACKAGE.c:amBlock)
      endif
      c:=PACKAGE.c:parentPkg
   end while

   for i:=len(m) to 1 step -1
      eval(m[i],PACKAGE.aClass:attribs,PACKAGE.aClass:methods)
   end for
return aClass

**********************************************************************
static function evalMethodsBlocks(aClass)
local t,m,c,i

   t:=eval(PACKAGE.aClass:pkgMethodsImplement)
   m:={}
   c:=aClass
   while(c!=nil)
      if (nil!=PACKAGE.c:methodsBlock)
         aadd(m,PACKAGE.c:methodsBlock)
      endif
      c:=PACKAGE.c:parentPkg
   end while

   for i:=len(m) to 1 step -1
      eval(m[i],t)
   end for
return aClass

*********************************************************************
