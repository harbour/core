/*
 * $Id$
 */

**********************************************************************
// Ezek az a priori oszt†lyok. Amikor objektumhoz ny£lunk, akkor
// ezek automatikusan feltîltìdnek.
static Object
static Behavior
static Meta
static MetaClass
**********************************************************************

#include "error.ch"

#include "objgen.ch"

#include "object.och"
#include "behavior.och"
#include "meta.och"
#include "metaclas.och"

#define _OBJECT_PRG_
#define setCMethods(pOsztaly) ciOBJECT(pOsztaly)
#define setCams(ta,tm)        amOBJECT(ta,tm)
#define _NO_BEHAVIOR_
#define _NO_CLOBJECT_
#include "object.och"
#undef _OBJECT_PRG_
#undef setCMethods
#undef setCams
#undef _NO_CLOBJECT_
#undef _NO_BEHAVIOR_
#undef CTHIS

#define _BEHAVIOR_PRG_
#define setCMethods(pOsztaly) ciBEHAVIOR(pOsztaly)
#define setCams(ta,tm)        amBEHAVIOR(ta,tm)
#define _NO_META_
#define _NO_CLBEHAVIOR_
#include "behavior.och"
#undef _BEHAVIOR_PRG_
#undef setCMethods
#undef setCams
#undef _NO_CLBEHAVIOR_
#undef _NO_META_
#undef CTHIS


#define _META_PRG_
#define setCMethods(pOsztaly) ciMETA(pOsztaly)
#define setCams(ta,tm)        amMETA(ta,tm)
#define _NO_METACLAS_
#define _NO_CLMETA_
#include "meta.och"
#undef _META_PRG_
#undef setCMethods
#undef setCams
#undef _NO_CLMETA_
#undef _NO_METACLAS_
#undef CTHIS


#include "metaclas.och"

// Kîtelezìen ugyanaz.
// #define ciMETACLAS(pOsztaly) ciMETA(pOsztaly)

**********************************************************************
*                                                                    *
*                        Az OBJECT oszt†ly                           * 
*                                                                    *
**********************************************************************

**********************************************************************
function clOBJECT()
   if (Object==nil)
      primitiveAllAPClassCreate()
   endif
return Object

**********************************************************************
// function clOBJECTCLASS()
// Az Object oszt†ly meta oszt†ly†t adja.
// Ez m†r megvan, a Behavior az.
// return clBEHAVIOR()

**********************************************************************
static function ob_oinit(this)
// Ez egy virtu†lis mñvelet, az implement†ci¢ja Åres.
return this

**********************************************************************
static function ob_getclass(this)
return eval(this[1][1])

**********************************************************************
static function ob_isClass(this)
return .f.

**********************************************************************
static function ob_getAttrib(this,attribId)
   if (valtype(attribId)=="C")
      attribId:=BEHAVIOR.(OBJECT.this:getClass()):attribIdx(attribId)
      if (attribId==nil)
         errAttrib("getAttrib",{BEHAVIOR.(OBJECT.this:getClass()):name,attribId})
      endif
   endif
return this[attribId]

**********************************************************************
static function ob_setAttrib(this,attribId,value)
   if (valtype(attribId)=="C")
      attribId:=BEHAVIOR.(OBJECT.this:getClass()):attribIdx(attribId)
      if (attribId==nil)
         errAttrib("getAttrib",{BEHAVIOR.(OBJECT.this:getClass()):name,attribId})
      endif
   endif
return this[attribId]:=value


**********************************************************************


**********************************************************************
*                                                                    *
*                        A BEHAVIOR oszt†ly                          * 
*                                                                    *
**********************************************************************

**********************************************************************
function clBEHAVIOR()
// Az oszt†ly elìszedÇse. (class)
   if (Behavior==nil)
      primitiveAllAPClassCreate()
   endif
return Behavior

**********************************************************************
// function clBEHAVIORCLASS()
// A Behavior oszt†ly meta oszt†ly†t adja.
// Ez m†r megvan, a Meta az.
// return clMETA()

**********************************************************************
static function be_onew(this)
// KÇsz°t egy objektumot, Çs inicializ†lja.
// Az objektum a 'this' oszt†lyba fog tartozni.

local obj

   obj:=BEHAVIOR.this:ocreate()
   OBJECT.obj:oinit()
return obj

**********************************************************************
static function be_ocreate(this)
// KÇsz°t egy objektumot, de nem inicializ†lja.
// Az objektum a 'this' oszt†lyba fog tartozni.

local obj
   obj:=primitiveCreateObj(this)
return obj

**********************************************************************
static function be_rawoinit(this,parent,name,nAttribs,nMethods,amBlock,methodsBlock)
// Inicializ†lja a 'this' oszt†lyt.

local w

   ob_oinit(this)
   primitiveInitClass(this,parent,name,nAttribs,nMethods,amBlock,methodsBlock)
   if (parent!=nil)
      if (BEHAVIOR.this:classId==nil)
         BEHAVIOR.this:parentClassIds:=BEHAVIOR.parent:parentClassIds
      else
         if (nil!=(w:=BEHAVIOR.parent:parentClassIds))
            BEHAVIOR.this:parentClassIds:=w:=aclone(w)
         else
            BEHAVIOR.this:parentClassIds:=w:={}
         endif
         if (len(w)<BEHAVIOR.this:classId)
            asize(w,BEHAVIOR.this:classId)
         endif
         w[BEHAVIOR.this:classId]:=BEHAVIOR.this:classId
      endif
   endif
return this

**********************************************************************
static function be_oinit(this,parent,name,nAttribs,nMethods,amBlock,methodsBlock)
// Inicializ†lja a 'this' oszt†lyt.
// Ellenìrzi, hogy az oszt†ly metaoszt†ly†nak van-e m†r eleme.
// Ezt az ellenìrzÇst a metaoszt†lyokn†l ki kell kapcsolni.

local classOf

   classOf:=this:getClass()

   if (classOf!=nil .and. META.classOf:ofClass!=nil)
      alert("Behavior:oinit: '"+name+"';"+;
            "Ennek a metaoszt†lynak m†r van lÇtrehozva eleme:;"+;
            "'"+META.classOf:name+"'")
      META.classOf:ofClass:=nil
   endif

   BEHAVIOR.this:rawoinit(parent,name,nAttribs,nMethods,amBlock,methodsBlock)

   META.classOf:ofClass:={||this}

return this

**********************************************************************
static function be_getmethodsimplement(this)
return eval(C.BEHAVIOR:objmethodsimplement)

**********************************************************************
static function be_isClass(this)
return .t.

**********************************************************************
static function be_attribIdx(this,name)
local w
   name:=lower(name)
   w:=ascan(BEHAVIOR.this:attribs,{|x| x[2]==name})
return if (w==0,nil,BEHAVIOR.this:attribs[w][1])

**********************************************************************
static function primitiveCreateObj(aClass)
// KÇsz°t egy objektumot, ami az aClass oszt†lyba fog tartozni.
// Akkor is mñkîdnie kell, ha az aClass parent-je nincs be†ll°tva.

local obj
   obj:=array(BEHAVIOR.aClass:objSize)
   obj[1]:=eval(BEHAVIOR.aClass:objMethodsImplement)
return obj


**********************************************************************
static function evalAmBlocks(aClass)
local m,c,i
// local t

   // t:=eval(BEHAVIOR.aClass:objMethodsImplement)
   m:={}
   c:=aClass
   while(c!=nil)
      if (nil!=BEHAVIOR.c:amBlock)
         aadd(m,BEHAVIOR.c:amBlock)
      endif
      c:=BEHAVIOR.c:parent
   end while

   for i:=len(m) to 1 step -1
      eval(m[i],BEHAVIOR.aClass:attribs,BEHAVIOR.aClass:methods)
   end for
return aClass

**********************************************************************
static function evalMethodsBlocks(aClass)
local t,m,c,i

   t:=eval(BEHAVIOR.aClass:objMethodsImplement)
   m:={}
   c:=aClass
   while(c!=nil)
      if (nil!=BEHAVIOR.c:methodsBlock)
         aadd(m,BEHAVIOR.c:methodsBlock)
      endif
      c:=BEHAVIOR.c:parent
   end while

   for i:=len(m) to 1 step -1
      eval(m[i],t)
   end for
return aClass

**********************************************************************
static function primitiveInitClass(aClass,parent,name,nAttribs,nMethods,;
                                   amBlock,methodsBlock)
// Inicializ†lja az aClass oszt†lyt.
// Akkor is mñkîdnie kell, ha az aClass parent-je nincs be†ll°tva, Çs
// a parent nil.

local t
   BEHAVIOR.aClass:parent  :=parent
   BEHAVIOR.aClass:name    :=name
   BEHAVIOR.aClass:objSize :=nAttribs
   BEHAVIOR.aClass:attribs :=array(nAttribs-CA_NIL_N)
   BEHAVIOR.aClass:methods :=array(nMethods-CM_NIL_N)
   BEHAVIOR.aClass:amBlock := amBlock
   BEHAVIOR.aClass:methodsBlock := methodsBlock

   t:=array(nMethods)
   t[1]:={||aClass}
   t[3]:=ob_getObjId()
   BEHAVIOR.aClass:objMethodsImplement    := {||t}

   // VÇgrehajtjuk az îsszes ìsre, meg mag†ra is a methodsBlock-ot.
   evalMethodsBlocks(aClass)
   // VÇgrehajtjuk az îsszes ìsre, meg mag†ra is az amBlock-ot.
   evalAmBlocks(aClass)
return aClass

**********************************************************************
static function primitiveMetaClassCreate()
// ElkÇsz°ti a MetaClass oszt†lyt. Ennek primit°vnek kell lenni, mert
// semmilyen objektum mñvelet nem tud futni, am°g nincs MetaClass.

local tMetaClass

   if (CA_METACLAS_N!=CA_META_N)
      alert("CA_METACLAS_N!=CA_META_N;"+;
            "A MetaClass-t nem lehet elkÇsz°teni.")
      quit
   endif

   // A MetaClass Çs a Meta mñveleti t†bl†j†nak ugyanannak kell lennie.
   if (CM_METACLAS_N!=CM_META_N)
      alert("CM_METACLAS_N!=CM_META_N;"+;
            "A MetaClass-t nem lehet elkÇsz°teni.")
      quit
   endif

   MetaClass:=array(CA_METACLAS_N)
   BEHAVIOR.MetaClass:parent  :=nil // Nem tudjuk kitîlteni, mert ez a Meta
                                    // lesz.
   BEHAVIOR.MetaClass:name    :="METACLAS"
   BEHAVIOR.MetaClass:objSize :=CA_METACLAS_N  
   BEHAVIOR.MetaClass:attribs :=array(CA_METACLAS_N)
   BEHAVIOR.MetaClass:methods :=array(CM_METACLAS_N)
   BEHAVIOR.MetaClass:amBlock :={|ta,tm| nil/*amMETACLAS(ta,tm)*/}
   BEHAVIOR.MetaClass:methodsBlock:={|t|t/*ciMETACLAS(t)*/}

   tMetaClass:=array(CM_META_N) 
   tMetaClass[1]:={||MetaClass}
   BEHAVIOR.MetaClass:objMethodsImplement := {||tMetaClass}

   // Ezzel a MetaClass kÇszen is van, azzal a kitÇtellel, hogy
   // nincs be†ll°tva a parent-je, Çs nem futottak az
   // amBlock-ok Çs a methodsBlock-ok.

return MetaClass


**********************************************************************
static function primitiveAllAPClassCreate()
// ElkÇsz°ti mind e nÇgy oszt†lyt.

   primitiveMetaClassCreate()

   Meta:=primitiveCreateObj(MetaClass)
   primitiveInitClass(Meta,nil,"META",CA_META_N,CM_META_N,nil,nil)

   Behavior:=primitiveCreateObj(Meta)
   primitiveInitClass(Behavior,nil,"BEHAVIOR",CA_BEHAVIOR_N,CM_BEHAVIOR_N,nil,nil)

   Object:=primitiveCreateObj(Behavior)
   primitiveInitClass(Object,nil,"OBJECT",CA_OBJECT_N,CM_OBJECT_N,nil,nil)

   // Most minden oszt†ly kÇszen van, kivÇve, hogy nincsenek be†ll°tva
   // a parentek, valamint nincsenek be†ll°tva Çs nem futottak az 
   // amBlock-ok Çs a methodsBlock-ok.
   
   // A szÅlì oszt†lyok be†llit†sa.
   BEHAVIOR.Object:parent    :=nil
   META.Behavior:parent      :=Object
   METACLAS.Meta:parent      :=Behavior
   METACLAS.MetaClass:parent :=Meta

   // Az amBlock-ok Çs a methodsBlockok be†ll°t†sa.

   BEHAVIOR.Object:amBlock         :={|ta,tm|amOBJECT(ta,tm)}
   BEHAVIOR.Object:methodsBlock    :={|t|ciOBJECT(t)}
   META.Behavior:amBlock           :={|ta,tm|amBEHAVIOR(ta,tm)}
   META.Behavior:methodsBlock      :={|t|ciBEHAVIOR(t)}
   METACLAS.Meta:amBlock           :={|ta,tm|amMETA(ta,tm)}
   METACLAS.Meta:methodsBlock      :={|t|ciMETA(t)}
   METACLAS.MetaClass:amBlock      :={|ta,tm|nil/*amMETACLAS(ta,tm)*/}
   METACLAS.MetaClass:methodsBlock :={|t|t/*ciMETACLAS(t)*/}

   // Az amBlock-ok Çs a methodsBlockok vÇgrehajt†sa.
   evalMethodsBlocks(Object)
   evalAmBlocks(Object)

   evalMethodsBlocks(Behavior)
   evalAmBlocks(Behavior)
   
   evalMethodsBlocks(Meta)
   evalAmBlocks(Meta)

   evalMethodsBlocks(MetaClass)
   evalAmBlocks(MetaClass)

return nil
**********************************************************************



**********************************************************************
*                                                                    *
*                          A META oszt†ly                            * 
*                                                                    *
**********************************************************************

**********************************************************************
function clMETA()
// Az oszt†ly elìszedÇse. (class)

   if (Meta==nil)
      primitiveAllAPClassCreate()
   endif
return Meta

**********************************************************************
// function clMETACLAS()
// A Meta oszt†ly meta oszt†ly†t adja.
// Ez m†r megvan.


**********************************************************************
static function me_oinit(this,parent,name,nAttribs,nMethods,amBlock,methodsBlock)
// Inicializ†lja a 'this' metaoszt†lyt.
// Mj.: H°vja a Behavior oinit()-jÇt, Çs inicializ†lja
// az ofClass-t 'nil'-re.

   be_rawoinit(this,parent,name,nAttribs,nMethods,amBlock,methodsBlock)
   META.this:ofClass:=nil
   // ciBEHAVIOR(eval(META.this:objMethodsImplement))
return this

**********************************************************************
static function me_onew(this,parent,name,nAttribs,nMethods,;
                        amBlock,methodsBlock)
// LÇtrehoz egy £j oszt†lyt, Çs inicializ†lja.
// Az £j oszt†ly a 'this' eleme lesz, teh†t az £j oszt†ly
// oszt†lya a 'this'lesz.

// Mj.: Ez a 'Meta' egy elemÇnek a mñvelete. A 'Meta' elemei
// pedig meta oszt†lyok. Teh†t ez egy meta osztaly egy elemÇt
// hozza lÇtre, a meta oszt†lyok elemei pedig oszt†lyok. Teh†t
// ez egy £j oszt†lyt (Çs nem metaoszt†lyt!!!) hoz lÇtre.

local obj

   #ifdef ATTEVE
   if (META.this:ofClass!=nil)
      alert("meta:onew Ennek a metaoszt†lynak m†r van lÇtrehozva eleme!;"+;
            "'"+META.this:name+"'")
      META.this:ofClass:=nil
   endif
   #endif
   obj:=META.this:ocreate() // Egy 'this' oszt†ly£ objektum lÇtrehoz†sa.
   BEHAVIOR.obj:oinit(parent,name,nAttribs,nMethods,amBlock,methodsBlock)
   // META.this:ofClass:=obj
   BEHAVIOR.obj:oinitclass()
return obj

**********************************************************************
static function me_ocreate(this)
// LÇtrehoz egy £j oszt†lyt, de nem inicializ†lja.

local c

   #ifdef NEMKELL
   if (META.this:ofClass!=nil)
      alert("meta:ocreate Ennek a metaoszt†lynak m†r van lÇtrehozva eleme!;"+;
            "'"+META.this:name+"'")
      META.this:ofClass:=nil
   endif
   #endif

   c:=be_ocreate(this)
   
   if (BEHAVIOR.c:needClassId())
      BEHAVIOR.c:classId:=getNextClassId()
   endif
return c

**********************************************************************

**********************************************************************
*                                                                    *
*                        A METACLAS oszt†ly                          * 
*                                                                    *
**********************************************************************


**********************************************************************
function clMETACLAS()
// Az oszt†ly elìszedÇse. (class)

   if (MetaClass==nil)
      primitiveAllAPClassCreate()
   endif
return MetaClass

**********************************************************************
// function clMETACLASCLASS()
// A MetaClass oszt†ly meta oszt†ly†t adja.
// Ez m†r megvan.

**********************************************************************
function retobj(obj)
return obj

**********************************************************************
function retnil()
return nil

**********************************************************************
function retemptystr()
return ""

**********************************************************************
function rettrue()
return .t.

**********************************************************************
function retfalse()
return .f.

**********************************************************************
function retzero()
return 0

**********************************************************************
function retemptydate()
return ctod("")

**********************************************************************
function errAbstract(this,methodName,methodIdx,params)
local err

   err:=errorNew()
   err:cargo:=this
   err:args:=params
   err:canRetry:=.f.
   err:description:="Can't execute abstract method"
   err:filename:=""
   err:severity:=ES_ERROR
   err:operation:=if(valtype(methodName)=='C',methodName+"()","")
   err:subSystem:="OBJC"
   err:subCode:=1
   eval(errorblock(),err)
return nil

**********************************************************************
function errAttrib(funName,params/*className,attribName*/)
local err

   err:=errorNew()
   err:cargo:={}
   err:args:=params
   err:canRetry:=.f.
   err:description:="Can't find attributum: "+;
                    if(valtype(params[1])=='C',params[1],"")
   err:filename:=""
   err:severity:=ES_ERROR
   err:operation:=funName
   err:subSystem:="OBJC"
   err:subCode:=1
   eval(errorblock(),err)
return nil

**********************************************************************
function ob_getObjId()
static t:={}
return t

**********************************************************************
function isObject(obj)
// Ez a fÅggvÇny ak†rmilyen (!) ÇrtÇkrìl megmondja, hogy objektum-e 
// vagy sem.

return valtype(obj)=='A' .and.;
       len(obj)>=1 .and.;
       valtype(obj[1])=='A' .and.;
       len(obj[1])>=3 .and.;
       valtype(obj[1][3])=='A' .and.;
       obj[1][3]==ob_getObjId()

**********************************************************************
function getNextClassId()
// Ezt kell h°vnia annak az oszt†lynak, aki id-t akar.
static id:=0
return ++id

**********************************************************************
static function be_needClassId(this)
return .f.

**********************************************************************
static function be_isInheritFrom(this,parentClass)
local w

   if (parentClass==nil)
      return .t. // Spec eset, v†laszthatn†nk a .f.-t is.
   endif
   if (nil!=(w:=BEHAVIOR.parentClass:classId))
      return BEHAVIOR.this:parentClassIds!=nil .and.;
             len(BEHAVIOR.this:parentClassIds)>=w .and.;
             BEHAVIOR.this:parentClassIds[w]==w
   endif

   // Fel a f†n.
   // alert("M†szunk: "+;
   //       "this: "+BEHAVIOR.this:name+";"+;
   //       "parentClass: "+BEHAVIOR.parentClass:name)
   w:=this
   while(w!=nil)
      if (w==parentClass)
         return .t.
      endif
      w:=BEHAVIOR.w:parent
   end while
return .f.
**********************************************************************
