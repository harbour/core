/*
 * $Id$
 */

//*******************************************************************
// token.prg: a TOKEN oszt†ly implement†ci¢ja.
// 1999, Csisz†r Levente

//*******************************************************************
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

#define _TOKEN_PRG_
#define _IMPLEMENT_ONEW_

#include "token.och"

// #define TKCL_NEV      "nev"
// #define TKCL_STRING   '"string"'
// #define TKCL_SZAMTOMB "1"

//*******************************************************************
implement oinit(id,str,file,line,pos)
   super:oinit()
   this:id   :=id
   this:str  :=str
   this:file :=file
   this:line :=line
   this:pos  :=pos
   
   if (id==TKID_CHAR)
      if (nil==(this:classify:=thisclass:findClassify(str)))
         if (len(str)==1 .and. str$"(){}[]")
            this:classify:=TKCL_PARENT
         else
            this:classify:=str
         endif
      endif
   elseif (id==TKID_NEV)
      this:classify:=TKCL_NEV
   elseif (id==TKID_STRING)
      this:classify:=TKCL_TRUE//TKCL_STRING
   elseif (id==TKID_SZAMTOMB)
      this:classify:=TKCL_SZAMTOMB
   endif
return this

//*******************************************************************
implement copyToken(id,str)
// KÇsz°t egy olyan tokent, ami ennek a m†solata, csak az id-je
// Çs az str-je m†s, Çs az error nil.
// Ha az str nil, akkor behelyettes°ti a saj†t str-jÇt.
return thisclass:onew(id,;
                      if(str==nil,this:str,str),;
                      this:file,this:line,this:pos)
   
//*******************************************************************
cimplement onewError(id,str,file,line,pos,error)
local o

   o:=class:onew(id,str,file,line,pos)
   CTHIS.o:error:=error
return o

//*******************************************************************
implement isError()
return this:error!=nil

//*******************************************************************
static function strTkErrCode(tkErrCode)
// Egy hibak¢db¢l stringet csin†l.
local str,i

   str:=padr(tkErrCode,4,' ')+" "

   if (0==(i:=ascan(C.TOKEN:errStrArray,{|x| x[1]==tkErrCode})))
      str+="Unknown error"
   else
      str+=C.TOKEN:errStrArray[i][2]
   endif
return str

*********************************************************************
static function levag(str,n)

   if (len(str)<=n)
      return str
   endif
return left(str,len(str)-4)+" ..."

//*******************************************************************
implement errorStr()

   if (!this:isError())
      return nil
   endif
   
   if (valtype(this:error)=="A")
      return toStr(this:error[1])+;
             " Error: "+strTkErrCode(this:error)+" "+;
             toStr(this:error[3])
   endif
return padr(this:file+"("+toStr(this:line)+")",20)+" Error: "+;
       strTkErrCode(this:error)+" "+;
       levag(if(this:str==nil,"",this:str),20)
   
//*******************************************************************
cimplement oinitclass()
  
   superclass:oinitclass()
   
   class:errStrArray:={}
                  
   #ifdef KESOBB
   aadd(class:errStrArray,{RDERR_ENDSTR           ,"Unterminated string: "})
   aadd(class:errStrArray,{RDERR_ENDPCCOMMENT     ,"Unterminated /* */ comment"})
   aadd(class:errStrArray,{RDERR_INCOMPLETE       ,"Incomplete statement (;;x)"})

   aadd(class:errStrArray,{PPCERR_INVALIDMN       ,"Invalid name follows '#'"})
   aadd(class:errStrArray,{PPCERR_UNSUPPORTED     ,"Unsupported '#' command"})
   aadd(class:errStrArray,{PPCERR_SDEFINE         ,"Syntax error in #define"})
   aadd(class:errStrArray,{PPCERR_LDEFINE         ,"Label missing in #define"})
   aadd(class:errStrArray,{PPCERR_PDEFINE         ,"Comma or right parenthesis missing in #define"})
   aadd(class:errStrArray,{PPCERR_MDUPLICATE      ,"Redefinition or duplicate definition of #define"})
   aadd(class:errStrArray,{PPCERR_UNDEF           ,"Label missing in #undef"})
   aadd(class:errStrArray,{PPCERR_IFDEF           ,"Label missing in #ifdef"})
   aadd(class:errStrArray,{PPCERR_IFNDEF          ,"Label missing in #ifndef"})
   aadd(class:errStrArray,{PPCERR_ELSE            ,"Syntax error in #else"})
   aadd(class:errStrArray,{PPCERR_ENDIF           ,"Syntax error in #endif"})
   aadd(class:errStrArray,{PPCERR_NMENDIF         ,"#endif does not match #if"}) 
   aadd(class:errStrArray,{PPCERR_INCLUDE         ,"Bad filename in #include"}) 
   aadd(class:errStrArray,{PPCERR_MAXINCLUDE      ,"Too many nested include"}) 
   aadd(class:errStrArray,{PPCERR_FINDINCLUDE     ,"Can't find include file"}) 
   aadd(class:errStrArray,{PPCERR_OPENINCLUDE     ,"Can't open include file"}) 

   aadd(class:errStrArray,{PPCERR_STRANCMD        ,"Missing => in #translate/#command"})
   aadd(class:errStrArray,{PPCERR_BTRANCMD        ,"Bad match marker in #translate/#command"})
   aadd(class:errStrArray,{PPCERR_LTRANCMD        ,"Label error in #translate/#command"})
   aadd(class:errStrArray,{PPCERR_UTRANCMD        ,"Unclosed optional clause in #translate/#command"})
   aadd(class:errStrArray,{PPCERR_UTRANCMD        ,"Result pattern contains nested clauses in #translate/#command"})
   #endif
   class:equivClass:={}
   aadd(class:equivClass,{"!","<>"})
   aadd(class:equivClass,{"@","#","$","|","\","<",">",".","?","->","<=",">=",".AND.",".OR."})
   aadd(class:equivClass,{"=",":",":=","==","+=","-=","*=","/=","%="})
   aadd(class:equivClass,{"++","--"})
   // aadd(class:equivClass,{"(a)","{a}"})
   aadd(class:equivClass,{".T.",".F.",'""'})
   // aadd(class:equivClass,{1,.1,1.1})
   
return class

//*******************************************************************
implement printToStr()
local str

   #ifdef OLD
   str:="Token: "+toStr(this:id)+": "+if(this:str==nil,"",this:str)
   if (this:isError())
      str+=", Error: "+this:errorStr()
   endif
   #endif

   str:="Token: "+toStr(this:id)+": "+this:getStr()
return str

//*******************************************************************
implement getStr()
local str

   str:=if(this:str==nil,"",this:str)
   if (this:id==TKID_REGULAR_MATCH_MARKER)
      str:="<"+str+">"
   elseif (this:id==TKID_WILD_MATCH_MARKER)
      str:="<*"+str+"*>"
   elseif (this:id==TKID_EXT_EXPR_MATCH_MARKER)
      str:="<("+str+")>"
   elseif (this:id==TKID_LIST_MATCH_MARKER)
      str:="<"+str+",...>"
   elseif (this:id==TKID_RESTRICTED_MATCH_MARKER)
      // Ennek kÅlîn oszt†lya van, Çs a getStr meg van °rva.
      str:="<"+str+": nem °rhat¢ ki>"

   elseif (this:id==TKID_DUMB_STR_RESULT_MARKER)
      str:="#<"+str+">"
   elseif (this:id==TKID_REGULAR_RESULT_MARKER)
      str:="<"+str+">"
   elseif (this:id==TKID_STRINGIFY_RESULT_MARKER)
      str:='<"'+str+'">'
   elseif (this:id==TKID_SMART_STR_RESULT_MARKER)
      str:="<("+str+")>"
   elseif (this:id==TKID_BLOCKIFY_RESULT_MARKER)
      str:="<{"+str+"}>"
   elseif (this:id==TKID_LOGIFY_RESULT_MARKER)
      str:="<."+str+".>"
      
   elseif (this:id==TKID_MALTERSET)
      // Ennek kÅlîn oszt†lya van, Çs a getStr meg van °rva.
      str:="[ 'malterset: nem °rhat¢ ki' ]"

   elseif (this:id==TKID_RALTER)
      // Ennek kÅlîn oszt†lya van, Çs a getStr meg van °rva.
      str:="[ 'ralter: nem °rhat¢ ki' ]"
   endif
      
   if (this:isError())
      str+=", Error: "+this:errorStr()+guessedEol()
   endif
return str

//*******************************************************************
cimplement copyFromToken(t,id,str)
// KÇsz°t egy olyan tokent, ami a t-nek a m†solata, csak az id-je
// Çs az str-je m†s, Çs az error nil.
// Ha az str nil, akkor behelyettes°ti a t:str-t.
return class:onew(id,; 
                  if(str==nil,TOKEN.t:str,str),;
                  TOKEN.t:file,TOKEN.t:line,TOKEN.t:pos)
   
//*******************************************************************
cimplement findClassify(str)
local i

   for i:=1 to len(class:equivClass)
      if (0!=ascan(class:equivClass[i],{|x| x==str}))
         return class:equivClass[i][1]
      endif
   end for
return nil
   
//*******************************************************************
   
