/*
 * $Id$
 */

//*******************************************************************
// freader.prg: Az freader oszt ly implement ci¢ja.
// 1999, Csisz r Levente

#include "fileio.ch"

//*******************************************************************
// #include "prserr.ch"

//*******************************************************************
#define FRERRGROUP          "freader"
#define FRERR_REOPEN        {FRERRGROUP,"reopen"}
#define FRERR_NOTFOUND      {FRERRGROUP,"notfound"}
#define FRERR_OPEN          {FRERRGROUP,"open"}
#define FRERR_NOTOPENREAD   {FRERRGROUP,"notopenread"}
#define FRERR_READ          {FRERRGROUP,"read"}
// #define FRERR_   {FRERRGROUP,""}

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "prserr.och"

//*******************************************************************
#define _FREADER_PRG_
#define _IMPLEMENT_ONEW_

#include "freader.och"

//*******************************************************************

implement oinit(errorStream)
   super:oinit("",errorStream)
return this

//*******************************************************************
implement pathName()
return addFPath(this:path,this:name)

//*******************************************************************
cimplement oinitclass()
// local errPrintBlock

   superclass:oinitclass()
   
   /*
   errPrintBlock:={|prserr,errStr|;
      padr(PRSERR.prserr:file+":",20)+;
      " Error: "+errStr+;
      if(PRSERR.prserr:params==nil,;
         "",;
         ", os error code: "+toStr(PRSERR.prserr:params));
   }
   */
   
   C.PRSERR:registerError(FRERR_REOPEN,;
      "Should not reopen a file in an FREADER object!")
   C.PRSERR:registerError(FRERR_NOTFOUND,;
      "File not found")
   C.PRSERR:registerError(FRERR_OPEN,;
      "Open error. OS error code: $1")
   C.PRSERR:registerError(FRERR_NOTOPENREAD,;
      "Read error, file is not open!")
   C.PRSERR:registerError(FRERR_READ,;
      "Read error. OS error code: $1")
return class

//*******************************************************************
implement open(path,fileName)
local wFid,pName
           
   if (this:fid!=nil)
      aadd(this:errorStream,C.PRSERR:onew(FRERR_REOPEN,nil,this:pathName(),0,0))
      return nil
   endif
   this:name:=fileName
   this:path:=path
   
   this:buffer:=""
   this:ibuffer:=1

   pName:=this:pathName()
                      
   if (!file(pName))
      aadd(this:errorStream,C.PRSERR:onew(FRERR_NOTFOUND,nil,this:pathName(),0,0))
      return nil
   endif
   if (-1==(wFid:=fopen(pName,FO_READ)))
      // this:error:={this:name,FRERR_OPEN,"Open error, os error code:  "+toStr(ferror())}
      aadd(this:errorStream,C.PRSERR:onew(FRERR_OPEN,{toStr(ferror())},this:pathName(),0,0))
      return nil
   endif
   this:fid:=wFid
   
return this
   
//*******************************************************************
implement close()

   if (this:fid!=nil)
      fclose(this:fid)
      this:fid:=nil
   endif
return nil

//*******************************************************************
implement isOpen()
return this:fid!=nil

//*******************************************************************
static function findFileInDir(pathArray,fileName)
local i

   for i:=1 to len(pathArray)
      if (file(addFPath(pathArray[i],fileName)))
         return {pathArray[i],fileName}
      endif
   end for
return nil

//*******************************************************************
implement dOpen(pathArray,fileName)
local pn

   if (nil==(pn:=findFileInDir(pathArray,fileName)))
      return nil
   endif
return this:open(pn[1],pn[2])

//*******************************************************************
implement destruct()
   this:close()
return nil

//*******************************************************************
implement readItem()
local c,buf,n
local wErr

   if (this:fid==nil)
      // this:error:={this:name,FRERR_READ,"Read error, file is not open!"}
      aadd(this:errorStream,C.PRSERR:onew(FRERR_NOTOPENREAD,nil,this:pathName(),0,0))
      return nil
   endif
   
   if (this:buffer==nil)
      // EOF
      return nil
   endif
              
   if (this:ibuffer>len(this:buffer))

      buf:=space(1024)
      
      if (0==(n:=fread(this:fid,@buf,len(buf))))
         // Nem lehet meg llap¡tani, hogy fil‚ v‚ge vagy hiba, ¡gy meg
         // kell n‚zni az ferror()-t.
         this:buffer:=nil
         if (0!=(wErr:=ferror()))
            // this:error:={this:name,FRERR_READ,"Read error, os error code: "+toStr(ferror())}
            aadd(this:errorStream,C.PRSERR:onew(FRERR_READ,{toStr(wErr)},this:pathName(),0,0))
         endif
         return nil
      endif
      
      this:buffer:=left(buf,n)
      this:ibuffer:=1

   endif

   c:=substr(this:buffer,this:ibuffer,1)
   this:ibuffer:=this:ibuffer+1

return c 
   
//*******************************************************************


