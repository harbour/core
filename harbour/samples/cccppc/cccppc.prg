/*
 * $Id$
 */

//*******************************************************************
// cccpp.prg: A fìprogram
// 1999, Csisz†r Levente

//*******************************************************************
#define VERSION "v0.7.06"

//*******************************************************************
#include "fileio.ch"

//*******************************************************************
#include "debug.ch"
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "freader.och"
#include "sreader.och"
// #include "lreader.och"
#include "nparser.och"
// #include "lparser.och"
#include "incl.och"
// #include "hparser.och"
// #include "mcontrol.och"
// #include "parser.och"
#include "token.och"

#include "defdict.och"
#include "xtrdict.och"
#include "extrdict.och"
//#include "prserr.och"

//*******************************************************************
#ifdef TEST
function main(p1,p2,p3,p4,p5,p6,p7,p8,p9)
   teszt(p1,p2,p3,p4,p5,p6,p7,p8,p9)
return nil
#else
#ifdef OLD
function main(; // 265 db paramÇter. Sajnos CCC-ben nincs szabv†nyos
              ; // m¢dszer tetsz. sz†m£ paramÇter †tvÇtelÇre.
   p00,p01,p02,p03,p04,p05,p06,p07,p08,p09,p0a,p0b,p0c,p0d,p0e,p0f,;
   p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p1a,p1b,p1c,p1d,p1e,p1f,;
   p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p2a,p2b,p2c,p2d,p2e,p2f,;
   p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p3a,p3b,p3c,p3d,p3e,p3f,;
   p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p4a,p4b,p4c,p4d,p4e,p4f,;
   p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p5a,p5b,p5c,p5d,p5e,p5f,;
   p60,p61,p62,p63,p64,p65,p66,p67,p68,p69,p6a,p6b,p6c,p6d,p6e,p6f,;
   p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p7a,p7b,p7c,p7d,p7e,p7f,;
   p80,p81,p82,p83,p84,p85,p86,p87,p88,p89,p8a,p8b,p8c,p8d,p8e,p8f,;
   p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p9a,p9b,p9c,p9d,p9e,p9f,;
   pa0,pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8,pa9,paa,pab,pac,pad,pae,paf,;
   pb0,pb1,pb2,pb3,pb4,pb5,pb6,pb7,pb8,pb9,pba,pbb,pbc,pbd,pbe,pbf,;
   pc0,pc1,pc2,pc3,pc4,pc5,pc6,pc7,pc8,pc9,pca,pcb,pcc,pcd,pce,pcf,;
   pd0,pd1,pd2,pd3,pd4,pd5,pd6,pd7,pd8,pd9,pda,pdb,pdc,pdd,pde,pdf,;
   pe0,pe1,pe2,pe3,pe4,pe5,pe6,pe7,pe8,pe9,pea,peb,pec,ped,pee,pef,;
   pf0,pf1,pf2,pf3,pf4,pf5,pf6,pf7,pf8,pf9,pfa,pfb,pfc,pfd,pfe,pff;
)

local p:={
   p00,p01,p02,p03,p04,p05,p06,p07,p08,p09,p0a,p0b,p0c,p0d,p0e,p0f,;
   p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p1a,p1b,p1c,p1d,p1e,p1f,;
   p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p2a,p2b,p2c,p2d,p2e,p2f,;
   p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p3a,p3b,p3c,p3d,p3e,p3f,;
   p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p4a,p4b,p4c,p4d,p4e,p4f,;
   p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p5a,p5b,p5c,p5d,p5e,p5f,;
   p60,p61,p62,p63,p64,p65,p66,p67,p68,p69,p6a,p6b,p6c,p6d,p6e,p6f,;
   p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p7a,p7b,p7c,p7d,p7e,p7f,;
   p80,p81,p82,p83,p84,p85,p86,p87,p88,p89,p8a,p8b,p8c,p8d,p8e,p8f,;
   p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p9a,p9b,p9c,p9d,p9e,p9f,;
   pa0,pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8,pa9,paa,pab,pac,pad,pae,paf,;
   pb0,pb1,pb2,pb3,pb4,pb5,pb6,pb7,pb8,pb9,pba,pbb,pbc,pbd,pbe,pbf,;
   pc0,pc1,pc2,pc3,pc4,pc5,pc6,pc7,pc8,pc9,pca,pcb,pcc,pcd,pce,pcf,;
   pd0,pd1,pd2,pd3,pd4,pd5,pd6,pd7,pd8,pd9,pda,pdb,pdc,pdd,pde,pdf,;
   pe0,pe1,pe2,pe3,pe4,pe5,pe6,pe7,pe8,pe9,pea,peb,pec,ped,pee,pef,;
   pf0,pf1,pf2,pf3,pf4,pf5,pf6,pf7,pf8,pf9,pfa,pfb,pfc,pfd,pfe,pff;
}
#else
#ifdef _CCC_
// Sajnos csak 192 paramÇtert tud leford°tani.
function main(;
   p00,p01,p02,p03,p04,p05,p06,p07,p08,p09,p0a,p0b,p0c,p0d,p0e,p0f,;
   p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p1a,p1b,p1c,p1d,p1e,p1f,;
   p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p2a,p2b,p2c,p2d,p2e,p2f,;
   p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p3a,p3b,p3c,p3d,p3e,p3f,;
   p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p4a,p4b,p4c,p4d,p4e,p4f,;
   p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p5a,p5b,p5c,p5d,p5e,p5f,;
   p60,p61,p62,p63,p64,p65,p66,p67,p68,p69,p6a,p6b,p6c,p6d,p6e,p6f,;
   p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p7a,p7b,p7c,p7d,p7e,p7f,;
   p80,p81,p82,p83,p84,p85,p86,p87,p88,p89,p8a,p8b,p8c,p8d,p8e,p8f,;
   p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p9a,p9b,p9c,p9d,p9e,p9f,;
   pa0,pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8,pa9,paa,pab,pac,pad,pae,paf,;
   pb0,pb1,pb2,pb3,pb4,pb5,pb6,pb7,pb8,pb9,pba,pbb,pbc,pbd,pbe,pbf;
)

local p:={;
   p00,p01,p02,p03,p04,p05,p06,p07,p08,p09,p0a,p0b,p0c,p0d,p0e,p0f,;
   p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p1a,p1b,p1c,p1d,p1e,p1f,;
   p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p2a,p2b,p2c,p2d,p2e,p2f,;
   p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p3a,p3b,p3c,p3d,p3e,p3f,;
   p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p4a,p4b,p4c,p4d,p4e,p4f,;
   p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p5a,p5b,p5c,p5d,p5e,p5f,;
   p60,p61,p62,p63,p64,p65,p66,p67,p68,p69,p6a,p6b,p6c,p6d,p6e,p6f,;
   p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p7a,p7b,p7c,p7d,p7e,p7f,;
   p80,p81,p82,p83,p84,p85,p86,p87,p88,p89,p8a,p8b,p8c,p8d,p8e,p8f,;
   p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p9a,p9b,p9c,p9d,p9e,p9f,;
   pa0,pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8,pa9,paa,pab,pac,pad,pae,paf,;
   pb0,pb1,pb2,pb3,pb4,pb5,pb6,pb7,pb8,pb9,pba,pbb,pbc,pbd,pbe,pbf;
}
#else
// Sajnos csak 16 paramÇtert tud leford°tani.
function main(;
   p00,p01,p02,p03,p04,p05,p06,p07,p08,p09,p0a,p0b,p0c,p0d,p0e,p0f;
)

local p:={;
   p00,p01,p02,p03,p04,p05,p06,p07,p08,p09,p0a,p0b,p0c,p0d,p0e,p0f;
}
#endif

#endif
local pos
   
   if (0!=(pos:=ascan(p,{|x| x==nil})))
      asize(p,pos-1)
   endif
   PDEBUG(outerr(p,newline()))
   // outerr(p,newline())
   ccp_appMain(p)   
   // teszt(p1,p2,p3,p4,p5,p6,p7,p8,p9)
   
return nil

//*******************************************************************
function cccpp_info()
   outstd(;
"cccppc "+VERSION+", CCC preprocessor, Copyright (c) 1999 Levente Csisz†r"+newline()+;
""+newline()+;
"Usage: cccppc [-I<incdir>] [-o<outputfile>] [-e<extension>]"+newline()+;
"              [-h] [-v] [-u<User include file>] [-Dname[=text]]"+newline()+;
"              [-l<number>] <inputfile>"+newline()+;
"   -I<incdir>    : Include directories. -I- deletes the list."+newline()+;
"   -o<outputfile>: Specify the output file."+newline()+;
"                   Default: <base of input file>.<extension>"+newline()+;
"   -e<extension> : Specify the output file extension. Default: ppc"+newline()+;
"   -h            : Help. Print this text."+newline()+;
"   -v            : Verbose. Force verbose diagnostic message."+newline()+;
"   -u<inclfile>  : User include file. Load this file before processing"+newline()+;
"                   input file."+newline()+;
"   -Dname[=text] : Define a macro with name 'name' and assign 'text' to it."+newline()+;
"   -l<number>    : Limit deep of nested include files. 0: no limit."+newline()+;
"                   Default: 32."+newline()+;
"   -t[r|s]       : Translate parser algorithm: -tr tree, -ts sequence."+newline()+;
"                   Default: -tr."+newline()+;
"")
return nil

//*******************************************************************
function ccp_appMain(p)
local opt,hibaSzoveg,isOpt
local fileName, incArray
local outFile,defExt,verbose,userFiles
local defMacros,maxInclDeep
local trPrsAlg

                  
   errorlevel(0)
   opt:=parseOpt("-I:,-H,-h,-o:,-e:,-v,-u:,-D:,-l:,-t:",p,@hibaSzoveg)
   if (!empty(hibaSzoveg))
      outerr(hibaSzoveg,newline())
      errorlevel(1)
      quit
   endif
   errorlevel(0)
   
   if (empty(opt))
      cccpp_info()
      errorlevel(1)
      quit
   endif
#define SHIFTOPT if(!empty(opt)); (adel(opt,1),asize(opt,len(opt)-1)); endif
               
   // Default be†ll°t†sok.
   fileName:=nil
   outFile:=nil
   isOpt:=.t.
   defExt:="ppc"
   verbose:=.f.
   incArray:={}
   userFiles:={}
   defMacros:={}
   maxInclDeep:=32
   trPrsAlg:=TRPRA_TREE
   while(!empty(opt))
      if (isOpt .and. opt[1]=="-H" .or. opt[1]=="-h")
         cccpp_info()
         errorlevel(0)
         quit
      elseif (isOpt .and. opt[1]=="-I")
         // Az include lista.
         /*
            UNIX-on: Csak a -I <dir> forma az elfogadott.
            DOSWIN-en: ';'-vel elv†lasztva tîbb 
                       dir-t is fel lehet sorolni.
         */
         // A -I- Tîrli a list†t.
         // Az opt hossz£s†g†t nem kell vizsg†lni, mert a parseOpt 
         // tesz ide egy Åreset.
         if (opt[2]=="-")
            incArray:={}
         elseif (!opt[2]=="")
            aappend(incArray,felbont(opt[2],";"))
         endif
         SHIFTOPT
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="-e")
         if (empty(opt[2]))
            outerr("Parameter is missing after "+opt[1])
            errorlevel(1)
            quit
         endif
         defExt:=opt[2]
         SHIFTOPT
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="-o")
         if (empty(opt[2]))
            outerr("Parameter is missing after "+opt[1])
            errorlevel(1)
            quit
         endif
         outFile:=opt[2]
         SHIFTOPT
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="-v")
         verbose:=.t.
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="-u")
         if (empty(opt[2]))
            outerr("Parameter is missing after "+opt[1])
            errorlevel(1)
            quit
         endif
         aadd(userFiles,opt[2])
         SHIFTOPT
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="-D")
         if (empty(opt[2]))
            outerr("Parameter is missing after "+opt[1])
            errorlevel(1)
            quit
         endif
         aadd(defMacros,makeDefFromPar(opt[2]))
         SHIFTOPT
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="-l")
         if (empty(opt[2]))
            outerr("Parameter is missing after "+opt[1])
            errorlevel(1)
            quit
         endif
         if (!beloleAll(opt[2],"0123456789"))
            outerr("Parameter should be a number after "+opt[1])
            errorlevel(1)
            quit
         endif
         maxInclDeep:=val(opt[2])
         SHIFTOPT
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="-t")
         if (empty(opt[2]))
            outerr("Parameter is missing after "+opt[1])
            errorlevel(1)
            quit
         endif
         if !(opt[2]=="r" .or. opt[2]=="s")
            outerr("Parameter should be 'r' or 's' after "+opt[1])
            errorlevel(1)
            quit
         endif
         trPrsAlg:=if(opt[2]=="r",TRPRA_TREE,TRPRA_SEQ)
         SHIFTOPT
         SHIFTOPT
      elseif (isOpt .and. opt[1]=="--")
         isOpt:=.f.
         SHIFTOPT
      else
         // A feldolgozand¢ file.
         if (fileName==nil)
            fileName:=opt[1]
         endif
         SHIFTOPT
      endif
   
   end while

   if (fileName==nil)
      cccpp_info()
      errorlevel(1)
      quit
   endif 
   
   if (outFile==nil)
      if (fileName=='-')
         // A standard inputr¢l kell olvasni.
         outFile:='-'
      else
         outFile:=extractFName(fileName)
         if (!empty(defExt))
            outFile+="."+defExt
         endif
      endif
   endif
   if (!fileName=='-' .and. fileName==outFile)
      outerr("input and output should be different: "+newline()+;
             "input : "+toStr(filename)+newline()+;
             "output: "+toStr(outfile)+newline())
      errorlevel(1)
      quit
   endif

   if (ccp_compile(fileName,outFile,defMacros,userFiles,;
                   incArray,maxInclDeep,verbose,trPrsAlg))
      errorlevel(1)
      quit
   endif

return nil

//*******************************************************************
static function makeDefFromPar(str)
// str: name[=text]
local i
local name,txt

   if (0!=(i:=at("=",str)))
      txt:=substr(str,i+1)
      name:=alltrim(left(str,i-1))
   else
      name:=alltrim(str)
      txt:=""
   endif
   

   if (empty(name))
      outerr("-D"+str+": name is missing",newline())
      errorlevel(1)
      quit
   endif
   
   for i:=1 to len(name)
      if (!if(i==1,;
              C.NPARSER:isStartnamechar(substr(name,i,1)),;
              C.NPARSER:isNamechar(substr(name,i,1))))
         outerr("-D"+str+": name error",newline())
         errorlevel(1)
         quit
      endif
   end for

return {name,txt}            
   

//*******************************************************************
static function defMacros2str(defMacros)
local i,str

   str:=""
   for i:=1 to len(defMacros)
      str+="#define "+defMacros[i][1]+" "+defMacros[i][2]+guessedEOL()
   end for
return str
   
//*******************************************************************
static function ccp_compile(fileName,outFile,defMacros,userFiles,;
                incArray,maxInclDeep,verbose,trPrsAlg)
// local fr,npr,lpr,hpr,mcr,t,lrd
local iArray
local fid,doClose,str,wStr
local defDict,xtrDict
local errorStream
local fr,hiba
local i
local treePrintBlock

   PDEBUG(outerr({filename,outFile,defMacros,userFiles,;
                  incArray,maxInclDeep,verbose},newline()))
   
   setPGuessEol(newline())

   if (!file(fileName))
      outerr(fileName+": file not found"+newline())
      errorlevel(2)
      quit
   endif   
   if (outfile=='-')
      fid:=1
      doClose:=.f.
   else
      if (-1==(fid:=fcreate(outFile)))
         if (-1==(fid:=fopen(outFile,FO_WRITE)))
            outerr(outFile+": open error: "+toStr(ferror())+newline())
            errorlevel(2)
            quit
         endif
      endif
      doClose:=.t.
   endif
   
   hiba:=.f.
   iArray:=aappend({dirFName(fileName)},incArray)
   defDict:=C.DEFDICT:onew()
   xtrDict:=C.XTRDICT:onew()
   errorStream:={}
   str:=""
   if (!hiba .and. !empty(defMacros))
      str+=defMacros2str(defMacros)
   endif
   if (!hiba .and. !empty(userFiles))
      for i:=1 to len(userFiles)
         if (nil==(wStr:=stringifyStr(userFiles[i],.t.)))
            outerr(toStr(userFiles[i])+;
                   " Error: Illegal "+'"'+" and "+"'"+" character "+;
                   "combination in file name."+newline())
            return .t.
         endif
         str+="#include "+wStr+newline()
         // outerr(str)
      end for
   endif
   if (!hiba .and. !empty(str))
      fr:=C.SREADER:onew(str,"commandline",errorStream)
      if (cccpp_processReader(fr,nil,iArray,maxInclDeep,;
                              defDict,xtrDict,errorStream,trPrsAlg))
         hiba:=.t.
      endif
   endif
   #ifdef OLD
   if (!hiba .and. userFile!=nil)
      fr:=C.FREADER:onew(errorStream)
      FREADER.fr:open("",userFile)
      if (cccpp_processReader(fr,nil,iArray,maxInclDeep,;
                              defDict,xtrDict,errorStream,trPrsAlg))
         hiba:=.t.
      endif
   endif
   #endif
   
   if (!hiba)
      fr:=C.FREADER:onew(errorStream)
      FREADER.fr:open("",fileName)
      if (cccpp_processReader(fr,fid,iArray,maxInclDeep,;
                              defDict,xtrDict,errorStream,trPrsAlg))
         hiba:=.t.
      endif
   endif
   
   if (doClose)
      fclose(fid)
   endif
   
   if (!empty(verbose))

      outerr("Define sz¢t†r: "+newline())
      DEFDICT.defDict:printStr({|x| outerr(x+newline())})

      outerr("XTranslate sz¢t†r: "+newline())
      XTRDICT.xtrDict:printStr({|x| outerr(x+newline())})

      outerr("XCommand sz¢t†r: "+newline())
      XTRDICT.xtrDict:printStr({|x| outerr(x+newline())},.t.)
      
      outerr("XTranslate fa: "+newline())
      treePrintBlock:=;
         {|node,level| ;
            outerr(space(level)+TOKEN.node[1]:printToStr()+;
            if (len(node)>=3 .and. !empty(node[3]),;
               " // "+EXTRDICT.node[3]:printStr(),""),;
            newline())}
      
      evalXTree(XTRDICT.xtrDict:trdicttree,treePrintBlock)

      outerr("XCommand fa: "+newline())
      evalXTree(XTRDICT.xtrDict:cmdicttree,treePrintBlock)
   endif
   
return hiba

//*******************************************************************
static function evalXTree(nodeList,block,level)
local i

   if (level==nil)
      level:=0
   endif
   for i:=1 to len(nodeList)
      eval(block,nodeList[i],level)
      if (!empty(nodeList[i][2]))
         evalXTree(nodeList[i][2],block,level+1)
      endif
   end for
return nil
      
//*******************************************************************

#ifdef OLD
//*******************************************************************
static function cccpp_printErrorStream(errorStream)
   if (!empty(errorStream))
      evalErrorStream(errorStream,{|x| outerr(x,newline())})
      asize(errorStream,0)
   endif
return nil

//*******************************************************************
function cccpp_processFile(inputFileName,outFid,;
                           incArray,defDict,xtrdict,errorStream)
// Feldolgoz egy filÇt, Egyenlìre nem vÇgez hiba ellezìrzÇst.

local fr,lrd,npr,lpr,hpr,mcr,t,inclObj
local line,emptyLines

   fr:=C.FREADER:onew(errorStream)
   FREADER.fr:open("",inputFileName)
   lrd:=C.LREADER:onew(CTK_BOS,CTK_EOS,READER.fr:name,errorStream)
   LREADER.lrd:pushReader(fr)
   npr:=C.NPARSER:onew(lrd,READER.lrd:name,errorStream)
   lpr:=C.LPARSER:onew(npr,READER.npr:name,errorStream)
   inclObj:=C.INCL:onew(lrd,incArray)
   // inclObj:=C.INCL:onew(lrd,{dirFName(filename)})
   hpr:=C.HPARSER:onew(lpr,READER.lpr:name,;
                       defDict/*C.DEFDICT:onew()*/,xtrDict/*C.XTRDICT:onew()*/,;
                       inclObj,errorStream)
   mcr:=C.MCONTROL:onew(hpr,READER.hpr:name,;
                        HPARSER.hpr:defDict,HPARSER.hpr:xtrdict,errorStream)

   line:=""
   emptyLines:=""
   while(nil!=(t:=PARSER.mcr:read()))
      // outstd(TOKEN.t:getStr())
      // wStr:=TOKEN.t:getStr()
      // fwrite(fid,wStr,len(wStr))
      if (outFid!=nil)
         // Itt kell kozmetik†zni a sorokat.
         /*
            1. öres sorok hossz†t null†ra reduk†ljuk.
            2. #line elìtti Åres sorokat tîrîljÅk. (BOS)
            3. EOS elìtti Åres sorokat tîrîljÅk.
         */
         if (TOKEN.t:id==TKID_UJSOR)
            // ój sor. Az Åres sorokat az emptyLines-ban t†roljuk.
            if (!empty(line))
               fwrite(outFid,emptyLines)
               emptyLines:=""
               fwrite(outFid,line)
               line:=""
               fwrite(outFid,TOKEN.t:getStr())
            else
               emptyLines+=TOKEN.t:getStr()
               line:=""
            endif
         elseif (TOKEN.t:id==TKID_EOS .or.;
                 TOKEN.t:id==TKID_BOS)
            // Az EOS Çs a BOS elìtti Åres sorokat tîrîlni kell.
            // Mj.: Itt nincs kezelve az az esetet, amikor nincs 
            //      sorvÇgjel az include filÇ vÇgÇn.
            if (!empty(line))
               fwrite(outFid,emptyLines)
               emptyLines:=""
               fwrite(outFid,line)
               line:=""
               fwrite(outFid,TOKEN.t:getStr())
            else
               emptyLines:=""
               line:=""
               // Ha az EOS/BOS °rni akar valamit, azt kitesszÅk.
               fwrite(outFid,TOKEN.t:getStr())
            endif
         else
            line+=TOKEN.t:getStr()
         endif   
      endif
      // Itt ki kell olvasni a hib†kat.
      cccpp_printErrorStream(errorStream)
   end while
   // Nem kell az utols¢ sorra figyelni, mert mindig jîn egy EOS.
   
   // ElvÇgezzÅk a szÅksÇges ellenìrzÇseket a filÇ vÇgÇn. 
   // (Lez†ratlan #if, etc)
   HPARSER.hpr:chkEndOfFile()
   cccpp_printErrorStream(errorStream)
return nil
#endif

//*******************************************************************

#endif

