/*
 * $Id$
 */

//*******************************************************************
// cccpp_pr.prg: Process input
// 1999, Csisz†r Levente
//*******************************************************************


//*******************************************************************
#include "debug.ch"
#include "ctoken.ch"

//*******************************************************************
#include "objgen.ch"

//*******************************************************************
#include "lreader.och"
#include "nparser.och"
#include "lparser.och"
#include "incl.och"
#include "hparser.och"
#include "mcontrol.och"
#include "parser.och"
#include "token.och"


//*******************************************************************
static function cccpp_printErrorStream(errorStream)
   if (!empty(errorStream))
      evalErrorStream(errorStream,{|x| outerr(x,newline())})
      asize(errorStream,0)
      return .t.
   endif
return .f.

//*******************************************************************
function cccpp_processReader(reader,outFid,;
                             incArray,maxInclDeep,;
                             defDict,xtrdict,errorStream,;
                             trPrsAlg)
// Feldolgoz egy filÇt, Egyenlìre nem vÇgez hiba ellezìrzÇst.

local lrd,npr,lpr,hpr,mcr,t,inclObj
local line,emptyLines,hiba
                          
   hiba:=.f.

   if (cccpp_printErrorStream(errorStream))
      hiba:=.t.
      return hiba
   endif

   // fr:=C.FREADER:onew(errorStream)
   // FREADER.fr:open("",inputFileName)
   lrd:=C.LREADER:onew(CTK_BOS,CTK_EOS,READER.reader:name,errorStream)
   LREADER.lrd:pushReader(reader)
   npr:=C.NPARSER:onew(lrd,READER.lrd:name,errorStream)
   lpr:=C.LPARSER:onew(npr,READER.npr:name,errorStream)
   inclObj:=C.INCL:onew(lrd,incArray,maxInclDeep)
   // inclObj:=C.INCL:onew(lrd,{dirFName(filename)})
   hpr:=C.HPARSER:onew(lpr,READER.lpr:name,;
                       defDict/*C.DEFDICT:onew()*/,xtrDict/*C.XTRDICT:onew()*/,;
                       inclObj,errorStream)
   mcr:=C.MCONTROL:onew(hpr,READER.hpr:name,;
                        HPARSER.hpr:defDict,HPARSER.hpr:xtrdict,;
                        errorStream,trPrsAlg)

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
      if (cccpp_printErrorStream(errorStream))
         hiba:=.t.
      endif
   end while
   // Nem kell az utols¢ sorra figyelni, mert mindig jîn egy EOS.
   
   // ElvÇgezzÅk a szÅksÇges ellenìrzÇseket a filÇ vÇgÇn. 
   // (Lez†ratlan #if, etc)
   HPARSER.hpr:chkEndOfFile()
   if (cccpp_printErrorStream(errorStream))
      hiba:=.t.
   endif
   
return hiba

//*******************************************************************

