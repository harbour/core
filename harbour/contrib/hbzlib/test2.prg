#define B_BOXLINES      "ÚÄ¿³ÙÄÀ³"
#include 'tzipfile.ch'
Function Main()
Local aArray,x,oZip

local aDir:=Directory("*.h")
local afiles:={}
local nLen
local aGauge
CLS
Create ZipFile  oZip file "test9658.zip" BLOCK {|cFile| qOut(cFile)} COMPRESS
For x:=1 to len(aDir)
   Add file adir[x,1] to oZip
//    aadd(afiles,adir[x,1])
next
Activate zip oZip
?
? "Number of Files in this Zipfile",oZip:GetNumberofFiles()
/*
nLen=len(afiles)
aGauge := GaugeNew( 5, 5, 7,40 , "W/B", "W+/B" ,'²')
GaugeDisplay( aGauge )                                            
Hb_ZIPFILE('test12.zip',afiles,8,{|cFile,nPos| GaugeUpdate(aGauge,nPos/nLen)})
Hb_ZIPFILE('test22.zip',afiles,8,{|nPos,cFile| qout(cFile)})

hb_zipfile('test22.zip',{'data/','data/hbdoc.obj','data/genhtm.obj'},8)
inkey(0)

local aGauge,nLen
aGauge := GaugeNew( 5, 5, 7,40 , "W/B", "W+/B" ,'²')
GaugeDisplay( aGauge )
x:= HB_GETUNZIPFILE('test22.zip')
? "number of files in zip",x
aArray:=HB_GETFILESINZIP('test22.zip')
for x:=1 to len(aArray)
 ? aArray[x]
 next
nlen:=HB_GETUNZIPFILE('test22.zip')
hb_UNZIPFILE('test22.zip',{|cFile,nPos| GaugeUpdate(aGauge,nPos/nLen),qout(cFile)},.T.)
*/
RETURN nil
