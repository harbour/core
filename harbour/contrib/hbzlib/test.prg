#define B_BOXLINES      "ÚÄ¿³ÙÄÀ³"
Function Main()
local aDir:=Directory("*.h")
local afiles:={}
Local x
local nLen
local aGauge
CLS
For x:=1 to len(aDir)

    aadd(afiles,adir[x,1])

next
nLen=len(afiles)
aGauge := GaugeNew( 5, 5, 7,40 , "W/B", "W+/B" ,'²')
GaugeDisplay( aGauge )                                            
Hb_ZIPFILE('test12.zip',afiles,8,{|nPos,cFile| GaugeUpdate(aGauge,nPos/nLen)})
Hb_ZIPFILE('test22.zip',afiles,8,{|nPos,cFile| qout(cFile)})
return nil
