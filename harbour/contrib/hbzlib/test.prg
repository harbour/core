Function Main()
/*
HB_ZIPFILE('test.zip','zip.h',,{|cFile| qout(cFile)})
HB_ZIPFILE('test2.zip','zip.h')
*/
Hb_ZIPFILE('test12.zip',{'.\test.prg','.\zlib.h','.\zip.h','..\..\obj\b32\test.obj'},8,{|cfile| qout(cfile)})
/*
erase zip.h
? 'unzipping file'
hb_unzipfile('test.zip')
*/
hb_unzipfile('test12.zip',{|cFile| qout(cFile)})

return nil
