Function Main()
HB_ZIPFILE('test.zip','zip.h')
Hb_ZIPFILE('test1.zip',{'.\test.prg','.\zlib.h','.\zip.h','..\..\obj\b32\test.obj'})
return nil
