// test of hb_file function
function main

if hb_file("filexist.prg") == .t.
   qout("file exist test works")
else
   qout("file exist test fails")
endif

if hb_file("filxxist.prg") == .f.
   qout("file exist test works")
else
   qout("file exist test fails")
endif

return nil
