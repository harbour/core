// test of file function
function main

if file("filexist.prg") == .t.
   qout("file exist test works")
else
   qout("file exist test fails")
endif

if file("filxxist.prg") == .f.
   qout("file exist test works")
else
   qout("file exist test fails")
endif

return nil
