// Testing the Harbour profiler

function Main()

   local oGet := GetNew()
   local oBrw := TBrowseNew()
   local n

   for n = 1 to 20000
      oGet:row = 10
   next

   Profiler()  // look for a generated profiler.txt file on your disk

return nil