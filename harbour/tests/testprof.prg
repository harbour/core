// Testing the Harbour profiler

function Main()

   local lPrevProf := __SetProfiler( .t. )  // First of all, we activate the profiler
   local oGet  := GetNew()
   local oBrw  := TBrowseNew()
   local n

   for n = 1 to 20000
      oGet:row = 10
   next

   hb_Profiler( "profile.txt" )  // look for a generated profiler.txt file on your disk

return nil