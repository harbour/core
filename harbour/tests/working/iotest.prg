// Testing Harbour file io features

function Main()

   local h    := 0
   local cstr := " "
   local ntmp := 0

   h := FCreate( "test.txt")
   qout('create handle',h)

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   h := FOpen("test.txt")
   qout('open handle',h)
   qout()
   /* try to read what is there */
   do while .t.
      ntmp := FRead( h, @cstr, 1)
      if ntmp == 0
         exit
      endif
      qqout(cstr)
   enddo

   FClose( h )

return nil
