// example from clipper reference guide

procedure main()
  local aArr1 := { "Tom", "Mary", "Sue" }
  local aArr2 := { "Tom", "Mary", "Sue", "Mary" }
  local aArr3 := {}
  local nLen, nStart := 1, nPos

  QOut( "Search 'Mary' in 'Tom, Mary, Sue' => " )
  QQOut( AScan( aArr1, "Mary" ) )
  QOut( "Search 'mary' in 'Tom, Mary, Sue' => " )
  QQOut( AScan( aArr1, "mary" ) )

  nLen := Len( aArr2 )
  do while ( nPos := AScan( aArr2, "Mary", nStart ) ) > 0
     QOut( "Search 'Mary' in 'Tom, Mary, Sue, Mary' from ", nStart, " => " )
     QQOut( nPos, aArr2[ nPos ] )
     nStart := nPos + 1
     if nStart > nLen
       exit
     endif
  enddo

  AAdd( aArr3, { "one",   "two"  } )
  AAdd( aArr3, { "three", "four" } )
  AAdd( aArr3, { "five",  "six"  } )
  QOut( AScan( aArr3, { | aVal | Dump( aVal ) } ) )

return

function Dump( aVal )

  QOut( ValType( aVal ) )

return
