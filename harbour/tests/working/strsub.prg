
// Testing strings concat

function main()

  LOCAL a := "STRINGS "
  LOCAL b := "CONCAT"
  LOCAL c
  LOCAL i

  for i := 1 to 3
    a += " "
    c := a
    c -= b
    QOut( "[" + c + "]" )
  next

  QOut()
  __Accept( "Press a key to raise an error!" )
  QOut( a - i )

return nil
