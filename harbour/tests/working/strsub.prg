
// Testing strings concat

// Expected result:
//
// [STRINGSCONCAT  ]
// [STRINGSCONCAT   ]
// [STRINGSCONCAT    ]
// >AB<
// >AB  <
// >AB   <
// >A B  <
// >B    <

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

  QOut( ">" + "A" - "B"    + "<" ) // "AB"
  QOut( ">" + "A  " - "B"  + "<" ) // "AB  "
  QOut( ">" + "A  " - "B " + "<" ) // "AB   "
  QOut( ">" + "A  " - " B" + "<" ) // "A B  "
  QOut( ">" + "   " - "B " + "<" ) // "B    "

  QOut()
  __Accept( "Press a key to raise an error!" )
  QOut( a - i )

return nil
