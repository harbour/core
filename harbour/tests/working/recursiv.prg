// testing recursive calls

function main()

  QOut( "Testing recursive calls" + Chr( 13 ) + Chr( 10 ) )

  QOut(f(10))

  QOut( 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1 )

return nil

function f(a)
return iif(a<2,1,a*f(a-1))

