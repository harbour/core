
/* test of instring $ operator */

function main
qout( "test of instring $ operator ")
qout('"d" $ "bcde"', "d" $ "bcde",.t.)
qout('"D" $ "BCDE"', "D" $ "BCDE",.t.)

qout('"a" $ "bcde"', "a" $ "bcde",.f.)
qout('"d" $ "BCDE"', "d" $ "BCDE",.f.)
qout('"D" $ "bcde"', "D" $ "bcde",.f.)

return nil

