/* test of pass by reference @ */

function main
local a := 10

qout('a := 10',a)
testfun(@a)
qout('return of reference should = 20',a,iif(a == 20,"worked","failed"))

return nil

function testfun(b)
b := b + 10
qout('pointer+10 =',b)

return nil

