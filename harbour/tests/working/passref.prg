/* test of pass by reference @ */

function main
local a := 10
local b := "X"

qout('a := 10',a)
qout('b := "X"',b)

testfun(@a, @b)
qout('return of "a" should = 20',a,iif(a == 20,"worked","failed"))
qout('return of "b" should = A',b,iif(b == "A","worked","failed"))

return nil

function testfun(b,c)
b := b + 10
c := "A"
qout('a pointer+10 =',b)
qout('b pointer := "A" =',c)

return nil

