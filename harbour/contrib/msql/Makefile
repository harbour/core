CompOptions = -m -n -I../include

objects = test.o msql.o tmsql.o 

precomp = test.c msql.c tmsql.c

source = test.prg tmsql.prg

test:  $(objects)
	gcc -O2 -o test.exe $(objects) -I..\include -L..\lib -lrtl -lvm -lgtos2 -lrdd -llang -lmacro -lpp -ldbfntx -lcommon -lrtl -lvm -llibmsql -lsocket

$(objects): $(precomp)
	gcc -O2 -c $(precomp) -I..\include

$(precomp): $(source)
	..\bin\harbour $(CompOptions) $(source)

