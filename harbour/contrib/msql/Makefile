CompOptions = -m -n -I../include


msqlobjects = msql.o tmsql.o
msqlprecomp = msql.c tmsql.c
msqlsource = tmsql.prg

tobjects = test.o
tprecomp = test.c
tsource = test.prg

dobjects = dbf2msql.o
dprecomp = dbf2msql.c
dsource = dbf2msql.prg

$(msqlobjects):	$(msqlprecomp)
		gcc -O2 -c $(msqlprecomp) -I..\include
$(msqlprecomp):	$(msqlsource)
		..\bin\harbour $(CompOptions) $(msqlsource)


test:	$(tobjects) $(msqlobjects)
	gcc -O2 -o test.exe $(tobjects) $(msqlobjects) -I..\include -L..\lib -lrtl -lvm -lgtos2 -lrdd -llang -lmacro -lpp -ldbfntx -lcommon -lrtl -lvm -llibmsql -lsocket

$(tobjects):	$(tprecomp)
		gcc -O2 -c $(tprecomp) -I..\include
$(tprecomp):	$(tsource)
		..\bin\harbour $(CompOptions) $(tsource)


dbf2msql:	$(dobjects) $(msqlobjects)
		gcc -O2 -o dbf2msql.exe $(dobjects) $(msqlobjects) -I..\include -L..\lib -lrtl -lvm -lgtos2 -lrdd -llang -lmacro -lpp -ldbfntx -lcommon -lrtl -lvm -llibmsql -lsocket

$(dobjects):	$(dprecomp)
		gcc -O2 -c $(dprecomp) -I..\include
$(dprecomp):	$(dsource)
		..\bin\harbour $(CompOptions) $(dsource)

