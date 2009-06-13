/*
 * $Id$
 */

REQUEST SQLMIX

PROC main()
   RDDSETDEFAULT("SQLMIX")
   DBCREATE("persons", {{"NAME", "C", 20, 0}, {"FAMILYNAME", "C", 20, 0}, {"BIRTH", "D", 8, 0}, {"AMOUNT", "N", 9, 2}},, .T., "persons")

   DBAPPEND();  AEVAL({PADR("Bil", 20),  PADR("Gatwick", 20),  STOD("19650124"), 123456.78}, {|X,Y| FIELDPUT(Y, X)})
   DBAPPEND();  AEVAL({PADR("Tom", 20),  PADR("Heathrow", 20), STOD("19870512"),   9086.54}, {|X,Y| FIELDPUT(Y, X)})
   DBAPPEND();  AEVAL({PADR("John", 20), PADR("Weber", 20),    STOD("19750306"),   2975.45}, {|X,Y| FIELDPUT(Y, X )})
   DBAPPEND();  AEVAL({PADR("Sim", 20),  PADR("Simsom", 20),   STOD("19930705"),  32975.37}, {|X,Y| FIELDPUT(Y, X )})

   DBGOTOP()
   BROWSE()

   INDEX ON FIELD->AMOUNT TO amount
   DBGOTOP()
   BROWSE()
   DBCLOSEALL()
RETURN


PROC RDDSYS();  RETURN
