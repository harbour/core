/*
 * $Id$
 */

28/may/2000 Harbour mSQL access classes - readme file



This is work in progress, so it has to be fully tested and needs a few more methods to cover mSQL 2.x possibilities.


This set of files gives you a mean to access an mSQL server, I've developed and tested them on a OS/2 platform, so changes to Makefile and import library for different platforms are not present.

In its present state mSQL classes are made up of these files:

msql.c:		low level wrapper around msql client API. It requires libmsql.a (or libmsql.lib if under
		windows) import library (under OS/2 you need, also, msql.dll client api library if it's
		not in your LIBPATH).
msql.h:		from mSQL 2.x distribution, type and defines of mSQL client api.
msql.ch:	clipper level defines of mSQL types
tmsql.prg:	mSQL access classes
test.prg:	a little test program which wont work for you :-) since it uses a .dbf file not
		provided. Use it as a small tutorial of tmsql.prg provided functions.
Makefile	my makefile for OS/2 gcc, you'll surely need to change it to adapt to your needs/platform.


tmsql.prg defines four classes:

TmSQLServer:	manages access to a mSQL server and returns an oServer object to which you'll send all your
		queries;
TmSQLQuery:	a standard query to an oServer with joins. Every query has a GetRow() method
		which on every call returns a TmSQLRow object which, in turn, contains requested fields.
		Query objects convert mSQL answer (which is an array of strings) to clipper level types.
		At present time N (with decimals), L, D, and C clipper types are supported.
TmSQLTable:	It's a descendant of a TmSQLQuery and you'll receive it when your query has no joins.
		It adds Update(), Append() and Delete() methods which receive a TmSQLRow object and
		reflect changes to the mSQL table from which they come.
		Please note that TmSQLQuery objects don't have these methods, so, if you want to change
		a row received from a TmSQLQuery object you need to construct a valid SQL query and submit
		it to an oServer object.
TmSQLRow:	Every row returned by a SELECT is converted to a TmSQLRow object. This object handles
		fields and has methods to access fields given a field name or position.

I'm aware that this brief document doesn't explain a lot about mSQL access classes and I'm sorry for that.
I'll try to update it as work on these classes goes by and I'll like to receive feedbak and suggestions
from users (if any :-))

Excuse my poor english and happy selecting :-)

Maurilio Longo - maurilio.longo@libero.it			

