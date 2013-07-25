                    Simple SQL Interface for Harbour



1. Introduction

   Simple SQL interface implements accessing SQL query result via RDD
interface. It is not intended to be replacement for "transparent" move of
DBFCDX application to SQL world.

   I want to discuss this in more detail. Many current RDDs for SQL servers
(ex. SQLRDD from xHarbour.com) tries to make a feeling you are working with
DBF file, but not with SQL database. SQL server does not support many
features, ex. RecNo(), deleted flag, file locks, record locks. These RDDs
are emulating these features to make feeling of DBF. Deleted() function is
emulated by creating additional table columns to store delete flag. Some
"hidden system" tables are used to register locking operations and emulate
record and file locks in DBF style. The idea of SQL query is also lost. If
you do a simple loop

 dbUseArea(, "select * from my_table" )
 DO WHILE ! Eof()
    somefunc( FIELD->some_sql_field )
    dbSkip()
 ENDDO

RDD usualy will read SQL rows in portions, let's say 100 records per query.
So, hidden queries are generated. If you are using indexes these queries
are really complicated. Let's have index on FIELD1 + Str(FIELD2). A seek to
value cValue1 + Str(nValue2) will generate a query like:

 SELECT * FROM my_table
     WHERE (FIELD1 == cValue1 and FIELD2 >= nValue2) or FIELD1 > cValue1
     ORDER BY FIELD1, FIELD2, _RECNO
     LIMIT 100

After evaluation of first 100 cached records, next query will be generated:

 SELECT * FROM my_table
     WHERE (FIELD1 == cLastField1 and FIELD2 == nLastValue2 and _RECNO > nLastRecno) or
           (FIELD1 == cLastField1 and FIELD2 > nLastValue2) or
           FIELD1 > cLastValue1
     ORDER BY FIELD1, FIELD2, _RECNO
     LIMIT 100

To optimize these queries the SQL index expresion should be
"FIELD1,FIELD2,_RECNO", but not "FIELD1,FIELD2" as written in INDEX ON
command.

   "Simple SQL interface" is too long to repeat every time I want to
address this library. I'll also use acronym "SSI" to address it.

   The idea of SSI is different. It does not make hidden queries. All
queries should be made explicitly by programmer. SSI gives access to query
result via RDD interface, it does not tries to emulate DBF and be
"plug-and-play" solution for DBF to SQL migration. If you do

 dbUseArea(, "select * from my_table")

all query (it could contain millions of records!) will be cached.

   The features of SSI approach are:

- It's possible to access SQL database of other applications. Other
 applications usualy does not follow agreement of "plug-and-play" SQL drivers
 about additional DELETED column, _RECNO in the end of index expression, etc.
 Access of SQL database of other applications is sometimes not possible.

- It's query oriented. That means a simple DO WHILE ! Eof() loop will iterate
 each records once and only once. This is not true for "plug-and-play" SQL
 drivers, if indexing is used. Just like in the case of loop over DBF file.
 It is not guaranteed that all records are included! Yes! If key value of the
 first record in index is changed to be the last record in index during the
 phase of record processing, DO WHILE ! Eof() loop will iterate only this
 single records even if the database contains millions of records. Your sould
 do FLock() on DBF to guarantee the records are not changed. Do you use FLock()
 before readonly DO WHILE ! Eof() loops? :)



2. Architecture


             +-------------+
             |             |
             | SQLMIX RDD  |
             |             |
             +-------------+
                  |  ^
                  V  |
             +-------------+    +---------+
             |             |--->|         |
             | SQLBASE RDD |    |   SDD   |
             |             |<---|         |
             +-------------+    +---------+


   SQLBASE RDD implements basic functionality for accessing SQL query result
via RDD interface. This RDD could be used, if indexing of query result is not
necessary or all indexing is done by SQL server (by using ORDER BY clause).

   SQLMIX RDD implements indexing of query result. This indexing is not
related to SQL server ORDER BY clause. SQLMIX do indexing of the query on the
client side.

   SDD is acronym for Sql Database Driver. RDD is used to implement access
of different database formats like DBF, SDF, etc. SDD is used to implement
access of different SQL databases. Every SQL server (MySQL, PostgreSQL, etc.)
has a corresponding SDD. SDD driver implements a specific part of data
exchange interface between SQLBASE and SQL server.



3. Modifying database

   SSI presents a query result via RDD interface and generates no hidden
SQL queries. So, how database can be changed? Does dbAppend() and FieldPut()
works, or is it readonly SQL interface?
   dbAppend(), FieldPut() and other similiar functions work on cached query
result, i.e. query can be appended by new rows and field values can be
changed, but SQL database is not changed. dbCreate() function can also be
used to create an "empty query result" but no table is created on SQL server.
So, SSI can also be used as implementation of "array RDD".
   The programmer must call SQL command explicitly to modify SQL tables.
SSI provides a method to detect which cached rows was changed or appended.
