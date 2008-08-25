/*
 * $Id$
 */

README 23/12/2003 - Harbour Low Level api for Postgres RDBMS

It's to be seem like Harbour TMysql routines.


IMPORTANT
---------
The class TPostgres, only works with versions 7.4.x or greater, because some methods use information_schema and
New PQexecParams, which use protocol 3.0 and this protocol is only compatible with 7.4 versions.

BORLAND BCC
-----------
Using this library with bcc, you will need import library, ex: implib libpq.lib libpq.dll or (advice this) build your own
Postgres Library.

Go to postgres directory \postgresql-7.4.5\src\interfaces\libpq

Edit bcc32.mak and change the default borland directory for your needs. Example: BCB=C:\borland\bcc55

change postgresql-7.4.5\src\include\pg_config.h.win32 to pg_config.h

after this

make -f bcc32.mak

Now look at \postgresql-7.4.5\src\interfaces\libpq\release, you will find the all .libs and .dlls

If want use the dll, link in your aplication blibpqdll.lib and send blibpq.dll together with your aplication

If you don't wanna use .dll link blibpq.lib, that's it.

LINUX GCC
----------
On Linux you will need link libpq or references by pq.
For full api documentation look at:
http://www.postgresql.org/docs/current/static/libpq.html

FILES:
postgres.c - Low level api
tpostgre.prg - Class implementation, it's to be seems like TMysql.
tests\simple.prg - Simple test class
tests\stress.prg - Stress test 
tests\cache.prg  - Show hot to use .dbf as pg cache, like TDataset for Delphi.

TODO:

That's all folks and sorry my poor english

Rodrigo Moreno - rodrigo_moreno@yahoo.com
