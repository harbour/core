/*
 * $Id$
 */

                +---------------------------------------------------------------+
                | 2001/November/22 - Harbour MySQL access classes - readme file |
                +---------------------------------------------------------------+


This is work in progress, so it has to be fully tested and needs a few more methods to cover MySQL possibilities.


This set of files gives you a mean to access a MySQL server, I've developed and tested them on a OS/2 platform,
In their present state MySQL classes are made up of these files:

mysql.c     :  low level wrapper around MySQL client API.
mysql.ch    :  clipper level defines of MySQL types
tmysql.prg  :  MySQL access classes
test.prg    :  a little test program which wont work for you :-) since it uses a .dbf file not
               provided. Use it as a small tutorial of tmysql.prg provided functions.


1) See INSTALL on how to obtain/install and configure Harbour build
   system for mysql.

2) Add hbmysql.hbc to your hbmk2 command line (you can also try to
   recompile programs in utils and tests subdirs just to test everything)
   and be sure to have MySQL dynamic libraries configured.


                              +---------------------+
                              |  Available Classes  |
                              +---------------------+


tmysql.prg defines four classes:

TMySQLServer:  manages access to a MySQL server and returns an oServer object to which you'll send all your
               queries;
TMySQLQuery:   a standard query to an oServer with joins. Every query has a GetRow() method
               which on every call returns a TMySQLRow object which, in turn, contains requested fields.
               Query objects convert MySQL answers (which is an array of strings) to clipper level types.
               At present time N (with decimals), L, D, and C clipper types are supported.
TMySQLTable:   It's a descendant of a TMySQLQuery and you'll receive it when your query has no joins.
               It adds Update(), Append() and Delete() methods which receive a TMySQLRow object and
               reflect changes to the MySQL table from which they come.
               Please note that TMySQLQuery objects don't have these methods, so, if you want to change
               a row received from a TMySQLQuery object you need to construct a valid SQL query and submit
               it to an oServer object.
TMySQLRow:     Every row returned by a SELECT is converted to a TMySQLRow object. This object handles
               fields and has methods to access fields given a field name or position.


I'm aware that this brief document doesn't explain a lot about MySQL access classes and I'm sorry for that
(please read the souce code, it has quite a few comments which can help you understand what's going on)

I'll try to update it as work on these classes goes by and I'll like to receive feedback and suggestions
from users (if any :-))

Excuse my poor english and happy selecting :-)

Maurilio Longo - <maurilio.longo@libero.it>
