See component details in readme.txt of sddmy contrib.


                +---------------------------------------------------------+
                | 2001-11-22 - Harbour MySQL access classes - readme file |
                +---------------------------------------------------------+


This is work in progress, so it has to be fully tested and needs a few more methods to cover MySQL possibilities.


Add hbmysql.hbc to your hbmk2 command line (you can also try to
recompile programs in utils and tests subdirs just to test everything)
and be sure to have MySQL dynamic libraries configured.


                              +---------------------+
                              |  Available Classes  |
                              +---------------------+


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

Excuse my poor English and happy selecting :-)

Maurilio Longo - <maurilio.longo@libero.it>
