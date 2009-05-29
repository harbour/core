/*
 * $Id$
 */

/*
 *------------------------------------------------------------------------
 *                  HARBOUR INTERFACE for SQLITE  
 *------------------------------------------------------------------------
 *
 * Copyright 2003 Alejandro de Garate <alex_degarate@hotmail.com>
 *
 * License: General Public License or for short GPL (GNU)
 * 
 * Developed using:
 *     Harbour 0.42 or upper 
 *     Borland C++ BCC 5.5.1 
 *
 */

 Requirements
 ------------
 Any windows platform W95, W98, W98SE or NT family 
 A C++ compiler, if you use other than Borland some minor changes could
 be required. 


 The program
 -----------
 The source program is quite small (about 50 Kbytes) and a litle basic, 
 but gives an easy way of accessing a SQL Database, like SQLite.
 Inside the prg you can find several C functions used to connect to the 
 database, and Clipper/Harbour code used for calling C functions.
 Most of the code is commented and is easy to follow.
 Of course many enhancement would be done, I will do if I found spare 
 time :)
 The library that come with the program it's based in the SQLite version
 2.8.6


 Purpose
 -------
 To access a sqlite database using Clipper/Harbour language.


 Use
 ---
 This program is mostly self-contained.
 For using you needs:
  1- Borland C++ Compiler BCC 5.5 or upper   (working)
  2- Harbour version 0.42 or upper  (working)
  3- hbsqlite.prg (main program /front end)
  4- hbsqlite.ch  (some defines)
  5- sqlite.lib   (library builded for BCC55 and windows platform)
  6- sqlite.h     (include file for sqlite code)
  7- sqliteInt.h  (include file for sqlite code)
  8- example.db   (a simple sqlite database for testings )

 Steps:
 1.- Unzip the package to the target directory (ie.: \sqlite)

 2.- Keep the zip file at safe place :)

 3.- Add sqlite.lib to the list of libraries 
     You also needs to add sqlite.lib to the list of libraries inside 
     the build.bat file at harbour\bin directory.

 4.- Move the static library sqlite.lib to the harbour\lib directory.

 5.- If you have (1) & (2) working, just compile hbsqlite.prg using:
      bld_b32 hbsqlite   // without extension


 Information about SQLITE
 ------------------------
 1. web site at www.sqlite.org

 2. User list for sqlite (do not ask about harbour stuffs) 
    subscribe at:
 <sqlite-users-subscribe@sqlite.org>         (one by one)

 <sqlite-users-digest-subscribe@sqlite.org>  (digest mode)


 What is inside the source package ?
 -----------------------------------
 Inside the hbsqlite_src.zip file you will find:

    File               Size     Description
 --------------------------------------------------------------------------
 1- hbsqlite.prg       46.401   (main program /front end)
 2- hbsqlite.ch         3.369   (some defines)
 3- sqlite.lib        352.256   (lib builded for BCC55 and windows platform)
 4- sqlite.h           31.222   (include file for sqlite code)
 5- sqliteInt.h        53.893   (include file for sqlite code)
 6- gpl.txt            17.989   (a copy of GPL license)
 7- todo.txt                    (a list of pending things )
 8- readme_en.txt               (this readme)

 Note:
 You may want also the sqlite manager and the sample database, in that 
 case you must download the binary package.


 What is inside the binary package ?
 -----------------------------------
 Inside the hbsqlite_bin.zip file you will find:

    File               Size     Description
 --------------------------------------------------------------------------
 1- hbsqlite.exe      741.376   (harbour front end to sqlite)
 2- sqlite.exe        285.444   (a comand-line Sqlite manager) 
 3- example.db         77.824   (a simple sqlite database for testings)
 4- test.db                     (another sqlite database for testings)
 5- readme_en.txt               (this readme)

 Final comments
 --------------
 You can adapt the program to your needs, but if you made any 
 improvements, fixes or found any error, let me know so I can add it
 to the program.
 Anyway if you want to ask something, or think a feature is missing
 send me a mail.

 Enjoy it!
 
 Alejandro

