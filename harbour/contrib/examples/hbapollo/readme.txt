/*
 * $Id$
 */

 RDD for VistaSoftware's Apollo Database Engine
 (Server and/or Local)
 Patrick Mast <email@patrickmast.com>

 To build hbapollo.lib you need these files:
   - sde62.dll ( you need this to build sde62.lib )
   - sde62.lib ( build it using implib sde62.lib sde62.dll )
   - sde62.h ( SDE C/C++ header file for sde62.dll )

   Don't forget to include hbapollo.lib and sde62.lib to your
   the make file or link script to build executables.

 To use a application that uses hbapollo.lib you need this file:
   - sde62.dll  ( The Apollo Engine )
     The SDE is VistaSoftware's 32-bit database engine which contains
     the database technology that all Apollo products rely on to
     perform the low-level data management and manipulation.


 Be aware that this is a work in progress. Any comments are welcome.
 More to come! ;-)

 Patrick Mast
 December 2001
