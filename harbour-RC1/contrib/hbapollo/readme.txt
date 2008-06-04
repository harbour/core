#
# $Id$
#

 RDD for VistaSoftware's Apollo Database Engine
 (Server and/or Local)
 Patrick Mast <email@patrickmast.com>
 
 To build apollo.lib you need these files:
   - sde60.dll ( you need this to build sde60.lib )
   - sde60.lib ( build it using implib sde60.lib sde60.dll )
   - sde60.h ( SDE C/C++ header file for sde60.dll )

   Use make_b32.bat or make_vc.bat to build the Apollo.lib

   Don't forget to include Apollo.lib and sde60.lib to your
   the make file or link script to build executables. See the
   bld.bat in the contrib/apollo/test directory.

 To use a application that uses apollo.lib you need this file:
   - sde60.dll  ( The Apollo Engine )
     The SDE is VistaSoftware's 32-bit database engine which contains
     the database technology that all Apollo products rely on to
     perform the low-level data management and manipulation.


 Be aware that this is a work in progress. Any comments are welcome.
 More to come! ;-)

 Patrick Mast
 December 2001