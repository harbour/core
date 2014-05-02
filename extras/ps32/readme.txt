1.) Download PageScript32 package and unzip it:
    http://www.abeelabs.com/downloads/PS32.zip

2.) Copy these files from PageScript32 installation
    to this folder:
       PScript.ch
       TPSCRIPT.PRG

    You may also want to copy this file to 'tests' subdir:
       PSTest.prg

3.) Apply supplied patch to the source using GNU Patch:
    patch -lNi ps32.dif

4.) Build it:
    hbmk2 ps32.hbp

5.) You're done.

6.) Finally, to build an application with PageScript32, use:
    hbmk2 myapp.prg ps32.hbc

[vszakats]
