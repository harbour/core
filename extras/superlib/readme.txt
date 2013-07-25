1.) Download original sources of SuperLib 3.5 from this page:
       http://www.the-oasis.net/ftpmaster.php3?content=ftplib.htm
    Direct link:
       http://www.the-oasis.net/files/library/supfree.zip

2.) Unpack it:
    unzip supfree.zip
    unzip SOURCE.ZIP

3.) *nix users will need to convert original filenames to lowercase
    and EOLs to native format, using these commands:
    hbformat -lFCaseLow=yes -nEol=0 -lIndent=no -lCase=no -lSpaces=no "*.c"
    hbformat -lFCaseLow=yes -nEol=0 -lIndent=no -lCase=no -lSpaces=no "*.prg"

4.) Apply supplied patch to the source using GNU Patch:
    patch -lNi superlib.dif

5.) Build it:
    hbmk2 superlib.hbp

6.) You're done.

7.) Finally, to build an application with SuperLib, use:
    hbmk2 myapp.prg superlib.hbc

[vszakats]
