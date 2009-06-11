/*
 * $Id$
 */

1.) Copy the full content of /SOURCE/DBU from your original
    CA-Cl*pper installation.

2.) *nix users will need convert original filenames to lowercase
    and EOLs to native format, using this command:
    hbformat -lFCaseLow=yes -nEol=0 -lIndent=no -lCase=no -lSpace=no *.prg

3.) Apply supplied patch to the source using GNU Patch:

    For CA-Cl*pper 5.2e sources:
       patch -N -i dbu52.dif

    For CA-Cl*pper 5.3b sources:
       patch -N -i dbu53.dif

4.) Build it:
    hbmk2 dbu.hbp

5.) You're done.

[vszakats]
