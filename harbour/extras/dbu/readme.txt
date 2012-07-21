/*
 * $Id$
 */

1.) Copy the full content of /SOURCE/DBU from your original
    CA-Cl*pper installation to this directory.

2.) *nix users will need to convert original filenames to lowercase
    and EOLs to native format, using this command:
    hbformat -lFCaseLow=yes -nEol=0 -lIndent=no -lCase=no -lSpaces=no "*.prg"

3.) Apply supplied patch to the source using GNU Patch:

    For CA-Cl*pper 5.2e sources:
       patch -lNi dbu52.dif

    For CA-Cl*pper 5.3b sources:
       patch -lNi dbu53.dif

4.) Build it:
    hbmk2 dbu.hbp

5.) You're done.

[vszakats]
