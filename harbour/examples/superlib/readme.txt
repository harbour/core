/*
 * $Id$
 */

1.) Download original sources of SuperLib 3.5 from this link and unpack it:
    http://www.the-oasis.net/ftpmaster.php3?content=ftplib.htm

2.) Unzip SOURCE.ZIP to this directory.

3.) *nix users will need convert original filenames to lowercase
    and EOLs to native format (f.e. with dos2unix).

4.) Apply supplied patch to the source using GNU Patch:
    patch -N -i hbsuper.dif

5.) Build it:
    hbmk2 hbsuper.hbp

6.) You're done.

[vszakats]
