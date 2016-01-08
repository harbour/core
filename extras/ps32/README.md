1. Download PageScript32 package and unzip it:

   <https://web.archive.org/web/20150201213531/http://abeelabs.com/downloads/PS32.zip>

   sha256: `c9a9fb6d81054468d2884a913275184a318be8a5c830bc901376ae88e720849d`

2. Copy these files from PageScript32 installation
   to this directory:

      `PScript.ch`
      `TPSCRIPT.PRG`

   You may also want to copy this file to the `tests` subdirectory:

      `PSTest.prg`

3. Apply supplied patch to the source using GNU Patch:

   `patch -lNi ps32.dif`

4. Build it:

   `hbmk2 ps32.hbp`

5. You're done.

6. Finally, to build an application with PageScript32, use:

   `hbmk2 myapp.prg ps32.hbc`

[vszakats]
