1. Download original sources of SuperLib 3.5:
      <http://www.the-oasis.net/files/library/supfree.zip>
   or
      <https://web.archive.org/web/20071108113537/http://www.the-oasis.net/files/library/supfree.zip>

   sha256: `48114a59c8a9ebf51749f74e31dac39e12403180ae2920afb9f93d82794a8fc6`

2. Unpack it:

   ```
   unzip supfree.zip
   unzip SOURCE.ZIP
   ```

3. *nix users will need to convert original filenames to lowercase
   and EOLs to native format, using these commands:

   `hbmk2 -sanitize *.c *.prg`

4. Apply supplied patch to the source using GNU Patch:

   `patch -lNi superlib.dif`

5. Build it:

   `hbmk2 superlib.hbp`

6. You're done.

7. Finally, to build an application with SuperLib, use:

   `hbmk2 myapp.prg superlib.hbc`

[vszakats]
