# Harbour switch handling spec

This spec goes for `CLIPPERCMD`, `HARBOURCMD`, Harbour compiler and
`#pragma` directives in the source code.

The command-line always overrides the envvar.

Note that some switches are not accepted in envvar, some others in
`#pragma`s.

First the parser should start to step through all the tokens in the
string separated by whitespace. (or just walk through all `argv[]`)

1. If the token begins with `-`, it should be treated as a new style switch.

   One or more switch characters can follow this. The `-` sign inside the
   token will turn off the switch.

   If the switch has an argument all the following characters are treated
   as part of the argument.

   The `/` sign has no special meaning here.

   ```
   Switch                Resulting options

   -wn                   ( W N )
   -w-n                  ( !W N )
   -wi/harbour/include/  ( W I=/harbour/include/ )
   -wi/harbour/include/n ( W I=/harbour/include/n )
   -wes0n                ( W ES=0 N )
   -wen                  ( W [invalid switch: e] N )
   -wesn                 ( W ES=Default(0) N )
   -wses                 ( W S ES=Default(0) )
   -wess                 ( W ES=Default(0) S )
   -                     ( [invalid switch] )
   -w-n-p                ( !W !N P )
   -w-n-p-               ( !W !N !P )

   -w- -w -w-            ( finally: !W )
   ```

2. If the token begins with `/`, it should be treated as a compatibility
   style switch.

   The parser scans the token for the next `/` sign or `EOS` and treats the
   resulting string as one switch.

   This means that a switch with an argument containing `/` sign has some
   limitations. This may be solved by allowing the usage of quote characters.
   This is mostly a problem on systems which use `/` as path separator.

   The `-` sign has no special meaning here, it cannot be used to disable
   a switch.

   ```
   Switch               Resulting options

   /w/n                 ( W N )
   /wo/n                ( [invalid switch: wo] N )
   /ihello/world/       ( I=hello [invalid switch: world] [invalid switch: /] )
   /i"hello/world/"/w   ( I=hello/world/ W )
   /ihello\world\       ( I=hello\world\ )
   ```

3. If the token begins with anything else it should be skipped.

   The Harbour switches are always case insensitive.

   In the Harbour command-line the two style can be used together:
   ```
   harbour -wnes2 /gc0/q0 -iC:\hb\include
   ```

   Exceptions:

   - Handling of the `/CREDIT` undocumented switch on Harbour command-line
     is unusual, check the current code for this.

   - `CLIPPER`, `HARBOUR` envvars and Harbour application command-line
     parsing is a different beast, see `src/vm/cmdarg.c` for a NOTE.

     Just some examples for the various accepted forms:

     ```
     //F20 == /F20 == F20 == F:20 == F20X
     F20//F:30000000 NOIDLE
     F0NOIDLEX10
     SQUAWKNOIDLE
     ```

     `//` should always be used on the command-line.

---
Copyright (c) 1999-2009 Viktor Szakats (vszakats.net/harbour)
Licensed under Creative Commons Attribution-ShareAlike 4.0:
<https://creativecommons.org/licenses/by-sa/4.0/>
See LICENSE.txt
