# Contributing to this fork of Harbour

# Table of Content

1. [How to Donate](#how-to-donate)
2. [How to Share](#how-to-share)
3. [How to Participate](#how-to-participate)
4. [Troubleshooting](#troubleshooting)

---

# How to Donate

  You can donate to support the effort that goes into responding to issues.

  * [PayPal](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BPSZQYKXMQJYG)


# How to Share

  Share by **linking** to this project from your forum, blog or Twitter.
  Link to commits, issues or source files, or whatever else you find worthy
  to pass to someone. This is the easiest, most useful and self-updating
  way of referring to this project. It's also the best form of crediting
  and appreciating this work without donating or participating directly.

  Examples:

  * [commit](https://github.com/vszakats/harbour-core/commit/35ffdc113f348fbf10203757073bbee564e4339f)
  * [commit - specific file](https://github.com/vszakats/harbour-core/commit/35ffdc113f348fbf10203757073bbee564e4339f#diff-2)
  * [issue](https://github.com/vszakats/harbour-core/issues/18)
  * [issue comment](https://github.com/vszakats/harbour-core/issues/22#issuecomment-47928889)
  * [source file](tests/hello.prg)
  * [source file - specific revision](https://github.com/vszakats/harbour-core/blob/dd2c3b3e9c0e7db7f1c18be2a079dc92f983122e/tests/hello.prg) ([more information here](https://help.github.com/articles/getting-permanent-links-to-files))
  * [source line](https://github.com/vszakats/harbour-core/blob/dd2c3b3e9c0e7db7f1c18be2a079dc92f983122e/tests/hello.prg#L5)
  * [source section](https://github.com/vszakats/harbour-core/blob/90ce13098244b0e27fc4b8c7af5586f892a09337/src/rtl/chruni.c#L101-L112)
  * [source directory](contrib/hbcurl/)
  * [source - specific revision](https://github.com/vszakats/harbour-core/tree/e46f2fdb75c493ff5b4c777f5a0963d2e7d6f65b)
  * [source - specific revision .zip archive](https://github.com/vszakats/harbour-core/archive/e46f2fdb75c493ff5b4c777f5a0963d2e7d6f65b.zip)


# How to Participate

- You can give feedback/suggestions by submitting an [issue](https://github.com/vszakats/harbour-core/issues/new).
- Submit a patch:
  1. Fork the repository
  2. Create a branch: `git checkout -b my_mod`
  3. Do commit pre-check and new log entry: `hbrun bin/commit`
  4. Commit your changes: `git commit -am "Added my feature"`
  5. Push to the branch: `git push origin my_mod`
  6. Open a Pull Request
- Make sure to use the same coding/formatting style as you find in
  the files you're modifying. The easiest way to achieve this
  is to use these commands to format the sources (use this
  with care - most existing sources are well-formatted, so make
  sure to only apply it to newly added or modified code sections)

        $ uncrustify -c <harbour_dir>/bin/harbour.ucf <source(.c|.h)>
        $ <harbour_dir>/bin/hbformat <source(.prg|.hb|.ch)>

- Text editor setting for Harbour files
  - Encoding is either 7-bit ASCII or UTF-8 (without [BOM](https://en.wikipedia.org/wiki/Byte_order_mark))
  - Use spaces, never tabs
  - Remove trailing spaces from lines
  - Keep one (not zero or multiple) newline at the end of file
  - Use platform native newline (CRLF or LF)
- In the rare case you need to send something large (> 100KB),
  use this [free service](https://transfer.sh).
- See this good guideline on how to contribute:
  <https://github.com/necolas/issue-guidelines/blob/master/CONTRIBUTING.md>
- And these:
  <https://guides.github.com/overviews/os-contributing/>
  <https://github.com/blog/1943-how-to-write-the-perfect-pull-request>
- You can also participate in localization:<br />
  [![Localization Status](https://www.transifex.com/projects/p/harbour/resource/hbmk2-vszakats/chart/image_png)](https://www.transifex.com/projects/p/harbour/)

> Personal/private e-mails will either be ignored or given a short
> answer to move the inquiry to somewhere public.<br />
> Donators can count on my reciprocity even in private.


# Troubleshooting

Evaluate these points before reporting an issue:

1.  Make sure to have carefully read this document.
2.  Make sure to do a `make clean` before doing a build after refreshing
    the sources.
3.  If that still fails, make sure to install fresh source tree in a new
    local directory and start over. See [How to Get](README.md#how-to-get)
    for instructions to get the source.
    In case you installed Harbour into system locations (this used to be
    the case with some *nix users, albeit mostly completely unnecessarily
    or wrongly - f.e. for unstable versions), you will need to remember
    cleaning off Harbour from all of these locations, too.
    Hint: Never install unstable Harbour versions to system locations.
4.  If you are doing a cross-build, make sure to have rebuilt the native
    Harbour executables for your host platform. See `HB_HOST_BIN`
    build messages to find their location.
5.  Keep your `PATH` clean from old, mixed compiler tools or other Harbour
    versions when building Harbour. The surest way to achieve this is to
    leave only the C compiler directory in `PATH`:

        set PATH=C:\<c_compiler_bin_dir>

    > If you use Harbour official binary distro on Windows, even above is
    > unnecessary and not recommended.
6.  Remove all old, unnecessary environment variables (for both Harbour
    and C compiler) from your environment. Also remove any custom settings
    for your C compiler.
    Use only those documented in this file.
7.  Remove any Harbour build settings documented in [Build Options](README.md#build-options).
8.  Do no or only minor modifications at once to the examples
    included in [Build Examples](README.md#build-examples).
    If it doesn't work, fall back to documented examples _as is_.
9.  If everything fails and you are to report a build problem to Harbour
    developers, make sure to include your OS version/language/CPU architecture,
    Harbour revision, C compiler name/release and version, environment
    variables and verbose log output containing **both stderr and stdout in
    one combined stream** (use `make > log.txt 2>&1`). Enable verbose
    mode using `HB_BUILD_VERBOSE=yes`. Preferably, configure your tools
    to output English language messages.
    Complete log output is rarely necessary, but make sure to include
    the top of the output (lines starting with `!`) and the area where
    problematic behavior occurred _first_. Make sure to not only include
    a link failure or a make tool failure, as it's most of the time not
    enough information. Compress your log using zip if it is larger
    than 25kB (use the extension `.zip`).
    With these, you have much better chance to get useful or any response.
10. Do not alter the directory layout and files in Harbour and 3rd party
    packages and tools (including C compilers).
11. If you are to report a build problem with a Harbour application,
    all of the above points apply, plus make sure to use hbmk2 with
    the `-trace` command-line option and redirect its output to
    a file (see above how). Also include your full command-line and
    any referenced build script in your report.
    It's good idea to first remove all manual references to Harbour
    core components from makefiles and custom environment. F.e. it's
    commom mistake to add C compiler header and/or lib dirs, Harbour core
    header and/or lib dirs, built-in constants to makefiles or environment.
    No such thing is necessary as all of these are automatically handled
    by hbmk2. IOW start simple and don't be overbusy with *fine-tuning*
    your configuration. If you need to, the problem is most probably
    elsewhere. It's also good idea to try with latest Harbour revision
    or Harbour's mainline branch first.
12. If you are to report a problem with Harbour itself, provide
    self-contained, minimal source code example. Do not use xhb contrib
    library (including `hbcompat.ch`), nor any 3rd party Harbour libraries.
    The example shall reproduce the problem using the latest Harbour revision
    at the time of the report. Do not post links to executables and other
    binary files. If your source contains non-ASCII and non-UTF-8 national,
    accented, special chars, make sure to mark the codepage/encoding used
    and use `Chr()`/`hb_BCode()` calls to form the strings. UTF-8 is
    recommended. Notice that code examples are likely to be executed as
    hbrun scripts for testing, so it's a good idea to make them work this
    way.<br />
    Also make sure not to report multiple issues under one GitHub Issue.<br />
    * See more on self-contained examples:
      <http://sscce.org/>
    * See more on how to report issues in an effective and useful way:
      <http://www.chiark.greenend.org.uk/~sgtatham/bugs.html>
13. Please do not report warnings or bugs &ndash; with the exception of build
    _errors_ &ndash; in 3rd party component hosted inside the Harbour source tree.
    You can recognize these from their source path, which always contains
    a subdirectory named: `/3rd/`. Report these to the respective component's
    maintainers instead.
14. If your example or report contains human readable text, use
    English only.
15. If your example involves compatibility components, make sure to test
    it against original implementation (for example, test legacy Cl*pper
    core language elements against real CA-Cl*pper 5.2e or 5.3b, or hbct
    functions against CT3 library, etc)
    Notice that Harbour is Cl*pper Summer '87 compatible exactly as
    much as Cl*pper 5.2e/5.3b is, meaning: almost, but not completely.


---
This document Copyright &copy;&nbsp;2009&ndash;2015 Viktor Szakáts (vszakats.net/harbour)<br />
[![Creative Commons Attribution-ShareAlike 4.0](https://upload.wikimedia.org/wikipedia/commons/d/d0/CC-BY-SA_icon.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
