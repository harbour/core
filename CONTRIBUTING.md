# Contributing to this fork of Harbour

# Table of Content

1. [How to Donate](#how-to-donate)
2. [How to Participate](#how-to-participate)
3. [Troubleshooting](#troubleshooting)

---

# How to Donate

  You can donate to support the effort that goes into responding to issues.

  * [PayPal](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BPSZQYKXMQJYG)
  * [Bitcoin](https://coinbase.com/checkouts/b90e7d8467c3d17f0083f9ad186c3c36)


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
  is to use these commands to format the sources:

        $ uncrustify -c <harbour_dir>/bin/harbour.ucf <source(.c|.h)>
        $ <harbour_dir>/bin/hbformat <source(.prg|.hb|.ch)>

- Text editor setting for Harbour files
  - Encoding is either 7-bit ASCII or UTF-8 (without [BOM](https://en.wikipedia.org/wiki/Byte_order_mark))
  - Use spaces, never tabs
  - Remove trailing spaces from lines
  - Keep one (not zero or multiple) newline at the end of file
  - Use platform native newline (CRLF or LF)
- See this good guideline on how to contribute:
  <https://github.com/necolas/issue-guidelines/blob/master/CONTRIBUTING.md>


# Troubleshooting

Evaluate these points before reporting an issue:

1.  Make sure to have carefully read this document.
2.  Make sure to do a ``make clean`` before doing a build after refreshing
    the sources.
3.  If that still fails, make sure to install fresh source tree in a new
    local directory and start over. See [How to Get](#how-to-get)
    for instructions to get the source.
    In case you installed Harbour into system locations (this used to be
    the case with some *nix users, albeit mostly completely unnecessarily
    or wrongly - f.e. for unstable versions), you will need to remember
    cleaning off Harbour from all of these locations, too.
    Hint: Never install unstable Harbour versions to system locations.
4.  If you are doing a cross-build, make sure to have rebuilt the native
    Harbour executables for your host platform. See `HB_HOST_BIN`
    build messages to find their location.
5.  Keep your PATH clean from old, mixed compiler tools or other Harbour
    versions when building Harbour. The surest way to achieve this is to
    leave only the C compiler directory in PATH:

        set PATH=C:\<c_compiler_bin_dir>

    > If you use Harbour official binary distro on Windows, even above is
    > unnecessary and not recommended.
6.  Remove all old, unnecessary environment variables (for both Harbour
    and C compiler) from your environment. Also remove any custom settings
    for your C compiler.
    Use only those documented in this file.
7.  Remove any Harbour build settings documented in [Build Options](#build-options).
8.  Do no or only small modifications at once to the examples
    included in [Build Examples](#build-examples).
    If it doesn't work, fall back to documented examples _as is_.
9.  If everything fails and you are to report a build problem to Harbour
    developers, make sure to include your OS version/language/CPU architecture,
    Harbour revision, C compiler name/release and version, environment
    variables and verbose log output containing **both stderr and stdout in
    one combined stream** (use `make > log.txt 2>&1`). Enable verbose
    mode using `HB_BUILD_VERBOSE=yes`.
    Complete log output is rarely necessary, but make sure to include
    the top of the output (lines starting with ``!``) and the area where
    problematic behavior occurred _first_. Make sure to not only include
    a link failure or a make tool failure, as it's most of the time not
    enough information. Compress your log using zip if it is larger
    than 25kB. (use the extension `.zip`)
    With these, you have much better chance to get useful or any response.
10. Do not alter the directory layout and files in Harbour and 3rd party
    packages.
11. If you are to report a build problem with a Harbour application,
    all of the above points apply, plus make sure to use ``-trace``
    command line option when running hbmk2 and redirect the result to
    a file (see above how).
    It's good idea to first remove all manual references to Harbour
    core components from makefiles and custom environment. F.e. it's
    commom mistake to add C compiler header and/or lib dirs, Harbour core
    header and/or lib dirs, built-in constants to makefiles or environment.
    No such thing is necessary as all of these are automatically handled
    by hbmk2. IOW start simple and don't be overbusy with *fine-tuning*
    your configuration. If you need to, the problem is most probably
    elsewhere. It's also good idea to try with Harbour nightly binary or
    official stable release first.
12. If you are to report a problem with Harbour itself, provide
    self-contained, minimal source code example. Do not use xhb contrib
    library, or any 3rd party Harbour libraries. The example shall reproduce
    the problem using official stable or nightly Harbour build.
    Do not post executables and other binary files. If your source contains
    non-ASCII (national, accented, special) chars, make sure to mark the
    codepage/encoding used (UTF-8 recommended) and attach the files
    compressed with zip. (use the extension `.zip`)<br />
    * See more on self-contained examples:
      <http://sscce.org/>
    * See more on how to report issues in an effective and useful way:
      <http://www.chiark.greenend.org.uk/~sgtatham/bugs.html>
13. If your example or report contains any human readable text, use
    English only.
14. If your example involves compatibility components, make sure to test
    it against original implementation (for example, test legacy Clipper
    core language elements against real CA-Clipper 5.2e or 5.3b, or hbct
    functions against CT3 library, etc)


---
This document Copyright &copy;&nbsp;2009&ndash;2014 Viktor Szakáts (vszakats.net/harbour)<br />
Licensed under [Creative Commons Attribution-ShareAlike 4.0](https://creativecommons.org/licenses/by-sa/4.0/)<br />
See [COPYING](COPYING.txt).
