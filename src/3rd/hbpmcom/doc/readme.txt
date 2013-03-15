readme
------



			   COMLib, version 1.0

		     RS-232 communication library

			  started: May/1998

	Program and Text Copyright, ( C ) 1998, Peter Marinov
			 All Rights Reserved.



1. Legal aspects
----------------

This library is freeware software. You are free to use, modify or
redistribute this software or your software using this library.


2. Features
-----------

-- Interrupt driven.
-- Hardware FIFOs supported.
-- Support for multiple simultaneously opened COM ports.
-- User call-back event handling functions.
-- Support for 9 bit protocols.
-- Rich set of API functions.
-- INFO style documentation with examples.
-- DJGPP and Borland C single source support.
-- Independent IRQs serving layer which can be used in different projects.


3. How to compile using DJGPP
-----------------------------

Unpack pmcom10.zip by using any zip archive maintainer you have,
there's directory structure in the archive that is to be preserved, so
using pkunzip provide '-d' switch, using Info-Zip's unzip will extract
automaticaly the directories.

Type 'make' to build the release version of the library (using
'makefile'). To take advantage of the build in library internal checks
you are encouraged to build and use the debug version while in
developing stage of your software: 'make _DEBUG=1'. There
are some additional definitions for fine tuning the library:

DISABLE_TIMING=1 -- will disable the library to use the system timer
   and this will disable all the xxxTimed() functions from operation as well.
DISABLE_PREEMPTING=1 -- will turn off the ability to be preempted the
   process of handling COM IRQs, useful in multy-thread	environment.

To build the help type 'makeinfo --no-split com.txi'. You can use
the help by typing 'info -f com.inf'. To add com.inf in the main info
directory you should edit \djgpp\info\dir file. Your file may look
like this:

>   -- PLEASE ADD DOCUMENTATION TO THIS TREE. (See INFO topic first.) --
>
>* Menu:
>
>* Knowledge Base: (kb.inf).
>        The DJGPP Knowledge Base

Change it to look like this:

>   -- PLEASE ADD DOCUMENTATION TO THIS TREE. (See INFO topic first.) --
>
>* Menu:
>
>* COM: (com.inf).
>        Serial Interface Library
>
>* Knowledge Base: (kb.inf).
>        The DJGPP Knowledge Base

Then copy com.inf to \djgpp\info. This will make the help accessible from
RHIDE as well. After starting RHIDE press F1 and then Ctrl+D. The
DJGPP info directory will appear containing com.inf as well.

If you'd like to print a copy of the documentation you have to build
plain text documentation which is usefull for simple browsing as well
- type 'makeinfo --no-split --no-headers -o com.doc com.txi'.


4. How to compile using BorlandC v3.1
-------------------------------------

Make a new directory (I suggest COM) and make this directory current.
Unpack pmcom10.zip by using any zip archive maintainer you have,
there's no directory structure in the archive to be preserved. Create
two sub-directories 'dbg' and 'rel'. If using Info-Zip's unzip provide
'-j' option while extracting. (Directory structure in the archive is
only for the GNU installation of the bundle)

Type 'make -f com_bc'. This will result in building release version of
the library. But you are strongly encouraged to build and use the
debug version of the library while you are in developing stage of your
software by typing 'make -f com.bc -D_DEBUG' to compile. In general to
build variant that suits you best provide some or all of the following
definitions while invoking make. The definitions are valid all across
debug and release versions.

-D_DEBUG -- to build debug version. Contains strict internal checks of
  the various functions parameters and internal routines functionality.
-DDISABLE_TIMING -- will disable the library to use the system timer
  and this will disable all the xxxTimed() functions from operation as
  well. If you intent yo use the system timer for you own needs this
  option will disable interception of the timer IRQ (IRQ0).
-DDISABLE_PREEMPTING -- will turn off the ability to be preempted the
  process of handling COM IRQs. If you intent to use the library along
  with a preemtive multi-thread library, use this define to allow
  smooth cooperative work of the modules.
-DP386 -- will turn on '-3' option of the compiler and will push
  all the 32bit registers in the IRQ wrappers. This is work around for
  -3 option that forces BC to emit 386 functions but defining
  'interrupt' functions don't save the 32-bit registers in the
  stack. Use this option if your project is 386 based.

As a result of compilation in dbg or rel directories will reside
com.lib that should be added to your project.

If you are only BC user you won't have access to hiper-text browser
programs that DJGPP provides -- console oriented Info or RHIDE's
built-in help viewer. Write me and I'll send you com.doc that is a
plain version of the hiper-text documentation. You will be able to use
any ASCII text editor to view or print the documentation.


5. How to contact me
--------------------

Please don't expect that I will reply your djgpp news group postings as
I'm checking news archives on a fairly irregular basis.	What I suggest
is to be contacted directly through my e-mail address.

mail: mar22@usa.net

