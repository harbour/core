Telepathy is the best serial communication library for Nantucket/CA-Cl*pper.
I ported a significant amount of the functionality contained in Telepathy in
this Flagship port.

The .prg (in the MyTelepathy.tar package) is meant to be compiled with
Flagship and provide some of the functionality of the Telepathy serial
library from Extrasensory Inc.  I've talked to Ira Emus, the proprietor of
Extrasensory and he's ok with what I've done here.  But Extrasensory has no
involvement or responsibility with this project.

I wrote this originally on RedHat Linux 5.1 and I've tested it on RedHat
6.2.  And now Neolinux 2.0.1 and 2.1.  I've tested it on kernels 2.0.x - 2.4.x.
Your mileage may vary.

I only ported the parts of Telepathy that I needed.  If you need other parts
of the telepathy libarary, Use the Source... (and send me your new functions)

I tried to keep parameter counts and orders as consistent as possible between
this Telepathy and the real Telepathy.  The only difference I can think of is
the parameter list for tp_open (and tp_reopen).  That's just because unix has
filename comports like /dev/ttyS0 and dos has com port numbers like 1.  I kept
the com port number thing so that all the telepathy functions (and all your
code) that uses com port number won't need to be changed.  But you do need to
change the calls to tp_open.

Feel free to use, modify, distribute, hack, kludge, append, extend or whatever
you like to this source code.  You can (at your own risk) compile it into any
program you wish and sell or freely distribute the resulting application.  I
would prefer you make your application Free and distribute the source.  But who
am I to say - the suite of applications that my company sells that I originally
wrote this for is closed source.

Though I use this code in a production environment for a suite of programs that
my company actually sells, I make no promises (expressed or implied) about its
stability, functionality, effectiveness or anything else.  I warn you now
that if you use my code, you're doing so at your own risk and it will probably
blow up your computer.  But that's a risk you'll just have to take.

There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE (thanks RMS).
This software is totally as is, you may use it at your own risk.

This is freeware.  But if you actually use this in your program, please send
me email at dan@boba-fett.net.  I'd just be curious to see if anyone ever
uses it.

If you find a bug or write some new code for this project, pretty please send
me email with it.

- Dan Levitt <dan@boba-fett.net>
