DOS Serial Library
==================

A serial port (UART) library for DOS written back in the days when 16-bit
systems were still a going concern and far pointers were a thing.

I'm putting it on Github because I still think it's way cool, and who knows...
Perhaps someone, somewhere, is in dire need of a serial library for DOS (I've
needed it from time to time to deal with some DOS based embedded piece of crap).

Supports simultaneous communication over up to 4 serial ports.

Have a look at term.c for an example of how to use it.



How do I build it?
==================

Good question! It was built to compile under Borland C++ 3 and whatever
Microsoft compiler happened to be current at the time. It conforms to ANSI C
(1989) and it doesn't do anything too crazy, so it shouldn't be too bad in a
more modern compiler. Have a go and let me know how it works!



License
=======

Copyright 1998 Karl Stenerud

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
