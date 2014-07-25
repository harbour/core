/* This is an original work by Ted Means and is placed in the public domain.

      Rev 1.5   07 Jan 1993 08:01:04   GLENN
   We forgot to escape the less-than symbols in the HIGHBYTE() and
   LOWBYTE() #translates.

      Rev 1.4   31 Dec 1992 21:35:46   GLENN
   Some typos made it into the most recent version of ftint86,
   particularly within the HIGHBYTE() macro.  This has been fixed.

      Rev 1.3   01 Jul 1992 01:00:52   GLENN
   Rodgers Moore submitted some fixes to the HIGHBYTE() and LOWBYTE()
   macros that take negative numbers into account.  Ted Means and
   Glenn Scott added #defines for the Flag registers.  General cleanup
   of formatting, etc.


      Rev 1.2   15 Aug 1991 23:08:48   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   27 May 1991 13:25:18   GLENN
   Revised for new version of ft_int86() package, which is written in C
   (cint86.c), assembler (aint86.asm).

      Rev 1.0   01 Apr 1991 01:02:38   GLENN
   Nanforum Toolkit
 */

/* Header file for users of ft_int86() function, not supported in Harbour */
