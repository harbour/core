/*
 * $Id$
 */

/*
 * File......: ORIGIN.C
 * Author....: Steve Larsen
 * CIS ID....: 76370,1532
 *
 * This is an original work by K. Stephan Larsen and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   09 Nov 1992 22:35:52   GLENN
 * Function was inadvertently named origin() instead of ft_origin() when
 * it went from an .asm to a .c file.  Renamed it back to ft_origin().
 *
 *    Rev 1.0   03 Oct 1992 02:13:54   GLENN
 * Initial revision.
 *
 */

/* $DOC$
 * $FUNCNAME$
 *    FT_ORIGIN()
 * $CATEGORY$
 *    Environment
 * $ONELINER$
 *    Report the drive, path and filename of the current program
 * $SYNTAX$
 *    FT_ORIGIN() -> cString
 * $ARGUMENTS$
 *    None
 * $RETURNS$
 *    A string containing the full drive/directory/filename of
 *    the currently executing file.
 * $DESCRIPTION$
 *    Often users will install multiple copies of application software,
 *    especially on networks and in situations where the user is trying
 *    to get around a copy protection scheme.
 *
 *    This function enables you to learn the name and source location
 *    of the currently executing file, so that you may take whatever
 *    action you need to.
 *
 *    Requires DOS v3.xx and above.
 * $EXAMPLES$
 *    cMyFile := FT_ORIGIN()
 *
 *    IF cMyFile <> "C:\APPDIR\MYFILE.EXE"
 *       ?"Incorrect startup file.  Please remove/rename and start again"
 *       QUIT
 *    ENDIF
 * $INCLUDE$
 *    extend.h
 * $SEEALSO$
 *    FT_WHEREIS() FT_TREE()
 * $END$
 */

#include <hbapi.h>

HB_FUNC(FT_ORIGIN)
{
#if defined(HB_OS_DOS) || defined(HB_OS_WIN_32)
   {

   extern char **_argv;

   hb_retc(  *_argv  );

   return;
}
#endif
}
