/*
 * $Id$
 *
 * Harbour compiler and runtime configuration file
*/

#ifndef hbsetuph
#define hbsetuph

/* The name of starting procedure
 * Note: You have to define it in case when Harbour cannot find the proper
 * starting procedure (due to incorrect order of static data initialization)
 * Now compilers that require it:
 * Watcom C/C++ 10.0
*/
#define HARBOUR_MAIN "MAIN"

/* The ability to create and link OBJ files created by Harbour compiler
 * By default it is disabled
*/
/*#define OBJ_GENERATION*/



#endif