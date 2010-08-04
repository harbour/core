/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* NOTE: Following comments are control commands for the generator.     */
/*       Do not edit them unless you know what you are doing.           */
/*          Syntax: // HB_FUNC_INCLUDE <func>                           */
/*                  // HB_FUNC_EXCLUDE <func>                           */
/* -------------------------------------------------------------------- */

// HB_FUNC_INCLUDE HBARRAY
// HB_FUNC_INCLUDE HBBLOCK
// HB_FUNC_INCLUDE HBCHARACTER
// HB_FUNC_INCLUDE HBDATE
// HB_FUNC_INCLUDE HBLOGICAL
// HB_FUNC_INCLUDE HBNIL
// HB_FUNC_INCLUDE HBNUMERIC
// HB_FUNC_INCLUDE HBSYMBOL
// HB_FUNC_INCLUDE HBTIMESTAMP

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated code below. DO NOT EDIT!            */
/*          Regenerate with HB_REBUILD_EXTERN=yes while using GCC       */
/*          compiler family. [vszakats]                                 */
/* -------------------------------------------------------------------- */

#ifndef __HBEXTERN_CH__HBEXTSCA__
#define __HBEXTERN_CH__HBEXTSCA__

#ifdef __HBEXTERN__HBEXTSCA__REQUEST
   ANNOUNCE __HBEXTERN__HBEXTSCA__
   #command DYNAMIC <fncs,...> => EXTERNAL <fncs>
#endif

DYNAMIC HBARRAY
DYNAMIC HBBLOCK
DYNAMIC HBCHARACTER
DYNAMIC HBDATE
DYNAMIC HBLOGICAL
DYNAMIC HBNIL
DYNAMIC HBNUMERIC
DYNAMIC HBSYMBOL
DYNAMIC HBTIMESTAMP

#ifdef __HBEXTERN__HBEXTSCA__REQUEST
   #uncommand DYNAMIC <fncs,...> => EXTERNAL <fncs>
#endif

#endif
