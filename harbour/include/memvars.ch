/*
 * $Id$
*/

/* NOTE: This file is also used by C code. */

// Values returned from __MVSCOPE function
//
#define MV_NOT_FOUND      -2   //not found in the symbols table
#define MV_UNKNOWN        -1   //not created yet
#define MV_ERROR           0   //information cannot be obtained
#define MV_PUBLIC          1   //PUBLIC variable
#define MV_PRIVATE_GLOBAL  2   //PRIVATE created outside of current function/procedure
#define MV_PRIVATE_LOCAL   3   //PRIVATE created in current function/procedure
