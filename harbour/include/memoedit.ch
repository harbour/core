/*
 * $Id$
 */

#ifndef _MEMOEDIT_CH
#define _MEMOEDIT_CH

/* User callback status modes */
#define ME_IDLE         0       /* Idle, all keys processed       */
#define ME_UNKEY        1       /* Unknown key, memo unaltered    */
#define ME_UNKEYX       2       /* Unknown key, memo altered      */
#define ME_INIT         3       /* Initialization mode            */

/* User callback return codes */
#define ME_DEFAULT      0       /* Perform default action         */
#define ME_IGNORE       32      /* Ignore unknown key             */
#define ME_DATA         33      /* Treat unknown key as data      */
#define ME_TOGGLEWRAP   34      /* Toggle word-wrap mode          */
#define ME_TOGGLESCROLL 35      /* Toggle scrolling mode          */
#define ME_WORDRIGHT    100     /* Perform word-right operation   */
#define ME_BOTTOMRIGHT  101     /* Perform bottom-right operation */

/* NOTE: Return codes 1-31 cause MEMOEDIT() to perform the */
/*       edit action corresponding to the key whose value is returned. */

#endif /* _MEMOEDIT_CH */
