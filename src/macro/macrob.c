/* hbexprb.c is also included from ../compiler/exproptb.c
 * However it produces a slighty different code if used in
 * macro compiler (there is an additional parameter passed to some functions)
 */

#define HB_MACRO_SUPPORT

#include "hbmacro.h"
#include "hbcomp.h"

#include "hbexprb.c"
