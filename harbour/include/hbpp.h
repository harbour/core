/*
 * $Id$
 */

/* Definitions shared by  harbour.y and preprocessor */

#ifndef HB_PP_H_
#define HB_PP_H_

/* ------------------------------------------ */
struct _DEFINES;
typedef struct _DEFINES
{
  char *name;
  char *pars;
  int npars;
  char *value;
  struct _DEFINES *last;
} DEFINES;

typedef struct
{
  int com_or_xcom;
  char *name;
  char *mpatt;
  char *value;
} COMMANDS, TRANSLATES;

#endif /* HB_PP_H_ */
