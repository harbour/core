/*
 * $Id$
 */

/* Definitions shared by  harbour.y and preprocessor */

#ifndef HB_PP_H_
#define HB_PP_H_

/* ------------------------------------------ */
typedef struct
{
  char *name;
  char *pars;
  int npars;
  char *value;
} DEFINES;

typedef struct
{
  int com_or_xcom;
  char *name;
  char *mpatt;
  char *value;
} COMMANDS, TRANSLATES;

#endif /* HB_PP_H_ */
