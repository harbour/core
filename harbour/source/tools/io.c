/*
 * $Id$
 */

#include "extend.h"
#include <ctype.h>

#ifdef __DOS__
#include <dos.h>
#include  <dir.h>
#include  <bios.h>
#endif

#define TRUE 1
#define FALSE 0


#ifdef __DOS__

HARBOUR HB_RENFILE(void)
{
  int ok;
  PHB_ITEM arg1 = hb_param(1,IT_STRING);
  PHB_ITEM arg2 = hb_param(2,IT_STRING);
  if( arg1 && arg2)
    {
      ok=rename(hb_parc(1),hb_parc(2));
      if(!ok)
	_retl(TRUE);
      else
	_retl(FALSE);
    }
}

/*  $DOC$
 *  $FUNCNAME$
 *     CD()
 *  $CATEGORY$
 *     DOS
 *  $ONELINER$
 *
 *  $SYNTAX$
 *     CD(<NDIR>)
 *  $ARGUMENTS$
 *    <NDIR> DIR TO BE CHANGED
 *  $RETURNS$
 *    .T. IF SUCEFUL
 *    .F. IF NOT
 *
 *  $DESCRIPTION$
 *    CHANGE THE CURRENT DIRECTORY
 *  $EXAMPLES$
 *  IF CD("OLA")
 *      RETURN(.T.)
 *  ELSE
 *      RETURN(.F.)
 *  ENDIF
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 * extend.h dos.h dir.h bios.h internal.h
 *  $END$
 */



/*  $DOC$
 *  $FUNCNAME$
 *     MD()
 *  $CATEGORY$
 *     DOS
 *  $ONELINER$
 *
 *  $SYNTAX$
 *     MD(<NDIR>)
 *  $ARGUMENTS$
 *    <NDIR> DIRECTORY TO BE CREATED
 *  $RETURNS$
 *    .T. IF SUCEFUL
 *    .F. IF NOT
 *
 *  $DESCRIPTION$
 *    CREATE A  DIRECTORY
 *  $EXAMPLES$
 *  IF MD("OLA")
 *      RETURN(.T.)
 *  ELSE
 *      RETURN(.F.)
 *  ENDIF
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 * extend.h dos.h dir.h bios.h internal.h
 *  $END$
 */

/*  $DOC$
 *  $FUNCNAME$
 *     RD()
 *  $CATEGORY$
 *     DOS
 *  $ONELINER$
 *
 *  $SYNTAX$
 *     RD(<NDIR>)
 *  $ARGUMENTS$
 *    <NDIR> DIR TO BE DELETED
 *  $RETURNS$
 *    .T. IF SUCEFUL
 *    .F. IF NOT
 *
 *  $DESCRIPTION$
 *    REMOVE A  DIRECTORY
 *  $EXAMPLES$
 *  IF RD("OLA")
 *      RETURN(.T.)
 *  ELSE
 *      RETURN(.F.)
 *  ENDIF
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 * extend.h dos.h dir.h bios.h internal.h
 *  $END$
 */


HARBOUR HB_CD(void)

{
  PHB_ITEM MEUDIR = hb_param(1,IT_STRING);
  if(MEUDIR)
    {
      hb_retni(chdir(hb_parc(1)));
    }
}

HARBOUR HB_MD(void)
{

  PHB_ITEM MEUDIR = hb_param(1,IT_STRING);
  if(MEUDIR)
    {

      hb_retni(mkdir(hb_parc(1)));
    }
}

HARBOUR HB_RD(void)
{
  PHB_ITEM MEUDIR = hb_param(1,IT_STRING);
  if(MEUDIR)
    {

      hb_retni(chdir(hb_parc(1)));
    }
}

/*  $DOC$
 *  $FUNCNAME$
 *     FILE()
 *  $CATEGORY$
 *     DOS
 *  $ONELINER$
 *
 *  $SYNTAX$
 *     RD(<NDIR>)
 *  $ARGUMENTS$
 *    <NDIR> FILE TO BE CHECKED
 *  $RETURNS$
 *    .T. IF SUCEFUL
 *    .F. IF NOT
 *
 *  $DESCRIPTION$
 *    VERIFY IF A FILE EXISTS
 *  $EXAMPLES$
 *  IF FILE("OLA.PRG")
 *      RETURN(.T.)
 *  ELSE
 *      RETURN(.F.)
 *  ENDIF
 *
 *  $SEEALSO$
 *
 *  $INCLUDE$
 * extend.h dos.h dir.h bios.h internal.h
 *  $END$
 */

HARBOUR HB_FILE(void)
{
  PHB_ITEM arg1 = hb_param(1,IT_STRING);
  char *arquivos;
  int achou;
  struct ffblk arquivo;
  if (arg1)             {
    arquivos=hb_parc(1);
    achou=findfirst(arquivos,&arquivo,FA_ARCH);
    if (achou)
      {
	_retl(TRUE);
      }
    else
      {
	_retl(FALSE);
      }
  }
}
HARBOUR HB_DISKUSED(void)
{
  long bytsfree,bytsfull;
  struct diskfree_t disk;
  _dos_getdiskfree(0,&disk);
  bytsfree = ((long) disk.avail_clusters *
	      (long) disk.sectors_per_cluster *
	      (long ) disk.bytes_per_sector);
  bytsfull = ((long) disk.total_clusters *
	      (long) disk.sectors_per_cluster *
	      (long ) disk.bytes_per_sector);
  hb_retnl(bytsfull-bytsfree);
}

HARBOUR HB_DISKFREE(void)
{
  long bytsfree;
  struct diskfree_t disk;
  _dos_getdiskfree(0,&disk);
  bytsfree = ((long) disk.avail_clusters *
	      (long) disk.sectors_per_cluster *
	      (long ) disk.bytes_per_sector);

  hb_retnl(bytsfree);
}

HARBOUR HB_DISKFULL(void)
{
  long bytsfull;
  struct diskfree_t disk;
  _dos_getdiskfree(0,&disk);

  bytsfull = ((long) disk.total_clusters *
	      (long) disk.sectors_per_cluster *
	      (long ) disk.bytes_per_sector);
  hb_retnl(bytsfull);
}

#endif
