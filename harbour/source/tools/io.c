#include <extend.h>
#include <dos.h>
#include  <dir.h>
#include  <stdio.h>
#include  <stdlib.h>
#include  <string.h>
#include  <bios.h>
#include  <ctype.h>

#define TRUE 1
#define FALSE 0


HARBOUR renfile()
{
int ok;
        PITEM arg1 = _param(1,IT_STRING);
        PITEM arg2 = _param(2,IT_STRING);
        if( arg1 && arg2)
{
ok=rename(_parc(1),_parc(2));
if(!ok)
_retl(TRUE);
else
_retl(FALSE);
}
}
/*


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

/*





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

/*
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
HARBOUR CD(void)

{
        PITEM MEUDIR = _param(1,IT_STRING);
        if(MEUDIR)
{
_retni(chdir(_parc(1)));
}
}

HARBOUR MD(void)
{

        PITEM MEUDIR = _param(1,IT_STRING);
        if(MEUDIR)
{

_retni(mkdir(_parc(1)));
}
}

HARBOUR RD(void)
{
        PITEM MEUDIR = _param(1,IT_STRING);
        if(MEUDIR)
{

_retni(chdir(_parc(1)));
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

HARBOUR file(void)
{
        PITEM arg1 = _param(1,IT_STRING);
char *arquivos;
int achou;
struct ffblk arquivo;
if (arg1)             {
arquivos=_parc(1);
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
HARBOUR DISKUSED(void)
{
long bytsfree,bytsfull;
struct diskfree_t disk;
_dos_getdiskfree(0,&disk);
bytsfree = (long) disk.avail_clusters * (long) disk.sectors_per_cluster * (long ) disk.bytes_per_sector;
bytsfull = (long) disk.total_clusters * (long) disk.sectors_per_cluster * (long ) disk.bytes_per_sector;
_retnl(bytsfull-bytsfree);
}
HARBOUR DISKFREE(void)
{
long bytsfree;
struct diskfree_t disk;
_dos_getdiskfree(0,&disk);
bytsfree = (long) disk.avail_clusters * (long) disk.sectors_per_cluster * (long ) disk.bytes_per_sector;

_retnl(bytsfree);
}
HARBOUR DISKFULL(void)
{
long bytsfull;
struct diskfree_t disk;
_dos_getdiskfree(0,&disk);

bytsfull = (long) disk.total_clusters * (long) disk.sectors_per_cluster * (long ) disk.bytes_per_sector;
_retnl(bytsfull);
}
