/*
 * $Id$
 */

//*******************************************************************
/*
   FILE: version.prg
   This program is automatically created by cccver v1.0.00.
   Do not modify this directly - changes will be lost!
*/

#define VERSION {1,0,02}
#define NAME                   _l_object
#ifndef _CCC_
#define NAME_MAJOR             object_1
#define NAME_MAJOR_MINOR       object_1_0
// #define NAME_MAJOR_MINOR_REV   object_1_0_02
#endif // _CCC_
#define NAMESTR                '_l_object'
//*******************************************************************

*********************************************************************
#include "error.ch"                                                
                                                                     
*********************************************************************
static function strVersion(version)                                  
// Megadja a verzi¢sz mot stringk‚nt.                                
return 'v'+;                                                         
       padl(version[1],1)+'.'+;                                      
       padl(version[2],1)+'.'+;                                      
       padl(version[3],2,'0')                                        
                                                                     
*********************************************************************
function NAME(rVersion,asString)                                     
local version:=VERSION                                               
local iVersion                                                       
local err,i                                                          
                                                                     
   if (rVersion!=nil)                                                
      iVersion:=aclone(rVersion)                                     
      asize(iVersion,3)                                              
      for i:=1 to len(iVersion)                                      
         if (iVersion[i]==nil)                                       
            iversion[i]:=0                                           
         endif                                                       
      end for                                                        
                                                                     
      for i:=1 to len(version)                                       
         if (version[i]>iVersion[i])                                 
            exit                                                     
         elseif (version[i]<iVersion[i])                             
            err:=errorNew()                                          
            err:cargo:=version                                       
            err:args:=rVersion                                       
            err:canRetry:=.f.                                        
            err:description:=NAMESTR+': A k”nyvt r verzi¢sz ma alacsonyabb;'+;  
                                  ' az alkalmaz s  ltal ig‚nyeltn‚l!;'+;        
                                  'K”nyvt r  : '+strVersion(version)+';'+;      
                                  'Alkalmaz s: '+strVersion(iVersion)           
            err:filename:=''                                         
            err:severity:=ES_ERROR                                   
            err:operation:=NAMESTR+'()'                              
            err:subsystem:='VER'                                     
            err:subCode:=''                                          
            return eval(errorblock(),err)                            
         endif                                                       
      end for                                                        
   endif                                                             
return if(!empty(asString),strVersion(version),aclone(version))      
                                                                     
*********************************************************************
#ifdef NAME_MAJOR                                                    
function NAME_MAJOR(iVersion)                                        
return NAME(iVersion)                                                
#endif                                                               
                                                                     
*********************************************************************
#ifdef NAME_MAJOR_MINOR                                              
function NAME_MAJOR_MINOR(iVersion)                                  
return NAME(iVersion)                                                
#endif                                                               
*********************************************************************
#ifdef NAME_MAJOR_MINOR_REV                                          
function NAME_MAJOR_MINOR_REV(iVersion)                              
return NAME(iVersion)                                                
#endif                                                               
                                                                     
*********************************************************************
