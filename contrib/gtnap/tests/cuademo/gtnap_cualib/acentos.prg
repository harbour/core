/* encoding: cp850 */

#pragma DebugInfo=on

#INCLUDE "xx.ch"
#INCLUDE "inkey.ch"

************
FUNC ACENTUA
************
*
STATIC N_Tecla_Ant := 0 
LOCAL  N_Tecla := LASTKEY(), C_Retorno
LOCAL  N_Pos
*
* processar acentuação (acentos comuns do Português passaram a ser
* gerados automaticamente pelo Windows / codepage selecionado.)
*
#DEFINE _AC_PAR_SIM_SIMPLIFICADO " º ª º ª"
#DEFINE _AC_PAR_NAO_SIMPLIFICADO "_o_a_O_A"
*
N_Pos := AT(CHR(N_Tecla_Ant)+CHR(N_Tecla),_AC_PAR_NAO_SIMPLIFICADO)
IF N_Pos # 0 .AND. n_Pos % 2 == 1        // se encontrar e for começo de par
   C_Retorno := IIF(SET(_SET_INSERT),CHR(K_BS),CHR(K_LEFT))+;
                SUBSTR(_AC_PAR_SIM_SIMPLIFICADO,N_Pos+1,1)
ENDIF
*
* Caso a tecla a ser retornada for o CHR(141)=="ì", o qual pode ser inserido
* no texto através do comando KEYB do DOS ou ALT,
* desconsiderar esta tecla, pois a mesma tem significado especial para o Clipper
* (soft carriage return - junto com o CHR(10))
*
IF CHR(N_Tecla) $ _AC_NAO_EXISTE   // Inclui o CHR(141)=="ì", e outras teclas
   C_Retorno := " "
ENDIF
*
N_Tecla_Ant := N_Tecla
RETURN C_Retorno
*
**************************** FIM DO ACENTOS *****************

