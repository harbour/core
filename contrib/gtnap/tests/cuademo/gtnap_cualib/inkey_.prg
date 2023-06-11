/* encoding: cp850 */

#pragma DebugInfo=Off

#INCLUDE "inkey.ch"
* Versão      : 1.0
FUNCTION        InKey_(L_Forever, N_Pilha)
* Função      : SIMULA INKEY(0) COM "WAIT STATE" (ATIVAÇÃO DE SET KEY"S)
* Autor       : MARCOS AUGUSTO
* Data        : 02/01/91
* Observações :
*
* Tecla pressionada = InKey_(.T.)     igual a INKEY(0)
*
*     Caso a tecla pressionada esteja setada (SET KEY) o bloco será executado
*          e será solicitada uma nova tecla
*     Caso a tecla pressionada não esteja setada será retornado o
*          código da referida tecla
*
* Tecla pressionada = InKey_(.F.)     igual a INKEY()
*
*     Caso exista uma tecla pressionada e esteja setada (SET KEY) o bloco será
*          executado e será solicitada uma nova tecla por um breve instante
*     Caso exista uma tecla pressionada e não esteja setada será retornado o
*          código da referida tecla
*     Caso não exista uma tecla pressionada será retornado 0
*
LOCAL N_Tecla, B_BlocoKey
LOCAL N_PaintRefresh_Old
*
IF N_Pilha == NIL
   N_Pilha := 1         // 1 = rotina chamadora
ENDIF
*
N_Tecla := IIF(L_Forever,INKEYX(0),INKEYX())
DO WHILE (B_BlocoKey := SETKEY(N_Tecla)) # NIL
    #INCLUDE "janela.ch"
    #INCLUDE "define_cua.ch"
    EVAL(B_BlocoKey, PROCNAME(N_Pilha), PROCLINE(N_Pilha))
    N_Tecla := IIF(L_Forever,INKEYX(0),INKEYX())
ENDDO
*
RETURN( N_Tecla )
*

***********
FUNC INKEYX ( N_SEG )   // resolve o problema do TIME SLICE
***********
LOCAL N_INKEY, N_SEGUNDOS, N_TIMEOUT
LOCAL N_PaintRefresh_Old

IF N_SEG==NIL        // INKEY(NIL) para o programa !!
   N_INKEY := INKEY()
ELSEIF N_SEG # 0
   * Tempo determinado pelo programador.
   * Neste caso nao existe TIMEOUT().
   N_INKEY := INKEY(N_SEG)
ELSE
   N_TIMEOUT := SETA_TIMEOUT()
   IF N_TIMEOUT == 0
      N_INKEY := INKEY(N_SEG)
   ELSE
      N_SEGUNDOS := SECONDS()
      N_INKEY    := INKEY(N_TIMEOUT)
      IF (SECONDS() - N_SEGUNDOS) >= N_TIMEOUT
         KEYBOARD REPL(CHR(K_ESC),50)
         SETA_TEMPOLIMITE(.T.)
      ENDIF
   ENDIF
ENDIF
RETURN N_INKEY

*****************
FUNC SETA_TIMEOUT(B_TIMEOUT_NEW)
*****************
STATIC B_TIMEOUT2 := 0
IF B_TIMEOUT_NEW # NIL
   B_TIMEOUT2 := B_TIMEOUT_NEW
ENDIF
RETURN B_TIMEOUT2
*
*********************
FUNC SETA_TEMPOLIMITE(L_LIMITE_NEW)
*********************
STATIC L_LIMITE := .F.
IF L_LIMITE_NEW # NIL
   L_LIMITE := L_LIMITE_NEW
ENDIF
RETURN L_LIMITE


// ******************************* FIM DA INKEY_()  ***********************

