/* encoding: cp850 */
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

*******************
PROC EXEMPLO_FORMS
*******************
LOCAL V_JANELA
*
CUA20 @ 15,20,25,70 JANELA V_JANELA ;
     TITULO "Escolha o tipo de formulário" SUBTITULO "%T";
     AJUDA "T?????"
*
ESPECIALIZE V_JANELA MENU
ADDOPCAO V_JANELA TEXTO "Formulário #básico" ;
    ACAO TST_FORM_BASICO() AJUDA "P?????"
*
ATIVE(V_JANELA)
*

********************************
STAT PROC TST_FORM_BASICO
********************************
LOCAL V_FORM := NAP_FORM_LOAD(DIRET_FORMS() + "Customer.nfm")
LOCAL C_NAME := "Francisco"
LOCAL C_LAST := "García Collado"
LOCAL C_ADDRESS := "Calle de la Cueva, 23"
LOCAL C_CITY := "Alicante"
LOCAL C_PHONE := "+34 600111777"
LOCAL C_USER := "fran75"
LOCAL C_PASS := "pass5566"
LOCAL C_BANK := {"WE0012", "556431", "887652", "223477"}
LOCAL C_CARD := {"5678", "9900", "1122", "3344", "5566"}
LOCAL L_MAIL_LIST := .T.
LOCAL L_SECURE_PASS := .F.
LOCAL L_SHOW_ALERTS := .T.
LOCAL L_CONNECT_BANK := .F.
// Mapping between Harbour variables and form control names
LOCAL V_BIND := { ;
                    {"edit_first_name", @C_NAME }, ;
                    {"edit_last_name", @C_LAST }, ;
                    {"edit_address", @C_ADDRESS }, ;
                    {"edit_city", @C_CITY }, ;
                    {"edit_phone", @C_PHONE }, ;
                    {"edit_user", @C_USER }, ;
                    {"edit_pass", @C_PASS }, ;
                    {"edit_bank1", @C_BANK[1] }, ;
                    {"edit_bank1", @C_BANK[1] }, ;
                    {"edit_bank2", @C_BANK[2] }, ;
                    {"edit_bank3", @C_BANK[3] }, ;
                    {"edit_bank4", @C_BANK[4] }, ;
                    {"edit_credit1", @C_CARD[1] }, ;
                    {"edit_credit2", @C_CARD[2] }, ;
                    {"edit_credit3", @C_CARD[3] }, ;
                    {"edit_credit4", @C_CARD[4] }, ;
                    {"edit_credit5", @C_CARD[5] }, ;
                    {"check_mail_list", @L_MAIL_LIST }, ;
                    {"check_secure_pass", @L_SECURE_PASS }, ;
                    {"check_show_alerts", @L_SHOW_ALERTS }, ;
                    {"check_connect_bank", @L_CONNECT_BANK } ;
                }

LOCAL N_RES := 0

// Window title
NAP_FORM_TITLE(V_FORM, "Primeiro exemplo de formulário GTNAP")
// Write the variable values into the form controls (Edit, Buttons, etc)
NAP_FORM_DBIND(V_FORM, V_BIND)
// Launch the form
N_RES := NAP_FORM_MODAL(V_FORM)



NAP_FORM_DESTROY(V_FORM)
