# GTNAP-Forms

GTNAP-Forms es un conjunto de funciones Harbour, integradas en GTNAP, que nos permiten cargar y ejecutar los formularios creados con [NapDesigner](./Readme.md).

## Compilar GTNAP-Forms

GTNAP-Forms está integrado en GTNAP, ampliando su API, por lo que no tenemos que hacer nada especial para compilarlo. Tan solo correr el script de build in `contrib\gtnap`. Más información en [Build GTNAP](../Readme.md#build-gtnap).

```
cd contrib/gtnap

:: Windows MinGW
build.bat -b [Debug|Release] -comp mingw64

:: Linux/macOS
bash ./build.sh -b [Debug|Release]
```
## Cargar un formulario

NapDesigner crea archivos tipo `*.nfm` (NAppGUI forms) que se pueden cargar en tiempo de ejecución. En [exemploforms.prg](../tests/cuademo/gtnap_cualib/exemploforms.prg) tienes el ejemplo completo.

### NAP_FORM_LOAD

Carga un formulario desde un archivo en disco.

```
LOCAL V_FORM := NAP_FORM_LOAD(DIRET_FORMS() + "Customer.nfm")

PAR1: Ruta al archivo que contiene el formulario.
RET: Objeto que contiene el formulario.
```

### NAP_FORM_TITLE

Establece un título para la ventana del formulario.

```
NAP_FORM_TITLE(V_FORM, "Primeiro exemplo de formulário GTNAP")

PAR1: Objeto formulario.
PAR2: Cadena de texto con el título.
```
### NAP_FORM_MODAL

Lanza el formulario en modo modal.

```
N_RES := NAP_FORM_MODAL(V_FORM)

PAR1: Objeto formulario.
RET: Valor numérico con el resultado al cierre del formulario. Estos valores pueden ser:
    - NAP_MODAL_ENTER (2). El formulario se ha cerrado pulsando la tecla [RETURN].
    - NAP_MODAL_ESC (1). El formulario se ha cerrado pulsando la tecla [ESC].
    - NAP_MODAL_X_BUTTON (3). El formulario se ha cerrado pulsando el icono [X] de la ventana.
    - OTHER. El formulario se ha cerrado mediante NAP_FORM_STOP_MODAL
```

### NAP_STOP_MODAL

Fuerza el cierre de un formulario lanzado mediante `NAP_FORM_MODAL`. Esta función debe ser invocada por alguna función callback, como respuesta a cualquier evento GUI (por ejemplo, pulsar un botón).

```
NAP_FORM_ONCLICK(V_FORM, "button_ok", {|| NAP_FORM_STOP_MODAL(V_FORM, 1000) })

PAR1: Objeto formulario.
PAR2: Valor numérico a devolver como respuesta en NAP_FORM_MODAL.
```
## Vinculación de datos

Para que el formulario sea realmente práctico, debemos poder conectar variables en la parte Harbour con los controles GUI (widgets). Para ello deberemos crear un vector de pares (id-variable). El `id` es el nombre del widget encargado de editar el valor de la `variable`.

```
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
```

Si la variable se proporciona por "valor" será de solo lectura. Veremos el valor de la misma en el widget, pero no se podrá grabar el valor asignado por el usuario. Para ello, pasar la variable por referencia.

### NAP_FORM_DBIND

Vincula un vector de parejas (id-variable) con el formulario.

```
NAP_FORM_DBIND(V_FORM, V_BIND)

PAR1: Objeto formulario.
PAR2: Vector de pares (id-variable)
```

Cuando el formulario se lance con `NAP_FORM_MODAL()`, el valor de las variables será mapeado de forma automática en los widgets. Si el usuario cambia cualquier valor, NO será grabado de vuelta hasta que llamemos a `NAP_FORM_DBIND_STORE()`.

### NAP_FORM_DBIND_STORE

Graba en contenido de los widgets del formulario en las variables proporcionadas por `NAP_FORM_DBIND()`

```
IF N_RES == NAP_MODAL_ENTER .OR. N_RES == 1000
    // Write the values from the GUI controls to Harbour variables
    NAP_FORM_DBIND_STORE(V_FORM)

PAR1: Objeto formulario.
```

Si la variable se pasó por valor, será imposible grabar los cambios. Pasar una referencia a la variable para hacer de lectura/escritura.

## Eventos de botón

Si la parte Harbour quiere realizar alguna acción si se pulsa un botón, será necesario asociar un bloque de código con el componente.

### NAP_FORM_ONCLICK

Establece el bloque de código que se ejecutará al pulsar un botón en el formulario.

```
NAP_FORM_ONCLICK(V_FORM, "button_ok", {|| NAP_FORM_STOP_MODAL(V_FORM, 1000) })

PAR1: Objeto formulario.
PAR2: ID del botón.
PAR3: Bloque de código que se ejecutará en la parte de Harbour.
```

En este ejemplo, pulsar el botón tiene asociado el cierre del formulario con el código de retorno `1000`.
