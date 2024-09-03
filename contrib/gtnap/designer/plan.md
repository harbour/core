# GTNAP Designer

El objetivo de este documento es realizar una introducción y planificación de la herramienta GTNAP-Designer para el diseño visual de formularios y ventanas de interfaz. En principio, esta pretende ser un "clon" del QT-Designer con algunas diferencias:

* GTNAP-Designer creará formularios compatibles con NAppGUI-SDK.
* Se podrán exportar los resultados a un archivo `.prg`, en lenguaje **Harbour** y compilable por `hbmk2`.
* Los formularios serán multiplataforma (Windows, macOS, Linux) y respetarán el look&feel nativo de cada sistema: Colores, Tipografías, Temas, etc.

**QT-Designer**
![QT-Designer](images/qtdesigner.png)

**GTNAP-Designer**
![GTNAP-Designer](images/gtnapdesigner.png)

## Planificación

El desarrollo de una herramienta de este tipo goza de cierta dificultad y es fácil de incurrir en retrasos y callejones sin salida por no delimitar bien los objetivos y funcionalidades. Por esta razón dividiremos el desarrollo en 4 fases:

* Operativa básica (12 Sprints).
* Componentes avanzados.
* Funciones avanzadas de edición.
* Formularios redimensionables.

## Fase 1: Operativa básica

**Importante** En esta fase el diseño interactivo será un poco tosco. Lo principal es poder crear formularios e importarlos en Harbour.

* Layout principal de la aplicación con tres áreas bien marcadas (igual que QtDesigner):
    * Parte izquierda: Zona widgets, donde están los elementos que podemos utilizar para el diseño.
    * Parte central: Canvas de dibujo. Donde diseñaremos los formularios.
    * Parte derecha: Inspector de propiedades.

* Elementos básicos de diseño:
    * Layout (Solo grid de momento). El grid permite crear VerticalLayout y HorizontalLayout.
    * Label.
    * Button.
    * CheckBox.
    * EditBox.

* Editor multi-formulario. En lugar de abrir formularios uno a uno, el editor permitirá tener varios de ellos y alternar entre ellos. Algo así como "open folder" abrirá todos los formularios existentes en una carpeta.

* Archivo de diseño (.ui). Archivo del formulario que puede abrirse con el editor. Seguramente en formato JSON.

* Exportar a Harbour. Generar un archivo .prg con el código capaz de crear el formulario en tiempo de ejecución.

* Comunicación con el formulario: El programa Harbour que crea el formulario debe ser capaz de comunicarse con él: Detectar si se ha pulsado un botón, obtener el texto de un EditBox, etc.

### Fase 1: Sprints

* Sprint 1: Crear el cuerpo de la aplicación. Ventanas, layouts, etc.

* Sprint 2: Posicionar layout y sublayouts en el canvas.

* Sprint 3: Añadir Label al canvas.

* Sprint 4: Añadir Button y CheckBox al canvas.

* Sprint 5: Poder grabar en disco los diseños. Funcionalidad multi-formulario.

* Sprint 6: Editar propiedades de layout.

* Sprint 7: Editar propiedades de controles (1).

* Sprint 8: Editar propiedades de controles (2).

* Sprint 9: Exportar a Harbour .prg (1).

* Sprint 10: Exportar a Harbour .prg (2).

* Sprint 11: Integración runtime con Harbour (1).

* Sprint 12: Integración runtime con Harbour (2).
