# GTNap-UNO

Harbour interface for access to LibreOffice documents

## Errors

Every call to any `NAP_XLS_??` function will store in cache the last error code.

```
// Error number
LOCAL N_Err := NAP_OFFICE_LAST_ERROR()

// Error string
LOCAL C_Err := NAP_OFFICE_ERROR(N_Err)
```

The error codes are in `gtnap.ch`
```
#define SDKRES_OK                   1
#define SDKRES_NO_ENVAR             2
#define SDKRES_PROC_KILL_FAIL       3
#define SDKRES_PROC_INIT_FAIL       4
#define SDKRES_CONECT_FAIL          5
#define SDKRES_COMPONENT_LOADER     6
#define SDKRES_OPEN_FILE_ERROR      7
#define SDKRES_SAVE_FILE_ERROR      8
#define SDKRES_CLOSE_DOC_ERROR      9
#define SDKRES_ACCESS_DOC_ERROR     10
#define SDKRES_ACCESS_CELL_ERROR    11
#define SDKRES_EDIT_CELL_ERROR      12
#define SDKRES_FORMAT_CELL_ERROR    13
#define SDKRES_ACCESS_COLUMN_ERROR  14
#define SDKRES_FORMAT_COLUMN_ERROR  15
```

## SpreadSheet

### Open Sheet

```
LOCAL O_XLS := NAP_XLS_OPEN({|| NAP_WORK_PATH() + "/../office/empty.ods" })

PAR1: Block that returns the full path for the document.
RET: The document object. Must be closed with NAP_XLS_CLOSE().
```

### Set Column Format

```
NAP_XLS_COLUMN_FORMAT(O_XLS, 0, 0, .T., .F., 6000)

PAR1: The sheet document.
PAR2: Sheet index (0-based).
PAR3: Column index (0-based).
PAR4: Column visible (.T. / .F.).
PAR5: Column with optimal width (.T. / .F.).
PAR6: Column width (in 1/100th mm)
```


### Set Cell Text

```
NAP_XLS_CELL_TEXT(O_XLS, 0, 0, 0, {|| "Hello World!"})

PAR1: The sheet document.
PAR2: Sheet index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Block that returns the cell text.
```

### Set Cell Format

```
NAP_XLS_CELL_FORMAT(O_XLS, 0, 0, 0, {|| "Arial"}, 16.0, .F., .F.)

PAR1: The sheet document.
PAR2: Sheet index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Block that returns the cell font family.
PAR6: Font size (height). Real value.
PAR7: Font weight. Bold (.T.) or Normal (.F.).
PAR8: Font style. Italic (.T.) or Normal (.F.).
```

### Save Sheet

```
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/edited.ods" })

PAR1: The sheet document.
PAR2: Block that returns the full path for the document copy.
```
> **Important:** It will save a copy of document, but will not close the original one.

### Close Sheet

```
NAP_XLS_CLOSE(O_XLS)

PAR1: The sheet document.
```

> **Important:** All sheets open with `NAP_XLS_OPEN` must be closed.


