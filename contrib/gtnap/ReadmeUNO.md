# GTNap-UNO

Harbour interface for access to LibreOffice documents

## Errors

Every call to any `NAP_XLS_??` function will store in cache the last error code.

```
// Error number
LOCAL N_Err := NAP_OFFICE_LAST_ERROR()

// Error string
LOCAL C_Err := NAP_OFFICE_ERROR_STR(N_Err)
```

The numeric error codes are in `gtnap.ch`
```
#define SDKRES_OK                   1
#define SDKRES_NO_ENVAR             2
#define SDKRES_PROC_KILL_FAIL       3
#define SDKRES_PROC_INIT_FAIL       4
#define SDKRES_CONECT_FAIL          5
#define SDKRES_COMPONENT_LOADER     6
#define SDKRES_CREATE_FILE_ERROR    7
#define SDKRES_OPEN_FILE_ERROR      8
#define SDKRES_SAVE_FILE_ERROR      9
#define SDKRES_CLOSE_DOC_ERROR      10
#define SDKRES_ACCESS_DOC_ERROR     11
#define SDKRES_ACCESS_CELL_ERROR    12
#define SDKRES_EDIT_CELL_ERROR      13
#define SDKRES_FORMAT_CELL_ERROR    14
#define SDKRES_ACCESS_COLUMN_ERROR  15
#define SDKRES_FORMAT_COLUMN_ERROR  16
```

## String parameters

Every string parameter in any `NAP_XLS_??` will accept a string directly or a harbour block that returns a string.

```
// Valid
LOCAL O_XLS := NAP_XLS_OPEN({|| NAP_WORK_PATH() + "/../office/empty.ods" })

// Valid
LOCAL O_XLS := NAP_XLS_OPEN( NAP_WORK_PATH() + "/../office/empty.ods" )
```

## SpreadSheet

### Open Sheet

```
LOCAL O_XLS := NAP_XLS_OPEN({|| NAP_WORK_PATH() + "/../office/empty.ods" })

PAR1: String with the full path for the document.
RET: The document object. Must be closed with NAP_XLS_CLOSE().
```

### Create an empty Sheet

```
LOCAL O_XLS := NAP_XLS_CREATE()

RET: The document object. Must be closed with NAP_XLS_CLOSE().
```

### Close Sheet

```
NAP_XLS_CLOSE(O_XLS)

PAR1: The sheet document.
```

> **Important:** All sheets open with `NAP_XLS_OPEN` or created with `NAP_XLS_CREATE` must be closed.

### Save Sheet

```
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/edited.ods" })

PAR1: The sheet document.
PAR2: String with the full path for the document copy.
```
> **Important:** It will save a copy of document, but will not close the original one.

### Set the page name

```
NAP_XLS_NAME(O_XLS, 0, "DEMOSTRAÇAO RECEITA E DESPESA")

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: String with the name.
```

### Protect one page

```
NAP_XLS_PROTECT(O_XLS, 0, .T., "ASDF01234")

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Protect (.T.) or unprotect (.F.)
PAR4: String with the password.
```

### Set Cell Text

```
NAP_XLS_CELL_TEXT(O_XLS, 0, 0, 0, {|| "Hello World!"})

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: String with the cell text.
```

### Set Cell Value

```
NAP_XLS_CELL_VALUE(O_XLS, 0, 0, 0, 917038.94)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Numeric value (double).
```

### Cell Numeric Format

```
NAP_XLS_CELL_NUMFORMAT(O_XLS, 0, 0, 0, SDK_NUMFORMAT_INT)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Numeric constant with the format.

#define SDK_NUMFORMAT_INT           1
#define SDK_NUMFORMAT_INT_1000      2
#define SDK_NUMFORMAT_DEC2          3
#define SDK_NUMFORMAT_DEC2_1000     4
#define SDK_NUMFORMAT_PERC_INT      5
#define SDK_NUMFORMAT_PERC_DEC2     6
```

### Cell Font Family

```
NAP_XLS_CELL_FONT_FAMILY(O_XLS, 0, 0, 0, "Arial")

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: String with the font family.
```

### Cell Font Size

```
NAP_XLS_CELL_FONT_SIZE(O_XLS, 0, 0, 0, 9.0)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Font size (height). Double value.
```

### Cell Bold

```
NAP_XLS_CELL_BOLD(O_XLS, 0, 0, 0, .T.)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Font weight. Bold (.T.) or Normal (.F.).
```

### Cell Italic

```
NAP_XLS_CELL_ITALIC(O_XLS, 0, 0, 0, .T.)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Font style. Italic (.T.) or Normal (.F.).
```

### Cell Horizontal Alignment

```
NAP_XLS_CELL_HALIGN(O_XLS, 0, 0, 0, SDK_HALIGN_RIGHT)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Numeric value with horizontal alignment.

#define SDK_HALIGN_LEFT             0
#define SDK_HALIGN_CENTER           1
#define SDK_HALIGN_RIGHT            2
```

### Cell Vertical Alignment

```
NAP_XLS_CELL_VALIGN(O_XLS, 0, 0, 0, SDK_VALIGN_CENTER)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Numeric value with vertical alignment.

#define SDK_VALIGN_TOP              0
#define SDK_VALIGN_CENTER           1
#define SDK_VALIGN_BOTTOM           2
```

### Cell Background color

```
NAP_XLS_CELL_BACKCOLOR(O_XLS, 0, 0, 0, NAP_OFFICE_RGB(205, 205, 205))

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Value returned by NAP_OFFICE_RGB
```

### Merge cells

```
NAP_XLS_CELL_MERGE(O_XLS, 0, 2, 8, 3, 8)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column-left index (0-based).
PAR4: Row-top index (0-based).
PAR5: Column-right index >= column-left.
PAR6: Row-bottom index >= row-top.
```

### Column Visible

```
NAP_XLS_COLUMN_VISIBLE(O_XLS, 0, 0, .T.)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Column visible (.T. / .F.).
```

### Column Optimal Width

```
NAP_XLS_COLUMN_OPTIMAL_WIDTH(O_XLS, 0, 0, .F.)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Column with optimal width (.T. / .F.).
```

### Column Width

```
NAP_XLS_COLUMN_WIDTH(O_XLS, 0, 0, 6000)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Column width (integer 1/100th mm)
```






