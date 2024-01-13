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

## Color parameters

Every color parameter is an 32bit integer rgb.

```
LOCAL N_Color := NAP_OFFICE_RGB(255, 0, 0)

PAR1: 0-255 red component.
PAR2: 0-255 green component.
PAR3: 0-255 blue component.
RET: The color (integer value).
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

### Save Sheet

```
NAP_XLS_SAVE(O_XLS, {|| NAP_WORK_PATH() + "/../office/edited.ods" })

PAR1: The sheet document.
PAR2: String with the full path for the document copy.
```
> **Important:** It will save a copy of document, but will not close the original one.

### Close Sheet

```
NAP_XLS_CLOSE(O_XLS)

PAR1: The sheet document.
```

> **Important:** All sheets open with `NAP_XLS_OPEN` or created with `NAP_XLS_CREATE` must be closed.

### Add new pages to Sheet

```
LOCAL N_Page := NAP_XLS_ADD(O_XLS)

PAR1: The sheet document.
RET: The page index (0-based).
```

> **Important:** `NAP_XLS_CREATE` creates a default 0-page.

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

### Freeze first rows and columns

```
NAP_XLS_FREEZE(O_XLS, 0, 2, 0)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Number of columns to freeze.
PAR4: Number of rows to freeze.
```

### Get Cell Reference for formulas

```
C_Ref := NAP_XLS_CELL_REF(O_XLS, 0, 0, 28)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
RET: A string with the cell reference "$'RREO-Anexo 01'.A29"
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

### Set Cell Date

```
NAP_XLS_CELL_DATE(O_XLS, 0, 0, 0, 31, 10, 2023)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Day.
PAR6: Month.
PAR7: Year.
```

### Set Cell Formula

```
NAP_XLS_CELL_FORMULA(O_XLS, 0, 10, 8, "B22+B23")

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: String with the formula.
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

#define SDK_NUMFORMAT_DATE_SYS_DDMMM            7
#define SDK_NUMFORMAT_DATE_SYS_DDMMYY           8
#define SDK_NUMFORMAT_DATE_SYS_DDMMYYYY         9
#define SDK_NUMFORMAT_DATE_SYS_DMMMMYYYY        10
#define SDK_NUMFORMAT_DATE_SYS_DMMMYY           11
#define SDK_NUMFORMAT_DATE_SYS_DMMMYYYY         12
#define SDK_NUMFORMAT_DATE_SYS_MMYY             13
#define SDK_NUMFORMAT_DATE_SYS_NNDMMMMYYYY      14
#define SDK_NUMFORMAT_DATE_SYS_NNDMMMYY         15
#define SDK_NUMFORMAT_DATE_SYS_NNNNDMMMMYYYY    16
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

### Cell Text Wrap

```
NAP_XLS_CELL_WRAP(O_XLS, 0, 0, 0, .T.)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR: Wrap text (.T. / .F.).
```

### Cell Text Color

```
NAP_XLS_CELL_COLOR(O_XLS, 0, 0, 0, NAP_OFFICE_RGB(255, 255, 255))

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Color value returned by NAP_OFFICE_RGB
```

### Cell Background color

```
NAP_XLS_CELL_BACKCOLOR(O_XLS, 0, 0, 0, NAP_OFFICE_RGB(205, 205, 205))

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Color value returned by NAP_OFFICE_RGB.
```

### Background color for a group of cells

```
NAP_XLS_CELLS_BACKCOLOR(O_XLS, 0, 0, 0, 10, 8 NAP_OFFICE_RGB(205, 205, 205))

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column-left index (0-based).
PAR4: Row-top index (0-based).
PAR5: Column-right index >= column-left.
PAR6: Row-bottom index >= row-top.
PAR7: Color value returned by NAP_OFFICE_RGB.
```

> **Important:** It will clean the inner grid lines.

### Insert a image in cell position

```
NAP_XLS_CELL_IMAGE(O_XLS, 0, 0, 0, {|| NAP_WORK_PATH() + "/../office/ods/cell_image_01.png" })

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Image file path.
```

> **Important:** The image is not part of the cell (it is not an attribute). It is inserted into the document and positioned and sized in the cell frame.

### Cell border

```
NAP_XLS_CELL_BORDER(O_XLS, 0, 0, 0, SDK_LINE_STYLE_SOLID, 50, NAP_OFFICE_RGB(255, 255, 255))

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column index (0-based).
PAR4: Row index (0-based).
PAR5: Line style.
PAR6: Line thickness (integer 1/100th mm)
PAR7: Color value returned by NAP_OFFICE_RGB.

#define SDK_LINE_STYLE_NONE                     1
#define SDK_LINE_STYLE_SOLID                    2
#define SDK_LINE_STYLE_DOTTED                   3
#define SDK_LINE_STYLE_DASHED                   4
#define SDK_LINE_STYLE_DOUBLE                   5
#define SDK_LINE_STYLE_THINTHICK_SMALLGAP       6
#define SDK_LINE_STYLE_THINTHICK_MEDIUMGAP      7
#define SDK_LINE_STYLE_THINTHICK_LARGEGAP       8
#define SDK_LINE_STYLE_THICKTHIN_SMALLGAP       9
#define SDK_LINE_STYLE_THICKTHIN_MEDIUMGAP      10
#define SDK_LINE_STYLE_THICKTHIN_LARGEGAP       11
#define SDK_LINE_STYLE_EMBOSSED                 12
#define SDK_LINE_STYLE_ENGRAVED                 13
#define SDK_LINE_STYLE_OUTSET                   14
#define SDK_LINE_STYLE_INSET                    15
#define SDK_LINE_STYLE_FINE_DASHED              16
#define SDK_LINE_STYLE_DOUBLE_THIN              17
#define SDK_LINE_STYLE_DASH_DOT                 18
#define SDK_LINE_STYLE_DASH_DOT_DOT             19
```

### Border for a group of cells

```
NAP_XLS_CELLS_BORDER(O_XLS, 0, 0, 0, 10, 8, SDK_LINE_STYLE_SOLID, 50, NAP_OFFICE_RGB(255, 255, 255))

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Column-left index (0-based).
PAR4: Row-top index (0-based).
PAR5: Column-right index >= column-left.
PAR6: Row-bottom index >= row-top.
PAR7: Line style.
PAR8: Line thickness (integer 1/100th mm)
PAR9: Color value returned by NAP_OFFICE_RGB.
```

> **Important:** The inner lines will not be drawn. Only the border of entire group.

### Merge cells

```
NAP_XLS_CELLS_MERGE(O_XLS, 0, 2, 8, 3, 8)

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

### Row Visible

```
NAP_XLS_ROW_VISIBLE(O_XLS, 0, 0, .T.)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Row index (0-based).
PAR4: Row visible (.T. / .F.).
```

### Row Optimal Height

```
NAP_XLS_ROW_OPTIMAL_HEIGHT(O_XLS, 0, 0, .F.)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Row index (0-based).
PAR4: Row with optimal height (.T. / .F.).
```

### Row Height

```
NAP_XLS_ROW_HEIGHT(O_XLS, 0, 0, 715)

PAR1: The sheet document.
PAR2: Page index (0-based).
PAR3: Row index (0-based).
PAR4: Row height (integer 1/100th mm)
```
