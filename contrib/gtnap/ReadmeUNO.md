# GTNap-UNO

Harbour interface for access to LibreOffice documents

## SpreadSheet

### Open Sheet

```
LOCAL O_XLS := NAP_XLS_OPEN({|| NAP_WORK_PATH() + "/../office/empty.ods" })

PAR1: Block that returns the full path for the document.
RET: The document object. Must be closed with NAP_XLS_CLOSE().
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


