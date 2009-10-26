/****************************************************************************
** Meta object code from reading C++ file 'hbqt_slots.h'
**
** Created: Sun Oct 25 19:49:13 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "hbqt_slots.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'hbqt_slots.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MyMainWindow[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

       0        // eod
};

static const char qt_meta_stringdata_MyMainWindow[] = {
    "MyMainWindow\0"
};

const QMetaObject MyMainWindow::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_MyMainWindow,
      qt_meta_data_MyMainWindow, 0 }
};

const QMetaObject *MyMainWindow::metaObject() const
{
    return &staticMetaObject;
}

void *MyMainWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MyMainWindow))
        return static_cast<void*>(const_cast< MyMainWindow*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int MyMainWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_MyDrawingArea[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      21,   15,   14,   14, 0x05,
      53,   15,   14,   14, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MyDrawingArea[] = {
    "MyDrawingArea\0\0event\0"
    "sg_mouseMoveEvent(QMouseEvent*)\0"
    "sg_keyPressEvent(QKeyEvent*)\0"
};

const QMetaObject MyDrawingArea::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MyDrawingArea,
      qt_meta_data_MyDrawingArea, 0 }
};

const QMetaObject *MyDrawingArea::metaObject() const
{
    return &staticMetaObject;
}

void *MyDrawingArea::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MyDrawingArea))
        return static_cast<void*>(const_cast< MyDrawingArea*>(this));
    return QWidget::qt_metacast(_clname);
}

int MyDrawingArea::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: sg_mouseMoveEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 1: sg_keyPressEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MyDrawingArea::sg_mouseMoveEvent(QMouseEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MyDrawingArea::sg_keyPressEvent(QKeyEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
static const uint qt_meta_data_HbDbfModel[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

       0        // eod
};

static const char qt_meta_stringdata_HbDbfModel[] = {
    "HbDbfModel\0"
};

const QMetaObject HbDbfModel::staticMetaObject = {
    { &QAbstractItemModel::staticMetaObject, qt_meta_stringdata_HbDbfModel,
      qt_meta_data_HbDbfModel, 0 }
};

const QMetaObject *HbDbfModel::metaObject() const
{
    return &staticMetaObject;
}

void *HbDbfModel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_HbDbfModel))
        return static_cast<void*>(const_cast< HbDbfModel*>(this));
    return QAbstractItemModel::qt_metacast(_clname);
}

int HbDbfModel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QAbstractItemModel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_HbTableView[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       9,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      19,   13,   12,   12, 0x05,
      48,   13,   12,   12, 0x05,
      80,   13,   12,   12, 0x05,
     119,   13,   12,   12, 0x05,
     152,   13,   12,   12, 0x05,
     187,   13,   12,   12, 0x05,
     215,   13,   12,   12, 0x05,
     268,  245,   12,   12, 0x05,
     335,  331,   12,   12, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_HbTableView[] = {
    "HbTableView\0\0event\0sg_keyPressEvent(QKeyEvent*)\0"
    "sg_mouseMoveEvent(QMouseEvent*)\0"
    "sg_mouseDoubleClickEvent(QMouseEvent*)\0"
    "sg_mousePressEvent(QMouseEvent*)\0"
    "sg_mouseReleaseEvent(QMouseEvent*)\0"
    "sg_wheelEvent(QWheelEvent*)\0"
    "sg_resizeEvent(QResizeEvent*)\0"
    "cursorAction,modifiers\0"
    "sg_moveCursor(HbTableView::CursorAction,Qt::KeyboardModifiers)\0"
    "x,y\0sg_scrollContentsBy(int,int)\0"
};

const QMetaObject HbTableView::staticMetaObject = {
    { &QTableView::staticMetaObject, qt_meta_stringdata_HbTableView,
      qt_meta_data_HbTableView, 0 }
};

const QMetaObject *HbTableView::metaObject() const
{
    return &staticMetaObject;
}

void *HbTableView::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_HbTableView))
        return static_cast<void*>(const_cast< HbTableView*>(this));
    return QTableView::qt_metacast(_clname);
}

int HbTableView::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QTableView::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: sg_keyPressEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 1: sg_mouseMoveEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 2: sg_mouseDoubleClickEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 3: sg_mousePressEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 4: sg_mouseReleaseEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 5: sg_wheelEvent((*reinterpret_cast< QWheelEvent*(*)>(_a[1]))); break;
        case 6: sg_resizeEvent((*reinterpret_cast< QResizeEvent*(*)>(_a[1]))); break;
        case 7: sg_moveCursor((*reinterpret_cast< HbTableView::CursorAction(*)>(_a[1])),(*reinterpret_cast< Qt::KeyboardModifiers(*)>(_a[2]))); break;
        case 8: sg_scrollContentsBy((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        default: ;
        }
        _id -= 9;
    }
    return _id;
}

// SIGNAL 0
void HbTableView::sg_keyPressEvent(QKeyEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void HbTableView::sg_mouseMoveEvent(QMouseEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void HbTableView::sg_mouseDoubleClickEvent(QMouseEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void HbTableView::sg_mousePressEvent(QMouseEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void HbTableView::sg_mouseReleaseEvent(QMouseEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void HbTableView::sg_wheelEvent(QWheelEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void HbTableView::sg_resizeEvent(QResizeEvent * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void HbTableView::sg_moveCursor(HbTableView::CursorAction _t1, Qt::KeyboardModifiers _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void HbTableView::sg_scrollContentsBy(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}
static const uint qt_meta_data_Slots[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
     113,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
       7,    6,    6,    6, 0x0a,
      17,    6,    6,    6, 0x0a,
      37,   29,    6,    6, 0x0a,
      53,    6,    6,    6, 0x0a,
      69,   63,    6,    6, 0x0a,
      87,    6,    6,    6, 0x0a,
      97,    6,    6,    6, 0x0a,
     114,  108,    6,    6, 0x0a,
     129,  108,    6,    6, 0x0a,
     154,  108,    6,    6, 0x0a,
     171,  108,    6,    6, 0x0a,
     192,  108,    6,    6, 0x0a,
     219,  108,    6,    6, 0x0a,
     240,    6,    6,    6, 0x0a,
     265,  258,    6,    6, 0x0a,
     283,  108,    6,    6, 0x0a,
     303,  258,    6,    6, 0x0a,
     332,  324,    6,    6, 0x0a,
     360,  354,    6,    6, 0x0a,
     377,    6,    6,    6, 0x0a,
     393,    6,    6,    6, 0x0a,
     410,  354,    6,    6, 0x0a,
     438,  428,    6,    6, 0x0a,
     469,    6,    6,    6, 0x0a,
     487,    6,    6,    6, 0x0a,
     503,    6,    6,    6, 0x0a,
     527,  522,    6,    6, 0x0a,
     548,  522,    6,    6, 0x0a,
     585,  568,    6,    6, 0x0a,
     651,  639,    6,    6, 0x0a,
     687,  639,    6,    6, 0x0a,
     721,  639,    6,    6, 0x0a,
     760,  755,    6,    6, 0x0a,
     792,  639,    6,    6, 0x0a,
     832,  639,    6,    6, 0x0a,
     866,  755,    6,    6, 0x0a,
     897,  639,    6,    6, 0x0a,
     931,    6,    6,    6, 0x0a,
     954,    6,    6,    6, 0x0a,
     991,  972,    6,    6, 0x0a,
    1041, 1033,    6,    6, 0x0a,
    1082, 1076,    6,    6, 0x0a,
    1112, 1107,    6,    6, 0x0a,
    1147, 1143,    6,    6, 0x0a,
    1188, 1165,    6,    6, 0x0a,
    1228, 1225,    6,    6, 0x0a,
    1256, 1247,    6,    6, 0x0a,
    1274,    6,    6,    6, 0x0a,
    1296, 1288,    6,    6, 0x0a,
    1335,    6,    6,    6, 0x0a,
    1355, 1076,    6,    6, 0x0a,
    1392, 1382,    6,    6, 0x0a,
    1416, 1076,    6,    6, 0x0a,
    1466, 1455,    6,    6, 0x0a,
    1538, 1519,    6,    6, 0x0a,
    1569,  522,    6,    6, 0x0a,
    1595, 1288,    6,    6, 0x0a,
    1636, 1288,    6,    6, 0x0a,
    1681, 1675,    6,    6, 0x0a,
    1716,    6,    6,    6, 0x0a,
    1739,    6,    6,    6, 0x0a,
    1759, 1753,    6,    6, 0x0a,
    1781, 1143,    6,    6, 0x0a,
    1803, 1798,    6,    6, 0x0a,
    1829, 1798,    6,    6, 0x0a,
    1849,    6,    6,    6, 0x0a,
    1867, 1860,    6,    6, 0x0a,
    1881,    6,    6,    6, 0x0a,
    1897, 1892,    6,    6, 0x0a,
    1931, 1921,    6,    6, 0x0a,
    1962, 1957,    6,    6, 0x0a,
    1993, 1984,    6,    6, 0x0a,
    2027, 2020,    6,    6, 0x0a,
    2059, 2051,    6,    6, 0x0a,
    2083, 2079,    6,    6, 0x0a,
    2105, 2103,    6,    6, 0x0a,
    2147,    6,    6,    6, 0x0a,
    2181, 2171,    6,    6, 0x0a,
    2201,    6,    6,    6, 0x0a,
    2215, 2171,    6,    6, 0x0a,
    2235,    6,    6,    6, 0x0a,
    2251, 2245,    6,    6, 0x0a,
    2277, 2245,    6,    6, 0x0a,
    2305, 2245,    6,    6, 0x0a,
    2334, 2245,    6,    6, 0x0a,
    2364, 2245,    6,    6, 0x0a,
    2396, 2245,    6,    6, 0x0a,
    2432, 2245,    6,    6, 0x0a,
    2457, 2245,    6,    6, 0x0a,
    2488, 2484,    6,    6, 0x0a,
    2514,    6,    6,    6, 0x0a,
    2552, 2534,    6,    6, 0x0a,
    2612, 2599,    6,    6, 0x0a,
    2650, 2632,    6,    6, 0x0a,
    2679, 2599,    6,    6, 0x0a,
    2705, 2599,    6,    6, 0x0a,
    2725, 2599,    6,    6, 0x0a,
    2800, 2757,    6,    6, 0x0a,
    2826, 2599,    6,    6, 0x0a,
    2875, 2846,    6,    6, 0x0a,
    2922, 2903,    6,    6, 0x0a,
    2965, 2962,    6,    6, 0x0a,
    2984, 2962,    6,    6, 0x0a,
    3003, 2962,    6,    6, 0x0a,
    3028, 3023,    6,    6, 0x0a,
    3051, 3023,    6,    6, 0x0a,
    3083, 3072,    6,    6, 0x0a,
    3106, 3072,    6,    6, 0x0a,
    3127, 3072,    6,    6, 0x0a,
    3148, 3072,    6,    6, 0x0a,
    3175, 3072,    6,    6, 0x0a,
    3196, 3072,    6,    6, 0x0a,
    3269, 3217,    6,    6, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Slots[] = {
    "Slots\0\0clicked()\0triggered()\0checked\0"
    "triggered(bool)\0hovered()\0state\0"
    "stateChanged(int)\0pressed()\0released()\0"
    "index\0activated(int)\0currentIndexChanged(int)\0"
    "highlighted(int)\0clicked(QModelIndex)\0"
    "doubleClicked(QModelIndex)\0"
    "entered(QModelIndex)\0viewportEntered()\0"
    "action\0hovered(QAction*)\0currentChanged(int)\0"
    "actionTriggered(int)\0min,max\0"
    "rangeChanged(int,int)\0value\0"
    "sliderMoved(int)\0sliderPressed()\0"
    "sliderReleased()\0valueChanged(int)\0"
    "iOld,iNew\0cursorPositionChanged(int,int)\0"
    "editingFinished()\0returnPressed()\0"
    "selectionChanged()\0text\0textChanged(QString)\0"
    "textEdited(QString)\0current,previous\0"
    "currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)\0"
    "item,column\0itemActivated(QTreeWidgetItem*,int)\0"
    "itemChanged(QTreeWidgetItem*,int)\0"
    "itemClicked(QTreeWidgetItem*,int)\0"
    "item\0itemCollapsed(QTreeWidgetItem*)\0"
    "itemDoubleClicked(QTreeWidgetItem*,int)\0"
    "itemEntered(QTreeWidgetItem*,int)\0"
    "itemExpanded(QTreeWidgetItem*)\0"
    "itemPressed(QTreeWidgetItem*,int)\0"
    "itemSelectionChanged()\0contentsChanged()\0"
    "frame,databaseName\0"
    "databaseQuotaExceeded(QWebFrame*,QString)\0"
    "request\0downloadRequested(QNetworkRequest)\0"
    "frame\0frameCreated(QWebFrame*)\0geom\0"
    "geometryChangeRequested(QRect)\0url\0"
    "linkClicked(QUrl)\0link,title,textContent\0"
    "linkHovered(QString,QString,QString)\0"
    "ok\0loadFinished(bool)\0progress\0"
    "loadProgress(int)\0loadStarted()\0visible\0"
    "menuBarVisibilityChangeRequested(bool)\0"
    "microFocusChanged()\0printRequested(QWebFrame*)\0"
    "dirtyRect\0repaintRequested(QRect)\0"
    "restoreFrameStateRequested(QWebFrame*)\0"
    "frame,item\0"
    "saveFrameStateRequested(QWebFrame*,QWebHistoryItem*)\0"
    "dx,dy,rectToScroll\0scrollRequested(int,int,QRect)\0"
    "statusBarMessage(QString)\0"
    "statusBarVisibilityChangeRequested(bool)\0"
    "toolBarVisibilityChangeRequested(bool)\0"
    "reply\0unsupportedContent(QNetworkReply*)\0"
    "windowCloseRequested()\0iconChanged()\0"
    "title\0titleChanged(QString)\0"
    "urlChanged(QUrl)\0font\0currentFontChanged(QFont)\0"
    "fontSelected(QFont)\0accepted()\0result\0"
    "finished(int)\0rejected()\0path\0"
    "currentChanged(QString)\0directory\0"
    "directoryEntered(QString)\0file\0"
    "fileSelected(QString)\0selected\0"
    "filesSelected(QStringList)\0filter\0"
    "filterSelected(QString)\0printer\0"
    "accepted(QPrinter*)\0yes\0copyAvailable(bool)\0"
    "f\0currentCharFormatChanged(QTextCharFormat)\0"
    "cursorPositionChanged()\0available\0"
    "redoAvailable(bool)\0textChanged()\0"
    "undoAvailable(bool)\0timeout()\0event\0"
    "keyPressEvent(QKeyEvent*)\0"
    "keyReleaseEvent(QKeyEvent*)\0"
    "mouseMoveEvent(QMouseEvent*)\0"
    "mousePressEvent(QMouseEvent*)\0"
    "mouseReleaseEvent(QMouseEvent*)\0"
    "mouseDoubleClickEvent(QMouseEvent*)\0"
    "wheelEvent(QWheelEvent*)\0"
    "resizeEvent(QResizeEvent*)\0x,y\0"
    "scrollContentsBy(int,int)\0geometriesChanged()\0"
    "logicalIndex,mode\0"
    "sectionAutoResize(int,QHeaderView::ResizeMode)\0"
    "logicalIndex\0sectionClicked(int)\0"
    "oldCount,newCount\0sectionCountChanged(int,int)\0"
    "sectionDoubleClicked(int)\0sectionEntered(int)\0"
    "sectionHandleDoubleClicked(int)\0"
    "logicalIndex,oldVisualIndex,newVisualIndex\0"
    "sectionMoved(int,int,int)\0sectionPressed(int)\0"
    "logicalIndex,oldSize,newSize\0"
    "sectionResized(int,int,int)\0"
    "logicalIndex,order\0"
    "sortIndicatorChanged(int,Qt::SortOrder)\0"
    "id\0buttonClicked(int)\0buttonPressed(int)\0"
    "buttonReleased(int)\0link\0"
    "linkActivated(QString)\0linkHovered(QString)\0"
    "row,column\0cellActivated(int,int)\0"
    "cellChanged(int,int)\0cellClicked(int,int)\0"
    "cellDoubleClicked(int,int)\0"
    "cellEntered(int,int)\0cellPressed(int,int)\0"
    "currentRow,currentColumn,previousRow,previousColumn\0"
    "currentCellChanged(int,int,int,int)\0"
};

const QMetaObject Slots::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Slots,
      qt_meta_data_Slots, 0 }
};

const QMetaObject *Slots::metaObject() const
{
    return &staticMetaObject;
}

void *Slots::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Slots))
        return static_cast<void*>(const_cast< Slots*>(this));
    return QObject::qt_metacast(_clname);
}

int Slots::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: clicked(); break;
        case 1: triggered(); break;
        case 2: triggered((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 3: hovered(); break;
        case 4: stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: pressed(); break;
        case 6: released(); break;
        case 7: activated((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 8: currentIndexChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 9: highlighted((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 10: clicked((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 11: doubleClicked((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 12: entered((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 13: viewportEntered(); break;
        case 14: hovered((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        case 15: currentChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 16: actionTriggered((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 17: rangeChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 18: sliderMoved((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 19: sliderPressed(); break;
        case 20: sliderReleased(); break;
        case 21: valueChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 22: cursorPositionChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 23: editingFinished(); break;
        case 24: returnPressed(); break;
        case 25: selectionChanged(); break;
        case 26: textChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 27: textEdited((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 28: currentItemChanged((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< QTreeWidgetItem*(*)>(_a[2]))); break;
        case 29: itemActivated((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 30: itemChanged((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 31: itemClicked((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 32: itemCollapsed((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1]))); break;
        case 33: itemDoubleClicked((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 34: itemEntered((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 35: itemExpanded((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1]))); break;
        case 36: itemPressed((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 37: itemSelectionChanged(); break;
        case 38: contentsChanged(); break;
        case 39: databaseQuotaExceeded((*reinterpret_cast< QWebFrame*(*)>(_a[1])),(*reinterpret_cast< QString(*)>(_a[2]))); break;
        case 40: downloadRequested((*reinterpret_cast< const QNetworkRequest(*)>(_a[1]))); break;
        case 41: frameCreated((*reinterpret_cast< QWebFrame*(*)>(_a[1]))); break;
        case 42: geometryChangeRequested((*reinterpret_cast< const QRect(*)>(_a[1]))); break;
        case 43: linkClicked((*reinterpret_cast< const QUrl(*)>(_a[1]))); break;
        case 44: linkHovered((*reinterpret_cast< const QString(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2])),(*reinterpret_cast< const QString(*)>(_a[3]))); break;
        case 45: loadFinished((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 46: loadProgress((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 47: loadStarted(); break;
        case 48: menuBarVisibilityChangeRequested((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 49: microFocusChanged(); break;
        case 50: printRequested((*reinterpret_cast< QWebFrame*(*)>(_a[1]))); break;
        case 51: repaintRequested((*reinterpret_cast< const QRect(*)>(_a[1]))); break;
        case 52: restoreFrameStateRequested((*reinterpret_cast< QWebFrame*(*)>(_a[1]))); break;
        case 53: saveFrameStateRequested((*reinterpret_cast< QWebFrame*(*)>(_a[1])),(*reinterpret_cast< QWebHistoryItem*(*)>(_a[2]))); break;
        case 54: scrollRequested((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< const QRect(*)>(_a[3]))); break;
        case 55: statusBarMessage((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 56: statusBarVisibilityChangeRequested((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 57: toolBarVisibilityChangeRequested((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 58: unsupportedContent((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        case 59: windowCloseRequested(); break;
        case 60: iconChanged(); break;
        case 61: titleChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 62: urlChanged((*reinterpret_cast< const QUrl(*)>(_a[1]))); break;
        case 63: currentFontChanged((*reinterpret_cast< const QFont(*)>(_a[1]))); break;
        case 64: fontSelected((*reinterpret_cast< const QFont(*)>(_a[1]))); break;
        case 65: accepted(); break;
        case 66: finished((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 67: rejected(); break;
        case 68: currentChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 69: directoryEntered((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 70: fileSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 71: filesSelected((*reinterpret_cast< const QStringList(*)>(_a[1]))); break;
        case 72: filterSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 73: accepted((*reinterpret_cast< QPrinter*(*)>(_a[1]))); break;
        case 74: copyAvailable((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 75: currentCharFormatChanged((*reinterpret_cast< const QTextCharFormat(*)>(_a[1]))); break;
        case 76: cursorPositionChanged(); break;
        case 77: redoAvailable((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 78: textChanged(); break;
        case 79: undoAvailable((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 80: timeout(); break;
        case 81: keyPressEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 82: keyReleaseEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 83: mouseMoveEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 84: mousePressEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 85: mouseReleaseEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 86: mouseDoubleClickEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 87: wheelEvent((*reinterpret_cast< QWheelEvent*(*)>(_a[1]))); break;
        case 88: resizeEvent((*reinterpret_cast< QResizeEvent*(*)>(_a[1]))); break;
        case 89: scrollContentsBy((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 90: geometriesChanged(); break;
        case 91: sectionAutoResize((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< QHeaderView::ResizeMode(*)>(_a[2]))); break;
        case 92: sectionClicked((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 93: sectionCountChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 94: sectionDoubleClicked((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 95: sectionEntered((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 96: sectionHandleDoubleClicked((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 97: sectionMoved((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 98: sectionPressed((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 99: sectionResized((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 100: sortIndicatorChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< Qt::SortOrder(*)>(_a[2]))); break;
        case 101: buttonClicked((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 102: buttonPressed((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 103: buttonReleased((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 104: linkActivated((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 105: linkHovered((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 106: cellActivated((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 107: cellChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 108: cellClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 109: cellDoubleClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 110: cellEntered((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 111: cellPressed((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 112: currentCellChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3])),(*reinterpret_cast< int(*)>(_a[4]))); break;
        default: ;
        }
        _id -= 113;
    }
    return _id;
}
static const uint qt_meta_data_Events[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

       0        // eod
};

static const char qt_meta_stringdata_Events[] = {
    "Events\0"
};

const QMetaObject Events::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Events,
      qt_meta_data_Events, 0 }
};

const QMetaObject *Events::metaObject() const
{
    return &staticMetaObject;
}

void *Events::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Events))
        return static_cast<void*>(const_cast< Events*>(this));
    return QObject::qt_metacast(_clname);
}

int Events::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
