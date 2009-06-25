/****************************************************************************
** Meta object code from reading C++ file 'hbqt_slots.h'
**
** Created: Wed Jun 24 22:45:58 2009
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
static const uint qt_meta_data_Slots[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      40,   12, // methods
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
     264,  258,    6,    6, 0x0a,
     290,  258,    6,    6, 0x0a,
     326,  319,    6,    6, 0x0a,
     344,  108,    6,    6, 0x0a,
     364,  319,    6,    6, 0x0a,
     393,  385,    6,    6, 0x0a,
     421,  415,    6,    6, 0x0a,
     438,    6,    6,    6, 0x0a,
     454,    6,    6,    6, 0x0a,
     471,  415,    6,    6, 0x0a,
     499,  489,    6,    6, 0x0a,
     530,    6,    6,    6, 0x0a,
     548,    6,    6,    6, 0x0a,
     564,    6,    6,    6, 0x0a,
     588,  583,    6,    6, 0x0a,
     609,  583,    6,    6, 0x0a,
     646,  629,    6,    6, 0x0a,
     712,  700,    6,    6, 0x0a,
     748,  700,    6,    6, 0x0a,
     782,  700,    6,    6, 0x0a,
     821,  816,    6,    6, 0x0a,
     853,  700,    6,    6, 0x0a,
     893,  700,    6,    6, 0x0a,
     927,  816,    6,    6, 0x0a,
     958,  700,    6,    6, 0x0a,
     992,    6,    6,    6, 0x0a,

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
    "event\0keyPressEvent(QKeyEvent*)\0"
    "mouseMoveEvent(QMouseEvent*)\0action\0"
    "hovered(QAction*)\0currentChanged(int)\0"
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
    "itemSelectionChanged()\0"
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
        case 14: keyPressEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 15: mouseMoveEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 16: hovered((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        case 17: currentChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 18: actionTriggered((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 19: rangeChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 20: sliderMoved((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 21: sliderPressed(); break;
        case 22: sliderReleased(); break;
        case 23: valueChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 24: cursorPositionChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 25: editingFinished(); break;
        case 26: returnPressed(); break;
        case 27: selectionChanged(); break;
        case 28: textChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 29: textEdited((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 30: currentItemChanged((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< QTreeWidgetItem*(*)>(_a[2]))); break;
        case 31: itemActivated((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 32: itemChanged((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 33: itemClicked((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 34: itemCollapsed((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1]))); break;
        case 35: itemDoubleClicked((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 36: itemEntered((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 37: itemExpanded((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1]))); break;
        case 38: itemPressed((*reinterpret_cast< QTreeWidgetItem*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 39: itemSelectionChanged(); break;
        default: ;
        }
        _id -= 40;
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
