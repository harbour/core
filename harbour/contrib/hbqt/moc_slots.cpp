/****************************************************************************
** Meta object code from reading C++ file hbqt_slots.h
**
** Created: Sat Jun 13 14:02:48 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "hbqt_slots.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file hbqt_slots.h doesnt include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Slots[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      17,   12, // methods
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
     171,    6,    6,    6, 0x0a,
     187,  108,    6,    6, 0x0a,
     214,    6,    6,    6, 0x0a,
     243,  237,  232,    6, 0x0a,
     258,  237,    6,    6, 0x0a,
     284,  237,    6,    6, 0x0a,
     320,  313,    6,    6, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Slots[] = {
    "Slots\0\0clicked()\0triggered()\0checked\0"
    "triggered(bool)\0hovered()\0state\0"
    "stateChanged(int)\0pressed()\0released()\0"
    "index\0activated(int)\0currentIndexChanged(int)\0"
    "highlighted(int)\0returnPressed()\0"
    "clicked_model(QModelIndex)\0viewportEntered()\0"
    "bool\0event\0event(QEvent*)\0"
    "keyPressEvent(QKeyEvent*)\0"
    "mouseMoveEvent(QMouseEvent*)\0action\0"
    "hovered(QAction*)\0"
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
        case 10: returnPressed(); break;
        case 11: clicked_model((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 12: viewportEntered(); break;
        case 13: { bool _r = event((*reinterpret_cast< QEvent*(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 14: keyPressEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 15: mouseMoveEvent((*reinterpret_cast< QMouseEvent*(*)>(_a[1]))); break;
        case 16: hovered((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 17;
    }
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
QT_END_MOC_NAMESPACE

