/****************************************************************************
 *  Copyright (C) 2013-2016 Woboq GmbH
 *  Olivier Goffart <contact at woboq.com>
 *  https://woboq.com/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <QtTest/QtTest>

class tst_AutoReturn : public QObject
{ Q_OBJECT
private slots:
    void autoReturn_data();
    void autoReturn();

    void autoReturnTemplate();
};


struct AutoReturnClass  :public QObject
{ Q_OBJECT
    static AutoReturnClass* someFunc();
public:
    struct LocalClass {};
public slots:
    auto trailingTypeSlot(int i) -> int { return i; }
    decltype(someFunc()) declTypeSlot(int i) { return i ? nullptr: this; }
    auto trailingDeclTypeSlot(int *i) -> decltype(12 + *i) { return 42; }
    auto autoDeducedSlot(double d) { return d; }
    void declTypeParamSlot(decltype(someFunc()) i) { Q_UNUSED(i); }
signals:
    auto trailingTypeSignal(int i) -> int;
    decltype(someFunc()) declTypeSignal(int i);
    auto trailingDeclTypeSignal(int *i) -> decltype(*i + 5);

    void declTypeParamSignal(decltype(someFunc()) i);

    LocalClass returnLocalClassSignal();
};

Q_DECLARE_METATYPE(QMetaMethod::MethodType);

void tst_AutoReturn::autoReturn_data()
{

    QTest::addColumn<QByteArray>("method");
    QTest::addColumn<QByteArray>("returnType");
    QTest::addColumn<QMetaMethod::MethodType>("methodType");
    typedef QByteArray _;

    QTest::newRow("trailingTypeSlot") << _("trailingTypeSlot(int)") << _("int") << QMetaMethod::Slot;
    QTest::newRow("declTypeSlot") << _("declTypeSlot(int)") << _("AutoReturnClass*") << QMetaMethod::Slot;
    QTest::newRow("trailingDeclTypeSlot") << _("trailingDeclTypeSlot(int*)") << _("int") << QMetaMethod::Slot;
    QTest::newRow("autoDeducedSlot") << _("autoDeducedSlot(double)") << _("double") << QMetaMethod::Slot;
    QTest::newRow("declTypeParamSlot") << _("declTypeParamSlot(AutoReturnClass*)") << _("void") << QMetaMethod::Slot;
    QTest::newRow("trailingTypeSignal") << _("trailingTypeSignal(int)") << _("int") << QMetaMethod::Signal;
    QTest::newRow("declTypeSignal") << _("declTypeSignal(int)") << _("AutoReturnClass*") << QMetaMethod::Signal;
    QTest::newRow("trailingDeclTypeSignal") << _("trailingDeclTypeSignal(int*)") << _("int") << QMetaMethod::Signal;
    QTest::newRow("declTypeParamSignal") << _("declTypeParamSignal(AutoReturnClass*)") << _("void") << QMetaMethod::Signal;
    QTest::newRow("returnLocalClassSignal") << _("returnLocalClassSignal()") << _("LocalClass") << QMetaMethod::Signal;
}

void tst_AutoReturn::autoReturn()
{
    QFETCH(QByteArray, method);
    QFETCH(QByteArray, returnType);
    QFETCH(QMetaMethod::MethodType, methodType);

    int i = AutoReturnClass::staticMetaObject.indexOfMethod(method);
    QVERIFY(i > 0);
    QMetaMethod m = AutoReturnClass::staticMetaObject.method(i);
    QCOMPARE(QByteArray(m.typeName()), returnType);
    QCOMPARE(m.methodType(), methodType);
}


template<typename T>
class AutoReturnTemplate : public QObject {
    Q_OBJECT
    static T foobar();
signals:
    auto mySignal(decltype(std::declval<T>() + 1) i) -> decltype(foobar() + 4);
public slots:
    auto mySlot1(decltype(std::declval<T>() + 1) i) -> decltype(i + 4) { return i; }
    auto mySlot2(decltype(std::declval<T>() + 1) i) { return i+54; }
};

void tst_AutoReturn::autoReturnTemplate()
{
    {
        AutoReturnTemplate<int> a;
        QVERIFY(connect(&a, & AutoReturnTemplate<int>::mySignal, [] (int x){ return x+34; }));
        QCOMPARE(a.mySignal(3) , (3+34));
    }
    {
        AutoReturnTemplate<double> a;
        QVERIFY(connect(&a, &AutoReturnTemplate<double>::mySignal, [] (double x){ return x+34; }));
        QCOMPARE(a.mySignal(7) , (7.+34));
    }
}



QTEST_MAIN(tst_AutoReturn)

#include "tst_autoreturn.moc"


