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

class tst_Nested : public QObject
{ Q_OBJECT
private slots:
    void nestedClass();

public:
    class Nested : public QObject {
        Q_OBJECT
    signals:
        int mySignal();
    public:
        int mem;
        Q_PROPERTY(int mem MEMBER mem);
    };

};

void tst_Nested::nestedClass()
{
    Nested n;
    QObject *nn = &n;
    QCOMPARE(qobject_cast<Nested*>(nn), &n);
    QCOMPARE(nn->metaObject()->className(), "tst_Nested::Nested");

    QVERIFY(nn->setProperty("mem", 345));
    QCOMPARE(nn->property("mem"), QVariant(345));

    QVERIFY(connect(&n, &Nested::mySignal, []{return 432;}));
    QCOMPARE(n.mySignal(), 432);
}


QTEST_MAIN(tst_Nested)

#include "tst_nested.moc"


