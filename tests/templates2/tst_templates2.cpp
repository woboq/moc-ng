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

#include "templatedobj.h"
#include <QtTest/QtTest>

class tst_Templates2 : public QObject
{ Q_OBJECT
private slots:
    void test1();
};

void tst_Templates2::test1()
{
    TemplatedObj<double> dObj;
    NormalObj nObj;
    connect(&dObj, &TemplatedObj<double>::mySignal, &nObj, &TemplatedObj<int>::mySignal);
    connect(&nObj, &TemplatedObj<int>::mySignal, &nObj, &NormalObj::mySignal2);
    connect(&nObj, &NormalObj::mySignal2, &nObj, &NormalObj::activate);
    dObj.mySignal();
    QCOMPARE(nObj.activated, 1);
}


QTEST_MAIN(tst_Templates2)

#include "tst_templates2.moc"


