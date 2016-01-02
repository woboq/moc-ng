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

/* Export to the Qt Binary Json format */

#pragma once

#include <map>
#include <string>
#include <vector>

namespace llvm {
namespace yaml {

class Node;
}

class raw_ostream;
}

namespace QBJS {
    enum Type {
        Null =  0x0,
        Bool = 0x1,
        Double = 0x2,
        String = 0x3,
        Array = 0x4,
        Object = 0x5,
        Undefined = 0x80
    };

    struct Value {
        Value() = default;
        Value(std::string S) : T(String) , Str(std::move(S)) {}
        Value(double D) : T(Double) , D(D) {}
        Value(bool B) : T(Bool) , D(B ? 1. : -1.) {}
        Type T = Undefined;
        std::map<std::string,Value> Props; // for Object
        std::vector<Value> Elems; // For Array
        std::string Str;
        double D = 0.;
        mutable int S = -1;

        int Size() const { if(S<0)S=ComputeSize(); return S; }
        int ComputeSize() const;
    };


    struct Stream {
        Stream(llvm::raw_ostream &OS) : OS(OS){}
        Stream &operator << (const Value &);

    private:
        int Col = 0;
        llvm::raw_ostream &OS;
        Stream &operator << (const std::string &Str);
        Stream &operator << (uint32_t);
        Stream &operator << (uint16_t);
        Stream &operator << (unsigned char);
    };

    bool Parse(llvm::yaml::Node *Node, Value &Root);
}
