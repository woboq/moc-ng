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

#include "qbjs.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/YAMLParser.h>
#include <llvm/Support/Casting.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/Twine.h>

#include <iostream>

static int StringSize(const std::string &Str) {
    //FIXME: Unicode
    return (2 + Str.size() + 3) & ~3;
}

int QBJS::Value::ComputeSize() const
{
    int D = 0;
    switch(T) {
        case Undefined: return 0;
        case Null: return 0;
        case Bool: return 0;
        case String: return StringSize(Str);
        case Double: return 8; // FIXME: int optimisation
        case Array:
            for (const auto &E : Elems) D += E.Size() + 4;
            return 12 + D;
        case Object:
            for (const auto &E : Props) {
                D += StringSize(E.first);
                D += E.second.Size() + 8;
            }
            return 12 + D;
        default:
            return -1;
    }
}

static uint32_t ComputeHeader(const QBJS::Value &V, int Off) {
    using namespace QBJS;
    uint32_t H = V.T & 0x7;
    if (V.T == String)
        H |= 1<<3; // FIXME: Unicode
    if (V.T == Bool) {
        if (V.D > 0)
            H |= 1 << 5;
    } else {
        H |= uint32_t(Off << 5);
    }
    return H;
}


QBJS::Stream& QBJS::Stream::operator<<(const QBJS::Value &V)
{
    if (V.T == Undefined)
        return *this;
    else if (V.T == Object) {
        llvm::SmallVector<uint32_t, 128> Table;

        (*this) << uint32_t(V.Size());
        (*this) << uint32_t(1 | V.Props.size() << 1);
        (*this) << uint32_t(V.Size() - V.Props.size() * 4);

        uint32_t Off = 12;
        for (auto E : V.Props) {
            Table.push_back(Off);
            Off += 4 + StringSize(E.first);
            uint32_t H = ComputeHeader(E.second, Off);
            H |= 1<<4;
            (*this) << H << E.first << E.second;
            Off += E.second.Size();
        }
        for (uint32_t T : Table)
            (*this) << T;
    } else if (V.T == Array) {
        llvm::SmallVector<uint32_t, 128> Table;

        (*this) << uint32_t(V.Size());
        (*this) << uint32_t(V.Elems.size() << 1);
        (*this) << uint32_t(V.Size() - V.Elems.size() * 4);

        uint32_t Off = 12;
        for (auto E : V.Elems) {
            Table.push_back(ComputeHeader(E, Off));
            (*this) << E;
            Off += E.Size();
        }
        for (uint32_t T : Table)
            (*this) << T;
    } else if (V.T == Double) {
        // Hum Hum:
        uint64_t D;
        memcpy(&D, &V.D, sizeof(double));
        (*this) << uint32_t(D & 0xffffffff);
        (*this) << uint32_t(D >> 32);
    } else if (V.T == String) {
        (*this) << V.Str;
    }
    return *this;
}

QBJS::Stream& QBJS::Stream::operator<<(const std::string &Str)
{
    (*this) << uint16_t(Str.size());
    for (unsigned char S : Str) (*this) << S;
    for (int I = Str.size() + 2; (I % 4)!=0 ;++I)
        (*this) << (unsigned char)('\0'); //Padding;
    return *this;
}

QBJS::Stream& QBJS::Stream::operator<<(uint32_t I)
{
    (*this) << uint16_t(I) << uint16_t(I>>16);
    return *this;
}

QBJS::Stream& QBJS::Stream::operator<<(uint16_t I)
{
    typedef unsigned char uchar;
    (*this) << uchar(I) << uchar(I>>8);
    return *this;

}

QBJS::Stream& QBJS::Stream::operator<<(unsigned char C)
{
    OS << "0x";
    OS.write_hex(C);
    OS << ",";
    Col++;
    if (Col > 15) {
        Col = 0;
        OS << "\n   ";
    }
    OS << " ";
    return *this;
}


bool QBJS::Parse(llvm::yaml::Node* Node, QBJS::Value& Root)
{
    if (!Node) return false;
    if (llvm::yaml::SequenceNode *Array = llvm::dyn_cast<llvm::yaml::SequenceNode>(Node)) {
        Root.T = QBJS::Array;
        for (llvm::yaml::SequenceNode::iterator AI = Array->begin(), AE = Array->end();
             AI != AE; ++AI) {
            Root.Elems.emplace_back();
            if (!Parse(AI, Root.Elems.back()))
                return false;
        }
        return true;
    } else if (llvm::yaml::MappingNode *Object = llvm::dyn_cast<llvm::yaml::MappingNode>(Node)) {
        Root.T = QBJS::Object;
        for (llvm::yaml::MappingNode::iterator KVI = Object->begin(), KVE = Object->end();
             KVI != KVE; ++KVI) {

            llvm::yaml::ScalarNode *KeyString = llvm::dyn_cast<llvm::yaml::ScalarNode>((*KVI).getKey());
            if (!KeyString)
                return false;
            llvm::yaml::Node *Value = (*KVI).getValue();
            if (!Value) return false;
            llvm::SmallString<20> Storage;
            if (!Parse(Value, Root.Props[KeyString->getValue(Storage)]))
                return false;
        }
        return true;
    } else if (llvm::yaml::ScalarNode *Scal = llvm::dyn_cast<llvm::yaml::ScalarNode>(Node)) {
        llvm::SmallString<20> Storage;
        Root = std::string(Scal->getValue(Storage));
        // FIXME: integer
        return true;
    } else if (Node->getType() == llvm::yaml::Node::NK_Null) {
        Root.T = Null;
        return true;
    } else {
        return false;
    }
}




