/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#include "qbjs.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/YAMLParser.h>
#include <llvm/Support/Casting.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Twine.h>

#include <iostream>

int QBJS::Value::ComputeSize() const
{
    switch(T) {
        case Undefined: return 0;
        case Null: return 0;
        case Bool: return 0;
        case String: return 2 + Str.size(); //FIXME: Unicode
        case Double: return 8; // FIXME: int optimisation
        case Array:
        case Object:
            break;
        default:
            return -1;
    }

    //Object or Array;
    int K = 0;
    int D = 0;
    for (const auto &E : Props) {
        K += 2 + E.first.size(); // FIXME: unicode
        D += E.second.Size();
    }
    if (T == Object)
        D += K;
    return 12 + 8 * Props.size() + D;
}


QBJS::Stream& QBJS::Stream::operator<<(const QBJS::Value &V)
{
    if (V.T == Undefined)
        return *this;
    else if (V.T == Object || V.T==Array) {
        llvm::SmallVector<int, 128> Table;

        (*this) << uint32_t(V.Size());

        uint32_t Len = V.Props.size() << 1;
        if (V.T == Object)
            Len |= 1;
        (*this) << Len;
        (*this) << uint32_t(V.Size() - V.Props.size() * 4);

        uint32_t Off = 12;
        for (auto E : V.Props) {
            Table.push_back(Off);
            uint32_t H = E.second.T & 0x7;
            // FIXME: Unicode (for two cases)
            if (E.second.T == String)
                H |= 1<<3;
            if (V.T == Object) {
                H |= 1<<4;
                Off += 2 + E.first.size();
            }
            Off += 4;
            if (E.second.T == Bool) {
                if (E.second.D > 0)
                    H |= 1 << 5;
            } else {
                H |= uint32_t(Off << 5);
            }
            (*this) << H;
            Off += E.second.Size();
            if (V.T == Object)
                (*this) << E.first;
            (*this) << E.second;
        }
        for (uint32_t T : Table) {
            (*this) << T;
        }
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

QBJS::Stream& QBJS::Stream::operator<<(std::string Str)
{
    (*this) << uint16_t(Str.size());
    for (unsigned char S : Str) (*this) << S;
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
        Root.T = Object; //FIXME
        int idx = 0;
        for (llvm::yaml::SequenceNode::iterator AI = Array->begin(), AE = Array->end();
             AI != AE; ++AI) {
            if (!Parse(AI, Root.Props[llvm::Twine(idx++).str()]))
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




