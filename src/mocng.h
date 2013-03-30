/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/


#pragma once

#include <string>
#include <vector>

namespace clang {
class CXXMethodDecl;
class CXXRecordDecl;
class CXXConstructorDecl;
class EnumDecl;
class Preprocessor;
class Sema;
}


struct PropertyDef {
    std::string name, type, member, read, write, reset, designable, scriptable, editable, stored,
    user, notify, inPrivateClass;
    int notifyId = -1;
    bool constant = false;
    bool final = false;
/*
    // ### ???
    enum Specification  { ValueSpec, ReferenceSpec, PointerSpec };
    Specification gspec = ValueSpec;
*/
    int revision = 0;
};


struct ClassDef {

    clang::CXXRecordDecl *Record = nullptr;

    // This list only includes the things registered with the keywords
    std::vector<clang::CXXMethodDecl*> Signals;
    std::vector<clang::CXXMethodDecl*> Slots;
    std::vector<clang::CXXMethodDecl*> Methods;
    std::vector<clang::CXXConstructorDecl*> Constructors;
    std::vector<std::pair<clang::EnumDecl*, bool>> Enums;  //### or string?

    std::vector<std::pair<std::string, std::string>> Interfaces;
    std::vector<std::pair<std::string, std::string>> ClassInfo;

    std::vector<PropertyDef> Properties;

    bool HasQObject = false;
    bool HasQGadget = false;

    //TODO: PluginData;
    //TODO: FLagAliases;
};



ClassDef parseClass (clang::CXXRecordDecl* RD, clang::Sema& Sema);
