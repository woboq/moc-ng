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

#pragma once

#include <string>
#include <vector>

namespace clang {
class ASTContext;
class CXXMethodDecl;
class SourceManager;
class QualType;
}

#include <clang/AST/PrettyPrinter.h>
#include "mocng.h"

struct ClassDef;



// From qmetaobject_p.h
enum PropertyFlags  {
    Invalid = 0x00000000,
    Readable = 0x00000001,
    Writable = 0x00000002,
    Resettable = 0x00000004,
    EnumOrFlag = 0x00000008,
    StdCppSet = 0x00000100,
//     Override = 0x00000200,
    Constant = 0x00000400,
    Final = 0x00000800,
    Designable = 0x00001000,
    ResolveDesignable = 0x00002000,
    Scriptable = 0x00004000,
    ResolveScriptable = 0x00008000,
    Stored = 0x00010000,
    ResolveStored = 0x00020000,
    Editable = 0x00040000,
    ResolveEditable = 0x00080000,
    User = 0x00100000,
    ResolveUser = 0x00200000,
    Notify = 0x00400000,
    Revisioned = 0x00800000
};
enum MethodFlags  {
    AccessPrivate = 0x00,
    AccessProtected = 0x01,
    AccessPublic = 0x02,
    AccessMask = 0x03, //mask
    MethodMethod = 0x00,
    MethodSignal = 0x04,
    MethodSlot = 0x08,
    MethodConstructor = 0x0c,
    MethodTypeMask = 0x0c,
    MethodCompatibility = 0x10,
    MethodCloned = 0x20,
    MethodScriptable = 0x40,
    MethodRevisioned = 0x80
};
enum MetaObjectFlags {
    DynamicMetaObject = 0x01,
    RequiresVariantMetaObject = 0x02,
    PropertyAccessInStaticMetaCall = 0x04
};
enum MetaDataFlags {
    IsUnresolvedType = 0x80000000,
    TypeNameIndexMask = 0x7FFFFFFF
};


enum { OutputRevision = 7,
       MetaObjectPrivateFieldCount = 14, //  = sizeof(QMetaObjectPrivate) / sizeof(int)
       mocOutputRevision = 67,
       QT_VERSION = 0x050100
};

#define MOCNG_VERSION_STR "alpha 1"

class Generator {
    const BaseDef *Def;
    const ClassDef *CDef;
    llvm::raw_ostream& OS;
    llvm::raw_ostream& OS_TemplateHeader;

    std::vector<std::string> Strings;

    std::string QualName;
    std::string BaseName;
    std::string TemplatePrefix; // what is in front of the template declaration ("template<typename t>")
    bool BaseHasStaticMetaObject = false;
    bool HasTemplateHeader;
    int MethodCount;

    clang::ASTContext &Ctx;
    clang::PrintingPolicy PrintPolicy;

    MocNg *Moc;

public:
    explicit Generator(const ClassDef *CDef, llvm::raw_ostream& OS, clang::ASTContext & Ctx, MocNg *Moc,
              llvm::raw_ostream *OS_TemplateHeader = nullptr);
    // For namespaces
    explicit Generator(const NamespaceDef *NDef, llvm::raw_ostream& OS, clang::ASTContext & Ctx, MocNg *Moc);

    bool IsQtNamespace = false;

    // plugin metadata from -M command line argument  (to be put in the JSON)
    std::vector<std::pair<llvm::StringRef, llvm::StringRef>> MetaData;

    void GenerateCode();
private:

    int StrIdx(llvm::StringRef);
    template <typename T>
    void GenerateFunctions(const std::vector<T> &V, const char *TypeName, MethodFlags Type, int &ParamIndex);
    template <typename T>
    void GenerateFunctionParameters(const std::vector<T*> &V, const char *TypeName);

    void GenerateProperties();
    void GenerateMetaCall();
    void GenerateStaticMetaCall();
    void GenerateSignal(const clang::CXXMethodDecl *MD, int Idx);

    void GenerateTypeInfo(clang::QualType Type);
    void GenerateEnums(int EnumIndex);
    void GeneratePluginMetaData(bool Debug);

    // Called when emiting the code to generate the invokation of a method.
    // Return true if the code was already emitted  (include the break;)
    // defined in workaroundtest.cpp
    static bool WorkaroundTests(llvm::StringRef ClassName, const clang::CXXMethodDecl* MD, llvm::raw_ostream &OS);
};
