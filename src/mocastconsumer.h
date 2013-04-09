/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#pragma once

#include <clang/Frontend/CompilerInstance.h>
#include <clang/AST/ASTConsumer.h>

#include "mocng.h"

class MocPPCallbacks;

class MocASTConsumer : public clang::ASTConsumer
{
protected:
    clang::CompilerInstance &ci;
    clang::ASTContext *ctx = nullptr;
    MocPPCallbacks *PPCallbacks = nullptr;

    std::vector<ClassDef> objects;
    MocNg Moc;

public:
    MocASTConsumer(clang::CompilerInstance &ci) :ci(ci)
    { }

    virtual void Initialize(clang::ASTContext& Ctx) override;
    void HandleTagDeclDefinition(clang::TagDecl* D) override;
};

