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
    clang::CompilerInstance &ci;
    clang::ASTContext *ctx = nullptr;
    MocPPCallbacks *PPCallbacks = nullptr;

    std::vector<ClassDef> objects;
    MocNg Moc;

    bool done = false;

    std::string generate() ;

public:
    MocASTConsumer(clang::CompilerInstance &ci) :ci(ci)
    {
        //ci.getLangOpts().DelayedTemplateParsing = (true);
    }

    virtual void Initialize(clang::ASTContext& Ctx) override;


    virtual bool HandleTopLevelDecl(clang::DeclGroupRef D) override;


    void HandleTagDeclDefinition(clang::TagDecl* D) override;


};

