/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/


#include "mocng.h"
#include "propertyparser.h"

#include <clang/Lex/Preprocessor.h>
#include <clang/AST/DeclCXX.h>


ClassDef parseClass (clang::CXXRecordDecl *RD, clang::Preprocessor &PP) {
    ClassDef Def;
    Def.Record = RD;

    for (auto it = RD->decls_begin(); it != RD->decls_end(); ++it) {
        if (clang::StaticAssertDecl *S = llvm::dyn_cast<clang::StaticAssertDecl>(*it) ) {
            if (auto *E = llvm::dyn_cast<clang::UnaryExprOrTypeTraitExpr>(S->getAssertExpr()))
                if (auto *Kw = llvm::dyn_cast<clang::StringLiteral>(E->getArgumentExpr())) {
                    llvm::StringRef key = Kw->getString();
                    if (key == "qt_property") {
                        PropertyParser Parser(S->getMessage()->getString(),
//                                              S->getMessage()->getStrTokenLoc(0),
                                              S->getMessage()->getLocationOfByte(
                                                  0, PP.getSourceManager(), PP.getLangOpts(), PP.getTargetInfo()),
                                              PP);
                        Def.Properties.push_back(Parser.parse());
                    } else if (key == "qt_qobject") {
                        Def.HasQObject = true;
                    } else if (key == "qt_qgadget") {
                        Def.HasQGadget = true;
                    }
                }
        } else if (clang::CXXMethodDecl *M = llvm::dyn_cast<clang::CXXMethodDecl>(*it)) {
       // int Clones = it->getNumParams() - it->getMinRequiredArguments();
            for (auto attr_it = M->specific_attr_begin<clang::AnnotateAttr>();
                attr_it != M->specific_attr_end<clang::AnnotateAttr>();
                ++attr_it) {

                const clang::AnnotateAttr *A = *attr_it;
                if (A->getAnnotation() == "qt_signal") {
            //        for (int i = 0; i < Clones; ++i)
                        Def.Signals.push_back(M);
                } else if (A->getAnnotation() == "qt_slot") {
        //         for (int i = 0; i < Clones; ++i)
                        Def.Slots.push_back(M);
                } else if (A->getAnnotation() == "qt_invokable") {
                    if (auto *C = llvm::dyn_cast<clang::CXXConstructorDecl>(M)) {
            //            for (int i = 0; i < Clones; ++i)
                            Def.Constructors.push_back(C);
                    } else {
            //            for (int i = 0; i < Clones; ++i)
                            Def.Methods.push_back(M);
                    }
                }
            }
        }
    }
/*
    if (Def.HasQObject) {
        std::cout << Def.Record->getQualifiedNameAsString() <<  " is " << Def.HasQObject << "a Q_OBJECT" << std::endl;
    }*/

    return Def;
}
