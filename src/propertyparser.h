/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#pragma once

#include <clang/Lex/Token.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Lex/Lexer.h>
#include <clang/Sema/Sema.h>
#include "mocng.h"


class PropertyParser {
    clang::Sema &Sema;
    clang::Preprocessor &PP;

    llvm::MemoryBuffer *Buf;
    clang::Lexer Lexer;
    clang::Token PrevToken;
    clang::Token CurrentTok;



    clang::SourceLocation BaseLoc;

    clang::CXXRecordDecl *RD;

    bool IsEnum = false;

public:

    clang::CXXRecordDecl *Extra = nullptr;

    PropertyParser(llvm::StringRef Text, clang::SourceLocation Loc, clang::Sema &Sema, clang::CXXRecordDecl *RD) :
        Sema(Sema), PP(Sema.getPreprocessor()),
        Buf(llvm::MemoryBuffer::getMemBufferCopy(Text, "Q_PROPERTY")),
//        Lexer(PP.getSourceManager().getSpellingLoc(Loc), PP.getLangOpts(), Text.begin(), Text.begin(), Text.end()),
        Lexer(PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Loc),
              Buf, PP.getSourceManager(), PP.getLangOpts()),
        BaseLoc(Loc), RD(RD)
    {  }

private:
    clang::SourceLocation OriginalLocation(clang::SourceLocation SpellingLoc = clang::SourceLocation()) {
        if (SpellingLoc.isInvalid())
            SpellingLoc = PrevToken.getLocation();
        return BaseLoc.getLocWithOffset(PP.getSourceManager().getFileOffset(SpellingLoc));
    }

    void Consume() {
        PrevToken = CurrentTok;
        Lexer.LexFromRawLexer(CurrentTok);
        if (CurrentTok.is(clang::tok::raw_identifier)) {
            PP.LookUpIdentifierInfo(CurrentTok);
        }
    }

    std::string Spelling() {
        return PP.getSpelling(PrevToken);
    }

    bool Test(clang::tok::TokenKind Kind) {
        if (!CurrentTok.is(Kind))
            return false;
        Consume();
        return true;
    }


    bool IsIdentChar(char c) {
        return (c=='_' || c=='$' || (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
    }

    std::string LexemUntil(clang::tok::TokenKind Until);
public:
    std::string parseUnsigned();
    std::string parseTemplateType();
    std::string parseType();
    PropertyDef parse();
};

