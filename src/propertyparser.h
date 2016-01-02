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
    bool IsPossiblyForwardDeclared = false;

public:

    clang::CXXRecordDecl *Extra = nullptr;

    PropertyParser(llvm::StringRef Text, clang::SourceLocation Loc, clang::Sema &Sema,
                   clang::CXXRecordDecl *RD) :
        Sema(Sema), PP(Sema.getPreprocessor()),
        Buf(maybe_unique(llvm::MemoryBuffer::getMemBufferCopy(Text/*, "Q_PROPERTY"*/))),
//        Lexer(PP.getSourceManager().getSpellingLoc(Loc), PP.getLangOpts(), Text.begin(), Text.begin(), Text.end()),
        Lexer(CreateFileIDForMemBuffer(PP, Buf, Loc),
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

    std::string LexemUntil(clang::tok::TokenKind Until, bool Templ = false);
public:
    std::string parseUnsigned();
    std::string parseTemplateType();
    std::string parseType(bool SupressDiagnostics = true);
    PropertyDef parseProperty(bool PrivateProperty = false);
    PrivateSlotDef parsePrivateSlot();
};

