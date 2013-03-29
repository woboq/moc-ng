/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/

#pragma once

#include <clang/Lex/Token.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Lex/Lexer.h>
#include "mocng.h"


class PropertyParser {

    llvm::MemoryBuffer *Buf;
    clang::Lexer Lexer;
    clang::Token PrevToken;
    clang::Token CurrentTok;

    clang::Preprocessor &PP;

    clang::SourceLocation BaseLoc;

public:

    PropertyParser(llvm::StringRef Text, clang::SourceLocation Loc, clang::Preprocessor &PP) :
        Buf(llvm::MemoryBuffer::getMemBufferCopy(Text)),
//        Lexer(PP.getSourceManager().getSpellingLoc(Loc), PP.getLangOpts(), Text.begin(), Text.begin(), Text.end()),
        Lexer(PP.getSourceManager().createFileIDForMemBuffer(Buf, clang::SrcMgr::C_User, 0, 0, Loc),
              Buf, PP.getSourceManager(), PP.getLangOpts()),
        PP(PP), BaseLoc(Loc)
    {  }

private:

    clang::SourceLocation OriginalLocation(clang::SourceLocation SpellingLoc) {
        return BaseLoc.getLocWithOffset(PP.getSourceManager().getFileOffset(SpellingLoc));
        //return PP.getSourceManager() BaseLoc;µ
      /*  std::cout << "_  _ _ " << BaseLoc.isFileID() << " " << BaseLoc.isMacroID() << " " << BaseLoc.isValid() << std::endl;
        clang::FullSourceLoc L(BaseLoc, PP.getSourceManager());
        L.dump();

        L.getExpansionLoc().dump();
        L.getSpellingLoc().dump();

        std::cout << std::endl <<  L.getSpellingLineNumber()  << std::endl;
*/

      //  return  BaseLoc;  //PP.getSourceManager().getExpansionLoc(BaseLoc);
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

