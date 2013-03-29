/****************************************************************************
 * Copyright (C) 2012 Woboq UG (haftungsbeschraenkt)
 * Olivier Goffart <contact at woboq.com>
 * http://woboq.com/
 ****************************************************************************/


#include "propertyparser.h"

std::string PropertyParser::LexemUntil(clang::tok::TokenKind Until) {
    int ParensLevel = 0;
    std::string Result;
    do {
        switch(+CurrentTok.getKind()) {
        case clang::tok::eof:
            return Result;
        case clang::tok::l_square:
        case clang::tok::l_paren:
        case clang::tok::l_brace:
            ++ParensLevel;
            break;
        case clang::tok::r_square:
        case clang::tok::r_paren:
        case clang::tok::r_brace:
            --ParensLevel;
            break;
        }

        Consume();
        auto Sp = Spelling();
        char Last = Result[Result.size()];
        if ((Last == '<' && Sp[0] == ':') || (IsIdentChar(Last) && IsIdentChar(Sp[0])))
            Result += " ";
        Result += Sp;
    } while (ParensLevel > 0 || !PrevToken.is(Until));
    return Result;
}


std::string PropertyParser::parseUnsigned() {
    switch(+CurrentTok.getKind()) {
    case clang::tok::kw_int:
        Consume();
        return "uint";
        break;
    case clang::tok::kw_long:
        Consume();
        if (Test(clang::tok::kw_int))
            return "unsigned long int";
        else if (Test(clang::tok::kw_long))
            return "unsigned long long";
        else
            return "uint";
        break;
    case clang::tok::kw_short:
    case clang::tok::kw_char:
        Consume();
        return "unsigned " + Spelling();
        break;
    default:
        return "unsigned";
        // do not consume;
    }
}

std::string PropertyParser::parseTemplateType() {
    std::string Result;
    int ParensLevel = 0;
    bool MoveConstToFront = true;
    bool HasConst = false;
    do {
        switch(+CurrentTok.getKind()) {
        case clang::tok::eof:
            return {};
        case clang::tok::greater:
            if (ParensLevel > 0)
                break;
            if (Result[Result.size()-1] == '>')
                Result += " ";
            Result += ">";
            Consume();
            return Result;
        case clang::tok::less:
            if (ParensLevel > 0 )
                break;
            Result += "<";
            Consume();
            Result += parseTemplateType();
            if (!PrevToken.is(clang::tok::greater))
                return {};
            MoveConstToFront = false;
            continue;
        case clang::tok::l_square:
        case clang::tok::l_paren:
        case clang::tok::l_brace:
            ++ParensLevel;
            break;
        case clang::tok::r_square:
        case clang::tok::r_paren:
        case clang::tok::r_brace:
            --ParensLevel;
            if (ParensLevel < 0)
                return {};
            break;
        case clang::tok::comma:
            if (ParensLevel > 0)
                break;
            Result += ",";
            return Result + parseTemplateType();

        case clang::tok::kw_const:
            if (MoveConstToFront) {
                HasConst = true;
                continue;
            }
            break;
        case clang::tok::kw_unsigned:
            if (IsIdentChar(Result[Result.size()]))
                Result+=" ";
            Result += parseUnsigned();
            continue;
        case clang::tok::amp:
        case clang::tok::ampamp:
        case clang::tok::star:
            MoveConstToFront = false;
            break;
        }

        Consume();
        auto Sp = Spelling();
        char Last = Result[Result.size()];
        if ((Last == '<' && Sp[0] == ':') || (IsIdentChar(Last) && IsIdentChar(Sp[0])))
            Result += " ";
        Result += Sp;
    } while (true);
    if (HasConst)
        Result = "const " + Result;
    return Result;
}

std::string PropertyParser::parseType() {
    std::string Result;
    bool HasConst = Test(clang::tok::kw_const);
    bool HasVolatile = Test(clang::tok::kw_volatile);

    bool MoveConstToFront = true;

    Test(clang::tok::kw_enum) || Test(clang::tok::kw_class) || Test(clang::tok::kw_struct);

    if (Test(clang::tok::kw_unsigned)) {
        Result += parseUnsigned();
    } else if (Test(clang::tok::kw_signed)) {
        Result += "signed";
        switch(+CurrentTok.getKind()) {
        case clang::tok::kw_int:
        case clang::tok::kw_long:
        case clang::tok::kw_short:
        case clang::tok::kw_char:
            Consume();
            Result += " " + Spelling();
        }
    } else {
        while(Test(clang::tok::kw_int)
                || Test(clang::tok::kw_long)
                || Test(clang::tok::kw_short)
                || Test(clang::tok::kw_char)
                || Test(clang::tok::kw_void)
                || Test(clang::tok::kw_bool)
                || Test(clang::tok::kw_double)
                || Test(clang::tok::kw_float)) {
            if (!Result.empty())
                Result += " ";
            Result += Spelling();
        }
    }

    if (Result.empty()) {
        if (Test(clang::tok::coloncolon))
            Result += Spelling();
        do {
            if (!Test(clang::tok::identifier))
                return {}; // that's an error
            Result += Spelling();

            if (Test(clang::tok::less)) {
                MoveConstToFront = false; // the official moc do not do it
                Result += "<";
                Result += parseTemplateType();

                if (!PrevToken.is(clang::tok::greater))
                    return {}; //error;
            }

            if (!Test(clang::tok::coloncolon))
                break;

            Result += Spelling();
        } while (true);
    }

    if (MoveConstToFront && Test(clang::tok::kw_const)) {
        HasConst = true;
    }

    while (Test(clang::tok::kw_volatile)
            || Test(clang::tok::star)
            || Test(clang::tok::amp)
            || Test(clang::tok::ampamp)
            || Test(clang::tok::kw_const)) {
        Result += Spelling();
    }

    if (HasVolatile)
        Result = "volatile " + Result;
    if (HasConst)
        Result = "const " + Result;

    return Result;
}

PropertyDef PropertyParser::parse() {
    PropertyDef Def;
    Consume();
    std::string type = parseType();
    if (type.empty()) {
        //Error
        return Def;
    }

    // Special logic in moc
    if (type == "QMap")
        type = "QMap<QString,QVariant>";
    else if (type == "QValueList")
        type = "QValueList<QVariant>";
    else if (type == "LongLong")
        type = "qlonglong";
    else if (type == "ULongLong")
        type = "qulonglong";

    Def.type = type;


    if (!Test(clang::tok::identifier)) {
        //Error
        return Def;
    }

    Def.name = Spelling();

    while(Test(clang::tok::identifier)) {
        std::string l = Spelling();
        clang::SourceLocation KeywordLocation = PrevToken.getLocation();
        if (l[0] == 'C' && l == "CONSTANT") {
            Def.constant = true;
            continue;
        } else if(l[0] == 'F' && l == "FINAL") {
            Def.final = true;
            continue;
        }
        std::string v, v2;
        if (Test(clang::tok::l_paren)) {
            v = LexemUntil(clang::tok::r_paren);
        } else if (Test(clang::tok::numeric_constant)) {
            v = Spelling();
            if (l != "REVISION") {
                //Error
                return Def;
            }
        } else {
            if (!Test(clang::tok::identifier)) {
                //Error
                return Def;
            }
            v = Spelling();
            if (CurrentTok.getKind() == clang::tok::l_paren) {
                v2 = LexemUntil(clang::tok::r_paren);
            } else if (v != "true" && v != "false")
                v2 = "()";
        }
        switch (l[0]) {
        case 'M':
            if (l == "MEMBER")
                Def.member = v;
            else
                return Def; // Error;
            break;
        case 'R':
            if (l == "READ")
                Def.read = v;
            else if (l == "RESET")
                Def.reset = v + v2;
            else if (l == "REVISION") {
                Def.revision = atoi(v.c_str());
                if (Def.revision < 0)
                    return Def; // Error;
            } else
                return Def; // Error;
            break;
        case 'S':
            if (l == "SCRIPTABLE")
                Def.scriptable = v + v2;
            else if (l == "STORED")
                Def.stored = v + v2;
            else
                return Def; // Error;
            break;
        case 'W':
            if (l != "WRITE") return Def; // Error;
            Def.write = v;
            break;
        case 'D':
            if (l != "DESIGNABLE") return Def; // Error;
            Def.designable = v + v2;
            break;
        case 'E':
            if (l != "EDITABLE") return Def; // Error;
            Def.editable = v + v2;
            break;
        case 'N':
            if (l != "NOTIFY") return Def; // Error;
            Def.notify = v;
            break;
        case 'U':
            if (l != "USER") return Def; // Error;
            Def.user = v + v2;
            break;
        default:
            auto D = PP.getDiagnostics().Report(OriginalLocation(KeywordLocation),
                                                PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                                        "unkown keyword in Q_PROPERTY"));
            return Def; // Error;
        }
    }
    if (!CurrentTok.is(clang::tok::eof)) {
        return Def; // Error;
    }
    return Def;
}


