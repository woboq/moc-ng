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

#include "propertyparser.h"
#include <clang/Sema/Lookup.h>
#include <clang/Sema/SemaDiagnostic.h>
#include <clang/AST/CanonicalType.h>
#include <iostream>

std::string PropertyParser::LexemUntil(clang::tok::TokenKind Until, bool Templ) {
    int ParensLevel = 0;
    int BrLevel = 0;
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
        case clang::tok::greater:
            if (!ParensLevel)
                BrLevel--;
            break;
        case clang::tok::less:
            if (!ParensLevel && BrLevel >= 0)
                BrLevel++;
            break;
        case clang::tok::greatergreater:
            if (!ParensLevel)
                BrLevel-=2;
            break;
        }

        Consume();
        auto Sp = Spelling();
        char Last = Result[Result.size()];
        if ((Last == '<' && Sp[0] == ':') || (IsIdentChar(Last) && IsIdentChar(Sp[0])))
            Result += " ";
        Result += Sp;
    } while ((ParensLevel != 0 || !PrevToken.is(Until) || (Templ && BrLevel > 0)) && ParensLevel >= 0);
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
            return "ulong";
        break;
    case clang::tok::kw_char:
    case clang::tok::kw_short:
        Consume();
        if (Test(clang::tok::kw_int))
            return "unsigned short int";
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
    clang::CXXScopeSpec SS;
    do {
        switch(+CurrentTok.getKind()) {
        case clang::tok::eof:
            return {};
        case clang::tok::greatergreater:
            if (ParensLevel > 0)
                break;
            CurrentTok.setKind(clang::tok::greater);
            PrevToken.setKind(clang::tok::greater);
            if (Result[Result.size()-1] == '>')
                Result += " ";
            Result += ">";
            return Result;
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
            Consume();
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
        case clang::tok::identifier: {
            clang::LookupResult Found(Sema, CurrentTok.getIdentifierInfo(), OriginalLocation(CurrentTok.getLocation()),
                                      clang::Sema::LookupNestedNameSpecifierName);
            Sema.LookupParsedName(Found, Sema.getScopeForContext(RD), &SS);
            clang::CXXRecordDecl* D = Found.getAsSingle<clang::CXXRecordDecl>();
            if (D && !D->hasDefinition())
                IsPossiblyForwardDeclared = true;
            Found.suppressDiagnostics();
            break;
          }
        case clang::tok::coloncolon:
            if (PrevToken.getIdentifierInfo())
                SS.Extend(Sema.getASTContext(), PrevToken.getIdentifierInfo(), OriginalLocation(), OriginalLocation(CurrentTok.getLocation()));
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

std::string PropertyParser::parseType(bool SupressDiagnostics) {
    std::string Result;
    bool HasConst = Test(clang::tok::kw_const);
    bool HasVolatile = Test(clang::tok::kw_volatile);

    bool NoTemplates = true;

    Test(clang::tok::kw_enum) || Test(clang::tok::kw_class) || Test(clang::tok::kw_struct);

    if (Test(clang::tok::kw_unsigned)) {
        Result += parseUnsigned();
    } else if (Test(clang::tok::kw_signed)) {
        Result += "signed";
        while (true) {
            switch(+CurrentTok.getKind()) {
            case clang::tok::kw_int:
            case clang::tok::kw_long:
            case clang::tok::kw_short:
            case clang::tok::kw_char:
                Consume();
                Result += " " + Spelling();
                continue;
            }
            break;
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
        clang::CXXScopeSpec SS;
        if (Test(clang::tok::coloncolon)) {
            SS.MakeGlobal(Sema.getASTContext(), OriginalLocation());
            Result += Spelling();
        } do {
            if (!Test(clang::tok::identifier)) {
                PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                                           PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                           "Invalid token while parsing type"));
                return {};
            }
            Result += Spelling();

            if (Test(clang::tok::less)) {
                NoTemplates = false;
                Result += "<";
                Result += parseTemplateType();

                if (!PrevToken.is(clang::tok::greater)) {
                    PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                                               PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                               "parse error in type"));
                    return {}; //error;
                }
            }

            clang::Token IdentTok = PrevToken;

            if (!Test(clang::tok::coloncolon))
                break;

            if (NoTemplates && !SupressDiagnostics) {
#if CLANG_VERSION_MAJOR >= 4
                clang::Sema::NestedNameSpecInfo NameInfo(IdentTok.getIdentifierInfo(),
                                                         OriginalLocation(IdentTok.getLocation()),
                                                         OriginalLocation(CurrentTok.getLastLoc()));
                if (Sema.ActOnCXXNestedNameSpecifier(Sema.getScopeForContext(RD), NameInfo, false, SS))
#else
                if (Sema.ActOnCXXNestedNameSpecifier(Sema.getScopeForContext(RD), *IdentTok.getIdentifierInfo(),
                    OriginalLocation(IdentTok.getLocation()), OriginalLocation(CurrentTok.getLastLoc()), {}, false, SS))
#endif
                {
                    SS.SetInvalid({OriginalLocation(IdentTok.getLocation()), OriginalLocation(CurrentTok.getLastLoc())});
                }
            }

            Result += Spelling();
        } while (true);

        if (NoTemplates && !SupressDiagnostics) {

            IsEnum = true; // That's how moc does it.

            if (SS.isNotEmpty() && SS.isValid()) {
                Extra = llvm::dyn_cast_or_null<clang::CXXRecordDecl>(Sema.computeDeclContext(SS));

                clang::LookupResult Found(Sema, PrevToken.getIdentifierInfo(), OriginalLocation(),
                                        clang::Sema::LookupNestedNameSpecifierName);
                /*if (SS.isEmpty())
                    Sema.LookupQualifiedName(Found, RD);
                else {*/
                clang::DeclContext* DC = Sema.computeDeclContext(SS);
                Sema.LookupQualifiedName(Found, DC ? DC : RD);
                //}
                clang::EnumDecl* R = Found.getAsSingle<clang::EnumDecl>();
                /*if (!R) {
                if (clang::TypedefDecl *TD = Found.getAsSingle<clang::TypedefDecl>()) {
                    const clang::TemplateSpecializationType* TDR = TD->getUnderlyingType()->getAs<clang::TemplateSpecializationType>();
                    if(TDR && TDR->getNumArgs() == 1 && TDR->getTemplateName().getAsTemplateDecl()->getName() == "QFlags") {
                        if (const clang::EnumType* ET = TDR->getArg(0).getAsType()->getAs<clang::EnumType>())
                            R = ET->getDecl();
                    }
                }*/

                /*if (!R)
                    IsEnum = false;*/

                if (Extra == RD) Extra = nullptr;
                if(Extra) {
                    bool isQObjectOrQGadget = false;
                    for (auto it = Extra->decls_begin(); it != Extra->decls_end(); ++it) {
                        auto ND = llvm::dyn_cast<clang::NamedDecl>(*it);
                        if (ND && ND->getIdentifier() && ND->getName() == "staticMetaObject") {
                            isQObjectOrQGadget = true;
                            break;
                        }
                    }
                    if (!isQObjectOrQGadget)
                        Extra = nullptr;
                }

                if (!R) {
                    clang::CXXRecordDecl* D = Found.getAsSingle<clang::CXXRecordDecl>();
                    if (D && !D->hasDefinition())
                        IsPossiblyForwardDeclared = true;
                }
            } else if (SS.isEmpty()) {
                clang::LookupResult Found(Sema, PrevToken.getIdentifierInfo(), OriginalLocation(),
                                          clang::Sema::LookupNestedNameSpecifierName);
                Sema.LookupName(Found, Sema.getScopeForContext(RD));
                clang::CXXRecordDecl* D = Found.getAsSingle<clang::CXXRecordDecl>();
                if (D && !D->hasDefinition()) {
                    IsPossiblyForwardDeclared = true;
                }
                Found.suppressDiagnostics();
            }
        }
    }

    if (NoTemplates && Test(clang::tok::kw_const)) {
        // The official moc don't move the const if there are templates
        HasConst = true;
    }

    while (Test(clang::tok::kw_volatile)
            || Test(clang::tok::star)
            || Test(clang::tok::kw_const)) {
        Extra = nullptr;
        IsEnum = false;
        Result += Spelling();
    }

    if (Test(clang::tok::amp)) {
        if (HasConst)
            HasConst = false; // remove const reference
        else
            Result += Spelling();
    } else {
        Test(clang::tok::ampamp); // skip rvalue ref
    }


    if (HasVolatile)
        Result = "volatile " + Result;
    if (HasConst)
        Result = "const " + Result;

    return Result;
}

PropertyDef PropertyParser::parseProperty(bool PrivateProperty) {
    PropertyDef Def;
    Consume();
    std::string type = parseType(false);
    if (type.empty()) {
        //Error
        return Def;
    }

    Def.PossiblyForwardDeclared = IsPossiblyForwardDeclared;

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

    Def.isEnum = IsEnum; // Well, that's what moc does.

    if (!CurrentTok.getIdentifierInfo()) {
        PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                        PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                        "Expected identifier as Q_PROPERTY name"));
        return Def;
    }
    Consume();

    Def.name = Spelling();

    while(Test(clang::tok::identifier)) {
        std::string l = Spelling();
        clang::SourceLocation KeywordLocation = OriginalLocation();
        if (l == "CONSTANT") {
            Def.constant = true;
            continue;
        } else if(l == "FINAL") {
            Def.final = true;
            continue;
        } else if (l == "REVISION") {
            if (!Test(clang::tok::numeric_constant)) {
                PP.getDiagnostics().Report(OriginalLocation(),
                                           PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                           "Expected numeric constant after REVISION in Q_PROPERTY"));
                return Def;
            }
            Def.revision = atoi(Spelling().c_str());
            if (Def.revision < 0) {
                PP.getDiagnostics().Report(OriginalLocation(),
                                           PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                           "Invalid REVISION number in Q_PROPERTY"));
                return Def;
            }
            continue;
        }

        std::string v, v2;
        bool IsIdent = false;
        clang::SourceLocation ParamLoc = OriginalLocation(CurrentTok.getLocation());
        if (CurrentTok.getKind() == clang::tok::l_paren) {
            v = LexemUntil(clang::tok::r_paren);
            v = v.substr(1, v.size() - 2); // remove the '(' and ')'
        } else if (Test(clang::tok::identifier)) {
            IsIdent = true;
            v = Spelling();
            if (CurrentTok.getKind() == clang::tok::l_paren) {
                v2 = LexemUntil(clang::tok::r_paren);
            } else {
                v2 = "()";
            }
        } else if(Test(clang::tok::kw_true) || Test(clang::tok::kw_false)) {
            v = Spelling();
        } else {
            PP.getDiagnostics().Report(OriginalLocation(),
                                       PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                       "Parse error in Q_PROPERTY: Expected identifier"));
            return Def;
        }

        if (l == "MEMBER")
            Def.member = v;
        else if (l == "READ") {
            Def.read = v;
            if (IsIdent && !PrivateProperty) {
                clang::LookupResult Found(Sema, PP.getIdentifierInfo(v), ParamLoc, clang::Sema::LookupMemberName);
                Sema.LookupQualifiedName(Found, RD);
                if (Found.empty()) {
#if (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR < 6) || CLANG_VERSION_MAJOR >= 9
                    clang::DeclFilterCCC<clang::CXXMethodDecl> Validator;
#endif
                    if (clang::TypoCorrection Corrected =
                            Sema.CorrectTypo(Found.getLookupNameInfo(), clang::Sema::LookupMemberName,
                                             nullptr, nullptr,
#if (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR < 6)  || CLANG_VERSION_MAJOR >= 9
                                             Validator,
#else
                                             llvm::make_unique<clang::DeclFilterCCC<clang::CXXMethodDecl>>(),
#endif
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR >= 5
                                             clang::Sema::CTK_ErrorRecovery,
#endif
                                             RD)) {
                        PP.getDiagnostics().Report(Found.getNameLoc(),
                                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                    "READ function %0 not found; did you mean %1"))
                            << Found.getLookupName() << Corrected.getCorrection()
                            << clang::FixItHint::CreateReplacement(Found.getNameLoc(),
                                                                   Corrected.getAsString(PP.getLangOpts()));
                        PP.getDiagnostics().Report(Corrected.getCorrectionDecl()->getLocation(), clang::diag::note_previous_decl)
                            << Corrected.getCorrection();

                    } else {
                        PP.getDiagnostics().Report(ParamLoc,
                                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                                   "READ function %0 not found")) << Found.getLookupName();
                    }
                } else if (!Found.isAmbiguous()) {
                    clang::CXXMethodDecl* M = Found.getAsSingle<clang::CXXMethodDecl>();
                    if (M) {
#if CLANG_VERSION_MAJOR != 3 || CLANG_VERSION_MINOR >= 5
                        clang::QualType T = M->getReturnType();
#else
                        clang::QualType T = M->getResultType();
#endif
                        if (T->isPointerType() && type.back() != '*') {
                          clang::PrintingPolicy PrPo(PP.getLangOpts());
                          PrPo.SuppressTagKeyword = true;
                          if (T->getPointeeType().getUnqualifiedType().getAsString(PrPo) == type)
                            Def.PointerHack = true;
                        }
                    }
                }
                Found.suppressDiagnostics();
            } //FIXME: else
        } else if (l == "RESET")
            Def.reset = v + v2;
        else if (l == "SCRIPTABLE")
            Def.scriptable = v + v2;
        else if (l == "STORED")
            Def.stored = v + v2;
        else if (l == "WRITE")
            Def.write = v;
        else if (l == "DESIGNABLE")
            Def.designable = v + v2;
        else if (l == "EDITABLE")
            Def.editable = v + v2;
        else if (l == "NOTIFY") {
            Def.notify.Str = v;
            Def.notify.Loc = ParamLoc;
        } else if (l == "USER")
            Def.user = v + v2;
        else {
            PP.getDiagnostics().Report(OriginalLocation(KeywordLocation),
                                       PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                       "Expected a Q_PROPERTY keyword"));
            return Def;
        }
    }
    if (!CurrentTok.is(clang::tok::eof)) {
        PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                   "Expected a Q_PROPERTY keyword"));
        return Def;
    }

    return Def;
}


PrivateSlotDef PropertyParser::parsePrivateSlot()
{
    PrivateSlotDef Slot;
    Consume();
    Slot.ReturnType = parseType();

    if (!Test(clang::tok::identifier)) {
        PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                            PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                            "Expected slot name"));
        return {};
    }

    Slot.Name = Spelling();

    if (!Test(clang::tok::l_paren)) {
        PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                    PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                    "Expected parenthesis in slot signature"));
        return {};
    }

    do {
        if (CurrentTok.is(clang::tok::eof)) {
            PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                                       PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                       "Missing closing parenthesis"));
            return Slot;
        }
        if (Test(clang::tok::r_paren)) {
            break;
        }
        std::string T = parseType();
        if (T.empty()) //Error;
            return Slot;

        Slot.Args.push_back(std::move(T));

        Test(clang::tok::identifier);


        if (Test(clang::tok::equal)) {
            Slot.NumDefault++;
            LexemUntil(clang::tok::comma, true);
            if (PrevToken.is(clang::tok::r_paren))
                break;
            if (!PrevToken.is(clang::tok::comma)) {
                PP.getDiagnostics().Report(OriginalLocation(),
                    PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                    "Parse error in default argument"));
                return Slot;
            }
            continue;
        } else if (Slot.NumDefault) {
            //FIXME: error;
        }

        if (Test(clang::tok::comma))
            continue;

        if (Test(clang::tok::r_paren)) {
            break;
        }

        PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                   "Expected comma in slot signature"));
        return Slot;
    } while (true);

    Test(clang::tok::kw_const);
    Test(clang::tok::kw_volatile);

    if (!CurrentTok.is(clang::tok::eof)) {
        PP.getDiagnostics().Report(OriginalLocation(CurrentTok.getLocation()),
                                   PP.getDiagnostics().getCustomDiagID(clang::DiagnosticsEngine::Error,
                                   "Unexpected token"));
    }

    return Slot;
}

