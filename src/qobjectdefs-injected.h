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


static const char Injected[] = R"-(
#if defined(Q_MOC_OUTPUT_REVISION) || defined(Q_MOC_RUN)

#ifdef Q_COMPILER_VARIADIC_MACROS
#define QT_ANNOTATE_CLASS(type, ...) \
    __extension__ _Static_assert(sizeof (#__VA_ARGS__), #type);
#else
#define QT_ANNOTATE_CLASS(type, anotation) \
    __extension__ _Static_assert(sizeof (#anotation), #type);
#endif
#define QT_ANNOTATE_CLASS2(type, a1, a2) \
    __extension__ _Static_assert(sizeof (#a1, #a2), #type);



#ifndef QT_NO_META_MACROS
# if defined(QT_NO_KEYWORDS)
#  define QT_NO_EMIT
# else
#   ifndef QT_NO_SIGNALS_SLOTS_KEYWORDS
#     undef  slots
#     define slots Q_SLOTS
#     undef  signals
#     define signals Q_SIGNALS
#   endif
# endif

# undef  Q_SLOTS
# define Q_SLOTS Q_SLOT
# undef  Q_SIGNALS
# define Q_SIGNALS public Q_SIGNAL
# undef  Q_PRIVATE_SLOT
# define Q_PRIVATE_SLOT(d, signature) QT_ANNOTATE_CLASS2(qt_private_slot, d, signature)


#undef Q_CLASSINFO
#undef Q_PLUGIN_METADATA
#undef Q_INTERFACES
#undef Q_PROPERTY
#undef Q_PRIVATE_PROPERTY
#undef Q_REVISION
#undef Q_ENUMS
#undef Q_FLAGS
#undef Q_SCRIPTABLE
#undef Q_INVOKABLE
#undef Q_SIGNAL
#undef Q_SLOT
#undef Q_ENUM
#undef Q_FLAG

#define Q_CLASSINFO(name, value)  __extension__ _Static_assert(sizeof (name, value), "qt_classinfo");
#define Q_PLUGIN_METADATA(x) QT_ANNOTATE_CLASS(qt_plugin_metadata, x)
#define Q_INTERFACES(x) QT_ANNOTATE_CLASS(qt_interfaces, x)
#ifdef Q_COMPILER_VARIADIC_MACROS
#define Q_PROPERTY(...) QT_ANNOTATE_CLASS(qt_property, __VA_ARGS__)
#else
#define Q_PROPERTY(text) QT_ANNOTATE_CLASS(qt_property, text)
#endif
#define Q_PRIVATE_PROPERTY(d, text)  QT_ANNOTATE_CLASS2(qt_private_property, d, text)

#define Q_REVISION(v) __attribute__((annotate("qt_revision:" QT_STRINGIFY2(v))))
#define Q_ENUMS(x) QT_ANNOTATE_CLASS(qt_enums, x)
#define Q_FLAGS(x) QT_ANNOTATE_CLASS(qt_flags, x)
#define Q_ENUM_IMPL(ENUM) \
    friend Q_DECL_CONSTEXPR const QMetaObject *qt_getEnumMetaObject(ENUM) Q_DECL_NOEXCEPT { return &staticMetaObject; } \
    friend Q_DECL_CONSTEXPR const char *qt_getEnumName(ENUM) Q_DECL_NOEXCEPT { return #ENUM; }
#define Q_ENUM(x) Q_ENUMS(x) Q_ENUM_IMPL(x)
#define Q_FLAG(x) Q_FLAGS(x) Q_ENUM_IMPL(x)
#define Q_SCRIPTABLE  __attribute__((annotate("qt_scriptable")))
#define Q_INVOKABLE  __attribute__((annotate("qt_invokable")))
#define Q_SIGNAL __attribute__((annotate("qt_signal")))
#define Q_SLOT __attribute__((annotate("qt_slot")))
#endif // QT_NO_META_MACROS


#undef QT_TR_FUNCTIONS
#ifndef QT_NO_TRANSLATION
#define QT_TR_FUNCTIONS \
    static inline QString tr(const char *s, const char *c = Q_NULLPTR, int n = -1) \
    { return staticMetaObject.tr(s, c, n); } \
    QT_DEPRECATED static inline QString trUtf8(const char *s, const char *c = Q_NULLPTR, int n = -1) \
    { return staticMetaObject.tr(s, c, n); } \
    QT_ANNOTATE_CLASS(qt_qobject, "")
#else
#define QT_TR_FUNCTIONS \
    QT_ANNOTATE_CLASS(qt_qobject, "")
#endif

#undef Q_GADGET
#define Q_GADGET \
public: \
    static const QMetaObject staticMetaObject; \
    void qt_check_for_QGADGET_macro(); \
    typedef void QtGadgetHelper; \
private: \
    Q_DECL_HIDDEN_STATIC_METACALL static void qt_static_metacall(QObject *, QMetaObject::Call, int, void **); \
    QT_ANNOTATE_CLASS(qt_qgadget, "")

//for qnamespace.h because Q_MOC_RUN is defined
#if defined(Q_MOC_RUN)
#undef Q_OBJECT
#define Q_OBJECT QT_ANNOTATE_CLASS(qt_qobject, "")
#endif

#undef Q_OBJECT_FAKE
#define Q_OBJECT_FAKE Q_OBJECT QT_ANNOTATE_CLASS(qt_fake, "")

#undef QT_MOC_COMPAT
#define QT_MOC_COMPAT  __attribute__((annotate("qt_moc_compat")))

#endif
)-";
