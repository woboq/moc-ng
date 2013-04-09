//#include <QtCore/qobjectdefs.h>

const char Injected[] = R"-(
#ifdef Q_MOC_OUTPUT_REVISION

#define QT_ANNOTATE_CLASS(type, anotation) \
    __extension__ _Static_assert(sizeof (#anotation), #type);
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

#define Q_CLASSINFO(name, value)  __extension__ _Static_assert(sizeof (name, value), "qt_classinfo");
#define Q_PLUGIN_METADATA(x) QT_ANNOTATE_CLASS(qt_plugin_metadata, x)
#define Q_INTERFACES(x) QT_ANNOTATE_CLASS(qt_interfaces, x)
#define Q_PROPERTY(text) QT_ANNOTATE_CLASS(qt_property, text)
#define Q_PRIVATE_PROPERTY(d, text)  QT_ANNOTATE_CLASS2(qt_private_property, d, text)

#define Q_REVISION(v) __attribute__((annotate("qt_revision:" QT_STRINGIFY2(v))))
#define Q_ENUMS(x) QT_ANNOTATE_CLASS(qt_enums, x)
#define Q_FLAGS(x) QT_ANNOTATE_CLASS(qt_flags, x)
#define Q_SCRIPTABLE  __attribute__((annotate("qt_scriptable")))
#define Q_INVOKABLE  __attribute__((annotate("qt_invokable")))
#define Q_SIGNAL __attribute__((annotate("qt_signal")))
#define Q_SLOT __attribute__((annotate("qt_slot")))
#endif // QT_NO_META_MACROS


#undef Q_OBJECT_CHECK
#define Q_OBJECT_CHECK \
    template <typename T> inline void qt_check_for_QOBJECT_macro(const T &_q_argument) const \
    { int i = qYouForgotTheQ_OBJECT_Macro(this, &_q_argument); i = i + 1; } \
    QT_ANNOTATE_CLASS(qt_qobject, "")

#undef Q_GADGET
#define Q_GADGET \
public: \
    static const QMetaObject staticMetaObject; \
private: \
    QT_ANNOTATE_CLASS(qt_qgadget, "")


#undef Q_OBJECT_FAKE
#define Q_OBJECT_FAKE Q_OBJECT QT_ANNOTATE_CLASS(qt_fake, "")

#undef QT_MOC_COMPAT
#define QT_MOC_COMPAT  __attribute__((annotate("qt_moc_compat")))

#endif
)-";
