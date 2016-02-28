load(moc)


CONFIG += testcase
CONFIG += parallel_test


QT = testlib

TARGET = tst_templates2
TEMPLATEEHEADERS += templatedobj.h

defineReplace(mocngCmdBase) {
    RET =
    !isEmpty(WIN_INCLUDETEMP) {
        incvar = @$$WIN_INCLUDETEMP
    } else {
        incvar =
        for (inc, MOC_INCLUDEPATH): \
            incvar += -I$$shell_quote($$inc)
            incvar += $$QMAKE_FRAMEWORKPATH_FLAGS
    }
    RET += $$QMAKE_MOC $(DEFINES) $$join(QMAKE_COMPILER_DEFINES, " -D", -D) $$incvar $$QMAKE_MOC_OPTIONS
    return($$RET)
}


for (s, TEMPLATEEHEADERS) {
    sf = $$absolute_path($$s, $$_PRO_FILE_PWD_)
    QMAKE_INTERNAL_INCLUDED_FILES += $$sf
    sfl = $$cat($$sf, lines)
    base = mocng_$$replace(sf, ^.*/([^./]+)[^/]*$, \\1)

    invar = $$upper($$base)_SOURCES
    $$invar = $$sf
    $${base}.input = $$invar
    $${base}.config += target_predeps
    $${base}.output = $$MOC_DIR/moc_${QMAKE_FILE_BASE}.cpp
    $${base}.variable_out = GENERATED_SOURCES
    $${base}.commands = ${QMAKE_FUNC_mocngCmdBase} ${QMAKE_FILE_IN}  -o $$MOC_DIR/moc_${QMAKE_FILE_BASE}.cpp -o $$MOC_DIR/moc_${QMAKE_FILE_BASE}.h
    $${base}.name = MOCNG ${QMAKE_FILE_IN}

    $${base}_th.input = $$invar
    $${base}_th.output = $$MOC_DIR/moc_${QMAKE_FILE_BASE}.h
    $${base}_th.depends = $$MOC_DIR/moc_${QMAKE_FILE_BASE}.cpp
    $${base}_th.CONFIG = no_link
    $${base}_th.commands = $$escape_expand(\\n)   # force creation of rule

    QMAKE_EXTRA_COMPILERS += $$base $${base}_th
}


SOURCES += templatedobj.cpp tst_templates2.cpp