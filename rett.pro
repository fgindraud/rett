### Compilation ###

TEMPLATE = app
CONFIG += c++11

CONFIG(release, debug|release): DEFINES += QT_NO_DEBUG_OUTPUT

QT += core widgets
HEADERS +=
SOURCES += main.cpp

### Misc information ###

VERSION = 0.1
DEFINES += RETT_VERSION=$${VERSION}

QMAKE_TARGET_COMPANY = Francois Gindraud
QMAKE_TARGET_PRODUCT = Rett
QMAKE_TARGET_DESCRIPTION = Relation Editor and Tracking Tool
QMAKE_TARGET_COPYRIGHT = Copyright (C) 2017 Francois Gindraud

