#
# $Id$
#
#
#
#
SERVERSOURCE=../java_japeserver
ENGINESOURCE=../../camlengine
BIN=.
#SERVERSCRIPT=exec java -jar japeserver.jar
#
#
#
all:    ; (cd $(SERVERSOURCE); pbxbuild -target japeserver build);  \
          (cd $(ENGINESOURCE); make jape)

clean:  ; rm -f $(BIN)/japeserver $(BIN)/japelogo.gif $(BIN)/jape;  \
          (cd $(SERVERSOURCE); pbxbuild -target japeserver clean);  \
          (cd $(ENGINESOURCE); make clean)

javaclean: ; cd $(SERVERSOURCE); pbxbuild -target japeserver clean

camlclean: ; cd $(ENGINESOURCE); make clean
#
#
#











