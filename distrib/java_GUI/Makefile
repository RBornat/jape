#
#       Rules that force a complete recompile from the root when a source file changes
#       I've not found a reliable java dependency generator, and
#       the compiler start overhead is too ...  large to
#       use it on individual files.
#
#       ANT here we come. (Now more than ever! BAS Feb 04)
#
.SUFFIXES:      .jar .java .class
.SILENT:
%.class:        %.java; @touch $*.class; echo $*.class "out of date"

#
#             
#
JAVA            = $(shell echo *.java)
RESOURCEFILES   = $(shell echo *.jpg *.gif)
CLASS           = $(JAVA:.java=.class)
JAVACOMPILE     = javac -verbose -d .
JAVABUILD       = BUILD/uk/org/jape
JAVASRC         = uk/org/jape
#
#
#

all:            $(JAVABUILD) Jape.jar;
$(JAVABUILD):   $(JAVA); mkdir -p $(JAVABUILD); \
                         cp $(JAVA) $(JAVABUILD);\
                         cp $(RESOURCEFILES) BUILD

Jape.jar: $(CLASS);\
                (cd BUILD;\
                 rm -f MANIFEST *.class;\
                 echo "Main-Class: uk.org.jape.Jape" > MANIFEST;\
                 echo $(JAVACOMPILE) $(JAVABUILD)/Jape.java ;\
                 $(JAVACOMPILE) $(JAVASRC)/Jape.java ;\
                 jar cmf MANIFEST Jape.jar $(RESOURCEFILES) $(JAVASRC)/*.class;\
                 mv Jape.jar ..) 
                



