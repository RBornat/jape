#
#       Rules that force a complete recompile from the root when a source file changes
#       I've not found a reliable java dependency generator, and
#       the compiler start overhead is too ...  large to
#       use it on individual files.
#
#       ANT here we come.
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
#
#
#

all:            Jape.jar; 

Jape.jar: $(CLASS);\
                rm -f MANIFEST *.class;\
                echo "Main-Class: Jape" > MANIFEST;\
                echo $(JAVACOMPILE) Jape.java ;\
                $(JAVACOMPILE) Jape.java ;\
                jar cmf MANIFEST Jape.jar *.class $(RESOURCEFILES)
                


