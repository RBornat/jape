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

all:            japeserver.jar; 

japeserver.jar: $(CLASS);\
                rm -f MANIFEST *.class;\
                echo "Main-Class: japeserver" > MANIFEST;\
                echo $(JAVACOMPILE) japeserver.java ;\
                $(JAVACOMPILE) japeserver.java ;\
                jar cmf MANIFEST japeserver.jar *.class $(RESOURCEFILES)
                

