#       $Id$
#
#       Rules that force a complete recompile from the root when a 
#       source file changes. 
#
#       Use ANT  if you want a more discriminating recompilation
#
.SUFFIXES:      .jar .java .class

#
#             
#
JAVABUILD       = ./BUILD
JAVASRC         = .
JAVA            = $(shell find $(JAVASRC) -name "*.java")
RESOURCEFILES   = $(shell echo *.jpg *.gif)
JAVACOMPILE     = javac -verbose -sourcepath $(JAVASRC) -d $(JAVABUILD)
#
#
#

all:            $(JAVABUILD) Jape.jar; 
$(JAVABUILD):   ; mkdir -p $(JAVABUILD)

Jape.jar: $(JAVA) $(JAVABUILD);  (rm -f MANIFEST;\
                                  echo "Main-Class: uk.org.jape.Jape" > MANIFEST;\
                                  $(JAVACOMPILE) uk/org/jape/Jape.java ;\
                                  jar cmf MANIFEST Jape.jar $(RESOURCEFILES) -C $(JAVABUILD) uk;\
                                 )

clean:          ; rm -rf Jape.jar $(JAVABUILD)


               
                





