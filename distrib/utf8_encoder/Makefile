#       $$
#
#       free software: do what you like with it.
#

.SUFFIXES:      .jar .java .class

#
#             
#

JAVACOMPILE     = javac -verbose 

#
#
#

encoder.jar: encoder.java;  (rm -f MANIFEST;\
                                  echo "Main-Class: encoder" > MANIFEST;\
                                  $(JAVACOMPILE) encoder.java ;\
                                  jar cmf MANIFEST encoder.jar encoder.java encoder.class;\
                                 )

clean:          ; rm -rf encoder.jar encoder.class


               
                





