#
#       Make the bootstrap installer classes
#
bootstrap: install.java; rm -rf bootstrap; mkdir bootstrap; javac -d . install.java

#
#       Test 'em
#
tests:  test1 test2 ; rm -rf test; mkdir test; mv test1.jar test2.jar test; cd test; java -jar test1.jar; java -jar test2.jar 
fails:  test3 test4 ;

test1: ; cp SHORTCUT.EXE bootstrap; \
         java bootstrap.install test1.jar\
                   -classwindows installjape installjape.class \
                   -cmdunix      "echo hurrah, you won" \
                   -cmdwindows   'SHORTCUT.EXE -f -t jape.cmd -n Jape -d . -i japeicon.ico' \
                   -app          "Test Application" \
                   -splash       japesplash.png \
                   SHORTCUT.EXE japeicon.ico  Makefile* install.java 
                   
test2: ; java bootstrap.install test2.jar -onunix "ls -l " -app "Another Application" Makefile* install.java
test3: ; java bootstrap.install -icon japesplash.gif Makefile* install.java
test4: ; java bootstrap.install Makefile* install.java

#
#
#
clean:  ; rm -rf test bootstrap test*.jar








