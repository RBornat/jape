#
#       Linux/Solaris installation script for jape
#       To be executed from the directory in which jape is unpacked
#
JAPEHOME=`pwd`
if which java
then
   JAVABIN=`which java`
else
   echo "Aborting Installation -- please arrange that java is on your PATH during installation"
   exit 1
fi
cat <<ENDSCRIPT>jape
        JAPEHOME=$JAPEHOME 
        export JAPEHOME
        JAPESERVER=$JAPEHOME/japeserver 
        export JAPESERVER
        $JAPEHOME/jape.engine "\$*"
ENDSCRIPT
echo  exec $JAVABIN -jar $JAPEHOME/japeserver.jar > japeserver
chmod +x jape japeserver jape.engine
echo "Jape is now installed in $JAPEHOME"
echo "You can make symbolic or other links to the executable $JAPEHOME/jape"


