#
#       Linux/Solaris installation script for jape
#       To be executed from the directory in which jape is unpacked
#
JAPEHOME=$1
cd $JAPEHOME
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
        exec java -jar \$JAPEHOME/Jape.jar -engine \$JAPEHOME/jape_engine \$*
ENDSCRIPT
chmod +x jape jape_engine
echo "Jape is now installed in $JAPEHOME"
echo "You can make symbolic or other links to the executable $JAPEHOME/jape"





