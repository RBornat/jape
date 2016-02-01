#
# this Makefile exists ONLY to execute the code that calculates major and minor numbers
# and calls ant to write Version.java.
# I would do it in build.xml if I knew how.
#

version:  ; export MAJOR=`git describe --tags --always --abbrev=0`; \
            export MINOR=`git rev-list $$MAJOR.. --count`; \
            ant version -Dguiversion=$$MAJOR.$$MINOR
