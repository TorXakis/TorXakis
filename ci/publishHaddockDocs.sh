if [ $TRAVIS_BRANCH == "develop" ]; then
 rm *.log;
 rm *.zip;
 ./updateAndPushGhPages.sh;
fi