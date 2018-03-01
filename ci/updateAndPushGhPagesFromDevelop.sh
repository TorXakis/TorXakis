# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

git remote remove origin;
git remote add origin https://github.com/TorXakis/TorXakis.git;
git fetch;
git checkout gh-pages;
git rebase develop;
rm ./doc/* -rf
DOC_DIR=$(stack path --local-doc-root)
echo $DOC_DIR
ls $DOC_DIR
# cp --verbose -r $DOC_DIR/. ./doc/;
# git add .;
# commitMsg="Haddock @ $(date +%Y%m%d_%H%M%S)";
# git commit -m "$(echo $commitMsg)";
# git push -f "https://keremispirli:$GITHUB_TOKEN@github.com/TorXakis/TorXakis.git";
