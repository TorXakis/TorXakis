# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

#remove gh-pages locally
git branch -D gh-pages
#remove gh-pages remotely
git push origin --delete gh-pages
#create new gh-pages branch
git checkout -b gh-pages;
#generated documentation
DOC_DIR=$(stack --work-dir .semaphore-cache/.stack-work --stack-root /home/runner/TorXakis/.semaphore-cache/.stack --allow-different-user --stack-yaml stack_linux.yaml path --local-doc-root)
echo $DOC_DIR
ls $DOC_DIR
#copy documentation (ensure doc directory is empty first)
rm ./doc/* -rf
cp -r $DOC_DIR/. ./doc/;
#add jekyll-theme for github pages
echo "theme: jekyll-theme-slate" > _config.yml
#add documentation to gh-pages branch
git add .;
commitMsg="Haddock @ $(date +%Y%m%d_%H%M%S)";
#commit changes to gh-pages branch
git commit -m "$(echo $commitMsg)";
#push changes to gh-pages branch to remote server
git push --set-upstream origin -f "https://torxakis-admin:$GITHUB_TOKEN@github.com/TorXakis/TorXakis.git";
