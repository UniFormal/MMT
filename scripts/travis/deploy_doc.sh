#!/bin/bash

SOURCE_BRANCH="master"
if [ "$TRAVIS_PULL_REQUEST" != "false" -o "$TRAVIS_BRANCH" != "$SOURCE_BRANCH" ]; then
    echo "Not on the master branch; nothing to commit"
    exit 0
fi

# Get the SHA
SHA=$(git rev-parse --short HEAD)

# Decrypt ssh key
ENCRYPTED_KEY_VAR="encrypted_${ENCRYPTION_LABEL}_key"
ENCRYPTED_IV_VAR="encrypted_${ENCRYPTION_LABEL}_iv"
ENCRYPTED_KEY=${!ENCRYPTED_KEY_VAR}
ENCRYPTED_IV=${!ENCRYPTED_IV_VAR}
openssl aes-256-cbc -K $ENCRYPTED_KEY -iv $ENCRYPTED_IV -in scripts/travis/deploy_key.enc -out scripts/travis/deploy_key -d
chmod 600 scripts/travis/deploy_key
eval `ssh-agent -s`
ssh-add scripts/travis/deploy_key

# Build the API doc
cd src && sbt apidoc && cd ..

# Clone the git repo and do some setup
git clone git@github.com:UniFormal/apidoc.git apidoc_deploy
cd apidoc_deploy && git config user.name "Travis CI" && cd ..
cd apidoc_deploy && git config user.email "$COMMIT_AUTHOR_EMAIL" && cd ..


# Cleanup and copy files
cd apidoc_deploy && rm -rf * && git checkout .nojekyll && cd ..
cp -r apidoc/* apidoc_deploy

# Make a commit and push
cd apidoc_deploy && git add -A . && git commit -m "Auto-generate api documenation from UniFormal/MMT@$SHA" && git push origin gh-pages && cd ..
