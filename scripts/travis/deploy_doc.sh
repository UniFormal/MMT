#!/bin/bash

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

# Clone the repository
git clone git@github.com:UniFormal/apidoc.git

# Build the API doc
cd src && sbt apidoc && cd ..

cd apidoc

# Configure git
git config user.name "Travis CI"
git config user.email "$COMMIT_AUTHOR_EMAIL"
git add .

git commit -m "Auto-generate api documenation from UniFormal/MMT@$SHA"
git push