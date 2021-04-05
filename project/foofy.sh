#!/usr/bin/env bash
cd "$(dirname "$0")"

echo "$MY_MESSAGE" > my_message.txt

gpg --import foofy.public
gpg --list-keys
gpg --batch --armor --recipient foofy --trust-model always --encrypt my_message.txt

cat my_message.txt.asc
