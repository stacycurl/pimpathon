#!/usr/bin/env bash
cd "$(dirname "$0")"

rm -f my_message.txt
echo "$MY_MESSAGE" > my_message.txt

rm -f my_message.txt.asc
gpg --import foofy.public
gpg --list-keys
gpg --batch --armor --recipient foofy2 --trust-model always --encrypt my_message.txt

cat my_message.txt.asc
