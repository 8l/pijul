#! /usr/bin/env bash

set -x
set -e

testdir=$(mktemp -d -t pijul-test.XXXXXXXXXX)
test ! -z $testdir

cd $testdir
mkdir a
cd a
$pijul init
echo saluton > file_to_record
echo malbonaĵo ĉi tie > file_to_leave_alone
$pijul add file_to_record
$pijul record
cd ..
$pijul get a b
cd b
grep saluton file_to_record
test ! -f file_to_leave_alone

rm -rf $testdir
