#! /usr/bin/env bash

set -x
set -e

testdir=$(mktemp -d -t pijul-test.XXXXXXXXXX)
test ! -z $testdir

function finish {
    if [ ! -z $testdir ]
    then rm -rf "$testdir"
    fi
}
trap finish EXIT

cd $testdir
mkdir a
cd a
$pijul init
echo saluton > file_to_record
echo malbonaĵo ĉi tie > file_to_leave_alone
$pijul add file_to_record
$pijul record patch1
$pijul add file_to_leave_alone
$pijul record patch2
echo yn | $pijul rollback
grep malbonaĵo file_to_leave_alone
cd ..
$pijul get a b
cd b
$pijul debug
dot -Tpdf -o /home/pe/Recherche/pijul/pijul/test.pdf debug
ls -a
grep saluton file_to_record
test ! -f file_to_leave_alone

rm -rf $testdir
