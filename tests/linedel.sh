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
echo malbonaĵo >> file_to_record
echo ĉi tie >> file_to_record
$pijul add file_to_record
$pijul record
cd ..
$pijul get a b

cd a
echo saluton > file_to_record
echo ĉi tie >> file_to_record
$pijul record

cd ../b
echo y | $pijul pull ../a

echo ynn | $pijul rollback

$pijul debug
dot -Tpdf -o /home/pe/Recherche/pijul/pijul/test.pdf debug

rm -rf $testdir
