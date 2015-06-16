#!/bin/bash

PW=$1

psql -U reffit -d reffit -h localhost -f sql/dropReffitTables.sql
psql -U reffit -d reffit -h localhost -f sql/createReffitTables.sql
.cabal-sandbox/bin/acidToSQL backupData.bin $PW
