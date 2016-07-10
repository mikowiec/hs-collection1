#!/bin/sh

set -e

( cd /opt/repos ; darcs get --partial $1 $2 ; bin/sync-pkg.hs )

darcs get --partial /opt/repos/$2 $3

