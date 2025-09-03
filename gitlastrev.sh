#!/bin/bash
read OLDGREV < GITLastChangeRevision.inc
NEWGREV=$(/usr/bin/git log --max-count=1 --format="'%h'")
if [ "$OLDGREV" != "${NEWGREV}" ]; then
  echo "${NEWGREV}" > GITLastChangeRevision.inc
fi

read GITCOMMIT < .git/refs/heads/master
echo "'${GITCOMMIT}'" > GitCommit.inc
