#!/bin/bash
read OLDGREV < GITLastChangeRevision.inc
NEWGREV=$(/usr/bin/git rev-list HEAD --count)
if [ "$OLDGREV" != "$NEWGREV" ]; then
  echo $NEWGREV > GITLastChangeRevision.inc
fi

read GITCOMMIT < .git/refs/heads/master
echo "'${GITCOMMIT}'" > GitCommit.inc
