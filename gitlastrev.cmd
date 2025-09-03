PATH=%PATH%;C:\Program Files\Git\cmd;C:\Cygwin64\bin
git log --max-count=1 --format="'%h'" > GITLastChangeRevision.inc

set /p GITCOMMIT=<.git\refs\heads\master
echo '%GITCOMMIT%' > GitCommit.inc
