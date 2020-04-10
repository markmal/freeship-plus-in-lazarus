PATH=%PATH%;C:\Program Files\Git\cmd
git rev-list HEAD --count 1>GITLastChangeRevision.inc

set /p GITCOMMIT=<.git\refs\heads\master
echo '%GITCOMMIT%' > GitCommit.inc
