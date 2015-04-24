svn update >/dev/null
svn info | grep 'Last Changed Rev:'|sed  's/Last Changed Rev: //' >SVNLastChangeRevision.inc
