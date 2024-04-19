set WIX="C:\Program Files (x86)\WiX Toolset v3.14\bin"
%WIX%\heat dir "%1" -directoryid "%1" -cg "%1" -gg -sfrag -suid -dr "APPLICATIONFOLDER" -var "var.%1" -wixvar -out "%2.wxs"
