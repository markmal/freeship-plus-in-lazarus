BEGIN { print "begin"}
{ #print $0; #$1 "," $2 "," $3;
  if ( $1 == "object") {
    deep++;
    Name=$2; sub(/\:/,"", Name);
    Parents[deep]=Name;
    Type=$3;
    Types[deep]=Type;
    print "  " Name " := " Type "\.Create(Self);";
    print "  with " Name " do";
    print "    begin";
    if(deep>1)
      if (Type == "TMenuItem") {
        if (Types[deep-1] == "TPopupMenu")
           print "    " Parents[deep-1] ".Items.Add(" Name ");" ;
        else if (Types[deep-1] == "TMenuItem")
           print "    " Parents[deep-1] ".Add(" Name ");" ;
        }
      else if (Type == "TAction") print "    ActionList := " Parents[deep-1] ";" ;
      else print "    Parent := " Parents[deep-1] ";" ;
  }
  else if ($1 == "end") {
    print "    end;"
    deep--;
  }
  else if ($3 == "{") {
    binInput=1
    print "    " $1 " := Hex2Bin(";
  }
  else if ($1 == "}") {
    binInput=2
    print "    );";
  }
  else {
    if (binInput==0)
      print "    " $1 " := " $3 $4 $5 $6 $7 $8 $9 ";";
    else if (binInput==1)
      print "    +'" $1 "'";
    else if (binInput==2)
      binInput=0;
  }
}
END {print "end;"}
