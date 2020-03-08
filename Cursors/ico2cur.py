#!/usr/bin/env python
# coding: utf-8

from optparse import OptionParser
import sys

usage = """%prog infile [options]

Reads an ICO file and writes a CUR file. The ICO file should contain a single
image. If no outfile name is provided, the infile name is used to create an
outfile name by changing the extension ".ico" to ".cur".

Example:

    %prog mycursor.ico -x 15 -y 16

This creates a file mycursor.cur with a hotspot at (15, 16).
"""

def main(argv):
    parser = OptionParser(usage=usage)
    parser.add_option("-f", "--outfile",
                      help="File to be created (should end with \".cur\").")
    parser.add_option("-x", "--hotspot-x", dest="x", type="int", default=0,
                      help="Horizontal hotspot position (default is 0).")
    parser.add_option("-y", "--hotspot-y", dest="y", type="int", default=0,
                      help="Vertical hotspot position (default is 0).")
    options, args = parser.parse_args(argv)
    
    if len(args) != 2:
        parser.print_help()
        return
    if not options.outfile:
        filename = args[1]
        if filename.lower().endswith(".ico"):
            filename = filename[:-4]
        options.outfile = filename + ".cur"
    
    try:
        infile = open(args[1], "rb")
        data = infile.read()
        infile.close()
    except IOError:
        print "File \"%s\" could not be read." % args[1]
        return
    
    data = (data[0:2] + chr(2) + data[3:10] + chr(options.x) + data[11:12] 
            + chr(options.y) + data[13:])
    
    try:
        outfile = open(options.outfile, "wb")
        outfile.write(data)
        outfile.close()
    except IOError:
        print "File \"%s\" could not be written." % options.outfile
        return
        
    print "Wrote file \"%s\"." % options.outfile


if __name__ == "__main__":
    main(sys.argv)

