MPEG-TS
=======

Haskell implementation of mpegts parser.
The program can print info for mpegts (188 B per packet), demux streams, detect PCR discontinuities etc.


Installation
============

    git clone github.com/hepek/MPEG-TS
    cd MPEG-TS
    cabal install


Usage
=====

Running the program without parameters will display its options:

    MPEG-TS - a program for MPEGTS stream analysis.
    Usage: MPEG-TS info          <FILE>               #to view stream info of a file
           MPEG-TS adaptation    <FILE>               #to view stream adaptation fields
           MPEG-TS discont <PID> <FILE>               #to view stream discontinuities
           MPEG-TS hasDiscont    <FILE>               #to check for discontinuities in any program
           MPEG-TS uniqPids <FILE>                    #to display a list of unique PIDs in a stream
           MPEG-TS demux pid <SOURCEFILE> <DESTFILE>  #to demux a file

info
----
 
Will display first valid PCR information

adaptation
----------

Will print all adaptation fields of a stream

discont and hasDiscont
----------------------

Attempts to discover discontinuities in the stream by examining appropriate flags in adaptation fields.

uniqPids
--------

Prints a list of unique PIDs found in a file. Similar to info, but may take more time, as it needs to pass trough entire file to terminate.

demux
-----

Demuxes a PID into a destination file. Inspect available PIDs with info or demux option.

TODO:
=====

1. Add parser config for user-configurable error handling
2. Add CRC32 checking
3. Add transmission error checking
4. Add continunuity count checking and/or discarding/see (1.)