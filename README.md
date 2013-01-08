MPEG-TS
=======

Haskell implementation of mpegts parser.

Can print info for mpegts (188 B per packet), demux streams, detect PCR discontinuities etc.

TODO:
=====

1. Add parser config for user-configurable error handling
2. Add CRC32 checking
3. Add transmission error checking
4. Add continunuity count checking and/or discarding/see (1.)