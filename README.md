A Linux-LVM compatible logical volume manager for mirage
========================================================

[![Build Status](https://travis-ci.org/mirage/mirage-block-volume.png?branch=master)](https://travis-ci.org/mirage/mirage-block-volume) [![Coverage Status](https://coveralls.io/repos/mirage/mirage-block-volume/badge.png?branch=master)](https://coveralls.io/r/mirage/mirage-block-volume?branch=master)


A logical volume manager allows you to group together multiple physical
disks (volumes) and treat them as a pool of disk blocks, from which you
can allocate multiple logical disks (volumes).

This implementation attempts to use the same on-disk format as Linux
LVM with the following features

- constant-time volume manipulation
- linear-mapped logical volumes

Quickstart guide
----------------

The 'mlvm' command-line tool demonstrates how to use the library under Unix:

First, create a fake disk
```
dd if=/dev/zero of=disk.raw bs=1M count=1 seek=128
```

Second, format this physical volume as a trivial volume group:

```
mlvm format disk.raw
```

Third, query the metadata stored on the volume:

```
mlvm read disk.raw

key                                 |value                                  |
------------------------------------|---------------------------------------|
name                                |volumegroup                            |
id                                  |fJvDZS-P7iF-T6aH-cGEa-QCA9-5Jc8-26t4Dp |
status                              |READ, WRITE                            |
extent_size                         |8192                                   |
max_lv                              |0                                      |
max_pv                              |0                                      |
physicalvolume/name                 |physicalvolume                         |
physicalvolume/id                   |Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw |
physicalvolume/stored_device        |disk.raw                               |
physicalvolume/real_device          |disk.raw                               |
physicalvolume/status               |ALLOCATABLE                            |
physicalvolume/size_in_sectors      |264192                                 |
physicalvolume/pe_start             |20608                                  |
physicalvolume/pe_count             |29                                     |
physicalvolume/label/id             |Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw |
physicalvolume/label/device_size    |135266304                              |
physicalvolume/label/extents        |1                                      |
physicalvolume/label/metadata_areas |1                                      |
free_space                          |29                                     |
```

Fourth, create a trivial logical volume (of size 0):
```
mlvm create disk.raw myfirstvolume
```

Fifth, query the metadata again, to see the volume:
```
mlvm read disk.raw

key                                 |value                                  |
------------------------------------|---------------------------------------|
name                                |volumegroup                            |
id                                  |fJvDZS-P7iF-T6aH-cGEa-QCA9-5Jc8-26t4Dp |
status                              |READ, WRITE                            |
extent_size                         |8192                                   |
max_lv                              |0                                      |
max_pv                              |0                                      |
physicalvolume/name                 |physicalvolume                         |
physicalvolume/id                   |Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw |
physicalvolume/stored_device        |disk.raw                               |
physicalvolume/real_device          |disk.raw                               |
physicalvolume/status               |ALLOCATABLE                            |
physicalvolume/size_in_sectors      |264192                                 |
physicalvolume/pe_start             |20608                                  |
physicalvolume/pe_count             |29                                     |
physicalvolume/label/id             |Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw |
physicalvolume/label/device_size    |135266304                              |
physicalvolume/label/extents        |1                                      |
physicalvolume/label/metadata_areas |1                                      |
myfirstvolume/name                  |myfirstvolume                          |
myfirstvolume/id                    |Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw |
myfirstvolume/tags                  |                                       |
myfirstvolume/status                |READ, VISIBLE                          |
myfirstvolume/segments              |1                                      |
free_space                          |29                                     |
```

