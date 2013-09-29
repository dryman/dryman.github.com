---
layout: post
title: "Installing Gentoo Linux on MBPR"
date: 2013-08-17 22:10
comments: true
published: false
categories: linux, Gentoo
---

## Boot USB

```
$ hdiutil convert -format UDRW install-amd64-minimal-20130816.iso -o gentoo.img bs=1m
$ mv gentoo.img.dmg gentoo.img
```

```
$ diskutil list
...
/dev/disk1
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:     FDisk_partition_scheme                        *8.0 GB     disk1
   1:                 DOS_FAT_32 GENTOO                  8.0 GB     disk1s1

$ diskutil unmount /dev/disk1
# dd if=gentoo.img of=/dev/disk1
```



## DiskUtil

Partition disk

Varify disk

Repair disk in rescue mode (cmd-r from when reboot)




## rEFInd

```
./install --esp
```

## CFLAGS

```
$ sysctl -n machdep.cpu.brand_string
Intel(R) Core(TM) i7-3615QM CPU @ 2.30GHz
```

http://gcc.gnu.org/onlinedocs/gcc/i386-and-x86_002d64-Options.html

```
‘corei7’
Intel Core i7 CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1 and SSE4.2 instruction set support. 
‘corei7-avx’
Intel Core i7 CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, AVX, AES and PCLMUL instruction set support. 
‘core-avx-i’
Intel Core CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, AVX, AES, PCLMUL, FSGSBASE, RDRND and F16C instruction set support. 
‘core-avx2’
Intel Core CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2 and F16C instruction set support.
```

```
-O2 -pipe -march=corei7-avx
```

http://www.broadcom.com/products/Wireless-LAN/802.11-Wireless-LAN-Solutions/BCM4331

Device Driver
-> Network device support
-> Wireless Lan
-> Boardcom 43xx wireless support (mac 80211 stack)

Device Driver
-> generic input layer
-> Mice
apple USB trouchpad support
apple USB BCM5974 Multitouch trackpad support
