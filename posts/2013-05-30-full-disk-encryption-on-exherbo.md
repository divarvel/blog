---
title: Full Disk Encryption on Exherbo
author: Clement Delafargue
tags: exherbo, crypto, gummiboot, efi, luks
---

I've received my new laptop and I've decided to protect it with full-disk
encryption (everything is encrypted, except the boot files). Since exherbo
does not provide a customisable initramfs (the default one provided by the
kernel is enough most of the time), I had to create one.

Additionnaly, this was the first time I've used a GPT ([GUID Partition
Table](http://en.wikipedia.org/wiki/GUID_Partition_Table)) and configured an
EFI boot.

## General setup

There are two partitions, `/` and a data partition. These two partitions will
be encrypted with LUKS ([Linux Unified Key Setup](http://en.wikipedia.org/wiki/LUKS)).

The keys will be stored in encrypted files on a different USB device, embedded in the Kernel's *initramfs*.

At boot, the *initramfs* is loaded, prompts the key decryption passwords and
gives the deciphered keys to LUKS, which unlock the two partitions. Then the
init script mounts `/` and calls `switch_root` to start the boot process.

The initramfs archive is bundled in the kernel, so the USB device only
contains bootloader config files and the kernel archive.

The bootloader I use is
[gummiboot](http://freedesktop.org/wiki/Software/gummiboot/), which is a
lightweight EFI bootloader.

## Disk setup


### Partition tables

To build a GPT, you need to use `gdisk` instead of `fdisk`. Their UIs are
essentially the same. I think `gparted` also handles GPT, but I've never
tried.

My laptop has a SSD, so it takes extra care when partitioning (it's important
to align partitions with disk sectors). 

Luckily this is handled cleanly by `gdisk` which aligns partitions by default.

One cool thing with GPT is that you can fit many partitions in a table (up to
128 IIRC). No more fiddling with primary / extended partitions and logical
drives \o/.

The boot partition will be handled separately. This also means that your
system can cleanly coexist with a windows install, without any fear of having
the boot sectors wiped.

### LUKS setup

#### Key generation

I've generated two random keyfiles with `dd`

    dd if=/dev/random of=/path/to/key bs=1 count=256

If you're not doing that from a live system, make sure to do it from a `tmpfs`
for extra security.

Next you need to encrypt the keyfiles with openssl:

    openssl aes-256-cbc -in /path/to/key -out /path/to/key.enc

Make sure to backup the encrypted keyfiles, but don't dump the plain keys yet.

#### LUKS setup

Now you need to setup the two LUKS partitions.

    cryptsetup -c <cipher> -y -s <key-size> luksFormat <device> /path/to/key

When it's done for the two partitions, you can delete the plain keys.

You can unlock the partitions now:

    openssl aes-256-cbc -d -in /path/to/key.inc | \
    cryptsetup [--allow-discards] --key-file=- luksOpen <device> <name>

If you have a SSD, you may want to use `--allow-discards`. It creates an
information leak but it enhances your SSD's lifetime.

Check this blog post for more information about trim and LUKS
<http://asalor.blogspot.de/2011/08/trim-dm-crypt-problems.html>

The unlocked device is now available in `/dev/mapper/<name>` and ready to be
formatted.


#### Formatting

For a regular disk, partition as you will.

    mkfs.ext4 /dev/mapper/<name>

For a SSD, some tuning can be useful. For instance it's important to disable
journaling

     sudo tune2fs -O ^has_journal /dev/mapper/<name>

#### Mounting the FS

For a regular disk, just mount it.

    mount /dev/mapper/<name> /path/to/mountpoint

For a SSD, some extra options are useful:

 - `noatime`, `nodiratime` to disable access time logging
 - `nobh`, `data=writeback` to reduce writes
 - `discard` if you have enabled `--allow-discards` in LUKS

### Install your system

You're now able to install the base system. Don't forget to put the mount
options in your `/etc/fstab`.

For an exherbo, I'd recommend the [Exherbo Install Guide](http://www.exherbo.org/docs/install-guide.html)

Don't forget to install an init system (I personally recommend systemd) but
you can choose your favorite.

For the following, I'll consider you've chrooted in your system.

### Make it boot

The next step is to configure your boot device.

#### Partition Table

Create a GPT on your external device, make sure the boot partition has the
correct type (EFI) and has the boot flag activated.

Format it in vfat.

#### Prepare your initramfs

The initramfs is the initial in-memory file system loaded by the boot manager.
It contains the encrypted keys, `cryptsetup` and `openssl` binaries (plus the
needed shared libraries) and an executable named `init` which will be called.
Its work is to unlock the partitions and launch your system init.

To ease things a bit, I've used `busybox` which provides a small shell and
many core tools in a single staticly-built binary.

Since copying binaries and libraries in the archive is tedious, let the kernel
build process do it for you: in a file named `initramfs`, you just have to
specify which files you need, where to get it and where to make it available.

Have a look at my init script, it's quite simple:

[init.bb on github](https://github.com/divarvel/init/blob/master/init.bb)

The `initramfs` file lists the needed files:

[initramfs on github](https://github.com/divarvel/init/blob/master/initramfs)

If you use directly my code, don't forget to `make bb` to make sure the init
file is copied at the right place.

#### Configure your kernel

The next step is quite simple.

Make sure the following options are enabled:

 - `EFI_STUB` (*Processor type and features -> EFI runtime service support -> EFI stub support*)
 - `CRAMFS` (*File systems -> Misc. File systems -> Compressed ROM file system support*)

Then go to *General setup* and configure *Initial RAM filesystem and RAM disk (initramfs/initrd) support*

Enter the path of your `initramfs`.

Now compile your Kernel.

#### Configure gummiboot

Mount the boot partition in `/boot` and run:

    gummiboot install

It will copy the needed files to your external device.

Then copy the kernel image on the device:

    kernel-install add <kernel-version> arch/x86/boot/bzImage

Now check the files in `/boot/loader/` to make sure everything is OK. You
don't have any option to pass to the kernel, the initramfs will be used. You
may want to pass the `quiet` option once your system boots correctly for an
uncluttered boot.

For more information about Gummiboot:

 - [Gummiboot site](http://freedesktop.org/wiki/Software/gummiboot/)
 - [Gummiboot on archwiki](https://wiki.archlinux.org/index.php/Gummiboot)


## Enjoy

As they say in the exherbo install guide

    reboot && sacrifice a goat && pray

You should be prompted the key passwords. Once you see the prompt, you can
safely remove the boot device, its contents are loaded in RAM.
