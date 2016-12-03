## Arch Linux installation notes
This process is based on the [installation guide](https://wiki.archlinux.org/index.php/Installation_guide) in the Arch Wiki. This it's just a collection of orderly steps to save time when installing a new Arch system. If you require more details about something please see the Wiki
### 1. Installation media
Download the last ISO
* [https://www.archlinux.org/download/](https://www.archlinux.org/download/)

Connect an empty USB memory and verify their assignation
```
# fdisk -l
```
Load the downloaded ISO in the device
```
# dd bs=4M if=[ARCHLINUX.ISO] of=/dev/sd[X] status=progress && sync
```
Boot with the USB device in UEFI mode
### 2. Set keyboard layout 
``` 
# loadkeys la-latin1
```
### 3. Verify that the boot mode is enabled in UEFI
``` 
# ls /sys/firmware/efi/efivars 
```
If the directory does not exist, the system is booted in BIOS mode
### 4. Connect to the Internet
Wired with DHCP
``` 
# dhcpd 
```
Wired with static IP
```
# ip link set eno1 up
# ip addr add [IP]/[MASK] broadcast [BROADCAST] dev eno1
# ip route add default via [GATEWAY]
```
Wireless 
``` 
# wifi-menu 
```
### 5. Update system clock 
``` 
# timedatectl set-ntp true
# timedatectl status
```
### 6. Partitioning 
If UEFI is enabled, create an EFI System Partition for boot

Identify your device
``` 
# fdisk -l 
```
Partitions
``` 
# parted /dev/sd[X] 
```
```
(parted) mktable gpt
(parted) mkpart ESP fat32 1MiB 513MiB
(parted) set 1 boot on
(parted) mkpart primary linux-swap 513MiB 2.5GiB
(parted) mkpart primary ext4 2.5GiB 42.5GiB
(parted) mkpart primary ext4 42.5GiB 100%
```
This configuration is for a disk of 120GB and UEFI mode
* Boot: 512MB
* Swap: 2GB
* Root: 40GB
* Home: ~70GB
### 7. Format partitions
Verify your partition table
```
# lsblk
```

Format partition (replace X with your device letter)
```
# mkfs.vfat /dev/sd[X]1
# mkswap /dev/sd[X]2
# mkfs.ext4 /dev/sd[X]3
# mkfs.ext4 /dev/sd[X]4
```
### 8. Mount the file system
Always replace X with yout device letter)
```
# swapon /dev/sd[X]2
# mount /dev/sd[X]3 /mnt
# mkdir /mnt/boot
# mount /dev/sd[X]1 /mnt/boot
# mkdir /mnt/home
# mount /dev/sd[X]4 /mnt/home
```
### 9. Configure mirrors
```
# cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup
# sed -i 's/^#Server/Server/' /etc/pacman.d/mirrorlist.backup
# rankmirrors -n 6 /etc/pacman.d/mirrorlist.backup > /etc/pacman.d/mirrorlist
# pacman -Syy
```
This file will later be copied to the new system by pacstrap
### 10. Install the base packages
``` 
# pacstrap /mnt base base-devel 
```
### 11. Configure base system
#### 11.1 File system
Generate the fstab file
``` 
# genfstab -U /mnt >> /mnt/etc/fstab 
```
#### 11.2 Change to the new system
``` 
# arch-chroot /mnt 
```
#### 11.3 Time
Set timezone (my current zone is America/Bogota)
``` 
# ln -s /usr/share/zoneinfo/America/Bogota /etc/localtime 
```
Adjust hardware clock
``` 
# hwclock --systohc --utc
```
#### 11.4 Locale
Uncomment needed localizations in __/etc/locale.gen__, and run:
``` 
# locale-gen 
```
In my case en_US.UTF-8 and es_CO.UTF-8

Set the LANG variable
```
# echo LANG=en_US.UTF-8 > /etc/locale.conf
```
Set the KEYMAP variable
```
# echo KEYMAP=la-latin1 > /etc/vconsole.conf
```
#### 11.5 Users
Create a new user for the system
```
# useradd -m -G wheel -s /bin/bash [USER]
# gpasswd --add [USER] uucp
# passwd [USER]
```
Configure sudo for scalate permissions 
```
# visudo
```
Uncomment line 
```
%wheel ALL=(ALL) ALL
```
Set password for root
```
# passwd
```
#### 11.6 Yaourt 
Edit the file /etc/pacman.conf
```
[archlinuxfr]
SigLevel = Never
Server = http://repo.archlinux.fr/$arch
```
Update repository and install Yaourt
```
# pacman -Syy
# pacman -S yaourt
```
#### 11.7 Initramfs
Initial RAM disk
```
# mkinitcpio -p linux
```
If your see some warnings about firmware __wd719x__ and __aic94xx__
```
# pacman -S base-devel
# su your_user
$ cd ~
$ yaourt -S wd719x-firmware aic94xx-firmware
$ exit
# mkinitcpio -p linux
```
#### 11.8 Boot
Boot using systemd
```
# bootctl install
```
Get UUID for the root partition
```
# blkid -s PARTUUID -o value /dev/sd[X]3 >> /boot/loader/entries/arch.conf
```
Edit /boot/loader/entries/arch.conf
```
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options root=PARTUUID=[UUID_FROM_PREVIOUS_STEP] wr
```
### 12 Reboot
Exit from chroot and reboot
```
# exit
# shutdown now
```
Remove the installation media and proceed to configure the new system
