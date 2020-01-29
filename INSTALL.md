# Installation sketch

## Links
* https://wiki.archlinux.org/index.php/installation_guide
* https://www.archlinux.org/download/

## BIOS setup
* disable secure boot

## Download and burn the installation medium
```sh
VERSION=2020.01.01
wget http://mirror.rackspace.com/archlinux/iso/$VERSION/archlinux-$VERSION-x86_64.iso
pacman-key -v archlinux-$VERSION-x86_64.iso.sig
sudo dd bs=4M if=archlinux-$VERSION-x86_64.iso of=/dev/null status=progress oflag=sync
```

## Live boot
* `wifi-menu`
* `timedatectl set-ntp true`
* `gdisk /dev/nvme0n1`, GPT, EFI + Linux
* `cryptsetup luksFormat --type=luks1 /dev/nvme0n1p2`
  - https://savannah.gnu.org/bugs/?55093
* `cryptsetup open /dev/nvme0n1p2 lvm`
* `pvcreate /dev/mapper/lvm`
* `vgcreate g0 /dev/mapper/lvm`
* `lvcreate --name=root --size=32G g0`
* `lvcreate --name=swap --size=16G g0`
* `lvcreate --name=home -l 100%FREE g0`
* `mkfs.ext4 /dev/mapper/g0-root`
* `mkfs.ext4 /dev/mapper/g0-home`
* `mkswap /dev/mapper/g0-swap`
* `swapon /dev/mapper/g0-swap`
* `mount /dev/mapper/g0-root /mnt`
* `mount /dev/nvme0n1p1 /mnt/mnt/efi`
* `mkdir /mnt/home /mnt/mnt/efi`
* `mount /dev/mapper/g0-home /mnt/home`
* `dd bs=512 count=4 if=/dev/random of=/mnt/keyfile iflag=fullblock`
* `chmod 0600 /mnt/keyfile`
* `cryptsetup luksAddKey /dev/nvme0n1p2 /mnt/keyfile`
* Edit mirror list (pick a few of a local/newly refreshed list)
* `pacstrap /mnt base linux linux-firmware netctl wpa_supplicant dhclent vim lvm2 grub efibootmgr intel-ucode`
* `genfstab -U /mnt >> /mnt/etc/fstab`
* `arch-chroot /mnt`
* `ln -sf /usr/share/zoneinfo/Region/City /etc/localtime`
* `hwclock --systohc`
* `echo en_US.UTF-8 UTF-8 > /etc/locale.gen`
* `locale-gen`
* `echo LANG=en_US.UTF-8 > /etc/locale.conf`
* `vim /etc/hostname`
* `vim /etc/hosts`
* `passwd`
* `vim /etc/mkinitcpio.conf`
  - `FILES=(/keyfile)`
  - `HOOKS=(base udev ... block encrypt lvm2 filesystems)`
* `mkinitcpio -P`
* `vim /etc/default/grub`
  - `GRUB_CMDLINE_LINUX_DEFAULT="loglevel=3 quiet cryptdevice=/dev/nvme0n1p2:g0 cryptkey=rootfs:/keyfile"`
  - `GRUB_PRELOAD_MODULES="part_gpt part_msdos lvm"`
  - `GRUB_ENABLE_CRYPTODISK=y`
* `grub-install --target=x86_64-efi --efi-directory=/mnt/efi --bootloader-id=GRUB`
* `grub-mkconfig -o /boot/grub/grub.cfg`
* reboot
