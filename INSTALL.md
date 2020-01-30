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
* `pacstrap /mnt base linux linux-firmware netctl wpa_supplicant dhclent vim lvm2 grub efibootmgr intel-ucode dialog`
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

## First boot
### Networking
* `wifi-menu`
* `vim /etc/netctl/hooks/dhcp`
```sh
#!/bin/sh
DHCPClient=dhclient
```
* `chmod +x /etc/netctl/hooks/dhcp`
* `systemctl start netctl-auto@wlp0s20f3.service`
* `systemctl enable netctl-auto@wlp0s20f3.service`

### Userspace (first pass)
* `pacman -S git tmux sudo bash-completion man`
* `pacman -S make gcc pkgconfig patch autoconf automake cmake`

* `useradd -m gustav`
* log in
* `mkdir bin`

* `git clone https://github.com/rootmos/config git/config`
  - assuming `cd ~/git/config` from here
* as root: `cp sudoers /etc/sudoers.d/username`

* `ln -sf ~/git/config/.bashrc ~/.bashrc`
* `./install.sh -h .profile`
* `./install.sh -h .bash_aliases`
* `./install.sh -h .tmux`

* `pacman -S ttf-inconsolata`
* `pacman -S fontconfig freetype2 libxft` (to build st)
* `build/st`

* `pacman -S ruby python`
* `build/vim`
* refresh PATH
* `./install.sh -h .vim`
* `./install.sh -h .vimrc`
* `git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim`
* `vim +BundleInstall`
* `build/YouCompleteMe`
* `build/command-t`

* `pacman -S chromium autocutsel`

* `pacman -S stack dzen2 conky dmenu ttf-dejavu`
* `./install.sh -h .xmonad`
* `~/.xmonad/build`

* `git clone https://github.com/rootmos/dvorak ~/git/dvorak`
* `ln -s ~/git/dvorak/bin/dvorak ~/bin/dv`
* `ln -s ~/git/dvorak/bin/swedish ~/bin/sv`
* `ln -s ~/git/dvorak/bin/english ~/bin/us`
* `ln -s ~/git/dvorak/bin/text ~/bin/text`

* `pacman -S lightdm xorg-server`
* as root: `mkdir /usr/share/xsessions/`
* as root: `cp root/usr/share/xsessions/custom.desktop /usr/share/xsessions/`
* `./install.sh -h .xsession`
* `groupadd -r autologin`
* `gpasswd -a username autologin`
* `vim /etc/lightdm/lightdm.conf`
```
[Seat:*]
autologin-user=username
autologin-session=custom
```
* as root: `systemctl enable lightdm`
* reboot

## Second boot

* `./install.sh -h .gitconfig`
* `./install.sh -h .gitignore_global`
* `pacman -S dunst libnotify`
* `./install.sh -h .config/dunst/dunstrc`

* `pacman -S httping`
* `./install.sh -b bin/drop-ping`

* `pacman -S calc redshift`
* `./install.sh -rs etc/udev/rules.d/backlight.rules`
* `gpasswd -a username video`
* `./install.sh -b bin/brightness`

### AUR
* `git clone https://aur.archlinux.org/yay.git ~/git/yay`
* `pacman -S fakeroot`
* `makepkg -si`

### Audio
* `yay -S spotify`
* `./install.sh -b bin/volume`
* `pacman -S pulseaudio-alsa pavucontrol alsa-utils`

### Credentials
* `pacman -S pass encfs gnupg gtk2 git-crypt`
* `sudo cryptsetup open /dev/sda1 keys`
* `mkdir -p mnt/keys`
* `sudo mount /dev/mapper/keys mnt/keys`
* `encfs ~/.sensitive ~/sensitive`
* `git clone ~/mnt/keys/password-store-private ~/sensitive/password-store/`
* `ln -s ~/sensitive/password-store/ ~/.password-store`
* `gpg --import ~/mnt/keys/gpg/...`
* `mv .gnupg/private-keys-v1.d ~/sensitive/*`
* `ln -s ~/sensitive/private-keys-v1.d ~/.gnupg/private-keys-v1.d`
* remove primary keys
  - `gpg --list-secret-keys --with-keygrip`
  - `rm ~/.gnupg/private-keys-v1.d/...`
* `./install.sh -h .gnupg/gpg-agent.conf`
* remove passphrases and add trust
  - `gpg --edit-key ...`: `passwd` and `trust`
* `./install.sh -b bin/pass-pick`
* `git-crypt unlock`
* `pacman -S usbutils`
* `./install.sh -b sensitive/bin/open*`
* `./install.sh -b sensitive/bin/close*`
* `./install.sh -b sensitive/bin/private`
* `./install.sh -b sensitive/bin/lock-status`

* `ssh-keygen -t rsa -b 4096 -C "your_email@example.com"`
  - generate secret key at `~/sensitive/id_rsa`
* `ln -s ~/sensitive/id_rsa ~/.ssh/id_rsa`
* `cp ~/.ssh/id_rsa ~/mnt/keys/keys/id_rsa_ar1`

* `pass generate laptop/hostname/root 24`
* `sudo passwd`

### Displays
* `pacman -S xorg-xrandr`
* `git clone git@github.com:rootmos/displayswitcheroo.git`
* `git submodule update --init`
* `stack install`
* `./install.sh -h .config/systemd/user/displayswitcheroo.service .config/displayswitcheroo.json`
* `systemctl --user enable displayswitcheroo`

### Firewall
TODO

### USB Guard
TODO

### Mail
TODO
