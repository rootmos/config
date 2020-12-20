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

* `pacman -S chromium autocutsel hsetroot`

* `pacman -S stack dzen2 conky dmenu ttf-dejavu libxrandr libxss`
* `./install.sh -h .xmonad`
* `~/.xmonad/build`

* `git clone https://github.com/rootmos/dvorak ~/git/dvorak`
* `ln -s ~/git/dvorak/bin/dvorak ~/bin/dv`
* `ln -s ~/git/dvorak/bin/swedish ~/bin/sv`
* `ln -s ~/git/dvorak/bin/english ~/bin/us`
* `ln -s ~/git/dvorak/bin/text ~/bin/text`

* `pacman -S lightdm xorg-server`
* as root: `mkdir /usr/share/xsessions/`
* as root: `cp usr/share/xsessions/custom.desktop /usr/share/xsessions/`
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

* `./install.sh -b bin/tmx`

* `./install.sh -h .gitconfig`
* `./install.sh -h .gitignore_global`
* `pacman -S dunst libnotify`
* `./install.sh -h .config/dunst/dunstrc`

* `./install.sh -b bin/border-width`

* `pacman -S calc redshift`
* `./install.sh -rs etc/udev/rules.d/backlight.rules`
* `gpasswd -a username video`
* `./install.sh -b bin/brightness`

* `pacman -S pacman-contrib curl`
* `bin/refresh-mirrorlist`

### OCaml
* `pacman -S opam`
* `opam init`

### AUR
* `git clone https://aur.archlinux.org/yay.git ~/git/yay`
* `pacman -S fakeroot`
* `makepkg -si`

### Audio
* `yay -S spotify`
* `./install.sh -b bin/volume`
* `pacman -S pulseaudio-alsa pavucontrol alsa-utils`

### Credentials
* `pacman -S pass encfs gnupg gtk2 git-crypt openssh inetutils udisks2`
* `sudo cryptsetup open /dev/sda1 keys`
* `mkdir -p mnt/keys`
* `sudo mount /dev/mapper/keys mnt/keys`
* `gpg --import ~/mnt/keys/gpg/...`
* `ssh-keygen -t rsa -b 4096 -C "your_email@example.com"`
  - generate secret key at `~/sensitive/id_rsa`
* `git-crypt unlock`
* `sudo cp sensitive/root/etc/polkit-1/rules.d/90-udisks2.rules /etc/polkit-1/rules.d/`
* `./install.sh -b sensitive/bin/zones`
* remove primary keys
  - `gpg --list-secret-keys --with-keygrip`
  - `rm ~/.gnupg/private-keys-v1.d/...`
* add trust

* `./install.sh -b sensitive/bin/open_keys`
* `./install.sh -b sensitive/bin/close_keys`

* `pass generate -n hosts/$(hostname)/root 32`
* `sudo passwd`

* `sudo cp bin/pass bin/pass-pick /usr/local/bin`

### Displays
* `pacman -S xorg-xrandr`
* `git clone git@github.com:rootmos/displayswitcheroo.git`
* `git submodule update --init`
* `stack install`
* `./install.sh -h .config/systemd/user/displayswitcheroo.service .config/displayswitcheroo.json`
* `systemctl --user enable displayswitcheroo`

### Mail
* `pacman -S mutt offlineimap w3m`
* `./install.sh -h .mutt .muttrc .offlineimap.py .offlineimaprc`
* `./install.sh -b bin/mm bin/fork-xargs-pipe`
* `systemctl --user enable offlineimap@private.service`
* `systemctl --user start offlineimap@private.service`
* `systemctl --user enable offlineimap@work.service`
* `systemctl --user start offlineimap@work.service`

### Firewall
* 'sudo pacman -S ufw'
* 'sudo ufw default deny'
* 'sudo ufw enable'
* 'sudo systemctl enable ufw.service'

Check logs for blocked traffic: `dmesg -w | grep UFW`.

### DNS over TLS
* `sudo pacman -S stubby dnsmasq`
* `sudo cp sensitive/root/etc/stubby/stubby.yml /etc/stubby/stubby.yml`
* `sudo cp root/etc/{resolv.conf,dnsmasq.conf} /etc`
* `sudo chattr +i /etc/resolv.conf`
* `sudo systemctl enable stubby dnsmasq`
* `sudo systemctl start stubby dnsmasq`

Use `tshark -Y dns` to verify that queries are not sent in plaintext.

### USB Guard
TODO

### Printer
* `pacman -S avahi nss-mdns`
* `vim /etc/nsswitch.conf`
  - `hosts: ... mdns_minimal [NOTFOUND=return] resolve [!UNAVAIL=return] dns ...`
* `systemctl enable avahi-daemon.service`
* `systemctl start avahi-daemon.service`
* `pacman -S cups foomatic-db-ppds`
* `systemctl enable org.cups.cupsd.service`
* `systemctl start org.cups.cupsd.service`

### PDF (zathura)
* `xdg-mime default org.pwmt.zathura-pdf-mupdf.desktop application/pdf`
* `./install.sh -h .config/zathura/zathurarc`

### Default applications
* `bin/xdg-install .local/share/applications/view.desktop`
* `bin/xdg-install .local/share/applications/play.desktop`

### Automounter
* `./install.sh -b bin/automount`
* `./install.sh -h .config/systemd/user/automount.service`
* `systemctl --user enable automount.service`
* `systemctl --user start automount.service`
