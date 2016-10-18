## Configuration
These are my most common configurations in a new installation. All these are based on [Wiki](https://wiki.archlinux.org/index.php/General_recommendations#Window_managers)
### Network
##### Hostname
```
# echo [HOSTNAME] > /etc/hostname
```
Edit and add to /etc/hosts:
```
127.0.1.1 [HOSTNAME].localdomain [HOSTNAME]
```
##### Device names
Identify devices
```
# ls /sys/class/net
```
Get the MAC address
```
# cat /sys/class/net/[DEVICE]/address >> /etc/udev/rules.d/10-network.rules
```
Create udev rule in /etc/udev/rules.d/10-network.rules
```
SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="aa:bb:cc:dd:ee:ff", NAME="ethernet"
```
Repeat the same steps for others devices, by example __wifi__ is the name for my wireless interface
##### Wired / Dynamic IP
Create file /etc/systemd/network/ethernet.network
```
[Match]
Name=ethernet

[Network]
DHCP=ipv4
```
##### Wired / Static IP
Create file /etc/systemd/network/ethernet.network
```
[Match]
Name=ethernet
   
[Network]
DNS=192.168.1.254
Address=192.168.1.94/24
Gateway=192.168.1.254
```
##### Wireless
Install packages
```
# pacman -S wpa_supplicant iw dialog
```

Setup wifi access
```
# wpa_passphrase [SSID] [PASSWORD] > /etc/wpa_supplicant/wpa_supplicant-wifi.conf
# systemctl enable wpa_supplicant@wifi.service
# systemctl start wpa_supplicant@wifi.service

```
Create file /etc/systemd/network/wireless.network
```
[Match]
Name=wifi

[Network]
DHCP=ipv4
```
##### Service
Enable and start networkd  the service
```
# systemctl enable systemd-networkd
# systemctl start systemd-networkd
```
### Xorg
#### Base packages
```
# pacman -S xorg-server xorg-server-utils xorg-apps xorg-xinit
```
#### Video drivers. 
NVIDIA video driver
```
# pacman -S nvidia
```
Choose nvidia-libgl, xf86-input-libinput

#### Bumblebee
Only for new hybrid video cards in new laptops
```
# pacman -S bumblebee mesa xf86-video-intel nvidia 
# gpasswd -a user bumblebee
# systemctl enable bumblebee.service
# systemctl start bumblebee.service
```
#### Keyboard
Create LA keyboard in /etc/X11/xorg.conf.d/00-keyboard.conf  
```
Section "InputClass"
	Identifier "default-keyboard"
	MatchIsKeyboard "on"
	Option "XkbLayout" "latam"
	Option "XkbModel" "pc105"
	Option "XkbVariant" "deadtilde"
EndSection
```
#### Multi-monitors
See the names of earch screen
```
# xrandr
```
Create an archive to configure all monitors /etc/X11/xorg.conf.d/10-monitor.conf
```
Section "Monitor"
    Identifier  "eDP-1"
	Option      "Primary" "true"
EndSection

Section "Monitor"
    Identifier  "HDMI-1"
	Option      "LeftOf" "eDP-1"
	Option		"Rotate" "left"
EndSection
```
### Microcode
For intel processors
```
# pacman -S intel-ucode
```
Use the initrd option twice in /boot/loader/entries/arch.conf
```
title   Arch Linux
linux   /vmlinuz-linux
initrd  /intel-ucode.img
initrd  /initramfs-linux.img
options ...
```
Verify that microcode got updated on boot
```
$ dmesg | grep microcode
```
### Zsh
```
# pacman -S zsh wget git
# sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
```
Syntax highlight 
```
# pacman -S zsh-syntax-highlighting
```
Add to ~/.zshrc:
```
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
```
Command autosuggestion
```
$ yaourt -S zsh-autosuggestions
```
Add to ~/.zshrc:
```
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
```
### Window manager
```
# pacman -S xmonad xmonad-contrib xmobar dmenu
```
Create the file ~/.xinitrc
```
exec xmonad
```
### Utilities
* Images
* Screenshot: scrot
* Color picker: gpick
* Terminal: termite
* Browser: surf google-chrome-stable
* Editor: vim (pacman -Rcs nano)
* System info: archey3
* Monitor: inxi lm_sensors net-tools hddtemp demicode

### Fonts
Install fonts
```
# pacman -S ttf-inconsolata
$ yaourt -S inconsolata-psf-git
$ yaourt -S ttf-roboto
```
#### Termite
Edit file /etc/xdg/termite/config
```
font=Inconsolata 16
```
#### Console
Edit /etc/vsconsole.conf
```
FONT=Inconsolata-32r
```
This is a big font, use -16r if you prefer a smaller
#### Early userspace
Add hooks in the file /etc/mkinitcpio.conf
```
HOOK="... consolefont keymap"
```
### Display Manager
```
# pacman -S lxdm
$ yaourt -S archlinux-lxdm-theme
```
Edit /etc/lxdm/lxdm.conf
```
theme=Archlinux
numlock=1
```
### Enviroment
Edit /etc/enviroment
```
BROWSER=surf
EDITOR=vim
```
Edit ~/.zshrc
```
export LANG=en_US.UTF-8
```
For man pages with colors add to ~/.zshrc
```
man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}
```
### Troubleshoting
#### Java interfaces do not appear
Fix java interfaces, add /etc/profile.d/jre.sh
```
export _JAVA_AWT_WM_NONREPARENTING=1
```
#### Gradle not found Android platform-tools binaries
Enable multilib in /etc/pacman.conf and install
```
# pacman -S 1lib32-libstdc++5 gcc-multilib lib32-zlib
```
