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
#### Xorg 
```
# pacman -S ...
```
#### Bumblebee
Only for new hybrid video cards
```
# pacman -S bumblebee bbswitch nvidia
# gpasswd -a user bumblebee
# systemctl enable bumblebee.service
# systemctl start bumblebee.service
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
# pacman -S notify-osd
```
Create the file ~/.xinitrc
```
exec xmonad
```
### Utilities
* Images: eog
* Screenshot: scrot
* Color picker: gpick
* Terminal: termite
* Browser: surf google-chrome-stable
* Editor: vim (pacman -Rcs nano)
* PDF: evince
* System info: archey3
* Monitor: inxi lm_sensors net-tools hddtemp demicode
* File manager: pcmanfm-gtk3

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
### Enviroment
Edit /etc/enviroment
```
BROWSER=surf
EDITOR=vim
```

/etc/profile.d/jre.sh
_JAVA_AWT_WM_NONREPARENTING=1

mkdir -p .config/termite
➜  ~ cp /etc/xdg/termite/config ~/.config/termite/config

mkdir -p .config/termite
➜  ~ cp /etc/xdg/termite/config ~/.config/termite/config
