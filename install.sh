# !/bin/bash

echo "This is about to replace your current configuration files"
read -p "Are you sure? " -n 1 -r 
echo 
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

cat ./i3 > /home/$USERNAME/.config/i3/config
cat ./vimrc > /home/$USERNAME/.vimrc
cat ./zshrc > /home/$USERNAME/.zshrc

if [ ! -d /home/$USERNAME/Pictures ]; then
      mkdir -p /home/$USERNAME/Pictures;
fi

cat wonder-space.jpg > /home/$USERNAME/Pictures/Wonder.jpg
