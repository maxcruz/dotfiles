## GitHub access through SSH
Install packages
```
# pacman -Ss openssh git xclip
```
### Generate a new SSH key
```
$ ssh-keygen -t rsa -b 4096
```
Keeping the default settings as they are. You'll be asked to enter a passphrase.

Add your key to the ssh-agent
```
$ eval "$(ssh-agent -s)"
$ ssh-add ~/.ssh/id_rsa
```
The response must be "Agent pid XXXX"

Add your SSH key to your account
```
$ xclip -sel clip < ~/.ssh/id_rsa.pub 
```
Copies the contents of the id_rsa.pub file to your clipboard

### Go to GitHub

1. In the top right corner of any page, click your profile photo, then click Settings.
2. In the user settings sidebar, click SSH keys.    
3. Click Add SSH key.
4. In the "Title" field, add a descriptive label for the new key. Paste your key into the "Key" field.
5. Click Add key.

### Authentication Test
```
$ ssh -T git@github.com
```
The response must be "Hi [USER_NAME]! You've successfully authenticated..."

Set username
```
$ git config --global user.name "[USER_NAME]"
$ git config --global user.email "[EMAIL]"
```
