from archlinux:latest

# install requirements
run pacman -Syqu --noconfirm base-devel binutils sudo tmux bash man libressl \
    fish powerline git openssh wget curl rxvt-unicode xorg-xrdb aspell-en \
    gtk2 lib32-glibc bdf-unifont ttf-hack openssh vim xorg-xauth xorg-xhost

# install httpie
run pacman -Syqu --noconfirm httpie

# install node and npm
run pacman -Syqu --noconfirm nodejs npm

# setup our developer user
workdir /root
run groupadd -r developer -g 1000
run useradd -u 1000 -r -g developer -d /developer -c "Software Developer" developer
run echo 'developer:l337P@$$wd!' | chpasswd
copy /developer /developer
run chown -R developer:developer /developer
run chmod +x /developer/bin/*
run echo "%developer ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# install leiningen
run curl -O https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
run chmod +x lein
run mv lein /sbin

# install Emacs
run pacman -Sqyu --noconfirm xorg-fonts-encodings xorg-font-util git emacs

# install Java
run pacman -Sy --noconfirm jdk-openjdk java-openjfx maven

# install some NPM packages
run npm install -g gulp tern js-beautify jshint eslint babel-eslint eslint-plugin-react

# setup leiningen
run /sbin/lein --version

# setup SSH
run /usr/bin/ssh-keygen -A
run echo "Port 2222" >> /etc/ssh/sshd_config
run echo "X11Forwarding yes" >> /etc/ssh/sshd_config
run echo "AllowTcpForwarding yes" >> /etc/ssh/sshd_config
run echo "X11UseLocalhost yes" >> /etc/ssh/sshd_config

# switch to developer user
user developer
workdir /developer

# install spacemacs
run git clone --recursive https://github.com/syl20bnr/spacemacs ~/.emacs.d
run emacs -nw -batch -u "${UNAME}" -q -kill

# the second time ensures it has completed ;-)
run emacs -nw -batch -u "${UNAME}" -q -kill

#
# Ports
#

# Emacs server
EXPOSE 8734

# SSH server
EXPOSE 2222

ENTRYPOINT ["/bin/bash"]
