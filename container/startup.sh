groupadd --gid $HOST_GID $HOST_USER
delgroup docker
groupadd --gid $HOST_DOCKER_GID docker
useradd $HOST_USER --home /home/$HOST_USER --gid $HOST_GID --uid $HOST_UID --shell /usr/bin/zsh
usermod -w $HOST_DOCKER_GID-$HOST_DOCKER_GID -aG docker $HOST_USER
echo "$HOST_USER:pw" | chpasswd

#setup lein for this user
mkdir /home/$HOST_USER/.lein
cp -r /root/.lein/* /home/$HOST_USER/.lein/

mkdir /home/$HOST_USER/.saves
cat /etc/zsh/zshrc > /home/$HOST_USER/.zshrc

#make sure all permissions are good to go.
chown -R $HOST_USER:$HOST_USER /home/$HOST_USER

chown $HOST_USER:$HOST_USER /dev/console

su - $HOST_USER
