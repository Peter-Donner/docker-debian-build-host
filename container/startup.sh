groupadd --gid $HOST_GID $HOST_USER
useradd $HOST_USER --home /home/$HOST_USER --gid $HOST_GID --uid $HOST_UID --shell /bin/bash
echo "$HOST_USER:pw" | chpasswd

#setup lein for this user
mkdir /home/$HOST_USER/.lein
cp -r /root/.lein/* /home/$HOST_USER/.lein/

mkdir /home/$HOST_USER/.emacs.d
cp -r /root/.emacs.d/* /home/$HOST_USER/.emacs.d/
cp /root/.emacs /home/$HOST_USER/.emacs

#make sure all permissions are good to go.
chown -R $HOST_USER:$HOST_USER /home/$HOST_USER

chown $HOST_USER:$HOST_USER /dev/console

su - $HOST_USER
