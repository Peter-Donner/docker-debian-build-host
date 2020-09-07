# Docker image for a Debian based build host
# ==========================================
#
# Supported Languages: Oracle Java, Clojure, JavaScript (node.js), Ruby (with Compass), Python, Perl
# Build Tools/Package Managers: Maven, Ant, Leiningen, Node Package Manager
# Tools: MongoDB, Emacs, Git

FROM debian:buster
MAINTAINER Peter Donner <peter@markup.at>

ENV LEIN_ROOT=t
ENV LEIN_SNAPSHOTS_IN_RELEASE=t
ENV MAVEN_HOME=/apache-maven-3.6.3
ENV ANT_HOME=/apache-ant-1.10.8

ENV PATH=$MAVEN_HOME/bin:$ANT_HOME/bin:$PATH

RUN apt-get update

RUN apt-get -y install apt-transport-https

RUN apt-get -y install ruby
RUN apt-get -y install ruby-dev
RUN apt-get -y install python2.7
RUN apt-get -y install perl6
RUN apt-get -y install wget
RUN apt-get -y install git
RUN apt-get -y install tmux
RUN apt-get -y install nmap
RUN apt-get -y install netcat
RUN apt-get -y install nvi
RUN apt-get -y install net-tools
RUN apt-get -y install build-essential
RUN apt-get -y install emacs-gtk
RUN apt-get -y install linux-kernel-headers
RUN apt-get -y install rlwrap
RUN apt-get -y install zsh
RUN apt-get -y install ca-certificates
RUN apt-get -y install curl
RUN apt-get -y install lxc
RUN apt-get -y install manpages man-db
RUN apt-get -y install kdiff3
RUN apt-get -y install exiftool
RUN apt-get -y install haskell-platform
RUN apt-get -y install erlang
RUN apt-get -y install openjdk-11-jdk
RUN apt-get -y install locales
RUN apt-get -y install locales-all
RUN apt-get -y install gcc-avr
RUN apt-get -y install avr-libc
RUN apt-get -y install php
RUN apt-get -y install elixir
RUN apt-get -y install php-mysql
RUN apt-get -y install libusb-dev
RUN apt-get -y install sassc
RUN apt-get -y install chromium
RUN apt-get -y install rustc
RUN apt-get -y install mariadb-client

# Docker
RUN apt-get -y install apt-transport-https
RUN apt-get -y install gnupg-agent
RUN apt-get -y install software-properties-common

# Google Chrome
RUN apt-get install libdbusmenu-glib4 libdbusmenu-gtk3-4 libindicator3-7 libappindicator3-1

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs

RUN gem install sass --no-user-install

RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && \
    chmod a+x lein && \
    mv lein /usr/local/bin && \
    /usr/local/bin/lein

RUN curl -O https://download.clojure.org/install/linux-install-1.10.1.536.sh
RUN chmod +x linux-install-1.10.1.536.sh
RUN bash ./linux-install-1.10.1.536.sh

RUN wget http://www-eu.apache.org/dist/maven/maven-3/3.6.3/binaries/apache-maven-3.6.3-bin.tar.gz && \
   tar -xzf apache-maven-3.6.3-bin.tar.gz && \
   rm apache-maven-3.6.3-bin.tar.gz

RUN wget http://www-eu.apache.org/dist/ant/binaries/apache-ant-1.10.8-bin.tar.gz && \
    tar -xzf apache-ant-1.10.8-bin.tar.gz && \
    rm apache-ant-1.10.8-bin.tar.gz

RUN bash -c "cd /usr/local/bin && curl -fsSLo boot https://github.com/boot-clj/boot-bin/releases/download/latest/boot.sh && chmod 755 boot"

RUN git clone https://github.com/robbyrussell/oh-my-zsh.git
RUN mv oh-my-zsh /usr/local/share

RUN npm i tern -g

RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
RUN add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/debian \
   $(lsb_release -cs) \
   stable"
RUN apt-get update
RUN apt-get -y install docker-ce docker-ce-cli containerd.io

RUN curl -L "https://github.com/docker/compose/releases/download/1.26.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
RUN chmod 755 /usr/local/bin/docker-compose
RUN chgrp docker /usr/local/bin/docker-compose

ADD fonts/sourcecodepro /usr/share/fonts/truetype/sourcecodepro
RUN fc-cache

RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN dpkg -i google-chrome-stable_current_amd64.deb

RUN apt-get -y install firefox-esr

RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
RUN unzip awscliv2.zip
RUN ./aws/install

ADD zshrc /etc/zsh/zshrc

ADD startup.sh /root/startup.sh
RUN chmod +x /root/startup.sh
