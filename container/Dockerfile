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
ENV MAVEN_HOME=/apache-maven-3.8.4
ENV ANT_HOME=/apache-ant-1.10.11
ENV RUSTUP_HOME=/rust
ENV CARGO_HOME=/cargo

ENV PATH=$MAVEN_HOME/bin:$ANT_HOME/bin:$RUSTUP_HOME/bin:$CARGO_HOME/bin:$PATH

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
RUN apt-get -y install mariadb-client
RUN apt-get -y install keepassxc

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

RUN curl -O https://download.clojure.org/install/linux-install-1.11.1.1113.sh
RUN chmod +x linux-install-1.11.1.1113.sh
RUN bash ./linux-install-1.11.1.1113.sh

RUN wget http://www-eu.apache.org/dist/maven/maven-3/3.8.4/binaries/apache-maven-3.8.4-bin.tar.gz && \
   tar -xzf apache-maven-3.8.4-bin.tar.gz && \
   rm apache-maven-3.8.4-bin.tar.gz

RUN wget http://www-eu.apache.org/dist/ant/binaries/apache-ant-1.10.11-bin.tar.gz && \
    tar -xzf apache-ant-1.10.11-bin.tar.gz && \
    rm apache-ant-1.10.11-bin.tar.gz

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

RUN echo "(curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly --no-modify-path) && rustup default nightly" > /install-rust.sh && chmod 755 /install-rust.sh

RUN /install-rust.sh

RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
RUN unzip awscliv2.zip
RUN ./aws/install

RUN npm install --global @angular/cli@next

RUN apt-get -y install git-flow
RUN apt-get -y install android-tools-adb

# Dependencies for embedded MySQL
RUN apt-get -y install libaio1
RUN apt-get -y install libncurses5

RUN curl -fsSL https://apt.releases.hashicorp.com/gpg | apt-key add -
RUN apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main"
RUN apt-get update && apt-get -y install terraform

RUN curl -sL https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor | tee /etc/apt/trusted.gpg.d/microsoft.gpg > /dev/null
RUN echo "deb [arch=amd64] https://packages.microsoft.com/repos/azure-cli/ buster main" | tee /etc/apt/sources.list.d/azure-cli.list
RUN apt-get update
RUN apt-get install -y azure-cli

RUN ln -sf /usr/share/zoneinfo/Europe/Vienna /etc/localtime
RUN echo Europe/Vienna > /etc/timezone

ADD zshrc /etc/zsh/zshrc

ADD startup.sh /root/startup.sh
RUN chmod +x /root/startup.sh
