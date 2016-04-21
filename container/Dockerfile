# Docker image for a Debian based build host
# ==========================================
#
# Supported Languages: Oracle Java, Clojure, JavaScript (node.js), Ruby (with Compass), Python, Perl
# Build Tools/Package Managers: Maven, Ant, Leiningen, Node Package Manager, Gulp, Grunt
# Tools: MongoDB, Emacs, Git

FROM debian:jessie
MAINTAINER Peter Donner <peter@markup.at>

ENV LEIN_ROOT=t
ENV LEIN_SNAPSHOTS_IN_RELEASE=t
ENV MAVEN_HOME=/apache-maven-3.3.9
ENV ANT_HOME=/apache-ant-1.9.7
ENV JAVA_HOME=/jdk1.8.0_92
ENV JRE_HOME=$JAVA_HOME/jre

ENV PATH=$MAVEN_HOME/bin:$ANT_HOME/bin:$PATH

RUN apt-get update

RUN apt-get -y install apt-transport-https

RUN apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D

RUN echo "deb https://apt.dockerproject.org/repo debian-jessie main" >> /etc/apt/sources.list.d/docker.list && \
    apt-get update

RUN apt-get -y install nodejs
RUN apt-get -y install npm
RUN apt-get -y install ruby
RUN apt-get -y install ruby-compass
RUN apt-get -y install python2.7
RUN apt-get -y install perl5
RUN apt-get -y install wget
RUN apt-get -y install git
RUN apt-get -y install screen
RUN apt-get -y install nmap
RUN apt-get -y install netcat
RUN apt-get -y install mongodb-clients
RUN apt-get -y install mongodb-server
RUN apt-get -y install nvi
RUN apt-get -y install net-tools
RUN apt-get -y install emacs
RUN apt-get -y install build-essential
RUN apt-get -y install linux-kernel-headers
RUN apt-get -y install docker-engine
RUN apt-get -y install rlwrap
RUN apt-get -y install zsh
RUN apt-get -y install ca-certificates
RUN apt-get -y install curl
RUN apt-get -y install lxc
RUN apt-get -y install iptables

RUN update-alternatives --install /usr/bin/node node /usr/bin/nodejs 10

RUN npm install -g gulp
RUN npm install -g grunt-cli
RUN npm install -g bower

RUN wget --no-cookies --no-check-certificate \
       --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" \
       "http://download.oracle.com/otn-pub/java/jdk/8u92-b14/jdk-8u92-linux-x64.tar.gz" && \
    tar -xzf jdk-8u92-linux-x64.tar.gz && \
    update-alternatives --install /usr/bin/java java /jdk1.8.0_92/bin/java 10 && \
    update-alternatives --install /usr/bin/javac javac /jdk1.8.0_92/bin/javac 10 && \
    rm jdk-8u92-linux-x64.tar.gz

RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && \
    chmod a+x lein && \
    mv lein /usr/local/bin && \
    /usr/local/bin/lein

RUN wget http://gd.tuwien.ac.at/pub/infosys/servers/http/apache/dist/maven/maven-3/3.3.9/binaries/apache-maven-3.3.9-bin.tar.gz && \
    tar -xzf apache-maven-3.3.9-bin.tar.gz && \
    rm apache-maven-3.3.9-bin.tar.gz

RUN wget http://gd.tuwien.ac.at/pub/infosys/servers/http/apache/dist/ant/binaries/apache-ant-1.9.7-bin.tar.gz && \
    tar -xzf apache-ant-1.9.7-bin.tar.gz && \
    rm apache-ant-1.9.7-bin.tar.gz

RUN git clone https://gist.github.com/7872253.git && \
    cp 7872253/.emacs ~ && \
    emacs --batch --eval '(load-file "~/.emacs")' && \
    rm -rf 7872253

RUN git clone https://github.com/jpetazzo/dind && \
    cp dind/wrapdocker /usr/local/bin && \
    chmod a+x /usr/local/bin && \
    rm -rf dind

RUN curl -L https://github.com/docker/compose/releases/download/1.5.2/docker-compose-`uname -s`-`uname -m` > /usr/local/bin/docker-compose && \
    chmod a+x /usr/local/bin/docker-compose

ADD startup.sh /root/startup.sh
RUN chmod +x /root/startup.sh

VOLUME /var/lib/docker