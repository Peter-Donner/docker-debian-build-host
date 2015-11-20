# Docker image for a Debian based build host
# ==========================================
#
# Supported Languages: Oracle Java, Clojure, JavaScript (node.js), Ruby (with Compass), Python, Perl
# Build Tools/Package Managers: Maven, Ant, Leiningen, Node Package Manager, Gulp, Grunt
# Tools: MongoDB Client, Emacs, Git

FROM debian:jessie
MAINTAINER Peter Donner <peter@markup.at>

ENV LEIN_ROOT=t
ENV MAVEN_HOME=/apache-maven-3.3.9
ENV ANT_HOME=/apache-ant-1.9.6
ENV JAVA_HOME=/jdk1.8.0_66
ENV JRE_HOME=$JAVA_HOME/jre

ENV PATH=$MAVEN_HOME/bin:$ANT_HOME/bin:$PATH

RUN \
  apt-get update && \
  apt-get -y install \
          nodejs npm ruby ruby-compass python2.7 perl5 wget git screen nmap netcat mongodb-clients nvi emacs aptitude build-essential linux-kernel-headers && \
  update-alternatives --install /usr/bin/node node /usr/bin/nodejs 10

RUN \
  npm install -g gulp

RUN \
  npm install -g grunt-cli

RUN \
  wget --no-cookies --no-check-certificate --header "Cookie: gpw_e24=http%3A%2F%2Fwww.oracle.com%2F; oraclelicense=accept-securebackup-cookie" "http://download.oracle.com/otn-pub/java/jdk/8u66-b17/jdk-8u66-linux-x64.tar.gz" && \
  tar -xzf jdk-8u66-linux-x64.tar.gz && \
  update-alternatives --install /usr/bin/java java /jdk1.8.0_66/bin/java 10 && \
  update-alternatives --install /usr/bin/javac javac /jdk1.8.0_66/bin/javac 10 && \
  rm jdk-8u66-linux-x64.tar.gz

RUN \
    wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && \
    chmod a+x lein && \
    ./lein && \
    update-alternatives --install /usr/bin/lein lein /lein 10

RUN \
    wget http://gd.tuwien.ac.at/pub/infosys/servers/http/apache/dist/maven/maven-3/3.3.9/binaries/apache-maven-3.3.9-bin.tar.gz && \
    tar -xzf apache-maven-3.3.9-bin.tar.gz

RUN \
    wget http://gd.tuwien.ac.at/pub/infosys/servers/http/apache/dist/ant/binaries/apache-ant-1.9.6-bin.tar.gz && \
    tar -xzf apache-ant-1.9.6-bin.tar.gz