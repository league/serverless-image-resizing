FROM amazonlinux

ADD etc/nodesource.gpg.key /etc

WORKDIR /tmp

RUN yum -y install gcc-c++
RUN rpm --import /etc/nodesource.gpg.key
RUN curl --location --output ns.rpm https://rpm.nodesource.com/pub_6.x/el/7/x86_64/nodejs-6.10.1-1nodesource.el7.centos.x86_64.rpm
RUN rpm --checksig ns.rpm
RUN rpm --install --force ns.rpm
RUN npm install -g npm@latest
RUN npm install -g aws-lambda-local
RUN npm cache clean --force
RUN yum clean all
RUN rm --force ns.rpm

RUN yum -y install shadow-utils
RUN useradd --uid 1000 -g users --no-user-group user
USER user
WORKDIR /build
