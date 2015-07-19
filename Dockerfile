FROM ubuntu:trusty

MAINTAINER Vinh <kurei@axcoto.com>

RUN apt-get update && apt-get -y upgrade && apt-get -y install wget

# Erlang dependencies
RUN apt-get install -y libwxgtk2.8 libwxgtk2.8-dev

RUN apt-get install -y git

RUN cd /tmp; wget http://packages.erlang-solutions.com/site/esl/esl-erlang/FLAVOUR_3_general/esl-erlang_18.0-1~ubuntu~trusty_amd64.deb && \
    dpkg -i esl-erlang_18.0-1~ubuntu~trusty_amd64.deb

# @TODO Consider to use hipe
RUN apt-get update && apt-get -y install erlang erlang-base build-essential

# Rebar
RUN git clone git://github.com/rebar/rebar.git
RUN cd rebar ; ./bootstrap; cp rebar /usr/bin

CMD ["/bin/bash -l"]
