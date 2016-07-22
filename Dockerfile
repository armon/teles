FROM alpine:3.4
MAINTAINER James Grady jlgrady1@gmail.com

# Environment
ENV ENVIRONMENT production
ENV INSTALL_DIR /opt/teles

# Define the location of the teles conf
ENV TELES_CONFIG_HOME /usr/local/etc/teles
ENV CONFIG_FILE $TELES_CONFIG_HOME/app.config

# Install dependencies
RUN apk update && \
    apk add build-base \
            erlang \
            erlang-crypto \
            erlang-dev \
            erlang-erl-interface \
            erlang-reltool \
            erlang-sasl \
            erlang-syntax-tools \
            erlang-tools \
            git \
            sudo

# Configure environment
COPY LICENSE /LICENSE
COPY Makefile /teles/
COPY src /teles/src
COPY rel /teles/rel
COPY rebar /teles
COPY rebar.config /teles
RUN mkdir -p $TELES_CONFIG_HOME
COPY teles.conf $CONFIG_FILE

# Build teles
RUN cd /teles && \
    make deps && \
    make rel
RUN mkdir -p /opt && \
    mv /teles/rel/teles /opt/teles
RUN rm -rf $INSTALL_DIR/etc/app.config && \
    ln -s $CONFIG_FILE $INSTALL_DIR/etc/app.config
RUN touch $INSTALL_DIR/log/run_erl.log
RUN adduser -s /bin/sh -D teles
RUN chown -R teles:teles $INSTALL_DIR

# Define mountable config location
VOLUME /etc/teles

# Define working directory
WORKDIR /opt/teles

# Exposed ports
EXPOSE 2856

# Default command
CMD /opt/teles/bin/teles start && tail -f $INSTALL_DIR/log/run_erl.log
