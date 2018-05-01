FROM openjdk:jre-alpine

# Build dependencies
RUN apk --no-cache --no-progress add bash git curl wget

# Get sbt
ENV SBT_URL=https://dl.bintray.com/sbt/native-packages/sbt
ENV SBT_RELEASE=0.13.15
ENV PATH=/opt/sbt/bin:${PATH}

# Install sbt
RUN mkdir -p /opt
WORKDIR /opt
RUN curl -jksSL "${SBT_URL}/${SBT_RELEASE}/sbt-${SBT_RELEASE}.tgz" | tar -xzf -
RUN rm -rf /var/cache/apk/*

# Add all of MMT
ADD src/ /build/MMT/src
ADD deploy/ /build/MMT/deploy

WORKDIR /build/MMT/src
RUN sbt deploy

# Runtime dependencies
FROM openjdk:jre-alpine
RUN apk --no-cache --no-progress add bash git

# Notice that "server on [port]" will not just work inside a docker container
# and "server on [port] 0.0.0.0" should be used instead
VOLUME /content
EXPOSE 8080

# Install MMT into /root/MMT
RUN mkdir -p /root/MMT/deploy
COPY --from=0 /build/MMT/deploy/mmt.jar /root/MMT/deploy/mmt.jar

# Run setup and setup the entry for the future
RUN java -jar /root/MMT/deploy/mmt.jar :setup "/root/MMT" "/content/" ":" "--no-content"

ADD scripts/docker/entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]