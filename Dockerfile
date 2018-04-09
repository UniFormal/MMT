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

# Setup folder structure
RUN mkdir -p /root/MMT/deploy \
    && mkdir -p /root/content \
    && ln -s /content/ /root/content/ \
    && mkdir -p /root/content/MathHub \
    && ln -s /content/mmtrc /root/MMT/deploy/mmtrc

# Copy over the MMT jar
COPY --from=0 /build/MMT/deploy/mmt.jar /root/MMT/deploy/mmt.jar

# The entrypoint is the MMT jar
ENTRYPOINT ["java", "-jar", "/root/MMT/deploy/mmt.jar"]