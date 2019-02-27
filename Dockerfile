# Start from the sbt builder image
FROM kwarc/sbt-builder

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

# Install MMT into /mmt/
RUN mkdir -p /mmt/deploy

# Add mmt.jar file and legal text
COPY --from=0 /build/MMT/deploy/mmt.jar /mmt/deploy/mmt.jar
ADD COPYING.txt /mmt/

# Add MMT script to PATH
ADD deploy/mmt /mmt/deploy/mmt
ENV PATH="/mmt/deploy:${PATH}"


# Run setup and setup the entry for the future
RUN mmt :setup "/mmt/" "/content/" ":" "--no-content"

ADD scripts/docker/install.msl /install.msl
ADD scripts/docker/entrypoint.sh /entrypoint.sh
ENTRYPOINT ["/entrypoint.sh"]