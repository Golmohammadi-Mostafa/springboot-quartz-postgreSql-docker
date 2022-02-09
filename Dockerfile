FROM adoptopenjdk/openjdk11:x86_64-alpine-jdk-11.0.13_8
VOLUME /tmp
ARG JAR_FILE=target/*.jar
COPY --chmod=755 ${JAR_FILE} app.jar
WORKDIR /opt/app
EXPOSE 8085
ENTRYPOINT ["java","-jar","/app.jar"]