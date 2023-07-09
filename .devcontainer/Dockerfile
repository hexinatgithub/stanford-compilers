FROM mcr.microsoft.com/vscode/devcontainers/base:ubuntu-18.04

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y flex bison build-essential csh libxaw7-dev libc6-i386

# Cleanup
RUN apt-get autoremove && apt-get autoclean
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
