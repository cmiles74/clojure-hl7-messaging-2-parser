FROM mcr.microsoft.com/vscode/devcontainers/java:16-bullseye

# update packages
RUN apt update -y
RUN apt upgrade -y

# install leiningen
RUN curl -O https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
RUN chmod +x lein
RUN mv lein /usr/local/sbin
RUN lein --version

# install clj-kondo
RUN curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
RUN chmod +x install-clj-kondo
RUN ./install-clj-kondo
RUN rm ./install-clj-kondo

# install node.js
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash -
RUN apt-get install -y nodejs

ENTRYPOINT ["/bin/bash"]
