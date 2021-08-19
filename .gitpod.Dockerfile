FROM gitpod/workspace-full

FROM gitpod/workspace-full

RUN sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/Ammonite/releases/download/2.4.0/2.13-2.4.0) > /usr/local/bin/amm213 && chmod +x /usr/local/bin/amm213'
RUN sudo sh -c '(echo "#!/usr/bin/env sh" && curl -L https://github.com/lihaoyi/Ammonite/releases/download/2.4.0/3.0-2.4.0) > /usr/local/bin/amm30 && chmod +x /usr/local/bin/amm30'

RUN brew install sbt

RUN bash -c ". /home/gitpod/.sdkman/bin/sdkman-init.sh && sdk install java 21.2.0.r16-grl"
