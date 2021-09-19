FROM gitpod/workspace-full

FROM gitpod/workspace-full

ENV SHELL=zsh

SHELL ["zsh", "-c"]

RUN . /home/gitpod/.sdkman/bin/sdkman-init.sh && \
    sdk install java 21.2.0.r11-grl

RUN curl -fLo cs https://git.io/coursier-cli-"$(uname | tr LD ld)" && \
    chmod +x cs && \
    ./cs install cs && \
    rm cs && \
    echo 'export PATH="$PATH:/home/gitpod/.local/share/coursier/bin"' >> ~/.zshrc

RUN source ~/.zshrc && \
    cs install sbt && \
    cs install scalafix && \
    cs install scalafmt && \
    cs install mdoc && \
    cs install bloop --only-prebuilt=true

RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
