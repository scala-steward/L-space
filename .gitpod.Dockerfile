FROM gitpod/workspace-full

USER gitpod

RUN bash -c ". /home/gitpod/.sdkman/bin/sdkman-init.sh && \
    sdk install java 17.0.3-tem && \
    sdk default java 17.0.3-tem"

RUN zsh -c ". /home/gitpod/.sdkman/bin/sdkman-init.sh && \
    sdk default java 17.0.3-tem && \
    curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes && \
    ./cs install sbt scalafix scalafmt mdoc && \
    ./cs install bloop --only-prebuilt=true && \
    echo 'export PATH="$PATH:/home/gitpod/.local/share/coursier/bin"' >> ~/.zshrc && \
    echo 'export PATH="$PATH:/home/gitpod/.local/share/coursier/bin"' >> ~/.bashrc && \
    rm cs"
