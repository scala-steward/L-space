FROM 	jupyter/minimal-notebook

LABEL 	maintainer="Thijs Broersen"

USER 	root

RUN 	apt-get update && apt-get upgrade -y

RUN 	apt-get install gnupg2 curl openjdk-8-jdk -y && \
        curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add - && \
        echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list && \
        apt-get update && apt-get install yarn -y

RUN 	apt-get clean && rm -rf /var/lib/apt/lists/*

USER 	$NB_UID

RUN 	pip install --upgrade pip && \
        conda update -n base conda && \
        conda install -c conda-forge jupyterlab && \
        curl -L -o coursier https://git.io/coursier && \
        chmod +x coursier && \
        ./coursier --help && \
        SCALA_VERSION=2.12.7 ALMOND_VERSION=0.1.10 && \
        ./coursier bootstrap \
        -i user -I user:sh.almond:scala-kernel-api_$SCALA_VERSION:$ALMOND_VERSION \
        sh.almond:scala-kernel_$SCALA_VERSION:$ALMOND_VERSION \
        -o almond && \
        ./almond --install && \
        jupyter labextension install @jupyterlab/git && \
        export NODE_OPTIONS=--max-old-space-size=4096 && jupyter lab build
