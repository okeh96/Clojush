# https://mybinder.readthedocs.io/en/latest/dockerfile.html#preparing-your-dockerfile
FROM jupyter/base-notebook:da2c5a4d00fa
RUN pip install --no-cache-dir notebook==5.*
RUN conda install -y openjdk
RUN conda install -y git
RUN pip install git+https://github.com/saulshanabrook/clojure-kernel.git

WORKDIR /opt/conda/bin
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
RUN chmod +x lein


ENV NB_USER jovyan
ENV NB_UID 1000
ENV HOME /home/${NB_USER}

RUN adduser --disabled-password \
    --gecos "Default user" \
    --uid ${NB_UID} \
    ${NB_USER}

  # Make sure the contents of our repo are in ${HOME}
COPY . ${HOME}
USER root
RUN chown -R ${NB_UID} ${HOME}
USER ${NB_USER}

WORKDIR $HOME

RUN lein deps
