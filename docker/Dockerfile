FROM ubuntu:22.04 AS deploy
COPY docker/nsswitch.conf /etc/ 
COPY docker/startup.sh /skaha/startup.sh
COPY docker/nofiles.conf /etc/security/limits.d/ 
RUN touch /etc/sudo.conf && echo "Set disable_coredump false" > /etc/sudo.conf 
RUN apt-get update && apt-get -y install gcc  xvfb  saods9 
RUN apt update -y && apt install -y xterm vim 
RUN apt install -y sssd-ad sssd-tools realmd adcli 
RUN apt install -y python3 
RUN apt install -y iraf
RUN apt install -y libgtk-3-dev
RUN apt install -y python3-wxgtk4.0 
RUN apt install -y libx11-dev 
RUN apt install -y libxpa-dev 
RUN apt install -y xpa-tools 
RUN apt install -y pip 
RUN apt install -y python3-venv
RUN apt install -y python3-scipy
RUN apt install -y python3-numpy
RUN apt install -y python3-pyds9 
RUN mkdir /opt/MOP
RUN python3 -m venv  --system-site-packages /opt/MOP
RUN /opt/MOP/bin/pip install pyraf 
RUN /opt/MOP/bin/pip install vos 
COPY wxPython-4.2.1-cp311-cp311-linux_x86_64.whl /tmp/ 
RUN /opt/MOP/bin/pip install -f /tmp/wxPython-4.2.1-cp311-cp311-linux_x86_64.whl wxPython
RUN mkdir /opt/MOP_src
RUN /opt/MOP/bin/pip install 'astropy<6.0'
RUN apt install -y gfortran
RUN /opt/MOP/bin/pip install 'requests >= 2.7'
RUN /opt/MOP/bin/pip install 'matplotlib<3.9'
RUN /opt/MOP/bin/pip install 'd2to1 >= 0.2.10'
RUN /opt/MOP/bin/pip install 'uncertainties'
RUN /opt/MOP/bin/pip install 'pypubsub > 4.0'
RUN /opt/MOP/bin/pip install 'mp_ephem'
WORKDIR /
RUN echo "deb https://repo.astromatic.net/ubuntu/bleeding "$(grep -oP "DISTRIB_CODENAME=\K\w+" /etc/lsb-release)" main" > /etc/apt/sources.list.d/astromatic-bleeding.list
RUN apt-key adv --fetch-keys https://repo.astromatic.net/astromatic.key
RUN apt update
RUN apt install -y sextractor
RUN apt install -y libcfitsio-dev
WORKDIR /opt/MOP_src
ADD src src
WORKDIR /opt/MOP_src/src/pipematt
RUN ./install_pipematt -p /opt/MOP/
WORKDIR /opt/MOP_src/src/jmp
RUN ./install_pipejmp -p /opt/MOP
WORKDIR /opt/MOP_src/src/ossos/core
RUN /opt/MOP/bin/pip install .
WORKDIR /
ADD docker/ossos.sh /etc/profile.d/
ADD docker/iraf.sh /etc/profile.d/
RUN apt install -y csh
RUN apt install -y wcstools

# Two build sets, deploy and test
# Stuff here below here is for setting up a test user that sort of looks like a canfar science portal user
FROM deploy as test
RUN groupadd -g 1001 jkavelaars && useradd -u 1001 -g 1001 -s /bin/bash --home-dir /arc/home/jkavelaars --create-home jkavelaars
WORKDIR /arc/home/jkavelaars
USER jkavelaars
ENV HOME=/arc/home/jkavelaars
ENTRYPOINT [ "/skaha/startup.sh" ]
