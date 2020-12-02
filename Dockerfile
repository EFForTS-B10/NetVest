FROM rocker/geospatial:4.0.0-ubuntu18.04

RUN apt-get update -y
RUN apt-get install -y libspatialindex-dev

RUN apt install python3-venv python3-pip

RUN add-apt-repository ppa:ubuntugis/ppa
RUN apt-get update
RUN apt-get install gdal-bin
RUN apt-get install libgdal-dev
RUN export CPLUS_INCLUDE_PATH=/usr/include/gdal
RUN export C_INCLUDE_PATH=/usr/include/gdal

RUN pip install --upgrade pip

RUN pip install GDAL==2.4.2

COPY . /InVEST/environment.txt
COPY . /EFForTS-ABM/01_EFForTS-ABM/tests/nlrx_simple.R


python3 -m venv pythonenvironment
source pythonenvironment/bin/activate


#install 'wheel' before?
pip install -r environment.txt


##install netlogo
#wget http://ccl.northwestern.edu/netlogo/6.1.1/NetLogo-6.1.1-64.tgz
#tar -xzf NetLogo-6.1.1-64.tgz
