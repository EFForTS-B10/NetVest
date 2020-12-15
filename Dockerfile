FROM rocker/geospatial:4.0.0-ubuntu18.04

WORKDIR .



RUN apt-get update -y
RUN apt-get install -y libspatialindex-dev

RUN apt install -y python3-venv python3-pip

RUN add-apt-repository -y ppa:ubuntugis/ppa
RUN apt-get -y update
RUN apt-get -y install gdal-bin
RUN apt-get -y install libgdal-dev
RUN export CPLUS_INCLUDE_PATH=/usr/include/gdal
RUN export C_INCLUDE_PATH=/usr/include/gdal

RUN pip3 install --upgrade pip

RUN pip3 install GDAL==2.4.2
RUN mkdir setupfiles
COPY . .
#COPY environment.txt /setupfiles/environment.txt 
#COPY /EFForTS-ABM/01_EFForTS-ABM/tests/nlrx_simple.R /setupfiles/nlrx_simple.R


#RUN python3 -m venv pythonenvironment
#RUN source pythonenvironment/bin/activate


#install 'wheel' before?
RUN pip install -r environment.txt


##install netlogo
#wget http://ccl.northwestern.edu/netlogo/6.1.1/NetLogo-6.1.1-64.tgz
#tar -xzf NetLogo-6.1.1-64.tgz

#install R packages
RUN apt-get install -y libssh-dev
RUN apt-get install -y libnlopt-dev
RUN apt-get install -y libudunits2-dev

RUN install2.r --error \
    nloptr \
    devtools \
    ssh \
    nlrx

RUN Rscript -e "devtools::install_github(\"nldoc/Refforts\", upgrade = \"always\")"     
