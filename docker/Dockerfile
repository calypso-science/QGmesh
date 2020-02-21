FROM fedora:30
MAINTAINER r.zyngfogel <r.zyngfogel@metocean.co.nz>


RUN dnf -y install 'dnf-command(copr)' git netcdf nco


###### install QGIS ############
RUN dnf -y copr enable dani/qgis
RUN dnf -y install qgis python3-qgis qgis-grass



####### install QGmesh #################
RUN cd /usr/src/ &&\
       git clone https://github.com/metocean/QGmesh.git
RUN pip3 install pygmsh scipy


###### install GMSH #################
RUN dnf -y install gmsh

####### compile NICEGRID ########
RUN dnf -y install gfortran
RUN cd /usr/src/QGmesh/qgmesh &&\
    rm nicegrid4 &&\
    gfortran -mcmodel=large -o nicegrid4 nicegrid4.f90

 


RUN mkdir -p /home/.local/share/QGIS/QGIS3/profiles/default/python/plugins/

RUN ln -s /usr/src/QGmesh/qgmesh /home/.local/share/QGIS/QGIS3/profiles/default/python/plugins/

USER $USER
ENV HOME /home/$USER

COPY startup.py /home/.local/share/QGIS/QGIS3/startup.py
COPY cust_file.ini /usr/src/cust_file.ini 

CMD ["qgis","-z","/usr/src/cust_file.ini"]

