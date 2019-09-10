# QGmesh

Qgis plugin to create finite element, quad or regular mesh for ocean modelling. QGmesh allow the user to create multi-structure grid ( combining quad and triangle mesh).


### QGIS prerequisites

Qgis need to be installed, must version >3:
```
https://qgis.org/en/site/forusers/download.html
```
GDAL must installed with QGIS

### GMSH prerequisites

The Gmesh software need to be installed:

```
http://gmsh.info/
```
copy the gmsh executable to your path

### PYTHON3 prerequisites

Installed the following library:
```
pygmsh
meshIO
```

### Installing

To install clone this repo to your home directory:
```
git clone git@github.com:metocean/QGmesh.git
```
Create a shortcut from Qgmesh/qgmesh to QGIS plugin folder.
On unix:
```
ln -s /home/USER/QGmesh/qgmesh/ /home/USER/.local/share/QGIS/QGIS3/profiles/default/python/plugins
```
On WIndows open command prompt:
```
mklink C:\Users\USER\Qgmesh\qgmesh C:\Users\USER\AppData\Roaming\QGIS\QGIS3\profiles\default\python\plugins
```
Open Qgis
go to Plugins>manage and install plugins>Settings
Tick the box eperimental plugins
got to Installed
Tick the box QGMESH


### Adding your own mesh Input/Outut code

With QGmesh the user can build its own code to read and write in the desire mesh format
code should be as follow:


```
Give an example
```

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Remy Zyngfogel** - *Initial work*

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* This work is based on the work done by Jonathan Lambrechts with his GMESH plugin https://plugins.qgis.org/plugins/gmsh/
* also some tools were copied from qmesh plugins https://bitbucket.org/qmesh-developers/qmesh-qgis-plugins
