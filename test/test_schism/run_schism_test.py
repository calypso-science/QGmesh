import sys,os
sys.path.append(os.path.join(os.path.dirname(__file__),'..','..','qgmesh')) 
from schism_test import RunSCHISMDialog
from qgis.core import (
     QgsApplication, 
     QgsProcessingFeedback, 
     QgsVectorLayer,
     QgsProject,
     QgsCoordinateTransform
)
# See https://gis.stackexchange.com/a/155852/4972 for details about the prefix 
QgsApplication.setPrefixPath('/usr', True)
qgs = QgsApplication([], False)
qgs.initQgis()

from qgis.utils import iface
print(dir(iface))
rundia = RunSCHISMDialog(['None'])
args = ["/home/remy/Software/QGmesh/qgmesh/nicegrid2","hgrid.gr3","hgrid.out","0"]
rundia.exec_(args)