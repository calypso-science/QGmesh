import sys,os
sys.path.append(os.path.join(os.path.dirname(__file__),'..','..','qgmesh')) 
from qgis.core import (
     QgsApplication, 
     QgsProcessingFeedback, 
     QgsVectorLayer,
     QgsProject,
     QgsCoordinateTransform,
     QgsMeshLayer,
)

#from exportGeometry import GeoFile
from utils import get_crs
import runGmsh
from mesh import Mesh
import meshio
import pygmsh
from netCDF4 import Dataset
import numpy as np
from collections import OrderedDict
from tools import get_format_from_gmsh


Mesh=Mesh([],[],[],[])
Mesh.ReadUnstructuredGridSMS('/home/remy/Software/QGmesh/test/mix_bnd/hgrid.gr3')
# Mesh.to_Gridshapefile('new_grid')
# msh=meshio.read('/home/remy/Software/QGmesh/test/mix_bnd/new_grid.msh')
# triangles,edges,physicalID=get_format_from_gmsh(msh)
# mesh=Mesh(msh.points[:,0],msh.points[:,1],msh.points[:,2],triangles,edges=edges,physical=msh.field_data,physicalID=physicalID)



root_grp = Dataset('cfl.nc', 'w', format='NETCDF4',clobber=True)
root_grp.Conventions = 'CF-1.0, UGRID-1.0'
# dimensions



root_grp.createDimension('time', 1)
root_grp.createDimension('nSCHISM_hgrid_node', len(mesh.x))
root_grp.createDimension('nSCHISM_hgrid_face', len(mesh.triangles))
root_grp.createDimension('nMaxSCHISM_hgrid_face_nodes', 4)
times = root_grp.createVariable('time', 'f8', ('time',))
latitudes = root_grp.createVariable('SCHISM_hgrid_node_y', 'f4', ('nSCHISM_hgrid_node',))
longitudes = root_grp.createVariable('SCHISM_hgrid_node_x', 'f4', ('nSCHISM_hgrid_node',))
faces = root_grp.createVariable('SCHISM_hgrid_face_nodes', 'f4', ('nSCHISM_hgrid_face','nMaxSCHISM_hgrid_face_nodes'))
cfl = root_grp.createVariable('cfl', 'f4', ('nSCHISM_hgrid_node',))
times[:]=0
latitudes[:]=mesh.y
longitudes[:]=mesh.x
tri=mesh.triangles[:,-1]==-1
faces[tri,0:3]=mesh.triangles[tri,0:3]+1
quad=mesh.triangles[:,-1]>=0
faces[quad,:]=mesh.triangles[quad,:]+1
Z=np.zeros((len(mesh.x)))
for i in range(0,len(CF)):
    Z[mesh.triangles[i]]=CF[i]
cfl[:]=Z
root_grp.close()
os.system('ncks -O --fl_fmt=classic cfl.nc cfl.nc')

import pdb;pdb.set_trace()
#mesh.AddBathy(layer.dataProvider().dataSourceUri())
mesh.writeUnstructuredGridSMS('fname2.gr3')


# See https://gis.stackexchange.com/a/155852/4972 for details about the prefix 
QgsApplication.setPrefixPath('/usr', True)
qgs = QgsApplication([], False)
qgs.initQgis()

# Append the path where processing plugin can be found
sys.path.append('/home/remy/.local/share/QGIS/QGIS3/profiles/default/processing/')

#import processing
#from processing.core.Processing import Processing
#Processing.initialize()



filename=os.path.join('/home/remy/Software/QGmesh/test/mix_bnd/','easy_test_mix_bnd.qgz')
print(filename)
proj = QgsProject.instance()
proj.read(filename)




ignoredLayers = set(proj.readEntry("qgmsh", "ignored_boundary_layers", "")[0].split("%%"))
tetraLayers = set(proj.readEntry("qgmsh", "tetralayers", "")[0].split("%%"))
quadLayers = set(proj.readEntry("qgmsh", "quadlayers", "")[0].split("%%"))
meshSizeLayerId = proj.readEntry("qgmsh", "mesh_size_layer", "None")[0]
projid = proj.readEntry("qgmsh", "projection", "")[0]
crs=get_crs(projid)


    
geo=GeoFile()
for child in proj.layerTreeRoot().findGroups(): 
    print(child.name())     

    if child.name() in ['Boundaries','Islands','Channels']:
        grid_type='tetra'#child.customProperty('grid type')
        for sub_subChild in child.children():
            layer = proj.mapLayer(sub_subChild.layerId())
            xform = QgsCoordinateTransform(layer.crs(), crs,proj)
            geo.add_layer(layer,xform,child.name(),grid_type)



geo.write_physical()
geo.writeSurface(grid_type)

for child in proj.layerTreeRoot().findGroups():      
    if child.name() in ['Sizing']:
    	for sub_subChild in child.children():
    		layer = proj.mapLayer(sub_subChild.layerId())
    		xform = QgsCoordinateTransform(layer.crs(), crs,proj)
    		geo.add_sizing(layer,xform,child.name())


with open('mix.geo', 'w') as f:
    f.write(geo.geo.get_code() + '\n')

f.close()



# main=runGmsh.RunGmshDialog(geo)
# main.show()

# myStream=runGmsh.EmittingStream()
# myStream.textWritten.connect(main.normalOutputWritten)

# sys.stdout = myStream

# msh=main.exec_()
msh=pygmsh.generate_mesh(geo.geo,extra_gmsh_arguments=['-2'])

mesh=meshio.Mesh(points=msh.points,cells=msh.cells,point_data=msh.point_data,cell_data=msh.cell_data,field_data=msh.field_data)

mesh=Mesh(mesh)
mesh.AddBathy(layer.dataProvider().dataSourceUri())
import pdb;pdb.set_trace()

mesh.writeUnstructuredGridSMS('fname2.gr3')
#mesh.writeShapefile('meshshape')
#stri=mesh._build_string()
#mesh.writeUnstructuredGridSMS('temp.sms')



# outLayer = QgsMeshLayer( mesh.stri, 'shape_name',"mesh_memory")

# proj.addMapLayer(outLayer)



