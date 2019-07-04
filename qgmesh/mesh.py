import numpy as np
from qgis.core import QgsPoint,QgsWkbTypes,QgsVectorFileWriter,QgsMeshLayer,QgsProject,QgsField,QgsFields,QgsLayerTreeGroup,QgsCoordinateReferenceSystem,QgsPointXY,QgsFeature,QgsGeometry

from PyQt5.QtCore import QThread
import gdal
from collections import OrderedDict

def bilinear(px, py,gt,band_array, no_data=np.nan):
    '''Bilinear interpolated point at (px, py) on band_array
    example: bilinear(2790501.920, 6338905.159)'''
    ny, nx = band_array.shape
    # Half raster cell widths
    hx = gt[1]/2.0
    hy = gt[5]/2.0
    # Calculate raster lower bound indices from point
    fx = (px - (gt[0] + hx))/gt[1]
    fy = (py - (gt[3] + hy))/gt[5]
    ix1 = int(np.floor(fx))
    iy1 = int(np.floor(fy))
    # Special case where point is on upper bounds
    if fx == float(nx - 1):
        ix1 -= 1
    if fy == float(ny - 1):
        iy1 -= 1
    # Upper bound indices on raster
    ix2 = ix1 + 1
    iy2 = iy1 + 1
    # Test array bounds to ensure point is within raster midpoints
    if (ix1 < 0) or (iy1 < 0) or (ix2 > nx - 1) or (iy2 > ny - 1):
        return no_data
    # Calculate differences from point to bounding raster midpoints
    dx1 = px - (gt[0] + ix1*gt[1] + hx)
    dy1 = py - (gt[3] + iy1*gt[5] + hy)
    dx2 = (gt[0] + ix2*gt[1] + hx) - px
    dy2 = (gt[3] + iy2*gt[5] + hy) - py
    # Use the differences to weigh the four raster values
    div = gt[1]*gt[5]
    return (band_array[iy1,ix1]*dx2*dy2/div +
            band_array[iy1,ix2]*dx1*dy2/div +
            band_array[iy2,ix1]*dx2*dy1/div +
            band_array[iy2,ix2]*dx1*dy1/div)

class Mesh(object) :

    def __init__(self,mesh) :
        self.mesh=mesh
        
        self.types=None
        self.x=mesh.points[:,0]
        self.y=mesh.points[:,1]
        self.z=mesh.points[:,2]
        self.physical=mesh.field_data
        self.physicalID=mesh.cell_data['line']['gmsh:physical']

        self.nodes=np.arange(1,len(self.x)+1)
        self.triangles=mesh.cells
        proj = QgsProject.instance()
        self.crs=proj.crs()

        tot=0
        tri_len=0
        quad_len=0
        if 'triangle' in mesh.cells:
            tri_len=len(mesh.cells['triangle'])
            tot+=tri_len

        if 'quad' in mesh.cells:
            quad_len=len(mesh.cells['quad'])
            tot+=quad_len

        self.triangles=np.ones((tot,4),int)*-1

        self.edges=[]
        for edge in mesh.cells['line']:
            self.edges.append(edge[0])
            self.edges.append(edge[1])
        self.edges=np.array(list(OrderedDict.fromkeys(self.edges)))

        if 'triangle' in mesh.cells:
            self.triangles[0:tri_len,0:3]=mesh.cells['triangle']

        if 'quad' in mesh.cells:
            self.triangles[tri_len:tot,:]=mesh.cells['quad']

        self.triangles=self.triangles

    def _build_string(self):
        stri=''
        for pt in range(0,len(self.nodes)):
            stri+='%.5f, %.5f \n' % (self.x[pt],self.y[pt])
        stri+="---\n"
        for face in range(0,len(self.triangles)):
            if self.triangles[face,-1]<0:
                stri+='%.f, %.f, %.f \n' % (self.triangles[face,0],self.triangles[face,1],self.triangles[face,2])
            else:
                stri+='%.f, %.f, %.f, %.f \n' % (self.triangles[face,0],self.triangles[face,1],self.triangles[face,2],self.triangles[face,3])


        self.stri=stri
    def to_Gridshapefile(self,shape_name):


        group='Mesh'
        proj = QgsProject.instance()

        childs=[]
        for child in proj.layerTreeRoot().children():
            if isinstance(child, QgsLayerTreeGroup):
                childs.append(child.name())


        if group not in childs:
            G=proj.layerTreeRoot().addGroup(group)
        else:
            G=proj.layerTreeRoot().findGroup(group)


        QThread.sleep(1)





        self._build_string()
        outLayer = QgsMeshLayer( self.stri, \
            shape_name, \
            "mesh_memory")

        QgsProject.instance().addMapLayer(outLayer,False)
        G.addLayer(outLayer)
        return G


    def writeShapefile(self, filename,shape_type,physical=None):

        if filename[-4:]!='.shp':
              filename += '.shp'

        proj = QgsProject.instance()
        crs=proj.crs()
        #Create new shapefile object, loop trough triangle edges and add each
        # edge as a line.
        if shape_type == 'points':
            shape=QgsWkbTypes.Point
        elif shape_type == 'edges':
            #shape=QgsWkbTypes.MultiLineString
            shape=QgsWkbTypes.Point



        fileWriter = QgsVectorFileWriter(filename,
                   "system", QgsFields(),shape , crs,
                   "ESRI Shapefile")

        if fileWriter.hasError() != QgsVectorFileWriter.NoError:
            raise Exception('Error when creating shapefile '+filename+
                        ' : '+str(fileWriter.hasError()))

        if shape_type == 'points':
            for node in range(0,len(self.x)):
                point = QgsPointXY()
                point.setX(self.x[node])
                point.setY(self.y[node])
                newFeature = QgsFeature()
                newFeature.setGeometry(QgsGeometry.fromPointXY(point))
                fileWriter.addFeature(newFeature)

        if shape_type=='edges':
            for ie,edge in enumerate(self.edges):
                if self.physicalID[ie]==physical:
                    point = QgsPointXY()
                    point.setX(self.x[edge])
                    point.setY(self.y[edge])
                    newFeature = QgsFeature()
                    newFeature.setGeometry(QgsGeometry.fromPointXY(point))
                    fileWriter.addFeature(newFeature)


        del fileWriter

    def AddBathy(self, raster):
        raster=gdal.Open(raster)
        bathy=raster.GetRasterBand(1).ReadAsArray()
        gt = raster.GetGeoTransform()
        for pt in range(0,len(self.x)):
            self.z[pt]=bilinear(self.x[pt],self.y[pt],gt,bathy, no_data=np.nan)


    def remove_hanging_nodes(self):
        unique_nodes=np.unique(self.triangles+1)
        unique_nodes=unique_nodes[unique_nodes!=0]

        hanging_nodes = (set(self.nodes) | set(unique_nodes)) - (set(self.nodes) & set(unique_nodes))
        hanging_nodes=sorted(hanging_nodes)
        X=self.x.tolist()
        Y=self.y.tolist()
        Z=self.z
        Z[np.isnan(Z)]=0
        Z=Z.tolist()
        triangles=self.triangles.astype('float')
        edges=self.edges.astype('float')
        triangles[triangles==-1]=np.nan
        for hanging_node in hanging_nodes:
            edges[edges==hanging_node]=np.nan




        
        if len(hanging_nodes)>0:
            node_removed=1
            for hanging_node in hanging_nodes:
                X[hanging_node-1]=np.nan
                Y[hanging_node-1]=np.nan
                Z[hanging_node-1]=np.nan

                triangles[triangles>hanging_node-node_removed]=triangles[triangles>hanging_node-node_removed]-1

                edges[edges>hanging_node-node_removed]=edges[edges>hanging_node-node_removed]-1
                node_removed+=1


        triangles[np.isnan(triangles)]=-1
        self.triangles=triangles.astype('int64')
        
        self.physicalID=np.asarray([self.physicalID[x] for x in range(0,len(edges)) if str(edges[x]) != 'nan'])
        self.edges=np.asarray([x for x in edges if str(x) != 'nan'])
        self.edges=self.edges.astype('int64')
        self.x=np.asarray([x for x in X if str(x) != 'nan'])
        self.y=np.asarray([x for x in Y if str(x) != 'nan'])
        self.z=np.asarray([x for x in Z if str(x) != 'nan'])

        
     



    def writeUnstructuredGridSMS(self, mesh):
        """
        Takes appropriate triangle, node, boundary type and coordinate data and
        writes out an SMS formatted grid file (mesh). The footer is largely static,
        but the elements, nodes and node strings are parsed from the input data.

       """

        self.remove_hanging_nodes()
        triangles=self.triangles+1
        x=self.x
        y=self.y
        z=self.z


        fileWrite = open(mesh, 'w')
        # Add a header
        fileWrite.write('hgrid create with QGmesh\n')
        fileWrite.write('%i\t%i\n' % (len(self.triangles),len(self.x)))

        # Write out the connectivity table (triangles)
        for node in range(0,len(self.x)):
            fileWrite.write('%i\t%f\t%f\t%f\t\n'% (node+1,self.x[node],self.y[node],self.z[node]))

        for face in range(0,len(self.triangles)):
            if self.triangles[face,-1]<0:
                fileWrite.write('%i\t3\t%i\t%i\t%i\t0\n' % (face+1 ,triangles[face,0],triangles[face,1],triangles[face,2]))
            else:
                fileWrite.write('%i\t4\t%i\t%i\t%i\t%i\n' % (face+1 ,triangles[face,0],triangles[face,1],triangles[face,2],triangles[face,3]))




        # number of open boundary
        Bnd={}
        for n in range(0,len(self.physicalID)):
            if not self.physicalID[n] in Bnd.keys():
                Bnd[self.physicalID[n]]=[]

            if n==0:
                bnd=[]
            elif self.physicalID[n]!=self.physicalID[n-1]:

                Bnd[self.physicalID[n-1]].append(bnd)
                bnd=[]


            bnd.append(self.edges[n])


            if n==len(self.physicalID)-1:
                Bnd[self.physicalID[n-1]].append(bnd)

        
        for key in Bnd:
            for b in range(0,len(Bnd[key])):
                if b==0:
                    tout=list(OrderedDict.fromkeys(Bnd[key][b]))
                    Bnd[key][b]=tout.copy()

                else:

                    Bnd[key][b]=list(OrderedDict.fromkeys(Bnd[key][b]))
                    Bnd[key][b]=[x for x in Bnd[key][b] if x not in tout]
                    tout=tout+Bnd[key][b]



        bnd=0
        for name in ['ocean','river']:
            if name in self.physical:
                bnd+=len(Bnd[self.physical[name][0]])

        fileWrite.write('%i = Number of open boundaries\n' % bnd)
        # numbr of boundary node
        nope=0
        for name in ['ocean','river']:
            if name in self.physical:
                for n in range(0,len(Bnd[self.physical[name][0]])):
                    nope+=len(Bnd[self.physical[name][0]][n])

        fileWrite.write('%i = Total number of open boundary nodes\n' % nope)

        nope=1
        for name in ['ocean','river']:
            if name in self.physical:
                
                for n in range(0,len(Bnd[self.physical[name][0]])):
                    nnode=len(Bnd[self.physical[name][0]][n])
                    fileWrite.write('%i = Number of nodes for open boundary %i\n' % (nnode,nope))
                    nope+=1
                    for no in range(0,nnode):
                        fileWrite.write('%s\n' % str(Bnd[self.physical[name][0]][n][no]+1))


        bnd=0
        for name in ['coast','island']:
            if name in self.physical:
                bnd+=len(Bnd[self.physical[name][0]])

        fileWrite.write('%i = Number of land boundaries\n' % bnd)
        # numbr of boundary node
        nope=0
        for name in ['coast','islands']:
            if name in self.physical:
                for n in range(0,len(Bnd[self.physical[name][0]])):
                    nope+=len(Bnd[self.physical[name][0]][n])

        fileWrite.write('%i = Total number of land boundary nodes\n' % nope)
        nope=1
        for name in ['coast','island']:
            if name in self.physical:

                if name is 'coast':
                    flag=0
                else:
                    flag=1


                for n in range(0,len(Bnd[self.physical[name][0]])):
                    nnode=len(Bnd[self.physical[name][0]][n])
                    fileWrite.write('%i\t%i = Number of nodes for land boundary %i\n' % (nnode,flag,nope))
                    nope+=1
                    for no in range(0,nnode):
                        fileWrite.write('%s\n' % str(Bnd[self.physical[name][0]][n][no]+1))



        fileWrite.close()
