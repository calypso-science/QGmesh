import numpy as np
from qgis.core import QgsPoint,QgsWkbTypes,QgsVectorFileWriter,QgsMeshLayer,QgsProject,QgsField,QgsFields,QgsLayerTreeGroup,QgsCoordinateReferenceSystem,QgsPointXY,QgsFeature,QgsGeometry
from PyQt5 import QtWidgets
from PyQt5.QtCore import QThread,QVariant,Qt
import gdal
from collections import OrderedDict
import numpy as np


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

def cal_tri_area(a):
    return np.absolute((a[0]*(a[3]-a[5])+a[2]*(a[5]-a[1])+a[4]*(a[1]-a[3]))/2.0)

     
def calculate_CFL(depth,A,dt):
    CFL=np.sqrt(np.pi*9.8*depth/(4*A))*dt
    return CFL

class Mesh(object) :

    def __init__(self,x,y,z,faces,edges=None,physical=None,physicalID=None) :

        self.types=None
        self.x=x
        self.y=y
        self.z=z
        
        
        self.physical=physical
        self.physicalID=physicalID
        self.edges=edges


        self.nodes=np.arange(1,len(self.x)+1)

        if len(x)>0:
            self.faces=faces.astype('int')
            self._calculate_face_nodes()
            self._remove_hanging_nodes()
            self._calculate_areas()
            self._calculate_res()


    def _calculate_res(self):
        self.res=2*np.sqrt(self.areas/np.pi)

    def _calculate_areas(self):
        self.areas=np.ones(len(self.faces))*-1.
        x=self.x
        y=self.y
        Elems=range(0,len(self.faces))
        ref=np.zeros((len(x),1))

        nodes_coor = np.hstack((np.vstack([x,y]).T,ref))

        for face in range(0,len(self.faces)):
            if self.faces[face,-1]<0:
                tri = np.zeros((1,3,3))
                for ivert in range(3):
                    tri[0,ivert,:]=nodes_coor[self.faces[face,ivert]] 
                self.areas[face] = cal_tri_area(tri[0,:,0:2].reshape(1,6).transpose())
            else:
                tri1 = np.zeros((1,3,3))
                tri2 = np.zeros((1,3,3))
                for iv,ivert in enumerate([0,1,3]):
                    tri1[0,iv,:] = nodes_coor[self.faces[face,ivert]] 
                for iv,ivert in enumerate([1,2,3]):
                    tri2[0,iv,:] = nodes_coor[self.faces[face,ivert]]

                a1=cal_tri_area(tri1[0,:,0:2].reshape(1,6).transpose())
                a2=cal_tri_area(tri2[0,:,0:2].reshape(1,6).transpose())

                self.areas[face] = a1+a2


    def _calculate_face_nodes(self):
        self.xctr=np.ones(len(self.faces))*-1.
        self.yctr=np.ones(len(self.faces))*-1.

        for face in range(0,len(self.faces)):
            if self.faces[face,-1]<0:
                self.xctr[face]=np.sum(self.x[self.faces[face,0:3]])/3
                self.yctr[face]=np.sum(self.y[self.faces[face,0:3]])/3

            else:
                self.xctr[face]=np.sum(self.x[self.faces[face,0:4]])/4
                self.yctr[face]=np.sum(self.y[self.faces[face,0:4]])/4


    def _build_string(self):
        stri=''
        for pt in range(0,len(self.nodes)):
            stri+='%.5f, %.5f \n' % (self.x[pt],self.y[pt])
        stri+="---\n"
        for face in range(0,len(self.faces)):
            if self.faces[face,-1]<0:
                stri+='%.f, %.f, %.f \n' % (self.faces[face,0],self.faces[face,1],self.faces[face,2])
            else:
                stri+='%.f, %.f, %.f, %.f \n' % (self.faces[face,0],self.faces[face,1],self.faces[face,2],self.faces[face,3])


        self.stri=stri

    def _remove_hanging_nodes(self):

        unique_nodes=np.unique(self.faces+1)
        unique_nodes=unique_nodes[unique_nodes!=0]

        hanging_nodes = (set(self.nodes) | set(unique_nodes)) - (set(self.nodes) & set(unique_nodes))
        hanging_nodes=sorted(hanging_nodes)
        X=self.x.tolist()
        Y=self.y.tolist()
        Z=self.z
        Z[np.isnan(Z)]=0
        Z=Z.tolist()
        triangles=self.faces.astype('float')
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
            self.faces=triangles.astype('int64')
            
            self.physicalID=np.asarray([self.physicalID[x] for x in range(0,len(edges)) if str(edges[x]) != 'nan'])
            self.edges=np.asarray([x for x in edges if str(x) != 'nan'])
            self.edges=self.edges.astype('int64')
            self.x=np.asarray([x for x in X if str(x) != 'nan'])
            self.y=np.asarray([x for x in Y if str(x) != 'nan'])
            self.z=np.asarray([x for x in Z if str(x) != 'nan'])


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


        # self._build_string()
        # outLayer = QgsMeshLayer( self.stri, \
        #     shape_name, \
        #     "mesh_memory")

        # QgsProject.instance().addMapLayer(outLayer,False)
        # G.addLayer(outLayer)
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
            fields=  QgsFields()      
            fields.append(QgsField("Id", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("Depth", QVariant.Double,'double', 9, 3))
        elif shape_type == 'edges':
            #shape=QgsWkbTypes.MultiLineString
            shape=QgsWkbTypes.Point
            fields = QgsFields()
            fields.append(QgsField("Id", QVariant.Int,'integer', 9, 0))
        elif shape_type == 'faces':
            #shape=QgsWkbTypes.MultiLineString
            shape=QgsWkbTypes.Polygon
            fields=  QgsFields()      
            fields.append(QgsField("Id", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("Node1", QVariant.Int,'integer',11,0))
            fields.append(QgsField("Node2", QVariant.Int,'integer',11,0))
            fields.append(QgsField("Node3", QVariant.Int,'integer',11,0))
            fields.append(QgsField("Node4", QVariant.Int,'integer',11,0))
            fields.append(QgsField("type", QVariant.Int,'integer', 1, 0))
            fields.append(QgsField("area", QVariant.Int,'integer', 9, 0))
            fields.append(QgsField("resolution", QVariant.Int,'integer', 9, 0))


        fileWriter = QgsVectorFileWriter(filename,
                   "system", fields,shape , crs,
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
                newFeature.setFields(fields)
                newFeature.setAttributes([int(node+1),float('%9.3f' % self.z[node])])
                fileWriter.addFeature(newFeature)

        if shape_type=='edges':
            for ie,edge in enumerate(self.edges):
                if self.physicalID[ie]==physical:
                    point = QgsPointXY()
                    point.setX(self.x[edge])
                    point.setY(self.y[edge])
                    newFeature = QgsFeature()
                    newFeature.setGeometry(QgsGeometry.fromPointXY(point))
                    newFeature.setFields(fields)
                    newFeature.setAttributes([int(edge+1)])
                    fileWriter.addFeature(newFeature)

        if shape_type=='faces':
            for ie in range(0,len(self.faces)):
                if self.faces[ie,-1]<0:
                    nface=3
                else:
                    nface=4
                
                points = []
                for fc in range(0,nface):
                    i=self.faces[ie,fc]
                    points.append(QgsPointXY(self.x[i],self.y[i]))

                i=self.faces[ie,0]
                points.append(QgsPointXY(self.x[i],self.y[i]))
                newFeature = QgsFeature()
                newFeature.setGeometry(QgsGeometry.fromPolygonXY([points]))
                newFeature.setFields(fields)
                newFeature.setAttributes([int(ie+1),int(self.faces[ie,0]+1),int(self.faces[ie,1]+1),int(self.faces[ie,2]+1),int(self.faces[ie,3]+1),int(nface),float('%9.f' % self.areas[ie]),float('%9.f' % self.res[ie])])
                fileWriter.addFeature(newFeature)


        del fileWriter

    def AddBathy(self, raster):
        raster=gdal.Open(raster)
        bathy=raster.GetRasterBand(1).ReadAsArray()
        gt = raster.GetGeoTransform()
        for pt in range(0,len(self.x)):
            self.z[pt]=bilinear(self.x[pt],self.y[pt],gt,bathy, no_data=np.nan)


    


    def map_np2ne(self,D):
        elem=self.faces
        idx3    = elem[:,-1]<0
        idx4    = quad=~idx3

        De = np.zeros((elem.shape[0],))
        De[idx3] = (D[elem[idx3,0]]+D[elem[idx3,1]]+D[elem[idx3,2]])/3
        De[idx4] = (D[elem[idx4,0]]+D[elem[idx4,1]]+D[elem[idx4,2]]+D[elem[idx4,3]])/4

        return De
    

    def get_CFL(self,dt):
        X=self.x
        Y=self.y
        D=self.z
        A=self.areas
        ele = self.faces
        De=self.map_np2ne(D)
        De[np.where(De<0.1)]=0.1
        CFL=calculate_CFL(De,A,dt)

        return CFL

