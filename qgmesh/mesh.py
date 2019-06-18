import numpy as np
from qgis.core import QgsMeshLayer,QgsProject,QgsField,QgsLayerTreeGroup


class Mesh(object) :

    def __init__(self,mesh) :
        self.mesh=mesh
        
        self.types=None
        self.x=mesh.points[:,0]
        self.y=mesh.points[:,1]
        self.z=mesh.points[:,2]
        self.nodes=np.arange(1,len(self.x)+1)
        self.triangles=mesh.cells
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
    def to_shapefile(self,shape_name):


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



        proj = QgsProject.instance()
        crs=proj.crs()
        self._build_string()
        outLayer = QgsMeshLayer( self.stri, \
            shape_name, \
            "mesh_memory")

        proj.addMapLayer(outLayer)

        root = QgsProject.instance().layerTreeRoot()
        layer = root.findLayer(outLayer.id())
        clone = layer.clone()
        G.insertChildNode(0, clone)
        root.removeChildNode(layer)



    def writeUnstructuredGridSMS(self, mesh):
        """
        Takes appropriate triangle, node, boundary type and coordinate data and
        writes out an SMS formatted grid file (mesh). The footer is largely static,
        but the elements, nodes and node strings are parsed from the input data.

        Input data is probably best obtained from one of:

            grid_tools.parseUnstructuredGridSMS()
            grid_tools.parseUnstructuredGridFVCOM()
            grid_tools.parseUnstructuredGridMIKE()

        which read in the relevant grids and output the required information for
        this function.

        The footer contains meta data and additional information. See page 18 in
        http://smstutorials-11.0.aquaveo.com/SMS_Gen2DM.pdf.

        In essence, four bits are critical:
            1. The header/footer MESH2D/BEGPARAMDEF
            2. E3T prefix for the connectivity:
                (elementID, node1, node2, node3, material_type)
            3. ND prefix for the node information:
                (nodeID, x, y, z)
            4. NS prefix for the node strings which indicate the open boundaries.

        As far as I can tell, the footer is largely irrelevant for FVCOM purposes.

        Parameters
        ----------
        triangles : ndarray
            Integer array of shape (ntri, 3). Each triangle is composed of
            three points and this contains the three node numbers (stored in
            nodes) which refer to the coordinates in X and Y (see below).
        nodes : ndarray
            Integer number assigned to each node.
        x, y, z : ndarray
            Coordinates of each grid node and any associated Z value.
        types : ndarray
            Classification for each open boundary. DHI MIKE21 .mesh format
            requires unique IDs for each open boundary (with 0 and 1
            reserved for land and sea nodes). Similar values can be used in
            SMS grid files too.
        mesh : str
            Full path to the output file name.

        """
        triangles=self.triangles
        nodes=self.nodes
        x=self.x
        y=self.y
        z=self.z
        if self.types is None:
            types=self.z
        else:
            types=self.types




        fileWrite = open(mesh, 'w')
        # Add a header
        fileWrite.write('MESH2D\n')

        # Write out the connectivity table (triangles)
        currentNode = 0
        for line in triangles:

            # Bump the numbers by one to correct for Python indexing from zero
            line = line + 1
            strLine = []
            # Convert the numpy array to a string array
            for value in line:
                strLine.append(str(value))

            currentNode+=1
            # Build the output string for the connectivity table
            if value==0:
                output = ['E3T'] + [str(currentNode)] + strLine[0:-1] + ['1']
            else:
                output = ['E4Q'] + [str(currentNode)] + strLine + ['1']

            output = ' '.join(output)
            #print output

            fileWrite.write(output + '\n')

        # Add the node information (nodes)
        for count, line in enumerate(nodes):

            # Convert the numpy array to a string array
            strLine = str(line)

            # Format output correctly
            output = ['ND'] + \
                    [strLine] + \
                    ['{:.8e}'.format(x[count])] + \
                    ['{:.8e}'.format(y[count])] + \
                    ['{:.8e}'.format(z[count])]
            output = ' '.join(output)

            fileWrite.write(output + '\n')

        # Convert MIKE boundary types to node strings. The format requires a prefix
        # NS, and then a maximum of 10 node IDs per line. The node string tail is
        # indicated by a negative node ID.

        # Iterate through the unique boundary types to get a new node string for
        # each boundary type (ignore types of less than 2 which are not open
        # boundaries in MIKE).
        for boundaryType in np.unique(types[types>1]):

            # Find the nodes for the boundary type which are greater than 1 (i.e.
            # not 0 or 1).
            nodeBoundaries = nodes[types==boundaryType]

            nodeStrings = 0
            for counter, node in enumerate(nodeBoundaries):
                if counter+1 == len(nodeBoundaries) and node > 0:
                    node = -node

                nodeStrings += 1
                if nodeStrings == 1:
                    output = 'NS  {:d} '.format(int(node))
                    fileWrite.write(output)
                elif nodeStrings != 0 and nodeStrings < 10:
                    output = '{:d} '.format(int(node))
                    fileWrite.write(output)
                elif nodeStrings == 10:
                    output = '{:d} '.format(int(node))
                    fileWrite.write(output + '\n')
                    nodeStrings = 0

            # Add a new line at the end of each block. Not sure why the new line
            # above doesn't work...
            fileWrite.write('\n')

        # Add all the blurb at the end of the file.
        #
        # BEGPARAMDEF = Marks end of mesh data/beginning of mesh model definition
        # GM = Mesh name (enclosed in "")
        # SI = use SI units y/n = 1/0
        # DY = Dynamic model y/n = 1/0
        # TU = Time units
        # TD = Dynamic time data (?)
        # NUME = Number of entities available (nodes, node strings, elements)
        # BGPGC = Boundary group parameter group correlation y/n = 1/0
        # BEDISP/BEFONT = Format controls on display of boundary labels.
        # ENDPARAMDEF = End of the mesh model definition
        # BEG2DMBC = Beginning of the model assignments
        # MAT = Material assignment
        # END2DMBC = End of the model assignments
        footer = 'BEGPARAMDEF\n\
    GM  "Mesh"\n\
    SI  0\n\
    DY  0\n\
    TU  ""\n\
    TD  0  0\n\
    NUME  3\n\
    BCPGC  0\n\
    BEDISP  0 0 0 0 1 0 1 0 0 0 0 1\n\
    BEFONT  0 2\n\
    BEDISP  1 0 0 0 1 0 1 0 0 0 0 1\n\
    BEFONT  1 2\n\
    BEDISP  2 0 0 0 1 0 1 0 0 0 0 1\n\
    BEFONT  2 2\n\
    ENDPARAMDEF\n\
    BEG2DMBC\n\
    MAT  1 "material 01"\n\
    END2DMBC\n'

        fileWrite.write(footer)

        fileWrite.close()