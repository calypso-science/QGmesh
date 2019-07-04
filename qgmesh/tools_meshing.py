

def export_mesh(self):
    qfd = QFileDialog()
    path = ""

    filetype=meshio.helpers.output_filetypes
    fileext=meshio.helpers._extension_to_filetype
    fileext = dict(map(reversed, fileext.items()))
    fil='GR3 (*.gr3);'
    for typ in filetype:
        if typ in fileext:
            fil+=typ.upper()+' (*.'+fileext[typ]+');;'


    fname = QFileDialog.getSaveFileName(qfd, 'exprt Mesh file', path, fil)
    fname=fname[0]
    if fname=='':
        return

    if not fname.endswith('.gr3'):
        meshio.write(fname,self.mesh)
    else:
        self.mesh.writeUnstructuredGridSMS(fname)

    self.iface.messageBar().pushMessage("Info", "%s exported " % fname, level=Qgis.Info)


def add_bathy(self):
    proj = QgsProject.instance()
    raster=[]
    for child in proj.layerTreeRoot().findLayers():   
        layer = proj.mapLayer(child.layerId())     
        if layer.type()==QgsMapLayer.RasterLayer:
            raster.append(layer.name())

    ex = raster_calculator(raster,'choose')
    ex.show()
    layer=ex.exec_()
    
    self.mesh.AddBathy(layer)

    self.iface.messageBar().pushMessage("Info", "Bathy interpolated to the mesh " , level=Qgis.Info)

def mesh_geofile(self):


    main=RunGmshDialog(self.geo)
    main.show()

    myStream=EmittingStream()
    myStream.textWritten.connect(main.normalOutputWritten)

    sys.stdout = myStream

    msh=main.exec_()
    msh=meshio.Mesh(points=msh.points,cells=msh.cells,point_data=msh.point_data,cell_data=msh.cell_data,field_data=msh.field_data)
    
    path_absolute = QgsProject.instance().readPath('./')
    mesh_out=os.path.join(path_absolute,'new_grid.msh')
    meshio.write(mesh_out,msh)

    proj = QgsProject.instance()
    proj.writeEntry("QGmsh", "mesh_file", mesh_out)

    self.mesh=Mesh(msh)
    G=self.mesh.to_Gridshapefile('new_grid')
    self.mesh.writeShapefile('/tmp/new_grid_point','points')

    QThread.sleep(1)
    vlayer = QgsVectorLayer('/tmp/new_grid_point.shp', "Nodes", "ogr")
    QgsProject.instance().addMapLayer(vlayer,False)
    G.addLayer(vlayer)

    for physical in self.mesh.physical.keys():
        if any(self.mesh.physicalID==self.mesh.physical[physical][0]):
            self.mesh.writeShapefile('/tmp/new_grid_bnd_'+physical,'edges',physical=self.mesh.physical[physical][0])
            vlayer = QgsVectorLayer('/tmp/new_grid_bnd_'+physical+'.shp', physical, "ogr")
            QgsProject.instance().addMapLayer(vlayer,False)
            G.addLayer(vlayer)
   