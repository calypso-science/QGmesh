def add_sizing_ball(self):
    inLayerGeometryType = ['Point','Line','Polygon'][QgsWkbTypes.PointGeometry]
    proj = QgsProject.instance()
    crs=proj.crs()
    outLayer = QgsVectorLayer( inLayerGeometryType+ '?crs='+crs.authid(), \
        'Ball', \
        'memory')

    outLayer.startEditing()
    outLayer.dataProvider().addAttributes([\
        QgsField("vin", QVariant.Double,"double",10,4),\
        QgsField("vout", QVariant.Double,"double",10,4),\
        QgsField("radius", QVariant.Double,"double",10,4)])

    outLayer.commitChanges()

    proj.addMapLayer(outLayer)

def add_sizing_box(self):
    inLayerGeometryType = ['Point','Line','Polygon'][QgsWkbTypes.PolygonGeometry]
    proj = QgsProject.instance()
    crs=proj.crs()
    outLayer = QgsVectorLayer( inLayerGeometryType+ '?crs='+crs.authid(), \
        'Box', \
        'memory')

    outLayer.startEditing()
    outLayer.dataProvider().addAttributes([\
        QgsField("vin", QVariant.Double,"double",10,4),\
        QgsField("vout", QVariant.Double,"double",10,4)])

    outLayer.commitChanges()

    proj.addMapLayer(outLayer)





def to_wavelength(self):
    proj = QgsProject.instance()
    raster=[]
    for child in proj.layerTreeRoot().findLayers():   
        layer = proj.mapLayer(child.layerId())     
        if layer.type()==QgsMapLayer.RasterLayer:
            raster.append(layer.name())

    ex = raster_calculator(raster,'wavelength')
    ex.show()
    ex.exec_()

def to_scale(self):
    proj = QgsProject.instance()
    raster=[]
    for child in proj.layerTreeRoot().findLayers():   
        layer = proj.mapLayer(child.layerId())     
        if layer.type()==QgsMapLayer.RasterLayer:
            raster.append(layer.name())

    ex = raster_calculator(raster,'scale')
    ex.show()
    ex.exec_()

def to_dist(self):
    proj = QgsProject.instance()
    raster=[]
    for child in proj.layerTreeRoot().findLayers():   
        layer = proj.mapLayer(child.layerId())     
        if layer.type()!=QgsMapLayer.RasterLayer:
            raster.append(layer.name())

    ex = raster_calculator(raster,'distance')
    ex.show()
    ex.exec_() 