    def update_geofile(self):
        proj = QgsProject.instance()
        crs=proj.crs()
        self.geo=GeoFile()
        added=[]
        transfinite=False
        for child in proj.layerTreeRoot().findGroups():        
            if child.name() in ['Boundaries','Islands','Channels']:
                grid_type=child.customProperty('grid type')
                if child.name()=='Boundaries':
                    transfinite=child.customProperty('wmsTitle')
                for sub_subChild in child.children():
                    layer = proj.mapLayer(sub_subChild.layerId())
                    xform = QgsCoordinateTransform(layer.crs(), crs,proj)
                    self.geo.add_layer(layer,xform,child.name(),grid_type)
                    added.append(child.name())
        
        if len(added)>0:
            self.iface.messageBar().pushMessage("Info", "layer(s): %s added to Geometry" % ','.join(added), level=Qgis.Info)

        self.geo.writeSurface(grid_type,transfinite)

        self.geo.write_physical()

        for child in proj.layerTreeRoot().findGroups():      
            if child.name() in ['Sizing']:
                for sub_subChild in child.children():
                    layer = proj.mapLayer(sub_subChild.layerId())
                    xform = QgsCoordinateTransform(layer.crs(), crs,proj)
                    self.geo.add_sizing(layer,xform,child.name())

        if len(self.geo.Field)>0:
            self.geo.geo.add_background_field(self.geo.Field,aggregation_type='Min')


    def export_geofile(self):
        self.update_geofile()
        qfd = QFileDialog()
        path = ""
        fil = "geo(*.geo)"
        fname = QFileDialog.getSaveFileName(qfd, 'exprt Geo file', path, fil)
        fname=fname[0]
        if fname=='':
            return

        if not fname.endswith('.geo'):
            fname=fname+'.geo'

        with open(fname, 'w') as f:
            f.write(self.geo.geo.get_code() + '\n')

        f.close()
        self.iface.messageBar().pushMessage("Info", "%s exported " % fname, level=Qgis.Info)