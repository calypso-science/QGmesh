def reload_data(self):
    proj = QgsProject.instance()
    mesh_out=proj.readEntry("QGmsh", "mesh_file", "")[0]
    if len(mesh_out)>0:
        msh=meshio.read(mesh_out)
        self.mesh=Mesh(msh)
        G=self.mesh.to_Gridshapefile('new_grid')
        




def initialize_folders_tetra(self):
    """Run method that performs all the real work"""
    # See if OK was pressed
    groups=['Boundaries','Channels','Islands','Sizing']
    proj = QgsProject.instance()




    childs=[]
    for child in proj.layerTreeRoot().children():
        if isinstance(child, QgsLayerTreeGroup):
            childs.append(child.name())

    exists_already=[]
    for group in groups:
        if group not in childs:
            
            gr=proj.layerTreeRoot().addGroup(group)
            gr.setCustomProperty('grid type','tetra')
        else:
            exists_already.append(group)

    #if len(exists_already)>1:
    #    self.iface.messageBar().pushMessage("Error", "layer(s): %s already exists" % ','.join(exists_already), level=Qgis.Warning)
def initialize_folders_quad(self):
    """Run method that performs all the real work"""
    # See if OK was pressed
    groups=['Boundaries','Islands','Sizing','Channels']
    proj = QgsProject.instance()




    childs=[]
    for child in proj.layerTreeRoot().children():
        if isinstance(child, QgsLayerTreeGroup):
            childs.append(child.name())

    exists_already=[]
    for group in groups:
        if group not in childs:
            gr=proj.layerTreeRoot().addGroup(group)
            gr.setCustomProperty('grid type','quad')

        else:
            exists_already.append(group)

def initialize_folders_transfinite(self):
    """Run method that performs all the real work"""
    # See if OK was pressed
    groups=['Boundaries','corners']
    proj = QgsProject.instance()




    childs=[]
    for child in proj.layerTreeRoot().children():
        if isinstance(child, QgsLayerTreeGroup):
            childs.append(child.name())

    exists_already=[]
    for group in groups:
        if group not in childs:
            gr=proj.layerTreeRoot().addGroup(group)
            gr.setCustomProperty('grid type','transfinite')

        else:
            exists_already.append(group)
