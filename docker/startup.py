from PyQt5.QtCore import QSettings
from qgis.utils import iface

packageName = 'qgmesh' # Name of your plugin folder
QSettings().setValue( "PythonPlugins/" + packageName, True )
iface.mapCanvas().snappingUtils().toggleEnabled()
iface.advancedDigitizeToolBar().show()
iface.shapeDigitizeToolBar().show()
