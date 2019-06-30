<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="3.6.3-Noosa" hasScaleBasedVisibilityFlag="0" minScale="1e+8" maxScale="0" styleCategories="AllStyleCategories">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <customproperties>
    <property key="WMSBackgroundLayer" value="false"/>
    <property key="WMSPublishDataSourceUrl" value="false"/>
    <property key="embeddedWidgets/count" value="0"/>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <rasterrenderer band="1" opacity="1" type="singlebandpseudocolor" classificationMax="0.98063" alphaBand="-1" classificationMin="0">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>MinMax</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader classificationMode="2" clip="0" colorRampType="INTERPOLATED">
          <colorramp name="[source]" type="gradient">
            <prop k="color1" v="215,25,28,255"/>
            <prop k="color2" v="43,131,186,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.25;253,174,97,255:0.5;255,255,191,255:0.75;171,221,164,255"/>
          </colorramp>
          <item alpha="255" color="#d7191c" label="0" value="0"/>
          <item alpha="255" color="#e85b3a" label="0.109" value="0.108958888888889"/>
          <item alpha="255" color="#f99e59" label="0.218" value="0.217917777777778"/>
          <item alpha="255" color="#fec980" label="0.327" value="0.326876666666667"/>
          <item alpha="255" color="#ffedaa" label="0.436" value="0.435835555555556"/>
          <item alpha="255" color="#edf8b9" label="0.545" value="0.544794444444444"/>
          <item alpha="255" color="#c7e9ad" label="0.654" value="0.653753333333333"/>
          <item alpha="255" color="#9dd3a7" label="0.763" value="0.762712222222222"/>
          <item alpha="255" color="#64abb0" label="0.872" value="0.871671111111111"/>
          <item alpha="255" color="#2b83ba" label="0.981" value="0.98063"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0"/>
    <huesaturation colorizeGreen="128" colorizeBlue="128" grayscaleMode="0" colorizeStrength="100" saturation="0" colorizeOn="0" colorizeRed="255"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
