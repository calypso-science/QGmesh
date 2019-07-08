import glob,os

plugin_folder='IO'
list_of_files = glob.glob(os.path.join(plugin_folder,'*IO.py'))


import_function={}

for file in list_of_files:
	filename=os.path.split(file)[-1].strip('.py')
	imp = getattr(__import__('IO.'+filename),filename)
	import_function[filename]={}
	import_function[filename]['extension']=getattr(imp, 'extension')
	import_function[filename]['import']=getattr(imp, 'import_file')
	import_function[filename]['export']=getattr(imp, 'export_file')



print(import_function[filename]['extension']())