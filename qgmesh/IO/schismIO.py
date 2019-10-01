import numpy as np
from collections import OrderedDict

def extension():
	return 'GR3 (*.gr3);'

def import_file(mesh, fname):
        """
        Takes appropriate triangle, node, boundary type and coordinate data and
       """

        f = open(fname)
        line = f.readline() #comment
        tmp = f.readline().strip().split(None,2) 
        n_elems=tmp[0]
        n_nodes=tmp[1]
        mesh.x=np.ones((int(n_nodes)))*-1.
        mesh.y=np.ones((int(n_nodes)))*-1.
        mesh.z=np.ones((int(n_nodes)))*-1.
        
        mesh.faces=np.ones((int(n_elems),4))*-1.

        for i in range(int(n_nodes)):
            nodeid,nodex,nodey,nodez = f.readline().strip().split(None,4)
            mesh.x[i]=float(nodex)
            mesh.y[i]=float(nodey)
            mesh.z[i]=float(nodez)


        for i in range(int(n_elems)):
            elemid,ty,tmp = f.readline().strip().split(None,2)
            tmp=tmp.split()[0:int(ty)]
            mesh.faces[i,0:int(ty)]=[float(x) for x in tmp]


        mesh.edges=[]
        mesh.physicalID=[]
        mesh.physical={}

        try:
            n_open_boundaries = int(f.readline().strip().split(None,1)[0])
        except:
            n_open_boundaries=0

        if n_open_boundaries>0:

            mesh.physical['ocean']=np.array([1,1])
            mesh.physical['coast']=np.array([2,1])
            mesh.physical['island']=np.array([3,1])
            #mesh.physical['river']=np.array([1,1])

            for n_open in range(n_open_boundaries):
                if n_open==0:
                    f.readline()

                nodes=int(f.readline().strip().split(None,1)[0])          
                for node in range(0,nodes):
                    mesh.edges.append(int(f.readline().strip().split(None,1)[0]))
                    mesh.physicalID.append(1)
                
                mesh.edges.append(0)
                mesh.physicalID.append(0)


            n_land_boundaries = int(f.readline().strip().split(None,1)[0])
            for n_land in range(n_land_boundaries):
                if n_land==0:
                    f.readline()


                tmp=f.readline().strip().split(None,2)
                nodes=tmp[0]
                ty=tmp[1]

                if ty.endswith('='):
                    ty=ty.replace('=','')
                nodes=int(nodes)  
           
                for node in range(0,nodes):
                    mesh.edges.append(int(f.readline().strip().split(None,1)[0]))
                    mesh.physicalID.append(2+int(ty))

                mesh.edges.append(0)
                mesh.physicalID.append(0)
            mesh.edges=np.array(mesh.edges).astype('int32')-1
            
        f.close()   

        mesh.nodes=np.arange(1,len(mesh.x)+1)
        mesh.faces=np.array(mesh.faces).astype('int32')-1

        return mesh



def export_file(mesh,fname):

    """
    Takes appropriate triangle, node, boundary type and coordinate data and
    writes out an SMS formatted grid file (mesh). The footer is largely static,
    but the elements, nodes and node strings are parsed from the input data.

   """

    
    triangles=mesh.faces+1
    x=mesh.x
    y=mesh.y
    z=mesh.z


    fileWrite = open(fname, 'w')
    # Add a header
    fileWrite.write('hgrid create with QGmesh\n')
    fileWrite.write('%i\t%i\n' % (len(mesh.faces),len(mesh.x)))

    # Write out the connectivity table (triangles)
    for node in range(0,len(mesh.x)):
        fileWrite.write('%i\t%f\t%f\t%f\t\n'% (node+1,mesh.x[node],mesh.y[node],mesh.z[node]))

    for face in range(0,len(mesh.faces)):
        if mesh.faces[face,-1]<0:
            fileWrite.write('%i\t3\t%i\t%i\t%i\t0\n' % (face+1 ,triangles[face,0],triangles[face,1],triangles[face,2]))
        else:
            fileWrite.write('%i\t4\t%i\t%i\t%i\t%i\n' % (face+1 ,triangles[face,0],triangles[face,1],triangles[face,2],triangles[face,3]))



    # number of open boundary
    Bnd={}
    for n in range(0,len(mesh.physicalID)):
        if not mesh.physicalID[n] in Bnd.keys() and mesh.physicalID[n]>0:
            Bnd[mesh.physicalID[n]]=[]

        if n==0:
            bnd=[]
        elif mesh.physicalID[n]<=0 and len(bnd)>0:
            Bnd[phyID].append(bnd)
            bnd=[]
        else:
            phyID=mesh.physicalID[n]
            bnd.append(mesh.edges[n])

#        if n==len(mesh.physicalID)-1:
#            Bnd[mesh.physicalID[n-1]].append(bnd)

    
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
    for name in ['ocean']:#,'river']:
        if name in mesh.physical:
            bnd+=len(Bnd[mesh.physical[name][0]])

    fileWrite.write('%i = Number of open boundaries\n' % bnd)
    # numbr of boundary node
    nope=0
    for name in ['ocean']:#,'river']:
        if name in mesh.physical:
            for n in range(0,len(Bnd[mesh.physical[name][0]])):
                nope+=len(Bnd[mesh.physical[name][0]][n])

    fileWrite.write('%i = Total number of open boundary nodes\n' % nope)

    nope=1
    for name in ['ocean']:#,'river']:
        if name in mesh.physical:
            
            for n in range(0,len(Bnd[mesh.physical[name][0]])):
                nnode=len(Bnd[mesh.physical[name][0]][n])
                fileWrite.write('%i = Number of nodes for open boundary %i\n' % (nnode,nope))
                nope+=1
                for no in range(0,nnode):
                    fileWrite.write('%s\n' % str(Bnd[mesh.physical[name][0]][n][no]+1))


    bnd=0
    for name in ['coast','island']:
        if name in mesh.physical:
            bnd+=len(Bnd[mesh.physical[name][0]])

    fileWrite.write('%i = Number of land boundaries\n' % bnd)
    # numbr of boundary node
    nope=0
    for name in ['coast','island']:
        if name in mesh.physical:
            for n in range(0,len(Bnd[mesh.physical[name][0]])):
                nope+=len(Bnd[mesh.physical[name][0]][n])

    fileWrite.write('%i = Total number of land boundary nodes\n' % nope)
    nope=1
    for name in ['coast','island']:
        if name in mesh.physical:

            if name is 'coast':
                flag=0
            else:
                flag=1


            for n in range(0,len(Bnd[mesh.physical[name][0]])):
                nnode=len(Bnd[mesh.physical[name][0]][n])
                fileWrite.write('%i\t%i = Number of nodes for land boundary %i\n' % (nnode,flag,nope))
                nope+=1
                for no in range(0,nnode):
                    fileWrite.write('%s\n' % str(Bnd[mesh.physical[name][0]][n][no]+1))



    fileWrite.close()