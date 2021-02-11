import numpy as np

def process(vmin,vmax,xmin,xmax,dis):
    
    if not np.isnan(dis):
        res=(((vmax-vmin)*(dis-xmin))/(xmax-xmin))+vmin
        res=max(res,vmin)
        res=min(res,vmax)
        print('At %.f, the resolution is %.f m' % (dis,res))



if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(prog='get_resolution.py', usage='%(prog)s vmin vmax xmin xmax')
    ## main arguments

    parser.add_argument('vmin', type=float)
    parser.add_argument('vmax', type=float)
    parser.add_argument('xmin', type=float)
    parser.add_argument('xmax', type=float)
    parser.add_argument('-dis', type=float,default=np.nan)
    parser.add_argument('-res', type=float,default=np.nan)

    args = parser.parse_args()


    process(args.vmin,args.vmax,args.xmin,args.xmax,args.dis)